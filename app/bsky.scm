;;;
;;; Bluesky API library
;;;

;; https://atproto.com/blog/create-post

(define-module app.bsky
  (use file.util)
  (use srfi.19)
  (use srfi.42)
  (use rfc.822)
  (use rfc.http)
  (use rfc.json)
  (export <bsky-session> <bsky-record> <bsky-error>
          bsky-error bsky-check-status!
          make-bsky-session
          bsky-post-text
          ))
(select-module app.bsky)

(define-constant *endpoint-host* "bsky.social")

;; We need <bsky-session> first, to do API calls.
;; NB: We don't keep app-password in <bsky> instance.  It is only
;; needed to initiate a session.
(define-class <bsky-session> ()
  ((handle :init-keyword :handle)       ;Bluesky handle
   (did :init-keyword :did)
   (access-jwt :init-keyword :access-jwt)
   (refresh-jwt :init-keyword :refresh-jwt)))

;; Some API returns a record consisting of uri and cid.
(define-class <bsky-record> ()
  ((uri :init-keyword :uri)
   (cid :init-keyword :cid)
   (original-json :init-keyword :original-json)))

;; Error is thrown with this condition.
;; payload is a parsed json, or a string if the response is not json.
(define-condition-type <bsky-error> <error> bsky-error?
  (status-code #f)
  (http-headers #f)
  (payload #f))

;; For the convenience, handle and appPassword pairs can be saved
;; in the settings file.  They are in JSON as follows:
;;
;;  [ {"handle" : "...", "appPassword" : "..."},
;;    ... ]
;;
;; By default, "~/.gauche-bsky.json" is assumed.

;; Internal.  Returns handle and app-password.
(define (bsky-load-settings :optional (handle #f)
                                      (settings-file
                                       (expand-path "~/.gauche-bsky.json")))
  (assume-type handle (<?> <string>))
  (let1 json (with-input-from-file settings-file parse-json)
    (unless (and (vector? json)
                 (>= (vector-length json) 1))
      (error "Invalid gauche-bsky settings in:" settings-file))
    (if handle
      (let/cc return
        (do-ec [: v json]
               (if (and (equal? (assoc-ref v "handle") handle)
                        (string? (assoc-ref v "appPassword")))
                 (return handle (assoc-ref v "appPassword"))))
        (errorf "No entry for handle ~s is found in: ~a"
                handle settings-file))
      (let ([handle (assoc-ref (~ json 0) "handle")]
            [pass   (assoc-ref (~ json 0) "appPassword")])
        (if (and handle pass)
          (values handle pass)
          (errorf "Invalid entry in: ~a" settings-file))))))

;; API
;; Throws a <bsky-error> condition
(define (bsky-error code hdrs body)
  (define payload
    (cond [(#/^application\/json/i (rfc822-header-ref hdrs "content-type" ""))
           (parse-json-string body)]
          [else body]))
  (error <bsky-error>
         :status-code code :http-headers hdrs :payload payload
         :message (if (list? payload)
                    (assoc-ref payload "message")
                    body)))

;; API
;; For convenience
(define (bsky-check-status! code hdrs body)
  (unless (equal? code "200")
    (bsky-error code hdrs body)))

;; internal
(define (bsky-post-json bsky path payload)
  (assume-type bsky (<?> <bsky-session>)) ; #f for making session
  (receive (code hdrs body)
      (http-post *endpoint-host* path
                 (construct-json-string payload)
                 :content-type "application/json"
                 :secure #t
                 :authorization (and bsky
                                     #"Bearer ~(~ bsky'access-jwt)"))
    (bsky-check-status! code hdrs body)
    (parse-json-string body)))

;; internal
;; convert the result json (as a record) to <bsky-record>
(define (json->record json)
  (make <bsky-record>
    :uri (assoc-ref json "uri")
    :cid (assoc-ref json "cid")
    :original-json json))

;; time can be <time> or unix seconds
(define (fmt-time time)
  (assume-type time (</> <integer> <time>))
  (let* ([t (if (integer? time) (make-time 'time-utc 0 time) time)]
         [d (time-utc->date t 0)])
    (date->string d "~4")))

;; API
(define (make-bsky-session :optional (handle #f) (app-password #f))
  (receive (handle app-password)
      (if (and handle app-password)
        (values handle app-password)
        (bsky-load-settings handle))
    (let1 session (bsky-post-json #f "/xrpc/com.atproto.server.createSession"
                                  `(("identifier" . ,handle)
                                    ("password" . ,app-password)))
      (make <bsky-session>
        :handle handle
        :did (assoc-ref session "did")
        :access-jwt (assoc-ref session "accessJwt")
        :refresh-jwt (assoc-ref session "refreshJwt")))))

;; API
;; Simple text posting
(define (bsky-post-text bsky text :key (created-at (current-time))
                                       (langs '()))
  (define post
    `(("$type" . "app.bsky.feed.post")
      ("text"  . ,text)
      ("createdAt" . ,(fmt-time created-at))))

  (assume-type bsky <bsky-session>)

  (json->record
   (bsky-post-json bsky "/xrpc/com.atproto.repo.createRecord"
                   `(("repo" . ,(~ bsky'did))
                     ("collection" . "app.bsky.feed.post")
                     ("record" . ,post)))))
