;;;
;;; Test app.bsky
;;;

(use gauche.test)

(test-start "app.bsky")
(use app.bsky)
(test-module 'app.bsky)

(test-section "facets")

(let ()
  ;; test internal routine
  (define %find-links (with-module app.bsky find-links))

  (test* "link facets"
         '()
         (%find-links "alphalpha"))

  (test* "link facets"
         '()
         (%find-links "http:abc"))

  (test* "link facets"
         '()
         (%find-links "http://abc"))

  (test* "link facets"
         '(("https://example.com/foo" 0 23))
         (%find-links "https://example.com/foo"))

  (test* "link facets"
         '(("https://example.com/foo" 8 31))
         (%find-links "link to https://example.com/foo ok?"))

  (test* "link facets"
         '(("https://example.com/foo" 8 31)
           ("https://example.net/foo/bar" 42 69))
         (%find-links "参照: https://example.com/foo および https://example.net/foo/bar"))

  (test* "link facets"
         '(("https://example.com/foo?abc#def" 4 35))
         (%find-links "See https://example.com/foo?abc#def ok?"))

  (test* "link facets"
         '(("https://example.com/#def" 4 28))
         (%find-links "See https://example.com/#def ok?"))

  (test* "link facets"
         '(("http://www.example.com/foo/bar/baz?(something)" 4 50))
         (%find-links "See http://www.example.com/foo/bar/baz?(something) ok?"))
  )

(let ()
  ;; test internal routine
  (define %scan-segmented-text (with-module app.bsky scan-segmented-text))

  (test* "segmented text -> facets"
         '("abc" #())
         ($ values->list $ %scan-segmented-text
            '("abc")))
  (test* "segmented text -> facets"
         '("abcdef" #())
         ($ values->list $ %scan-segmented-text
            '("abc" "def")))
  (test* "segmented text -> facets"
         '("abcdefghi"
           #((("index" ("byteStart" . 3) ("byteEnd" . 6))
              ("features" . #((("$type" . "app.bsky.richtext.facet#link")
                               ("uri" . "http://example.com")))))))

         ($ values->list $ %scan-segmented-text
            '("abc" ("def" :link "http://example.com") "ghi")))
  (test* "segmented text -> facets"
         '("abcdefとghi"
           #((("index" ("byteStart" . 3) ("byteEnd" . 6))
              ("features" . #((("$type" . "app.bsky.richtext.facet#link")
                               ("uri" . "http://example.com")))))
             (("index" ("byteStart" . 9) ("byteEnd" . 12))
              ("features" . #((("$type" . "app.bsky.richtext.facet#link")
                               ("uri" . "http://example.net/foo")))))))
         ($ values->list $ %scan-segmented-text
            '("abc"
              ("def" :link "http://example.com")
              "と"
              ("ghi" :link "http://example.net/foo"))))
  )

(test-end :exit-on-failure #t)
