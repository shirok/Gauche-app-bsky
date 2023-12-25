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


(test-end :exit-on-failure #t)
