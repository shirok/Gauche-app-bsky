;;;
;;; Test app.bsky
;;;

(use gauche.test)

(test-start "app.bsky")
(use app.bsky)
(test-module 'app.bsky)


(test-end :exit-on-failure #t)
