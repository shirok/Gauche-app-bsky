;;;
;;; Test app.bsky
;;;

(use gauche.test)

(test-start "app.bsky")
(use app.bsky)
(test-module 'app.bsky)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-gauche_app_bsky" "gauche_app_bsky is working"
       (test-gauche_app_bsky))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
