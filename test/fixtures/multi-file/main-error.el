;; Main file with type error
;; Should fail when processed after prelude.el because
;; double expects an int but gets a string

;; Type error: double expects int, got string
(double "not a number")
