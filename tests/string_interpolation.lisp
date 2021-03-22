(ns-import 'test)

(def stri1 "val1")
(def stri2 200)

(assert-equal "val1" "$stri1")
(assert-equal "200" "$stri2")
(assert-equal "val1 " "$stri1 ")
(assert-equal "200 " "$stri2 ")
(assert-equal "val1" "${stri1}")
(assert-equal "200" "${stri2}")

(let ((stri1 "let1")(stri2 111))
    (assert-equal "let1" "$stri1")
    (assert-equal "111" "$stri2")
    (assert-equal "let1" "${stri1}")
    (assert-equal "let1:111" "${stri1}:${stri2}")
    (assert-equal "111" "${stri2}"))

(assert-equal "val1" "$stri1")
(assert-equal "200" "$stri2")
(assert-equal "val1" "${stri1}")
(assert-equal "200" "${stri2}")


(export 'STRI_TEST "exp1")

(assert-equal "exp1" "$STRI_TEST")
(assert-equal "exp1" "${STRI_TEST}")
(assert-equal "exp1:val1" "${STRI_TEST}:$stri1")
(assert-equal "exp1Sval1" "${STRI_TEST}S${stri1}")

(assert-equal "echo1" "$(echo echo1)")
(assert-equal "print1" "$(str print1)")
(assert-equal "exp1Xval1:echo1" "${STRI_TEST}X${stri1}:$(echo echo1)")
(assert-equal "exp1Xval1:print1" "${STRI_TEST}X${stri1}:$(str print1)")
