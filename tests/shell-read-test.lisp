$(export XXX1 hello)
(test::assert-equal "hello" $XXX1)
$(export XXX2 test:$XXX1)
(test::assert-equal "test:hello" $XXX2)
$(export XXX2 $XXX1:test)
(test::assert-equal "hello:test" $XXX2)
$(export XXX3 $XXX1:test-$XXX2)
(test::assert-equal "hello:test-hello:test" $XXX3)
$(export XXX3 $XXX1:test-${XXX2}xxx)
(test::assert-equal "hello:test-hello:testxxx" $XXX3)
$(export XXX3 $XXX1:test-\${XXX2}xxx)
(test::assert-equal "hello:test-\${XXX2}xxx" $XXX3)
$(export XXX3 $XXX1:test-\$XXX2xxx)
(test::assert-equal "hello:test-\$XXX2xxx" $XXX3)
$(export XXX3 $XXX1)
(test::assert-equal "hello" $XXX3)

$(export YYY $(echo "1\n20\n10" | grep 1 | grep 0))
(test::assert-equal "10\n" $YYY)
(def yyy (str $(echo "1\n20\n10" | grep 1 | grep 0)))
(test::assert-equal "10\n" $yyy)
