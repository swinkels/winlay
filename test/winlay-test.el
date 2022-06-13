;;; winlay-test.el --- Tests for winlay

(require 'winlay)

(ert-deftest test-remove-xdotool-debug-output()
  (let ((xdotool-output '("command: search" "71303212" "71303213")))
    (should (equal '("71303212" "71303213") (drop-xdotool-debug-output xdotool-output)))))
