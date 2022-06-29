;;; winlay-test.el --- Tests for winlay

(require 'winlay)

(ert-deftest test-remove-xdotool-debug-output()
  (let ((xdotool-output '("command: search" "71303212" "71303213")))
    (should (equal '("71303212" "71303213") (drop-xdotool-debug-output xdotool-output)))))

(ert-deftest test-extract-property-list()
  (let* ((xdotool-shell-output '("WINDOW=88094637" "X=1922" "Y=28" "WIDTH=1276" "HEIGHT=1373" "SCREEN=0"))
         (expected-property-list '(window 88094637 x 1922 y 28 width 1276 height 1373 screen 0)))
    (let ((actual-property-list (winlay--extract-property-list xdotool-shell-output)))
      (should (equal expected-property-list actual-property-list)))))

(ert-deftest test-extract-property-list-without-debug-output()
  (let* ((xdotool-shell-output '("command: getwindowgeometry" "WINDOW=88094637" "X=1922" "Y=28" "WIDTH=1276" "HEIGHT=1373" "SCREEN=0"))
         (expected-property-list '(window 88094637 x 1922 y 28 width 1276 height 1373 screen 0)))
    (let ((actual-property-list (winlay--extract-property-list xdotool-shell-output)))
      (should (equal expected-property-list actual-property-list)))))

(ert-deftest test-xwindow-seems-snapped()
  (should (winlay--seems-snapped '(width 1276 height 1373) '(width 1280 height 1440))))

(ert-deftest test-xwindow-does-not-seem-snapped()
  (should (not (winlay--seems-snapped '(width 2560 height 1375) '(width 1280 height 1440)))))

(ert-deftest test-snapped-size-is-half-display-width()
  (should (equal '(width 1280 height 1440) (winlay--snapped-size '(width 2560 height 1440)))))

(ert-deftest test-tile-and-focus-when-both-xwindows-are-maximized()
  (let ((xwindow-geometry '(window 90177539 width 2560 height 1375))
        (other-xwindow-geometry '(window 88094637 width 2560 height 1375))
        (display-geometry '(width 2560 height 1440))
        (expected-commands (list (cons #'snap-window-to-right 90177539)
                                 (cons #'snap-window-to-left 88094637)
                                 (cons #'winlay-move-focus 88094637))))
    (let ((actual-commands
           (winlay--get-tile-and-focus-commands xwindow-geometry
                                                other-xwindow-geometry
                                                display-geometry)))
      (should (equal expected-commands actual-commands)))))

(ert-deftest test-tile-and-focus-when-only-one-xwindow-is-maximized()
  (let ((xwindow-geometry '(window 90177539 width 2560 height 1375))
        (other-xwindow-geometry '(window 88094637 width 1276 height 1373))
        (display-geometry '(width 2560 height 1440))
        (expected-commands (list (cons #'snap-window-to-right 90177539)
                                 (cons #'winlay-move-focus 88094637))))
    (let ((actual-commands
           (winlay--get-tile-and-focus-commands xwindow-geometry
                                                other-xwindow-geometry
                                                display-geometry)))
      (should (equal expected-commands actual-commands)))))
