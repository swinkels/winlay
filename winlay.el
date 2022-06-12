;; winlay.el --- Provide commands to layout Emacs windows and X windows

(defun show-server ()
  (interactive)
  (let ((pdb-buffer (get-buffer "*gud-pdb*")))
    (if (not pdb-buffer)
        (message "Unable to show server buffer: no buffer named *gud-pdb* found")
      (delete-other-windows)
      (split-window-vertically -18)
      (other-window 1)
      (switch-to-buffer pdb-buffer)
      (end-of-buffer)
      (other-window 1))
    pdb-buffer))

(defun show-server-and-layout-xwindows ()
  (interactive)
  (when (show-server)
    (layout-xwindows "75497516")))


(defun layout-xwindows (other-window-id)
  (snap-current-terminal-to-right)
  (snap-window-to-left other-window-id)
  (move-focus-to-window other-window-id))

(defun snap-current-terminal-to-right ()
  (call-process "xdotool" nil nil nil "getactivewindow" "windowstate" "--remove" "MAXIMIZED_HORZ" "windowstate" "--remove" "MAXIMIZED-HORZ" "windowsize" "1274" "1374" "windowmove" "3204" "0"))

(defun snap-window-to-left (window-id)
  (call-process "xdotool" nil nil nil "windowstate" "--remove" "MAXIMIZED_HORZ" window-id "windowsize" window-id "1280" "1373" "windowmove" window-id "1920" "0" "windowactivate" window-id))

(defun move-focus-to-window (window-id)
  (call-process "xdotool" nil nil nil "windowactivate" window-id))

(global-set-key (kbd "<f5>") 'show-server-and-layout-xwindows)

(provide 'winlay)

;; winlay.el --- Provide command to select and activate tox virtualenv
