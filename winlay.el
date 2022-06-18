;;; winlay.el --- Provide commands to layout Emacs windows and X windows

;; Version: 0.0.0
;; Package-Requires: (helm)

(provide 'winlay)

(defvar winlay--other-xwindow nil "ID of X window to tile with")

(defun winlay-pull-up-buffer-and-tile-xwindows (ask-for-other-xwindow)
  (interactive "P")
  (when (winlay--pull-up-buffer)
    (when (or ask-for-other-xwindow (not winlay--other-xwindow))
      (setq winlay--other-xwindow (winlay--ask-for-other-xwindow)))
    (winlay-tile-xwindows winlay--other-xwindow)
    (winlay-move-focus winlay--other-xwindow)))

(defun winlay--pull-up-buffer ()
  (interactive)
  (let ((pdb-buffer (get-buffer "*gud-pdb*")))
    (if (not pdb-buffer)
        (message "Unable to show server buffer: no buffer named *gud-pdb* found")
      (delete-other-windows)
      (split-window-vertically -18)
      (other-window 1)
      (switch-to-buffer pdb-buffer)
      (goto-char (point-max))
      (other-window 1))
    pdb-buffer))

(defun winlay-tile-xwindows (other-xwindow)
  (snap-current-terminal-to-right)
  (snap-window-to-left other-xwindow))

(defun winlay--ask-for-other-xwindow ()
  (let* ((xwindow-ids (drop-xdotool-debug-output (process-lines "xdotool" "search" "--name" ".* Mozilla Firefox")))
         (xwindow-names (mapcar 'get-window-name xwindow-ids)))
    (cdr (assoc (helm-comp-read "Select window to tile: " xwindow-names)
                (mapcar* #'cons xwindow-names xwindow-ids)))))

(defun drop-xdotool-debug-output (xdotool-output)
  (seq-remove '(lambda (line) (string-prefix-p "command: " line)) xdotool-output))

(defun get-window-name (window-id)
  (car (drop-xdotool-debug-output (process-lines "xdotool" "getwindowname" window-id))))

(defun snap-current-terminal-to-right ()
  ;; this only works if the current terminal is the active window
  (call-process "xdotool" nil nil nil "key" "Super_L+Right"))

(defun snap-window-to-left (window-id)
  (call-process "xdotool" nil nil nil "key" "--window" window-id "Super_L+Left"))

(defun winlay-move-focus (xwindow)
  (call-process "xdotool" nil nil nil "windowactivate" xwindow))

(global-set-key (kbd "<f5>") 'winlay-pull-up-buffer-and-tile-xwindows)

;;; winlay.el ends here
