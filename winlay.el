;;; winlay.el --- Provide commands to layout Emacs windows and X windows

;; Version: 0.0.0
;; Package-Requires: (helm)

(provide 'winlay)

(setq winlay-pull-up-buffer-name-0 "*gud-pdb*")

(defvar winlay--other-xwindow nil "ID of X window to tile with")

(defun winlay-pull-up-buffer-and-tile-xwindows (ask-for-other-xwindow)
  (interactive "P")
  (let ((emacs-window-id (get-current-window-id)))
    (when (winlay--pull-up-buffer-by-name winlay-pull-up-buffer-name-0)
      (when (or ask-for-other-xwindow (not winlay--other-xwindow))
        (setq winlay--other-xwindow (winlay--ask-for-other-xwindow)))
      (winlay-tile-xwindows winlay--other-xwindow emacs-window-id)
      (winlay-move-focus winlay--other-xwindow))))

(defun winlay--pull-up-buffer-by-name (buffer-name)
  (interactive)
  (let ((pdb-buffer (get-buffer buffer-name)))
    (if (not pdb-buffer)
        (message (concat "Unable to show server buffer: no buffer named " buffer-name " found"))
      (delete-other-windows)
      (split-window-vertically -18)
      (other-window 1)
      (switch-to-buffer pdb-buffer)
      (goto-char (point-max))
      (other-window 1))
    pdb-buffer))

(defun winlay-tile-xwindows (window-to-left window-to-right)
  (snap-window-to-right window-to-right)
  (snap-window-to-left window-to-left))

(defun winlay--ask-for-other-xwindow ()
  (let* ((xwindow-ids (drop-xdotool-debug-output (process-lines "xdotool" "search" "--name" ".* Mozilla Firefox")))
         (xwindow-names (mapcar 'get-window-name xwindow-ids)))
    (cdr (assoc (helm-comp-read "Select window to tile: " xwindow-names)
                (mapcar* #'cons xwindow-names xwindow-ids)))))

(defun drop-xdotool-debug-output (xdotool-output)
  (seq-remove '(lambda (line) (string-prefix-p "command: " line)) xdotool-output))

(defun get-window-name (window-id)
  (car (drop-xdotool-debug-output (process-lines "xdotool" "getwindowname" window-id))))

(defun get-current-window-id ()
  (car (process-lines "xdotool" "getactivewindow")))

(defun snap-window-to-left (window-id)
  (call-process "xdotool" nil nil nil "key" "--window" window-id "Super_L+Left"))

(defun snap-window-to-right (window-id)
  (call-process "xdotool" nil nil nil "key" "--window" window-id "Super_L+Right"))

(defun winlay-move-focus (xwindow)
  (call-process "xdotool" nil nil nil "windowactivate" xwindow))

(global-set-key (kbd "<f5>") 'winlay-pull-up-buffer-and-tile-xwindows)

;;; winlay.el ends here
