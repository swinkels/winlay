;;; winlay.el --- Provide commands to layout Emacs windows and X windows

;; Version: 0.0.0
;; Package-Requires: (helm)

(provide 'winlay)

;;; * Customization

(defgroup winlay nil
  "Layout Emacs windows and X Windows."
  :group 'convenience)

(defcustom winlay-name-of-buffer-to-pull-up "*gud-pdb*"
  "Name of Emacs buffer to pull up.
Clear this field if no buffer should be pulled-up."
  :type 'string :group 'winlay)

(defcustom winlay-pattern-of-other-xwindow-title ".* Mozilla Firefox"
  "Pattern of title of X window to tile with."
  :type 'string :group 'winlay)

;;; * Internal variables

;; ID of current Emacs X window
(setq winlay--emacs-xwindow nil)

;; ID of X window to tile with
(setq winlay--other-xwindow nil)

;;; * Public functions

(defun winlay-pull-up-buffer-and-tile-xwindows (ask-for-other-xwindow)
  (interactive "P")
  (when (or (= (length winlay-name-of-buffer-to-pull-up) 0)
            (winlay--pull-up-buffer-by-name winlay-name-of-buffer-to-pull-up))
    (unless winlay--emacs-xwindow
      (setq winlay--emacs-xwindow (get-current-xwindow)))
    (unless (and winlay--other-xwindow (not ask-for-other-xwindow))
      (setq winlay--other-xwindow (winlay--ask-for-other-xwindow)))
    (winlay--execute-xwindow-commands
     (winlay--get-tile-and-focus-commands
      (winlay--get-xwindow-geometry winlay--emacs-xwindow)
      (winlay--get-xwindow-geometry winlay--other-xwindow)
      (winlay--get-display-geometry)))))

;;; * Private functions

(defun winlay--get-tile-and-focus-commands (xwindow-geometry
                                            other-xwindow-geometry
                                            display-geometry)
  (let ((xwindow-id (plist-get xwindow-geometry 'window))
        (other-xwindow-id (plist-get other-xwindow-geometry 'window))
        (snapped-xwindow-size (winlay--snapped-size display-geometry)))
    (seq-filter (lambda (element) element)
                (list
                 (unless (winlay--seems-snapped xwindow-geometry
                                                snapped-xwindow-size)
                   (cons #'snap-window-to-right xwindow-id))
                 (unless (winlay--seems-snapped other-xwindow-geometry
                                                snapped-xwindow-size)
                   (cons #'snap-window-to-left other-xwindow-id))
                 (cons #'winlay-move-focus other-xwindow-id)))))

(defun winlay--execute-xwindow-commands (xwindow-commands)
  (mapc (lambda (command) (funcall (car command) (cdr command))) xwindow-commands))

(defun winlay--pull-up-buffer-by-name (buffer-name)
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (message (concat "Unable to show buffer: no buffer named " buffer-name " found"))
      (delete-other-windows)
      (split-window-vertically -18)
      (other-window 1)
      (switch-to-buffer buffer)
      (goto-char (point-max))
      (other-window 1))
    buffer))

(defun winlay-tile-xwindows (window-to-left window-to-right)
  (snap-window-to-right window-to-right)
  (snap-window-to-left window-to-left))

(defun winlay--ask-for-other-xwindow ()
  (let* ((xwindow-ids
          (mapcar 'string-to-number
                  (drop-xdotool-debug-output
                   (process-lines "xdotool"
                                  "search"
                                  "--name"
                                  winlay-pattern-of-other-xwindow-title))))
         (xwindow-names (mapcar 'get-window-name xwindow-ids)))
    (cdr (assoc (helm-comp-read "Select window to tile: " xwindow-names)
                (mapcar* #'cons xwindow-names xwindow-ids)))))

(defun drop-xdotool-debug-output (xdotool-output)
  (seq-remove '(lambda (line) (string-prefix-p "command: " line)) xdotool-output))

(defun get-window-name (window-id)
  (car (drop-xdotool-debug-output (process-lines "xdotool" "getwindowname" (number-to-string window-id)))))

(defun get-current-xwindow ()
  (string-to-number (car (drop-xdotool-debug-output (process-lines "xdotool" "getactivewindow")))))

(defun snap-window-to-left (window-id)
  (call-process "xdotool" nil nil nil "windowactivate" "--sync" (number-to-string window-id) "key" "Super_L+Left"))

(defun snap-window-to-right (window-id)
  (call-process "xdotool" nil nil nil "windowactivate" "--sync" (number-to-string window-id) "key" "Super_L+Right"))

(defun winlay-move-focus (xwindow)
  (call-process "xdotool" nil nil nil "windowactivate" (number-to-string xwindow)))

(defun winlay--get-xwindow-geometry (xwindow-id)
  (winlay--extract-property-list
   (process-lines "xdotool"
                  "getwindowgeometry"
                  "--shell"
                  (number-to-string xwindow-id))))

(defun winlay--get-display-geometry ()
  (winlay--extract-property-list
   (process-lines "xdotool"
                  "getdisplaygeometry"
                  "--shell")))

(defun winlay--extract-property-list (xdotool-shell-output)
  (flatten-tree (mapcar #'winlay--extract-property
                        (drop-xdotool-debug-output xdotool-shell-output))))

(defun winlay--extract-property (line)
  (let ((property-name (intern (downcase (car (split-string line "=")))))
        (property-value (string-to-number (car (cdr (split-string line "="))))))
    (list property-name property-value)))

(defun winlay--seems-snapped (xwindow-size xwindow-snapped-size)
  (let ((max-rel-diff 0.10))
    (and (<= (winlay--rel-diff xwindow-snapped-size xwindow-size 'width)
             max-rel-diff)
         (<= (winlay--rel-diff xwindow-snapped-size xwindow-size 'height)
             max-rel-diff))))

(defun winlay--rel-diff (property-list-a property-list-b symbol)
  (/ (abs (- (plist-get property-list-a symbol) (plist-get property-list-b symbol)))
     (float (plist-get property-list-a symbol))))

(defun winlay--snapped-size (display-geometry)
  (list 'width
        (/ (plist-get display-geometry 'width) 2)
        'height
        (plist-get display-geometry 'height)))

(global-set-key (kbd "<f5>") 'winlay-pull-up-buffer-and-tile-xwindows)

;;; winlay.el ends here
