;; lifted heavily from https://github.com/magnars/.emacs.d/blob/master/site-lisp/nand2teris-autotest.el

(defun nand2teris/clear-buffer ()
  (delete-region (point-min) (point-max)))

(defun nand2tetris/get-test-file (buffer)
  "get the test file for buffer."
  (let ((test-file (concat
                    (file-name-sans-extension
                     (with-current-buffer buffer
                       (buffer-file-name))) ".tst")))
    (unless (file-exists-p test-file)
      (error "could not find the test file for %s" (buffer-name)))
    test-file))

(defun nand2teris/run-expectations (current-buffer)
  "Run `HardwareSimulator.sh' on current tst file."
  (interactive)
  (shell-command (concat nand2tetris-hardware-simulator " " (nand2tetris/get-test-file current-buffer)) (current-buffer)))

(defun nand2teris/ansi-colorize-buffer ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun nand2teris/fit-window-snuggly (max-height)
  (window-resize nil (- (max 4 (min max-height
                                    (1+ (line-number-at-pos (point-max)))))
                        (window-height))))

(defun nand2teris/recenter-bottom ()
  (recenter (- -1 (min (max 0 scroll-margin)
                       (truncate (/ (window-body-height) 4.0))))))

(defvar nand2teris/buffer "*nand2teris-autotest*")

(defmacro with-nand2teris-buffer (&rest body)
  `(let ((current (current-buffer))
         (window (get-buffer-window "*nand2teris-autotest*")))
     (if window
         (select-window window)
       (let ((window (split-window-vertically -4)))
         (select-window window)
         (switch-to-buffer nand2teris/buffer)
         (set-window-dedicated-p window t)))
     ,@body
     (switch-to-buffer-other-window current)))

(defun nand2teris/run-tests ()
  (let ((current (current-buffer)))
    (with-nand2teris-buffer
    (nand2teris/clear-buffer)
    (nand2teris/run-expectations current)
    (nand2teris/ansi-colorize-buffer)
    (nand2teris/fit-window-snuggly 10)
    (goto-char (point-min))
    (if (looking-at "Error refreshing environment")
        (search-forward "cause")
      (goto-char (point-max))
      (nand2teris/recenter-bottom)))))

(defun nand2teris/run-tests-hook ()
  (when (and (derived-mode-p 'nand2tetris-mode)
             (s-ends-with? ".hdl" (buffer-file-name)))
    (nand2teris/run-tests)))

(defun nand2teris-autotest-init ()
  (interactive)
  (add-hook 'after-save-hook 'nand2teris/run-tests-hook))

(defun nand2teris-autotest-stop-it ()
  (interactive)
  (remove-hook 'after-save-hook 'nand2teris/run-tests-hook)
  (kill-buffer nand2teris/buffer))

(provide 'nand2tetris-autotest)
