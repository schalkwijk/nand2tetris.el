;;; nand2tetris-jack.el --- Major mode for Jack files in the nand2tetris course
;;; https://www.coursera.org/course/nand2tetris1

;; Keywords: nand2tetris, hdl, jack
;; Version: 1.0.0
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:

;; Useful functions to make following the Nand2Tetris course easier.
;; See: https://www.coursera.org/course/nand2tetris1

;;; Code:
(require 'cc-mode)

;; flycheck
(flycheck-define-checker nand2tetris-jack
  "A syntax-checker for the nand2tetris jack language"
  :command ("JackCompiler.sh" source)
  :error-patterns
  ((error line-start "In " (file-name) " (line " line "): " (message) line-end))
  :modes nand2tetris-jack-mode)

;; font-lock
(setq nand2tetris-jack-font-lock-keywords
  `(;;Keywords
    (,(rx symbol-start (group (or "class" "method" "field" "constructor" "function")) symbol-end
          (1+ space) (group (1+ word)) (group (zero-or-more (1+ space) (or (1+ word)))))
     (1 font-lock-constant-face)
     (2 font-lock-type-face)
     (3 font-lock-variable-name-face))

    (,(rx symbol-start (group (or "//") (1+ (or space word)) line-end))
     (1 font-lock-comment-face))

    (,(rx symbol-start (group (or "/*") (1+ anything) "*/" line-end))
     (1 font-lock-comment-face))

    (,(rx symbol-start (group (or "if" "while" "do" "var" "let" "return") symbol-end))
     (1 font-lock-constant-face))

    (,(rx symbol-start (group (or "this" "true" "false") symbol-end))
     (1 font-lock-warning-face))

    (,(rx (or "var" "let") (1+ space) (group (1+ (or word))))
     (1 font-lock-variable-name-face))

    ;; function declarations
    (,(rx (not (any "do")) space (group (1+ (or word ?_)))
          "(" (* (or word space)) ")")
     (1 font-lock-function-name-face))))


;; major-mode here
(define-derived-mode nand2tetris-jack-mode prog-mode
  "nand2tetris-jack"
  "Major mode for editing Jack files for the Nand2Tetris course"

  ;; gimme all your indentation, c-mode
  (c-init-language-vars-for 'c-mode)
  (c-common-init 'c-mode)

  ;; flycheck configuration
  (add-to-list 'flycheck-checkers 'nand2tetris-jack)
  (add-to-list 'flycheck-global-modes 'nand2tetris-jack-mode)
  ;; ugly hack to get flycheck to find the compiler executable
  (add-to-list 'exec-path nand2tetris-tools-dir)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       `(,nand2tetris-jack-font-lock-keywords nil nil nil nil)))

(provide 'nand2tetris-jack)
