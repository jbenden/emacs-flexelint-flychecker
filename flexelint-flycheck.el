;;; flexelint-flycheck.el --- Flexelint flycheck checker

;; Author: Joseph Benden <joe@benden.us>
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:
;; Flycheck checker for C/C++ using the Flexelint/PC-Lint
;; tool.
;;
;; Add the following lines to your .emacs:
;;
;;   (add-to-list 'load-path "flexelint-flycheck")
;;   (require 'flexelint-flycheck)

;;; Code:

(eval-when-compile
  (require 'flycheck)
  (require 'rx))

(defgroup flexelint nil
  "Support for Flycheck in C/C++ via flexelint"
  :group 'flycheck)

(defcustom c-c++-flexelint-config-path ".flexelint.lnt"
  "Default name of a project specific Flexelint configuration file."
  :type 'string
  :group 'flycheck)

(defcustom flycheck-c-c++-flexelint-codecheck-executable "flexelint"
  "Flexelint/PC-Lint executable."
  :type 'string
  :group 'flycheck)

(defcustom c-c++-flexelint-header-args '("-e750" "-e751" "-e752" "-e753" "-e754" "-e1526" "-e1714")
  "Default set of arguments passed to Flexelint for header files."
  :type 'alist
  :group 'flycheck)

(flycheck-define-checker c-c++-flexelint-codecheck
  "A flychecker using the Flexelint static source code analyzer tool
from Gimpel.

See URL `http://www.gimpel.com/html/flex.htm'."
  :command ("flexelint"
            "-v"
            "-b"
            "-format=%f  %l %c  %t %n: %m"
            (eval (if c-c++-flexelint-config-path
                      `(,c-c++-flexelint-config-path)))
            "-hsFr_1"
            "-width(4096,0)"
            "-zero(400)"
            (eval (if (string-match "^\(h\\|H\\|hh\\|hpp\\|h\+\+\\|hxx\)$"
                                    (file-name-extension (buffer-file-name)))
                      `(,@c-c++-flexelint-header-args)))
            source-original)

  :error-parser c-c++//flexelint-error-parser

  :next-checkers ((info . c/c++-clang))

  :modes (c-mode c++-mode))

(define-error 'flexelint-parse-error
  "Raised when Flexelint is not able to parse an output line")

(defun c-c++//flexelint-parse-line (line)
  "Parses the Flexelint specific output one line at a time."
  (let ((line-regexp (rx
                      (and line-start
                           (submatch (one-or-more (not (any " "))))
                           "  "
                           (submatch (one-or-more (any digit)))
                           (zero-or-more
                            (and " " (submatch (one-or-more (any digit)))))
                           "  "
                           (submatch (or "Info" "Warning" "Error" "Note"))
                           " "
                           (submatch (one-or-more (any digit)))
                           ": "
                           (submatch (one-or-more not-newline)) line-end)))
        (exclude-regexp (rx
                         (and line-start
                              (submatch (or "During Specific Walk:"
                                            (and "  File " (one-or-more not-newline))
                                            ""))
                              line-end)))
        (level-alist '(("Info" . info)
                       ("Note" . info)
                       ("Warning" . warning)
                       ("Error" . error)))
        (current-error-plist))
    (progn
      (unless (string-match exclude-regexp line)
        ;; (message "flexelint regexp: %s ----- %s ----- %s" line-regexp (string-match line-regexp line) (match-string 4 line))
        (if (string-match line-regexp line)
            (progn ;; true
              (setq current-error-plist
                    (plist-put current-error-plist 'file-name
                               (match-string 1 line)))
              (setq current-error-plist
                    (plist-put current-error-plist 'line
                               (flycheck-string-to-number-safe
                                (match-string 2 line))))
              (setq current-error-plist
                    (plist-put current-error-plist 'column
                               (if (equal (length (match-string 3 line)) 0)
                                   0
                                 (flycheck-string-to-number-safe
                                  (match-string 3 line)))))
              (setq current-error-plist
                    (plist-put current-error-plist 'level
                               (cdr
                                (assoc
                                 (match-string 4 line)
                                 level-alist))))
              (setq current-error-plist
                    (plist-put current-error-plist 'code
                               (match-string 5 line)))
              (setq current-error-plist
                    (plist-put current-error-plist 'message
                               (match-string 6 line))))
          (progn
            (message "Flexelint parse error with line: %s" line)
            (signal 'flexelint-parse-error (format "Flexelint parser was unable to parse: %s" line))))
        current-error-plist))))

(defun c-c++//flexelint-error-parser (output checker buffer)
  "Returns parsed Flexelint error results for Flycheck checker."
  (let* ((file-lines (with-temp-buffer
                       (insert output)
                       (split-string (buffer-string) "\n" t)))
         (current-error-plist)
         (errors))
    (mapc (lambda (it)
            (let ((error-plist (c-c++//flexelint-parse-line it)))
              (if error-plist
                  (progn
                    ;; check if current error is applicable
                    (if (or (string= (plist-get error-plist 'code) "830")
                            (string= (plist-get error-plist 'code) "831"))
                        (progn
                          (setq current-error-plist (plist-put current-error-plist 'line (plist-get error-plist 'line)))
                          (setq current-error-plist (plist-put current-error-plist 'column (plist-get error-plist 'column)))
                          (setq error-plist nil)))
                    ;; if we have current-error-plist; output an error
                    (if current-error-plist
                        (push (flycheck-error-new
                               :buffer   buffer
                               :checker  checker
                               :filename (plist-get current-error-plist 'file-name)
                               :line     (plist-get current-error-plist 'line)
                               :column   (plist-get current-error-plist 'column)
                               :id       (plist-get current-error-plist 'code)
                               :message  (plist-get current-error-plist 'message)
                               :level    (plist-get current-error-plist 'level))
                              errors))
                    ;; set our current error
                    (setq current-error-plist error-plist))))) file-lines)
    ;; if we have current-error-plist left; output our final error
    (if current-error-plist
        (push (flycheck-error-new
               :buffer   buffer
               :checker  checker
               :filename (plist-get current-error-plist 'file-name)
               :line     (plist-get current-error-plist 'line)
               :column   (plist-get current-error-plist 'column)
               :id       (plist-get current-error-plist 'code)
               :message  (plist-get current-error-plist 'message)
               :level    (plist-get current-error-plist 'level))
              errors))
    errors))

(add-to-list 'flycheck-checkers 'c-c++-flexelint-codecheck)

(provide 'flexelint-flycheck)

;;; flexelint-flycheck.el ends here
