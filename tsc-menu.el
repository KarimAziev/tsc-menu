;;; tsc-menu.el --- Transient- interface for TypeScript compiler -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/tsc-menu
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (transient "0.5.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `tsc-menu' provides a transient interface for interacting with the TypeScript
;; compiler (tsc).

;; Usage:

;; `M-x tsc-menu' - This will bring up the transient interface, where you can
;; select compiler options and arguments. Once you have configured the command
;; to your liking, you can execute it directly from the interface.

;; The menu is dynamically generated from the output of `tsc --all`, ensuring
;; that all available compiler options are presented to the user. Options are
;; grouped logically, and the interface supports multi-value options, such as
;; specifying multiple files.

;;; Code:

(require 'transient)


(defcustom tsc-menu-project--arguments-props '(("--project" "-p"
                                                "project"
                                                "--project "
                                                :class transient-files
                                                :reader
                                                transient-read-existing-file)
                                               ("--outFile"
                                                "-of"
                                                "outFile"
                                                "--outFile "
                                                :class transient-files
                                                :reader transient-read-file)
                                               ("--outDir"
                                                "-od"
                                                "outDir"
                                                "--outDir "
                                                :class transient-files
                                                :reader transient-read-directory))
  "Doc."
  :group 'tsc-menu
  :type '(alist
          :key-type string
          :value-type (list
                       string
                       string
                       string
                       (plist :inline t))))

(eval-and-compile
  (defun tsc-menu--expand (init-fn)
    "Expand the given macro and return the expanded form.
Argument INIT-FN is the macro to be expanded."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defun tsc-menu--key-builder-generate-shortcut-key (word key-len shortcuts
                                                         all-keys)
  "Generate a unique shortcut key.

Argument WORD is a string from which the shortcut key is generated.

Argument KEY-LEN is an integer that specifies the maximum length of the
generated shortcut key.

Argument SHORTCUTS is a list of strings representing existing shortcuts to avoid
conflicts.

Argument ALL-KEYS is a list of strings representing all possible keys to ensure
uniqueness."
  (let ((short
         (downcase
          (substring-no-properties word 0
                                   (min key-len
                                        (length word))))))
    (setq short (if (string-match-p short "[a-z]")
                    (replace-regexp-in-string "^[^a-z]+" "" short)
                  short))
    (setq short
          (seq-find
           (lambda (it)
             (not
              (seq-find
               (apply-partially
                #'string-prefix-p it)
               shortcuts)))
           (append
            (tsc-menu--key-builder-get-all-key-strategies
             word
             key-len)
            (let ((random-variants
                   (tsc-menu--key-builder-get-alphabet)))
              (or (seq-remove (lambda (key)
                                (seq-find (apply-partially
                                           #'string-prefix-p
                                           (downcase key))
                                          all-keys))
                              random-variants)
                  random-variants)))))
    (while (and
            (< (length short) key-len))
      (setq short (concat (or short "")
                          (number-to-string (random 10)))))
    short))

(defun tsc-menu--key-splitted-variants (word len separator)
  "Generate unique key variants from a word.

Argument WORD is a string to be split into variants.

Argument LEN is an integer representing the length of each variant to be
generated.

Argument SEPARATOR is a string used as the delimiter for splitting WORD."
  (when-let* ((slen
               (when (> len 1)
                 (1- len)))
              (splitted (mapcar (apply-partially
                                 #'tsc-menu--key-builder-safe-substring 1)
                                (seq-drop (split-string word separator t)
                                          1)))
              (first-letter (tsc-menu--key-builder-safe-substring 1 word)))
    (seq-uniq
     (append (reverse (mapcar (lambda (it)
                                (unless (> slen (length it))
                                  (concat first-letter
                                          (string-join it ""))))
                              (seq-split splitted slen)))
             (list
              (mapconcat (lambda (_) first-letter)
                         (number-sequence 0 slen)
                         ""))))))

(defun tsc-menu--key-builder-shared-start (s1 s2)
  "Find common string prefix between S1 and S2.

Argument S1 is a string to compare.

Argument S2 is another string to compare against S1."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun tsc-menu--key-builder-capitalize-variants (word)
  "Generate capitalized variants of a string.

Argument WORD is a string to be split and capitalized in various ways."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join
                                              (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join
                                            (seq-drop parts (1+ i))
                                            "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun tsc-menu--key-builder-safe-substring (len word)
  "Truncate WORD to LEN characters, preserving properties.

Argument LEN is an integer representing the maximum length of the substring.

Argument WORD is a string from which the substring will be extracted."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun tsc-menu--key-builder-get-all-key-strategies (word len)
  "Generate unique key strategies from a word.

Argument WORD is a string to be processed for key strategies.

Argument LEN is an integer specifying the desired length of the key strategies."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short
                                           (number-to-string (random 10)))))
                     (tsc-menu--key-builder-safe-substring len short)))
         (vars
          (mapcar finalize (tsc-menu--key-builder-capitalize-variants
                            (tsc-menu--key-builder-safe-substring
                             len
                             word)))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (tsc-menu--key-builder-shared-start (downcase word)
                                                            (downcase it))))))
     #'>
     (seq-uniq (delq nil
                     (append
                      vars
                      (tsc-menu--key-splitted-variants word len "^[a-z]")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'tsc-menu--key-builder-safe-substring
                                             n)
                                            parts "")))
                       (number-sequence 1 (min len parts-len)))
                      (tsc-menu--key-splitted-variants word len "")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'tsc-menu--key-builder-safe-substring
                                             n)
                                            (reverse parts) "")))
                       (number-sequence 1 (min len parts-len)))))))))

(defmacro tsc-menu--cond (&rest pairs)
  "Transform conditions into a lambda function.

Remaining arguments PAIRS are lists where each list contains a condition and a
result form."
  (declare (pure t)
           (indent defun)
           (side-effect-free error-free))
  (setq pairs (mapcar (lambda (it)
                        (if (listp it)
                            (apply #'vector it)
                          it))
                      pairs))
  (let ((args (make-symbol "arguments")))
    `(lambda (&rest ,args)
       (cond ,@(mapcar (lambda (v)
                         (list (if (eq (aref v 0) t) t
                                `(apply ,@(tsc-menu--expand (aref v 0)) ,args))
                          `(apply ,@(tsc-menu--expand (aref v 1)) ,args)))
                pairs)))))

(defmacro tsc-menu--compose (&rest functions)
  "Compose FUNCTIONS into a single callable chain.

Remaining arguments FUNCTIONS are Lisp functions to be composed."
  (declare (debug t)
           (pure t)
           (indent defun)
           (side-effect-free t))
  (setq functions (reverse functions))
  (let ((args-var (make-symbol "arguments")))
    `(lambda (&rest ,args-var)
       ,@(let ((init-fn (pop functions)))
          (list
           (seq-reduce
            (lambda (acc fn)
              `(funcall ,@(tsc-menu--expand fn) ,acc))
            functions
            `(apply ,@(tsc-menu--expand init-fn) ,args-var)))))))

(defun tsc-menu--key-builder-get-alphabet ()
  "Generate list of lowercase, uppercase letters and symbols @ .."
  (append
   (mapcar #'char-to-string
           (number-sequence (string-to-char
                             "a")
                            (string-to-char
                             "z")))
   (mapcar #'char-to-string
           (number-sequence (string-to-char
                             "A")
                            (string-to-char
                             "Z")))
   (list "@" ".")))

(defun tsc-menu--key-builder-default-value-fn (key value)
  "Generate list with key and value(s).

Argument KEY is the key associated with the VALUE in the key-value pair.

Argument VALUE is the value associated with the KEY; it can be a proper list or
any other object."
  (if (proper-list-p value)
      (append (list key) value)
    (cons key value)))

(defun tsc-menu--key-builder-default-key-fn (def)
  "Generate string from symbol or return argument.

Argument DEF is a symbol or a string that represents the key to be built."
  (if (symbolp def)
      (symbol-name def)
    def))

(defun tsc-menu--key-builder-generate-shortcuts (items &optional key-fn value-fn
                                                       used-keys key-len
                                                       no-transform)
  "Generate shortcuts for menu items.

Argument ITEMS is a list of items for which shortcuts are generated.

Optional argument KEY-FN is a function that generates a string from an item; it
defaults to `tsc-menu--key-builder-default-key-fn'.

Optional argument VALUE-FN is a function that generates a value from a shortcut
and an item; it defaults to `tsc-menu--key-builder-default-value-fn'.

Optional argument USED-KEYS is a list of strings representing already used keys;
it defaults to nil.

Optional argument KEY-LEN is an integer specifying the desired length of the
generated shortcuts; it defaults to nil.

Optional argument NO-TRANSFORM is a function that determines whether an item
should bypass transformation; it defaults to nil."
  (unless key-fn (setq key-fn #'tsc-menu--key-builder-default-key-fn))
  (unless value-fn (setq value-fn #'tsc-menu--key-builder-default-value-fn))
  (let ((min-len
         (or key-len
             (if used-keys
                 (length (car (seq-sort-by #'length #'> used-keys)))
               (let ((variants-len (length (tsc-menu--key-builder-get-alphabet)))
                     (total (length items)))
                 (cond ((>= variants-len total)
                        1)
                       ((>= variants-len (/ total 2))
                        2)
                       (t 3)))))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar
                     (tsc-menu--compose
                       (tsc-menu--cond
                         [(lambda (it)
                            (when-let ((res (and no-transform (funcall
                                                               no-transform it))))
                              res))
                          identity]
                         [(tsc-menu--compose not
                            (apply-partially #'string-match-p
                                             "[a-z]"))
                          identity]
                         [t (apply-partially #'replace-regexp-in-string
                                             "^[^a-z]+" "")])
                       (lambda (it)
                         (funcall key-fn it)))
                     items))
          (result))
      (dotimes (i (length items))
        (let ((word (nth i all-keys))
              (def (nth i items)))
          (when-let* ((shortcut
                       (if (and no-transform
                                (funcall no-transform word))
                           word
                         (when (not (member word used-words))
                           (tsc-menu--key-builder-generate-shortcut-key
                            word
                            min-len
                            shortcuts
                            all-keys))))
                      (value (funcall value-fn shortcut def)))
            (setq used-words (push word used-words))
            (setq shortcuts (push shortcut shortcuts))
            (setq result (push value result)))))
      (reverse result))))

(defun tsc-menu--split-choices (str)
  "Split STR into a list of choices, separated by commas and optional spaces.

Argument STR is a string to be split into choices."
  (split-string str ",[\s]+" t))

(defun tsc-menu--analyze-arg (arg-spec)
  "Analyze argument specifications for transient commands.

Argument ARG-SPEC is an association list describing the argument specification."
  (pcase-let* ((`(,arg ,_descr ,alist) arg-spec)
               (`(,argument ,shortarg) arg)
               (description (substring-no-properties argument 2))
               (single-choices (cdr (assoc-string "one of" alist)))
               (multi-choices (cdr (assoc-string "one or more" alist)))
               (type (cdr (assoc-string "type" alist)))
               (default (cdr (assoc-string "default" alist))))
    (cond ((cdr (assoc-string argument tsc-menu-project--arguments-props))
           (cdr (assoc-string argument tsc-menu-project--arguments-props)))
          (multi-choices
           (list
            description
            (concat argument " ")
            :class 'transient-option
            :multi-value 'repeat
            :choices (or (tsc-menu--split-choices multi-choices)
                         (list ""))))
          (single-choices
           (list
            description
            (concat argument " ")
            :class 'transient-option
            :choices (tsc-menu--split-choices single-choices)))
          (t
           (pcase type
             ("integer" (list
                         description
                         (concat argument " ")
                         :prompt "an integer: "
                         :reader 'transient-read-number-N+))
             ("string" (list
                        description
                        (concat argument " ")
                        :prompt "string: "))
             ("boolean"
              (if (and default
                       (string= default "false"))
                  (list description
                        (if shortarg
                            (list argument shortarg)
                          argument))
                (list description
                      (concat argument " true"))))
             ((guard (stringp shortarg))
              (list shortarg
                    description
                    (list argument shortarg)))
             (_
              (list description
                    argument)))))))

(defun tsc-menu--parse-meta ()
  "Parse and return metadata type-value pair from buffer line."
  (let ((case-fold-search nil))
    (when (looking-at
           "^\\([a-z]+[^:]+\\):[ ]")
      (let ((type (match-string-no-properties 1))
            (value (buffer-substring-no-properties (match-end 0)
                                                   (line-end-position))))
        (forward-line 1)
        (cons type value)))))

(defun tsc-menu--parse-arg ()
  "Parse command-line arguments from buffer text."
  (let ((case-fold-search nil))
    (when
        (looking-at
         "^\\([-]\\{1,2\\}\\([^\s\n,]+\\)\\)\\(,[\s]\\([-]\\{1\\}\\([^\n]+\\)\\)\\)?")
      (let ((long-arg (match-string-no-properties 1))
            (short-arg (match-string-no-properties 4)))
        (forward-line 1)
        (list long-arg short-arg)))))

(defun tsc-menu--parse-description ()
  "Extract and return the current line's description text."
  (let ((case-fold-search nil))
    (when (looking-at
           "^[A-Z][^A-Z][^\n]+")
      (let ((descr (match-string-no-properties 0)))
        (forward-line 1)
        descr))))

(defun tsc-menu--parse-group-title ()
  "Extract and return the current line's group title."
  (let ((case-fold-search nil))
    (when
        (looking-at
         "^\\(\\([A-Z][A-Z]+[^\n]+\\)\\|\\([#]\\{1,6\\}[\s\t]+\\([^\n]+\\)\\)\\)")
      (let ((value (or (match-string-no-properties 2)
                       (match-string-no-properties 4))))
        (forward-line 1)
        value))))

(defun tsc-menu--parse-argument ()
  "Parse command-line arguments for a transient menu."
  (when-let ((args-spec (tsc-menu--parse-arg)))
    (let ((result)
          (descr (tsc-menu--parse-description))
          (meta))
      (while (setq meta (or (tsc-menu--parse-meta)))
        (push meta result))
      (let* ((value (list args-spec descr (nreverse result)))
             (res (tsc-menu--analyze-arg value)))
        (if res
            res
          (print value)
          nil)))))

(defun tsc-menu--parse-args ()
  "Parse arguments and return them as a reversed list."
  (let ((args))
    (while
        (when-let ((arg (tsc-menu--parse-argument)))
          (progn
            (skip-chars-forward "\n")
            (push arg args))))
    (nreverse args)))

(defun tsc-menu--parse-buffer ()
  "Parse and return structured menu data from buffer."
  (let ((groups)
        (used-keys)
        (case-fold-search nil))
    (while
        (let ((title (tsc-menu--parse-group-title)))
          (if title
              (progn (forward-line -1)
                     nil)
            (zerop (forward-line 1)))))
    (while (progn (skip-chars-forward "\s\t\n")
                  (let* ((title (progn (skip-chars-forward "\s\t\n")
                                       (tsc-menu--parse-group-title)))
                         (descr (progn (skip-chars-forward "\s\t\n")
                                       (tsc-menu--parse-description)))
                         (args (progn (skip-chars-forward "\s\t\n")
                                      (tsc-menu--parse-args))))
                    (when args
                      (setq args (tsc-menu--key-builder-generate-shortcuts
                                  args
                                  #'car
                                  (lambda (key value)
                                    (if (string-prefix-p "-" key)
                                        value
                                      (append (list key) value)))
                                  used-keys
                                  3
                                  (lambda (key)
                                    (string-prefix-p "-" key))))
                      (setq used-keys (append used-keys
                                              (mapcar #'car args))))
                    (dolist (it (list "" title descr))
                      (when it
                        (push (truncate-string-to-width it 50 nil nil t)
                              groups)))
                    (when args
                      (setq groups (append (nreverse args) groups)))
                    (or title descr args))))
    (nreverse groups)))

(defun tsc-menu--parse-help ()
  "Parse TypeScript compiler help output."
  (let ((cmd (executable-find "tsc")))
    (with-temp-buffer
      (let ((status (call-process cmd nil (current-buffer) nil "--all")))
        (when (zerop status)
          (goto-char (point-min))
          (tsc-menu--parse-buffer))))))

(defvar tsc-menu--children nil
  "List of submenu children for a tree-sitter construct.")

(defun tsc-menu--init-children ()
  "Split parsed items into sublists of equal length."
  (let* ((items (tsc-menu--parse-help))
         (len (/ (length items) 3)))
    (seq-split items len)))

(defvar tsc-menu--multi-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                #'tsc-menu-throw-done)
    (define-key map (kbd "C-M-j")
                #'tsc-menu-throw-done)
    map)
  "Keymap used in `autofix-read-keyword'.")

(defun tsc-menu-throw-done ()
  "Throw to the catch for done and return nil from it."
  (interactive)
  (throw 'done nil))


(defun tsc-menu-get-project-root ()
  "Find the root directory of a TypeScript or JavaScript project."
  (or (locate-dominating-file default-directory "tsconfig.json")
      (locate-dominating-file default-directory "jsconfig.json")
      (locate-dominating-file default-directory "package.json")))

(defun tsc-menu--multi-file-reader (&optional prompt _initial-input _history)
  "Read multiple standard Keywords headers with PROMPT."
  (let* ((choices)
         (curr))
    (catch 'done
      (while (setq curr
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (use-local-map
                          (make-composed-keymap
                           tsc-menu--multi-completion-map
                           (current-local-map))))
                     (read-file-name
                      (concat
                       (or prompt "File: \s") (substitute-command-keys
                                               "(`\\<tsc-menu--multi-completion-map>\
\\[tsc-throw-done]')\s")
                       (if choices
                           (concat
                            "("
                            (string-join
                             choices
                             ", ")
                            ")")
                         "")))))
        (setq choices (append choices (list curr)))))
    choices))

(transient-define-argument tsc-menu--files ()
  :description "Files"
  :argument "--files="
  :multi-value 'rest
  :reader 'tsc-menu--multi-file-reader
  :class 'transient-files)

(defun tsc-menu-get-arguments ()
  "Retrieve and process arguments for tsc command."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    (if-let ((files (seq-find (lambda (it)
                                (equal (car-safe it)
                                       "--files="))
                              raw-args)))
        (list (cdr files)
              (remove files raw-args))
      raw-args)))

(defun tsc-menu-format-args (args)
  "Format tsc arguments by joining lists and substituting hints.

Argument ARGS is a list of arguments to be formatted."
  (let ((new-args (mapcar
                   (lambda (it)
                     (cond ((listp it)
                            (string-join it " "))
                           (t it)))
                   args)))
    (string-join new-args "\s")))

(defun tsc-menu-get-formatted-transient-args ()
  "Format transient arguments for tsc."
  (tsc-menu-format-args (tsc-menu-get-arguments)))

;;;###autoload (autoload 'tsc-menu-show-args "tsc-menu" nil t)
(transient-define-suffix tsc-menu-show-args ()
  "Display formatted tsc command with arguments."
  :transient t
  :description "Show arguments"
  (interactive)
  (let ((args
         (tsc-menu-get-formatted-transient-args)))
    (message
     (propertize args
                 'face 'success))))



;;;###autoload (autoload 'tsc-menu "tsc-menu" nil t)
(transient-define-prefix tsc-menu ()
  "Transient menu for typescript commands."
  [[:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (append (list '("--" "Files" tsc-menu--files))
               (nth 0 tsc-menu--children))))]
   [:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (nth 1 tsc-menu--children)))]
   [:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (nth 2 tsc-menu--children)))]
   [:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (append (nth 3 tsc-menu--children)
               (list
                '("C-c C-a" tsc-menu-show-args)
                '("RET" "Run"
                  (lambda ()
                    (interactive)
                    (let
                        ((command (read-string "Run: "
                                   (string-trim
                                    (concat
                                     (executable-find "tsc")
                                     " "
                                     (tsc-menu-get-formatted-transient-args))))))
                      (let* ((compilation-read-command nil)
                             (compile-command command))
                       (compile compile-command)))))))))]
   []
   []]
  (interactive)
  (setq tsc-menu--children (tsc-menu--init-children))
  (transient-setup #'tsc-menu))

(provide 'tsc-menu)
;;; tsc-menu.el ends here
