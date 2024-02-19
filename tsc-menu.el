;;; tsc-menu.el --- Transient interface for TypeScript compiler -*- lexical-binding: t; -*-

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

(declare-function ansi-color-apply-on-region "ansi-color")

(defcustom tsc-menu-project-arguments-props '(("--project" "-p"
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
                                               :reader
                                               transient-read-directory))
  "Alist of command-line argument properties for TypeScript compiler.

A list of argument specifications for TypeScript compiler options used in
transient menus.

These specifications are used to dynamically generate transient commands for
interacting with the TypeScript compiler. Each specification defines how an
option is presented in the transient interface and how user input is handled."
  :group 'tsc-menu
  :type '(alist
          :key-type string
          :value-type (list
                       string
                       string
                       string
                       (plist :inline t))))


(defcustom tsc-menu-init-with-all-options nil
  "Whether to show all options on initial view."
  :type 'boolean
  :group 'tsc-menu)

(defcustom tsc-menu-browser-args '("--lib" "esnext" "--lib" "dom" "--lib"
                                   "dom.iterable")
  "Arguments passed to TypeScript compiler for browser targets.

A list of command-line arguments to pass to the TypeScript compiler when
compiling code for the browser using `tsc-menu-compile-region-for-browser'. The
default arguments enable the \"esnext\", \"dom\", and \"dom.iterable\"
libraries.

Each element in the list should be a string that represents a valid command-line
argument for the TypeScript compiler. These arguments are used to customize the
behavior of the TypeScript compiler for browser-specific code generation."
  :type '(repeat string)
  :group 'tsc-menu)

(eval-and-compile
  (defun tsc-menu--expand (init-fn)
    "Expand the given macro and return the expanded form.
Argument INIT-FN is the macro to be expanded."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defvar tsc-menu--all-options nil
  "List of options parsed from --all help output.")

(defvar tsc-menu--common-options nil
  "List of options parsed from --help output.")

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
                         (funcall finalize
                                  (mapconcat
                                   (apply-partially
                                    #'tsc-menu--key-builder-safe-substring
                                    n)
                                   parts "")))
                       (number-sequence 1 (min len parts-len)))
                      (tsc-menu--key-splitted-variants word len "")
                      (mapcar
                       (lambda (n)
                         (funcall finalize
                                  (mapconcat
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
               (let ((variants-len (length
                                    (tsc-menu--key-builder-get-alphabet)))
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
                            (when-let ((res (and no-transform
                                                 (funcall
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

(defun tsc-menu--get-arg-help-str (arg)
  "Parse description for ARG."
  (let ((cmd (executable-find "tsc")))
    (with-temp-buffer
      (let ((status (call-process cmd nil (current-buffer) nil "--all")))
        (when (zerop status)
          (goto-char (point-max))
          (when (re-search-backward (concat "^\\(\s*\\)--" (regexp-quote arg))
                                    nil t 1)
            (let* ((beg (point)))
              (tsc-menu--parse-arg)
              (tsc-menu--parse-description)
              (let ((result)
                    (meta))
                (while (setq meta (or (tsc-menu--parse-meta)))
                  (push meta result))
                (buffer-substring-no-properties beg (point))))))))))


(defun tsc-menu--show-help (suffix)
  "Display tsc help for a given suffix.

Argument SUFFIX is an object containing a description property."
  (interactive)
  (let* ((descr (oref suffix description))
         (full-description (tsc-menu--get-arg-help-str descr))
         (buffer (get-buffer-create
                  "*tsc-menu-help*"))
         (orign-wnd (selected-window)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-at-bottom
                '((window-height . fit-window-to-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq truncate-lines nil)
                (when-let* ((quit-key (where-is-internal
                                       'quit-window
                                       special-mode-map
                                       t t t))
                            (map
                             (make-sparse-keymap))
                            (buff (current-buffer)))
                  (define-key map quit-key
                              (lambda ()
                                (interactive)
                                (when (and orign-wnd
                                           (window-live-p orign-wnd))
                                  (select-window orign-wnd)
                                  (transient-resume)
                                  (when (buffer-live-p buff)
                                    (kill-buffer buff)))))
                  (use-local-map
                   (make-composed-keymap
                    map
                    (current-local-map))))
                (when full-description
                  (let ((pos (point)))
                    (insert full-description)
                    (fill-region-as-paragraph pos (point))))))
            (select-window window))))))

(defun tsc-menu--analyze-arg (arg-spec)
  "Analyze argument specifications for transient commands.

Argument ARG-SPEC is an association list describing the argument specification."
  (pcase-let* ((`(,arg ,full-description ,alist) arg-spec)
               (`(,argument ,shortarg) arg)
               (description (substring-no-properties argument 2))
               (single-choices (cdr (assoc-string "one of" alist)))
               (multi-choices (cdr (assoc-string "one or more" alist)))
               (type (cdr (assoc-string "type" alist)))
               (default (cdr (assoc-string "default" alist)))
               (spec
                (cond ((cdr (assoc-string argument
                                          tsc-menu-project-arguments-props))
                       (cdr (assoc-string argument
                                          tsc-menu-project-arguments-props)))
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
                                  (concat argument "=true"))))
                         ((guard (stringp shortarg))
                          (list shortarg
                                description
                                (list argument shortarg)))
                         (_
                          (list description
                                argument)))))))
    (append spec
            (list :show-help
                  (lambda (_suffix)
                    (interactive)
                    (tsc-menu--show-description
                     argument
                     shortarg
                     full-description
                     alist))))))

(defun tsc-menu--show-description (argument shortarg description alist)
  "Display a help buffer with details for a command and its arguments.

Argument ARGUMENT is a string representing the argument to be displayed.

Argument SHORTARG is a string representing the short form of the argument.

Argument DESCRIPTION is a string providing a detailed description of the
argument.

Argument ALIST is an association list where each element is a cons cell (KEY .
VALUE), both of which are strings."
  (let ((buffer (get-buffer-create
                 "*tsc-menu-help*"))
        (orign-wnd (selected-window)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-at-bottom
                '((window-height . shrink-window-if-larger-than-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq truncate-lines nil)
                (let* ((buff (current-buffer))
                       (exit-cmd (lambda ()
                                   (interactive)
                                   (when (and orign-wnd
                                              (window-live-p orign-wnd))
                                     (select-window orign-wnd)
                                     (transient-resume)
                                     (when (buffer-live-p buff)
                                       (kill-buffer buff)))))
                       (quit-key (where-is-internal
                                  'quit-window
                                  special-mode-map
                                  t t t))
                       (kquit-key (where-is-internal
                                   'keyboard-quit
                                   nil t))
                       (map
                        (make-sparse-keymap)))
                  (when kquit-key
                    (define-key map kquit-key exit-cmd))
                  (when quit-key
                    (define-key map quit-key
                                exit-cmd))
                  (use-local-map
                   (make-composed-keymap
                    map
                    (current-local-map))))
                (when argument
                  (insert (propertize argument 'face
                                      'font-lock-keyword-face)))
                (when shortarg
                  (insert ", "
                          (propertize shortarg 'face
                                      'font-lock-keyword-face)))
                (when description
                  (newline 2)
                  (let ((pos (point)))
                    (insert description)
                    (fill-region-as-paragraph pos
                                              (point))
                    (newline)))
                (when alist
                  (pcase-dolist (`(,k . ,v) alist)
                    (when (or k v)
                      (newline)
                      (let ((pos (point)))
                        (when k
                          (insert
                           (propertize k
                                       'face
                                       'font-lock-keyword-face)))
                        (when v (insert (if k ": " "") v))
                        (fill-region-as-paragraph
                         pos
                         (point))))))))
            (select-window window)))
      (shrink-window-if-larger-than-buffer))))

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


(defun tsc-menu--parse-all-help ()
  "Parse TypeScript compiler help output."
  (let ((cmd (executable-find "tsc")))
    (with-temp-buffer
      (let ((status (call-process cmd nil (current-buffer) nil "--all")))
        (when (zerop status)
          (goto-char (point-min))
          (tsc-menu--parse-buffer))))))

(defun tsc-menu--parse-common-help ()
  "Parse man TypeScript compiler help output."
  (let ((cmd (executable-find "tsc")))
    (with-temp-buffer
      (let ((status (call-process cmd nil (current-buffer) nil "--help")))
        (when (zerop status)
          (goto-char (point-min))
          (when (re-search-forward
                 "^\\([-]\\{1,2\\}\\([^\s\n,]+\\)\\)\\(,[\s]\\([-]\\{1\\}\\([^\n]+\\)\\)\\)?"
                 nil t 1)
            (goto-char (match-beginning 0))
            (when-let ((beg (save-excursion
                              (skip-chars-backward "\s\t\n")
                              (beginning-of-line)
                              (when (tsc-menu--parse-group-title)
                                (forward-line -1)
                                (point)))))
              (goto-char beg))
            (tsc-menu--parse-buffer)))))))

(defun tsc-menu--drop-last-while (pred lst)
  "Remove trailing elements from LST that satisfy PRED.

Argument PRED is a predicate function to test each element.

Argument LST is the list from which elements are dropped while PRED returns
true."
  (nreverse (seq-drop-while pred (reverse lst))))


(defun tsc-menu--split-options (options)
  "Split OPTIONS into sublists of roughly one-third the original length.

Argument OPTIONS is a sequence to be split into sub-sequences."
  (mapcar (apply-partially 'tsc-menu--drop-last-while #'stringp)
          (seq-split options (/ (length options) 3))))

(defun tsc-menu--init-all-options ()
  "Initialize TypeScript with all compiler options for menu."
  (setq tsc-menu--common-options
        (tsc-menu--split-options
         (tsc-menu--parse-common-help)))
  (setq tsc-menu--all-options
        (tsc-menu--split-options
         (tsc-menu--parse-all-help))))

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


(defclass tsc-menu-input-files (transient-infix)
  ((key         :initform "--")
   (argument    :initform "--")
   (reader      :initform #'tsc-menu--multi-file-reader)
   (always-read :initform t))
  "A transient class to read list of files.
The slot `value' is either a list of files or a single buffer.")

(defun tsc-menu--get-default-file-list-buffer ()
  "Return the default list of files or buffer to print.
In `dired-mode', get the marked files.  In other modes, if a
buffer has a file get the filename, otherwise return the buffer
itself."
  (seq-filter
   (lambda (f)
     (member (file-name-extension f) '("ts" "tsx")))
   (if (and (derived-mode-p 'dired-mode)
            (fboundp 'dired-get-marked-files))
       (dired-get-marked-files)
     (let ((ff (buffer-file-name)))
       (when (and ff (file-readable-p ff))
         (list ff))))))

(cl-defmethod transient-format-value ((this tsc-menu-input-files))
  "Format THIS value for display and return the result."
  (let ((argument (oref this argument)))
    (if-let ((value (oref this value)))
        (propertize
         (if (listp value)
             ;; Should be list of files.
             (mapconcat (lambda (x)
                          (file-relative-name
                           (abbreviate-file-name (string-trim x "\"" "\""))))
                        value " ")
           ;; Should be a buffer
           (prin1-to-string value))
         'face 'transient-value)
      (propertize argument 'face 'transient-inactive-value))))


(transient-define-argument tsc-menu--files ()
  :description "Files"
  :argument "--files="
  :class 'tsc-menu-input-files)


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

(defun tsc-menu--get-region ()
  "Return current active region as string or nil."
  (when (and (region-active-p)
             (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning)
                  (region-end)))))

(defun tsc-menu--compile-typescript-string (code &rest tsc-args)
  "Compile typescript CODE string with TSC-ARGS."
  (setq tsc-args (flatten-list tsc-args))
  (let* ((temp-file (concat (temporary-file-directory)
                            (make-temp-name "script") ".ts"))
         (outfile (concat (file-name-sans-extension temp-file) "." "js"))
         (command (string-join
                   (delq nil (append (list "tsc"
                                           (shell-quote-argument
                                            temp-file))
                                     (mapcar #'string-trim tsc-args)))
                   "\s")))
    (when-let ((buff (get-file-buffer outfile)))
      (with-current-buffer buff
        (set-buffer-modified-p nil))
      (kill-buffer buff))
    (let ((inhibit-message nil))
      (write-region code nil temp-file)
      (with-temp-buffer
        (shell-command command (current-buffer)
                       (current-buffer))
        (if (file-exists-p outfile)
            (with-temp-buffer
              (insert-file-contents outfile)
              (buffer-string))
          (minibuffer-message "An error: %s" (buffer-string))
          nil)))))


(defun tsc-menu--wrap-code (code &optional compile &rest args)
  "Wrap and optionally COMPILE TypeScript CODE string.

Argument CODE is the TypeScript code to wrap or compile.

Optional argument COMPILE is a boolean indicating whether to compile the
TypeScript code.

Remaining arguments ARGS are additional arguments passed to the TypeScript
compiler."
  (let ((normalized
         (format "(() => { %s })()"
                 (replace-regexp-in-string
                  "\\_<\\(export\\)\\_>"
                  "" code))))
    (if compile
        (replace-regexp-in-string ";$" ""
                                  (string-trim
                                   (apply #'tsc-menu--compile-typescript-string
                                          normalized
                                          args)))
      normalized)))

(defvar js-base-mode-hook)
(defvar js-mode-hook)
(defvar js-ts-mode-hook)

;;;###autoload
(defun tsc-menu-compile-region-for-browser ()
  "Compile TypeScript code for browser from selected region."
  (interactive)
  (let* ((normalized
          (format "(() => { %s })()"
                  (replace-regexp-in-string
                   "\\_<\\(export\\)\\_>"
                   "" (or (tsc-menu--get-region)
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max))))))
         (compiled
          (replace-regexp-in-string ";$" ""
                                    (string-trim
                                     (apply
                                      #'tsc-menu--compile-typescript-string
                                      normalized
                                      tsc-menu-browser-args)))))
    (kill-new compiled)
    (message "Copied")
    (tsc-menu-show-js-result (get-buffer-create "tsc-menu")
                             compiled)))


(defun tsc-menu-show-js-result (buffer result)
  "Display JavaScript RESULT in a BUFFER window.

Argument BUFFER is the buffer where the JavaScript RESULT will be displayed.

Argument RESULT is the JavaScript code execution result to be inserted into the
buffer."
  (with-current-buffer buffer
    (with-current-buffer-window
        buffer
        (cons 'display-buffer-in-direction
              '((window-height . fit-window-to-buffer)
                (preserve-size . window-preserve-size)))
        (lambda (window _value)
          (with-selected-window window
            (setq buffer-read-only t)
            (let ((inhibit-read-only t)
                  (js-base-mode-hook nil)
                  (js-mode-hook nil)
                  (js-ts-mode-hook nil))
              (require 'js nil t)
              (funcall (if (and (fboundp 'treesit-available-p)
                                (treesit-available-p)
                                (fboundp 'js-ts-mode))
                           #'js-ts-mode
                         #'js-mode))
              (erase-buffer)
              (when-let* ((quit-key (where-is-internal
                                     'quit-window
                                     special-mode-map
                                     t t t))
                          (map (make-sparse-keymap)))
                (define-key map quit-key #'quit-window)
                (use-local-map (make-composed-keymap
                                map
                                (current-local-map))))
              (save-excursion
                (insert result))))
          (select-window window)))))

(defun tsc-menu--find-arg-spec (argument)
  "Find ARGUMENT specification for `tsc-menu'.

ARGUMENT ARGUMENT is a string representing the command-line argument to be
analyzed."
  (unless tsc-menu--all-options
    (tsc-menu--init-all-options))
  (let* ((descr (string-trim-left argument "--"))
         (search-fn (lambda (it)
                      (pcase-let ((`(,_k ,desc . _rest)
                                   (if (numberp (car-safe it))
                                       (seq-drop 1 it)
                                     it)))
                        (when (stringp desc)
                          (string= descr desc))))))
    (or (seq-find search-fn
                  (apply #'append tsc-menu--all-options))
        (seq-find search-fn
                  (apply #'append tsc-menu--common-options)))))

(defun tsc-menu--compile-async (proc-name buff callback program &rest args)
  "Compile code asynchronously and run CALLBACK on completion.

Argument PROC-NAME is a string naming the asynchronous process.

Argument BUFF is the buffer or the name of the buffer associated with the
process.

Argument CALLBACK is the function to call when the process terminates.

Argument PROGRAM is the program to execute.

Remaining arguments ARGS are strings passed as command-line arguments to
PROGRAM."
  (require 'ansi-color)
  (let ((proc (apply #'start-file-process proc-name buff
                     program args)))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((proc-status (process-status process)))
         (when (memq proc-status '(exit signal))
           (with-current-buffer (process-buffer process)
             (ansi-color-apply-on-region (point-min)
                                         (point-max)))
           (when-let ((buff (process-buffer process)))
             (kill-buffer buff))
           (funcall callback)))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    proc))

(defvar org-mode-hook)

;;;###autoload
(defun tsc-menu-showcase-modules (&optional args)
  "Compile TypeScript code with various module options.

Optional argument ARGS is a list of arguments for the TypeScript compiler
command."
  (interactive (list (tsc-menu-get-arguments)))
  (let* ((program (executable-find "tsc"))
         (files
          (when-let ((arg (car-safe args)))
            (and (listp arg)
                 (seq-filter #'file-exists-p arg))))
         (filtered-args
          (seq-remove
           (lambda (it)
             (and (stringp it)
                  (string-match-p "^--\\(module\\|outFile\\)[=\s]" it)))
           (if files
               (seq-drop args 1)
             args)))
         (region (tsc-menu--get-region))
         (source-file (if region
                          (make-temp-file "script" nil ".ts" region)
                        (or
                         (when-let ((file (seq-find
                                           (lambda (it)
                                             (not (file-directory-p it)))
                                           files)))
                           file)
                         buffer-file-name
                         (make-temp-file "script" nil ".ts"
                                         (buffer-substring-no-properties
                                          (point-min)
                                          (point-max))))))
         (modules
          (cadr
           (member :choices
                   (tsc-menu--find-arg-spec "--module"))))
         (code (with-temp-buffer
                 (insert-file-contents
                  source-file)
                 (buffer-string)))
         (final-args (mapcar (lambda (m)
                               (append
                                (list
                                 "--module"
                                 m)
                                (mapcan (lambda (arg)
                                          (if (string-prefix-p
                                               "--" arg)
                                              (split-string arg
                                                            " "
                                                            t)
                                            (list arg)))
                                        (flatten-list
                                         filtered-args))))
                             modules))
         (result-buffer (with-current-buffer (get-buffer-create "*tsc-modules*")
                          (when-let* ((quit-key (where-is-internal
                                                 'quit-window
                                                 special-mode-map
                                                 t t t))
                                      (map (make-sparse-keymap)))
                            (define-key map quit-key #'quit-window)
                            (use-local-map (make-composed-keymap
                                            map
                                            (current-local-map)))
                            (setq buffer-read-only t)
                            (let ((inhibit-read-only t)
                                  (js-base-mode-hook nil)
                                  (js-mode-hook nil)
                                  (js-ts-mode-hook nil)
                                  (org-mode-hook nil))
                              (delete-region (point-min)
                                             (point-max))
                              (require 'org nil t)
                              (org-mode)))
                          (current-buffer))))
    (dolist (margs final-args)
      (let* ((module (cadr margs))
             (temp-file (make-temp-file
                         (replace-regexp-in-string "/" "-" module) nil
                         ".ts"
                         code))
             (outfile (concat (file-name-sans-extension temp-file)
                              "." "js"))
             (proc-name (generate-new-buffer-name (format "tsc-%s" module)))
             (buff (generate-new-buffer-name proc-name)))
        (apply #'tsc-menu--compile-async proc-name
               buff
               (lambda ()
                 (delete-file temp-file)
                 (when (file-exists-p outfile)
                   (let ((content
                          (with-temp-buffer (insert-file-contents
                                             outfile)
                                            (buffer-string))))
                     (with-current-buffer result-buffer
                       (let ((inhibit-read-only t))
                         (goto-char (point-max))
                         (newline)
                         (insert (string-join
                                  (list
                                   (concat "* " module "\n")
                                   (concat "#+name: " module)
                                   "#+begin_src js"
                                   (concat "// " (string-join
                                                  (delq nil margs) " "))
                                   content
                                   "#+end_src")
                                  "\n")))))
                   (delete-file outfile)))
               program
               (append (list temp-file)
                       margs))))
    (unless (get-buffer-window result-buffer)
      (with-selected-window
          (let ((wind-target
                 (if (minibuffer-selected-window)
                     (with-minibuffer-selected-window
                       (let ((wind (selected-window)))
                         (or
                          (window-right wind)
                          (window-left wind)
                          (split-window-sensibly)
                          wind)))
                   (let ((wind (selected-window)))
                     (or
                      (window-right wind)
                      (window-left wind)
                      (split-window-sensibly)
                      wind)))))
            wind-target)
        (unless (get-buffer-window result-buffer)
          (pop-to-buffer-same-window result-buffer))))))

;;;###autoload
(defun tsc-menu-compile-region (&optional args)
  "Compile TypeScript code from region or buffer into a new window.

Optional argument ARGS is a list of strings representing additional arguments
for the TypeScript compiler."
  (interactive (list (tsc-menu-get-arguments)))
  (let ((result (tsc-menu--wrap-code
                 (or (tsc-menu--get-region)
                     (buffer-substring-no-properties
                      (point-min)
                      (point-max)))
                 t
                 args))
        (buffer (get-buffer-create "tsc-menu")))
    (tsc-menu-show-js-result buffer result)))

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
(defvar compilation-read-command)

(defun tsc-menu-compile (&optional args)
  "Compile TypeScript using ARGS."
  (interactive (list (tsc-menu-get-arguments)))
  (let ((command (read-string "Run: "
                              (string-trim
                               (concat
                                (executable-find "tsc")
                                " "
                                (tsc-menu-format-args args))))))
    (let ((compilation-read-command nil)
          (compile-command command))
      (compile compile-command))))



(defun tsc-menu--get-options ()
  "Return either all options or common options based on a condition."
  (if tsc-menu-init-with-all-options
      tsc-menu--all-options
    tsc-menu--common-options))

(transient-define-suffix tsc-menu-toggle-options-view ()
  "Toggle display between all and common options."
  :description (lambda ()
                 (concat "Show " (if tsc-menu-init-with-all-options
                                     "common options"
                                   "all options")))
  (interactive)
  (setq tsc-menu-init-with-all-options (not tsc-menu-init-with-all-options))
  (transient-setup transient-current-command))


;;;###autoload (autoload 'tsc-menu "tsc-menu" nil t)
(transient-define-prefix tsc-menu ()
  "Transient menu for typescript commands."
  [[:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix transient--prefix)
       (append (list '("--" "Files" tsc-menu--files)
                     '("." tsc-menu-toggle-options-view))
               (nth 0 (tsc-menu--get-options))
               (list
                "Actions"
                '("C-c C-a" tsc-menu-show-args)
                '("C-c C-b" "Compile for browser"
                  tsc-menu-compile-region-for-browser)
                '("C-c RET" "Showcase all modules" tsc-menu-showcase-modules)
                '("C-c C-c" "Compile region" tsc-menu-compile-region)
                '("RET" "Run" tsc-menu-compile)))))]
   [:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (nth 1 (tsc-menu--get-options))))]
   [:setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        transient--prefix)
       (append (nth 2 (tsc-menu--get-options))
               (nth 3 (tsc-menu--get-options)))))]]
  (interactive)
  (tsc-menu--init-all-options)
  (transient-setup #'tsc-menu))

(provide 'tsc-menu)
;;; tsc-menu.el ends here
