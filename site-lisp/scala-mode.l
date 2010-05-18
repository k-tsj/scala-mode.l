;;;
;;; scala-mode.l
;;;
;;;   Copyright (c) 2010 Kazuki Tsujimoto, All rights reserved.
;;;
;;;   -- scala-mode-indnet.el --
;;;   Copyright (C) 2009 Scala Dev Team at EPFL
;;;   Authors:
;;;     * Main mode developers
;;;     ** Michel Schinz <Michel.Schinz at epfl.ch>
;;;     ** Anders Bach Nielsen <andersbach.nielsen at epfl.ch>
;;;     * Contributions
;;;     ** Iulian Dragos <dragos at epfl.ch>
;;;     ** Stephane Micheloud <michelou at epfl.ch>
;;;     ** Victor Rodriguez <victorr at gmail.com>
;;;     ** ? <cwitty at newtonlabs.com>
;;;     ** Hemant Kumar <gethemant at gmail.com>
;;;     ** Ulrick Muller <ulm@gentoo.org>
;;;
;;;   License
;;;
;;;   SCALA LICENSE
;;;
;;;   Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;;;   All rights reserved.
;;;
;;;   This software was developed by the Programming Methods Laboratory of the
;;;   Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;;
;;;   Permission to use, copy, modify, and distribute this software in source
;;;   or binary form for any purpose with or without fee is hereby granted,
;;;   provided that the following conditions are met:
;;;
;;;      1. Redistributions of source code must retain the above copyright
;;;         notice, this list of conditions and the following disclaimer.
;;;
;;;      2. Redistributions in binary form must reproduce the above copyright
;;;         notice, this list of conditions and the following disclaimer in the
;;;         documentation and/or other materials provided with the distribution.
;;;
;;;      3. Neither the name of the EPFL nor the names of its contributors
;;;         may be used to endorse or promote products derived from this
;;;         software without specific prior written permission.
;;;
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;;   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;   ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;;   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;;   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;   SUCH DAMAGE.
;;;

;;;
;;; Usage:
;;;
;;; Put site-lisp/scala-mode.l, etc/Scala on $XYZZY, and add following lines to
;;; your .xyzzy.
;;;
;;;   (require "scala-mode")
;;;   (push '("\\.scala$" . ed::scala-mode) *auto-mode-alist*)
;;;

(provide "scala-mode")
(in-package "editor")

(export '(scala-mode
          *scala-mode-hook*
          *scala-mode-map*
          *scala-mode-syntax-table*
          *scala-mode-indent-step*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-join (strs delim)
    (let ((r nil))
      (dolist (i strs (apply #'concat (rest (nreverse r))))
        (push delim r)
        (push i r)))))

(defconstant *scala-expr-start-re*
  (string-join (append (mapcar #'(lambda (i) (concat "\\<" i "\\>")) '("if" "else" "for" "do" "yield"))
                       '("=" "=>"))
               "\\|"))

(defconstant *scala-expr-starter*
  (mapcar (lambda (pair) (cons (car pair) (concat "\\<" (cdr pair) "\\>")))
          '(("else" . "if")
            ("yield" . "for")
            ("do" . "for")
            ("extends" . "class")
            ("with" . "class")
            ("=>" . "case"))))

(defconstant *scala-compound-expr-re*
  "\\<else\\s +if\\>")

(defconstant *scala-expr-middle-re*
  (string-join (mapcar #'(lambda (pair) (concat "\\<" (car pair) "\\>")) *scala-expr-starter*) "\\|"))

(defvar *scala-mode-indent-step* 2)

(defvar *scala-mode-hook* nil)

(defvar *scala-comment-column* nil)

(defvar *scala-keyword-hash-table* nil)
(defvar *scala-keyword-file* "Scala")

(defvar *scala-mode-map*  nil)
(unless *scala-mode-map*
  (setq *scala-mode-map* (make-sparse-keymap))
  (define-key *scala-mode-map* #\) 'scala-electric-close)
  (define-key *scala-mode-map* #\} 'scala-electric-close)
  (define-key *scala-mode-map* #\] 'scala-electric-close)
  (define-key *scala-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *scala-mode-map* #\TAB 'scala-indent-line)
  (define-key *scala-mode-map* #\RET 'scala-newline-and-indent))

(defvar *scala-mode-syntax-table* nil)
(unless *scala-mode-syntax-table*
  (setq *scala-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))
      ((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
        (set-syntax-punctuation *scala-mode-syntax-table* c))))
  (set-syntax-string *scala-mode-syntax-table* #\")
  (set-syntax-quote *scala-mode-syntax-table* #\')
  (set-syntax-escape *scala-mode-syntax-table* #\\)
  (set-syntax-symbol *scala-mode-syntax-table* #\_)
  (set-syntax-symbol *scala-mode-syntax-table* #\#)
  (set-syntax-word *scala-mode-syntax-table* #\<)
  (set-syntax-word *scala-mode-syntax-table* #\>)
  (set-syntax-word *scala-mode-syntax-table* #\=)
  (set-syntax-match *scala-mode-syntax-table* #\( #\))
  (set-syntax-match *scala-mode-syntax-table* #\{ #\})
  (set-syntax-match *scala-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *scala-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *scala-mode-syntax-table* "*/")
  (set-syntax-start-c++-comment *scala-mode-syntax-table* #\/)
  (set-syntax-end-c++-comment *scala-mode-syntax-table* #\LFD))

(defvar *scala-parse-limit* 100000)

(defun scala-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'scala-mode)
  (setq mode-name "Scala")
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'scala-build-summary-of-functions)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command #'scala-indent-line)
  (use-keymap *scala-mode-map*)
  (use-syntax-table *scala-mode-syntax-table*)
  (and *scala-keyword-file*
       (null *scala-keyword-hash-table*)
       (setq *scala-keyword-hash-table*
             (load-keyword-file *scala-keyword-file*)))
  (when *scala-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *scala-keyword-hash-table*))
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "/\\(\\*+\\|/\\)[ \t]*")
  (setq comment-indent-function 'c-comment-indent)
  (when *scala-comment-column*
    (setq comment-column *scala-comment-column*))
  (run-hooks '*scala-mode-hook*))

;; TODO
(defun scala-build-summary-of-functions ()
  nil)

(defun scala-indent-line ()
  "Indent current line as smartly as possible.
When called repeatedly, indent each time one stop further on the right."
  (interactive)
  (if (or (eq *last-command* 'scala-indent-line)
          (eq *last-command* 'scala-undent-line))
      (scala-indent-line-to (+ (current-indentation) *scala-mode-indent-step*))
    (ignore-errors
     (let ((indentation (scala-indentation)))
       (scala-indent-line-to indentation)))))

(defun current-indentation ()
  (save-excursion
    (- (progn (back-to-indentation) (point))
       (progn (beginning-of-line) (point)))))

(defun scala-indent-line-to (column)
  (smart-indentation column))

(defun scala-indentation ()
  "Return the suggested indentation for the current line."
  (save-excursion
    (goto-bol)
    (or (and (scala-in-comment-p)
             (not (eq (char-after (1- (point))) #\/))
             (scala-comment-indentation))
        (scala-indentation-from-following)
        (scala-indentation-from-preceding)
        (scala-indentation-from-block)
        0)))

(defun scala-indentation-debug ()
  (save-excursion
    (goto-bol)
    (msgbox "~S" (list 'scala-indentation-from-following (scala-indentation-from-following)
                       'scala-indentation-from-preceding (scala-indentation-from-preceding)
                       'scala-indentation-from-block (scala-indentation-from-block)))))

(defun scala-in-comment-p (&optional (p (point)))
  "Return t iff the point is inside a comment."
  (eq (parse-point-syntax p) :comment))

(defun scala-in-string-p (&optional (p (point)))
  "Return t iff the point is inside a string."
  (eq (parse-point-syntax p) :string))

;; builtin skip-white-forwad function skips even comments.
(defun skip-only-white-forward ()
  (skip-while #'(lambda (p) (syntax-whitespace-p (char-after p))) *scala-parse-limit*))

(defun scala-comment-indentation ()
  ;; Return suggested indentation inside of a comment.
  (forward-line -1)
  (goto-bol)
  (skip-only-white-forward)
  (if (looking-at "/\\*")
      (+ 1 (current-column))
    (current-column)))

(defun scala-indentation-from-following ()
  ;; Return suggested indentation based on the following part of the
  ;; current expression. Return nil if indentation cannot be guessed.
  (save-excursion
    (scala-forward-spaces (scala-point-after (goto-eol)))
    (cond
     ((eobp) nil)
     ((syntax-close-p (char-after (point)))
      (backward-up-list)
      (goto-char (1+ (point)))
      (- (scala-block-indentation) *scala-mode-indent-step*))
     ((looking-at *scala-expr-middle-re*)
      ;; [...] this is a somewhat of a hack.
      (let ((matching-kw (cdr (assoc (match-string 0)
                                     *scala-expr-starter*
                                     :test #'equal))))
        (while (and (re-search-backward matching-kw t)
                    (when (or (scala-in-comment-p) (scala-in-string-p))
                      (backward-char) t))))
      (scala-move-if (backward-word 1)
                     (looking-at *scala-compound-expr-re*))
      (current-column)))))

(defmacro scala-move-if (&rest body)
  (let ((pt-sym (gensym))
        (res-sym (gensym)))
    `(let ((,pt-sym (point))
           (,res-sym (progn ,@body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))

(defun scala-indentation-from-preceding ()
  ;; Return suggested indentation based on the preceding part of the
  ;; current expression. Return nil if indentation cannot be guessed.
  (save-excursion
    (scala-backward-spaces)
    (and (not (bobp))
         (if (syntax-open-p (char-before (point)))
             (scala-block-indentation)
           (progn
             (when (and (syntax-close-p (char-before (point)))
                        (not (eq (char-before (point)) #\}))) ;; See scala-indentation-from-preceding(6).
               (backward-sexp)
               (scala-backward-spaces))
             (scala-looking-at-backward *scala-expr-start-re*)))
         (+ (current-indentation) *scala-mode-indent-step*))))

(defun scala-indentation-from-block ()
  ;; Return suggested indentation based on the current block.
  (save-excursion
    (let ((p (point)))
      (backward-up-list 1 t)
      (if (= p (point))
          0
        (progn
          (goto-char (1+ (point)))
          (scala-block-indentation))))))

(defmacro scala-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))

(defun scala-forward-spaces (&optional limit)
  (if limit
      (save-restriction
        (narrow-to-region (point) limit)
        (forward-comment *scala-parse-limit*))
    (forward-comment *scala-parse-limit*)))

(defun scala-backward-spaces ()
  (forward-comment (- *scala-parse-limit*)))

(defun skip-while (pred cnt)
  (let ((init-point (point)))
    (multiple-value-bind (lmt stp end-test)
        (if (< cnt 0)
            (values (point-min) #'1- #'<)
          (values (point-max) #'1+ #'>))
      (when (and (< cnt 0)
                 (funcall pred (1- (point))))
        (goto-char (1- (point))))
      (do ((p (point) (funcall stp p)))
          ((funcall end-test p lmt) (goto-char lmt))
        (unless (funcall pred p)
          (goto-char p)
          (when (< (point) init-point)
            (forward-char))
          (return))))
    (abs (- init-point (point)))))

(defun forward-comment (cnt)
  (skip-while
   #'point-syntax-whitespace-or-comment-p
   cnt))

(defun scala-newline-and-indent ()
  (interactive)
  (delete-trailing-spaces)
  (newline)
  (scala-indent-line)
  (when (scala-in-multi-line-comment-p)
    (insert "* ")))

(defun scala-looking-at-backward (re)
  (save-excursion
    (when (= 0 (skip-while #'point-syntax-word-or-symbol-p (- *scala-parse-limit*))) (backward-char))
    (looking-at re)))

(defun point-syntax-whitespace-or-comment-p (p)
  (let ((c (char-after p)))
    (or (eq c #\LFD)
        (eq c #\NUL)
        (syntax-whitespace-p c)
        (scala-in-comment-p p))))

(defun point-syntax-word-or-symbol-p (p)
  (or (syntax-word-p (char-after p))
      (syntax-symbol-p (char-after p))))

;; For debugging
;; (defmacro d (&rest body)
;;   (let ((sym (gensym)))
;;     `(let ((,sym ,@body))
;;        (msgbox "~A" ,sym)
;;        ,sym)))

(defun scala-block-indentation ()
  (if (eq #\{ (char-before (point)))
      (+ (current-indentation) *scala-mode-indent-step*)
    (let ((block-start-eol (scala-point-after (goto-eol)))
          (block-after-spc (scala-point-after (scala-forward-spaces))))
      (if (> block-after-spc block-start-eol)
          (progn
            (goto-bol)
            (when (save-restriction
                    (narrow-to-region (point) block-start-eol)
                    (search-forward ")" t))
              (forward-char)
              (backward-sexp))
            (+ (current-indentation) *scala-mode-indent-step*))
        (current-column)))))

(defun scala-electric-close ()
  "Insert a brace, and if alone on a non-comment line, reindent."
  (interactive)
  (let ((on-empty-line-p (save-excursion
                           (goto-bol)
                           (looking-at "^\\s *$"))))
    ;; Calling self-insert-command will blink to the matching open-brace
    ;; (if blink-matching-paren is enabled); we first indent, then
    ;; call self-insert-command, so that the close-brace is correctly
    ;; positioned during the blink.
    (when on-empty-line-p
      (insert *last-command-char*)
      (scala-indent-line)
      (delete-backward-char 1))
    (call-interactively 'self-insert-command)))

(defun scala-in-single-line-comment-p ()
  "Return t iff the point is inside a single line comment."
  (let
      (begin
       end
       subst
       match)
    (save-excursion
      (setq end (point))
      (goto-bol)
      (setq begin (point))
      (setq subst (buffer-substring begin end))
      (setq match (string-match "//" subst))
      (if match t nil))))

(defun scala-in-multi-line-comment-p ()
  "Return t iff the point is inside a multi line comment."
  (and (not (scala-in-single-line-comment-p))
       (scala-in-comment-p)))
