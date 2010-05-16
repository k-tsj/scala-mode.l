;;;
;;; test-scala-mode.l
;;;
;;;   Copyright (c) 2010 Kazuki Tsujimoto, All rights reserved.
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

;;; Note:
;;;   test.l(a unit testing library for xyzzy. http://github.com/k-tsj/test.l) is required.


(load-library "scala-mode")

;; Utilities ---------------------------------------------------
(defvar *after-setup-buffer-hooks* nil)

;; `bufstat' spec
;;   e.g. "a_|_b"  => buffer-substring: "ab"
;;                    point: 1
(defun apply-bufstat (bufstat)
  (insert (substitute-string bufstat "_|_" ""))
  (goto-char (or (string-match "_|_" bufstat) (length bufstat))))

(defun buffer-bufstat ()
  "Return bufstat of the current buffer"
  (let* ((p (point))
         (bufstr (buffer-substring (point-min) (point-max)))
         (bufstat (concat (substring bufstr 0 p) "_|_" (substring bufstr p))))
    (values bufstat bufstr p)))

(defun cmd-execute (&rest strs)
  #'(lambda ()
      (dolist (str strs)
        (command-execute (substitute-string str "\n" (string #\RET))))))

;; doesn't work correctly...
;; (defun cmd-execute (&rest strs)
;;   (let ((str (substitute-string (apply #'concat strs) "\n" (string #\RET))))
;;     #'(lambda () (command-execute str))))

(defun equal-bufstat (expect _)
  (equal (cond
          ((listp expect) (apply #'concat expect))
          (t expect))
         (buffer-bufstat)))

(defun test-on-buffer (msg expect bufstat-list thunk &optional (compare #'equal-bufstat))
  (save-excursion
    (let ((buffer (get-buffer-create " *Test Buffer* ")))
      (erase-buffer buffer)
      (setup-temp-buffer buffer)
      (switch-to-buffer buffer)
      (run-hooks '*after-setup-buffer-hooks*)
      (apply-bufstat (apply #'concat bufstat-list))
      (test msg expect thunk compare))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-start "scala-mode")
(setq *test-report-error* nil)

(unless *after-setup-buffer-hooks*
  (add-hook '*after-setup-buffer-hooks* #'ed::scala-mode))

(setq ed::*scala-mode-indent-step* 2)

;; --------------------------------------------------------------
(test-section "defun: scala-forward-spaces")
(test-on-buffer
 "scala-forward-spaces(1)"
 '("  \n"
   "  _|_a")
 '("_|_  \n"
   "  a")
 #'ed::scala-forward-spaces)

(test-on-buffer
 "scala-forward-spaces(2)"
 '("  _|_\n"
   "  a")
 '("_|_  \n"
   "  a")
 #'(lambda () (ed::scala-forward-spaces 2)))

;; --------------------------------------------------------------
(test-section "defun: scala-backward-spaces")
(test-on-buffer
 "scala-backward-spaces(1)"
 '("  a_|_  ")
 '("  a_|_  ")
 #'ed::scala-backward-spaces)

(test-on-buffer
 "scala-backward-spaces(2)"
 '("  a_|_  \n"
   "  ")
 '("  a  \n"
   "  _|_")
 #'ed::scala-backward-spaces)

;; --------------------------------------------------------------
(test-section "defun: scala-block-indentation")
(test-on-buffer "scala-block-indentation(1)"
                0
                '("_|_")
                #'ed::scala-block-indentation
                #'equal)


(test-on-buffer "scala-block-indentation(2)"
                7
                '("     (  ) _|_ \n")
                #'ed::scala-block-indentation
                #'equal)

(test-on-buffer "scala-block-indentation(3)"
                5
                '("   _|_\n"
                  "  ")
                #'ed::scala-block-indentation
                #'equal)

(test-on-buffer "scala-block-indentation(4)"
                3
                '("   _|_   ")
                #'ed::scala-block-indentation
                #'equal)

(test-on-buffer "scala-block-indentation(5)"
                0
                '("_|_  match {")
                #'ed::scala-block-indentation
                #'equal)

(test-on-buffer "scala-block-indentation(6)"
                8
                '("  match _|_{")
                #'ed::scala-block-indentation
                #'equal)

;; --------------------------------------------------------------
(test-section "defun: scala-indentation-from-following")
(test-on-buffer "scala-indentation-from-following(1)"
                nil
                '("_|_")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(2)"
                *test-error*
                '("_|_}")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(3)"
                3
                '("   {\n"
                  "_|_}")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(4)"
                1
                '("_|_ else ")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(5)"
                3
                '("   if // if\n"
                  "_|_else")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(6)"
                3
                '("   else if\n"
                  "_|_else")
                #'ed::scala-indentation-from-following
                #'equal)

(test-on-buffer "scala-indentation-from-following(7)"
                nil
                '("_|_ a ")
                #'ed::scala-indentation-from-following
                #'equal)

;; --------------------------------------------------------------
(test-section "defun: scala-indentation-from-preceding")
(test-on-buffer "scala-indentation-from-preceding(1)"
                nil
                '("  \n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(2)"
                5
                '("   (\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(3)"
                nil
                '("   ( )\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(4)"
                5
                '("   for ( )\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)


(test-on-buffer "scala-indentation-from-preceding(5)"
                *test-error*
                '("   )\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(6)"
                nil
                '("a = {\n"
                  "}\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(7)"
                nil
                '("format()\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(8)"
                2
                '("aaa.bbb(\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(9)"
                nil ;; 8
                '("aaa.bbb(ccc,\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(9)"
                2
                '("aaa.bbb {\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

(test-on-buffer "scala-indentation-from-preceding(10)"
                2
                '("aaa.bbb { ccc =>\n"
                  "_|_")
                #'ed::scala-indentation-from-preceding
                #'equal)

;; --------------------------------------------------------------
(test-section "defun: scala-looking-at-backward")
(test-on-buffer "scala-looking-at-backward(1)"
                nil
                '(" _|_ab")
                #'(lambda ()
                    (ed::scala-looking-at-backward "ab"))
                #'equal)

(test-on-buffer "scala-looking-at-backward(2)"
                t
                '(" a_|_b")
                #'(lambda ()
                    (ed::scala-looking-at-backward "ab"))
                #'equal)

(test-on-buffer "scala-looking-at-backward(3)"
                t
                '(" ab_|_")
                #'(lambda ()
                    (ed::scala-looking-at-backward "ab"))
                #'equal)

(test-on-buffer "scala-looking-at-backward(4)"
                nil
                '(" ab _|_")
                #'(lambda ()
                    (ed::scala-looking-at-backward "ab"))
                #'equal)

(test-on-buffer "scala-looking-at-backward(4)"
                t
                '(" =>_|_")
                #'(lambda ()
                    (ed::scala-looking-at-backward "=>"))
                #'equal)

;; --------------------------------------------------------------
(test-section "defun: scala-indentation-from-block")
(test-on-buffer "scala-indentation-from-block(1)"
                0
                '("  _|_")
                #'ed::scala-indentation-from-block
                #'equal)

(test-on-buffer "scala-indentation-from-block(2)"
                4
                '("  {{}_|_")
                #'ed::scala-indentation-from-block
                #'equal)

;; --------------------------------------------------------------
(test-section "defun: skip-only-white-forward")
(test-on-buffer "skip-only-white-forward(1)"
                '("  _|_/*  */  ")
                '("_|_  /*  */  ")
                #'ed::skip-only-white-forward)

(test-on-buffer "skip-only-white-forward(2)"
                '("  _|_\n  a  ")
                '("_|_  \n  a  ")
                #'ed::skip-only-white-forward)


;; ----------------------------------------------------------------
(test-section "command-execute")
(test-on-buffer
 "command-execute(1)"
 '("    /*\n"
   "     * _|_")
 '()
 (cmd-execute "    /*\n"))

(test-on-buffer
 "command-execute(2)"
 '("{\n"
   "  _|_")
 '()
 (cmd-execute "{\n"))

(test-on-buffer
 "command-execute(3)"
 '("if\n"
   "  _|_")
 '()
 (cmd-execute "if\n"))

(test-on-buffer
 "command-execute(4)"
 '("if\n"
   "  _|_")
 '()
 (cmd-execute "if\n"))

(test-on-buffer
 "command-execute(5)"
 '("{\n"
   "  [\n"
   "    (\n"
   "    )\n"
   "  ]\n"
   "}_|_")
 '()
 (cmd-execute "{\n"
              "[\n"
              "(\n"
              ")\n"
              "]\n"
              "}"))

(test-on-buffer
 "command-execute(6)"
 '("{\n"
   "\n"
   "\n"
   "}_|_")
 '()
 (cmd-execute "{\n"
              "\n"
              "\n"
              "}"))

(test-on-buffer
 "command-execute(7)"
 '("a.b { c =>\n"
   "  d(\n"
   "  )\n"
   "}_|_")
 '()
 (cmd-execute "a.b { c =>\n"
              "d(\n"
              ")\n"
              "}"))

;; ----------------------------------------------------------------
(test-section "known issues(scala-mode.l)")
(test-on-buffer
 "known issues(scala-mode.l)(1)"
 '("  _|_/* aaa */ // bbb \n"
   "  a")
 ;; expect
 ;; '("  /* aaa */ // bbb \n"
 ;;   "  _|_a")
 '("_|_  /* aaa */ // bbb \n"
   "  a")
 #'ed::scala-forward-spaces)

;; ----------------------------------------------------------------
(test-section "known issues(scala.el)")
(test-on-buffer
 "known issues(scala.el)(1)"
 '("a = (b)\n"
   "  _|_")
 '()
 (cmd-execute "a = (b)\n"))



;; ----------------------------------------------------------------
(test-end)
