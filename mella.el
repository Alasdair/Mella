;;; Basic Proof General Support for ProveIt.

;;; From demoisa.el, but seems unneccessary.
;;(eval-and-compile
;;  (require 'proof-site)
;;  (proof-ready-for-assistant 'mella))

(require 'proof)
(require 'proof-easy-config)

(defvar mella-keywords
  '("def" "intros" "proof" "qed" "axioms" "signature" "waldmeister" "≡" "by" "reasoning" "λ" "Id" "rewrite" "rtl" "ltr" "call" "\\" "?")
  "Mella Keywords")

(defvar mella-builtin
  '("suc" "ℕ" "zero")
  "Mella Builtins")

(defvar mella-keywords-regexp (regexp-opt mella-keywords 'words))

(defvar mella-builtin-regexp (regexp-opt mella-builtin 'words))

(setq mella-colouring
  `((,mella-keywords-regexp . font-lock-keyword-face)
    (,mella-builtin-regexp . font-lock-builtin-face)))

(defconst mella-match-nothing-regexp "^a\ba$"
  "Match nothing, not even the empty string")

(defun mella-find-and-forget (target-span)
  (let ((temp (mella-write-string-to-temp-file
               (buffer-substring (span-start target-span) (proof-unprocessed-begin)))))
    (list (concat "pg-undo '" temp "'."))))

(defun mella-write-string-to-temp-file (string)
  (with-temp-buffer
    (insert string)
    (let ((temp (make-temp-file "mella")))
      (write-region (point-min) (point-max) temp)
      temp)))

(proof-easy-config 'mella "mella"
  proof-prog-name "/home/alasdair/.cabal/bin/mella --proof-general"
  proof-terminal-string "."

  proof-script-comment-start "{-" ; Only used for inserting comments
  proof-script-comment-end "-}"

  proof-shell-quit-cmd "quit."
  proof-non-undoables-regexp "undo"
  proof-undo-n-times-command "undo %s." ; Won't work, but it doesn't seem to be used.

  ;; We don't have a concept of save and goal commands in mella, so these are
  ;; set to match nothing.
  proof-save-command-regexp mella-match-nothing-regexp
  proof-goal-command-regexp mella-match-nothing-regexp

  proof-shell-annotated-prompt-regexp "^MELLA>"

  proof-shell-start-goals-regexp "=+\\s-GOALS\\s-=+"
  proof-shell-error-regexp "=+\\s-\\w*\\s-?ERROR\\s-=+"
  proof-shell-truncate-before-error 't

  proof-script-font-lock-keywords '(mella-colouring)

  proof-goal-command "theorem thy : \"%s\""

  proof-find-and-forget-fn 'mella-find-and-forget)

(provide 'mella)
