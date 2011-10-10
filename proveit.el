;;; Basic Proof General Support for ProveIt.

;;; From demoisa.el, but seems unneccessary.
;;(eval-and-compile
;;  (require 'proof-site)
;;  (proof-ready-for-assistant 'proveit))

(require 'proof)
(require 'proof-easy-config)

(defvar proveit-keywords
  '("def" "intros" "proof" "qed" "axioms" "signature" "waldmeister" "≡" "by" "reasoning" "λ" "Id" "rewrite" "rtl" "ltr" "call" "\\" "?")
  "Proveit Keywords")

(defvar proveit-builtin
  '("suc" "ℕ" "zero")
  "Proveit Builtins")

(defvar proveit-keywords-regexp (regexp-opt proveit-keywords 'words))

(defvar proveit-builtin-regexp (regexp-opt proveit-builtin 'words))

(setq proveit-colouring
  `((,proveit-keywords-regexp . font-lock-keyword-face)
    (,proveit-builtin-regexp . font-lock-builtin-face)))

(defconst proveit-match-nothing-regexp "^a\ba$"
  "Match nothing, not even the empty string")

(defun proveit-find-and-forget (target-span)
  (let ((temp (proveit-write-string-to-temp-file
               (buffer-substring (span-start target-span) (proof-unprocessed-begin)))))
    (list (concat "pg-undo '" temp "'."))))

(defun proveit-write-string-to-temp-file (string)
  (with-temp-buffer
    (insert string)
    (let ((temp (make-temp-file "proveit")))
      (write-region (point-min) (point-max) temp)
      temp)))

(proof-easy-config 'proveit "proveit"
  proof-prog-name "/home/alasdair/.cabal/bin/proveit --proof-general"
  proof-terminal-string "."

  proof-script-comment-start "{-" ; Only used for inserting comments
  proof-script-comment-end "-}"

  proof-shell-quit-cmd "quit."
  proof-non-undoables-regexp "undo"
  proof-undo-n-times-command "undo %s." ; Won't work, but it doesn't seem to be used.

  ;; We don't have a concept of save and goal commands in proveit, so these are
  ;; set to match nothing.
  proof-save-command-regexp proveit-match-nothing-regexp
  proof-goal-command-regexp proveit-match-nothing-regexp

  proof-shell-annotated-prompt-regexp "^PROVEIT>"

  proof-shell-start-goals-regexp "=+\\s-GOALS\\s-=+"
  proof-shell-error-regexp "=+\\s-\\w*\\s-?ERROR\\s-=+"
  proof-shell-truncate-before-error 't

  proof-script-font-lock-keywords '(proveit-colouring)

  proof-goal-command "theorem thy : \"%s\""

  proof-find-and-forget-fn 'proveit-find-and-forget)

(provide 'proveit)
