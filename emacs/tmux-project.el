;;; tmux-project.el --- Drive a tmux `work' session from Emacs -*- lexical-binding: t; -*-

;; Treats a persistent tmux session as Emacs's process layer.  Works for
;; both local projects and TRAMP-backed remote projects: every tmux
;; command runs on whichever host owns `default-directory'.  No external
;; scripts required — the remote machine only needs `tmux' on PATH.
;;
;; Commands (suggested bindings):
;;   C-c t r  tmux-project-runner          ensure runner pane (idle shell)
;;   C-c t c  tmux-project-claude          ensure claude pane
;;   C-c t s  tmux-project-send            send a command to runner pane
;;   C-c t p  tmux-project-pop             spawn foot attached to project window
;;   C-c t k  tmux-project-capture-runner  capture runner output (read-only)
;;   C-c t l  tmux-project-capture-claude  capture claude output (read-only)
;;
;; Setup:
;;   (require 'tmux-project)
;;   (global-set-key (kbd "C-c t r") #'tmux-project-runner)
;;   (global-set-key (kbd "C-c t c") #'tmux-project-claude)
;;   (global-set-key (kbd "C-c t s") #'tmux-project-send)
;;   (global-set-key (kbd "C-c t p") #'tmux-project-pop)
;;   (global-set-key (kbd "C-c t k") #'tmux-project-capture-runner)
;;   (global-set-key (kbd "C-c t l") #'tmux-project-capture-claude)

(require 'project)
(require 'cl-lib)

(defgroup tmux-project nil
  "Drive a persistent tmux session from Emacs."
  :group 'tools)

(defcustom tmux-project-session "work"
  "Name of the persistent tmux session."
  :type 'string)

(defcustom tmux-project-terminal "foot"
  "Local terminal emulator used by `tmux-project-pop'.
For remote projects, this terminal runs `ssh -t <host> tmux attach …'."
  :type 'string)

(defcustom tmux-project-capture-lines 2000
  "Number of lines of pane history to capture (passed as -S -N)."
  :type 'integer)

(defvar tmux-project--last-send nil
  "Last command sent via `tmux-project-send', used as default for re-runs.")

;; ---------------------------------------------------------------------------
;; Project + remote awareness
;; ---------------------------------------------------------------------------

(defun tmux-project--root ()
  "Return current project's root.  Strips TRAMP prefix for remote projects.
Errors if there's no current project."
  (let ((proj (project-current)))
    (unless proj
      (user-error "No current project"))
    (let ((root (directory-file-name (expand-file-name (project-root proj)))))
      (if (file-remote-p root)
          (file-remote-p root 'localname)
        root))))

(defun tmux-project--name ()
  "Return the tmux window name for the current project (basename of root)."
  (file-name-nondirectory (tmux-project--root)))

(defun tmux-project--remote-ssh-target ()
  "Return `user@host' (or `host') for `default-directory', or nil if local."
  (when (file-remote-p default-directory)
    (let ((host (file-remote-p default-directory 'host))
          (user (file-remote-p default-directory 'user)))
      (if user (format "%s@%s" user host) host))))

;; ---------------------------------------------------------------------------
;; tmux primitives — every command runs on the host of `default-directory'
;; ---------------------------------------------------------------------------

(defun tmux-project--tmux (&rest args)
  "Run `tmux ARGS' on the host of `default-directory'.
Return trimmed stdout, or signal an error on non-zero exit."
  (with-temp-buffer
    (let ((exit (apply #'process-file "tmux" nil t nil args)))
      (unless (zerop exit)
        (user-error "tmux %s failed: %s"
                    (mapconcat #'identity args " ")
                    (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun tmux-project--tmux-noerr (&rest args)
  "Like `tmux-project--tmux' but returns nil on failure instead of erroring.
Used for existence checks where failure is expected."
  (with-temp-buffer
    (when (zerop (apply #'process-file "tmux" nil t nil args))
      (string-trim (buffer-string)))))

(defun tmux-project--session-exists-p ()
  "Return non-nil if the tmux session exists on the current host."
  (tmux-project--tmux-noerr "has-session" "-t" tmux-project-session))

(defun tmux-project--window-target (name)
  "Return the tmux target string `<session>:<name>' for window NAME."
  (format "%s:%s" tmux-project-session name))

(defun tmux-project--window-exists-p (name)
  "Return non-nil if a window named NAME exists in the session."
  (when (tmux-project--session-exists-p)
    (let ((windows (tmux-project--tmux "list-windows"
                                       "-t" tmux-project-session
                                       "-F" "#W")))
      (member name (split-string windows "\n" t)))))

(defun tmux-project--ensure-window ()
  "Ensure a window for the current project exists; return its target."
  (let ((name (tmux-project--name))
        (path (tmux-project--root)))
    (cond
     ((not (tmux-project--session-exists-p))
      (tmux-project--tmux "new-session" "-d"
                          "-s" tmux-project-session
                          "-n" name "-c" path))
     ((not (tmux-project--window-exists-p name))
      (tmux-project--tmux "new-window"
                          "-t" (concat tmux-project-session ":")
                          "-n" name "-c" path)))
    (tmux-project--window-target name)))

;; ---------------------------------------------------------------------------
;; Pane management — find existing, adopt sole untitled, or split
;; ---------------------------------------------------------------------------

(defun tmux-project--list-panes (window)
  "Return list of (PANE-ID TITLE CURRENT-COMMAND) for WINDOW."
  (let ((output (tmux-project--tmux "list-panes" "-t" window
                                    "-F" "#{pane_id}\t#{pane_title}\t#{pane_current_command}")))
    (cl-loop for line in (split-string output "\n" t)
             for parts = (split-string line "\t")
             collect (list (nth 0 parts)
                           (or (nth 1 parts) "")
                           (or (nth 2 parts) "")))))

(defun tmux-project--find-pane (window title)
  "Return (PANE-ID TITLE CURRENT-COMMAND) of pane in WINDOW with TITLE, or nil."
  (cl-find title (tmux-project--list-panes window)
           :key #'cl-second :test #'string=))

(defun tmux-project--ensure-pane (title &optional command)
  "Ensure pane titled TITLE is alive in current project's window.
If COMMAND is given, run it on creation, adoption, or when the existing
pane is stale (title matches but COMMAND isn't the running process —
e.g. claude exited).  Returns the pane target.

Lookup:
1. Pane with this title exists:
   - if no COMMAND or COMMAND matches the running process → reuse as-is.
   - else → pane is stale, re-run COMMAND in it.
2. Single pane not yet titled `claude' or `runner' → adopt, title, run.
3. Otherwise → split below, title, run."
  (let* ((window (tmux-project--ensure-window))
         (panes  (tmux-project--list-panes window))
         (existing (cl-find title panes :key #'cl-second :test #'string=)))
    (cond
     ;; (1) Existing pane.
     (existing
      (let* ((pane-id (cl-first existing))
             (current (cl-third existing))
             (target  (format "%s.%s" window pane-id))
             ;; Match by basename — `claude' the command may show up as
             ;; `claude', `node', or similar depending on how it's invoked.
             ;; A simple substring match on the command name is good enough.
             (alive   (or (null command)
                          (string-match-p (regexp-quote
                                           (car (split-string command)))
                                          current))))
        (unless alive
          (tmux-project--tmux "send-keys" "-t" target command "Enter"))
        target))
     ;; (2) Adopt sole untitled pane.
     ((and (= 1 (length panes))
           (not (member (cl-second (car panes)) '("claude" "runner"))))
      (let* ((pane-id (cl-first (car panes)))
             (target  (format "%s.%s" window pane-id)))
        (tmux-project--tmux "select-pane" "-t" target "-T" title)
        (when command
          (tmux-project--tmux "send-keys" "-t" target command "Enter"))
        target))
     ;; (3) Split below.
     (t
      (let* ((new-id (tmux-project--tmux "split-window" "-t" window "-v" "-P"
                                         "-F" "#{pane_id}"
                                         "-c" "#{pane_current_path}"))
             (target (format "%s.%s" window new-id)))
        (tmux-project--tmux "select-pane" "-t" target "-T" title)
        (when command
          (tmux-project--tmux "send-keys" "-t" target command "Enter"))
        target)))))

;; ---------------------------------------------------------------------------
;; User commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tmux-project-runner ()
  "Ensure a `runner' pane exists in the current project's tmux window.
The pane runs an idle shell; commands can be pushed to it via
`tmux-project-send'.  Creates the window and the pane if needed."
  (interactive)
  (message "tmux runner: %s" (tmux-project--ensure-pane "runner")))

;;;###autoload
(defun tmux-project-claude ()
  "Ensure a `claude' pane runs in the current project's tmux window.
Creates the window and the pane if needed."
  (interactive)
  (message "tmux claude: %s" (tmux-project--ensure-pane "claude" "claude")))

;;;###autoload
(defun tmux-project-send (command)
  "Send COMMAND to the current project's runner pane.
Creates the runner pane on first use.  Does not touch claude.
Default value is the previous send, so `RET' re-runs."
  (interactive
   (list (read-string
          (format "Send to runner%s: "
                  (if tmux-project--last-send
                      (format " [%s]" tmux-project--last-send)
                    ""))
          nil nil tmux-project--last-send)))
  (setq tmux-project--last-send command)
  (let ((target (tmux-project--ensure-pane "runner")))
    (tmux-project--tmux "send-keys" "-t" target command "Enter")
    (message "tmux runner ← %s" command)))

;;;###autoload
(defun tmux-project-pop ()
  "Spawn foot attached to the current project's tmux window.
For TRAMP projects, foot runs `ssh -t <host> tmux attach …' to reach
the remote tmux server."
  (interactive)
  (let* ((target     (tmux-project--ensure-window))
         (ssh-target (tmux-project--remote-ssh-target))
         ;; setsid detaches foot from Emacs so closing Emacs doesn't kill it.
         (args (if ssh-target
                   (list tmux-project-terminal "-e" "ssh" "-t" ssh-target
                         "tmux" "attach-session" "-t" target)
                 (list tmux-project-terminal "-e"
                       "tmux" "attach-session" "-t" target))))
    (apply #'call-process "setsid" nil 0 nil args)
    (message "tmux pop: %s%s"
             (if ssh-target (format "%s " ssh-target) "")
             target)))

;; ---------------------------------------------------------------------------
;; Capture
;; ---------------------------------------------------------------------------

(defun tmux-project--capture (title)
  "Return captured text from pane titled TITLE in current project's window.
Errors if the pane doesn't exist."
  (let* ((window  (tmux-project--ensure-window))
         (pane-id (tmux-project--find-pane window title)))
    (unless pane-id
      (user-error "No `%s' pane in %s" title window))
    (tmux-project--tmux "capture-pane" "-p" "-J"
                        "-t" (format "%s.%s" window pane-id)
                        "-S" (format "-%d" tmux-project-capture-lines))))

(defun tmux-project--show-capture (title)
  "Capture pane TITLE and display in a read-only buffer."
  (let* ((text (tmux-project--capture title))
         (name (format "*tmux-%s: %s*" title (tmux-project--name)))
         (buf  (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-max))
      (view-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun tmux-project-capture ()
  "Capture the runner pane's output into a read-only buffer."
  (interactive)
  (tmux-project--show-capture "runner"))

;;;###autoload
(defun tmux-project-capture-claude ()
  "Capture the claude pane's output into a read-only buffer."
  (interactive)
  (tmux-project--show-capture "claude"))

(provide 'tmux-project)
;;; tmux-project.el ends here
