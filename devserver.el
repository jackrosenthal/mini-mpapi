(defvar server-process nil)
(defun make-server-process ()
  (setq server-process
        (start-process "devserver" "*devserver*"
                       "racket" "server.rkt" "-c" "config.rktd"))
  (pop-to-buffer "*devserver*"))
(make-server-process)
(defun restart-server ()
  (interactive)
  (delete-process server-process)
  (with-current-buffer "*devserver*"
    (erase-buffer)
    (point-min)
    (insert "==> ")
    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
    (insert " <==\n"))
  (make-server-process))
