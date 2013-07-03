;;;; deluge-client.lisp

(in-package #:deluge-client)

;;; "deluge-client" goes here. Hacks and glory await!

(defparameter *default-config-values*
  '("add_paused" "compact_allocation" "download_location"
    "max_connections_per_torrent" "max_download_speed_per_torrent"
    "move_completed" "move_completed_path" "max_upload_slots_per_torrent"
    "max_upload_speed_per_torrent" "prioritize_first_last_pieces"))

(defparameter *default-update-ui-values*
  '("queue" "name" "total_size" "state" "progress" "num_seeds"
    "total_seeds" "num_peers" "total_peers" "download_payload_rate"
    "upload_payload_rate" "eta" "ratio" "time_added" "tracker_host"
    "save_path" "total_done" "total_uploaded" "seeds_peers_ratio"))

(let ((hosts nil))
  (defun login (password &optional (host "localhost") (port 8112))
    (deluge:set-host host port)
    (if (deluge:login password)
        (progn
          (setf hosts (mapcar (lambda (h) (deluge:get-host-status (car h)))
                              (deluge:get-hosts)))
          (loop
             for h in hosts
             for i = 0 then (1+ i)
             do (format t "[~a] ~{~a~^ ~}~%" i (cdr h))))
        (format t "Login failed!")))
  (defun connect (n)
    (deluge:connect (car (nth n hosts)))
    (if (deluge:connected)
        (format t "Connected")
        (format t "Failed to connect!"))))

(defun refresh (&optional state tracker &rest params)
  (apply #'deluge:update-ui
         state tracker (or params *default-update-ui-values*)))

(defun torrent+ (path)
  (let ((response (deluge:upload-torrent *host* *port* (pathname path))))
    (when (not (and (deluge:success-p response)
                    (deluge:deluge-success-p response)))
      (error "Upload failed!"))
    (let ((config (apply #'deluge:get-config-values *default-config-values*))
          (file (car (deluge:deluge-result response))))
      ;; The length of the priority list must equal the
      ;; number of paths specified. For now, since we're
      ;; only uploading one file at a time, we can hard-code
      ;; this.
      (setf (gethash "file_priorities" config) '(1))
      (deluge:add-torrent file config))))
