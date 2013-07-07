;;;; deluge-client.lisp

(in-package #:deluge-client)

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
        (hosts)
        (format t "Login failed!")))
  (defun hosts ()
    (setf hosts (mapcar (lambda (h) (deluge:get-host-status (car h)))
                        (deluge:get-hosts)))
    (loop
       for h in hosts
       for i = 0 then (1+ i)
       do (format t "[~a] ~{~a~^ ~}~%" i (cdr h))))
  (defun connect (n)
    (deluge:connect (car (nth n hosts)))
    (if (deluge:connected)
        (format t "Connected")
        (format t "Failed to connect!"))))

(let (torrents stats torrent-ids torrent-tree)
  (defun refresh (&key state tracker (params *default-update-ui-values*) (sort :name))
    (let ((ui (apply #'deluge:update-ui
                     (state-to-string state)
                     (tracker-to-string tracker)
                     params)))
      (setf stats (gethash "stats" ui)
            torrents (gethash "torrents" ui)
            torrent-ids (make-hash-table)
            torrent-tree (make-node)))
    (stats)
    (format t "~%")
    (torrents sort))
  (defun stats ()
    (with-hash-table-values (stats)
      (format t "U:~a/s D:~a/s Free space:~a~%"
              (readable-bytes >upload-rate)
              (readable-bytes >download-rate)
              (readable-bytes >free-space))))
  (defun torrent-lessp (t1 t2 sort)
    "Compares two torrents based on the key specified in the sort parameter"
    (let* ((k (to-key sort))
           (v1 (gethashes (t1 k) torrents))
           (v2 (gethashes (t2 k) torrents))
           (lt (cond
                 ((numberp v1) #'<)
                 ((stringp v1) #'string-lessp)
                 (t (error "Don't know how to compare that!")))))
      (funcall lt v1 v2)))  
  (defun torrents (sort)
    (loop
       for k being the hash-keys in torrents
       using (hash-value v)
       for i = 0 then (1+ i)
       do
         (bst-insert torrent-tree k #'(lambda (k1 k2) (torrent-lessp k1 k2 sort))))
    (let ((i 0))
      (bst-traverse
       torrent-tree
       #'(lambda (k)
           (let ((v (gethash k torrents)))
             (setf (gethash (incf i) torrent-ids) k)
             (print-torrent v i))))))
  (defun pause (id)
    (deluge:pause-torrent (gethash id torrent-ids)))
  (defun resume (id)
    (deluge:resume-torrent (gethash id torrent-ids)))
  (defun rm (id &optional with-data)
    (deluge:remove-torrent (gethash id torrent-ids) with-data)))

(defun print-torrent (torrent index)
  (with-hash-table-values (torrent)
    (format t "[~a] ~a~%    ~a -- D:~a/s U:~a/s Ratio:~$~%"
            index >name >state
            (readable-bytes >download-payload-rate)
            (readable-bytes >upload-payload-rate)
            >ratio)))

(defun torrent+ (path)
  (let ((response (deluge:upload-torrent (pathname path))))
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
