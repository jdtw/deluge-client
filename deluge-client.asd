;;;; deluge-client.asd

(asdf:defsystem #:deluge-client
  :serial t
  :description "Deluge torrent client"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:deluge)
  :components ((:file "package")
               (:file "util")
               (:file "tree")
               (:file "deluge-client")))

