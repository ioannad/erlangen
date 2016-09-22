;;;; port-mapper: Standalone port mapper daemon.

(require :asdf)

(asdf:load-system :erlangen)

(defun port-mapper ()
  (when (intersection *command-line-argument-list*
                      '("-h" "-help" "--help")
                      :test 'string=)
    (princ "Usage: erlangen-port-mapper [host] [registry-port] [directory-port]
       erlangen-port-mapper -h|-help|--help")
    (quit 0))
  (destructuring-bind (exe &optional host rport dport)
      *command-line-argument-list*
    (declare (ignore exe))
    (apply 'erlangen.distribution.protocol.port-mapper:port-mapper
           (append (when host
                     `(:registry-host ,host :directory-host ,host))
                   (when rport
                     `(:registry-port ,(parse-integer rport)))
                   (when dport
                     `(:directory-port ,(parse-integer dport)))))))

(gc)

(save-application
 "bin/erlangen-port-mapper"
 :toplevel-function 'port-mapper
 :error-handler :quit
 :purify t
 :prepend-kernel t)