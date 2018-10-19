(defpackage erlangen.distribution-test
  (:use :cl
	:erlangen))

(spawn 'node)
(setf *agent-debug* t)

;; Precondition: on the local host there is a dedicated instrumentation node
;; `dist-integration', registered as :INSTRUMENT, and of course
;; erlangen-port-mapper must be running.
(defparameter *instrument*
  (format nil "~a/dist-integration/:INSTRUMENT" (machine-instance)))

;; let’s wait a while for *INSTRUMENT* to come up.
(sleep 5)

;(format *standard-output* "instrument node: ~a~%" *instrument*)


;; SPAWN tests

(defun test-remote-spawn-invalid-mode ()
  ;; spawning with invalid attachment mode
  (handler-case (spawn 'receive
		       :node "dist-integration"
		       :attach :invalid
		       :mailbox-size 1)
    (error (error)
      (declare (ignore error)))
    (:no-error ()
      (error "REMOTE-SPAWN succeeded with invalid mode."))))

(defun test-remote-spawn-invalid-call ()
    (handler-case (spawn '(42)
			 :node "dist-integration"
			 :attach nil
			 :mailbox-size 1)
      (error (error)
	(declare (ignore error)))
      (:no-error ()
	(error "REMOTE-SPAWN succeeded with invalid call."))))

(defun test-remote-spawn-monitor ()
  (let ((echo (spawn 'receive
		     :node "dist-integration"
		     :attach :monitor
		     :mailbox-size 1)))
    (send :foo echo)
    (assert (equal (receive :timeout 5)
		   `(,echo :ok :foo)))))

(defun test-remote-spawn-link ()
  (let ((linker (spawn (lambda ()
			 (let ((echo (spawn 'receive
					    :node "dist-integration"
					    :attach :link)))
			   (send :foo echo)
			   (assert (equal (receive :timeout 5)
					  `(,echo :ok :foo)))))
		       :attach :monitor)))
    (exit "fine" linker)
    (assert (equal (cons :exit "fine")
		   (rest (receive :timeout 5))))))

(defun test-remote-spawn ()
  (test-remote-spawn-invalid-mode)
  (test-remote-spawn-invalid-call)
  (test-remote-spawn-monitor)
  (test-remote-spawn-link))

;; SEND tests

(defun test-send-receive ()
  (let ((echo (spawn 'receive :node "dist-integration" :attach :monitor)))
    (send :foo echo)
    (assert (equal (receive :timeout 5)
		   `(,echo :ok :foo)))))

;; tests during downtime

(defun test-send-during-downtime ()
  (let ((echo (spawn 'receive :node "dist-integration" :attach :monitor)))
    (send '(:downtime 5) *instrument*) ; cause a downtime
    (sleep 2) ; wait a while for the remote to shut down
    (send :foo echo) ; this will be lost
    (send :bar echo) ; XXX - need two SEND to detect connection failure
    (sleep 5) ; wait until dist-integration comes up
    (send :baz echo) ; this should go through
    (assert (equal (receive :timeout 5)
		   `(,echo :ok :baz)))))


(format *standard-output* "testing remote spawn ..... ")

(test-remote-spawn)

(format *standard-output* "ok.~%testing send - receive ..... ") 

(test-send-receive)

(format *standard-output* "ok.~%testing send during downtime ..... ") 

(test-send-during-downtime)

(sleep 5)

(format *standard-output* "ok.~%All tests passed.~%")

(quit)
