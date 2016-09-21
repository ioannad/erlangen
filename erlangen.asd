;;;; System definition for ERLANGEN.

(defsystem erlangen
  :description
  "Distributed asychronous message passing system for Clozure Common Lisp."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "Not licensed"
  :components ((:file "packages")
               (:file "conditions"
                      :depends-on ("packages"))
               (:file "mailbox"
                      :depends-on ("packages"))
               (:file "algorithms"
                      :depends-on ("packages"))
               (:file "agent"
                      :depends-on ("packages"
                                   "mailbox"
                                   "algorithms"
                                   "conditions"))
               (:file "registry"
                      :depends-on ("packages"
                                   "agent"))
               (:file "macros"
                      :depends-on ("packages"
                                   "algorithms"))
               (:file "distribution/call"
                      :depends-on ("packages"))
               (:file "distribution/protocol/buffers"
                      :depends-on ("packages"))
               (:file "distribution/protocol/common"
                      :depends-on ("packages"
                                   "distribution/protocol/buffers"))
               (:file "distribution/protocol/port-mapper"
                      :depends-on ("packages"
                                   "macros"
                                   "distribution/protocol/buffers"
                                   "distribution/protocol/common"))
               (:file "distribution/id"
                      :depends-on ("packages"
                                   "agent"))
               (:file "distribution/protocol/node"
                      :depends-on ("packages"
                                   "agent"
                                   "macros"
                                   "distribution/call"
                                   "distribution/protocol/buffers"
                                   "distribution/protocol/common"))
               (:file "erlangen"
                      :depends-on ("packages"
                                   "agent"
                                   "registry"
                                   "conditions"
                                   "macros"
                                   "distribution/call"))
               ;; Tests
               (:file "test/agent-test"
                      :depends-on ("agent"))
               (:file "test/port-mapper-test"
                      :depends-on ("distribution/protocol/port-mapper"
                                   "erlangen"))
               (:file "test/node-test"
                      :depends-on ("agent"
                                   "distribution/protocol/node"
                                   "distribution/protocol/port-mapper"
                                   "distribution/id")))
  :depends-on ("jpl-queues"
               "trivial-utf-8"
               "fast-io"
               "cl-conspack"
               "split-sequence"))
