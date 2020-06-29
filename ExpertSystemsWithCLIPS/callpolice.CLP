(deftemplate fire(slot id) (slot type))
(assert (fire(id 1) (type "normal")))
(deftemplate police (slot action))

(defrule notify
(fire (type "irregent")) => (printout t "calling police" crlf))

(defrule notify2
(fire (type "irregent")) => (assert (police (action "call police"))))

(assert (fire (id 2) (type "irregent")))
