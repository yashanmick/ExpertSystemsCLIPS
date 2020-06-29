;---------------------------
;template for contract facts
;---------------------------

(deftemplate contract "structure of a contract"
	(slot contract-id (type INTEGER))
	(slot car-luxury (type SYMBOL) (default FALSE))
	(slot car-power (type INTEGER))
	(slot car-displacement (type NUMBER))
	(slot new-customer (type SYMBOL) (default FALSE))
	(slot contract-class (type INTEGER) (default 15))
	(slot max-refund (type NUMBER) (default 2000000))
	(slot driver-age (type NUMBER))
	(slot driver-gender (type SYMBOL))
	(slot driver-city-population (type NUMBER))
	(slot base-price (type NUMBER))
	(slot final-price (type NUMBER) (default 0))
	(slot no-more-bonus (type SYMBOL) (default FALSE))
	)


;--------------------------------
; rules to calculate the final price
;--------------------------------

;rule based upon the city population: it has more salience than the
;	other rules since it may cancel other possible bonuses, this rule
;	directly uses the base price because no other bonus rule should 
;	have fired thanks to its higher salience

(defrule city-population "rule for city population"
	(declare (salience 20))
	?f <- (contract
			(no-more-bonus FALSE)
			(driver-city-population ?v_driver-city-population&:( < ?v_driver-city-population 100000))
			(base-price ?v_base-price)
		)

	=>
		(modify ?f
			(final-price (* 0.65 ?v_base-price))
			(no-more-bonus TRUE)
		)
	)

; this rule will change all remaining facts (with final-price still to 0)
; to have the final price set to the base price

(defrule reset-price "final price to base price"
	(declare (salience 15))
	?f <- (contract
			(no-more-bonus FALSE)
			(base-price ?v_base-price)
			(final-price 0)
		)
	=>
		(modify ?f
			(final-price ?v_base-price)
		)
	)

;----------------------------------
; normal *bonus* rules
;----------------------------------

; a driver more than 26 years old has a bonus of 10% on the final price

(defrule older-driver "bonus for older driver"
	(declare (salience 10))
	?f <- (contract
			(no-more-bonus FALSE)
			(driver-age ?a&:(> ?a 26))
			(final-price ?v_final-price)
		)
	=>
		(modify ?f
			(final-price (* 0.9 ?v_final-price))
			(driver-age 0) ; don't reevaluate
		)
	)


; a woman on a car of less than 1.3 liter displacement has a 5% bonus

(defrule woman-small-car "bonus for woman on small cars"
	(declare (salience 10))
	?f <- (contract
			(no-more-bonus FALSE)
			(driver-gender FEMALE)
			(car-displacement ?a&:(< ?a 1.3))
			(final-price ?v_final-price)
		)
	=>
		(modify ?f
			(final-price (* 0.95 ?v_final-price))
			(car-displacement 1000) ; don't reevaluate
		)
	)


; new customers (less than 30 days) have a special 50 USD discount

(defrule new-customer-discount "discount for newly acquired customers"
	(declare (salience 10))
	?f <- (contract
			(no-more-bonus FALSE)
			(new-customer TRUE)
			(final-price ?v_final-price)
		)
	=>
		(modify ?f
			(final-price ( - ?v_final-price 50))
			(new-customer FALSE) ; don't reevaluate
		)
	)

;---------------------------------------
; normal *extra cost* rules
;---------------------------------------

;luxury cars will pay an extra 10% on calculated price

(defrule luxury-car-extra "price higher for luxury cars"
	(declare (salience 5))
	?f <- (contract
			(car-luxury TRUE)
			(final-price ?v_final-price)
		)
	=>
		(modify ?f
			(final-price (* ?v_final-price 1.1))
			(car-luxury FALSE) ; don't reevaluate
		)
	)


; powerful cars will pay an extra 100 USD on calculated price

(defrule power-extra "price higher for power cars"
	(declare (salience 5))
	?f <- (contract
			(car-power ?a&:(> ?a 100))			
			(final-price ?v_final-price)
		)
	=>
		(modify ?f
			(final-price ( + ?v_final-price 100))
			(car-power 0) ; don't reevaluate
		)
	)



;end

