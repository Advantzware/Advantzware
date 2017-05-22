/* ------------------------------------------------- cec/probepr1.i 10/96 JLF */

ASSIGN
v-yld = if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty
v-comm = (probeit.sell-price - (IF v-basis EQ "G" THEN probeit.fact-cost ELSE 0)) *
         (v-com / 100).

IF cerunc-dec EQ 0 THEN
DO:
    put "Commission"
        string(v-com,">>9.99%")               to 30
	    v-comm * v-yld                        to {1} format ">>>,>>9.99"
	    v-comm * v-yld * qm                   to 80  format ">>>>,>>9.99" skip.

    put "FULL COST" probeit.full-cost * v-yld to {1} format ">>>,>>9.99"
        probeit.full-cost * v-yld * qm        to 80  format ">>>>,>>9.99" skip.

    if v-rollfac then
      put "FULL COST PER ROLL"
          probeit.full-cost * v-yld      / (qty / xeb.yld-qty) to {1} format ">>>,>>9.99"
	      probeit.full-cost * v-yld * qm / (qty / xeb.yld-qty) to 80  format ">>>>,>>9.99".

    IF ce-ctrl.sell-by EQ "S" THEN
       put "Markup on Fact Cost"         format "x(19)"
           string(v-pct-s,"->>9.99%")    to 29
           v-prf-s * v-yld               to {1} format ">>>,>>9.99"
           v-prf-s * v-yld * qm          to 80  format ">>>>,>>9.99" skip.

    put "Net Margin"
	    string(probeit.net-profit,"->>9.99%")                 to 29
        probeit.sell-price * (v-pct / 100) * v-yld            to {1} format "->>,>>9.99"
	    probeit.sell-price * (v-pct / 100) * v-yld * qm       to 80  format "->>>,>>9.99" skip.

    IF cerunc EQ "Fibre" THEN
       PUT "Available Margin"
           STRING(probe.market-price,"->>9.99%")               TO 29
           probe.sell-price * (probe.market-price / 100)       TO {1} FORMAT "->>,>>9.99"
           probe.sell-price * (probe.market-price / 100) * qm  TO 80  FORMAT "->>>,>>9.99"
           SKIP.

    put "SELLING PRICE"
        probeit.sell-price * v-yld                            to {1} format ">>>,>>9.99"
        probeit.sell-price * v-yld * qm                       to 80  format ">>>>,>>9.99" skip.

    if v-rollfac then
       put "SELLING PRICE PER ROLL"
           probeit.sell-price * v-yld      / (qty / xeb.yld-qty) to {1} format ">>>,>>9.99"
	       probeit.sell-price * v-yld * qm / (qty / xeb.yld-qty) to 80  format ">>>>,>>9.99" skip.
END.
ELSE
DO:
   put "Commission"
       string(v-com,">>9.99%")               to 30
	   v-comm * v-yld                        to {1} format ">,>>>,>>9.99"
	   v-comm * v-yld * qm                   to 80  format ">,>>>,>>9.99" skip.

   put "FULL COST" probeit.full-cost * v-yld to {1} format ">,>>>,>>9.99"
       probeit.full-cost * v-yld * qm        to 80  format ">,>>>,>>9.99" skip.

   if v-rollfac then
      put "FULL COST PER ROLL"
          probeit.full-cost * v-yld      / (qty / xeb.yld-qty) to {1} format ">,>>>,>>9.99"
	      probeit.full-cost * v-yld * qm / (qty / xeb.yld-qty) to 80  format ">,>>>>,>>9.99".

   IF ce-ctrl.sell-by EQ "S" THEN
      put "Markup on Fact Cost"         format "x(19)"
          string(v-pct-s,"->>9.99%")    to 29
          v-prf-s * v-yld               to {1} format ">,>>>,>>9.99"
          v-prf-s * v-yld * qm          to 80  format ">,>>>,>>9.99" skip.

   put "Net Margin"
	   string(probeit.net-profit,"->>9.99%")                 to 29
       probeit.sell-price * (v-pct / 100) * v-yld            to {1} format "->,>>>,>>9.99"
	   probeit.sell-price * (v-pct / 100) * v-yld * qm       to 80  format "->,>>>,>>9.99" skip.

   IF cerunc EQ "Fibre" THEN
      PUT "Available Margin"
          STRING(probe.market-price,"->>9.99%")               TO 29
          probe.sell-price * (probe.market-price / 100)       TO {1} FORMAT "->,>>>,>>9.99"
          probe.sell-price * (probe.market-price / 100) * qm  TO 80  FORMAT "->,>>>,>>9.99"
          SKIP.

   put "SELLING PRICE"
        probeit.sell-price * v-yld                            to {1} format ">>,>>>,>>9.99"
        probeit.sell-price * v-yld * qm                       to 80  format ">>,>>>,>>9.99" skip.

   if v-rollfac then
      put "SELLING PRICE PER ROLL"
          probeit.sell-price * v-yld      / (qty / xeb.yld-qty) to {1} format ">>,>>>,>>9.99"
	      probeit.sell-price * v-yld * qm / (qty / xeb.yld-qty) to 80  format ">>,>>>,>>9.99" skip.   
END.
put skip(1).
