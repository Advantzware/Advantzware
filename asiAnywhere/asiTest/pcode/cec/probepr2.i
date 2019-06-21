/* ------------------------------------------------- cec/probepr2.i 10/96 JLF */

v-comm = (probe.sell-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0)) *
         (v-com / 100).

IF cerunc-dec EQ 0 THEN
DO:
   put "Commission"
       string(v-com,">>9.99%")             to 30
	   v-comm                              to {1} format ">>>,>>9.99"
	   v-comm * qm                         to 80  format ">>>>,>>9.99" skip.

   put "FULL COST" probe.full-cost         to {1} format ">>>,>>9.99"
       probe.full-cost * qm                to 80  format ">>>>,>>9.99" skip.

   IF ce-ctrl.sell-by EQ "S" THEN
      put "Margin on Fact Cost"
          string(v-pct-s,"->>9.99%")        to 29
          v-prf-s                           to {1} format ">>>,>>9.99"
	      v-prf-s * qm                      to 80  format ">>>>,>>9.99" skip.

   put "Net Margin"
       string(probe.net-profit,"->>9.99%")              to 29
       probe.sell-price * (probe.net-profit / 100)      to {1} format "->>,>>9.99"
	   probe.sell-price * (probe.net-profit / 100) * qm to 80  format "->>>,>>9.99" skip.

   IF cerunc EQ "Fibre" THEN
      PUT "Available Margin"
          STRING(probe.market-price,"->>9.99%")              TO 29
          probe.sell-price * (probe.market-price / 100)      TO {1} FORMAT "->>,>>9.99"
          probe.sell-price * (probe.market-price / 100) * qm TO 80  FORMAT "->>>,>>9.99"
          SKIP.

   put "SELLING PRICE"
       probe.sell-price      to {1} format ">>>,>>9.99"
       probe.sell-price * qm to 80  format ">>>>,>>9.99" skip.
END.
ELSE
DO:
   put "Commission"
       string(v-com,">>9.99%")             to 30
	   v-comm                              to {1} format ">,>>>,>>9.99"
	   v-comm * qm                         to 80  format ">,>>>,>>9.99" skip.

   put "FULL COST" probe.full-cost         to {1} format ">,>>>,>>9.99"
       probe.full-cost * qm                to 80  format ">,>>>,>>9.99" skip.

   IF ce-ctrl.sell-by EQ "S" THEN
      put "Margin on Fact Cost"
          string(v-pct-s,"->>9.99%")        to 29
          v-prf-s                           to {1} format ">,>>>,>>9.99"
	      v-prf-s * qm                      to 80  format ">,>>>,>>9.99" skip.

   put "Net Margin"
       string(probe.net-profit,"->>9.99%")              to 29
       probe.sell-price * (probe.net-profit / 100)      to {1} format "->,>>>,>>9.99"
	   probe.sell-price * (probe.net-profit / 100) * qm to 80  format "->,>>>,>>9.99" skip.

   IF cerunc EQ "Fibre" THEN
      PUT "Available Margin"
          STRING(probe.market-price,"->>9.99%")              TO 29
          probe.sell-price * (probe.market-price / 100)      TO {1} FORMAT "->,>>>,>>9.99"
          probe.sell-price * (probe.market-price / 100) * qm TO 80  FORMAT "->,>>>,>>9.99"
          SKIP.

   put "SELLING PRICE"
       probe.sell-price      to {1} format ">>,>>>,>>9.99"
       probe.sell-price * qm to 80  format ">>,>>>,>>9.99" skip.
END.
put skip(1).
