/* -------------------------------------------------- ce/probepr2.i 10/96 JLF */

FIND FIRST probe-ref NO-LOCK
    WHERE probe-ref.reftable EQ "probe-ref"
      AND probe-ref.company  EQ probe.company
      AND probe-ref.loc      EQ ""
      AND probe-ref.code     EQ probe.est-no
      AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.

v-comm = (probe.sell-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0)) *
         (v-com / 100) * qm.

IF cerunf EQ "Dee" AND AVAIL probe-ref THEN
  PUT "Royalty"                                     FORMAT "x(19)"
      STRING(probe-ref.val[3] / 100000,">>9.99%")   TO 30
      probe-ref.val[7] / qm                         TO {1} FORMAT ">>>,>>9.99"
      probe-ref.val[7]                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Warehouse Markup"                            FORMAT "x(19)"
      STRING(probe-ref.val[4] / 100000,">>9.99%")   TO 30
      probe-ref.val[8] / qm                         TO {1} FORMAT ">>>,>>9.99"
      probe-ref.val[8]                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Customer Markup"                             FORMAT "x(19)"
      STRING(probe-ref.val[5] / 100000,">>9.99%")   TO 30
      probe-ref.val[9] / qm                         TO {1} FORMAT ">>>,>>9.99"
      probe-ref.val[9]                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Commission"                                  FORMAT "x(19)"
      STRING(probe-ref.val[2] / 100000,">>9.99%")   TO 30
      probe-ref.val[6] / qm                         TO {1} FORMAT ">>>,>>9.99"
      probe-ref.val[6]                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "NON MANF. COST"                              FORMAT "x(19)"
      (probe-ref.val[6] + probe-ref.val[7] +
       probe-ref.val[8] + probe-ref.val[9]) / qm    TO {1} FORMAT ">>>,>>9.99"
      (probe-ref.val[6] + probe-ref.val[7] +
       probe-ref.val[8] + probe-ref.val[9])         TO 80  FORMAT ">>>>,>>9.99"
      SKIP.
ELSE
  PUT "Commission on " +
      (IF v-basis EQ "G" THEN "GM"
                         ELSE "SP")     FORMAT "x(19)"
      STRING(v-com,">>9.99%") TO 30     
      v-comm / qm TO {1}                FORMAT ">>>,>>9.99"
      v-comm      TO 80
      SKIP.

put "FULL COST" probe.full-cost         to {1} format ">>>,>>9.99"
    probe.full-cost * qm                to 80  format ">>>>,>>9.99" skip.

IF ce-ctrl.sell-by EQ "S" THEN
  put "Margin on Fact Cost"
      string(v-pct-s,"->>>9.99%")        to 29
      v-prf-s                           to {1} format "->>,>>9.99"
	  v-prf-s * qm                      to 80  format "->>>,>>9.99" skip.

put "Net Margin"
	string(probe.net-profit,"->>>9.99%")              to 29
    probe.sell-price * (probe.net-profit / 100)      to {1} format "->>,>>9.99"
	probe.sell-price * (probe.net-profit / 100) * qm to 80  format "->>>,>>9.99" skip.

IF cerunf EQ "Fibre" THEN
  PUT "Available Margin"
      STRING(probe.market-price,"->>>9.99%") TO 29
      probe.sell-price * (probe.market-price / 100)      TO {1} FORMAT "->>,>>9.99"
      probe.sell-price * (probe.market-price / 100) * qm TO 80  FORMAT "->>>,>>9.99"
      SKIP.

put "SELLING PRICE"
    probe.sell-price      to {1} format "->>,>>9.99"
    probe.sell-price * qm to 80  format "->>>,>>9.99" skip.

put skip(1).
