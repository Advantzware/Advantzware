/* ------------------------------------------------- cec/probepr1.i 10/96 JLF */

v-yld = IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE eb.quantityPerSet.

    
v-comm = (probeit.sell-price - (IF v-basis EQ "G" THEN probeit.fact-cost ELSE 0)) *
         (v-com / 100) * qm.



 IF cerunf EQ "Dee" THEN
  PUT "Royalty"                                     FORMAT "x(19)"
      STRING(probeit.pctRoyalty,">>9.99%")            TO 30
      probeit.totCostRoyalty / qm                         TO {1} FORMAT ">>>,>>9.99"
      probeit.totCostRoyalty                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Warehouse Markup"                            FORMAT "x(19)"
      STRING(probeit.pctWarehouse,">>9.99%")            TO 30
      probeit.totCostWarehousr / qm                         TO {1} FORMAT ">>>,>>9.99"
      probeit.totCostWarehousr                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Customer Markup"                             FORMAT "x(19)"
      STRING(probeit.pctCustMargin,">>9.99%")            TO 30
      probeit.totCostCustMargin / qm                         TO {1} FORMAT ">>>,>>9.99"
      probeit.totCostCustMargin                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "Commission"                                  FORMAT "x(19)"
      STRING(probeit.pctCommission,">>9.99%")            TO 30
      probeit.totCostCommission / qm                         TO {1} FORMAT ">>>,>>9.99"
      probeit.totCostCommission                              TO 80  FORMAT ">>>>,>>9.99"
      SKIP

      "NON MANF. COST"                              FORMAT "x(19)"
      (probeit.totCostCommission + probeit.totCostRoyalty +
       probeit.totCostWarehousr + probeit.totCostCustMargin) / qm    TO {1} FORMAT ">>>,>>9.99"
      (probeit.totCostCommission + probeit.totCostRoyalty +
       probeit.totCostWarehousr + probeit.totCostCustMargin)         TO 80  FORMAT ">>>>,>>9.99"
      SKIP.


ELSE
  PUT "Commission on " +
      (IF v-basis EQ "G" THEN "GM"
                         ELSE "SP")     FORMAT "x(19)"
      STRING(v-com,">>9.99%") TO 30     
      v-comm / qm TO {1}                FORMAT ">>>,>>9.99"
      v-comm      TO 80                 FORMAT ">>>>,>>9.99"
      SKIP.

put "FULL COST" probeit.full-cost * v-yld to {1} format ">>>,>>9.99"
    probeit.full-cost * v-yld * qm        to 80  format ">>>>,>>9.99" skip.

IF ce-ctrl.sell-by EQ "S" THEN
  put "Markup on Fact Cost"         format "x(19)"
      string(v-pct-s,"->>>9.99%")    to 29
      v-prf-s * v-yld               to {1} format "->>,>>9.99"
      v-prf-s * v-yld * qm          to 80  format "->>>,>>9.99" skip.

put "Net Margin"
	string(probeit.net-profit,"->>>9.99%")                 to 29
    probeit.sell-price * (v-pct / 100) * v-yld            to {1} format "->>,>>9.99"
	probeit.sell-price * (v-pct / 100) * v-yld * qm       to 80  format "->>>,>>9.99" skip.

IF cerunf EQ "Fibre" THEN
  PUT "Available Margin"
      STRING(probe.market-price,"->>>9.99%") TO 29
      probeit.sell-price * (probe.market-price / 100)      TO {1} FORMAT "->>,>>9.99"
      probeit.sell-price * (probe.market-price / 100) * qm TO 80  FORMAT "->>>,>>9.99"
      SKIP.

put "SELLING PRICE"
    probeit.sell-price * v-yld                            to {1} format "->>,>>9.99"
    probeit.sell-price * v-yld * qm                       to 80  format "->>>,>>9.99" skip.

put skip(1).
