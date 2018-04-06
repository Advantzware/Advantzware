/* ------------------------------------------------- cec/pr4-mcl1.i 05/97 JLF */
/* Update est-summ totals                                                       */
/* -------------------------------------------------------------------------- */

ASSIGN
 v-pct = probe.net-profit
 v-comm = ({1}.sell-price - (IF v-basis EQ "G" THEN {1}.fact-cost ELSE 0)) *
          (v-com / 100)           
 vmcl-desc = "Commission on " + (IF v-basis EQ "G" THEN "GM" ELSE "SP").

FIND FIRST est-summ
    WHERE est-summ.company  EQ probe.company
      AND est-summ.est-no   EQ probe.est-no
      AND est-summ.e-num    EQ probe.line
      AND est-summ.summ-tot BEGINS STRING("commission","x(20)")   +
                                   STRING(v-form-no,"9999999999") +
                                   SUBSTR(vmcl-desc,1,14)
    NO-ERROR.

IF NOT AVAIL est-summ THEN DO:
  {cec/est-summ.i vmcl-desc vmcl-cost}
  est-summ.summ-tot = STRING("commission","x(20)")   +
                      STRING(v-form-no,"9999999999") +
                      vmcl-desc.
END.
IF {2} THEN est-summ.per-m = 0.
ASSIGN
est-summ.per-m = est-summ.per-m + (v-comm * v-yld)
vmcl-desc = "    Commission %".

FIND FIRST est-summ
    WHERE est-summ.company  EQ probe.company
      AND est-summ.est-no   EQ probe.est-no
      AND est-summ.e-num    EQ probe.line
      AND est-summ.summ-tot EQ STRING("commission","x(20)")       +
                               STRING(v-form-no,"9999999999") +
                               vmcl-desc
    NO-ERROR.

IF NOT AVAIL est-summ THEN DO:
  {cec/est-summ.i vmcl-desc vmcl-cost}
  est-summ.summ-tot = STRING("commission","x(20)")       +
                      STRING(v-form-no,"9999999999") +
                      vmcl-desc.
END.
est-summ.per-m = v-com.

vmcl-desc = "FULL COST".
{cec/est-summ.i vmcl-desc vmcl-cost}
IF {2} THEN est-summ.per-m = 0.
est-summ.per-m = est-summ.per-m + ({1}.full-cost * v-yld).

IF v-rollfac THEN DO:
  vmcl-desc = "FULL COST PER ROLL".
  {cec/est-summ.i vmcl-desc vmcl-cost}
  IF {2} THEN est-summ.per-m = 0.
  est-summ.per-m = est-summ.per-m +
                   ({1}.full-cost * qm / (qty / xeb.quantityPerSet) * v-yld).
END.

IF ce-ctrl.sell-by EQ "S" THEN DO:
  vmcl-desc = "Margin on Fact Cost".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot BEGINS STRING("profit-s","x(20)")     +
                                     STRING(v-form-no,"9999999999") +
                                     vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("profit-s","x(20)")     +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  IF {2} THEN est-summ.per-m = 0.
  ASSIGN
  est-summ.per-m = est-summ.per-m + (v-prf-s * v-yld)
  vmcl-desc = "    Fact Margin %".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot EQ STRING("profit-s","x(20)")       +
                                 STRING(v-form-no,"9999999999") +
                                 vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("profit-s","x(20)")     +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  est-summ.per-m = v-pct-s.
END.

ELSE DO:
  vmcl-desc = "Net Margin".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot BEGINS STRING("profit","x(20)")       +
                                     STRING(v-form-no,"9999999999") +
                                     vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("profit","x(20)")       +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  IF {2} THEN est-summ.per-m = 0.
  /* for item's sell price override YSK 01/17/03 */

  IF xest.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
     ASSIGN
     est-summ.per-m = est-summ.per-m +
                      ( ({1}.sell-price - {1}.full-cost - ({1}.sell-price * probe.set-chg / 100)) * v-yld )
     v-pct = ROUND(({1}.sell-price - {1}.full-cost - ({1}.sell-price * probe.set-chg / 100)) / {1}.sell-price * 100,2)
     vmcl-desc = "    Net Margin %".
  ELSE
     ASSIGN
     est-summ.per-m = est-summ.per-m +
                      (({1}.sell-price - {1}.full-cost) * v-yld)
     v-pct = ROUND(({1}.sell-price - {1}.full-cost) / {1}.sell-price * 100,2)
     vmcl-desc = "    Net Margin %".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot EQ STRING("profit","x(20)")       +
                                 STRING(v-form-no,"9999999999") +
                                 vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("profit","x(20)")       +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  est-summ.per-m = v-pct.
END.

IF cerunc EQ "Fibre" THEN DO:
  vmcl-desc = "Available Margin".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot BEGINS STRING("avail margin","x(20)") +
                                     STRING(v-form-no,"9999999999") +
                                     vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("avail margin","x(20)") +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  IF {2} THEN est-summ.per-m = 0. 
  ASSIGN
  est-summ.per-m = est-summ.per-m +
                   (({1}.sell-price * probe.market-price / 100) * v-yld)
  vmcl-desc = "    Available Margin %".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot EQ STRING("avail margin","x(20)") +
                                 STRING(v-form-no,"9999999999") +
                                 vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("avail margin","x(20)") +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  est-summ.per-m = probe.market-price.
END.

IF xest.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
DO:
   vmcl-desc = "Matching Markup".
   FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot BEGINS STRING("matching markup","x(20)") +
                                     STRING(v-form-no,"9999999999") +
                                     vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("matching markup","x(20)") +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  IF {2} THEN est-summ.per-m = 0.
  ASSIGN
     est-summ.per-m = est-summ.per-m +
                      ({1}.sell-price * probe.set-chg / 100) * v-yld
     vmcl-desc = "    Matching Markup %".

  FIND FIRST est-summ
      WHERE est-summ.company  EQ probe.company
        AND est-summ.est-no   EQ probe.est-no
        AND est-summ.e-num    EQ probe.line
        AND est-summ.summ-tot EQ STRING("matching markup","x(20)") +
                                 STRING(v-form-no,"9999999999") +
                                 vmcl-desc
      NO-ERROR.

  IF NOT AVAIL est-summ THEN DO:
    {cec/est-summ.i vmcl-desc vmcl-cost}
    est-summ.summ-tot = STRING("matching markup","x(20)") +
                        STRING(v-form-no,"9999999999") +
                        vmcl-desc.
  END.
  est-summ.per-m = ROUND(probe.set-chg,2).
END.

vmcl-desc = "SELLING PRICE".
{cec/est-summ.i vmcl-desc vmcl-cost}
IF {2} THEN est-summ.per-m = 0.
est-summ.per-m = est-summ.per-m + ({1}.sell-price * v-yld).

IF v-rollfac THEN DO:
  vmcl-desc = "SELLING PRICE PER ROLL".
  {cec/est-summ.i vmcl-desc vmcl-cost}
  IF {2} THEN est-summ.per-m = 0.
  est-summ.per-m = est-summ.per-m +
                   ({1}.sell-price * qm / (qty / xeb.quantityPerSet) * v-yld).
END.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
