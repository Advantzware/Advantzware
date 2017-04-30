/* -------------------------------------------------- oe/invposty.p 03/00 JLF */
/* Invoicing  - Post Invoicing Transactions - Job Costing                     */
/* -------------------------------------------------------------------------- */
                               
DEF INPUT PARAM v-inv-no  LIKE inv-line.inv-no.
DEF INPUT PARAM v-i-no    LIKE itemfg.i-no.
DEF INPUT PARAM v-qty     LIKE inv-line.inv-qty.
DEF INPUT PARAM v-uom     LIKE itemfg.prod-uom.
DEF INPUT PARAM v-lab     LIKE itemfg.std-lab-cost.
DEF INPUT PARAM v-fix     LIKE itemfg.std-fix-cost.
DEF INPUT PARAM v-var     LIKE itemfg.std-var-cost.
DEF INPUT PARAM v-mat     LIKE itemfg.std-mat-cost.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}


FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.

FIND FIRST itemfg
    {sys/look/itemfgrlW.i}
      AND itemfg.i-no EQ v-i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN
FOR EACH prodl
    WHERE prodl.company EQ cocode
      AND prodl.procat  EQ itemfg.procat
    NO-LOCK,
    FIRST prod
    WHERE prod.company EQ cocode
      AND prod.prolin  EQ prodl.prolin
    NO-LOCK:

  RUN oe/invpostx.p (prod.cgs-dl, v-lab, v-qty, NO, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.fg-lab, v-lab, v-qty, YES, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.cgs-fo, v-fix, v-qty, NO, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.fg-fo, v-fix, v-qty, YES, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.cgs-vo, v-var, v-qty, NO, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.fg-vo, v-var, v-qty, YES, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.cgs-mat, v-mat, v-qty, NO, v-i-no, v-inv-no, v-uom).

  RUN oe/invpostx.p (prod.fg-mat, v-mat, v-qty, YES, v-i-no, v-inv-no, v-uom).

  LEAVE.
END.
 
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
