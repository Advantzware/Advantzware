DEF INPUT-OUTPUT PARAMETER iop-pur-uom LIKE fg-bin.pur-uom NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopQty LIKE fg-bin.qty NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopPartial-count LIKE fg-bin.partial-count NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopStd-Tot-Cost LIKE fg-bin.std-tot-cost NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopStd-mat-cost LIKE fg-bin.std-mat-cost NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopStd-lab-cost LIKE fg-bin.std-lab-cost NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopStd-var-cost LIKE fg-bin.std-var-cost NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iopStd-fix-cost LIKE fg-bin.std-fix-cost NO-UNDO.
DEF INPUT PARAMETER iopProdUom   LIKE itemfg.prod-uom NO-UNDO.
DEF INPUT PARAMETER iopt-qty     LIKE fg-rctd.t-qty NO-UNDO.
DEF INPUT PARAMETER ioppartial   LIKE fg-rctd.partial NO-UNDO.
DEF INPUT PARAMETER iopjob-no    LIKE fg-rctd.job-no NO-UNDO.
DEF INPUT PARAMETER iopcompany   LIKE fg-rctd.company NO-UNDO.
DEF INPUT PARAMETER iopjob-no2   LIKE fg-rctd.job-no2 NO-UNDO.
DEF INPUT PARAMETER iopi-no      LIKE fg-rctd.i-no NO-UNDO.
DEF INPUT PARAMETER iopRita-code      LIKE fg-rctd.rita-code NO-UNDO.
DEFINE INPUT  PARAMETER ipcLv-uom AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdCvtCost AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opdCost AS DECIMAL     NO-UNDO. /* for v-cost */
DEFINE OUTPUT PARAMETER opdBinQty AS DECIMAL   NO-UNDO. /* for v-binqty */
DEFINE OUTPUT PARAMETER opdQty AS DECIMAL      NO-UNDO. /* for v-qty */
DEFINE OUTPUT PARAMETER opdTagCost AS DECIMAL      NO-UNDO. /* for v-tagcost */

DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-binqty AS INT NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR v-tagcost AS DEC NO-UNDO.

if iop-pur-uom eq "" then iop-pur-uom = iopProdUom.

assign                         
 v-binqty = iopQty   * (if iopQty   lt 0 then -1 else 1)
 v-qty    = iopt-qty * (if iopt-qty lt 0 then -1 else 1).

IF iop-pur-uom EQ "M" THEN
  v-tagcost = iopStd-Tot-Cost.
ELSE
  RUN sys/ref/convcuom.p(iop-pur-uom, "M", 0, 0, 0, 0,
                         iopStd-Tot-Cost, OUTPUT v-tagcost).

IF ipcLv-uom EQ "M" THEN
  v-cost = ipdCvtCost.
ELSE
  RUN sys/ref/convcuom.p(ipcLv-uom, "M", 0, 0, 0, 0, ipdCvtCost, OUTPUT v-cost).

ASSIGN
 iopQty           = iopQty + iopt-qty
 iopPartial-count = iopPartial-count + ioppartial.

IF iopQty EQ 0 THEN v-binqty = 0.

IF ioprita-code NE "A" OR v-cost NE 0 THEN DO:
  IF ioprita-code EQ "A" AND v-cost NE 0 THEN
    iopStd-Tot-Cost = ((v-cost * v-binqty) + (v-cost * v-qty)) /
                       (v-binqty + v-qty).
  ELSE
    iopStd-Tot-Cost = ((v-tagcost * v-binqty) + (v-cost * v-qty)) /
                       (v-binqty + v-qty).

  IF iopProdUom NE "M" THEN
    RUN sys/ref/convcuom.p("M", iopProdUom, 0, 0, 0, 0,
                           iopStd-Tot-Cost, OUTPUT iopStd-Tot-Cost).
END.

iop-pur-uom = iopProdUom.

RELEASE reftable.

IF iopjob-no NE "" AND NOT AVAIL job-hdr THEN DO:
  FIND FIRST job
      WHERE job.company EQ iopcompany
        AND job.job-no  EQ iopjob-no
        AND job.job-no2 EQ iopjob-no2
      USE-INDEX job NO-LOCK NO-ERROR.
  IF AVAIL job THEN
  FIND FIRST reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job.job,"999999999")
        AND reftable.code2    EQ iopi-no
      USE-INDEX reftable NO-LOCK NO-ERROR.
END.

IF AVAIL reftable AND reftable.val[5] NE 0 THEN
  ASSIGN
   v-cost           = reftable.val[5]
   iopStd-mat-cost = iopStd-Tot-Cost * (reftable.val[2] / v-cost)
   iopStd-lab-cost = iopStd-Tot-Cost * (reftable.val[1] / v-cost)
   iopStd-var-cost = iopStd-Tot-Cost * (reftable.val[3] / v-cost)
   iopStd-fix-cost = iopStd-Tot-Cost * (reftable.val[4] / v-cost).

ELSE
IF AVAIL job-hdr AND job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                      job-hdr.std-var-cost + job-hdr.std-fix-cost NE 0 THEN
  ASSIGN
   v-cost           = job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                      job-hdr.std-var-cost + job-hdr.std-fix-cost
   iopStd-mat-cost = iopStd-Tot-Cost * (job-hdr.std-mat-cost / v-cost)
   iopStd-lab-cost = iopStd-Tot-Cost * (job-hdr.std-lab-cost / v-cost)
   iopStd-var-cost = iopStd-Tot-Cost * (job-hdr.std-var-cost / v-cost)
   iopStd-fix-cost = iopStd-Tot-Cost * (job-hdr.std-fix-cost / v-cost).

ELSE
  ASSIGN
   iopStd-mat-cost = iopStd-Tot-Cost
   iopStd-lab-cost = 0
   iopStd-var-cost = 0
   iopStd-fix-cost = 0.

iopStd-Tot-Cost = iopStd-mat-cost + iopStd-lab-cost +
                   iopStd-var-cost + iopStd-fix-cost.

ASSIGN opdCost    = v-cost
       opdBinQty  = v-binqty
       opdQty     = v-qty
       opdTagCost = v-tagcost.
