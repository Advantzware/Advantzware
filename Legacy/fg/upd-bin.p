/* DEF INPUT-OUTPUT PARAMETER iop-pur-uom LIKE fg-bin.pur-uom NO-UNDO.             */
/* DEF INPUT-OUTPUT PARAMETER iopQty LIKE fg-bin.qty NO-UNDO.                      */
/* DEF INPUT-OUTPUT PARAMETER iopPartial-count LIKE fg-bin.partial-count NO-UNDO.  */
/* DEF INPUT-OUTPUT PARAMETER iopStd-Tot-Cost LIKE fg-bin.std-tot-cost NO-UNDO.    */
/* DEF INPUT-OUTPUT PARAMETER iopStd-mat-cost LIKE fg-bin.std-mat-cost NO-UNDO.    */
/* DEF INPUT-OUTPUT PARAMETER iopStd-lab-cost LIKE fg-bin.std-lab-cost NO-UNDO.    */
/* DEF INPUT-OUTPUT PARAMETER iopStd-var-cost LIKE fg-bin.std-var-cost NO-UNDO.    */
/* DEF INPUT-OUTPUT PARAMETER iopStd-fix-cost LIKE fg-bin.std-fix-cost NO-UNDO.    */
/* DEF INPUT PARAMETER iopProdUom   LIKE itemfg.prod-uom NO-UNDO.                  */
/* DEF INPUT PARAMETER iopt-qty     LIKE fg-rctd.t-qty NO-UNDO.                    */
/* DEF INPUT PARAMETER ioppartial   LIKE fg-rctd.partial NO-UNDO.                  */
/* DEF INPUT PARAMETER iopjob-no    LIKE fg-rctd.job-no NO-UNDO.                   */
/* DEF INPUT PARAMETER iopcompany   LIKE fg-rctd.company NO-UNDO.                  */
/* DEF INPUT PARAMETER iopjob-no2   LIKE fg-rctd.job-no2 NO-UNDO.                  */
/* DEF INPUT PARAMETER iopi-no      LIKE fg-rctd.i-no NO-UNDO.                     */
/* DEF INPUT PARAMETER iopRita-code      LIKE fg-rctd.rita-code NO-UNDO.           */
/* DEFINE INPUT  PARAMETER ipcLv-uom AS CHARACTER   NO-UNDO.                       */
/* DEFINE INPUT  PARAMETER ipdCvtCost AS DECIMAL     NO-UNDO.                      */
/* DEFINE OUTPUT PARAMETER opdCost AS DECIMAL     NO-UNDO. /* for v-cost */        */
/* DEFINE OUTPUT PARAMETER opdBinQty AS DECIMAL   NO-UNDO. /* for v-binqty */      */
/* DEFINE OUTPUT PARAMETER opdQty AS DECIMAL      NO-UNDO. /* for v-qty */         */
/* DEFINE OUTPUT PARAMETER opdTagCost AS DECIMAL      NO-UNDO. /* for v-tagcost */ */

/* RUN fg/upd-bin.p (INPUT-OUTPUT    tt-fg-bin.pur-uom         */
/*                   INPUT-OUTPUT    tt-fg-bin.qty ,           */
/*                   INPUT-OUTPUT    tt-fg-bin.partial-count , */
/*                   INPUT-OUTPUT    tt-fg-bin.std-tot-cost ,  */
/*                   INPUT-OUTPUT    tt-fg-bin.std-mat-cost ,  */
/*                   INPUT-OUTPUT    tt-fg-bin.std-lab-cost ,  */
/*                   INPUT-OUTPUT    tt-fg-bin.std-var-cost ,  */
/*                   INPUT-OUTPUT    tt-fg-bin.std-fix-cost ,  */
/*                   INPUT           itemfg.prod-uom ,         */
/*                   INPUT           fg-rctd.t-qty ,           */
/*                   INPUT           fg-rctd.partial ,         */
/*                   INPUT           fg-rctd.job-no ,          */
/*                   INPUT           fg-rctd.company ,         */
/*                   INPUT           fg-rctd.job-no2 ,         */
/*                   INPUT           fg-rctd.i-no ,            */
/*                   INPUT           fg-rctd.rita-code ,       */
/*                   INPUT           lv-uom  ,                 */
/*                   INPUT           ld-cvt-cost ,             */
/*                   OUTPUT          v-cost     ,              */
/*                   OUTPUT          v-binqty,                 */
/*                   OUTPUT          v-qty,                    */
/*                   OUTPUT          v-tagcost).               */
{fg/upd-bini.i}
