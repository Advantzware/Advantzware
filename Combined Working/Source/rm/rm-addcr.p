/* -------------------------------------------------- rm/rm-addcr.i 02/97 JLF */
/*                                                                            */
/* raw materials posting - create adder for posted board                      */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-rm-rctd FOR rm-rctd.

DEF TEMP-TABLE tt-bin NO-UNDO LIKE rm-bin FIELD tt-date AS DATE.

DEF VAR ll-neg AS LOG NO-UNDO.
DEF VAR ll-bin AS LOG NO-UNDO.
DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR dLastAssigned AS DEC DECIMALS 10 NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DO TRANSACTION:
  {sys/inc/rmpostgl.i}
END.
DEF TEMP-TABLE tt-rm-rctd-qty LIKE rm-rctd.
DEF TEMP-TABLE tt-rctd LIKE rm-rctd FIELD tt-row-id AS ROWID
                                    FIELD rm-row-id AS ROWID
                                    FIELD has-rec   AS LOG INIT NO
                                    FIELD seq-no    AS INT
                                    INDEX seq-no seq-no i-no.

DEF TEMP-TABLE tt-mat NO-UNDO FIELD frm LIKE job-mat.frm
                              FIELD qty LIKE job-mat.qty
                              INDEX frm frm.

{jc/jcgl-sh.i NEW}

def var v-avg-cst as LOG NO-UNDO.
DEF VAR v-current-rowid AS ROWID NO-UNDO.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

FIND b-rm-rctd WHERE ROWID(b-rm-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL b-rm-rctd THEN
FOR EACH job NO-LOCK
    WHERE job.company EQ b-rm-rctd.company
      AND job.job-no  EQ b-rm-rctd.job-no
      AND job.job-no2 EQ b-rm-rctd.job-no2,
    EACH job-mat NO-LOCK
    WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
      AND job-mat.frm     EQ b-rm-rctd.s-num
    USE-INDEX seq-idx,

    FIRST item NO-LOCK
    WHERE item.company  EQ job-mat.company
      AND item.i-no     EQ job-mat.rm-i-no
      AND item.mat-type EQ "A"
      AND item.i-code   EQ "R":
  
  ld-qty = b-rm-rctd.qty.

  IF b-rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
    IF item.r-wid EQ 0 THEN
      RUN rm/convquom.p(b-rm-rctd.pur-uom, item.cons-uom,
                        (IF job-mat.basis-w NE 0 THEN job-mat.basis-w
                         ELSE item.basis-w),         
                        (IF job-mat.len     NE 0 THEN job-mat.len
                         ELSE item.s-len),
                        (IF job-mat.wid     NE 0 THEN job-mat.wid
                         ELSE item.s-wid),
                        item.s-dep,
                        ld-qty,
                        OUTPUT ld-qty).
    ELSE
      RUN rm/convquom.p(b-rm-rctd.pur-uom, item.cons-uom,
                        (IF job-mat.basis-w NE 0 THEN job-mat.basis-w
                         ELSE item.basis-w),
                        job-mat.len,
                        (IF job-mat.wid     NE 0 THEN job-mat.wid
                         ELSE item.r-wid),
                        item.s-dep,
                        ld-qty,
                        OUTPUT ld-qty).

  {rm/rm-autoi.i rm-rctd rm-rctd item ld-qty ADDER}
END.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
