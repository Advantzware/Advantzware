  
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DO TRANSACTION:
  {sys/inc/rmpostgl.i}
END.

DEF TEMP-TABLE tt-rctd LIKE rm-rctd FIELD tt-row-id AS ROWID
                                    FIELD rm-row-id AS ROWID
                                    FIELD has-rec   AS LOG INIT NO
                                    FIELD seq-no    AS INT
                                    INDEX seq-no seq-no i-no.

DEF TEMP-TABLE tt-mat NO-UNDO FIELD frm LIKE job-mat.frm
                              FIELD qty LIKE job-mat.qty
                              INDEX frm frm.

{jc/jcgl-sh.i NEW}

DEF VAR ld-qty LIKE rm-bin.qty NO-UNDO.
DEF VAR ll-neg AS LOG NO-UNDO.
DEF VAR ll-bin AS LOG NO-UNDO.
DEF VAR dLastAssigned AS DEC DECIMALS 10 NO-UNDO.
DEF VAR li AS INT NO-UNDO.
def var v-avg-cst as LOG NO-UNDO.
DEF VAR v-current-rowid AS ROWID NO-UNDO.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

{rm/d-selbin.i NEW}
DEF TEMP-TABLE tt-rm-rctd-qty LIKE rm-rctd.

FIND job-mat WHERE ROWID(job-mat) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job-mat THEN DO:
  FIND FIRST item NO-LOCK
      {sys/look/itemW.i}
        AND item.i-no EQ job-mat.rm-i-no
      NO-ERROR.
  IF AVAIL item THEN DO:
    ld-qty = job-mat.qty.

    IF job-mat.qty-uom NE item.cons-uom AND item.cons-uom NE "" THEN
      RUN sys/ref/convquom.p(job-mat.qty-uom,
                             item.cons-uom,
                             job-mat.basis-w,
                             job-mat.len,
                             job-mat.wid,
                             job-mat.dep,
                             ld-qty,
                             OUTPUT ld-qty).

    {rm/rm-autoi.i rm-rctd rm-rctd item ld-qty I}
  END.
END.
