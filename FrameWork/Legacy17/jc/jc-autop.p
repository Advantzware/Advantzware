
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-factor AS DEC DECIMALS 10 no-undo.
DEF INPUT PARAM ip-qty AS DEC DECIMALS 10 no-undo.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

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

DEF BUFFER b-item FOR item.

DEF TEMP-TABLE tt-bin NO-UNDO LIKE rm-bin FIELD tt-date AS DATE.
DEF TEMP-TABLE tt-rm-rctd-qty LIKE rm-rctd.

DEF VAR ll-neg AS LOG NO-UNDO.
DEF VAR ll-bin AS LOG NO-UNDO.
DEF VAR dLastAssigned AS DEC DECIMALS 10 NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v AS INT NO-UNDO.
DEF VAR ll-po AS LOG NO-UNDO.
DEF VAR lv-qty-uom LIKE job-mat.qty-uom NO-UNDO.
def var v-avg-cst as LOG NO-UNDO.
DEF VAR v-current-rowid AS ROWID NO-UNDO.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

{jc/job-cls3.i NEW}


FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.
FIND job-mat WHERE ROWID(job-mat) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job-mat AND job-mat.post THEN DO:
  FIND FIRST job NO-LOCK
      WHERE job.company EQ job-mat.company
        AND job.job     EQ job-mat.job
        AND job.job-no  EQ job-mat.job-no
        AND job.job-no2 EQ job-mat.job-no2
      NO-ERROR.

  FIND FIRST item NO-LOCK
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
      NO-ERROR.
END.

IF AVAIL item THEN DO:
  IF item.mat-type EQ "B" THEN
  FOR EACH po-ordl NO-LOCK
      WHERE po-ordl.company   EQ job-mat.company
        AND po-ordl.job-no    EQ job-mat.job-no
        AND po-ordl.job-no2   EQ job-mat.job-no2
        AND po-ordl.s-num     EQ job-mat.frm
        AND po-ordl.item-type EQ YES
        AND CAN-FIND(FIRST b-item
                     WHERE b-item.company  EQ po-ordl.company
                       AND b-item.i-no     EQ po-ordl.i-no
                       AND b-item.mat-type EQ "B"):
    ll-po = YES.
    LEAVE.
  END.

  IF NOT ll-po THEN DO:
    IF item.mat-type EQ "B" AND AVAIL job THEN DO:
      ASSIGN
       ip-qty     = 0
       lv-qty-uom = "EA".

      RUN jc/job-cls3.p (BUFFER job).

      FOR EACH w-qty
          WHERE w-qty.s-num EQ job-mat.frm
          BREAK BY w-qty.b-num
                BY w-qty.seq
                BY w-qty.dept
                BY w-qty.pass:
        ip-qty = (w-qty.fin + w-qty.wst) / w-qty.out * ip-factor.
        LEAVE.
      END.
    END.

    ELSE 
      ASSIGN
       ip-qty     = IF ip-qty EQ 0 THEN (job-mat.qty * ip-factor) ELSE ip-qty
       lv-qty-uom = job-mat.qty-uom.
    IF CAN-DO("C,D",item.mat-type) THEN ip-qty = ROUND(ip-qty,0).
    IF lv-qty-uom NE item.cons-uom AND item.cons-uom NE "" THEN        
        RUN sys/ref/convquom.p(job-mat.qty-uom,
                             item.cons-uom,
                             job-mat.basis-w,
                             job-mat.len,
                             job-mat.wid,
                             job-mat.dep,
                             ip-qty,
                             OUTPUT ip-qty).


   IF NOT CAN-DO("I,G",ITEM.mat-type) THEN DO:
     ip-qty = TRUNCATE(ip-qty,3).  /*to handle small decimal place errors from ip-factor*/
     {sys/inc/roundup.i ip-qty}
   END.                        

   {rm/rm-autoi.i rm-rctd rm-rctd item ip-qty I}
  END.
END.
