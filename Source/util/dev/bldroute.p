
{custom/globdefs.i}

{sys/inc/var.i "new shared"}

assign
 cocode = g_company
 locode = g_loc.
    
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

DEF BUFFER xop FOR est-op.

def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def new shared var fil_id as recid no-undo.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
def NEW shared var v-chk-qty as dec no-undo.
def NEW shared var v-sht-qty as dec no-undo.
def NEW shared var v-rc-seq as int init 9999 no-undo.

{ce/mach-ink.i new}


FOR EACH xest
    WHERE xest.company EQ cocode
      AND NOT CAN-FIND(FIRST est-op
                       WHERE est-op.company EQ xest.company
                         AND est-op.est-no  EQ xest.est-no
                         AND est-op.line    LT 500)
      /*AND NOT CAN-FIND(FIRST probe
                       WHERE probe.company EQ xest.company
                         AND probe.est-no  EQ xest.est-no)*/
    TRANSACTION:

  IF CAN-FIND(FIRST est-op
              WHERE est-op.company EQ xest.company
                AND est-op.est-no  EQ xest.est-no
                AND est-op.line    GE 500) THEN
  FOR EACH est-op
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    GT 500
      NO-LOCK:
    CREATE xop.
    BUFFER-COPY est-op EXCEPT rec_key line TO xop
    ASSIGN
     xop.line = xop.line - 500.
  END.

  ELSE
  /*IF xest.recalc EQ YES THEN*/ DO:
    FOR EACH ef
        WHERE ef.company EQ xest.company
          AND ef.est-no  EQ xest.est-no:
      
      ef.op-lock = NO.
    END.
  
    FIND FIRST xef WHERE xef.company = xest.company 
                     AND xef.est-no = xest.est-no
                     NO-LOCK NO-ERROR.
    FIND FIRST xeb WHERE xeb.company = xef.company 
                     AND xeb.est-no = xef.est-no
                     AND xeb.form-no = xef.form-no
                     NO-LOCK NO-ERROR.

    IF xest.est-type EQ 1 OR
       xest.est-type EQ 5 OR
       xest.est-type EQ 6 THEN
    FOR EACH est-qty
        WHERE est-qty.company EQ xest.company
          AND est-qty.est-no  EQ xest.est-no
        NO-LOCK:
        
      FOR EACH ef
          WHERE ef.company eq xest.company
            AND ef.est-no  eq xest.est-no:
      
        ef.op-lock = NO.
      END.
      
      IF xest.est-type EQ 1 THEN RUN ce/mach-seq.p (est-qty.eqty).
                            ELSE RUN cec/mach-seq.p (0, est-qty.eqty, NO).
    END.

    ELSE
    IF xest.est-type EQ 2 THEN RUN ce/box/mach-seq.p.

    ELSE
    IF xest.est-type EQ 3 THEN RUN ce/tan/mach-seq.p. 
 
    ELSE
    IF xest.est-type EQ 4 THEN RUN ce/com/mach-seq.p (0).

    ELSE
    IF xest.est-type EQ 8 THEN RUN cec/mach-seq.p (0, 0, YES).
  END.
END.
