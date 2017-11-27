/* Be sure whatever mods you make take into account multi-invoice customers  */
&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME inv-head

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
def buffer xoe-relh for oe-relh.
def buffer xoe-bolh for oe-bolh.
def buffer xoe-boll for oe-boll.

def var v-rel-bal like oe-rell.qty.
def var v-lines-wt as int.
def var v-nxt-bordno like oe-relh.b-ord-no.
def var v-nxt-rno like oe-relh.r-no.
DEF VAR lv-partial LIKE fg-bin.partial-count NO-UNDO.
DEF VAR ll-invoice AS LOG NO-UNDO.
DEF VAR v-do-bol AS LOG NO-UNDO.

DEF TEMP-TABLE w-r-no FIELD w-r-no LIKE {&TABLENAME}.r-no.
DEF TEMP-TABLE w-b-no FIELD w-b-no LIKE inv-line.b-no.


find first oe-ctrl where oe-ctrl.company = {&TABLENAME}.company no-lock no-error.
find last xoe-relh use-index r-no no-lock no-error.
if avail xoe-relh then assign v-nxt-rno = xoe-relh.r-no.
else assign v-nxt-rno = 0.

DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.
DISABLE TRIGGERS FOR LOAD OF oe-ordm.

CREATE w-r-no.
w-r-no = {&TABLENAME}.r-no.

work-loop:
DO WHILE AVAIL w-r-no.
  FOR EACH inv-line
      WHERE inv-line.r-no EQ w-r-no
        AND NOT CAN-FIND(FIRST w-b-no WHERE w-b-no EQ inv-line.b-no)
      NO-LOCK:
    CREATE w-b-no.
    w-b-no = inv-line.b-no.
  END.

  FOR EACH w-b-no,
      EACH inv-line
      WHERE inv-line.b-no EQ w-b-no
        AND NOT CAN-FIND(FIRST w-r-no WHERE w-r-no EQ inv-line.r-no)
      NO-LOCK:
    CREATE w-r-no.
    w-r-no = inv-line.r-no.
  END.

  RELEASE w-r-no.

  FOR EACH w-r-no,
      FIRST inv-line
      WHERE inv-line.r-no EQ w-r-no
        AND NOT CAN-FIND(FIRST w-b-no WHERE w-b-no EQ inv-line.b-no)
      NO-LOCK:
    LEAVE.
  END.
END.

FOR EACH w-b-no,
    FIRST oe-bolh WHERE oe-bolh.b-no EQ w-b-no NO-LOCK,
    EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
    USE-INDEX b-no NO-LOCK:

  FOR EACH xoe-boll
      WHERE xoe-boll.company    EQ oe-boll.company
        AND xoe-boll.ord-no     EQ oe-boll.ord-no
        AND ((xoe-boll.rel-no   EQ oe-boll.rel-no AND
              xoe-boll.b-ord-no GT oe-boll.b-ord-no)    /*OR
             (xoe-boll.i-no     EQ oe-boll.i-no   AND
              xoe-boll.line     EQ oe-boll.line   AND
              xoe-boll.rel-no   GT oe-boll.rel-no AND
              xoe-boll.s-code   NE "B"            AND
              oe-boll.s-code    NE "B")*/)
        AND NOT CAN-FIND(FIRST xoe-bolh
                         WHERE xoe-bolh.b-no   EQ xoe-boll.b-no
                           AND xoe-bolh.posted EQ YES)
      USE-INDEX ord-no NO-LOCK:
    MESSAGE "Can not delete invoice until all subsequent " +
            "invoices and bills of lading are deleted."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
END.

FOR EACH w-r-no:
  FOR EACH inv-line WHERE inv-line.r-no EQ w-r-no:
    /* oe/invpost4.p calls fg/fgcalca&b.p to get q-alloc, but if the posted flag */
    /* is yes, it won't pick up the correct qty                                  */
    FOR EACH  w-b-no 
        WHERE w-b-no = inv-line.b-no,
          FIRST oe-bolh WHERE oe-bolh.b-no EQ w-b-no:
          oe-bolh.posted = NO.
    END.
    IF oe-ctrl.u-inv THEN RUN oe/invpost4.p (RECID(inv-line), -1).
    DELETE inv-line.
  END.
  
  FOR EACH inv-misc WHERE inv-misc.r-no EQ w-r-no:
    /** Re-Set billing flag to (I)nvoiced **/
    FIND FIRST oe-ordm
        WHERE oe-ordm.company EQ inv-misc.company
          AND oe-ordm.ord-no  EQ inv-misc.ord-no
          AND oe-ordm.line    EQ inv-misc.line
        NO-ERROR.
    IF AVAIL oe-ordm        AND
       oe-ordm.bill  EQ "I" AND
       inv-misc.bill EQ "Y" THEN oe-ordm.bill = "Y".

    DELETE inv-misc.
  END. /* each inv-misc */

  FIND FIRST b-{&TABLENAME} WHERE b-{&TABLENAME}.r-no EQ w-r-no EXCLUSIVE NO-ERROR. 
  IF AVAIL b-{&TABLENAME} THEN DO:
    IF NOT b-{&TABLENAME}.multi-invoice THEN
      RUN oe/updmulti.p (BUFFER b-{&TABLENAME}, BUFFER b-{&TABLENAME}).
    DELETE b-{&TABLENAME}.
  END.
END.

FOR EACH w-b-no,
    FIRST oe-bolh WHERE oe-bolh.b-no EQ w-b-no:

  RUN oe/bolpundo.p (BUFFER oe-bolh, OUTPUT ll-invoice).
     
  IF ll-invoice THEN DO:
    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no:
      {oe/bollrell.i}
      DELETE oe-boll.
    END.

    {oe/bolhrell.i}

    FIND FIRST oe-relh
        WHERE oe-relh.company  EQ oe-bolh.company
          AND oe-relh.release# EQ oe-bolh.release#
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL oe-relh THEN oe-relh.posted = NO.

    DELETE oe-bolh.
  END.
END.

IF NOT {&TABLENAME}.multi-invoice THEN
  RUN oe/updmulti.p (BUFFER {&TABLENAME}, BUFFER {&TABLENAME}).

FIND FIRST reftable WHERE
     reftable.reftable EQ "brokerbol" AND
     reftable.CODE EQ STRING(inv-head.inv-no)
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
