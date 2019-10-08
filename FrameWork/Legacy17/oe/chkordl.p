
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR lv-bin-qty LIKE fg-bin.qty NO-UNDO.
DEF VAR lv-ord-list AS CHAR NO-UNDO.
DEF VAR ll-one-ord AS LOG NO-UNDO.

DEF BUFFER b-ord  FOR oe-ord.
DEF BUFFER b-ordl FOR oe-ordl.

DEF TEMP-TABLE w-ord NO-UNDO FIELD w-ord-no LIKE oe-ordl.ord-no.

{oe/chkordl.i}


IF NOT CAN-FIND(FIRST w-ordl WHERE w-rowid EQ ip-rowid) THEN
FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN DO:
  CREATE w-ordl.
  ASSIGN
   w-rowid = ip-rowid
   w-ok    = YES.

  {sys/inc/addrelse.i}

  IF v-do-chk THEN DO: /* Check for unshipped orders for this line item */
    IF addrelse-dec EQ 1 THEN
    FOR EACH b-ordl
        WHERE b-ordl.company EQ oe-ordl.company
          AND b-ordl.opened  EQ YES
          AND b-ordl.i-no    EQ oe-ordl.i-no
          AND ROWID(b-ordl)  NE ROWID(oe-ordl)
        USE-INDEX opened NO-LOCK:
      {oe/chkordl1.i}
    END.

    ELSE
    IF addrelse-dec EQ 2 THEN
    FOR EACH b-ordl
        WHERE b-ordl.company EQ oe-ordl.company
          AND b-ordl.i-no    EQ oe-ordl.i-no
          AND ROWID(b-ordl)  NE ROWID(oe-ordl)
        USE-INDEX item NO-LOCK:
      {oe/chkordl1.i}
    END.

    lv-ord-list = "".

    FOR EACH w-ord BREAK BY w-ord:
      lv-ord-list = TRIM(lv-ord-list) + " " + TRIM(STRING(w-ord,">>>>>>>>")) + ",".

      IF LAST(w-ord) THEN DO:
        ASSIGN
         w-ok   = NO
         w-auto = YES.

        IF SUBSTR(lv-ord-list,LENGTH(TRIM(lv-ord-list)),1) EQ "," THEN
          SUBSTR(lv-ord-list,LENGTH(TRIM(lv-ord-list)),1) = "".

        ll-one-ord = NUM-ENTRIES(lv-ord-list) LE 1.

        MESSAGE "Order" + TRIM(STRING(ll-one-ord,"/s")) +
                ": " + TRIM(lv-ord-list)
                SKIP
                "for FG Item#: " + TRIM(oe-ordl.i-no) + " " +
                TRIM(STRING(ll-one-ord,"has/have")) + " not been fully shipped, " +
                "continue with this release?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE w-ok.
      END.
    END.
  END.
END.
