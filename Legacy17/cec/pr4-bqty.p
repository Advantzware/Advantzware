
DEF INPUT  PARAM ip-vend-no LIKE vend.vend-no NO-UNDO.
DEF INPUT  PARAM ip-fg      AS   INT NO-UNDO.
DEF OUTPUT PARAM op-fg      AS   INT NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF VAR v-out AS INT NO-UNDO.
DEF VAR v-rm AS DEC NO-UNDO.
DEF VAR li LIKE j NO-UNDO.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.

DEF TEMP-TABLE w-qty FIELD w-qty AS DEC.

{rm/bestvend.i NEW}

{cec/msfcalc.i}

op-fg = ip-fg.

FIND FIRST item NO-LOCK 
    {sys/look/itemW.i} 
      AND item.i-no EQ xef.board 
    NO-ERROR.
    
IF AVAIL item AND item.i-code EQ "E"                  AND
   NOT CAN-FIND(FIRST item-bom 
                WHERE item-bom.company  EQ item.company
                  AND item-bom.parent-i EQ item.i-no
                  AND item-bom.line# LT 9) THEN DO:
    
  RUN rm/bestvnd1.p (ROWID(xeb)).  /* create temp-tables tt-ei, tt-eiv */

  FIND FIRST tt-ei NO-ERROR.

  IF AVAIL tt-ei THEN DO:
    FIND FIRST tt-eiv WHERE
        tt-eiv.company EQ tt-ei.company AND
        tt-eiv.i-no    EQ tt-ei.i-no AND
        tt-eiv.item-type EQ YES AND
        tt-eiv.vend-no   EQ (IF ip-vend-no EQ "bestvendor" THEN ""
                             ELSE ip-vend-no)
        NO-ERROR.

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-out).

    /* msf */
    ASSIGN
     v-rm  = IF v-corr THEN
               (((xef.gsh-wid * xef.gsh-len) * (op-fg / xeb.num-up))
                 / v-out) * .000007
             ELSE
               (((xef.gsh-wid * xef.gsh-len) * (op-fg / xeb.num-up))
                 / v-out) / 144000.

    IF tt-ei.std-uom NE "MSF" THEN
      RUN sys/ref/convquom.p("MSF", tt-ei.std-uom, item.basis-w,
                             xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                             v-rm, OUTPUT v-rm).

    IF ip-vend-no EQ "bestvendor" OR xeb.pur-man THEN DO:
      RUN rm/bestvend.p (ROWID(xef), v-rm, OUTPUT lv-bestvend-id).
      IF lv-bestvend-id NE ? THEN
      FIND tt-eiv WHERE tt-eiv.row-id EQ lv-bestvend-id NO-LOCK NO-ERROR.
    END.

    IF AVAIL tt-eiv THEN
    DO li = 1 TO 19:
      CREATE w-qty.
      w-qty = tt-eiv.run-qty[li].
    END.
    ELSE
    DO li = 1 TO 19:
      CREATE w-qty.
      w-qty = tt-ei.run-qty[li].
    END.

    FOR EACH w-qty WHERE w-qty NE 0 BREAK BY w-qty:
      IF w-qty GE v-rm AND NOT LAST(w-qty) THEN DO:
        IF tt-ei.std-uom NE "EA" THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom, "EA", item.basis-w,
                                 xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                                 w-qty, OUTPUT w-qty).
                                 
        w-qty = w-qty * xeb.num-up * v-out.
        
        IF w-qty - TRUNC(w-qty,0) EQ 0 THEN w-qty + 1.
                                 
        {sys/inc/roundup.i w-qty}
         
        op-fg = w-qty.

        LEAVE.
      END.
    END.
  END.
END.      

IF op-fg GT 999999999 THEN op-fg = 999999999.
