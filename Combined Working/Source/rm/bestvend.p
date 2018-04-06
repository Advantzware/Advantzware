/* -------------------------------------------------- rm/bestvend.p 05/03 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-qty   AS DEC DECIMALS 10 NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

{rm/bestvend.i}

DEF BUFFER b-e-itemv FOR e-item-vend.
DEF BUFFER b-e-item FOR e-item.
DEF BUFFER b-item FOR item.

DEF VAR lv-len LIKE item.s-len NO-UNDO.
DEF VAR lv-wid LIKE item.s-wid NO-UNDO.
DEF VAR lv-dep LIKE item.s-dep NO-UNDO.
DEF VAR lv-adder LIKE ef.adder EXTENT 20 NO-UNDO.
DEF VAR lv-lowest AS DEC NO-UNDO.
DEF VAR lv-unit AS DEC NO-UNDO.
DEF VAR lv-setup AS DEC NO-UNDO.
DEF VAR lv-board AS DEC NO-UNDO.
DEF VAR lv-added AS DEC NO-UNDO.
DEF VAR lv-add-qty LIKE ip-qty NO-UNDO.
DEF VAR lv-msf LIKE ip-qty NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.

DEF TEMP-TABLE tt-b-e-itemv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

ASSIGN
 lv-lowest = 9999999999.9999999999
 op-rowid  = ?.

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

FIND FIRST item
    WHERE item.company EQ ef.company
      AND item.i-no    EQ ef.board
    NO-LOCK NO-ERROR.

IF AVAIL item THEN
FIND FIRST eb
    WHERE eb.company EQ ef.company
      AND eb.est-no  EQ ef.est-no
      AND eb.form-no EQ ef.form-no
    NO-LOCK NO-ERROR.

IF NOT AVAIL eb THEN RETURN.

ASSIGN
 lv-len  = ef.gsh-len
 lv-wid  = ef.gsh-wid
 lv-dep  = ef.gsh-dep.

DO li = 1 TO 6:
  lv-adder[li] = ef.adder[li].
END.

IF NOT CAN-FIND(FIRST tt-ei) THEN RUN rm/bestvnd1.p (ROWID(eb)).

FOR EACH tt-ei NO-LOCK,
    EACH tt-eiv WHERE
        tt-eiv.company EQ tt-ei.company AND
        tt-eiv.i-no EQ tt-ei.i-no AND
        (tt-eiv.vend-no NE "" OR eb.pur-man) AND
        lv-wid          GE tt-eiv.roll-w[27] AND
        lv-wid          LE tt-eiv.roll-w[28] AND
        lv-len          GE tt-eiv.roll-w[29] AND
        lv-len          LE tt-eiv.roll-w[30]
    NO-LOCK
    BREAK BY tt-eiv.vend-no:

  ASSIGN
   lv-unit  = ?
   lv-setup = 0.

  DO li = 1 TO 20:
    IF tt-eiv.run-qty[li] GE ip-qty THEN DO:

      ASSIGN
       lv-unit  = tt-eiv.run-cost[li]
       lv-setup = tt-eiv.setups[li].

      RUN est/dim-charge.p (tt-eiv.rec_key,
                          lv-wid,
                          lv-len,
                          INPUT-OUTPUT lv-unit).

      LEAVE.
    END.
  END.

  IF lv-unit NE ? THEN DO:
    ASSIGN
     lv-board = (ip-qty * lv-unit) + lv-setup
     lv-added = 0.

    IF tt-ei.std-uom EQ "MSF" THEN
      lv-msf = ip-qty.
    ELSE
      RUN sys/ref/convquom.p(tt-ei.std-uom, "MSF",
                             item.basis-w, lv-len, lv-wid, lv-dep,
                             ip-qty, OUTPUT lv-msf).

    IF NOT eb.pur-man THEN adder-blok:
    DO li = 1 TO 10.
      IF lv-adder[li] NE "" THEN DO:
        ASSIGN
         lv-unit  = ?
         lv-setup = 0.

        FOR FIRST b-item
            WHERE b-item.company EQ tt-eiv.company
              AND b-item.i-no    EQ lv-adder[li]
            NO-LOCK,

            FIRST b-e-item OF b-item NO-LOCK,

            FIRST b-e-itemv OF b-e-item
            WHERE b-e-itemv.item-type EQ YES
              AND b-e-itemv.vend-no   EQ tt-eiv.vend-no
            NO-LOCK:

          IF b-e-item.std-uom EQ "MSF" THEN
             lv-add-qty = lv-msf.
          ELSE
            RUN sys/ref/convquom.p("MSF", b-e-item.std-uom,
                                   b-item.basis-w,
                                   lv-len, lv-wid, lv-dep,
                                   lv-msf, OUTPUT lv-add-qty).

          EMPTY TEMP-TABLE tt-b-e-itemv.
          CREATE tt-b-e-itemv.

          DO lj = 1 TO 10:
             ASSIGN
                tt-b-e-itemv.run-qty[lj] = b-e-itemv.run-qty[lj]
                tt-b-e-itemv.run-cost[lj] = b-e-itemv.run-cost[lj]
                tt-b-e-itemv.setups[lj] = b-e-itemv.setups[lj].
          END.

          
          IF AVAIL b-e-itemv THEN
          DO:
                      
             DO lj = 1 TO 10:
                ASSIGN
                   tt-b-e-itemv.run-qty[lj + 10] = b-e-itemv.runQtyXtra[lj]
                   tt-b-e-itemv.run-cost[lj + 10] = b-e-itemv.runCostXtra[lj]
                   tt-b-e-itemv.setups[lj + 10] = b-e-itemv.setupsXtra[lj].
             END.
          END.

          DO lj = 1 TO 20:
             IF b-e-itemv.run-qty[lj] GE lv-add-qty THEN DO:
                ASSIGN
                   lv-unit  = b-e-itemv.run-cost[lj]
                   lv-setup = b-e-itemv.setups[lj].
                LEAVE.
            END.
          END.

          IF lv-unit EQ ? THEN LEAVE adder-blok.
          ELSE lv-added = lv-added + ((lv-add-qty * lv-unit) + lv-setup).
        END.
      END.
    END.

    IF lv-unit NE ? THEN
      IF lv-lowest EQ 9999999999.9999999999 OR
         lv-board + lv-added LT lv-lowest   THEN
        ASSIGN
         lv-lowest = lv-board + lv-added
         op-rowid  = tt-eiv.row-id.
  END.
END.

/* end ---------------------------------- copr. 2003 advanced software, inc. */
