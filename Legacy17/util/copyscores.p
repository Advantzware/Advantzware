
DEF VAR lv-eqty LIKE eb.eqty NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ls AS CHAR NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.

DEF TEMP-TABLE tt-array NO-UNDO FIELD tt-dec AS DEC.


DISABLE TRIGGERS FOR LOAD OF eb.

PAUSE 0 BEFORE-HIDE.

FOR EACH eb BY eb.company BY eb.est-no:
  DISPLAY "Processing Company/Est#: " +
          TRIM(eb.company) + "/" + TRIM(eb.est-no) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  lv-eqty = 0.
  FOR EACH box-design-hdr NO-LOCK
      WHERE box-design-hdr.design-no EQ 0
        AND box-design-hdr.company   EQ eb.company
        AND box-design-hdr.est-no    EQ eb.est-no
        AND box-design-hdr.form-no   EQ eb.form-no
        AND box-design-hdr.blank-no  EQ eb.blank-no
      BREAK BY box-design-hdr.eqty:

    IF box-design-hdr.eqty EQ eb.eqty OR
       LAST(box-design-hdr.eqty)      THEN DO:
      lv-eqty = box-design-hdr.eqty.
      LEAVE.
    END.
  END.

  EMPTY TEMP-TABLE tt-array.

  ll = YES.
  DO li = 1 TO EXTENT(eb.k-len-array):
    IF eb.k-len-array[li] NE 0 THEN DO:
      ll = NO.
      LEAVE.
    END.
  END.

  IF ll THEN DO:
    FOR EACH box-design-hdr NO-LOCK
        WHERE box-design-hdr.design-no EQ 0
          AND box-design-hdr.company   EQ eb.company
          AND box-design-hdr.est-no    EQ eb.est-no
          AND box-design-hdr.eqty      EQ lv-eqty
          AND box-design-hdr.form-no   EQ eb.form-no
          AND box-design-hdr.blank-no  EQ eb.blank-no
        BREAK BY box-design-hdr.eqty:

      ls = "".
      DO li = 1 TO LENGTH(box-design-hdr.lscore):
        IF SUBSTR(box-design-hdr.lscore,li,1) NE " " THEN
          ls = ls + SUBSTR(box-design-hdr.lscore,li,1).

        IF ls NE ""                                      AND
           (SUBSTR(box-design-hdr.lscore,li,1) EQ " " OR
            li EQ LENGTH(box-design-hdr.lscore))         THEN DO:
          ld = DEC(ls) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN DO:
            CREATE tt-array.
            tt-dec = ld.
          END.
          ls = "".
        END.
      END.

      LEAVE.
    END.

    IF CAN-FIND(FIRST tt-array) THEN DO:
      RUN 16-to-dec.

      ASSIGN
       eb.k-len-array2 = 0
       li              = 0.

      FOR EACH tt-array:
        li = li + 1.
        IF li GT EXTENT(eb.k-len-array2) THEN LEAVE.
        eb.k-len-array2[li] = tt-dec.
      END.
    END.
  END.

  ll = YES.
  DO li = 1 TO EXTENT(eb.k-wid-array):
    IF eb.k-wid-array[li] NE 0 THEN DO:
      ll = NO.
      LEAVE.
    END.
  END.

  IF ll THEN DO:
    EMPTY TEMP-TABLE tt-array.

    FOR EACH box-design-line NO-LOCK
        WHERE box-design-line.design-no EQ 0
          AND box-design-line.company   EQ eb.company
          AND box-design-line.est-no    EQ eb.est-no
          AND box-design-line.eqty      EQ lv-eqty
          AND box-design-line.form-no   EQ eb.form-no
          AND box-design-line.blank-no  EQ eb.blank-no
          AND box-design-line.wscore    NE "":

      ld = DEC(box-design-line.wscore) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
        CREATE tt-array.
        tt-dec = ld.
      END.
    END.

    IF CAN-FIND(FIRST tt-array) THEN DO:
      RUN 16-to-dec.

      ASSIGN
       eb.k-wid-array2 = 0
       li              = 0.

      FOR EACH tt-array:
        li = li + 1.
        IF li GT EXTENT(eb.k-wid-array2) THEN LEAVE.
        eb.k-wid-array2[li] = tt-dec.
      END.
    END.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

RETURN.

PROCEDURE 16-to-dec:

  FOR EACH tt-array:
    li = TRUNC(tt-dec,0).

    IF tt-dec NE li THEN
      ASSIGN
       ld     = (tt-dec - li) * 100
       li     = li * 16
       tt-dec = (li + ld) / 16.
  END.

END PROCEDURE.

