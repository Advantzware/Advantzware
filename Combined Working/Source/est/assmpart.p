/* Is this estimate blank the header for an Assembled Partion Set */

DEF PARAM BUFFER io-eb FOR eb.
DEF OUTPUT PARAM op-assem-part AS LOG NO-UNDO.

DEF VAR ld-wid AS DEC NO-UNDO.


IF AVAIL io-eb THEN DO:
  op-assem-part = io-eb.form-no EQ 0.

  IF op-assem-part THEN
  FOR EACH eb NO-LOCK
      WHERE eb.company EQ io-eb.company
        AND eb.est-no  EQ io-eb.est-no
        AND ROWID(eb)  NE ROWID(io-eb)
      BREAK BY eb.part-no:

    IF FIRST-OF(eb.part-no) THEN DO:
      IF ld-wid EQ 0 THEN ld-wid = eb.wid.

      ACCUM 1 (TOTAL).

      IF eb.dep NE 0                                  OR
         eb.wid NE ld-wid                             OR
         NOT CAN-FIND(FIRST style
                      WHERE style.company EQ eb.company
                        AND style.style   EQ eb.style
                        AND CAN-DO("P,R",style.type)) THEN DO:
        op-assem-part = NO.
        LEAVE.
      END.
    END.
  END.

  op-assem-part = op-assem-part AND (ACCUM TOTAL 1) EQ 2.
END.
