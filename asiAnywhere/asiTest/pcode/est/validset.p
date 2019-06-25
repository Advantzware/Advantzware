
DEF PARAM BUFFER io-eb FOR eb.
     
DEF BUFFER set-eb FOR eb.

DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
DEF VAR li-part-no AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF eb.

IF AVAIL io-eb THEN
FIND FIRST est NO-LOCK
    WHERE est.company   EQ io-eb.company
      AND est.est-no    EQ io-eb.est-no
      AND (est.est-type EQ 2 OR est.est-type EQ 6)
    NO-ERROR.

IF AVAIL est THEN
FIND FIRST set-eb
    WHERE set-eb.company       EQ est.company
      AND set-eb.est-no        EQ est.est-no
      AND set-eb.form-no       EQ 0
      AND set-eb.blank-no      EQ 0
      AND TRIM(set-eb.part-no) NE ""
    NO-ERROR.

IF AVAIL set-eb THEN DO:
  lv-part-no = SUBSTR(set-eb.part-no,1,12).

  FIND eb
      WHERE eb.company EQ set-eb.company
        AND eb.est-no  EQ set-eb.est-no
        AND eb.form-no NE 0
      NO-ERROR.

  IF AVAIL eb THEN /*eb.part-no = set-eb.part-no*/.

  ELSE
  IF CAN-FIND(FIRST eb
              WHERE eb.company EQ set-eb.company
                AND eb.est-no  EQ set-eb.est-no
                AND eb.part-no EQ set-eb.part-no
                AND ROWID(eb)  NE ROWID(set-eb)) THEN
  FOR EACH eb
      WHERE eb.company EQ set-eb.company
        AND eb.est-no  EQ set-eb.est-no
        AND ROWID(eb)  NE ROWID(set-eb)
      BY eb.form-no
      BY eb.blank-no:

    IF eb.stock-no EQ "" THEN
      ASSIGN
       li-part-no = li-part-no + 1
       eb.part-no = TRIM(lv-part-no) + "-" + STRING(li-part-no).
  END.
END.
