
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-dont-reset AS LOG NO-UNDO.

DEF VAR lv-frm LIKE eb.form-no INIT 0 NO-UNDO.
DEF VAR lv-blk LIKE eb.blank-no INIT 0 NO-UNDO.
DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
DEF VAR li-part-no AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll-waiting AS LOG NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER xeb-form-ef FOR ef.


FIND est WHERE ROWID(est) EQ ip-rowid EXCLUSIVE.

IF AVAIL est THEN DO:
  lv-part-no = "".

  IF NOT ip-dont-reset                              AND
     NOT CAN-FIND(FIRST eb
                  WHERE eb.company EQ est.company
                    AND eb.est-no  EQ est.est-no
                    AND eb.ord-no  NE 0)            AND
     est.est-type EQ 6                              THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company  EQ est.company
          AND b-eb.est-no   EQ est.est-no
          AND b-eb.form-no  EQ 0
          AND b-eb.blank-no EQ 0
        NO-LOCK NO-ERROR.

    IF AVAIL b-eb THEN DO:
      /*FOR EACH ef
          WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
          NO-LOCK,
          EACH eb
          WHERE eb.company  EQ ef.company
            AND eb.est-no   EQ ef.est-no
            AND eb.form-no  EQ ef.form-no
            AND eb.stock-no NE ""
          NO-LOCK:
        LEAVE.
      END.

      IF NOT AVAIL eb THEN DO:*/
        ll = NO.
        MESSAGE "Resequence Customer Part Number(s)?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
        IF ll THEN lv-part-no = SUBSTR(b-eb.part-no,1,12).
      /*END.*/
    END.
  END.

  li-part-no = 0.
  FOR EACH ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
      BREAK BY ef.form-no:

    ASSIGN
     lv-frm     = lv-frm + 1
     lv-blk     = 0.

    FOR EACH eb
        WHERE eb.company  EQ ef.company
          AND eb.est-no   EQ ef.est-no
          AND eb.form-no  EQ ef.form-no
        BY eb.blank-no:

      lv-blk = lv-blk + 1.

      IF eb.form-no  NE lv-frm OR
         eb.blank-no NE lv-blk THEN DO:
        {sys/inc/xeb-form.i "eb." "eb.blank-no" "lv-frm" "lv-blk"}

        ASSIGN
         eb.form-no  = lv-frm
         eb.blank-no = lv-blk.
      END.

      IF lv-part-no NE "" AND eb.stock-no EQ "" THEN
        ASSIGN
         li-part-no = li-part-no + 1
         eb.part-no = TRIM(lv-part-no) + "-" + STRING(li-part-no).
    END.

    IF lv-blk EQ 0 THEN DELETE ef.

    ELSE
    IF ef.form-no NE lv-frm THEN DO:
      {sys/inc/xeb-form.i "ef." "0" "lv-frm" "0"}

      ASSIGN
       ef.form-no   = lv-frm
       ef.blank-qty = lv-blk.
    END.
  END.

  est.form-qty = lv-frm.
END.
