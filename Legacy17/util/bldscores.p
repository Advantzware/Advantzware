
{sys/inc/var.i "new" "shared"}

{cec/descalc.i "new"}


DEF BUFFER reftable2 FOR reftable.

DEF VAR lv-dim LIKE eb.k-wid-array2 NO-UNDO.
DEF VAR v-code AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-score-char LIKE v-lscore-c EXTENT 12 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-str AS CHAR NO-UNDO.

DEF TEMP-TABLE tt NO-UNDO LIKE eb.

{sys/inc/f16to32.i}

PAUSE 0 BEFORE-HIDE.

FOR EACH eb WHERE eb.tab-in NE ?
              AND eb.len NE eb.wid,
    FIRST est NO-LOCK
    WHERE est.company EQ eb.company
      AND est.est-no  EQ eb.est-no
    TRANSACTION:

  DISPLAY eb.est-no FORMAT "x(10)"
          eb.style  FORMAT "x(10)"
          eb.flute  FORMAT "x(10)".

  EMPTY TEMP-TABLE tt.
  CREATE tt.
  BUFFER-COPY eb TO tt.

  ASSIGN
   eb.k-wid-array2 = 0
   eb.k-len-array2 = 0

   cocode = eb.company
   locode = eb.loc.

  RUN cec/descalc.p (RECID(est), RECID(eb)).

  BUFFER-COPY tt TO eb.

  IF NOT v-lscore-c BEGINS "No" THEN DO:
    ASSIGN
     i      = 0
     lv-dim = 0
     v-code = "2"
     ll     = NO.

    FOR EACH w-box-design-line:
      i = i + 1.
      lv-dim[i] = w-box-design-line.wscore-d.
      {sys/inc/k16bb.i lv-dim[i]}
    END.

    DO i = 1 TO EXTENT(lv-dim):
      IF eb.k-wid-array2[i] - lv-dim[i] GT 0 THEN ll = YES.
      lv-dim[i] = eb.k-wid-array2[i] - lv-dim[i].
      IF lv-dim[i] LT 0 THEN lv-dim[i] = 0.
    END.

    IF ll THEN RUN upd-ref.

    ASSIGN
     v-score-char = ""
     j            = 1
     lv-dim       = 0
     v-code       = "7"
     ll           = NO.

    FIND FIRST item NO-LOCK
        WHERE item.company EQ eb.company
          AND item.i-no    EQ eb.adhesive
        NO-ERROR.

    IF AVAIL item THEN
      IF item.mat-type EQ "G" THEN v-code = IF eb.tab-in THEN "3" ELSE "4".
      ELSE
      IF item.mat-type EQ "S" THEN v-code = IF eb.tab-in THEN "5" ELSE "6".

    DO i = 1 TO 80:
      IF SUBSTR(v-lscore-c,i,1) NE "" THEN DO:
        v-score-char[j] = v-score-char[j] + SUBSTR(v-lscore-c,i,1).
        IF SUBSTR(v-lscore-c,i + 1,1) EQ "" THEN
          ASSIGN
           v-score-char[j] = TRIM(v-score-char[j])
           j               = j + 1.
      END.
      IF j GT 12 THEN LEAVE.
    END.

    DO i = 1 to 12:

      IF v-cecscrn-dec AND v-score-char[i] NE "" THEN
         ASSIGN
            v-index = INDEX(v-score-char[i],".")
            v-str = SUBSTRING(v-score-char[i],v-index + 1)
            v-str = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))
            SUBSTRING(v-score-char[i],v-index) = v-str.

      lv-dim[i] = DEC(v-score-char[i]).
      {sys/inc/k16bb.i lv-dim[i]}.
    END.

    DO i = 1 TO EXTENT(lv-dim):
      IF eb.k-len-array2[i] - lv-dim[i] GT 0 THEN ll = YES.
      lv-dim[i] = eb.k-len-array2[i] - lv-dim[i].
      IF lv-dim[i] LT 0 THEN lv-dim[i] = 0.
    END.

    IF ll THEN RUN upd-ref.
  END.
END.

HIDE ALL NO-PAUSE.

RETURN.

PROCEDURE upd-ref.
  DEF VAR a AS INT NO-UNDO.
  DEF VAR b AS INT NO-UNDO.
  DEF VAR c AS INT NO-UNDO.
  DEF VAR d AS INT NO-UNDO.


  FIND FIRST reftable
      WHERE reftable.reftable EQ "STYFLU"
        AND reftable.company  EQ eb.style
        AND reftable.loc      EQ eb.flute
        AND reftable.code     EQ v-code
        AND reftable.code2    EQ ""
      NO-ERROR.

  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = 'styflu'
     reftable.company  = eb.style
     reftable.loc      = eb.flute
     reftable.code     = v-code
     reftable.code2    = "".
  END.

  FIND FIRST reftable2
      WHERE reftable2.reftable EQ "STYFLU"
        AND reftable2.company  EQ eb.style
        AND reftable2.loc      EQ eb.flute
        AND reftable2.code     EQ v-code
        AND reftable2.code2    EQ "1"
      NO-ERROR.

  IF NOT AVAIL reftable2 THEN DO:
    CREATE reftable2.
    ASSIGN
     reftable2.reftable = 'styflu'
     reftable2.company  = eb.style
     reftable2.loc      = eb.flute
     reftable2.code     = v-code
     reftable2.code2    = "1".
  END.

  DO i = 1 TO 12:
    IF reftable.val[i] NE 0 THEN DO:
      ll = NO.
      LEAVE.
    END.
  END.

  IF ll THEN
  DO i = 1 TO 8:
    IF reftable2.val[i] NE 0 THEN DO:
      ll = NO.
      LEAVE.
    END.
  END.

  IF ll THEN DO:
    c = 0.
    d = 0.

    DO i = 1 TO 12:
      reftable.val[i] = {sys/inc/k16v.i lv-dim[i]}.
      b = TRUNC(reftable.val[i],0).
      c = c + b.
      d = d + ((reftable.val[i] - b) * 100).
    END.

    DO i = 1 TO 8:
      j = i + 12.
      reftable2.val[i] = {sys/inc/k16v.i lv-dim[j]}.
      b = TRUNC(reftable2.val[i],0).
      c = c + b.
      d = d + ((reftable2.val[i] - b) * 100).
    END.

    c = c + TRUNC(d / 16,0).

    ASSIGN
     reftable.val[13]  = c + ((d MODULO 16) / 100)
     reftable2.val[13] = reftable.val[13].
  END.

END PROCEDURE.
