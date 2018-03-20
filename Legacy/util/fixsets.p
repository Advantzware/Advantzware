DEF VAR x AS INT NO-UNDO.
DEF VAR y AS INT NO-UNDO.
  
      
DEF BUFFER b-eb FOR eb.

SESSION:SET-WAIT-STATE ("general").

FOR EACH eb WHERE eb.form-no NE 0,    
    FIRST est OF eb
    WHERE est.est-type GE 5
      AND est.est-type LE 6
    BREAK BY eb.company
          BY eb.est-no:

  IF FIRST-OF(eb.est-no) AND (NOT LAST-OF(eb.est-no) OR eb.quantityPerSet GT 1) THEN DO:
    est.est-type = 6.

    FOR EACH ef OF est:
      ef.est-type = est.est-type.
    END.

    FOR EACH b-eb OF est:
      b-eb.est-type = est.est-type.
    END.

    FIND FIRST b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND b-eb.form-no EQ 0
        NO-ERROR.
    IF NOT AVAIL b-eb THEN DO:
      {ce/set-info.a 6 "b-"}
    END.

    FIND itemfg NO-LOCK
        WHERE itemfg.company EQ b-eb.company
          AND itemfg.est-no  EQ b-eb.est-no
          AND itemfg.isaset  EQ YES
        NO-ERROR.

    IF AVAIL itemfg THEN
      ASSIGN
        b-eb.part-no          = itemfg.part-no
        b-eb.stock-no         = itemfg.i-no
        b-eb.part-dscr1       = itemfg.i-name 
        b-eb.part-dscr2       = itemfg.part-dscr1
        b-eb.set-is-assembled = NOT itemfg.alloc.

    ELSE DO:
      IF b-eb.part-no EQ "" AND eb.part-no NE "" THEN
        IF LAST-OF(eb.est-no) THEN
          b-eb.part-no = eb.part-no.
        ELSE
          b-eb.part-no = SUBSTR(TRIM(eb.part-no),1,LENGTH(TRIM(eb.part-no)) - 2).

      IF b-eb.stock-no EQ "" AND LENGTH(TRIM(eb.stock-no)) GT 2 THEN
        IF LAST-OF(eb.est-no) THEN
          b-eb.stock-no = eb.stock-no.
        ELSE
          b-eb.stock-no = SUBSTR(TRIM(eb.stock-no),1,LENGTH(TRIM(eb.stock-no)) - 2) + "00".

      IF b-eb.part-dscr1 EQ "" THEN
        b-eb.part-dscr1 = eb.part-dscr1.

      IF b-eb.part-dscr2 EQ "" THEN
        b-eb.part-dscr2 = eb.part-dscr2.

      IF b-eb.cust-no EQ "" THEN
        b-eb.cust-no = eb.cust-no.

      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ b-eb.company
           AND itemfg.i-no    EQ b-eb.stock-no
            AND b-eb.stock-no  NE ""
          NO-ERROR.
    END.

    IF AVAIL itemfg THEN DO:
      FIND CURRENT itemfg NO-ERROR.

      ASSIGN
       b-eb.procat   = itemfg.procat
       itemfg.isaset = NOT LAST-OF(eb.est-no).

      FOR EACH b-eb OF est WHERE b-eb.form-no NE 0 NO-LOCK:
        FIND FIRST fg-set
            WHERE fg-set.company EQ itemfg.company
              AND fg-set.set-no  EQ itemfg.i-no
              AND fg-set.part-no EQ b-eb.stock-no
            NO-ERROR.
        IF NOT AVAIL fg-set THEN DO:
          FIND LAST fg-set USE-INDEX s-no NO-LOCK NO-ERROR.
          x = IF AVAIL fg-set THEN fg-set.s-no + 1 ELSE 1.
          FIND LAST fg-set
              WHERE fg-set.company EQ itemfg.company
                AND fg-set.set-no  EQ itemfg.i-no
              NO-LOCK NO-ERROR.
          y = IF AVAIL fg-set THEN fg-set.line + 1 ELSE 1.

          CREATE fg-set.
          ASSIGN
           fg-set.company = itemfg.company
           fg-set.set-no  = itemfg.i-no
           fg-set.s-no    = x
           fg-set.line    = y
           fg-set.part-no = b-eb.stock-no.
        END.

        fg-set.part-qty = INTEGER(b-eb.quantityPerSet).
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").

MESSAGE "Fixing sets complete..." VIEW-AS ALERT-BOX.
