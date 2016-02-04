
{sys/inc/var.i NEW SHARED}

DEF VAR li AS INT NO-UNDO.
DEF VAR ll-inked AS LOG NO-UNDO.
DEF VAR rec-list AS CHAR NO-UNDO.

DEF WORK-TABLE w-itemfg-ink LIKE itemfg-ink.
    

FOR EACH eb WHERE eb.stock-no GT ""
    USE-INDEX stock NO-LOCK
    BY eb.company
    BY eb.stock-no
    BY eb.est-no
    
    TRANSACTION:

  DISPLAY eb.company
          eb.stock-no FORMAT "x(20)"
          eb.est-no   FORMAT "x(10)".

  ll-inked = NO.

  DO li = 1 TO EXTENT(eb.i-code):
    IF eb.i-code[li] NE "" THEN DO:
      ll-inked = YES.
      LEAVE.
    END.
  END.

  IF NOT ll-inked THEN
  DO li = 1 TO EXTENT(eb.i-code2):
    IF eb.i-code2[li] NE "" THEN DO:
      ll-inked = YES.
      LEAVE.
    END.
  END.

  IF ll-inked THEN DO:
    FOR EACH w-itemfg-ink:
      DELETE w-itemfg-ink.
    END.

    DO li = 1 TO EXTENT(eb.i-code):
      IF eb.i-code[li] NE "" THEN DO:
        CREATE w-itemfg-ink.
        ASSIGN
         w-itemfg-ink.rm-i-no = eb.i-code[li]
         w-itemfg-ink.dscr    = eb.i-dscr[li]
         w-itemfg-ink.pass    = eb.i-ps[li]
         w-itemfg-ink.cover%  = eb.i-%[li].
      END.
    END.

    DO li = 1 TO EXTENT(eb.i-code2):
      IF eb.i-code2[li] NE "" THEN DO:
        CREATE w-itemfg-ink.
        ASSIGN
         w-itemfg-ink.rm-i-no = eb.i-code2[li]
         w-itemfg-ink.dscr    = eb.i-dscr2[li]
         w-itemfg-ink.pass    = eb.i-ps2[li]
         w-itemfg-ink.cover%  = eb.i-%2[li].
      END.
    END.

    FOR EACH w-itemfg-ink:
      FIND FIRST item
           WHERE item.company EQ eb.company
             AND item.i-no    EQ w-itemfg-ink.rm-i-no
           NO-LOCK NO-ERROR.
      IF AVAIL item THEN w-itemfg-ink.press-type = item.press-type.
      ELSE DELETE w-itemfg-ink.
    END.

    FOR EACH w-itemfg-ink
        BREAK BY w-itemfg-ink.rm-i-no
              BY w-itemfg-ink.in-out:
      IF NOT FIRST-OF(w-itemfg-ink.in-out) THEN DELETE w-itemfg-ink.
    END.

    rec-list = "".

    FOR EACH w-itemfg-ink
        BREAK BY w-itemfg-ink.pass
              BY w-itemfg-ink.press-type:

      IF FIRST-OF(w-itemfg-ink.press-type) THEN
      FOR EACH itemfg-ink
          WHERE itemfg-ink.company EQ eb.company
            AND itemfg-ink.i-no    EQ eb.stock-no
            AND itemfg-ink.pass    EQ w-itemfg-ink.pass,
          FIRST item
          WHERE item.company    EQ itemfg-ink.company
            AND item.i-no       EQ itemfg-ink.rm-i-no
            AND item.press-type EQ w-itemfg-ink.press-type
          NO-LOCK:
        DELETE itemfg-ink.
      END.

      FIND FIRST itemfg-ink
          WHERE itemfg-ink.company EQ eb.company
            AND itemfg-ink.i-no    EQ eb.stock-no
            AND itemfg-ink.rm-i-no EQ w-itemfg-ink.rm-i-no
            AND itemfg-ink.in-out  EQ w-itemfg-ink.in-out
            AND itemfg-ink.pass    EQ w-itemfg-ink.pass
          NO-ERROR.

      IF NOT AVAIL itemfg-ink THEN CREATE itemfg-ink.

      RUN update-itemfg-ink.

      FIND FIRST reftable
          WHERE reftable.rec_key  EQ itemfg-ink.rec_key
            AND reftable.reftable EQ "itemfg-ink.occurs"
          USE-INDEX rec_key NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN
         reftable.rec_key  = itemfg-ink.rec_key
         reftable.reftable = "itemfg-ink.occurs"
         reftable.company  = itemfg-ink.company.
      END.

      IF NOT CAN-DO(rec-list,itemfg-ink.rec_key) THEN
        ASSIGN
         rec-list        = TRIM(rec-list)                       +
                           (IF rec-list EQ "" THEN "" ELSE ",") +
                           itemfg-ink.rec_key
         reftable.val[1] = 0.

      reftable.val[1] = reftable.val[1] + 1.
    END.
  END.
END.

HIDE ALL NO-PAUSE.

RETURN.

PROCEDURE update-itemfg-ink.

  DISABLE TRIGGERS FOR LOAD OF itemfg-ink.

  BUFFER-COPY w-itemfg-ink EXCEPT rec_key TO itemfg-ink
  ASSIGN
   itemfg-ink.company = eb.company
   itemfg-ink.i-no    = eb.stock-no.

END PROCEDURE.

