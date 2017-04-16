
DEF PARAM BUFFER new-stack-flute FOR stack-flute.

DEF BUFFER stack FOR reftable.
DEF BUFFER b-s-f FOR stack-flute.

DEF TEMP-TABLE w-test NO-UNDO FIELD w-test LIKE eb.test.

DEF TEMP-TABLE tt-stack NO-UNDO
                        FIELD tt-test LIKE eb.test
                        FIELD tt-stak LIKE eb.stack-code
                        FIELD tt-valu LIKE eb.tr-cnt
                        FIELD tt-page AS INT
                        FIELD tt-row  AS INT
                        FIELD tt-col  AS INT
                        INDEX tt-stack tt-test tt-stak
                        INDEX tt-stak  tt-stak tt-page
                        INDEX tt-page  tt-page tt-row tt-col.

DEF VAR li  AS INT NO-UNDO.
DEF VAR lic AS INT NO-UNDO.
DEF VAR lir AS INT NO-UNDO.
DEF VAR lip AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF stack-flute.
DISABLE TRIGGERS FOR LOAD OF b-s-f.

FOR EACH stack-flute NO-LOCK
    WHERE stack-flute.company EQ new-stack-flute.company
      AND stack-flute.loc     EQ new-stack-flute.loc
      AND stack-flute.code    EQ new-stack-flute.code
      AND stack-flute.pallet  EQ new-stack-flute.pallet:
  DO lir = 1 TO EXTENT(stack-flute.row-value) - 1:
    IF stack-flute.row-value[lir] NE ""                         AND
       NOT CAN-FIND(FIRST w-test
                    WHERE w-test EQ stack-flute.row-value[lir]) THEN DO:
      CREATE w-test.
      w-test = stack-flute.row-value[lir].
    END.
  END.
END.

FOR EACH w-test:
  FOR EACH stack NO-LOCK
      WHERE stack.reftable EQ "STACK"
        AND stack.company  EQ ""
        AND stack.loc      EQ ""
      BY stack.code:
    CREATE tt-stack.
    ASSIGN
     tt-test = w-test
     tt-stak = stack.code.
  END.
END.

ASSIGN
 lir = 0
 lic = 0.

FOR EACH stack-flute NO-LOCK
    WHERE stack-flute.company EQ new-stack-flute.company
      AND stack-flute.loc     EQ new-stack-flute.loc
      AND stack-flute.code    EQ new-stack-flute.code
      AND stack-flute.pallet  EQ new-stack-flute.pallet:

  DO lir = 1 TO EXTENT(stack-flute.row-value) - 1:
    IF CAN-FIND(FIRST w-test WHERE w-test EQ stack-flute.row-value[lir]) THEN
    DO lic = 1 TO EXTENT(stack-flute.col-value):
      FIND FIRST tt-stack
          WHERE tt-test EQ stack-flute.row-value[lir]
            AND tt-stak EQ stack-flute.col-value[lic]
          NO-ERROR.
      IF NOT AVAIL tt-stack THEN DO:
        CREATE tt-stack.
        ASSIGN
         tt-test = stack-flute.row-value[lir]
         tt-stak = stack-flute.col-value[lic].
      END.
      tt-valu = stack-flute.val[(lir * 10) + lic].
    END.
  END.
END.

FOR EACH tt-stack BREAK BY tt-test BY tt-stak:
  IF tt-test EQ ""         OR
     tt-stak EQ ""         OR
     NOT FIRST-OF(tt-stak) THEN DELETE tt-stack.
END.

ASSIGN
 li  = 1
 lir = 0
 lic = 0.

FOR EACH tt-stack BREAK BY tt-test BY tt-stak:
  IF FIRST-OF(tt-test) THEN DO:
    lir = lir + 1.

    IF lir GT EXTENT(stack-flute.row-value) - 1 THEN
      ASSIGN
       li  = li + 1
       lir = 1.
  END.

  ASSIGN
   tt-row  = lir
   tt-page = li.
END.

ASSIGN
 lip = li
 li  = 1.

FOR EACH tt-stack WHERE tt-page LE lip BREAK BY tt-stak BY tt-page:
  IF FIRST-OF(tt-stak) THEN DO:
    lic = lic + 1.

    IF lic GT EXTENT(stack-flute.col-value) THEN
      ASSIGN
       li  = li + 1
       lic = 1.
  END.

  ASSIGN
   tt-col  = lic
   tt-page = ((li - 1) * lip) + tt-page.
END.

FOR EACH tt-stack BY tt-page DESC:
  lip = tt-page.
  LEAVE.
END.

FOR EACH b-s-f
    WHERE b-s-f.company EQ new-stack-flute.company
      AND b-s-f.loc     EQ new-stack-flute.loc
      AND b-s-f.code    EQ new-stack-flute.code
      AND b-s-f.pallet  EQ new-stack-flute.pallet
      AND b-s-f.page-no GT lip:
  DELETE b-s-f.
END.

DO li = 1 TO lip:
  FIND FIRST stack-flute
      WHERE stack-flute.company EQ new-stack-flute.company
        AND stack-flute.loc     EQ new-stack-flute.loc
        AND stack-flute.code    EQ new-stack-flute.code
        AND stack-flute.pallet  EQ new-stack-flute.pallet
        AND stack-flute.page-no EQ li
      NO-ERROR.
  IF NOT AVAIL stack-flute THEN DO:
    CREATE stack-flute.
    ASSIGN
     stack-flute.company = new-stack-flute.company
     stack-flute.loc     = new-stack-flute.loc
     stack-flute.code    = new-stack-flute.code
     stack-flute.pallet  = new-stack-flute.pallet
     stack-flute.page-no = li.
  END.
  ASSIGN
   stack-flute.row-value = ""
   stack-flute.col-value = ""
   stack-flute.val       = 0.

  FOR EACH tt-stack WHERE tt-page EQ li BY tt-row BY tt-col:
    ASSIGN
     stack-flute.row-value[tt-row]           = tt-test
     stack-flute.col-value[tt-col]           = tt-stak
     stack-flute.val[(tt-row * 10) + tt-col] = tt-valu.
  END.
END.

/*li = 0.
FOR EACH stack-flute
    WHERE stack-flute.company EQ new-stack-flute.company
      AND stack-flute.loc     EQ new-stack-flute.loc
      AND stack-flute.code    EQ new-stack-flute.code
      AND stack-flute.pallet  EQ new-stack-flute.pallet
    BY stack-flute.page-no
    BY INT(ROWID(stack-flute) NE ROWID(new-stack-flute)):

  li = li + 1.

  IF ROWID(stack-flute) NE ROWID(new-stack-flute) THEN stack-flute.page-no = li.
END.*/
