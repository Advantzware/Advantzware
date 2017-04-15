/* farmNav.i */

  DEFINE INPUT PARAMETER ipFarmNav AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.

  DEFINE VARIABLE ebRowID AS ROWID NO-UNDO EXTENT 2.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ebRowID = ROWID(eb).
  CASE ipFarmNav:
    WHEN 'F':U THEN DO:
      GET FIRST {&BROWSE-NAME}.
      DO WHILE AVAILABLE(eb):
        IF eb.pur-man THEN DO:
          ebRowID[2] = ROWID(eb).
          LEAVE.
        END.
        GET NEXT {&BROWSE-NAME}.
      END. /* while avail */
    END. /* first */
    WHEN 'N':U THEN DO:
      GET NEXT {&BROWSE-NAME}.
      DO WHILE AVAILABLE(eb):
        IF eb.pur-man THEN DO:
          ebRowID[2] = ROWID(eb).
          LEAVE.
        END.
        GET NEXT {&BROWSE-NAME}.
      END. /* while avail */
    END. /* next */
    WHEN 'P':U THEN DO:
      GET PREV {&BROWSE-NAME}.
      DO WHILE AVAILABLE(eb):
        IF eb.pur-man THEN DO:
          ebRowID[2] = ROWID(eb).
          LEAVE.
        END.
        GET PREV {&BROWSE-NAME}.
      END. /* while avail */
    END. /* prev */
    WHEN 'L':U THEN DO:
      GET LAST {&BROWSE-NAME}.
      DO WHILE AVAILABLE(eb):
        IF eb.pur-man THEN DO:
          ebRowID[2] = ROWID(eb).
          LEAVE.
        END.
        GET PREV {&BROWSE-NAME}.
      END. /* while avail */
    END. /* last */
  END CASE.
  IF ipFarmNav NE '' THEN DO:
    i = 1.
    GET FIRST {&BROWSE-NAME}.
    DO WHILE AVAILABLE(eb):
      IF ebRowID[2] EQ ROWID(eb) THEN LEAVE.
      GET NEXT {&BROWSE-NAME}.
      i = i + 1.
    END. /* while avail */
    QUERY {&BROWSE-NAME}:REPOSITION-TO-ROW(i).
    RUN dispatch('row-changed':U).
    opNavType = IF CAN-DO('F,L':U,ipFarmNav) THEN ipFarmNav
           ELSE IF ipFarmNav EQ 'N':U AND ebRowID[1] EQ ebRowID[2] THEN 'L':U
           ELSE IF ipFarmNav EQ 'P':U AND ebRowID[1] EQ ebRowID[2] THEN 'F':U
           ELSE IF ebRowID[1] EQ ebRowID[2] THEN 'B':U ELSE '':U.
  END. /* if ipfarmnav */
