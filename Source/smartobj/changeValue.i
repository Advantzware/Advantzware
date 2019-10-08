/* changeValue.i */

/* note: definitions section
         DEFINE VARIABLE changeValue AS <datatype?> NO-UNDO..
         value-change trigger of browser
         changeValue = IF AVAILABLE <table> THEN {1} ELSE <datatype value>.
*/

  DEFINE INPUT PARAMETER ipNavType AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  IF ipNavType EQ '' THEN RETURN.
  ASSIGN
    i = QUERY {&BROWSE-NAME}:CURRENT-RESULT-ROW
    j = i.
  CASE ipNavType:
    WHEN 'F':U THEN DO:
      GET FIRST {&BROWSE-NAME}.
      i = 1.
    END. /* next */
    WHEN 'N':U THEN DO:
      GET NEXT {&BROWSE-NAME}.
      DO WHILE AVAIL(quotehd):
        i = i + 1.
        IF changeValue NE {1} THEN LEAVE.
        GET NEXT {&BROWSE-NAME}.
      END. /* do while */
    END. /* next */
    WHEN 'P':U THEN DO:
      GET PREV {&BROWSE-NAME}.
      DO WHILE AVAIL(quotehd):
        i = i - 1.
        IF changeValue NE {1} THEN LEAVE.
        GET PREV {&BROWSE-NAME}.
      END. /* do while */
    END. /* next */
    WHEN 'L':U THEN DO:
      GET LAST {&BROWSE-NAME}.
      i = QUERY {&BROWSE-NAME}:NUM-RESULTS.
    END. /* next */
  END CASE.
  QUERY {&BROWSE-NAME}:REPOSITION-TO-ROW(i).
  RUN dispatch ('row-changed':U).
  ASSIGN
    changeValue = {1}
    opNavType = IF CAN-DO('F,L':U,ipNavType) THEN ipNavType
           ELSE IF ipNavType EQ 'N':U AND i EQ j THEN 'L':U
           ELSE IF ipNavType EQ 'P':U AND i EQ j THEN 'F':U
           ELSE IF i EQ j THEN 'B':U ELSE '':U.
