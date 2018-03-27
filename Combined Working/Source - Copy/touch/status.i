/* status.i */

&Scoped-define BUTTON-COUNT 10

DEFINE INPUT-OUTPUT PARAMETER j AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.

DEFINE VARIABLE k            AS INTEGER   NO-UNDO.
DEFINE VARIABLE x            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cImage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLastName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFirstName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmployee    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEMail       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cActivity    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEditor      AS CHARACTER NO-UNDO.

ENABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
IF j + {&BUTTON-COUNT} GT NUM-ENTRIES(itemlist,'|') OR
   j GT NUM-ENTRIES(itemlist,'|') THEN
j = NUM-ENTRIES(itemlist,'|') - {&BUTTON-COUNT} + 1.
IF j LT 1 THEN
j = 1.
DO WITH FRAME {&FRAME-NAME}:
  DO k = j TO NUM-ENTRIES(itemlist,'|'):
    x = x + 1.
    IF x GT {&BUTTON-COUNT} THEN
    LEAVE.
    IF ipcType EQ 'Machine' THEN
    ASSIGN
      cImage     = ENTRY(1,ENTRY(k,itemlist,'|'))
      cLastName  = ENTRY(2,ENTRY(k,itemlist,'|'))
      cFirstName = ENTRY(3,ENTRY(k,itemlist,'|'))
      cEmployee  = ENTRY(4,ENTRY(k,itemlist,'|'))
      cEMail     = ENTRY(5,ENTRY(k,itemlist,'|'))
      cActivity  = ENTRY(6,ENTRY(k,itemlist,'|'))
      cEditor    = cLastName + ', '
                 + cFirstName + ' ('
                 + cEmployee + ')'
                 + CHR(10)
                 + cEMail + CHR(10)
                 + cActivity
                 .
    ELSE
    ASSIGN
      cImage       = ENTRY(1,ENTRY(k,itemlist,'|'))
      cMachine     = ENTRY(2,ENTRY(k,itemlist,'|'))
      cDescription = ENTRY(3,ENTRY(k,itemlist,'|'))
      cActivity    = ENTRY(4,ENTRY(k,itemlist,'|'))
      cEditor      = cMachine + CHR(10)
                   + cDescription + CHR(10)
                   + cActivity
                   .
    CASE x:
      WHEN 1 THEN DO:
        IMAGE-1:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-1:SCREEN-VALUE = cEditor.
      END.
      WHEN 2 THEN DO:
        IMAGE-2:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-2:SCREEN-VALUE = cEditor.
      END.
      WHEN 3 THEN DO:
        IMAGE-3:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-3:SCREEN-VALUE = cEditor.
      END.
      WHEN 4 THEN DO:
        IMAGE-4:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-4:SCREEN-VALUE = cEditor.
      END.
      WHEN 5 THEN DO:
        IMAGE-5:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-5:SCREEN-VALUE = cEditor.
      END.
      WHEN 6 THEN DO:
        IMAGE-6:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-6:SCREEN-VALUE = cEditor.
      END.
      WHEN 7 THEN DO:
        IMAGE-7:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-7:SCREEN-VALUE = cEditor.
      END.
      WHEN 8 THEN DO:
        IMAGE-8:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-8:SCREEN-VALUE = cEditor.
      END.
      WHEN 9 THEN DO:
        IMAGE-9:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-9:SCREEN-VALUE = cEditor.
      END.
      WHEN 10 THEN DO:
        IMAGE-10:LOAD-IMAGE(SEARCH(cImage)).
        EDITOR-10:SCREEN-VALUE = cEditor.
      END.
    END CASE.
  END.
  j = k.
  DO k = x + 1 TO {&BUTTON-COUNT}:
    CASE k:
      WHEN 1 THEN
      ASSIGN
        IMAGE-1:HIDDEN  = YES
        EDITOR-1:HIDDEN = YES
        .
      WHEN 2 THEN
      ASSIGN
        IMAGE-2:HIDDEN  = YES
        EDITOR-2:HIDDEN = YES
        .
      WHEN 3 THEN
      ASSIGN
        IMAGE-3:HIDDEN  = YES
        EDITOR-3:HIDDEN = YES
        .
      WHEN 4 THEN
      ASSIGN
        IMAGE-4:HIDDEN  = YES
        EDITOR-4:HIDDEN = YES
        .
      WHEN 5 THEN
      ASSIGN
        IMAGE-5:HIDDEN  = YES
        EDITOR-5:HIDDEN = YES
        .
      WHEN 6 THEN
      ASSIGN
        IMAGE-6:HIDDEN  = YES
        EDITOR-6:HIDDEN = YES
        .
      WHEN 7 THEN
      ASSIGN
        IMAGE-7:HIDDEN  = YES
        EDITOR-7:HIDDEN = YES
        .
      WHEN 8 THEN
      ASSIGN
        IMAGE-8:HIDDEN  = YES
        EDITOR-8:HIDDEN = YES
        .
      WHEN 9 THEN
      ASSIGN
        IMAGE-9:HIDDEN  = YES
        EDITOR-9:HIDDEN = YES
        .
      WHEN 10 THEN
      ASSIGN
        IMAGE-10:HIDDEN  = YES
        EDITOR-10:HIDDEN = YES
        .
    END CASE.
  END.
END.
