&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : brwcustom.i
    Purpose     : Browser Triggers and Internal Procedures

    Syntax      : {methods/template/brwcustom.i}

    Description : Browser customization as per new enhanced UI 

    Author(s)   : Anjly    (RStark)
    Created     : 08/10/20 (10.15.2020)
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(exclude-brw-custom) EQ 0 AND "{1}" EQ "" &THEN
DEFINE VARIABLE colHand     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colHandList AS CHARACTER     NO-UNDO.
DEFINE VARIABLE iCnt        AS INTEGER       NO-UNDO. 
&GLOBAL-DEFINE exclude-brw-custom TRUE
&ENDIF

&IF "{1}" NE "" &THEN
DEFINE VARIABLE colHand{1}     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colHandList{1} AS CHARACTER     NO-UNDO.
DEFINE VARIABLE iCnt{1}        AS INTEGER       NO-UNDO. 
&ENDIF

&IF DEFINED(sdBrowseName) EQ 0 &THEN
&Scoped-define sdBrowseName {&BROWSE-NAME}
&ENDIF

ASSIGN
    FRAME {&FRAME-NAME}:BGCOLOR       = 15
    {&sdBrowseName}:BGCOLOR           = 25
    {&sdBrowseName}:SEPARATOR-FGCOLOR = 15
    {&sdBrowseName}:ROW-HEIGHT-CHARS  = 0.84
    {&sdBrowseName}:FONT              = 1
    .

colHand{1} = {&sdBrowseName}:FIRST-COLUMN.
DO WHILE VALID-HANDLE(colHand{1}):
    ASSIGN
        colHandList{1} = colHandList{1} + ","  + STRING(colHand{1}).
        colHand{1}     = colHand{1}:NEXT-COLUMN
        .
END. /* do while */
colHandList{1} = TRIM(colHandList{1}, ",").
 
&IF DEFINED(exclude-row-display) EQ 0 &THEN
ON "ROW-DISPLAY":U OF {&sdBrowseName} 
DO:
    IF CURRENT-RESULT-ROW("{&sdBrowseName}") / 2 NE INTEGER(CURRENT-RESULT-ROW("{&sdBrowseName}") / 2) THEN 
    DO iCnt{1} = 1 TO NUM-ENTRIES(colHandList{1},","):
        ASSIGN
            colHand{1} = HANDLE(ENTRY(iCnt{1},colHandList{1},","))
            colHand{1}:BGCOLOR = 25
            . 
    END. /* do cnt */ 
    ELSE
    DO iCnt{1} = 1 TO NUM-ENTRIES(colHandList{1},","):
        ASSIGN
            colHand{1} = HANDLE(ENTRY(iCnt{1},colHandList{1},","))
            colHand{1}:BGCOLOR = 26
            .
    END. /* do cnt */
END.
&ENDIF
