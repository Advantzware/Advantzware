&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : brwcustom.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwcustom.i}

    Description : Browser customization as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{methods/template/brwcustomdef.i}

ASSIGN
    FRAME {&FRAME-NAME}:BGCOLOR      = 15
    {&BROWSE-NAME}:BGCOLOR           = 25
    {&BROWSE-NAME}:FGCOLOR           = 0
    {&BROWSE-NAME}:SEPARATOR-FGCOLOR = 15
    {&BROWSE-NAME}:ROW-HEIGHT-CHARS  = 0.84
    {&BROWSE-NAME}:FONT              = 22
    {&BROWSE-NAME}:FIT-LAST-COLUMN   = TRUE
    .
hColumnRowColor = {&BROWSE-NAME}:FIRST-COLUMN.
DO WHILE VALID-HANDLE(hColumnRowColor):
    ASSIGN
        cColHandList    = cColHandList + ","  + string(hColumnRowColor)
        hColumnRowColor = hColumnRowColor:NEXT-COLUMN
        .
END. /* do while */
cColHandList = TRIM(cColHandList, ",").

&IF DEFINED(exclude-row-display) EQ 0 &THEN
ON ROW-DISPLAY OF {&BROWSE-NAME} DO:
    {methods/template/brwrowdisplay.i}
END.
&ENDIF
