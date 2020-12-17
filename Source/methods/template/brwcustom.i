&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browsercustom.i
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

FRAME {&frame-name}:BGCOLOR      = 15.
{&BROWSE-NAME}:BGCOLOR           = 25.
{&BROWSE-NAME}:FGCOLOR           = 0.
{&BROWSE-NAME}:SEPARATOR-FGCOLOR = 15.
{&BROWSE-NAME}:ROW-HEIGHT-CHARS  = 0.84.
{&BROWSE-NAME}:FONT              = 22.

hColumnRowColor = {&BROWSE-NAME}:first-column.
DO WHILE VALID-HANDLE(hColumnRowColor).
    cColHandList =  cColHandList + ","  + string(hColumnRowColor).
    hColumnRowColor      = hColumnRowColor:NEXT-COLUMN.
END.
cColHandList = TRIM(cColHandList, ",").

&if defined(exclude-row-display) = 0 &then
    ON 'row-display' OF {&BROWSE-NAME} DO:
        {methods/template/brwrowdisplay.i}
    END.
&endif




