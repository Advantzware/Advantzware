&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browser.i
    Purpose     : Browser Triggers and Internal Procedures

    Syntax      : {methods/template/browser.i}

    Description : Browser customization as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


&if defined(exclude-brw-custom) = 0 &then
DEFINE VARIABLE colHand      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colHandList AS CHAR NO-UNDO.
DEFINE VARIABLE icnt        as integer no-undo. 

&global-define exclude-brw-custom true
&endif

frame {&frame-name}:BGCOLOR = 15.
{&BROWSE-NAME}:bgcolor = 25.
{&BROWSE-NAME}:separator-fgcolor = 15.
{&BROWSE-NAME}:row-height-chars = 0.84.
{&BROWSE-NAME}:font = 22.

colHand = {&BROWSE-NAME}:first-column.
do while valid-handle(colHand).
colHandList =  colHandList + ","  + string(colHand).
colHand = colHand:next-column.
END.
 colHandList = trim(colHandList, ",").
 
  &if defined(exclude-row-display) = 0 &then
 on 'row-display' of {&BROWSE-NAME} do:
  IF CURRENT-RESULT-ROW("{&BROWSE-NAME}") / 2 <> INT (CURRENT-RESULT-ROW("{&BROWSE-NAME}") / 2) then 
  DO iCnt = 1 TO num-entries(colHandList, ","): 
     colHand = handle(entry(iCnt,colHandList,",")).
     colHand:BGCOLOR = 25. 
  END. 
  else
  DO iCnt = 1 TO num-entries(colHandList, ","):
     colHand = handle(entry(iCnt,colHandList,",")).
     colHand:BGCOLOR = 26.
  END.
end.
&endif




