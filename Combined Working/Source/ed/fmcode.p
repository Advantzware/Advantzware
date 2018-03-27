/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\fmcode.p
**       By:
** Descript:
12.21.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Added file-list and proc-order.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/callvar.i}
FORM
  WITH FRAME f-det row 3 CENTER.
FORM
  edcode.path-in  COLON 10
  edcode.path-out COLON 10
  WITH FRAME f-details row 12 CENTER side-labels.
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
VIEW FRAME f-view.
IF NOT called THEN
DO:
  {ed/getpart.i}
END.
ELSE
DO:
  FIND edmast WHERE edmast.partner = ws_partner NO-LOCK NO-ERROR.
  IF NOT AVAIL edmast THEN
  DO:
    BELL.
    MESSAGE COLOR VALUE(c_err)
      "Invalid partner code:" ws_partner "setup first".
    PAUSE.
    RETURN.
  END.
END.
DISPLAY ws_partner WITH FRAME f-view
  .
{rc/scrvars.i}
{rc/scrfm3.i
  &FUNCTIONS  = "YYYY"
  &ROWS       = 5
  &TITLE      = " Transaction Code Setup "
  &FILE       = "EDCode"
  &INDEX      = " "
  &CONDITION  = "WHERE edcode.partner = ws_partner"
  &POSIT      = " "
  &DETFUNCT   = " "
  &CHOOSE     = "setid"
  &KEYEDIT    = " "
  &DISPLAYF   =
  "edcode.direction
    version
    agency
    customized
    custom-proc
    test-prod
    /* proc-order  column-label 'Order' */
   "
  &DATAEDIT   = " "
  &TERMKEY    = " "
  &UPFLDS     =
  "path-in    when edcode.direction = 'I'
   /* file-list  when edcode.direction = 'I' */
   path-out   when edcode.direction = 'O'
 "
  &HELPKEY    = " "
  &DETEDIT    = " "
  &ADDCODE    = "assign edcode.partner = ws_partner."
  }
/*
&ADDPOST    = " "
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
HIDE FRAME f-det NO-PAUSE.
HIDE FRAME f-details NO-PAUSE.
