/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\fmxship.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/callvar.i}
{rc/statline.i}
{rc/viewline.i &displayf=ws_partner}
IF NOT called THEN
DO:
  {ed/getpart.i}
END.
ELSE
DISPLAY ws_partner WITH FRAME f-view.
FORM
  EDShipto.by-code
  EDShipto.ref-type
  EDShipto.st-code
  EDShipto.Cust
  edshipto.ship-to
  ws_char FORMAT "x(25)"
  WITH FRAME f-det row 3 COLUMN 2.
FORM
  EDShipto.Name  COLON 8
  EDShipto.Phone  COLON 60
  EDShipto.Addr1 COLON 8
  EDShipto.Fax    COLON 60
  EDShipto.Addr2 COLON 8
  EDShipto.Opened COLON 60
  EDShipto.City LABEL "C/S/Z" COLON 8
  EDShipto.State NO-LABEL EDShipto.Zip NO-LABEL
  EDShipto.Dest-Zone COLON 60
  EDShipto.Attention LABEL "Att" COLON 8
  edshipto.cust-region COLON 60
  EDShipto.Description LABEL "Descr" COLON 8
  WITH FRAME f-details row 15 COLUMN 1 side-labels.
FORM EDShipto.Comments[1 FOR 5] WITH FRAME f-comments CENTER TITLE "Comments"
  OVERLAY NO-LABELS.
{rc/scrvars.i}
{rc/scrfm3.i
  &FUNCTIONS  = "YYYY"
  &init = "f-details-title = 'Details - Press f3 for Comments'."
  &ROWS       = 8
  &TITLE      = " Partner Location Cross-Ref Setup "
  &FILE       = "edshipto"
  &INDEX      = " "
  &CONDITION  = "WHERE edshipto.partner = ws_partner"
  &POSIT      = " "
  &DETFUNCT   =
  "ws_char = edshipto.city + ', ' + edshipto.state.
    display ws_char column-label 'City/State'
    with frame f-det."
  &CHOOSE     = "by-code"
  &KEYEDIT    = " "
  &DISPLAYF   = "
  EDShipto.by-code
  EDShipto.ref-type
  EDShipto.st-code
  EDShipto.Cust
  edshipto.ship-to
  "
  &DATAEDIT   = " "
  &TERMKEY    = " "
  &UPFLDS     =
  "
  EDShipto.Name
  EDShipto.Addr1
  EDShipto.Addr2
  EDShipto.City
  EDShipto.State
  EDShipto.Zip
  EDShipto.Attention
  EDShipto.Description
  EDShipto.Phone
  EDShipto.Fax
  EDShipto.Opened
  EDShipto.Dest-Zone
  EDShipto.cust-region
"
  &HELPKEY    = " "
  &DETEDIT    =
  "if keylabel(lastkey) = 'f3' then do:
    update text(edshipto.comments) with frame f-comments.
    hide frame f-comments.
    next.
 end."
  &ADDCODE    = "assign edshipto.partner = ws_partner."
  }
/*
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
&ADDPOST    = " "
*/
