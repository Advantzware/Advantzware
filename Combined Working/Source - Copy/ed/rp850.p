/***************************************************************************\
*****************************************************************************
**  Program: E:\CLIENTS\ASI\FOLD\RCO35\E
**       By: Chris Heins, RCI (c) 1997 All Rights Reserved.
** Descript: Print Purchase Orders
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/hdg-std.i "ed/rp850.p" "PURCHASE ORDER EDIT LIST"}
DEF VAR view_unposted AS LOGICAL NO-UNDO INITIAL TRUE LABEL "Unposted Only?".
DEF VAR view_partner LIKE ws_partner NO-UNDO.
DEF VAR lo_adddate AS DATE NO-UNDO LABEL "Created From".
DEF VAR hi_adddate AS DATE NO-UNDO LABEL "Thru".
{rc/statline.i}
{rc/viewline.i &displayf="view_partner
	help 'Enter specific partner code or * for ALL, HELP for pick list'
	validate(input frame f-view view_partner = '*'
	    or can-find(first edmast
	where edmast.partner = input frame f-view view_partner),
	'There are no partners which match your entry')
    view_unposted
	help 'Enter YES to print only unposted entries, NO for ALL'
    lo_adddate
	help 'Enter earliest create date or ? for any'
    hi_adddate
	help 'Enter most recent create date or ? for any'
    "}
view_partner = IF ws_partner > "" THEN
ws_partner ELSE "*"
.
DO WITH FRAME f-view:
  UPDATE
    view_partner
    view_unposted
    lo_adddate
    hi_adddate.
  IF view_partner = ""
    OR view_partner = ?
    THEN
  view_partner = "*".
END.
{rc/getprint.i}
{rc/hdg-noco.i}
VIEW FRAME hdg-std.
FOR EACH eddoc NO-LOCK
    WHERE eddoc.fgid = "PO"
    AND eddoc.partner MATCHES view_partner
    AND (IF lo_adddate = ? THEN TRUE ELSE eddoc.adddate >= lo_adddate)
    AND (IF hi_adddate = ? THEN TRUE ELSE eddoc.adddate <= hi_adddate)
    AND (IF view_unposted THEN eddoc.posted = FALSE ELSE TRUE)
    :
  DISPLAY eddoc WITH FRAME f-doc 3 COLUMN
    TITLE "EDDOC DOCUMENT" WIDTH 144.
  FIND edpotran OF eddoc.
  DISPLAY edpotran EXCEPT partner seq
    WITH FRAME f-tran
    3 COLUMN TITLE "EDPOTRAN HEADER" WIDTH 144.
  FOR EACH edpoline OF edpotran:
    DISPLAY edpoline EXCEPT partner seq
      WITH 3 COLUMN FRAME f-line TITLE "EDPOLINE ITEM" WIDTH 144.
  END.
  FOR EACH edpoaddon OF edpotran:
    DISPLAY edpoaddon EXCEPT partner seq
      WITH 3 COLUMN FRAME f-addon TITLE "EDPOADDON LINE".
  END.
END.
{rc/endprint.i}
