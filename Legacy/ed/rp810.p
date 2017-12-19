/***************************************************************************\
*****************************************************************************
**  Program: E:\CLIENTS\ASI\FOLD\RCO35\E
**       By: Chris Heins, RCI (c) 1997 All Rights Reserved.
** Descript: Print Invoice Edit list.
10.17.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added record selection on partner, create dates, posted status.
2.  Reformatted for 144 cols, 3 column groups, remove repeating fields, etc.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/hdg-std.i "ed/rp810.p" "INVOICE EDIT LIST"}
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
printdest = "CLIPBOARD".
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
    WHERE eddoc.fgid = "IN"
    AND eddoc.partner MATCHES view_partner
    AND (IF lo_adddate = ? THEN TRUE ELSE eddoc.adddate >= lo_adddate)
    AND (IF hi_adddate = ? THEN TRUE ELSE eddoc.adddate <= hi_adddate)
    AND (IF view_unposted THEN eddoc.posted = FALSE ELSE TRUE)
    :
  DISPLAY eddoc WITH FRAME f-doc 3 COLUMN
    TITLE "EDDOC DOCUMENT" WIDTH 144.
  FIND edivtran OF eddoc.
  DISPLAY edivtran EXCEPT partner seq
    WITH FRAME f-tran 3 COLUMN TITLE "EDIVTRAN HEADER" WIDTH 144.
  FOR EACH edivline OF edivtran:
    DISPLAY edivline EXCEPT partner seq
      WITH 3 COLUMN FRAME f-line TITLE "EDIVLINE ITEM" WIDTH 144.
  END.
  FOR EACH edivaddon OF edivtran:
    DISPLAY edivaddon EXCEPT partner seq
      WITH 3 COLUMN FRAME f-addon TITLE "EDIVADDON LINE" WIDTH 144.
  END.
END.
{rc/endprint.i}
