{ed/sharedv.i}
{rc/hdg-wide.i "ed/rp832.p" "VENDOR CATALOG"}
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
for each eddoc no-lock
    WHERE eddoc.setid = "832"
    AND eddoc.partner MATCHES view_partner
    AND (IF lo_adddate = ? THEN TRUE ELSE eddoc.adddate >= lo_adddate)
    AND (IF hi_adddate = ? THEN TRUE ELSE eddoc.adddate <= hi_adddate)
    AND (IF view_unposted THEN eddoc.posted = FALSE ELSE TRUE)
:
    display eddoc with 3 columns frame doc width 144.
    for each edcat of eddoc no-lock:
        display edcat with 3 columns frame f-cat width 144.
        for each edcatline of edcat no-lock:
            display 
                edcatline with frame f-catline 3 columns width 144.
            /*
            with frame f-det down width 144:
            display
                edcatline.vendor-item       format 'x(15)'
                edcatline.description[1]    format 'x(40)'
                /* edcatline.description[2]    format 'x(01)' */
                edcatline.price             format "->>,>>9"
                .
            if description[2] > "" then do:
                down.
                display description[2] @ description[1].
            end.
            */
            for each edcatprice of edcatline:
                display edcatprice
                with frame f-catprice down width 144.
            end.
        end.
    end.
end.
{rc/endprint.i}
