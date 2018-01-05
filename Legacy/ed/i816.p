/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\i816.
**       By: Chris Heins
** Descript: Import shipto addresses into host system.
04.30.99 by CAH on \\ricky\asi\patch Log#0000:
1.  Modified to exclude shiptos which have no city/state/zip.
**
*****************************************************************************
\***************************************************************************/
PAUSE 0.
{ed/sharedv.i}
DEF SHARED STREAM s-out.
DEF SHARED FRAME hdg-std.
{rc/hdg-wide.i "ed/i816.p" "SHIPTO IMPORT EDIT LIST" "(s-out)"}
{rc/stats.i}
{rc/hdg-noco.i}
VIEW STREAM s-out FRAME hdg-std.
start = TIME.
{rc/statline.i}
{rc/fcurrent.i}
start = TIME.
pause 0.
VIEW FRAME f-current.
VIEW FRAME f-stats.
DEF var char_seq AS char NO-UNDO.
DEF var import_program LIKE next_program NO-UNDO.
FIND edco WHERE RECID(edco) = ws_edco_rec NO-LOCK NO-ERROR.
IF NOT AVAIL edco THEN
RETURN error.
import_program = "ed/" + edco.system + "/shp2ship.p".
IF SEARCH(import_program) = ? THEN
DO:
  ws_char = "Can't find import procedure for this system: " + import_program.
  bell.
  message color value(c_err) ws_char.
  pause 2.
  RUN rc/debugmsg.p (ws_char).
  RETURN error.
END.
FOR EACH eddoc
    WHERE eddoc.partner = ws_partner
    AND eddoc.setid = ws_setid
    AND eddoc.posted = FALSE:
  char_seq = string(eddoc.seq).
  DISPLAY
    eddoc.partner
    eddoc.seq
    eddoc.direction
    eddoc.setid
    eddoc.adddate
    eddoc.status-flag
    eddoc.error-count
    WITH FRAME f-current.
  FIND edmast WHERE edmast.partner = eddoc.partner NO-LOCK NO-ERROR.
  ws_edmast_rec = RECID(edmast).
  ASSIGN
    {rc/incr.i ws_amt_read}
    {rc/incr.i ws_amt_selected}
    .
  FOR EACH edshipto
      WHERE edshipto.partner = ws_partner
      AND edshipto.description = char_seq
      AND edshipto.ship-to = ?
      and (edshipto.city > "" and edshipto.state > "" and edshipto.zip > ""):
    {rc/incr.i ws_recs_read}.
    {rc/incr.i ws_recs_selected}.
    RUN VALUE(import_program) (RECID(edshipto), YES, YES).
    if top-debug then DISPLAY STREAM S-OUT edshipto
      WITH FRAME F-SHIPTO SIDE-LABELS WIDTH 144 3 COLUMNS.
    IF ws_recs_read MOD 10 = 0 THEN
    DO:
      {rc/statsdis.i}
    END.
  END.
  ASSIGN
    eddoc.posted = TRUE
    eddoc.openitem = FALSE
    eddoc.stat = 9
    eddoc.status-flag = "WIP"
    {rc/stampcht.i eddoc}
    {rc/incr.i ws_amt_changed}
    eddoc.interpdate = TODAY
    eddoc.interptime = TIME
    .
END.
{rc/statsdis.i}
/* 9810 CAH: clear error status */
error-status:error = false.
pause 0.
HIDE FRAME f-current.
HIDE FRAME f-stats.
