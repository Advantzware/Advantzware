/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\asi\i864.
**       By: Chris Heins
** Descript: Import shipto addresses into host system.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF SHARED STREAM s-out.
DEF SHARED FRAME hdg-std.
{rc/hdg-wide.i "ed/asi/i864.p" "SHIPTO EDIT LIST" "(s-out)"}
{rc/stats.i}
{rc/hdg-noco.i}
VIEW STREAM s-out FRAME hdg-std.
start = TIME.
{rc/statline.i}
{rc/fcurrent.i}
start = TIME.
VIEW FRAME f-current.
VIEW FRAME f-stats.
DEF var char_seq AS char NO-UNDO.
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
      AND edshipto.ship-to = ?:
    {rc/incr.i ws_recs_read}.
    {rc/incr.i ws_recs_selected}.
    /*
    RUN ed/asi/shp2ship.p (RECID(edshipto), YES, YES).
    */
    DISPLAY STREAM S-OUT edshipto
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
PAUSE BEFORE-HIDE.
HIDE FRAME f-current.
HIDE FRAME f-stats.
