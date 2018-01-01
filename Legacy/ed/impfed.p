/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\IMPSUD.P
**       By: Chris Heins RCI (c) 1997
** Descript: Import Sears Unit Directory into EDI tables.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
{rc/stringv.i}
{rc/fcurrent.i}
{rc/statline.i}
{rc/stats.i}
DEF STREAM s-in.
DEF VAR ws_by-code LIKE edshipto.by-code NO-UNDO.
DEF VAR ws_ref-type LIKE edshipto.ref-type NO-UNDO LABEL "Ref Type".
{rc/viewline.i &displayf="ws_partner ws_ref-type"}
{ed/getpart.i}
UPDATE ws_ref-type
  HELP "Enter reference type for this list, e.g. BY=Store, ST=Warehouse"
  WITH FRAME f-view.
def var c as char no-undo extent 20.
{rc/gettextv.i}
filename = "feder.csv".
{rc/gettextq.i "stream s-in"}
FIND edmast WHERE edmast.partner = ws_partner NO-LOCK.
HIDE FRAME f-textfile.
VIEW FRAME f-current.
VIEW FRAME f-stats.
start = TIME.
REPEAT:
  C = ''.
  IMPORT STREAM s-in c.
  {rc/incr.i ws_recs_read}.
  IF NOT c[1] > "1" AND C[1] < "99999" THEN
  NEXT.
  {rc/incr.i ws_recs_selected}.
  ASSIGN
    ws_by-code = c[1]
    .
  RUN ed/fixidin.p (RECID(edmast), ws_by-code,
    OUTPUT ws_by-code).
  DISPLAY
    ws_by-code
    WITH FRAME f-current.
  FIND edshipto WHERE edshipto.partner = ws_partner
    AND edshipto.ref-type = ws_ref-type
    AND edshipto.by-code = ws_by-code EXCLUSIVE NO-ERROR.
  IF NOT AVAIL edshipto THEN
  DO:
    CREATE edshipto.
    ASSIGN
      edshipto.partner = ws_partner
      edshipto.ref-type = ws_ref-type
      edshipto.by-code = ws_by-code
      {rc/incr.i ws_recs_added}.
  END.
  ASSIGN
    edshipto.name       = c[2]
    edshipto.addr1      = c[3]
    edshipto.addr2      = ''
    edshipto.city       = c[4]
    edshipto.state      = c[5]
    edshipto.zip        = c[6]
    edshipto.phone      = c[7]
    .
  if length(edshipto.zip) = 9
  then edshipto.zip = substring(edshipto.zip,1,5) + '-'
	+ substring(edshipto.zip,6,4).
  DISPLAY name addr1 city state zip phone opened WITH FRAME f-current.
  {rc/incr.i ws_recs_changed}.
  {rc/statsdis.i}
END.
INPUT STREAM s-in CLOSE.
{rc/statsdis.i}
/*
"1" "7101QNS" "MACY'S REGO PARK" "QUEENS" "11373" "8-549-7200"
"2" "7101QUE" "MACY'S QUEENS" "QUEENS" "11373" "718/271-7200  X"
"3" "7102BKN" "MACY'S FULTON STREET" "BROOKLYN" "11201" "718/875-7200"
"4" "7102BRK" "MACY'S BROOKLYN" "BROOKLYN" "11202" "718-802-7363"
*/
