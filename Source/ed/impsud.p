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
{rc/gettextv.i}
filename = "ab.prn".
{rc/gettextq.i "stream s-in"}
FIND edmast WHERE edmast.partner = ws_partner NO-LOCK.
HIDE FRAME f-textfile.
VIEW FRAME f-current.
VIEW FRAME f-stats.
start = TIME.
REPEAT:
  IMPORT STREAM s-in str_buffa.
  {rc/incr.i ws_recs_read}.
  IF NOT str_buffa BEGINS "0" THEN
  NEXT.
  {rc/incr.i ws_recs_selected}.
  ASSIGN
    {rc/substr.i ws_by-code 1 7}
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
    edshipto.name = "SEARS, ROEBUCK & CO."
    {rc/substr.i edshipto.addr1             10 30}
    {rc/substr.i edshipto.addr2             42 30}
    {rc/substr.i edshipto.city              74 13}
    {rc/substr.i edshipto.state             89 02}
    {rc/substr.i edshipto.zip               93 09}
    {rc/substr.i edshipto.description       104 30}
    {rc/substr.i edshipto.cust-region       136 07}
    {rc/substr.i edshipto.dest-zone         145 02}
    {rc/substr.i ws_decimal                 149 05 DECIMAL}
    {rc/substr.i edshipto.phone             156 12}
    .
  IF ws_decimal > 0 AND ws_decimal < 99999 THEN
  RUN rc/jul2greg.p (INPUT ws_decimal,
    OUTPUT edshipto.opened, OUTPUT ws_int).
  ELSE
  edshipto.opened = ?.
  /*
  RUN ed/fixidin.p (RECID(edmast), edshipto.st-code,
    OUTPUT edshipto.st-code).
  */
  RUN ed/fixidin.p (RECID(edmast), edshipto.cust-region,
    OUTPUT edshipto.cust-region).
  if not edshipto.dest-zone begins "DZ" then do:
    edshipto.dest-zone = "DZ" + edshipto.dest-zone.
  end.
  if length(edshipto.zip) = 9
  then edshipto.zip = substring(edshipto.zip,1,5) + '-'
        + substring(edshipto.zip,6,4).
  DISPLAY addr1 city state zip phone opened WITH FRAME f-current.
  {rc/incr.i ws_recs_changed}.
  {rc/statsdis.i}
END.
INPUT STREAM s-in CLOSE.
{rc/statsdis.i}
