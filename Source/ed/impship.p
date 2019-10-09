/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\IMPSUD.P
**       By: Chris Heins RCI (c) 1997
** Descript: Import Sears Unit Directory into EDI tables.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF buffer DC FOR edshipto.
{rc/statline.i}
{rc/stats.i "new shared"}
{rc/fcurrent.i}
{rc/ftopsl.i "Options"}
{ed/edivars.i}
{rc/stringv.i}
{rc/fcurrent.i}
DEF STREAM s-in.
DEF VAR i AS INT NO-UNDO.
DEF VAR ws_ref-type LIKE edshipto.ref-type NO-UNDO LABEL "Ref Type"
  INITIAL "BY".
DEF VAR ref-type-list AS CHAR NO-UNDO FORMAT 'x(30)' INITIAL "*"
  LABEL "EDI Shipto file record types".
DEF VAR create-shipto AS LOGICAL NO-UNDO
  LABEL "Create Shipto?" initial TRUE.
DEF VAR create-soldto AS LOGICAL NO-UNDO
  LABEL "Create Soldto?" initial TRUE.
DEF var back_end AS logical NO-UNDO initial FALSE.
DEF VAR ws_by-code LIKE edshipto.by-code NO-UNDO.
DEF var c AS char NO-UNDO extent 30.
{rc/viewline.i &displayf="ws_partner edmast.cust no-label
    ws_ref-type
    create-shipto
    create-soldto"}
VIEW FRAME f-view.
FIND EDCO WHERE RECID(EDCO) = WS_EDCO_REC NO-LOCK NO-ERROR.
IF NOT AVAIL EDCO THEN FIND FIRST EDCO NO-LOCK.
ws_company = edco.company.
next_program = "ed/" + edco.system + "/shp2ship.p".
IF SEARCH(next_program) = ? THEN
DO:
  BELL.
  ws_char = "Could not locate import procedure: " + next_program.
  MESSAGE COLOR VALUE(c_err) ws_char.
  RUN rc/debugmsg.p (ws_char).
  MESSAGE COLOR VALUE(c_err)
    "EDI Shipto to Application Shipto will not be run".
  PAUSE.
  ASSIGN back_end = FALSE create-shipto = FALSE create-soldto = FALSE.
  DISPLAY create-shipto create-soldto WITH FRAME f-view.
END.
ELSE
back_end = TRUE.
{ed/getpart.i}
FIND edmast WHERE edmast.partner = ws_partner NO-LOCK NO-ERROR.
ws_edmast_rec = RECID(edmast).
FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK NO-ERROR.
ws_partner = edmast.partner.
DISPLAY ws_partner edmast.cust
  WITH FRAME f-view.
UPDATE ws_ref-type
  HELP "Enter reference type for this list, e.g. BY=Store, ST=Warehouse"
  create-shipto WHEN back_end
  create-soldto WHEN back_end
  WITH FRAME f-view.
{rc/gettextv.i}
ASSIGN
  filename = "shipto.csv"
  WS_DELIM = ','.
on f5 get.
{rc/gettextq.i "stream s-in"}
on f5 help.
HIDE FRAME f-textfile.
VIEW FRAME f-current.
VIEW FRAME f-stats.
start = TIME.
REPEAT:
  C = ''.
  IMPORT STREAM s-in c.
  {rc/incr.i ws_recs_read}.
  IF c[1] <= " " THEN
  NEXT.
  IF c[1] BEGINS "shipto" THEN
  NEXT.
  {rc/incr.i ws_recs_selected}.
  DO i = 1 TO 30:
    c[i] = TRIM(c[i]).
  END.
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
      edshipto.ship-to = ?  /* flag that is has not been written to app-db */
      {rc/incr.i ws_recs_added}.
  END.
  ASSIGN
    edshipto.name       = c[2]
    edshipto.addr1      = c[3]
    edshipto.addr2      = c[4]
    edshipto.city       = c[5]
    edshipto.state      = c[6]
    edshipto.zip        = c[7]
    edshipto.attention  = c[8]
    edshipto.phone      = c[9]
    edshipto.fax        = c[10]
    edshipto.st-code    = c[11]
    edshipto.cust-region = c[12] + "," + c[13] + "," + c[14]
        /* tax-code,warehouse,carrier */
    edshipto.dest-zone  = c[15]
    edshipto.comments[1] = c[16]
    edshipto.comments[2] = c[17]
    edshipto.comments[3] = c[18]
    edshipto.comments[4] = c[19]
    .
  /* touchups */
  ASSIGN
    edshipto.st-code = edshipto.by-code
    edshipto.cust = edmast.cust
    edshipto.attention = "RECEIVING"
    edshipto.description = "Created: " + string(TODAY).
  IF length(edshipto.zip) < 5
    AND edshipto.zip > ""
    THEN
  DO:
    RUN rc/str2int.p (edshipto.zip, OUTPUT ws_int).
    edshipto.zip = string(ws_int, "99999").
  END.
  IF length(edshipto.zip) = 9
    THEN
  edshipto.zip = substring(edshipto.zip,1,5) + '-'
  + substring(edshipto.zip,6,4).
  DISPLAY name addr1 city state zip phone opened WITH FRAME f-current.
  {rc/incr.i ws_recs_changed}.
  if ws_recs_read mod 10 = 0 then do:
  {rc/statsdis.i}
  end.
END.
INPUT STREAM s-in CLOSE.
{rc/statsdis.i}
/*
"1" "7101QNS" "MACY'S REGO PARK" "QUEENS" "11373" "8-549-7200"
"2" "7101QUE" "MACY'S QUEENS" "QUEENS" "11373" "718/271-7200  X"
"3" "7102BKN" "MACY'S FULTON STREET" "BROOKLYN" "11201" "718/875-7200"
"4" "7102BRK" "MACY'S BROOKLYN" "BROOKLYN" "11202" "718-802-7363"
*/
IF back_end = FALSE THEN
RETURN.
HIDE MESSAGE NO-PAUSE.
MESSAGE COLOR VALUE(c_wrn) "About to run back end database import".
PAUSE 5.
start = TIME.
{rc/statsclr.i}
VIEW FRAME f-current.
VIEW FRAME f-stats.
FOR EACH edshipto OF edmast EXCLUSIVE-LOCK
    WHERE CAN-DO(ref-type-list, edshipto.ref-type):
  ASSIGN
    {rc/incr.i ws_recs_read}
    {rc/incr.i ws_recs_selected}
    .
  DISPLAY
    edshipto.by-code @ ws_by-code
    name addr1 city state zip phone opened 
    WITH FRAME f-current.
  RUN VALUE(next_program) (RECID(edshipto), create-shipto, create-soldto).
  IF ws_recs_read MOD 10 = 0 THEN
  DO:
    {rc/statsdis.i}
  END.
END.
{rc/statsdis.i}
PAUSE BEFORE-HIDE.
HIDE FRAME f-current.
HIDE FRAME f-stats.
