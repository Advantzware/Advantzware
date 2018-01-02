/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\ASI\IMPSHIP.P
**       By: Chris Heins, Report Concepts (c) 1997
** Descript: Update ASI Ship and Sold to's from EDShipto Master
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
{rc/stats.i}
{rc/viewline.i &displayf="ws_partner cust.cust-no cust.name no-label"}
{ed/getpart.i}
{rc/fcurrent.i}
{rc/ftopsl.i "Options"}
DEF VAR i AS INT NO-UNDO.
DEF VAR ref-type-list AS CHAR NO-UNDO FORMAT 'x(30)' INITIAL "*"
  LABEL "EDI Shipto file record types".
DEF VAR create-shipto AS LOGICAL NO-UNDO
  LABEL "Create Shipto?".
DEF VAR create-soldto AS LOGICAL NO-UNDO
  LABEL "Create Soldto?".
FIND cust WHERE cust.company = ws_company
  AND cust.cust-no = edmast.cust NO-LOCK.
DISPLAY cust.cust-no cust.name WITH FRAME f-view.
DO WITH FRAME f-top ON ERROR UNDO, RETRY:
  UPDATE
    ref-type-list   COLON 30
    HELP "BY=Stores, ST=Warehouses, etc."
    create-shipto   COLON 30
    HELP "Enter YES to create shipto records (Normally with BY,ST codes)"
    create-soldto   COLON 30
    HELP "Enter YES to create soldto records (Normally with ST codes)"
    WITH FRAME f-top.
  IF create-shipto = FALSE AND create-soldto = FALSE
    THEN
  DO:
    BELL.
    MESSAGE COLOR VALUE(c_err) "You must select at least one create option".
    UNDO, RETRY.
  END.
  {rc/confirm.i}
  IF NOT confirm THEN
  RETURN.
END.
HIDE FRAME f-top NO-PAUSE.
start = TIME.
VIEW FRAME f-current.
VIEW FRAME f-stats.
FOR EACH edshipto OF edmast EXCLUSIVE-LOCK
    WHERE CAN-DO(ref-type-list, edshipto.ref-type):
  DISPLAY
    edshipto.ref-type   COLUMN-LABEL "Type"
    edshipto.by-code    COLUMN-LABEL "BY"
    edshipto.addr1      COLUMN-LABEL "Address"  FORMAT 'x(20)'
    edshipto.city       COLUMN-LABEL "City"     FORMAT 'x(15)'
    edshipto.state      COLUMN-LABEL "St"
    edshipto.zip        COLUMN-LABEL "Zip"
    WITH FRAME f-current.
  ASSIGN
    {rc/incr.i ws_recs_read}
    {rc/incr.i ws_recs_selected}
    .
  IF create-shipto THEN
  DO:
    if edmast.ship-to-mask <> 0 then do:
        ws_int = integer(edshipto.by-code) + edmast.ship-to-mask.
        ws_char = string(ws_int).
    end.
    else ws_char = edshipto.by-code.
    FIND FIRST shipto
      WHERE shipto.company = cust.company
      AND shipto.cust-no = cust.cust-no
      AND shipto.ship-id = ws_char
      NO-ERROR.
    IF NOT AVAIL shipto THEN
    DO:
      CREATE shipto.
      ASSIGN
        shipto.ship-name    = cust.name
        shipto.ship-addr[1] = cust.addr[1]
        shipto.ship-addr[2] = cust.addr[2]
        shipto.ship-city    = cust.city
        shipto.ship-state   = cust.state
        shipto.ship-zip     = cust.zip
        shipto.loc          = cust.loc
        shipto.carrier      = cust.carrier
        shipto.notes[1]     = cust.notes[1]
        shipto.notes[2]     = cust.notes[2]
        shipto.notes[3]     = cust.notes[3]
        shipto.notes[4]     = cust.notes[4]
        shipto.Postal       = cust.Postal
        shipto.country      = cust.country
        shipto.ship-id      = ws_char
        edshipto.cust       = edmast.cust
        edshipto.ship-to    = ws_char
        .
      {rc/incr.i ws_recs_added}.
    END.
    /* fields not in cust
    assign
    shipto.dest-dscr    = cust.dest-dscr
    shipto.del-time     = cust.del-time
    shipto.del-chg      = cust.del-chg
    shipto.broker       = cust.broker
    .
    */
    /* override defaults with edshipto fields where applicable */
    IF edshipto.city > "" AND edshipto.state > "" AND edshipto.zip > ""
      THEN
    ASSIGN
      shipto.ship-addr[1] = edshipto.addr1
      shipto.ship-addr[2] = edshipto.addr2
      shipto.ship-city = edshipto.city
      shipto.ship-state = edshipto.state
      shipto.ship-zip = edshipto.zip
      shipto.country = "".
    IF edshipto.name > "" THEN
    ASSIGN shipto.ship-name = edshipto.name.
    DO i = 1 TO 4:
      IF edshipto.comments[i] > "" THEN
      ASSIGN shipto.notes[i]
        = edshipto.comments[i].
    END.
    edshipto.ship-to = shipto.ship-id.
    {rc/incr.i ws_recs_changed}.
  END. /* ship-to's */
  IF create-soldto THEN
  DO:
    if edmast.ship-to-mask <> 0 then do:
        ws_int = integer(edshipto.by-code) + edmast.ship-to-mask.
        ws_char = string(ws_int).
    end.
    else ws_char = edshipto.by-code.
    FIND soldto WHERE soldto.company = cust.company
      AND soldto.cust-no = cust.cust-no
      AND soldto.sold-id = ws_char NO-ERROR.
    IF NOT AVAIL soldto THEN
    DO:
      FIND LAST soldto WHERE soldto.company = cust.company AND
        soldto.cust-no = cust.cust-no
        USE-INDEX sold-no NO-LOCK NO-ERROR.
      IF AVAIL soldto THEN
      ASSIGN i = soldto.sold-no + 1.
      ELSE
      ASSIGN i = 1.
      CREATE soldto.
      ASSIGN
        soldto.company   = cust.company
        soldto.cust-no   = cust.cust-no
        soldto.sold-name = cust.name
        soldto.sold-no   = i
        soldto.sold-id   = ws_char.
      {rc/incr.i ws_recs_added}.
    END.
    /* override defaults with edshipto fields where applicable */
    IF edshipto.city > "" AND edshipto.state > "" AND edshipto.zip > ""
      THEN
    ASSIGN
      soldto.sold-addr[1] = edshipto.addr1
      soldto.sold-addr[2] = edshipto.addr2
      soldto.sold-city    = edshipto.city
      soldto.sold-state   = edshipto.state
      soldto.sold-zip     = edshipto.zip
      soldto.country      = edshipto.country
      .
    IF edshipto.name > "" THEN
    ASSIGN soldto.sold-name = edshipto.name.
    IF edshipto.ship-to = ""
      THEN
    ASSIGN edshipto.ship-to = soldto.sold-id.
    {rc/incr.i ws_recs_changed}.
  END.    /* create sold to's */
  IF ws_recs_read MOD 10 = 0 THEN
  DO:
    {rc/statsdis.i}
  END.
END.
{rc/statsdis.i}
HIDE FRAME f-current.
HIDE FRAME f-stats.
