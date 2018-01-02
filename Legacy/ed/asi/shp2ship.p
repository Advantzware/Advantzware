/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\asi\shp2s
**       By:
** Descript:
10.06.98 by CAH on \\ricky\rv8 Log#0000:
1.  Enhanced to overwrite if necessary from edshipto.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_rec AS RECID NO-UNDO.
DEF INPUT PARAM create-shipto AS LOGICAL NO-UNDO
  LABEL "Create Shipto?".
DEF INPUT PARAM create-soldto AS LOGICAL NO-UNDO
  LABEL "Create Soldto?".
{ed/sharedv.i}
{rc/statvars.i "shared" }
DEF VAR i AS INT NO-UNDO.
FIND edshipto WHERE RECID(edshipto) = p_rec EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL edshipto THEN
RETURN error.
FIND edmast OF edshipto NO-LOCK NO-ERROR.
IF NOT AVAIL edmast THEN
RETURN error.
FIND cust WHERE cust.company = ws_company
  AND cust.cust-no = edmast.cust NO-LOCK.
IF NOT AVAIL cust THEN
RETURN error.
DEF buffer DC FOR edshipto.
IF edshipto.st-code = ""
  AND edshipto.dest-zone > ""
  THEN
DO:   /* xref to default ST code */
  FIND FIRST dc
    WHERE dc.partner = edshipto.partner
    AND dc.ref-type = "ST"
    AND dc.dest-zone = edshipto.dest-zone
    AND dc.CUST-region <> "DELETED"
    NO-LOCK NO-ERROR.
  IF AVAIL dc THEN
  ASSIGN edshipto.st-code = dc.by-code.
END.
IF create-shipto THEN
DO:
  /* on a rerun this would be true */
  IF edshipto.ship-to <> ""
    AND edshipto.ship-to <> ?
    THEN
  ws_char = edshipto.ship-to.
  ELSE
  DO:
    IF edmast.ship-to-mask <> 0 THEN
    DO:
      IF {rc/isdigit.i substring(edshipto.by-code,1,1) }
        THEN
      DO:
        ws_int = integer(edshipto.by-code) + edmast.ship-to-mask.
        ws_char = string(ws_int).
      END.
      ELSE
      DO:
        ws_char = edshipto.by-code.
        RUN rc/str2int.p (edshipto.by-code, OUTPUT ws_int).
      END.
    END.
    ELSE
    DO:
      ws_char = edshipto.by-code.
      RUN rc/str2int.p (edshipto.by-code, OUTPUT ws_int).
    END.
  END.
  FIND FIRST shipto
    WHERE shipto.company = cust.company
    AND shipto.cust-no = cust.cust-no
    AND shipto.ship-id = ws_char
    NO-ERROR.
  IF NOT AVAIL shipto THEN
  DO:
    CREATE shipto.
    {rc/incr.i ws_recs_added}.
  END.
  ASSIGN
    shipto.ship-name    = cust.name
    shipto.ship-addr[1] = cust.addr[1]
    shipto.ship-addr[2] = cust.addr[2]
    shipto.ship-city    = cust.city
    shipto.ship-state   = cust.state
    shipto.ship-zip     = cust.zip
    shipto.loc          =
    IF NUM-ENTRIES(edshipto.cust-region) = 3
    THEN ENTRY(2, edshipto.cust-region)
    ELSE cust.loc
    shipto.carrier      =
    IF NUM-ENTRIES(edshipto.cust-region) = 3
    THEN ENTRY(3, edshipto.cust-region)
    ELSE cust.carrier
    shipto.notes[1]     = cust.notes[1]
    shipto.notes[2]     = cust.notes[2]
    shipto.notes[3]     = cust.notes[3]
    shipto.notes[4]     = cust.notes[4]
    shipto.Postal       = cust.Postal
    shipto.dest-code    =
    IF edshipto.dest-zone > "" THEN edshipto.dest-zone
    ELSE cust.del-zone
    shipto.country      = cust.country
    shipto.ship-id      = ws_char
    shipto.tax-code     =
    IF NUM-ENTRIES(edshipto.cust-region) = 3
    THEN ENTRY(1,edshipto.cust-region)
    ELSE cust.tax-gr
    edshipto.cust       = edmast.cust
    edshipto.ship-to    = ws_char
    {rc/incr.i ws_recs_changed}
    .
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
END. /* ship-to's */
IF create-soldto THEN
DO:
  IF edshipto.cust-region = "DELETED"
    AND edshipto.ship-to <> ?
    AND edshipto.cust = edmast.cust
    THEN
  DO:
    FIND FIRST soldto
      WHERE soldto.company = cust.company
      AND soldto.cust-no = cust.cust-no
      AND soldto.sold-id = edshipto.ship-to
      EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL soldto THEN
    DO:
      /* delete logic still has to be implemented
      e.g. no open orders, etc.
      */
    END.
  END.
  IF edmast.ship-to-mask <> 0 THEN
  DO:
    IF {rc/isdigit.i substring(edshipto.by-code,1,1) }
      THEN
    DO:
      ws_int = integer(edshipto.by-code) + edmast.ship-to-mask.
      ws_char = string(ws_int).
    END.
    ELSE
    DO:
      ws_char = edshipto.by-code.
      RUN rc/str2int.p (edshipto.by-code, OUTPUT ws_int).
    END.
  END.
  ELSE
  DO:
    ws_char = edshipto.by-code.
    RUN rc/str2int.p (edshipto.by-code, OUTPUT ws_int).
  END.
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
      soldto.sold-no   = i
      soldto.sold-id   = ws_char
      soldto.sold-name = cust.name
      {rc/incr.i ws_recs_added}.
  END.
  /* override defaults with edshipto fields where applicable */
  IF edshipto.city > "" AND edshipto.state > "" AND edshipto.zip > ""
    THEN
  ASSIGN
    soldto.sold-name    =
    IF edshipto.name > ""
    THEN edshipto.name
    ELSE soldto.sold-name
    soldto.sold-addr[1] = edshipto.addr1
    soldto.sold-addr[2] = edshipto.addr2
    soldto.sold-city    = edshipto.city
    soldto.sold-state   = edshipto.state
    soldto.sold-zip     = edshipto.zip
    soldto.country      = edshipto.country
    .
  {rc/incr.i ws_recs_changed}.
  IF edshipto.name > "" THEN
  ASSIGN soldto.sold-name = edshipto.name.
  IF edshipto.ship-to = ""  OR edshipto.ship-to = ?
    THEN
  ASSIGN edshipto.ship-to = soldto.sold-id.
END.    /* create sold to's */
