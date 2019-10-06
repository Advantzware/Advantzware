/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\ASI\IMPSHIP.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
{rc/stats.i}
{rc/viewline.i &displayf="ws_partner cust.cust-no cust.name no-label"}
{ed/getpart.i}
{rc/fcurrent.i}
DEF VAR i AS INT NO-UNDO.
FIND cust WHERE cust.company = ws_company
  AND cust.cust-no = edmast.cust NO-LOCK.
DISPLAY cust.cust-no cust.name WITH FRAME f-view.
{rc/confirm.i}
IF NOT confirm THEN
RETURN.
start = TIME.
VIEW FRAME f-current.
VIEW FRAME f-stats.
FOR EACH edshipto OF edmast exclusive:
  ASSIGN
    {rc/incr.i ws_recs_read}
    {rc/incr.i ws_recs_selected}
    .
  FIND FIRST shipto
    WHERE shipto.company = cust.company
    AND shipto.cust-no = cust.cust-no
    AND shipto.ship-id = edshipto.by-code
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
      shipto.ship-id      = edshipto.by-code
      edshipto.cust = shipto.ship-id
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
  edshipto.cust = shipto.ship-id.
  {rc/incr.i ws_recs_changed}.
  DISPLAY
    edshipto.ref-type edshipto.by-code edshipto.st-code
    shipto.ship-id shipto.ship-no FORMAT ">>>>9"
    WITH FRAME f-current.
  IF ws_recs_read MOD 10 = 0 THEN
  DO:
    {rc/statsdis.i}
  END.
END.
{rc/statsdis.i}
HIDE FRAME f-current.
HIDE FRAME f-stats.
/* ------------------------------------------------ sys/ref/shipto.a 2/92 cd  */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */
FIND LAST shipto WHERE shipto.company = cust.company AND
  shipto.cust-no = cust.cust-no
  USE-INDEX ship-no NO-LOCK NO-ERROR.
IF AVAIL shipto THEN
ASSIGN i = shipto.ship-no + 1.
ELSE
ASSIGN i = 1.
CREATE shipto.
ASSIGN
  shipto.company   = cust.company
  shipto.cust-no   = cust.cust-no
  shipto.ship-no   = i
  shipto.ship-id   = TRIM(STRING(i, ">>>>>>>9"))
  shipto.ship-name = cust.name
  shipto.tax-code  = cust.tax-gr
  shipto.dest-code = cust.del-zone
  shipto.loc       = cust.loc
  shipto.carrier   = cust.carrier.
/* end ---------------------------------- copr. 1992  advanced software, inc. */
