/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\asi\fix
**       By: Chris Heins
** Descript: Strip prefixes from shiptos make same as customer codes.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
{rc/fcurrent.i}
{ed/getpart.i}
MESSAGE "Global Change Shipto mapping?".
{rc/confirm.i}
IF NOT confirm THEN
RETURN.
FOR EACH edshipto EXCLUSIVE
    WHERE edshipto.partner = ws_partner:
  DISPLAY edshipto.partner
    edshipto.ref-type
    edshipto.by-code
    edshipto.cust
    edshipto.ship-to
    WITH FRAME f-current.
  IF edshipto.cust <> edmast.cust AND edshipto.ship-to <> ?
    THEN
  ASSIGN edshipto.cust = edmast.cust.
  FIND FIRST shipto
    WHERE shipto.company = ws_company
    AND shipto.cust-no = edshipto.cust
    AND shipto.ship-id = edshipto.ship-to EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL shipto THEN
  DO:
    ASSIGN shipto.ship-id = edshipto.by-code.
  END.
  FIND FIRST soldto
    WHERE soldto.company = ws_company
    AND soldto.cust-no = edshipto.cust
    AND soldto.sold-id = edshipto.ship-to EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL soldto THEN
  DO:
    ASSIGN soldto.sold-id = edshipto.by-code.
  END.
  ASSIGN edshipto.comments[2] = edshipto.ship-to
  edshipto.ship-to = edshipto.by-code.
END.
