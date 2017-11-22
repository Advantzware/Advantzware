/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\asi\unp
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
def var ws_cust-po like oe-ord.po-no no-undo label "Customer PO#".
{rc/viewline.i &displayf="ws_partner ws_date ws_cust-po"}
UPDATE ws_partner ws_date LABEL "Orders Created Since"
    ws_cust-po help "Enter specific customer PO# or blank for any"
  WITH FRAME f-view.
MESSAGE color value(c_wrn) "Continue with order deletion?".
{rc/confirm.i}
{rc/fcurrent.i}
IF NOT confirm THEN
RETURN.
view frame f-current.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.partner = ws_partner
    AND eddoc.adddate >= ws_date
    and eddoc.setid = "850"
    and (if ws_cust-po = "" then true else eddoc.docid = ws_custpo)
    AND eddoc.posted = TRUE:
  DISPLAY eddoc.partner eddoc.adddate eddoc.status-flag eddoc.unique-order-no
    eddoc.userref format 'x(12)' WITH FRAME f-current.
  FIND oe-ord
    WHERE oe-ord.company = "RPI"
    AND oe-ord.ord-no = eddoc.unique-order-no EXCLUSIVE.
  FOR EACH oe-ordl
      WHERE oe-ordl.company = oe-ord.company
      AND oe-ordl.ord-no = oe-ord.ord-no:
    IF oe-ordl.qty <> 0 THEN
    DO:
      FIND itemfg OF oe-ordl EXCLUSIVE.
      ASSIGN itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
    END.
    DELETE oe-ordl validate(TRUE,"").
  END.
  FOR EACH oe-rel
      WHERE oe-rel.company = oe-ord.company
      AND oe-rel.ord-no = oe-ord.ord-no:
    DELETE oe-rel validate(TRUE,"").
  END.
  DELETE oe-ord validate(TRUE,"").
  eddoc.posted = FALSE.
END.
