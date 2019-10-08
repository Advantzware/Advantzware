/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\lin.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
/* variables for unrolling arrays */
DEF var group_offset AS int NO-UNDO.
DEF var group_extent AS int NO-UNDO.
DEF var group_size AS int NO-UNDO.
DEF var n AS int NO-UNDO.
DEF var x1 AS int NO-UNDO.
DEF var s1 AS int NO-UNDO.
DEF var x2 AS int NO-UNDO.
DEF var s2 AS int NO-UNDO.
IF ws_segment <> "LIN" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i  item_assigned_id          18  20}
      .
    ASSIGN
      group_offset = 38
      group_extent = 15
      s1 = 2
      s2 = 48
      group_size = s1 + s2.
  END.   /* 4010 */
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i  item_assigned_id          18  20}
      .
    ASSIGN
      group_offset = 38
      group_extent = 14
      s1 = 2
      s2 = 40
      group_size = s1 + s2.
  END.  /* 3060 */
END CASE /* version */.
_group:
DO n = 1 TO group_extent:
  x1 = group_offset + ((n - 1) * group_size).
  x2 = x1 + s1.
  ASSIGN
    {rc/substr.i item_product_qualifier    x1 s1}
    {rc/substr.i product_id                x2 s2}
    .
  IF item_product_qualifier = "" THEN
  LEAVE _group.
  RUN ixref_qual.ip (item_product_qualifier).
END.  /* do loop */
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE item_product_qualifier:
  WHEN "CB" THEN
  product_id = customer_item_number.
  WHEN "VC" THEN
  product_id = vendor_item_number.
  WHEN "UP" THEN
  product_id = upc_code.
  WHEN "IZ" THEN
  product_id = customer_sku_number.
  WHEN "EN" THEN
  product_id = ean_code.
  when "IN" then product_id = customer_item_number.
  when "PN" then product_id = customer_item_number.
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized item_product_qualifier: " + item_product_qualifier
    + " product_id " + product_id).
END CASE.
CASE ws_version:
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i item_assigned_id          18  20}
    {rc/outstr.i item_product_qualifier    29  02}
    {rc/outstr.i product_id                31  30}
    .
END.
WHEN "3060" THEN
DO:
  ASSIGN
    {rc/outstr.i item_assigned_id          18  20}
    {rc/outstr.i item_product_qualifier    38  02}
    {rc/outstr.i product_id                40  40}
    .
END.
WHEN "4010" THEN
DO:
  ASSIGN
    {rc/outstr.i item_assigned_id          18  20}
    {rc/outstr.i item_product_qualifier    38  02}
    {rc/outstr.i product_id                40  48}
    .
END.
END CASE /* version */.
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
        ws_segment
        item_assigned_id    label "Seq"
        product_id label "Product ID" space(0) "/" space(0) item_product_qualifier no-label
    with side-labels no-box width 144.    
end.
procedure ixref_qual.ip:
DEF INPUT PARAM pqual AS char NO-UNDO.
CASE pqual:
WHEN "CB" THEN
customer_item_number   = product_id.
WHEN "VC" THEN
vendor_item_number     = product_id.
WHEN "UP" THEN
upc_code               = product_id.
WHEN "IZ" THEN
customer_sku_number    = product_id.
WHEN "EN" THEN
ean_code               = product_id.
WHEN "IN" THEN customer_item_number = product_id.
when "PN" then customer_item_number = product_id.
OTHERWISE RUN rc/debugmsg.p
  ("Unrecognized item_product_qualifier: " + pqual
  + " product_id " + product_id).
END CASE.
END procedure.
