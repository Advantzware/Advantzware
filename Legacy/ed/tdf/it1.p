/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\it1.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
DEF var ws_int AS int NO-UNDO.
/* variables for unrolling arrays */
DEF var group_offset AS int NO-UNDO.
DEF var group_extent AS int NO-UNDO.
DEF var group_size AS int NO-UNDO.
DEF var n AS int NO-UNDO.
DEF var x1 AS int NO-UNDO.
DEF var s1 AS int NO-UNDO.
DEF var x2 AS int NO-UNDO.
DEF var s2 AS int NO-UNDO.


IF ws_segment <> "IT1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i    item_assigned_id        18  11}
      {ed/tdf/substrde.i  quantity_invoiced   29  11  0}
      {rc/substr.i    unit_of_measure         40  02}
      {ed/tdf/substrde.i  unit_price          42  15  4}
      {rc/substr.i    item_product_qualifier  59  02}
      {rc/substr.i    product_id              61  30}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i    item_assigned_id        18  20}
      {ed/tdf/substrde.i  quantity_invoiced   38  11  0}
      {rc/substr.i    unit_of_measure         49  02}
      {ed/tdf/substrde.i  unit_price          51  18  4}
      .
    ASSIGN
      group_offset = 71
      group_extent = 10
      s1 = 2
      s2 = 40
      group_size = s1 + s2.
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
      CASE item_product_qualifier:
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
      OTHERWISE RUN rc/debugmsg.p
        ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
        + " product_id " + product_id).
    END CASE.
  END.  /* do loop */
END. /* 3060 */
END.  /* version  case */
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
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
    + " product_id " + product_id).
END CASE.
/* check mandatory assignments ... */
IF item_product_qualifier = ""
  THEN
DO:
  RUN rc/debugmsg.p
    ("Mandatory elements missing (item_product_qualifier)" ).
  RETURN error.
END.
CASE ws_version:
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i    item_assigned_id        18  11}
    {ed/tdf/outstrde.i  quantity_invoiced   29  11  0}
    {rc/outstr.i    unit_of_measure         40  02}
    {ed/tdf/outstrde.i  unit_price          42  15  4}
    {rc/outstr.i    item_product_qualifier  59  02}
    {rc/outstr.i    product_id              61  30}
    .
END.
OTHERWISE /*  "3060" */
  DO:
  ASSIGN
    {rc/outstr.i    item_assigned_id        18  20}
    {ed/tdf/outstrde.i  quantity_invoiced   38  11  0}
    {rc/outstr.i    unit_of_measure         49  02}
    {ed/tdf/outstrde.i  unit_price          51  18  6}
    {rc/outstr.i    item_product_qualifier  71  02}
    {rc/outstr.i    product_id              73  40}
    .
END.    /* 3060 */
END CASE.   /* on version */
END.    /* O */
