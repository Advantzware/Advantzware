/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ipo1.
**       By: Chris Heins, RCI
** Descript:
05.18.99 by CAH on \\ricky\robj8 Log#0000:
1.  Added VA as qualifier for vendor artical number, used by Federated
in 850/4010.
2.  Added IN as qualifier for buyers item#,     /* sears.850.4010 */
    Added VN as qualifier for vendors item#.    /* sears.850.4010 */
10.05.98 by CAH on \\ricky\rv8 Log#0000:
1.  Corrected length of price field, was 17 should be 18.
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
IF ws_segment <> "PO1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i  customer_line_number    18 11 INTEGER}
      {ed/tdf/substrde.i  quantity_ordered  29 10}
      {rc/substr.i  unit_of_measure         39 02}
      {ed/tdf/substrde.i  unit_price        41 15}
      {rc/substr.i  price_basis             56 02}
      {rc/substr.i  item_product_qualifier  58 02}
      {rc/substr.i  product_id              60 30}
      .
    RUN map_inbound_product_qualifier.ip.
  END    /* 3020 */.
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i  customer_line_number      18  20 INTEGER}
      {ed/tdf/substrde.i  quantity_ordered    38  10 0}
      {rc/substr.i  unit_of_measure           48  02}
      {ed/tdf/substrde.i  unit_price          50  18 4}
      {rc/substr.i  price_basis               68  02}
      .
    ASSIGN
      group_offset = 70
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
      RUN map_inbound_product_qualifier.ip.
    END.  /* do loop */
  END.   /* 3060 */
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i  customer_line_number      18  20 INTEGER}
      {ed/tdf/substrde.i  quantity_ordered    38  16 0}
      {rc/substr.i  unit_of_measure           54  02}
      {ed/tdf/substrde.i  unit_price          56  18 4}
      {rc/substr.i  price_basis               74  02}
      .
    ASSIGN
      group_offset = 76
      group_extent = 10
      s1 = 2
      s2 = 48
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
      RUN map_inbound_product_qualifier.ip.
    END.  /* do loop */
  END.   /* 4010 */
END CASE /* version */.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE item_product_qualifier:
  WHEN "CB" or when "IN" THEN
  product_id = customer_item_number.
  WHEN "VA" OR WHEN "VC" or when "VN" THEN
  product_id = vendor_item_number.
  WHEN "UP" THEN
  product_id = upc_code.
  WHEN "IZ" or when "SM" or when "SZ" THEN
  product_id = item_size.    /* 9905 CAH: Used to be customer_sku_number. */
  WHEN "EN" THEN
  product_id = ean_code.
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
    + " product_id " + product_id).
END CASE.
CASE ws_version:
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i  string(customer_line_number) 18 11}
    {ed/tdf/outstrde.i  quantity_ordered  29 10 0}
    {rc/outstr.i  unit_of_measure         39 02}
    {ed/tdf/outstrde.i  unit_price        41 15 4}
    {rc/outstr.i  price_basis             56 02}
    {rc/outstr.i  item_product_qualifier  58 02}
    {rc/outstr.i  product_id              60 30}
    .
END    /* 3020 */.
WHEN "3060" THEN
DO:
  ASSIGN
    {rc/outstr.i  string(customer_line_number) 18  20}
    {ed/tdf/outstrde.i  quantity_ordered    38  10 0}
    {rc/outstr.i  unit_of_measure           48  02}
    {ed/tdf/outstrde.i  unit_price          50  18 4}
    {rc/outstr.i  price_basis               68  02}
    {rc/outstr.i item_product_qualifier     70  2}
    {rc/outstr.i product_id                 72 40}
    .
END.   /* 3060 */
WHEN "4010" THEN
DO:
  ASSIGN
    {rc/outstr.i  string(customer_line_number) 18  20}
    {ed/tdf/outstrde.i  quantity_ordered    38  16 0}
    {rc/outstr.i  unit_of_measure           54  02}
    {ed/tdf/outstrde.i  unit_price          56  18 4}
    {rc/outstr.i  price_basis               74  02}
    {rc/outstr.i item_product_qualifier     76  2}
    {rc/outstr.i product_id                 78 48}
    .
END.   /* 4010 */
END CASE /* version */.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    customer_line_number        LABEL "PO Line#"    FORMAT ">>>9"
    quantity_ordered            LABEL "Ordered"     FORMAT ">>>>>>.99<<"
    unit_of_measure             LABEL "UOM"
    unit_price                  LABEL "Price"       FORMAT ">>>>>>.99<<"
    price_basis                 LABEL "Per"
    customer_item_number        LABEL "Cust-Item"       FORMAT 'x(15)'
    vendor_item_number          LABEL "Vendor-Item"     FORMAT 'x(15)'
    skip space(9)
    upc_code                    LABEL "UPC"             FORMAT 'x(15)'
    ean_code                    LABEL "EAN"             FORMAT 'x(15)'
    customer_sku_number         LABEL "SKU"             FORMAT 'x(15)'
    WITH side-labels width 144 no-box.
END.
procedure map_inbound_product_qualifier.ip:
CASE item_product_qualifier:
WHEN "CB" or when "IN" THEN
customer_item_number   = product_id.
WHEN "VA" OR WHEN "VC" or when "VN" THEN
vendor_item_number     = product_id.
WHEN "UP" THEN
upc_code               = product_id.
WHEN "IZ" or when "SM" or when "SZ" THEN
item_size   = product_id.   /* used to be customer_sku_number */
WHEN "EN" THEN
ean_code               = product_id.
OTHERWISE RUN rc/debugmsg.p
  ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
  + " product_id " + product_id).
END CASE.
END procedure.
