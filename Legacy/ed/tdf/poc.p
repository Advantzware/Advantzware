/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\POC.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "POC" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
    CASE ws_version:
    when "4010" then do:
  ASSIGN
    {rc/substr.i        customer_line_number    18   20 INTEGER}
    {rc/substr.i        change_indicator        38   02}
    {ed/tdf/substrde.i  quantity_cumulative     40   16 0}
    {ed/tdf/substrde.i  order_change_quantity   56   10 0}
    {rc/substr.i        unit_of_measure         66   02}
    {ed/tdf/substrde.i  unit_price              212  18 4}
    {rc/substr.i        price_basis             229  02}
    {rc/substr.i        item_product_qualifier  231  02}
    {rc/substr.i        product_id              233  48}
    .
    
    end.
    otherwise do:
  ASSIGN
    {rc/substr.i        customer_line_number    18  20 INTEGER}
    {rc/substr.i        change_indicator        38  02}
    {ed/tdf/substrde.i  quantity_cumulative     40  10 0}
    {ed/tdf/substrde.i  order_change_quantity   50  10 0}
    {rc/substr.i        unit_of_measure         60  02}
    /*
    {ed/tdf/substrde.i  unit_price              50  18 4}
    {rc/substr.i        price_basis             68  02}
    */
    {rc/substr.i        item_product_qualifier  225  02}
    {rc/substr.i        product_id              227  40}
    .
  end.    
  end case.
  CASE item_product_qualifier:
  WHEN "CB" or when "IN" THEN
  customer_item_number   = product_id.
  WHEN "VC" or when "VN" or when "VA" THEN
  vendor_item_number     = product_id.
  WHEN "UP" THEN
  upc_code               = product_id.
  WHEN "IZ" or when "SM" or when "SZ" THEN
  item_size    = product_id.
  WHEN "EN" THEN
  ean_code               = product_id.
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
    + " product_id " + product_id).
END CASE.
END.    /* I */
IF command matches "*O*" THEN
DO:
  CASE item_product_qualifier:
  WHEN "CB" or when "IN" THEN
  customer_item_number   = product_id.
  WHEN "VC" or when "VN" or when "VA" THEN
  vendor_item_number     = product_id.
  WHEN "UP" THEN
  upc_code               = product_id.
  WHEN "IZ" THEN
  item_size              = product_id.
  WHEN "EN" THEN
  ean_code               = product_id.
  OTHERWISE RUN rc/debugmsg.p
    ("Unrecognized item_product_qualifier in P01: " + item_product_qualifier
    + " product_id " + product_id).
END CASE.
CASE ws_version:
    when "4010" then do:
  ASSIGN
    {rc/outstr.i        string(customer_line_number) 18  20}
    {rc/outstr.i        change_indicator         38  02}
    {ed/tdf/outstrde.i  quantity_cumulative      40  16 0}
    {ed/tdf/outstrde.i  order_change_quantity    56  10 0}
    {rc/outstr.i        unit_of_measure          66  02}
    {ed/tdf/outstrde.i  unit_price              212  18 4}
    {rc/outstr.i        price_basis             229  02}
    {rc/outstr.i        item_product_qualifier  231  02}
    {rc/outstr.i        product_id              233  48}
    .
    
    end.
    otherwise do:
  ASSIGN
    {rc/outstr.i        string(customer_line_number) 18  20}
    {rc/outstr.i        change_indicator        38  02}
    {ed/tdf/outstrde.i  quantity_cumulative     40  10 0}
    {ed/tdf/outstrde.i  order_change_quantity   50  10 0}
    {rc/outstr.i        unit_of_measure         60  02}
    /*
    {ed/tdf/outstrde.i  unit_price              50  18 4}
    {rc/outstr.i        price_basis             68  02}
    */
    {rc/outstr.i        item_product_qualifier  225  02}
    {rc/outstr.i        product_id              227  40}
    .
  end.    
  end case.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    customer_line_number        LABEL "PO Line#"    FORMAT ">>>9"
    change_indicator            label "Chg Code"
    quantity_cumulative         LABEL "Ordered"     FORMAT "->>>,>>>,>>>.99<<"
    order_change_quantity       label "Qty Chg"     FORMAT "->>>,>>>,>>>.99<<"
    unit_of_measure             LABEL "UOM"
    unit_price                  label "Price"       format "->>>>>.99<<"
    price_basis                 label "Per"
    customer_item_number        LABEL "Cust-Item"       FORMAT 'x(15)'
    skip space(9)
    vendor_item_number          LABEL "Vendor-Item"     FORMAT 'x(15)'
    upc_code                    LABEL "UPC"             FORMAT 'x(15)'
    ean_code                    LABEL "EAN"             FORMAT 'x(15)'
    customer_sku_number         LABEL "SKU"             FORMAT 'x(15)'
    WITH side-labels width 144 no-box.
END.
