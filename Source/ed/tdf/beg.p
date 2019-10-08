/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\big.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
IF ws_segment <> "BEG" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    transaction_purpose_code    18 02}
      {rc/substr.i    purchase_order_type         20 02}
      {rc/substr.i    purchase_order_number       22 22}
      {rc/substr.i    release_number              44 30}
      {rc/substr.i    purchase_order_date         74 08}    /* ccyymmdd */
      {rc/substr.i    contract_number             82 30}
      {rc/substr.i    misc_elem[7]               112 02}    /* ack type */
      {rc/substr.i    misc_elem[8]               114 03}    /* invoice type */
      {rc/substr.i    misc_elem[9]               117 02}    /* contract type */
      {rc/substr.i    misc_elem[10]              119 02}    /* purchase category */
      {rc/substr.i    misc_elem[11]              121 02}    /* security level */
      {rc/substr.i    misc_elem[12]              123 02}    /* transaction type */
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i    transaction_purpose_code    18 2}
      {rc/substr.i    purchase_order_type         20 2}
      {rc/substr.i    purchase_order_number       22 22}
      {rc/substr.i    release_number              44 30}
      {rc/substr.i    purchase_order_date         74 6} /*date as char */
      {rc/substr.i    contract_number             80 30}
      .
  END.
END CASE.
{rc/xyymmdd.i purchase_order_date purchase_order_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   purchase_order_date# = ?
    OR   purchase_order_number = ""
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (purchase_order_date or purchase_order_number)" ).
    RETURN error.
  END.
  ASSIGN
    purchase_order_date            = {rc/dt2ymd.i purchase_order_date#}
    purchase_order_date     =
    IF purchase_order_date# = ? THEN "" ELSE
    {rc/dt2ymd.i purchase_order_date#}
    .
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    transaction_purpose_code    18 02}
      {rc/outstr.i    purchase_order_type         20 02}
      {rc/outstr.i    purchase_order_number       22 22}
      {rc/outstr.i    release_number              44 30}
      {rc/outstr.i    purchase_order_date         74 08}    /* ccyymmdd */
      {rc/outstr.i    contract_number             82 30}
      {rc/outstr.i    misc_elem[7]               112 02}    /* ack type */
      {rc/outstr.i    misc_elem[8]               114 03}    /* invoice type */
      {rc/outstr.i    misc_elem[9]               117 02}    /* contract type */
      {rc/outstr.i    misc_elem[10]              119 02}    /* purchase category */
      {rc/outstr.i    misc_elem[11]              121 02}    /* security level */
      {rc/outstr.i    misc_elem[12]              123 02}    /* transaction type */
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i    transaction_purpose_code    18 2}
      {rc/outstr.i    purchase_order_type         20 2}
      {rc/outstr.i    purchase_order_number       22 22}
      {rc/outstr.i    release_number              44 30}
      {rc/outstr.i    purchase_order_date         74 6} /*date as char */
      {rc/outstr.i    contract_number             80 30}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    transaction_purpose_code    LABEL "Purpose"
    purchase_order_type         LABEL "Type"
    purchase_order_number       LABEL "PO#"
    release_number              LABEL "Rel#"        FORMAT "x(10)"
    purchase_order_date#        LABEL "Date"
    contract_number             LABEL "Contract#"   FORMAT "x(20)"
    WITH side-labels width 144 no-box.
END.
