/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\PRF.P
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
{rc/datev.i}
IF ws_segment <> "PRF" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i purchase_order_number          18  22}
      {rc/substr.i release_number                 40  30}
      {rc/substr.i change_order_seq_number        70  08}
      {rc/substr.i purchase_order_date            78  06}
      {rc/substr.i item_assigned_id               84  11}
      {rc/substr.i contract_number                95  30}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i purchase_order_number          18  22}
      {rc/substr.i release_number                 40  30}
      {rc/substr.i change_order_seq_number        70  08}
      {rc/substr.i purchase_order_date            78  06}
      {rc/substr.i item_assigned_id               84  20}
      {rc/substr.i contract_number               104  30}
      {rc/substr.i transaction_type_code         134  02}
      .
  END.
END CASE.
{rc/xyymmdd.i purchase_order_date purchase_order_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   purchase_order_number = "" THEN
  RETURN error.
  ASSIGN
    purchase_order_date     = {rc/dt2ymd.i purchase_order_date#}
    purchase_order_date     =
    IF purchase_order_date# = ? THEN "" ELSE
    {rc/dt2ymd.i purchase_order_date#}
    .
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/outstr.i purchase_order_number          18  22}
      {rc/outstr.i release_number                 40  30}
      {rc/outstr.i change_order_seq_number        70  08}
      {rc/outstr.i purchase_order_date            78  06}
      {rc/outstr.i item_assigned_id               84  11}
      {rc/outstr.i contract_number                95  30}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i purchase_order_number          18  22}
      {rc/outstr.i release_number                 40  30}
      {rc/outstr.i change_order_seq_number        70  08}
      {rc/outstr.i purchase_order_date            78  06}
      {rc/outstr.i item_assigned_id               84  20}
      {rc/outstr.i contract_number               104  30}
      {rc/outstr.i transaction_type_code         134  02}
      .
  END.
END CASE.
END.    /* O */
