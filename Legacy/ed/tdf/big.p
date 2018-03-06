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
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
IF ws_segment <> "BIG" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i invoice_date                   18  06}
    {rc/substr.i invoice_number                 24  22}
    {rc/substr.i purchase_order_date            46  06}
    {rc/substr.i purchase_order_number          52  22}
    {rc/substr.i release_number                 74  30}
    {rc/substr.i change_order_seq_number       104  08}
    {rc/substr.i transaction_type_code         112  02}
    .
  CASE ws_version:
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i transaction_purpose_code      114  02}
      {rc/substr.i action_code                   116  02}
      {rc/substr.i alt_invoice_number            118  22}
      .
  END.
END CASE.
{rc/xyymmdd.i invoice_date invoice_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   invoice_date# = ?
    OR invoice_number = ""
    OR invoice_number = ?
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (invoice_date or invoice_number)" ).
    RETURN error.
  END.
  ASSIGN
    invoice_date            = {rc/dt2ymd.i invoice_date#}
    purchase_order_date     =
    IF purchase_order_date# = ? THEN "" ELSE
    {rc/dt2ymd.i purchase_order_date#}
    .
  ASSIGN
    {rc/outstr.i invoice_date                   18  06}
    {rc/outstr.i invoice_number                 24  22}
    {rc/outstr.i purchase_order_date            46  06}
    {rc/outstr.i purchase_order_number          52  22}
    {rc/outstr.i release_number                 74  30}
    {rc/outstr.i change_order_seq_number       104  08}
    {rc/outstr.i transaction_type_code         112  02}
    .
  CASE ws_version:
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/outstr.i transaction_purpose_code      114  02}
      {rc/outstr.i action_code                   116  02}
      {rc/outstr.i alt_invoice_number            118  22}
      .
  END.
END CASE.
END.    /* O */
