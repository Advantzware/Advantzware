/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\bcd.p
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
DEF SHARED STREAM s-out.
DEF var bcd01   AS char     NO-UNDO FORMAT "x(08)"  LABEL "Memo Date".
DEF var bcd02   AS char     NO-UNDO FORMAT "x(22)"  LABEL "Adj#".
DEF var bcd03   AS char     NO-UNDO FORMAT "x(02)"  LABEL "Handling".
DEF var bcd03d  AS char     NO-UNDO FORMAT "x(40)".
DEF var bcd04   AS decimal  NO-UNDO FORMAT ">>>,>>>,>>>.99DR"  LABEL "Amount".
DEF var bcd05   AS char     NO-UNDO FORMAT "x(01)"  LABEL "DR/CR".
DEF var bcd06   AS char     NO-UNDO FORMAT "x(08)"  LABEL "Invoice Date".
DEF var bcd07   AS char     NO-UNDO FORMAT "x(22)"  LABEL "Invoice#".
DEF var bcd08   AS char     NO-UNDO FORMAT "x(22)"  LABEL "Order#".
DEF var bcd09   AS char     NO-UNDO FORMAT "x(08)"  LABEL "PO Date".
DEF var bcd10   AS char     NO-UNDO FORMAT "x(22)"  LABEL "PO#".
DEF var bcd11   AS char     NO-UNDO FORMAT "x(02)"  LABEL "Purpose".
DEF var bcd12   AS char     NO-UNDO FORMAT "x(02)"  LABEL "Type".
DEF var bcd13   AS char     NO-UNDO FORMAT "x(03)"  LABEL "Ref Qual".
DEF var bcd14   AS char     NO-UNDO FORMAT "x(30)"  LABEL "Ref#".
DEF var bcd15   AS char     NO-UNDO FORMAT "x(02)"  LABEL "Action".
IF ws_segment <> "BCD" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i    bcd01       18  08}
      {rc/substr.i    bcd02       26  22}
      {rc/substr.i    bcd03       48  02}
      {ed/tdf/substrde.i bcd04    50  16}
      {rc/substr.i    bcd05       66  01}
      {rc/substr.i    bcd06       67  08}
      {rc/substr.i    bcd07       75  22}
      {rc/substr.i    bcd08       97  22}
      {rc/substr.i    bcd09      119  08}
      {rc/substr.i    bcd10      127  22}
      {rc/substr.i    bcd11      149  02}
      {rc/substr.i    bcd12      151  02}
      {rc/substr.i    bcd13      153  03}
      {rc/substr.i    bcd14      156  30}
      {rc/substr.i    bcd15      186  02}
      .
  END.
END CASE.
IF bcd05 = "C" THEN
bcd04 = -1.00 * bcd04.
{rc/xyymmdd.i bcd01 extra_date#}
{rc/xyymmdd.i bcd06 invoice_date#}
{rc/xyymmdd.i bcd09 purchase_order_date#}
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  RUN rc/dt2char.p (extra_date#, "cymd", OUTPUT bcd01).
  RUN rc/dt2char.p (invoice_date#, "cymd", OUTPUT bcd06).
  RUN rc/dt2char.p (purchase_order_date#, "cymd", OUTPUT bcd09).
  IF bcd04 < 0 THEN
  ASSIGN
    bcd05 = "C"
    bcd04 = -1.00 * bcd04.
  ELSE
  bcd05 = "D".
  CASE ws_version:
  OTHERWISE
  DO:
    ASSIGN
      {rc/outstr.i    bcd01       18  08}
      {rc/outstr.i    bcd02       26  22}
      {rc/outstr.i    bcd03       48  02}
      {ed/tdf/outstrde.i bcd04    50  16 2}
      {rc/outstr.i    bcd05       66  01}
      {rc/outstr.i    bcd06       67  08}
      {rc/outstr.i    bcd07       75  22}
      {rc/outstr.i    bcd08       97  22}
      {rc/outstr.i    bcd09      119  08}
      {rc/outstr.i    bcd10      127  22}
      {rc/outstr.i    bcd11      149  02}
      {rc/outstr.i    bcd12      151  02}
      {rc/outstr.i    bcd13      153  03}
      {rc/outstr.i    bcd14      156  30}
      {rc/outstr.i    bcd15      186  02}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  CASE bcd03:
  WHEN "A" THEN
  bcd03d = "OFF INVOICE (DEDUCTION FROM ORIGINAL INVOICE)".
  WHEN "O" THEN
  bcd03d = "DEDUCT FROM NEXT REMITTANCE".
  WHEN "Q" THEN
  bcd03d = "CREDIT DUE, PAYMENT BEING ISSUED".
END CASE.
DISPLAY STREAM s-out
  ws_segment
  extra_date# LABEL "Memo Date"
  bcd02
  bcd03
  bcd03d LABEL "-"
  bcd04
  skip space(9)
  bcd11
  bcd12
  bcd15
  invoice_date#     label "Invoice Date"
  bcd07
  bcd08
  SKIP
  SPACE(9)
  purchase_order_date#  label "PO Date"
  bcd10
  bcd14   SPACE(0) "/" SPACE(0) bcd13 NO-LABEL
  WITH no-box side-labels width 144.
END.
