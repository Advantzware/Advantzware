/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\cdd.p
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
DEF var cdd01   AS char     NO-UNDO FORMAT "x(02)"  LABEL "Adj Reason".
DEF var cdd02   AS char     NO-UNDO FORMAT "x(01)"  LABEL "DR/CR".
DEF var cdd03   AS char     NO-UNDO FORMAT "x(20)"  LABEL "Adj#".
DEF var cdd04   AS decimal  NO-UNDO FORMAT ">>>,>>>,>>>.99DR"  LABEL "Amount".
DEF var cdd05   AS char     NO-UNDO FORMAT "x(01)"  LABEL "Y/N".
DEF var cdd06   AS char     NO-UNDO FORMAT "x(03)"  LABEL "Price Bracket".
DEF var cdd07   AS decimal  NO-UNDO FORMAT "->>>,>>>,>>>.9999" LABEL "Quantity".
DEF var cdd08   AS char     NO-UNDO FORMAT "x(02)"  LABEL "UOM".
DEF var cdd09   AS decimal  NO-UNDO FORMAT "->>>,>>>,>>>.9999" LABEL "Price Diff".
DEF var cdd10   AS char     NO-UNDO FORMAT "x(03)"  LABEL "Price ID Code".
DEF var cdd11   AS decimal  NO-UNDO FORMAT "->>>,>>>,>>>.9999" LABEL "Unit Price".
DEF var cdd12   AS char     NO-UNDO FORMAT "x(03)"  LABEL "Price ID Code".
DEF var cdd13   AS decimal  NO-UNDO FORMAT "->>>,>>>,>>>.9999" LABEL "Unit Price".
IF ws_segment <> "cdd" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i cdd01              18  02}
      {rc/substr.i cdd02              20  01}
      {rc/substr.i cdd03              21  20}
      {ed/tdf/substrde.i cdd04        41  16}
      {rc/substr.i cdd05              57  01}
      {rc/substr.i cdd06              58  03}
      {ed/tdf/substrde.i cdd07        61  11}
      {rc/substr.i cdd08              72  02}
      {ed/tdf/substrde.i cdd09        74  16}
      {rc/substr.i cdd10              90  03}
      {ed/tdf/substrde.i cdd11        93  18}
      {rc/substr.i cdd12             111  03}
      {ed/tdf/substrde.i cdd13       114  18}
      .
  END.
END CASE.
IF cdd02 = "C" THEN
ASSIGN cdd04 = -1.00 * cdd04.
total_amount1 = cdd04.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
END.    /* O */
IF command matches "*P*" THEN
DO:
  IF cdd04 < 0 THEN
  ASSIGN cdd02 = "C" cdd04 = -1.00 * cdd04.
  ELSE
  cdd02 = "D".
  CASE ws_version:
  OTHERWISE
  DO:
    ASSIGN
      {rc/outstr.i cdd01              18  02}
      {rc/outstr.i cdd02              20  01}
      {rc/outstr.i cdd03              21  20}
      {ed/tdf/outstrde.i cdd04        41  16 2}
      {rc/outstr.i cdd05              57  01}
      {rc/outstr.i cdd06              58  03}
      {ed/tdf/outstrde.i cdd07        61  11 4}
      {rc/outstr.i cdd08              72  02}
      {ed/tdf/outstrde.i cdd09        74  16 4}
      {rc/outstr.i cdd10              90  03}
      {ed/tdf/outstrde.i cdd11        93  18 4}
      {rc/outstr.i cdd12             111  03}
      {ed/tdf/outstrde.i cdd13       114  18 4}
      .
  END.
END CASE.
DISPLAY STREAM s-out
  ws_segment
  cdd01
  cdd02
  cdd03
  cdd04
  cdd05
  /* cdd06 price bracket */
  cdd07
  skip space(9)
  cdd08
  cdd09
  cdd10
  cdd11
  cdd12
  cdd13
  WITH no-box side-labels width 144.
END.
