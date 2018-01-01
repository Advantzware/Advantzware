/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ctp.p
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
IF ws_segment <> "CTP" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i    class_of_trade_code         18  02}
      {rc/substr.i    price_id_code               20  03}
      {ed/tdf/substrde.i  unit_price              23  15  4}
      .
  END.
  OTHERWISE /* 3060 */
    DO:
    ASSIGN
      {rc/substr.i    class_of_trade_code         18  02}
      {rc/substr.i    price_id_code               20  03}
      {ed/tdf/substrde.i  unit_price              23  18  4}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF (class_of_trade_code = "" AND price_id_code = "")
    OR unit_price = 0
    THEN
  RETURN error.
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/outstr.i    class_of_trade_code         18  02}
      {rc/outstr.i    price_id_code               20  03}
      {ed/tdf/outstrde.i  unit_price              23  15  4}
      .
  END.
  OTHERWISE /* 3060 */
    DO:
    ASSIGN
      {rc/outstr.i    class_of_trade_code         18  02}
      {rc/outstr.i    price_id_code               20  03}
      {ed/tdf/outstrde.i  unit_price              23  18  4}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    class_of_trade_code
    price_id_code
    unit_price
    WITH side-labels width 144 no-box.
END.
