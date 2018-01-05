/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\tds.p
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
IF ws_segment <> "TDS" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {ed/tdf/substrde.i total_amount1  18  11  2}
      {ed/tdf/substrde.i total_amount2  29  11  2}
      {ed/tdf/substrde.i total_amount3  40  11  2}
      {ed/tdf/substrde.i total_amount4  51  11  2}
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {ed/tdf/substrde.i total_amount1  18  16  2}
      {ed/tdf/substrde.i total_amount2  34  16  2}
      {ed/tdf/substrde.i total_amount3  50  16  2}
      {ed/tdf/substrde.i total_amount4  66  16  2}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {ed/tdf/outstrde.i total_amount1  18  11  2}
      {ed/tdf/outstrde.i total_amount2  29  11  2}
      {ed/tdf/outstrde.i total_amount3  40  11  2}
      {ed/tdf/outstrde.i total_amount4  51  11  2}
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {ed/tdf/outstrde.i total_amount1  18  16  2}
      {ed/tdf/outstrde.i total_amount2  34  16  2}
      {ed/tdf/outstrde.i total_amount3  50  16  2}
      {ed/tdf/outstrde.i total_amount4  66  16  2}
      .
  END.
END CASE.
END.    /* O */
