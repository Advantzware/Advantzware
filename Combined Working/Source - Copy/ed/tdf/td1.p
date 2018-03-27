/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\td1.p
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
IF ws_segment <> "TD1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i packaging_code             18  05}
      {ed/tdf/substrde.i lading_quantity      23  08   0}
      {rc/substr.i commodity_code_qual        31  01}
      {rc/substr.i commodity_code             32  16}
      {rc/substr.i lading_desc                48  50}
      {rc/substr.i weight_qual                98  02}
      {ed/tdf/substrde.i total_weight         100 09  0}
      {rc/substr.i weight_unit_measure        109 02}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i packaging_code             18  05}
      {ed/tdf/substrde.i lading_quantity      23  08   0}
      {rc/substr.i commodity_code_qual        31  01}
      {rc/substr.i commodity_code             32  30}
      {rc/substr.i lading_desc                62  50}
      {rc/substr.i weight_qual                112 02}
      {ed/tdf/substrde.i total_weight         114 10  0}
      {rc/substr.i weight_unit_measure        125 02}
      {ed/tdf/substrde.i total_volume         127 10  0}
      {rc/substr.i volume_unit_measure        136 02}
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
      {rc/outstr.i packaging_code             18  05}
      {ed/tdf/outstrde.i lading_quantity      23  08   0}
      {rc/outstr.i commodity_code_qual        31  01}
      {rc/outstr.i commodity_code             32  16}
      {rc/outstr.i lading_desc                48  50}
      {rc/outstr.i weight_qual                98  02}
      {ed/tdf/outstrde.i total_weight         100 09  0}
      {rc/outstr.i weight_unit_measure        109 02}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i packaging_code             18  05}
      {ed/tdf/outstrde.i lading_quantity      23  08   0}
      {rc/outstr.i commodity_code_qual        31  01}
      {rc/outstr.i commodity_code             32  30}
      {rc/outstr.i lading_desc                62  50}
      {rc/outstr.i weight_qual                112 02}
      {ed/tdf/outstrde.i total_weight         114 11  0}
      {rc/outstr.i weight_unit_measure        125 02}
      {ed/tdf/outstrde.i total_volume         127 09  0}
      {rc/outstr.i volume_unit_measure        136 02}
      .
  END.
END CASE.
END.    /* O */
