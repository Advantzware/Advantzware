/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\shd.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM S-OUT.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF  ws_segment <> "SHD" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* 4010 */
    DO:
    ASSIGN
      {ed/tdf/substrde.i quantity_shipped           18  11}
      {rc/substr.i  unit_of_measure                 37  02}
      {rc/substr.i  carrier_scac_code               79  04}
      .
  END.
END CASE.
END.    /* I */
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* 4010 */
    DO:
    ASSIGN
      {ed/tdf/outstrde.i quantity_shipped           18  11 0}
      {rc/outstr.i  unit_of_measure                 37  02}
      {rc/outstr.i  carrier_scac_code               79  04}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    quantity_shipped
    unit_of_measure
    carrier_scac_code
    WITH side-labels width 144 no-box.
END.
