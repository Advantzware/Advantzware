/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\SN1.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "SN1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i             item_assigned_id        18        20}
    {ed/tdf/substrde.i quantity_shipped        38        10        0}
    {rc/substr.i       unit_of_measure                49        02}
    {ed/tdf/substrde.i quantity_cumulative        51        09        0}
    {ed/tdf/substrde.i quantity_ordered        61        09        0}
    {rc/substr.i       unit_of_measure2        71        02}
    {rc/substr.i       returnable_container_code        73        02}
    {rc/substr.i       item_status_code        75        02}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/outstr.i       item_assigned_id         18  11}
      {ed/tdf/outstrde.i quantity_shipped         29  11  0}
      {rc/outstr.i       unit_of_measure          40  02}
      {ed/tdf/outstrde.i quantity_cumulative      42  10  0}
      {ed/tdf/outstrde.i quantity_ordered         52  10  0}
      {rc/outstr.i       unit_of_measure2         62  02}
      {rc/outstr.i       returnable_container_code 64 02}
      {rc/outstr.i       item_status_code         66  02}
      .
  END.
  OTHERWISE /* 3060 */
    DO:
    ASSIGN
      {rc/outstr.i       item_assigned_id         18  20}
      {ed/tdf/outstrde.i quantity_shipped         38  11  0}
      {rc/outstr.i       unit_of_measure          49  02}
      {ed/tdf/outstrde.i quantity_cumulative      51  10  0}
      {ed/tdf/outstrde.i quantity_ordered         61  10  0}
      {rc/outstr.i       unit_of_measure2         71  02}
      {rc/outstr.i       returnable_container_code 73 02}
      {rc/outstr.i       item_status_code         75  02}
      .
  END.
END CASE.
END.    /* O */
