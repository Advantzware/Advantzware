/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\in4.p
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
IF ws_segment <> "N4" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3060" OR WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    city                         18  30}
      {rc/substr.i    state                        48   2}
      {rc/substr.i    zip                          50  15}
      {rc/substr.i    country                      65   3}
      {rc/substr.i    location_qual                68   2}
      {rc/substr.i    location_code                70  30}
      .
  END.  /* 3060 */
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/substr.i    city                         18  19}
      {rc/substr.i    state                        37   2}
      {rc/substr.i    zip                          39   9}
      {rc/substr.i    country                      48   2}
      {rc/substr.i    location_qual                50   2}
      {rc/substr.i    location_code                52  25}
      .
  END.
END CASE.  /* N4 */
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF city = "" AND country = "" AND location_qual = "" THEN
  RETURN error.
  CASE ws_version:
  WHEN "3060" OR WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    city                         18  30}
      {rc/outstr.i    state                        48   2}
      {rc/outstr.i    zip                          50  15}
      {rc/outstr.i    country                      65   3}
      {rc/outstr.i    location_qual                68   2}
      {rc/outstr.i    location_code                70  30}
      .
  END.  /* 3060 */
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/outstr.i    city                         18  19}
      {rc/outstr.i    state                        37   2}
      {rc/outstr.i    zip                          39   9}
      {rc/outstr.i    country                      48   2}
      {rc/outstr.i    location_qual                50   2}
      {rc/outstr.i    location_code                52  25}
      .
  END.
END CASE.  /* N4 */
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    city state zip country
    location_qual LABEL "Location" SPACE(0) "/" SPACE(0) location_code NO-LABEL
    WITH side-labels width 144 no-box.
END.
