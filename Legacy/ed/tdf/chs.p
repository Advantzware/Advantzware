/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ichs.
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
IF ws_segment <> "CHS" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    ship_complete_code           18  2}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    {rc/outstr.i    ship_complete_code           18  2}
    .
  CASE ws_version:
END CASE.
END.    /* O */
