/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\hl.p
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
IF ws_segment <> "HL" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i  hl_id                            18        12}
    {rc/substr.i        hl_parent_id                        30        12}
    {rc/substr.i         hl_level_code                        42        02}
    {rc/substr.i        hl_child_id                        44        01}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    {rc/outstr.i  hl_id                            18        12}
    {rc/outstr.i        hl_parent_id                        30        12}
    {rc/outstr.i         hl_level_code                        42        02}
    {rc/outstr.i        hl_child_id                        44        01}
    .
  CASE ws_version:
END CASE.
END.    /* O */
