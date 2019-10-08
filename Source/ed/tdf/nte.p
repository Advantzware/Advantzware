/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\inte.
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM command AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "NTE" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN  /* 3060 */
    {rc/substr.i    note_reference_code          18  3}
    {rc/substr.i    note                         21 80}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN  /* 3060 */
    {rc/outstr.i    note_reference_code          18  3}
    {rc/outstr.i    note                         21 80}
    .
  CASE ws_version:
END CASE.
END.    /* O */
