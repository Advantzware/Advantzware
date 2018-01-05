/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\in3.p
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
IF ws_segment <> "SLN" THEN
RETURN error.
/*
IF command matches "*I*" THEN
DO:
  ASSIGN
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    .
  CASE ws_version:
END CASE.
END.    /* O */
*/
