/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ibmg.
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
IF ws_segment <> "BMG" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/substr.i   transaction_purpose_code         18  2}
      {rc/substr.i   transaction_description          20 80}
      {rc/substr.i   transaction_type_code           100  2}
      .
  END.
END.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/outstr.i   transaction_purpose_code         18  2}
      {rc/outstr.i   transaction_description          20 80}
      {rc/outstr.i   transaction_type_code           100  2}
      .
  END.
END.
END.    /* O */
