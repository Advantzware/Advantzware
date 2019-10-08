/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\asi.p
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
{rc/datev.i}
IF ws_segment <> "ASI" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i action_code                    18  02}
      {rc/substr.i maint_type_code                20  03}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   action_code = "" OR maint_type_code = ""
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (action_code or maint_type_code)" ).
    RETURN error.
  END.
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i action_code                    18  02}
      {rc/outstr.i maint_type_code                20  03}
      .
  END.
END CASE.
END.    /* O */
