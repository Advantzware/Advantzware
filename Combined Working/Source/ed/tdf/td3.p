/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\td3.p
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
IF ws_segment <> "TD3" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    equipment_code              18  2}
    {rc/substr.i    equipment_initial           20  04}
    {rc/substr.i    trailer_number              24  10}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   equipment_code = ""
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (equipment_code)" ).
    RETURN error.
  END.
  ASSIGN
    {rc/outstr.i    equipment_code              18  2}
    {rc/outstr.i    equipment_initial           20  04}
    {rc/outstr.i    trailer_number              24  10}
    .
  CASE ws_version:
END CASE.
END.    /* O */
