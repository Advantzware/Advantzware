/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\isac.
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
IF ws_segment <> "SSS" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    allow_charge_indicator       18  1}
    {rc/substr.i    sac_agency_qualifier         19  2}
    {rc/substr.i    sac_agency_code              21 10}
    {rc/substr.i    sac_description              96 80}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF allow_charge_indicator = "" THEN
  RETURN error.
  /*
  then do:
  run rc/debugmsg.p
  ("Mandatory elements missing (allow_charge_indicator)" ).
  return error.
  end.
  */
  ASSIGN
    {rc/outstr.i    allow_charge_indicator       18  1}
    {rc/outstr.i    sac_agency_qualifier         19  2}
    {rc/outstr.i    sac_agency_code              21 10}
    {rc/outstr.i    sac_description              96 80}
    .
  CASE ws_version:
END CASE.
END.    /* O */
