/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\per.p
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
DEF var ws_tel_number AS char NO-UNDO.
IF ws_segment <> "PER" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    contact_function_code           18  2}
      {rc/substr.i    contact_name                    20  60}
      {rc/substr.i    contact_number_qualifier        80  2}
      {rc/substr.i    ws_tel_number                   82  80}
      .
  END.
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i    contact_function_code           18  2}
      {rc/substr.i    contact_name                    20  35}
      {rc/substr.i    contact_number_qualifier        55  2}
      {rc/substr.i    ws_tel_number                   57  25}
      .
  END.
END CASE.
CASE contact_number_qualifier:
WHEN "FX" THEN
contact_fax_number = ws_tel_number.
OTHERWISE contact_phone_number = ws_tel_number.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF contact_function_code = ""
    THEN
  DO:
    RUN rc/debugmsg.p ("Mandatory elements missing (contact_function_code)" ).
    RETURN error.
  END.
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    contact_function_code           18  2}
      {rc/outstr.i    contact_name                    20  60}
      {rc/outstr.i    contact_number_qualifier        80  2}
      {rc/outstr.i    ws_tel_number                   82  80}
      .
  END.
  OTHERWISE
  DO:
    ASSIGN
      {rc/outstr.i    contact_function_code           18  2}
      {rc/outstr.i    contact_name                    20  35}
      {rc/outstr.i    contact_number_qualifier        55  2}
      {rc/outstr.i    ws_tel_number                   57  25}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    contact_name    label "Contact" SPACE(0) "/" SPACE(0) contact_function_code no-label
    ws_tel_number   label "Number"  SPACE(0) "/" SPACE(0) contact_number_qualifier no-label
    WITH side-labels no-box width 144.
END.
