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
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "SAC" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    allow_charge_indicator       18 01}
    {rc/substr.i    misc_elem[2]                 19 04} /* charge code */
    {rc/substr.i    sac_agency_qualifier         23 02}
    {rc/substr.i    sac_agency_code              25 10}
    {rc/substr.i    sac_reference_id            105 30}
    {rc/substr.i    sac_description             155 80}
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
    {rc/outstr.i    allow_charge_indicator       18 01}
    {rc/outstr.i    misc_elem[2]                 19 04}
    {rc/outstr.i    sac_agency_qualifier         23 02}
    {rc/outstr.i    sac_agency_code              25 10}
    {rc/outstr.i    sac_reference_id            105 30}
    {rc/outstr.i    sac_description             155 80}
    .
  CASE ws_version:
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    allow_charge_indicator      LABEL "Indicator"
    misc_elem[2]                LABEL "Code"        FORMAT 'x(04)'
    sac_agency_qualifier        LABEL "Agency"
    sac_agency_code             LABEL "Agency Code"
    sac_reference_id            LABEL "Reference"
    sac_description             LABEL "Description"
    WITH side-labels width 144 no-box.
END.
