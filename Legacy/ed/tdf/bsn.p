/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\BSN.P
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
IF ws_segment <> "BSN" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i transaction_purpose_code     18  02}
      {rc/substr.i shipment_id                  20  30}
      {rc/substr.i manifest_create_date                    50  06}
      {rc/substr.i manifest_create_time                    56  08}
      {rc/substr.i hl_structure_code            64  04}
      {rc/substr.i transaction_type_code        68  02}
      {rc/substr.i status_reason_code           70  03}
      .
  END.
END CASE.
{rc/xyymmdd.i manifest_create_date manifest_create_date#}.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    manifest_create_date            = {rc/dt2ymd.i manifest_create_date#}
    .
  /* check mandatory assignments ... */
  IF   shipment_id = ""
    OR transaction_purpose_code = ""
    OR manifest_create_date = ""
    OR manifest_create_time = ""
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (id/purpose/date/time):" +
      shipment_id + "/" + transaction_purpose_code + "/" + manifest_create_date + "/" + manifest_create_time).
    RETURN error.
  END.
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/outstr.i transaction_purpose_code     18  02}
      {rc/outstr.i shipment_id                  20  30}
      {rc/outstr.i manifest_create_date         50  06}
      {rc/outstr.i manifest_create_time         56  06}
      {rc/outstr.i hl_structure_code            62  04}
      .
  END.
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i transaction_purpose_code     18  02}
      {rc/outstr.i shipment_id                  20  30}
      {rc/outstr.i manifest_create_date         50  06}
      {rc/outstr.i manifest_create_time         56  08}
      {rc/outstr.i hl_structure_code            64  04}
      {rc/outstr.i transaction_type_code        68  02}
      {rc/outstr.i status_reason_code           70  03}
      .
  END.
END CASE.
END.    /* O */
