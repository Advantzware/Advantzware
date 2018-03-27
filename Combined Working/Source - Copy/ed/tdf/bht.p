/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\bht.p
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
IF ws_segment <> "BHT" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/substr.i hl_structure_code              18  04}
      {rc/substr.i transaction_purpose_code       22  02}
      {rc/substr.i ref_number                     24  30}
      {rc/substr.i extra_date                     54  06}
      {rc/substr.i char_time                      60  08}
      {rc/substr.i transaction_type_code          68  02}
      .
  END.
END CASE.
IF length(extra_date) >= 6
  THEN
DO:
  {rc/xyymmdd.i extra_date extra_date#}.
END.
ELSE
ASSIGN extra_date# = ?.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF   extra_date# = ?
    THEN
  DO:
    RUN rc/debugmsg.p
      ("Mandatory elements missing (extra_date)" ).
    RETURN error.
  END.
  ASSIGN
    extra_date            = {rc/dt2ymd.i extra_date#}
    .
  CASE ws_version:
  OTHERWISE /* "3060" */
    DO:
    ASSIGN
      {rc/outstr.i hl_structure_code              18  04}
      {rc/outstr.i transaction_purpose_code       22  02}
      {rc/outstr.i ref_number                     24  30}
      {rc/outstr.i extra_date                     54  06}
      {rc/outstr.i char_time                      60  08}
      {rc/outstr.i transaction_type_code          68  02}
      .
  END.
END CASE.
END.    /* O */
