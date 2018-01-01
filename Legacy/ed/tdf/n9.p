/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\n9.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM S-OUT.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
DEF var n904    AS char NO-UNDO.
DEF var n905    AS char NO-UNDO.
DEF var n906    AS char NO-UNDO.
DEF var n907    AS char NO-UNDO.
DEF var check_number AS char NO-UNDO.
IF  ws_segment <> "N9" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE /* 4010 */
    DO:
    ASSIGN
      {rc/substr.i  ref_qual                        18  03}
      {rc/substr.i  ref_number                      21  30}
      {rc/substr.i  ref_desc                        51  45}
      {rc/substr.i  n904                            96  08}
      {rc/substr.i  n905                            104 08}
      {rc/substr.i  n906                            112 02}
      {rc/substr.i  n907                            114 03}
      .
  END.
END CASE.
CASE ref_qual:
WHEN "AH" THEN. /* FEDER.850.4010 unknown qualifier */
WHEN "ST" THEN
shipto_store_number = ref_number.
WHEN "BY" THEN
ordering_store_number = ref_number.
WHEN "BT" THEN
.
WHEN "ZZ" THEN
delivery_zone = ref_number.
WHEN "19" THEN
sales_division = ref_number.
WHEN "SD" THEN
sales_division = ref_number.
WHEN "AE" THEN
AFE_number = ref_number.
WHEN "DP" THEN
department_number = ref_number.
WHEN "IA" THEN
vendor_number = ref_number.
WHEN "MR" THEN
merchandise_type = ref_number.
WHEN "PD" THEN
promotion_event_code = ref_number.
WHEN "BM" THEN
bill_of_lading_number = ref_number.
WHEN "BN" THEN
booking_number = ref_number.
WHEN "CN" THEN
pro_number = ref_number.
WHEN "SN" THEN
seal_number = ref_number.
WHEN "CK" THEN
check_number = ref_number.
WHEN "IV" THEN
invoice_number = ref_number. /* 9901 812.4010 */
WHEN "L1" THEN .    /* 9905 CAH: sears "SPECIAL INSTRUCTIONS" - leave it in ref number */
OTHERWISE
  DO:
  RUN rc/debugmsg.p
    ("Unrecognized ref_qual: " + ref_qual + " ref_number: " + ref_number).
  RETURN error.
END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF ref_qual = "" THEN
  RETURN error.
  CASE ref_qual:
  WHEN "AH" THEN .  /* 9905 cah FEDER.850.4010 UNKNOWN QUALIFIER */
  WHEN "ST" THEN
  ref_number = shipto_store_number.
  WHEN "BY" THEN
  ref_number = ordering_store_number.
  WHEN "ZZ" THEN
  ref_number = delivery_zone.
  WHEN "19" OR WHEN "SD" THEN
  ref_number = sales_division.
  WHEN "AE" THEN
  ref_number = AFE_number.
  WHEN "DP" THEN
  ref_number = department_number.
  WHEN "IA" THEN
  ref_number = vendor_number.
  WHEN "MR" THEN
  ref_number = merchandise_type.
  WHEN "PD" THEN
  ref_number = promotion_event_code.
  WHEN "BM" THEN
  ref_number = bill_of_lading_number.
  WHEN "BN" THEN
  ref_number = booking_number.
  WHEN "CN" THEN
  ref_number = pro_number.
  WHEN "SN" THEN
  ref_number = seal_number.
  WHEN "BT" THEN
  .
  WHEN "CK" THEN
  ref_number = check_number.      /* 9901 820.4010 */
  WHEN "IV" THEN
  ref_number = invoice_number.    /* 9901 812.4010 */
  WHEN "L1" THEN .      /* 9905 sears 850.4010 special instructions */
  OTHERWISE
    DO:
    RUN rc/debugmsg.p ("Unrecognized ref_qual: " + ref_qual + " ref_number: " + ref_number).
    RETURN error.
  END.
END CASE.
/* check mandatory assignments ... */
IF ref_number <= " "
  THEN
DO:
  RUN rc/debugmsg.p
    ("Mandatory element missing (ref_number) for qualifier: " + ref_qual).
  RETURN error.
END.
CASE ws_version:
OTHERWISE /* 4010 */
  DO:
  ASSIGN
    {rc/outstr.i  ref_qual                        18  03}
    {rc/outstr.i  ref_number                      21  30}
    {rc/outstr.i  ref_desc                        51  45}
    {rc/outstr.i  n904                            96  08}
    {rc/outstr.i  n905                            104 08}
    {rc/outstr.i  n906                            112 02}
    {rc/outstr.i  n907                            114 03}
    .
END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    ref_number  label "Ref#" SPACE(0) '/' SPACE(0) ref_qual no-label
    ref_desc    label "Desc" FORMAT 'x(30)'
    WITH side-labels width 144 no-box.
END.
