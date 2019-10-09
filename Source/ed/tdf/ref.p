/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ref.p
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
IF  ws_segment <> "REF" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "3020" THEN
  DO:
    ASSIGN
      {rc/substr.i  ref_qual                          18  2}
      {rc/substr.i  ref_number                        20 30}
      {rc/substr.i  ref_desc                          50 80}
      .
  END.
  OTHERWISE /* 3060 + 4010 */
    DO:
    ASSIGN
      {rc/substr.i  ref_qual                          18  3}
      {rc/substr.i  ref_number                        21 30}
      {rc/substr.i  ref_desc                          51 80}
      {rc/substr.i  reference_1_qualifier            131  3}
      {rc/substr.i  reference_1                      134 30}
      {rc/substr.i  reference_2_qualifier            164  3}
      {rc/substr.i  reference_2                      167 30}
      {rc/substr.i  reference_3_qualifier            197  3}
      {rc/substr.i  reference_3                      200 30}
      .
  END.
END CASE.
CASE ref_qual:
WHEN "ST" THEN
shipto_store_number = ref_number.
WHEN "BY" THEN
ordering_store_number = ref_number.
WHEN "BT" THEN
misc_elem[1] = ref_number. /* batch# */
WHEN "ZZ" THEN
delivery_zone = ref_number.
WHEN "19" THEN
sales_division = ref_number.
WHEN "SD" THEN
sales_division = ref_number.
WHEN "AE" THEN
AFE_number = ref_number.
WHEN "CO" THEN
misc_elem[1] = ref_number.   /* customer order# */
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
WHEN "MR" THEN
misc_elem[1] = ref_number.
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
  ref_number = misc_elem[1].
  WHEN "CO" THEN
  ref_number = misc_elem[1].
  WHEN "BT" THEN
  ref_number = misc_elem[1].
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
WHEN "3020" THEN
DO:
  ASSIGN
    {rc/outstr.i    ref_qual                        18  2}
    {rc/outstr.i    ref_number                      20 30}
    {rc/outstr.i    ref_desc                        50 80}
    .
END.
OTHERWISE /* 360 + 4010 */
  DO:
  ASSIGN
    {rc/outstr.i  ref_qual                          18  3}
    {rc/outstr.i  ref_number                        21 30}
    {rc/outstr.i  ref_desc                          51 80}
    /* 9808 CAH: Added repeat in 4 and 5
    as translator thinks they are mandatory ...
    {rc/outstr.i  ref_qual                          131  3}
    {rc/outstr.i  ref_number                        134 30}
    9809 CAH: Got templates with that error corrected.
    */
    /*
    {rc/outstr.i  reference_1_qualifier            131  3}
    {rc/outstr.i  reference_1                      134 30}
    {rc/outstr.i  reference_2_qualifier            164  3}
    {rc/outstr.i  reference_2                      167 30}
    {rc/outstr.i  reference_3_qualifier            197  3}
    {rc/outstr.i  reference_3                      200 30}
    */
    .
END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    ref_number LABEL "Ref#" SPACE(0) '/' SPACE(0) ref_qual NO-LABEL
    ref_desc    FORMAT 'x(30)' LABEL "Desc"
    WITH side-labels width 144 no-box.
END.
