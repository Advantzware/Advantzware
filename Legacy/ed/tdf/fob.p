/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\ifob.
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
IF ws_segment <> "FOB" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    shipment_method_payment_code 18  2}
    {rc/substr.i    ship_location_qualifier      20  2}
    .
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    misc_elem[3]             22 80} /* description */
      {rc/substr.i    misc_elem[4]            102 02} /* transport terms qual */
      {rc/substr.i    misc_elem[5]            104 03} /* transport terms code */
      {rc/substr.i    misc_elem[6]            107 02} /* location qual */
      {rc/substr.i    misc_elem[7]            109 80} /* description */
      {rc/substr.i    misc_elem[8]            189 02} /* risk of loss code */
      {rc/substr.i    misc_elem[9]            191 80} /* description */
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF shipment_method_payment_code = "" THEN
  DO:
    RETURN error.
  END.
  ASSIGN
    {rc/outstr.i    shipment_method_payment_code 18  2}
    {rc/outstr.i    ship_location_qualifier      20  2}
    .
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    misc_elem[3]             22 80} /* description */
      {rc/outstr.i    misc_elem[4]            102 02} /* transport terms qual */
      {rc/outstr.i    misc_elem[5]            104 03} /* transport terms code */
      {rc/outstr.i    misc_elem[6]            107 02} /* location qual */
      {rc/outstr.i    misc_elem[7]            109 80} /* description */
      {rc/outstr.i    misc_elem[8]            189 02} /* risk of loss code */
      {rc/outstr.i    misc_elem[9]            191 80} /* description */
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    shipment_method_payment_code
    ship_location_qualifier
    misc_elem[3]            FORMAT "x(80)" LABEL "Description"
    misc_elem[4]            FORMAT "x(02)" LABEL "Trans terms"
    SPACE(0) "/" SPACE(0)
    misc_elem[5]            FORMAT "x(03)" NO-LABEL  /* transport terms */
    SKIP
    SPACE(10)
    misc_elem[6]            FORMAT "x(02)" LABEL "Loc qual"
    misc_elem[7]            FORMAT "x(80)" LABEL "Description"
    SKIP
    SPACE(10)
    misc_elem[8]            FORMAT "x(02)" LABEL "Risk of loss code"
    misc_elem[9]            FORMAT "x(80)" LABEL "Description"
    WITH side-labels width 144 no-box.
END.
