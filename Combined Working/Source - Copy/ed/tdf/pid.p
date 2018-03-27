/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\pid.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM s-out.
def var temp_desc as char no-undo.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "PID" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i    item_description_type       18  01}
      {rc/substr.i    product_characteristic_code 19  03}
      {rc/substr.i    sac_agency_qualifier        22  02}
      {rc/substr.i    product_description_code    24  12}
      {rc/substr.i    temp_desc            36  80}
      .
  END.
END CASE.
CASE product_characteristic_code:
WHEN "08" THEN do:
 
    if item_description > "" then do:
        {rc/listadd.i second_description temp_desc}
    end.
    else item_description = temp_desc.
end.        
WHEN "75" THEN
ASSIGN item_color = temp_desc.
WHEN "91" THEN
ASSIGN item_size = temp_desc.
OTHERWISE RUN rc/debugmsg.p
  ("Unrecognized product_characteristic_code in segment PID: "
  + product_characteristic_code
  + " desc: "
  + temp_desc).
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE product_characteristic_code:
  WHEN "08" THEN
  ASSIGN temp_desc = item_description.
  WHEN "75" THEN
  ASSIGN temp_desc =  item_color.
  WHEN "91" THEN
  ASSIGN temp_desc =  item_size.
  OTHERWISE
    RUN rc/debugmsg.p
    ("Unrecognized product_characteristic_code in segment PID: "
    + product_characteristic_code
    + " desc: "
    + temp_desc).
END CASE.
/* check mandatory assignments ... */
IF item_description_type = ""
  THEN
DO:
  RUN rc/debugmsg.p
    ("Mandatory elements missing (item_description_type)" ).
  RETURN error.
END.
ASSIGN
  {rc/outstr.i    item_description_type       18  01}
  {rc/outstr.i    product_characteristic_code 19  03}
  {rc/outstr.i    sac_agency_qualifier        22  02}
  {rc/outstr.i    product_description_code    24  12}
  {rc/outstr.i    temp_desc            36  80}
  .
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    item_description_type           LABEL "Tp"
    product_characteristic_code     LABEL "Cd"
    sac_agency_qualifier            LABEL "Agency Qual"
    product_description_code        LABEL "Desc Cd"
    temp_desc                       LABEL "Desc"        FORMAT 'x(40)'
    WITH side-labels width 144 no-box.
END.
