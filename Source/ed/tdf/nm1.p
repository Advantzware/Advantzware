DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED VAR WS_PARTNER AS CHAR NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
def var nm102       as char no-undo format 'x(01)' label "Entity Type".
def var nm104       as char no-undo format 'x(25)' label "First Name".
def var nm105       as char no-undo format 'x(25)' label "Middle Name".
def var nm106       as char no-undo format 'x(10)' label "Prefix".
def var nm107       as char no-undo format 'x(10)' label "Suffix".
IF ws_segment <> "NM1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {rc/substr.i    entity_id                   18  03}
    {rc/substr.i    NM102                       21  01}
    {rc/substr.i    company_name                22  35}
    {rc/substr.i    NM104                       57  25}
    {rc/substr.i    NM105                       82  25}
    {rc/substr.i    NM106                      107  10}
    {rc/substr.i    NM107                      117  10}
    {rc/substr.i    id_code_qualifier          127  02}
    {rc/substr.i    id_code                    129  80}
    {rc/substr.i    entity_relationship_code   209  02}
    {rc/substr.i    entity_identifier_code     211  03}
    .
  END.  
  END CASE.  /* version */
CASE entity_id:
WHEN "BU" THEN
ASSIGN ordering_store_number = id_code.
WHEN "BY" THEN
ASSIGN ordering_store_number = id_code.
WHEN "BT" THEN
.
WHEN "FR" THEN
.    /* 816 FROM source of message */
WHEN "ST" OR WHEN "CQ" THEN
ASSIGN shipto_name = company_name shipto_store_number = id_code.
WHEN "Z7" OR WHEN "ZZ" THEN
ASSIGN mark_for_store_number = id_code.
WHEN "VN" THEN
ASSIGN vendor_name = company_name vendor_number = id_code.
WHEN "MA" THEN
ASSIGN contact_name = company_name.   /* 9810 CAH for FEDER */
OTHERWISE RUN rc/debugmsg.p
  ("unrecognized entity_id in N1 segment: " + entity_id + " name: "
  + company_name).
END.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  /* check mandatory assignments ... */
  IF entity_id = ""
    THEN
  DO:
    RUN rc/debugmsg.p ("Mandatory elements missing (entity_id)" ).
    RETURN error.
  END.
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {rc/outstr.i    entity_id                   18  03}
    {rc/outstr.i    NM102                       21  01}
    {rc/outstr.i    company_name                22  35}
    {rc/outstr.i    NM104                       57  25}
    {rc/outstr.i    NM105                       82  25}
    {rc/outstr.i    NM106                      107  10}
    {rc/outstr.i    NM107                      117  10}
    {rc/outstr.i    id_code_qualifier          127  02}
    {rc/outstr.i    id_code                    129  80}
    {rc/outstr.i    entity_relationship_code   209  02}
    {rc/outstr.i    entity_identifier_code     211  03}
    .
  END.  
  END CASE.  /* version */
END.    /* O */
if command matches "*P*" then do:
  display stream s-out
    ws_segment
    entity_id
    NM102
    company_name    format "x(35)" /*
    NM104
    NM105
    NM106
    NM107   
    */
    id_code format "x(30)" space(0) "/" space(0) id_code_qualifier no-label
    entity_relationship_code    label "Ent Relationship"
    entity_identifier_code      label "End ID Code"
  with side-labels width 144 no-box.  
end.      
