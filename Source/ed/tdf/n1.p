/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\in1.p
**       By:
** Descript:
05.20.99 by CAH on \\ricky\robj8\dev Log#0000:
1.  Conditional assignment on SU and VN as it might overwrite vendor data
from prior REF IA.  feder.850.4010..
10.09.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added ZZ = mark_for_store on inbound.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED VAR WS_PARTNER AS CHAR NO-UNDO.
DEF SHARED var top-debug AS logical NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}

IF ws_segment <> "N1" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    entity_id                   18  03}
      {rc/substr.i    company_name                21  60}
      {rc/substr.i    id_code_qualifier           81  02}
      {rc/substr.i    id_code                     83  80}
      {rc/substr.i    entity_relationship_code   163  02}
      {rc/substr.i    entity_identifier_code     165  03}
      .
  END.
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/substr.i    entity_id                       18  2}
      {rc/substr.i    company_name                    20  35}
      {rc/substr.i    id_code_qualifier               55  2}
      {rc/substr.i    id_code                         57  17}
      {rc/substr.i    entity_relationship_code        77  2}
      {rc/substr.i    entity_identifier_code          79  2}
      .
  END.
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/substr.i    entity_id                       18  2}
      {rc/substr.i    company_name                    20  35}
      {rc/substr.i    id_code_qualifier               55  2}
      {rc/substr.i    id_code                         57  17}
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
WHEN "MA" THEN
ASSIGN ws_contact_name = company_name.   /* 9810 CAH for FEDER */
WHEN "PE" THEN
.    /* 9901 CAH PAYEE FROM SEARS 4010 820 */
WHEN "PR" THEN
.    /* 9901 CAH PAYER FROM SEARS 4010 820 */
WHEN "RI" THEN
remit_number = id_code.
WHEN "SU" or WHEN "VN" THEN ASSIGN /* 9905 CAH: N1.SU was overwriting REF.IA */
    vendor_name = (IF company_name > "" then company_name else vendor_name)
    vendor_number = (if id_code > "" then id_code else vendor_number).
OTHERWISE RUN rc/debugmsg.p
  ("unrecognized entity_id in N1 segment: " + entity_id + " name: "
  + company_name).
END.

IF top-debug THEN
RUN rc/debugmsg.p
  ( "entity_id:" + entity_id
  + " company_name:" + company_name
  + " id_code_qualifier:" + id_code_qualifier
  + " id_code:" + id_code
  + " entity_relationship_code:" + entity_relationship_code
  + " entity_identifier_code:" + entity_identifier_code).
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
      {rc/outstr.i    company_name                21  60}
      {rc/outstr.i    id_code_qualifier           81  02}
      {rc/outstr.i    id_code                     83  80}
      {rc/outstr.i    entity_relationship_code   163  02}
      {rc/outstr.i    entity_identifier_code     165  03}
      .
  END.
  WHEN "3060" THEN
  DO:
    ASSIGN
      {rc/outstr.i    entity_id                       18  2}
      {rc/outstr.i    company_name                    20  35}
      {rc/outstr.i    id_code_qualifier               55  2}
      {rc/outstr.i    id_code                         57  17}
      {rc/outstr.i    entity_relationship_code        77  2}
      {rc/outstr.i    entity_identifier_code          79  2}
      .
  END.
  OTHERWISE /* "3020" */
    DO:
    ASSIGN
      {rc/outstr.i    entity_id                       18  2}
      {rc/outstr.i    company_name                    20  35}
      {rc/outstr.i    id_code_qualifier               55  2}
      {rc/outstr.i    id_code                         57  17}
      .
  END.
END CASE.  /* version */
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    company_name    LABEL "Name" FORMAT 'X(30)' SPACE(0) "/" SPACE(0) entity_id NO-LABEL
    id_code         LABEL "ID"   FORMAT 'X(30)' SPACE(0) "/" SPACE(0) id_code_qualifier NO-LABEL
    entity_relationship_code    LABEL "Ent Relationship"
    entity_identifier_code      LABEL "Ent ID Code"
    WITH side-labels width 144 no-box.
END.
