
/*------------------------------------------------------------------------
    File        : txi.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Nov 29 15:25:13 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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
DEF SHARED VAR WS_PARTNER AS CHAR    NO-UNDO.
DEF SHARED var top-debug  AS logical NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
/*MESSAGE "test in txi tax_type" tax_type skip                 */
/*    "command" command skip                                   */
/*    "ws_seg" ws_segment "total_tax_dollars" total_tax_dollars*/
/*    VIEW-AS ALERT-BOX.                                       */
IF ws_segment <> "TXI" THEN
    RETURN error.
IF command matches "*I*" THEN
DO:


    IF top-debug THEN
        RUN rc/debugmsg.p
            ( ":" + entity_id
            + " company_name:" + company_name
            + " id_code_qualifier:" + id_code_qualifier
            )
  .
END.    /* I */
ELSE
    IF command matches "*O*" THEN
    DO:
        /* check mandatory assignments ... */
        IF tax_type = ""
            THEN
        DO:
            RUN rc/debugmsg.p ("Mandatory elements missing (tax_type)" ).
            RETURN error.
        END.
        DEFINE VARIABLE cTotal_Tax_Dollars AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTax_pct AS CHARACTER NO-UNDO.
        ASSIGN 
          cTotal_Tax_Dollars = string(total_tax_dollars)
          cTax_pct = string(tax_pct)
          .
        CASE ws_version:
            WHEN "4010" THEN
                DO:
    ASSIGN
      {rc/outstr.i    tax_type                   18  03}
      {rc/outstr.i    cTotal_Tax_Dollars         21  18}
      {rc/outstr.i    cTax_Pct                   39  18}
      /*
      {ed/tdf/substrde.i  total_tax_dollars      51  18  4}
      {ed/tdf/substrde.i   tax_pct               69  18  2} */
                    .
                END.
            WHEN "3060" THEN
                DO:

    ASSIGN
      {rc/outstr.i        tax_type               18  03}
  
      {rc/outstr.i    cTotal_Tax_Dollars         21  18}
      {rc/outstr.i    cTax_Pct                   39  18}
                        .
                END.
            OTHERWISE /* "3020" */
            DO:

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
