
/*------------------------------------------------------------------------
    File        : cur.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Nov 29 15:25:29 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cBtHolder AS CHARACTER NO-UNDO INIT "BT".

/* ********************  Preprocessor Definitions  ******************** */

/* currency_denom_seller */
/* ***************************  Main Block  *************************** */

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
/*MESSAGE "test in cur entity_id"                    */
/*    "command" command skip                         */
/*    "ws_seg" ws_segment "cur" currency_denom_seller*/
/*    VIEW-AS ALERT-BOX.                             */
IF ws_segment <> "CUR" THEN
    RETURN error.
IF command matches "*I*" THEN
DO:


/*MESSAGE "test in n1 entity_id" entity_id "command" COMMAND skip*/
/*    "id_code" id_code skip                                     */
/*    "currency" currency_denom_seller skip                      */
/*    "cmd" COMMAND skip                                         */
/*    "ver" ws_version                                           */
/*    VIEW-AS ALERT-BOX.                                         */
IF top-debug THEN
    RUN rc/debugmsg.p
        ( "currency_denom_seller:" + currency_denom_seller
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
IF currency_denom_seller = ""
    THEN
DO:
    RUN rc/debugmsg.p ("Mandatory elements missing (currency_denom_seller)" ).
    RETURN error.
END.
CASE ws_version:
    WHEN "4010" THEN
        DO:
    ASSIGN
      {rc/outstr.i    cBtHolder                           18  03}
      {rc/outstr.i    currency_denom_seller                21  60}
            .
        END.
    WHEN "3060" THEN
        DO:
  
   ASSIGN
      {rc/outstr.i    cBtHolder                            18  03}
      {rc/outstr.i    currency_denom_seller                21  60}
                .
        END.
    OTHERWISE /* "3020" */
    DO:

    END.
END CASE.  /* version */
END.    /* O */
IF command matches "*P*" THEN
DO:
    DISPLAY STREAM s-out
        ws_segment
        company_name    LABEL "Name" FORMAT 'X(30)' SPACE(0) "/" SPACE(0) entity_id NO-LABEL
    currency_denom_seller LABEL "Currency!Seller"
        WITH side-labels width 144 no-box.
END.
