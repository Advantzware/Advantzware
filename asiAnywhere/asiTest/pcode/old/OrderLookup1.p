
/*------------------------------------------------------------------------
    File         : OrderLookup
    Purpose     : customer lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrderLookup1.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER PrmOrder as character no-undo.
DEFINE INPUT PARAMETER prmCustname as character no-undo.
DEFINE INPUT PARAMETER prmEst as character no-undo.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrd2.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

ASSIGN prmAction = "search".

IF prmOrder = ? THEN ASSIGN prmOrder = "".
IF prmCustname = ? THEN ASSIGN prmCustname = "".
IF prmEst = ? THEN ASSIGN prmEst = ""
.

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:        
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-Ord2Query:QUERY-PREPARE(v-qry-string).        
        DATASET dsOrd2:FILL().

        FOR EACH oe-ord no-lock:
            
        END.  /*FOR EACH oe-ord*/
    END. /*WHEN "search" THEN DO: */
END CASE.
/* ***************************  Procedures  *************************** */

PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    ASSIGN prm-query = " FOR EACH oe-ord NO-LOCK WHERE oe-ord.company = '" + prmComp + "' and oe-ord.cust-no = '" + prmCust + "' ".
    
    IF prmOrder <> "" THEN ASSIGN prm-query = prm-query + " and oe-ord.ord-no begins '" + prmOrder  + "'".
    IF prmCustname <> "" THEN ASSIGN prm-query = prm-query + " and oe-ord.cust-name begins '" + prmCustname + "'".
    IF prmEst <> "" THEN ASSIGN prm-query = prm-query + " and oe-ord.est-no begins '" + prmEst + "'".

END PROCEDURE.
              







