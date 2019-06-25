
/*------------------------------------------------------------------------
    File        : orderenq.p
    Purpose     : Order Enquiry Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Jyoti Bajaj
    Created     : Sat August 25 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{dsOrdEnq1.i}
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPonum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartdesc  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFrmdate   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTodate    AS CHARACTER  NO-UNDO.
/*DEFINE INPUT PARAMETER prmPartDisp  AS CHARACTER  NO-UNDO.*/


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
assign prmAction = "search".

DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:
        
        RUN build-qry. /*IN THIS-PROCEDURE (OUTPUT v-qry-string).*/
        
        ASSIGN
            v-qry-handle = QUERY q-OrderQuery:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsOrder:FILL().
    END.
 
END CASE.
/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
   /* DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.*/
    IF prmPonum = "" or poPartnum = "" or prmPartdesc = "" or prmFrmdate = "" or prmTodate = "" THEN
            FOR EACH oe-ord NO-LOCK :
    ELSE
              FOR EACH oe-ord NO-LOCK:
              Find first oe-ordl WHERE oe.ordl.po-no = oe-ord.po-no and oe-ordl.ord-no = oe-ord.ord-no no-lock no-error.          
END PROCEDURE.

