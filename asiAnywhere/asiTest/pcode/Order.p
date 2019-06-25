
/*------------------------------------------------------------------------
    File        : Order.p
    Purpose     : Order

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : SEP 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Order.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder1.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.


DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
       RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
        
        ASSIGN
            v-qry-handle = QUERY q-Order1Query:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsOrder1:FILL().
   

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
        ASSIGN
            prm-query = "FOR EACH oe-ordl NO-LOCK".
    
                                               
END PROCEDURE.
