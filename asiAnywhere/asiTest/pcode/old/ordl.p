
/*------------------------------------------------------------------------
    File        : ordl.p
    Purpose     : Order

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Sep 21, 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ordl.i}
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsordl.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.


DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
       RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
        
        ASSIGN
            v-qry-handle = QUERY q-ordlQuery:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsordl:FILL().
   

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
        ASSIGN
            prm-query = "FOR EACH oe-ord NO-LOCK".
    
                                               
END PROCEDURE.
