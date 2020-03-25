
/*------------------------------------------------------------------------
    File        : POProcs.p
    Purpose     : 

    Syntax      :

    Description : Holds procedures for entering, editing and processing purchase orders

    Author(s)   : Rahul Rawat
    Created     : Wed Mar 25 02:17:43 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckPOLineStatus:
/*------------------------------------------------------------------------------
 Purpose: To Check a PO line status
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiPoNo
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE po-ordl AND NOT po-ordl.opened THEN DO:
        RUN DisplayMessage("19").
        RETURN ERROR.
    END. 


END PROCEDURE.

