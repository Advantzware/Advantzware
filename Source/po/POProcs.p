
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

PROCEDURE PO_GetLineScoresAndTypes:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch the score size and types from reftable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-reftable1 FOR reftable.
    DEFINE BUFFER bf-reftable2 FOR reftable.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER bf-job-mat   FOR job-mat.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ ipcCompany
           AND bf-po-ordl.po-no   EQ ipiPoID
           AND bf-po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.

    FIND FIRST bf-reftable1
         WHERE bf-reftable1.reftable EQ "POLSCORE"
           AND bf-reftable1.company  EQ bf-po-ordl.company
           AND bf-reftable1.loc      EQ "1"
           AND bf-reftable1.code     EQ STRING(bf-po-ordl.po-no,"9999999999")
           AND bf-reftable1.code2    EQ STRING(bf-po-ordl.line, "9999999999")
         NO-ERROR.
    FIND FIRST bf-reftable2
         WHERE bf-reftable2.reftable EQ "POLSCORE"
           AND bf-reftable2.company  EQ bf-po-ordl.company
           AND bf-reftable2.loc      EQ "2"
           AND bf-reftable2.code     EQ STRING(bf-po-ordl.po-no,"9999999999")
           AND bf-reftable2.code2    EQ STRING(bf-po-ordl.line, "9999999999")
        NO-ERROR.

    IF AVAILABLE bf-reftable1 THEN DO:
        DO iIndex = 1 TO 12:
            IF bf-reftable1.val[iIndex] EQ 0 THEN
                LEAVE.

            ASSIGN
                opdScores[iIndex]     = bf-reftable1.val[iIndex]
                opcScoreTypes[iIndex] = SUBSTRING(bf-reftable1.dscr, iIndex, 1)
                .
        END.
    END.
        
    IF AVAILABLE bf-reftable2 THEN DO:
        DO iIndex = 1 TO 8:
            IF bf-reftable2.val[iIndex] EQ 0 THEN
                LEAVE.

            ASSIGN
                opdScores[12 + iIndex]     = bf-reftable2.val[iIndex]
                opcScoreTypes[12 + iIndex] = SUBSTRING(bf-reftable2.dscr,iIndex,1)
                .            
        END.
    END.        
END PROCEDURE.

