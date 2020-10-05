
/*------------------------------------------------------------------------
    File        : OperationProcTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for OperationProcs.p

    Author(s)   : BV
    Created     : Wed Sep 16 16:39:16 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdOpProcs    AS HANDLE.
DEFINE VARIABLE hdSession    AS HANDLE.

DEFINE VARIABLE gcCompany    AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE gcEstimateID AS CHARACTER NO-UNDO INITIAL "   14227".


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN est\OperationProcs.p PERSISTENT SET hdOpProcs.
RUN system\session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN ClearAttributes IN hdOpProcs.
RUN pGetOperationStandardsSingle(gcCompany, gcEstimateID).


/* **********************  Internal Procedures  *********************** */


PROCEDURE pGetOperationStandardsTest PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given company, location, and operation ID, display the run standards that apply to a
        given machine estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dOpMRWaste  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpMRHours  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpeed AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpoil AS DECIMAL   NO-UNDO.
    
    FIND FIRST est NO-LOCK 
        WHERE est.company EQ ipcCompany
        AND est.est-no EQ ipcEstimateID
        NO-ERROR.
    IF AVAILABLE est THEN 
    DO:
        FOR EACH eb NO-LOCK OF est,
            FIRST ef NO-LOCK OF eb:
            RUN SetAttributesFromRowid IN hdOpProcs (ROWID(eb), OUTPUT lError, OUTPUT cMessage).
            FOR EACH est-op NO-LOCK 
                WHERE est-op.company EQ eb.company
                AND est-op.est-no EQ eb.est-no
                AND est-op.s-num EQ eb.form-no:
                RUN GetOperationStandards IN hdOpProcs (est.company, est.loc, est-op.m-code, 
                    OUTPUT dOpMRWaste, OUTPUT dOpMRHours, OUTPUT dOpRunSpeed, OUTPUT dOpRunSpoil, OUTPUT lError, OUTPUT cMessage).
                MESSAGE "Machine: " est-op.m-code SKIP 
                    "Form-Blank: " est-op.s-num "-" est-op.b-num SKIP 
                    "MR Waste: " dOpMRWaste SKIP 
                    "MR Hours: " dOpMRHours SKIP 
                    "Run Speed: " dOpRunSpeed SKIP 
                    "Run Spoil: " dOpRunSpoil
                    VIEW-AS ALERT-BOX.            
            END.                
        END.
    END.
    
END PROCEDURE.

PROCEDURE pGetOperationStandardsSingle PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given company, location, and operation ID, display the run standards that apply to a
        given machine estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dOpMRWaste  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpMRHours  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpeed AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpoil AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMachineTarget AS CHARACTER NO-UNDO INITIAL "PRBV".
    
    FIND FIRST est NO-LOCK 
        WHERE est.company EQ ipcCompany
        AND est.est-no EQ ipcEstimateID
        NO-ERROR.
    IF AVAILABLE est THEN 
    DO:
        FOR EACH eb NO-LOCK OF est,
            FIRST ef NO-LOCK OF eb, 
            FIRST est-op NO-LOCK
                WHERE est-op.company EQ eb.company
                AND est-op.est-no EQ eb.est-no
                AND est-op.s-num EQ eb.form-no
                AND est-op.m-code EQ cMachineTarget:
            RUN SetAttributesFromEb IN hdOpProcs (ROWID(eb), OUTPUT lError, OUTPUT cMessage).
            
            RUN GetOperationStandards IN hdOpProcs (est.company, est.loc, cMachineTarget, 
                OUTPUT dOpMRWaste, OUTPUT dOpMRHours, OUTPUT dOpRunSpeed, OUTPUT dOpRunSpoil, OUTPUT lError, OUTPUT cMessage).
            MESSAGE  
                "MR Waste: " dOpMRWaste SKIP 
                "MR Hours: " dOpMRHours SKIP 
                "Run Speed: " dOpRunSpeed SKIP 
                "Run Spoil: " dOpRunSpoil SKIP 
                lError cMessage
                VIEW-AS ALERT-BOX.            
            
        END.
    END.
    
END PROCEDURE.

