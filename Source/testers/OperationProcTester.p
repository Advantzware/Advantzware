
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


{est\ttOperationAttribute.i}
{est\OperationProcsTT.i }
{est\ttMachineRoutings.i}
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN est\OperationProcs.p PERSISTENT SET hdOpProcs.
RUN system\session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

// RUN pChangeOperation.
//RUN Operations_ClearAttributes IN hdOpProcs.
RUN pGetOperationStandardsTest(gcCompany, gcEstimateID).
//RUN pBuildRoutingTest.
//RUN pBuildEstOPTest.

/* **********************  Internal Procedures  *********************** */


PROCEDURE pBuildRoutingTest PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO INIT "  101547".
    
    
    
    
    FIND FIRST est no-lock
        WHERE est.est-no = cEstNo NO-ERROR.
        
    IF NOT AVAIL est then
    DO:
        MESSAGE "No Estimate"
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
    RUN Operations_BuildEstimateRoutingTT IN hdOpProcs (est.company, est.est-no, 0,est.est-qty[1]).
    
    RUN Operations_GetEstimateRoutingTT IN hdOpProcs (OUTPUT TABLE ttRouting).
    
    FOR EACH ttRouting:
        
        MESSAGE 
        "Company" ttRouting.Company SKIP
        "EstimateNo" ttRouting.EstimateNo SKIP
        "F " ttRouting.FormId " B " ttRouting.BlankId  " P " ttRouting.Pass Skip
        "Machine" ttRouting.OperationId skip
        "Dept" ttRouting.departmentID
        
        VIEW-AS ALERT-BOX.
        
    END. 


END PROCEDURE.

PROCEDURE pBuildEstOPTest PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INIT "001".
    DEFINE VARIABLE cEstNo   AS CHARACTER NO-UNDO INIT "  101547".
    
    cEstNo = "  101496".
    
    
    FIND FIRST est no-lock
        WHERE est.company = cCompany
          AND est.est-no = cEstNo NO-ERROR.
        
    IF NOT AVAIL est then
    DO:
        MESSAGE "No Estimate"
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    FOR EACH Est-op exclusive-lock
        WHERE est-op.company = est.company
        AND est-op.est-no  = est.est-no:
        DELETE est-op.
    END.
    
    RUN Operations_BuildEstimateRouting IN hdOpProcs (est.company, est.est-no, 0,est.est-qty[1]).
    
    FOR EACH Est-op NO-LOCK
        WHERE est-op.company = est.company
        AND est-op.est-no  = est.est-no:
        
        MESSAGE 
        "F " est-op.s-num " B " est-op.b-num  " P " est-op.op-pass Skip
        "Machine" est-op.m-code " :: " est-op.dept skip
        "Speed" est-op.op-speed skip
        "MR Hrs" est-op.op-mr skip
        "Spoil" est-op.op-spoil skip
        "MR waste" est-op.op-waste skip
        "Num Sheet" est-op.num-sh skip
        
        VIEW-AS ALERT-BOX.
        
    END. 


END PROCEDURE.


PROCEDURE pChangeOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
     DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
    
    FIND FIRST job NO-LOCK 
        WHERE job.company EQ '001'
        AND job.job-no EQ 'W01356'
        AND job.job-no2 EQ 0
        NO-ERROR.
    RUN Operations_ProcessOperationChange IN hdOpProcs (job.company,"PRESS", job.job, 1,0,1,"PR", OUTPUT cAction).

END PROCEDURE.

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
            FOR EACH est-op NO-LOCK 
                WHERE est-op.company EQ eb.company
                AND est-op.est-no EQ eb.est-no
                AND est-op.s-num EQ eb.form-no:
                RUN Operations_ClearAttributes IN hdOpProcs.
                RUN Operations_SetAttributesFromEstOp IN hdOpProcs (ROWID(est-op), eb.loc, eb.eqty, OUTPUT lError, OUTPUT cMessage).
                RUN Operations_GetOperationStandards IN hdOpProcs (est.company, est.loc, est-op.m-code, 
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
            RUN Operations_SetAttributesFromEstOp IN hdOpProcs (ROWID(est-op), eb.loc, eb.eqty, OUTPUT lError, OUTPUT cMessage).
            
            RUN Operations_GetOperationStandards IN hdOpProcs (est.company, est.loc, cMachineTarget, 
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

