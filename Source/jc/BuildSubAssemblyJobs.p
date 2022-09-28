
/*------------------------------------------------------------------------
    File        : BuildSubAssemblyJobs.p
    Purpose     : 

    Syntax      :

    Description : Builds all jobs for Sub Assemblies given a fully built master 
                  job rowid

    Author(s)   : BV
    Created     : Wed Jun 08 14:38:59 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE ttSubAssemblyJob
    FIELD riEst                     AS ROWID
    FIELD riJobHdr                  AS ROWID
    FIELD sourceEstimateID          AS CHARACTER
    FIELD quantityPerMasterSet      AS DECIMAL
    FIELD quantityForSubAssemblyJob AS DECIMAL
    .
    

DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER,
    ipcEstTypeID AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN jc\JobProcs.p PERSISTENT SET hdJobProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJobProcs).

RUN pBuildSubAssemblyJobs (ipriJob, OUTPUT oplError, OUTPUT opcMessage).

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJobProcs).
DELETE PROCEDURE hdJobProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildSubAssemblyJobs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main processing procedure.  Given a rowid for a job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-est     FOR est.
    DEFINE BUFFER bf-eb      FOR eb.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-SubEst  FOR est.
    
    DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-job NO-LOCK 
        WHERE ROWID(bf-job) EQ ipriJob NO-ERROR.

    IF NOT AVAILABLE bf-job THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Rowid for Job"
            . 
        RETURN.
    END.
    cJob = DYNAMIC-FUNCTION("sfFormat_TrimmedJobWithHyphen",bf-job.job-no,bf-job.job-no2).
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ bf-job.company
        AND bf-est.est-no EQ bf-job.est-no
        NO-ERROR. 
    IF AVAILABLE bf-est THEN 
    DO:
        IF fIsSet(bf-est.est-type, bf-est.estimateTypeID) THEN 
        DO:
            FIND FIRST bf-job-hdr NO-LOCK 
                WHERE bf-job-hdr.company EQ bf-job.company
                AND bf-job-hdr.job EQ bf-job.job
                AND bf-job-hdr.frm EQ 0
                AND bf-job-hdr.blank-no EQ 0
                NO-ERROR.
            IF NOT AVAILABLE bf-job-hdr THEN 
                FIND FIRST bf-job-hdr NO-LOCK 
                    WHERE bf-job-hdr.company EQ bf-job.company
                    AND bf-job-hdr.job EQ bf-job.job
                    AND bf-job-hdr.frm EQ 1
                    AND bf-job-hdr.blank-no EQ 1
                    NO-ERROR.
            IF AVAILABLE bf-job-hdr THEN 
            DO:
                FOR EACH bf-eb NO-LOCK 
                    WHERE bf-eb.company EQ bf-job.company
                    AND bf-eb.est-no EQ bf-job.est-no
                    AND bf-eb.sourceEstimate NE "":
                
                    FIND FIRST bf-SubEst NO-LOCK
                        WHERE bf-SubEst.company EQ bf-eb.company
                        AND bf-SubEst.est-no EQ bf-eb.sourceEstimate
                        NO-ERROR.
                    IF AVAILABLE bf-SubEst THEN 

                    DO:
                        CREATE ttSubAssemblyJob.
                        ASSIGN 
                            ttSubAssemblyJob.riEst                     = ROWID(bf-SubEst)
                            ttSubAssemblyJob.riJobHdr                  = ROWID(bf-job-hdr)
                            ttSubAssemblyJob.sourceEstimateID          = bf-SubEst.est-no
                            ttSubAssemblyJob.quantityPerMasterSet      = bf-eb.quantityPerSet
                            ttSubAssemblyJob.quantityForSubAssemblyJob = bf-eb.quantityPerSet * bf-job-hdr.qty
                            .
                    END.
                END.
            END.  //Each bf-eb for set 
        END.    
        ELSE 
            opcMessage = "Job " + cJob + " is not a set. No subassembly jobs built".        
    END.
    ELSE 
        opcMessage = "Invalid estimate number" + bf-job.est-no + " for job " + cJob. 
        
    IF CAN-FIND(FIRST ttSubAssemblyJob) THEN 
        RUN pCreateSubAssemblyJobs(OUTPUT oplError, INPUT-OUTPUT opcMessage).
    
    
END PROCEDURE.

PROCEDURE pCreateSubAssemblyJob PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Creates a
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEst AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipriJobHdr AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-est FOR est.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-job FOR job.
    
    DEFINE VARIABLE iJobNoInternal AS INTEGER NO-UNDO.
    DEFINE VARIABLE cJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2 AS INTEGER NO-UNDO.
            
    FIND FIRST bf-est NO-LOCK
        WHERE ROWID(bf-est) EQ ipriEst 
        NO-ERROR.
    FIND FIRST bf-job-hdr NO-LOCK 
        WHERE ROWID(bf-job-hdr) EQ ipriJobHdr
        NO-ERROR.
        
    IF NOT AVAILABLE bf-est OR NOT AVAILABLE bf-job-hdr THEN RETURN.
    ASSIGN 
        cJobNo = bf-job-hdr.job-no
        iJobNo2 = bf-job-hdr.frm
        .
         
    RUN Job_GetJobNoInternal (
        bf-est.company,
        OUTPUT iJobNoInternal
        ).    
     
    CREATE bf-job.    
    ASSIGN
        bf-job.job        = iJobNoInternal
        bf-job.company    = bf-est.company
        bf-job.loc        = bf-est.loc
        bf-job.stat       = "R"
        bf-job.est-no     = bf-est.est-no 
        bf-job.csrUser_id = USERID("asi") 
        .
    RUN Job_GetJobNo(bf-est.company,
                     bf-est.est-no, 
                     0, //Order 
                     iJobNoInternal, 
                     bf-job-hdr.job, 
                     YES,  //is subassembly 
                     INPUT-OUTPUT cJobNo, 
                     INPUT-OUTPUT iJobNo2).
    
                     
    ASSIGN
        bf-job.job-no  = cJobNo
        bf-job.job-no2 = iJobNo2
        bf-job.spare-int-1 = bf-job-hdr.job.  
       
    RUN jc/BuildJob.p (ROWID(bf-job), 0, bf-job-hdr.qty, OUTPUT oplError, OUTPUT iopcMessage).                       
    //RUN jc/jc-calc.p (RECID(bf-job), YES) NO-ERROR.

    //aRUN pUpdateFGItemQty(BUFFER bf-job).


END PROCEDURE.

PROCEDURE pCreateSubAssemblyJobs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Processes the tt to create the jobs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.
    
    FOR EACH ttSubAssemblyJob,
    FIRST job-hdr:
        RUN pCreateSubAssemblyJob(ttSubAssemblyJob.riEst, ttSubAssemblyJob.riJobHdr, OUTPUT oplError, INPUT-OUTPUT iopcMessage).
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER, ipcEstTypeID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimate type number and typeID, determine if Set.
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cEstType AS CHARACTER NO-UNDO.

    cEstType = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", ipiEstType, ipcEstTypeID).	
    RETURN DYNAMIC-FUNCTION("fEstimate_IsSetType", cEstType).

		
END FUNCTION.



