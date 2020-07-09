
/*------------------------------------------------------------------------
    File        : BuildJobTester.p
    Purpose     : 

    Syntax      :

    Description : Tools for Testing Build Job

    Author(s)   : BV
    Created     : Wed Jun 10 20:53:57 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttJobHdr LIKE job-hdr.
DEFINE TEMP-TABLE ttJobMat LIKE job-mat.
DEFINE TEMP-TABLE ttJobMch LIKE job-mch.
DEFINE TEMP-TABLE ttJobPrep LIKE job-prep.
DEFINE TEMP-TABLE ttJobFarm LIKE job-farm.

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE lNew AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE cJobID AS CHARACTER NO-UNDO INITIAL 'W14347'.
DEFINE VARIABLE iJobID2 AS INTEGER NO-UNDO INITIAL 0.

/* ********************  Preprocessor Definitions  ******************** */
RUN system\session.p PERSISTENT SET hdSession.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdSession).
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutputProcs).

/* ***************************  Main Block  *************************** */

RUN pExportFiles(cCompany,cJobID,iJobID2).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pExport PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdTT AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcName AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempFolder AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cTempFolder = "C:\temp\"
        ipcName = (IF lNew THEN "New" ELSE "Old") + ipcName + TRIM(cJobID) + "-" + STRING(iJobID2,"99")
        //ipcName = ipcName + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
        .
    
    RUN Output_TempTableToCSV(iphdTT, cTempFolder + "\" + ipcName + ".csv", YES, NO, OUTPUT lError, OUTPUT cErrorMessage).
    
END PROCEDURE.

PROCEDURE pExportFiles PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job-no EQ ipcJobID
        AND job-hdr.job-no2 EQ ipiJobID2
        :
        CREATE ttJobHdr.
        BUFFER-COPY job-hdr TO ttJobHdr.
    END.
    RUN pExport(TEMP-TABLE ttJobHdr:HANDLE,"JobHdr").

    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ ipcCompany
        AND job-mat.job-no EQ ipcJobID
        AND job-mat.job-no2 EQ ipiJobID2
        :
        CREATE ttJobMat.
        BUFFER-COPY job-mat TO ttJobMat.
    END.
    RUN pExport(TEMP-TABLE ttJobMat:HANDLE,"JobMat").
    
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ ipcCompany
        AND job-mch.job-no EQ ipcJobID
        AND job-mch.job-no2 EQ ipiJobID2
        :
        CREATE ttJobMch.
        BUFFER-COPY job-mch TO ttJobMch.
    END.
    RUN pExport(TEMP-TABLE ttJobMch:HANDLE,"JobMch").
    
    FOR EACH job-prep NO-LOCK
        WHERE job-prep.company EQ ipcCompany
        AND job-prep.job-no EQ ipcJobID
        AND job-prep.job-no2 EQ ipiJobID2
        :
        CREATE ttJobPrep.
        BUFFER-COPY job-prep TO ttJobPrep.
    END.
    RUN pExport(TEMP-TABLE ttJobPrep:HANDLE,"JobPrep").
    
    FOR EACH job-farm NO-LOCK
        WHERE job-farm.company EQ ipcCompany
        AND job-farm.job-no EQ ipcJobID
        AND job-farm.job-no2 EQ ipiJobID2
        :
        CREATE ttJobFarm.
        BUFFER-COPY job-farm TO ttJobFarm.
    END.
    RUN pExport(TEMP-TABLE ttJobFarm:HANDLE,"JobFarm").
    
END PROCEDURE.

