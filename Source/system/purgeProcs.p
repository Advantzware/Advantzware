
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : purgeProcs.p
    Purpose     : Structured Procedure for purging records; created as super procedure
    Syntax      : RUN Purge (<quoted-table-name>,<record-rowid>, output lSuccess, output cMessage)
                  RUN prePurge (<quoted-table-name>,<record-rowid>, output lSuccess, output cMessage)
    Description : Purge procedure creates lists and deletes, prePurge ONLY creates lists
    Author(s)   : MYT
    Created     : Thu Sep 26 07:02:59 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING system.sharedConfig.

DEFINE STREAM datafiles.
DEFINE STREAM listfile.
DEFINE STREAM sReftable.

DEF VAR cOutDir AS CHAR NO-UNDO.
DEF VAR cListFile AS CHAR NO-UNDO.
DEF VAR lPurge AS LOG NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR locode AS CHAR NO-UNDO.
DEF VAR lVerbose AS LOG NO-UNDO.

DEFINE VARIABLE hdOutputProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE hdPurgeProcs  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHAR  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fStartPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStartPurge Procedure
FUNCTION fStartPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pfWriteLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfWriteLine Procedure
FUNCTION pfWriteLine RETURNS LOGICAL PRIVATE
  ( INPUT cTable AS CHAR, INPUT cNames AS CHAR, INPUT cData AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF




/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pDeleteJobRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteJobRecords Procedure
PROCEDURE pDeleteJobRecords PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Delete the job and its child tables 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob               AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableList         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge             AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplLogChildRecords   AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplCalledFromTrigger AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE hdBuffer      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTempTable   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTTBuffer    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdCompany     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJob         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJobNo       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJobNo2      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobHdrRefTbl AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iAuditId      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE scInstance    AS CLASS system.SharedConfig NO-UNDO.
    
    scInstance = SharedConfig:instance.
    
    cJobHdrRefTbl = "JOB-HDR01,JOB-HDR02,JOB-HDR03,JOB-HDR04".  

    DO iCount = 1 TO NUM-ENTRIES(ipcTableList):
        cTableName = ENTRY(iCount,ipcTableList).
        CREATE BUFFER hdBuffer FOR TABLE cTableName.
        CREATE QUERY hdQuery.
        
        hdTempTable = HANDLE(scInstance:GetValue("JobPurge-" + TRIM(cTableName))) NO-ERROR.
        IF NOT VALID-HANDLE(hdTempTable) THEN DO:      
            CREATE TEMP-TABLE hdTempTable.      
            hdTempTable:CREATE-LIKE(hdBuffer).
            hdTempTable:TEMP-TABLE-PREPARE(cTableName).
            
            /*Store Dynamic Temp-table handle in shared config object, later used in util/wjobPurge.w */
            scInstance:SetValue("JobPurge-" + TRIM(cTableName),STRING(hdTempTable)).
        END.   
        hdTTBuffer = hdTempTable:DEFAULT-BUFFER-HANDLE.      
                            
        ASSIGN 
            hdCompany = hdBuffer:BUFFER-FIELD("Company")
            hdJob     = hdBUffer:BUFFER-FIELD("job")
            hdJobNo   = hdBuffer:BUFFER-FIELD("job-no")
            hdJobNo2  = hdBuffer:BUFFER-FIELD("job-no2")
            NO-ERROR.
            
        /* IF none of the above fields are available or if only company field is available then skip*/    
        IF (hdCompany   EQ ? AND hdJob    EQ ? 
            AND hdJobNo EQ ? AND hdJobNo2 EQ ?) OR 
           (hdCompany   NE ? AND hdJob    EQ ?
            AND hdJobNo EQ ? AND hdJobNo2 EQ ?) THEN 
            NEXT.
             
        cQueryString = "FOR EACH " + cTableName + " NO-LOCK WHERE "       
                       + (IF hdCompany NE ? THEN cTableName + ".company EQ " + QUOTER(ipcCompany) ELSE "")
                       + (IF hdJob NE ? THEN (IF hdCompany NE ? THEN " AND " + cTableName + ".job EQ " + (IF hdJob:DATA-TYPE EQ "CHARACTER" THEN QUOTER(ipiJob) ELSE STRING(ipiJob)) 
                          ELSE cTableName + ".job EQ " + (IF hdJob:DATA-TYPE EQ "CHARACTER" THEN QUOTER(ipiJob) ELSE STRING(ipiJob))) ELSE "")
                       + (IF hdJobNo NE ? THEN (IF hdcompany NE ? OR hdjob NE ? THEN " AND " +  cTableName + ".job-no EQ " + QUOTER(ipcJobNo) ELSE cTableName + ".job-no EQ " + QUOTER(ipcJobNo) ) ELSE "")
                       + (IF hdjobNo2 NE ? THEN (IF hdcompany NE ? OR hdJob NE ? OR hdJobNo2 NE ? THEN " AND " + cTableName + ".job-no2 EQ " + STRING(ipiJobNo2)  ELSE cTableName + ".job-no2 EQ " + STRING(ipiJobNo2))  ELSE "")
                       . 
                            
        hdQuery:ADD-BUFFER(hdBuffer).
        hdQuery:QUERY-PREPARE(cQueryString).
        hdQuery:QUERY-OPEN().
        hdQuery:GET-FIRST().
        
        IF NOT iplCalledFromTrigger THEN 
            hdBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
            
        IF iplPurge AND NOT iplCalledFromTrigger THEN 
            OUTPUT STREAM datafiles TO VALUE (cOutDir + "\DataFiles\" + cTableName + ".d") APPEND.
               
        DO WHILE NOT hdQuery:QUERY-OFF-END:                
            IF cTableName = "job-hdr" AND iplPurge THEN DO:
                DO iIndex = 1 TO NUM-ENTRIES(cJobHdrRefTbl):
                    FOR EACH reftable EXCLUSIVE-LOCK
                        WHERE reftable.reftable EQ ENTRY(iIndex,cJobHdrRefTbl) + ipcCompany
                          AND reftable.code2    EQ hdBuffer:BUFFER-FIELD ("j-no"):BUFFER-VALUE:
                        EXPORT STREAM sReftable reftable.                                
                        DELETE reftable.
                    END.
                END.
            END.
            IF hdBuffer:NAME EQ "job" AND hdBuffer:BUFFER-FIELD("exported"):BUFFER-VALUE THEN DO:
                hdQuery:GET-CURRENT(EXCLUSIVE-LOCK).
                hdBuffer:BUFFER-FIELD("stat"):BUFFER-VALUE= "X".
                RUN jc/kiwiexp2.p (hdBuffer:RECID).
            END. 
             
            /* Log Child Records*/  
            IF iplLogChildRecords OR (hdBuffer:NAME EQ "job" AND NOT iplCalledFromTrigger) THEN DO:
                hdTTBuffer:BUFFER-CREATE().
                hdTTBuffer:BUFFER-COPY(hdBuffer).
            END.    
            IF iplPurge THEN DO:
                IF NOT iplCalledFromTrigger THEN DO:
                    IF hdBuffer:NAME EQ "job" THEN 
                        RUN Session_CreateAuditHistory(
                            INPUT "DELETE",
                            INPUT "ASI",
                            INPUT hdBUffer
                            ).          
                    PUT STREAM datafiles UNFORMATTED DYNAMIC-FUNCTION("DynExport" IN hdPurgeProcs,hdBuffer," ") SKIP.
                END.
                hdQuery:GET-CURRENT(EXCLUSIVE-LOCK).
                hdBuffer:BUFFER-DELETE().    
                hdBuffer:BUFFER-RELEASE(). 
            END.   
            hdQuery:GET-NEXT().     
        END.
        
        IF iplPurge AND NOT iplCalledFromTrigger THEN  
            OUTPUT STREAM datafiles CLOSE.  
         
        /* hdTempTable handle deletion in done in util/w-purge.w, do not delete it here */   
        IF VALID-HANDLE(hdQuery) THEN 
            DELETE OBJECT hdQuery. 
        IF VALID-HANDLE(hdBuffer) THEN 
            DELETE OBJECT hdBuffer.                               
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Purge_SimulateAndPurgeJobRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Purge_SimulateAndPurgeJobRecords Procedure
PROCEDURE Purge_SimulateAndPurgeJobRecords:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ip-bf-job            FOR job.
    DEFINE INPUT  PARAMETER iplPurge             AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplgLogChildRecords  AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplCalledFromTrigger AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER INITIAL "OK".
    
    DEFINE BUFFER bf-job FOR job.
    
    DEFINE VARIABLE cTableList       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrphanTableList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurgeDirectory  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    RUN util/PurgeProcs.p    PERSISTENT SET hdPurgeProcs.
            
    ASSIGN     
        cTableList       = "job-hdr,job-mat,job-mch,job-prep,job-farm,job-farm-rctd,mat-act,mch-act,misc-act"        
        cOrphanTableList = "sbStatus,sbNote,jobMatl,jobMach,sbJob,jobItems,jobStack,jobSheet,jobCad,jobPrep," +
                           "jobNotes,jobs,asi2corr,corr2asi,job-all,job-brd"
        .
        
    IF NOT iplCalledFromTrigger THEN DO:
        /*If not called from trigger then delete the job in the end*/       
        cOrphanTableList = cOrphanTableList + ",job".                                          
    END.        
        
    DO TRANSACTION:
        IF iplPurge THEN DO:
            IF NOT iplCalledFromTrigger THEN DO:
                RUN jc/jc-dall.p (RECID(ip-bf-job)).
                OUTPUT STREAM sReftable TO VALUE(cOutDir + "\DataFiles\reftable.d") APPEND.
            END.    
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company   EQ ip-bf-Job.company
                 AND reftable.loc       EQ ""
                 AND reftable.code      EQ STRING(ip-bf-Job.job,"999999999"):
                EXPORT STREAM sReftable reftable.     
                DELETE reftable.
            END.
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "job.create-time"
                  AND reftable.company  EQ ip-bf-Job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(ip-bf-Job.job,"9999999999"):
                EXPORT STREAM sReftable reftable.                      
                DELETE reftable.
            END.
            
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "job.qty-changed"
                  AND reftable.company  EQ ip-bf-Job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(ip-bf-Job.job,"9999999999"):
                EXPORT STREAM sReftable reftable.                      
                DELETE reftable.
            END.
        END.
        RUN pDeleteJobRecords(
            INPUT ip-bf-job.company,
            INPUT ip-bf-job.job,
            INPUT ip-bf-job.job-no,
            INPUT ip-bf-job.job-no2,
            INPUT IF iplCalledFromTrigger THEN cTableList ELSE cTableList + "," + cOrphanTableList, /* Table List */
            INPUT iplPurge,              /* Purge records? */   
            INPUT iplgLogChildRecords,   /* Create .csv files for child tables ? */
            INPUT iplCalledFromTrigger   /* Called from trigger? */
            ).
        IF iplPurge AND NOT iplCalledFromTrigger THEN
            OUTPUT STREAM sReftable CLOSE.                              
    END. /* Transaction */    
    PROCESS EVENTS.
    
    IF VALID-HANDLE(hdOutputProcs) THEN 
        DELETE PROCEDURE hdOutputProcs.
        
    IF VALID-HANDLE(hdPurgeProcs) THEN 
        DELETE PROCEDURE hdPurgeProcs.    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    pfWriteLIne(cTable, "End", "").
    OUTPUT STREAM listfile CLOSE.

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHAR  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE crOutDir AS CHARACTER NO-UNDO.
    
    RUN FileSys_GetTempDirectory(
        OUTPUT cOutDir
        ).
    
    ASSIGN 
        cOutDir = cOutDir + "\" + cTable + "purge" + STRING(YEAR(TODAY),"9999") + 
                                          STRING(MONTH(TODAY),"99") + 
                                          STRING(DAY(TODAY),"99") + 
                                          "-" + STRING(TIME)
        cListFile = cOutDir + "\purgelist.txt"
        crOutDir = cOutDir.

    RETURN crOutDir.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fStartPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStartPurge Procedure
FUNCTION fStartPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    OS-CREATE-DIR VALUE(cOutDir).
    OUTPUT STREAM listfile TO VALUE(cListFile).

    pfWriteLine("job", "Start", "").

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pfWriteLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfWriteLine Procedure
FUNCTION pfWriteLine RETURNS LOGICAL PRIVATE
    ( INPUT cTable AS CHAR, INPUT cNames AS CHAR, INPUT cData AS CHAR  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cOutline AS CHAR NO-UNDO.
    DEF VAR cTime AS CHAR NO-UNDO.
    DEF VAR cAction AS CHAR NO-UNDO.
        
    ASSIGN 
        cTime = STRING(TIME,"HH:MM:SS") + "  "
        cAction = IF lPurge THEN "Deleting " ELSE "Simulate deleting ".
           
    IF cNames EQ "Start" THEN 
    DO:
        ASSIGN 
            cOutLine = cTime + "Beginning " + cTable + " purge.".
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE IF cNames EQ "End" THEN 
    DO:
        ASSIGN 
            cOutLine = cTime + "Ending " + cTable + " purge.".
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE IF cNames EQ "Head" THEN DO:
        ASSIGN 
            cOutLine = cTime + cAction + "records for " + cTable + ": " + cData.
        PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
    END.
    ELSE 
    DO:
        IF lVerbose THEN DO:
            ASSIGN 
                cOutline = cTime + "   " + cAction + cTable + " WHERE ".
            DO iCtr = 1 TO NUM-ENTRIES(cNames,"|"):
                ASSIGN 
                    cOutline = cOutline + ENTRY(iCtr,cNames,"|") + "=" + ENTRY(iCtr,cData,"|") + " AND ".
            END.
            ASSIGN 
                cOutline = SUBSTRING(cOutline, 1, LENGTH(cOutline) - 5) + ".".
            PUT STREAM listfile UNFORMATTED cOutline + CHR(10).
        END. 
    END.
         
    RETURN TRUE.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

