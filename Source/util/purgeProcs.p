&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : purgeProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Wed Jun 24 09:43:52 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING system.sharedConfig.

DEFINE STREAM datafiles.
DEFINE STREAM listfile.
DEFINE STREAM sReftable.
DEFINE STREAM sDump.
DEFINE STREAM sReport.

{util/ttPurge.i}

DEFINE VARIABLE cOutDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPurge AS LOG NO-UNDO.
DEFINE VARIABLE iCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE VARIABLE locode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lVerbose AS LOG NO-UNDO.
DEFINE VARIABLE hOutputProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE cUserId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSecurityLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE cGroupList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPurgeList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgramList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtilName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHotkey AS CHARACTER NO-UNDO.
DEFINE VARIABLE ttCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE cParm AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cCompanyList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecKeyPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE cThisCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE hActnumField AS HANDLE NO-UNDO.
DEFINE VARIABLE hBNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hBolNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE hBufferField AS HANDLE NO-UNDO.
DEFINE VARIABLE hCompanyField AS HANDLE NO-UNDO.
DEFINE VARIABLE hCustNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hInoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hInvNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hLocField AS HANDLE NO-UNDO.
DEFINE VARIABLE hOrdNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hPoNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hJobField AS HANDLE NO-UNDO.
DEFINE VARIABLE hJobNoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hJobNo2Field AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hRecKey AS HANDLE NO-UNDO.
DEFINE VARIABLE hRnoField AS HANDLE NO-UNDO.
DEFINE VARIABLE hTestField AS HANDLE NO-UNDO.
DEFINE VARIABLE hXnoField AS HANDLE NO-UNDO.
DEFINE VARIABLE iErrorCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iProcessedCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iWarningCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  (INPUT hRecord AS HANDLE,
   INPUT cDelim AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-fDateString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateString Procedure
FUNCTION fDateString RETURNS CHARACTER 
  (INPUT iYrOffset AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetDataDumpDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDataDumpDir Procedure
FUNCTION fGetDataDumpDir RETURNS CHARACTER 
  (INPUT ipcInitValue AS CHARACTER,
   INPUT ipcTable AS CHARACTER,
   OUTPUT oplError AS LOG,
   OUTPUT opcMessage AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHARACTER  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fStartPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStartPurge Procedure
FUNCTION fStartPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pfWriteLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfWriteLine Procedure
FUNCTION pfWriteLine RETURNS LOGICAL PRIVATE
  ( INPUT cTable AS CHARACTER, INPUT cNames AS CHARACTER, INPUT cData AS CHARACTER ) FORWARD.

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

&IF DEFINED(EXCLUDE-analyzePurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE analyzePurge Procedure
PROCEDURE analyzePurge:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputDir AS CHAR.
    DEFINE INPUT PARAMETER ipcAnalysisFile AS CHAR.
    DEFINE INPUT PARAMETER TABLE FOR ttParmsByPurge.
    DEFINE INPUT PARAMETER ipcCompanyList AS CHAR.
    DEFINE INPUT PARAMETER iplOrphanPurge AS LOG.
    DEFINE VARIABLE cKeyList AS CHAR.    
    DEFINE VARIABLE lCreateOK AS LOG NO-UNDO.
    /* Make sure the target directory is created */
    RUN filesys_createDirectory (ipcOutputDir, OUTPUT lCreateOK, OUTPUT cMessage).
    IF NOT lCreateOK THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    /* Write a file containing the parameters selected for this run */
    OUTPUT TO VALUE(ipcOutputDir + "\PurgeParms.csv").
    FIND FIRST ttParmsByPurge.
    PUT UNFORMATTED ttParmsByPurge.ttcPurge + CHR(10).
    PUT UNFORMATTED "Label,Begin Value,End Value" + CHR(10).
    FOR EACH ttParmsByPurge:
        PUT UNFORMATTED 
            ttParmsByPurge.ttcLabel + "," +
            ttParmsByPurge.ttcStartValue + "," +
            ttParmsByPurge.ttcEndValue + CHR(10).
        IF ttParmsByPurge.ttlRange EQ TRUE THEN ASSIGN 
            cKeyList = cKeyList + ttParmsByPurge.ttcLabel + ",".
        ELSE ASSIGN 
            cKeyList = cKeyList + ",".
    END.
    PUT UNFORMATTED "This/All Companies" + "," + ipcCompanyList + CHR(10).
    PUT UNFORMATTED "Include Orphan Purge" + "," + STRING(iplOrphanPurge) + CHR(10).
    OUTPUT CLOSE.
    
    /* Use the temp-table to determine which analysis to run and run it */
    FIND FIRST ttParmsByPurge.
    CASE ttParmsByPurge.ttcPurge:
        WHEN "Purge Jobs (NF!)" THEN RUN pAnalyzeJobs (INPUT ipcCompanyList, INPUT iplOrphanPurge).
        WHEN "Purge POs (NF9)" THEN RUN pAnalyzePOs (INPUT ipcCompanyList, INPUT iplOrphanPurge).
    END CASE.

    OUTPUT TO VALUE(ipcOutputDir + "\PurgeList.csv").
    PUT UNFORMATTED "Table,Company," + cKeyList + "Rowid" + CHR(10). 
    FOR EACH ttPurgeList:
        PUT UNFORMATTED 
            ttPurgeList.cTable + "," +
            ttPurgeList.cCompany + "," +
            ttPurgeList.cKey1 + "," +
            ttPurgeList.cKey2 + "," +
            ttPurgeList.cKey3 + "," +
            ttPurgeList.cKey4 + "," +
            ttPurgeList.cKey5 + "," +
            STRING(ttPurgeList.rRowid) + CHR(10).
    END.
    OUTPUT CLOSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pAnalyzePOs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAnalyzePOs Procedure
PROCEDURE pAnalyzePOs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompanies AS CHAR.
    DEFINE INPUT PARAMETER iplOrphans AS LOG.
    
    DEFINE VARIABLE iBeginPoNo AS INT NO-UNDO.
    DEFINE VARIABLE iEndPoNo AS INT NO-UNDO.
    DEFINE VARIABLE daBeginPoEnterDate AS DATE NO-UNDO.
    DEFINE VARIABLE daEndPoEnterDate AS DATE NO-UNDO.
    DEFINE VARIABLE cBeginVendNo AS CHAR NO-UNDO.
    DEFINE VARIABLE cEndVendNo AS CHAR NO-UNDO.
    DEFINE VARIABLE lLinked AS LOG NO-UNDO.
    DEFINE VARIABLE lOnlyClosed AS LOG NO-UNDO.
    
    FOR EACH ttParmsByPurge:
        IF ttParmsByPurge.ttcParm EQ "po-no" THEN ASSIGN
            iBeginPoNo = INTEGER(ttParmsByPurge.ttcStartValue)
            iEndPoNo = INTEGER(ttParmsByPurge.ttcEndValue).
        IF ttParmsByPurge.ttcParm EQ "po-date" THEN ASSIGN
            daBeginPoEnterDate = DATE(ttParmsByPurge.ttcStartValue)
            daEndPoEnterDate = DATE(ttParmsByPurge.ttcEndValue).
        IF ttParmsByPurge.ttcParm EQ "vend-no" THEN ASSIGN
            cBeginVendNo = ttParmsByPurge.ttcStartValue
            cEndVendNo = ttParmsByPurge.ttcEndValue.
    END. 
              
    EMPTY TEMP-TABLE ttPurgeList.
    
    FOR EACH company NO-LOCK WHERE
        company.company EQ (IF ipcCompanies EQ "*" THEN company.company ELSE ipcCompanies):
        FOR EACH po-ord NO-LOCK WHERE 
            po-ord.company EQ company.company AND
            po-ord.po-no GE iBeginPoNo AND po-ord.po-no LE iEndPoNo AND 
            po-ord.po-date GE daBeginPoEnterDate AND po-ord.po-date LE daEndPoEnterDate AND 
            po-ord.vend-no GE cBeginVendNo AND po-ord.vend-no LE cEndVendNo:
            CREATE ttPurgeList.
            ASSIGN 
                ttPurgeList.cTable = "po-ord"
                ttPurgeList.cCompany = po-ord.company
                ttPurgeList.rRowid = ROWID(po-ord)
                ttPurgeList.cKey1 = STRING(po-ord.po-no)
                ttPurgeList.cKey2 = STRING(po-ord.po-date)
                ttPurgeList.cKey3 = STRING(po-ord.vend-no,"x(24)")
                ttPurgeList.cKey4 = ""
                ttPurgeList.cKey5 = ""
                .
            FOR EACH po-ordl NO-LOCK WHERE
                po-ordl.company EQ po-ord.company AND 
                po-ordl.po-no EQ po-ord.po-no: 
                CREATE ttPurgeList.
                ASSIGN 
                    ttPurgeList.cTable = "po-ordl"
                    ttPurgeList.cCompany = po-ord.company
                    ttPurgeList.rRowid = ROWID(po-ordl)
                    ttPurgeList.cKey1 = STRING(po-ord.po-no)
                    ttPurgeList.cKey2 = STRING(po-ord.po-date)
                    ttPurgeList.cKey3 = STRING(po-ord.vend-no,"x(24)")
                    ttPurgeList.cKey4 = ""
                    ttPurgeList.cKey5 = ""
                    .
            END.
            FOR EACH po-all NO-LOCK WHERE
                po-all.company EQ po-ord.company AND 
                po-all.po-no EQ po-ord.po-no: 
                CREATE ttPurgeList.
                ASSIGN 
                    ttPurgeList.cTable = "po-all"
                    ttPurgeList.cCompany = po-ord.company
                    ttPurgeList.rRowid = ROWID(po-all)
                    ttPurgeList.cKey1 = STRING(po-ord.po-no)
                    ttPurgeList.cKey2 = STRING(po-ord.po-date)
                    ttPurgeList.cKey3 = STRING(po-ord.vend-no,"x(24)")
                    ttPurgeList.cKey4 = ""
                    ttPurgeList.cKey5 = ""
                    .
            END.
            FOR EACH po-ordl-add NO-LOCK WHERE
                po-ordl-add.company EQ po-ord.company AND 
                po-ordl-add.po-no EQ po-ord.po-no: 
                CREATE ttPurgeList.
                ASSIGN 
                    ttPurgeList.cTable = "po-ordl-add"
                    ttPurgeList.cCompany = po-ord.company
                    ttPurgeList.rRowid = ROWID(po-ordl-add)
                    ttPurgeList.cKey1 = STRING(po-ord.po-no)
                    ttPurgeList.cKey2 = STRING(po-ord.po-date)
                    ttPurgeList.cKey3 = STRING(po-ord.vend-no,"x(24)")
                    ttPurgeList.cKey4 = ""
                    ttPurgeList.cKey5 = ""
                    .
            END.
        END. 
    END.       
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBuildParmsByPurgeTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildParmsByPurgeTT Procedure
PROCEDURE pBuildParmsByPurgeTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttParmsByPurge.
    DO ttCtr = 1 TO 10:
        IF cParm[ttCtr] EQ "" THEN LEAVE.
        CREATE ttParmsByPurge.
        ASSIGN 
            ttParmsByPurge.ttcPurge = ENTRY(1,cParm[ttCtr])
            ttParmsByPurge.ttcParm = ENTRY(2,cParm[ttCtr])
            ttParmsByPurge.ttcLabel = ENTRY(3,cParm[ttCtr])
            ttParmsByPurge.ttcDataType = ENTRY(4,cParm[ttCtr])
            ttParmsByPurge.ttcFormat = ENTRY(5,cParm[ttCtr])
            ttParmsByPurge.ttlRange = LOGICAL(ENTRY(6,cParm[ttCtr]))
            ttParmsByPurge.ttcStartValue = ENTRY(7,cParm[ttCtr])
            ttParmsByPurge.ttcEndValue = ENTRY(8,cParm[ttCtr])
        .
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-BuildPurgesByGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildPurgesByGroup Procedure
PROCEDURE pBuildPurgesByGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO ttCtr = 1 TO NUM-ENTRIES(cGroupList):
        CREATE ttPurgeByGroup.
        ASSIGN 
            ttPurgeByGroup.ttcGroup = ENTRY(ttCtr,cGroupList)
            ttPurgeByGroup.ttcPurge = ENTRY(ttCtr,cPurgeList)
            ttPurgeByGroup.ttcMasterTable = ENTRY(ttCtr,cTableList).
            
        IF INDEX(ttPurgeByGroup.ttcPurge,"(") NE 0 THEN DO:
            ASSIGN 
                cHotkey = SUBSTRING(ttPurgeByGroup.ttcPurge,INDEX(ttPurgeByGroup.ttcPurge,"(") + 1)
                cHotkey = REPLACE(cHotkey,")","")
                cUtilName = SUBSTRING(ttPurgeByGroup.ttcPurge,1,INDEX(ttPurgeByGroup.ttcPurge,"(") - 2).
            FIND FIRST prgrms NO-LOCK WHERE 
                prgrms.mnemonic EQ cHotkey
                NO-ERROR.
            IF AVAILABLE prgrms THEN ASSIGN 
                ttPurgeByGroup.ttcUserList = IF prgrms.can_run NE "" THEN prgrms.can_run ELSE "*"
                ttPurgeByGroup.ttiSecurityLevel = prgrms.securityLevelUser.
            ELSE DO:
                FIND FIRST utilities NO-LOCK WHERE 
                    utilities.description EQ cUtilName
                    NO-ERROR.
                ASSIGN 
                    ttPurgeByGroup.ttcUserList = "*"
                    ttPurgeByGroup.ttiSecurityLevel = IF AVAILABLE utilities THEN utilities.securityLevel ELSE 900.
            END. 
                    
        END.
    END.        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-checkPurgeSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkPurgeSecurity Procedure
PROCEDURE checkPurgeSecurity:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPurgeName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    
    FIND FIRST ttPurgeByGroup WHERE 
        ttPurgeByGroup.ttcPurge EQ ipcPurgeName
        NO-ERROR.
    IF AVAILABLE ttPurgeByGroup THEN DO:
        IF NOT CAN-DO(ttPurgeByGroup.ttcUserList, cUserID) 
        OR iSecurityLevel LT ttPurgeByGroup.ttiSecurityLevel THEN ASSIGN 
            oplError = TRUE. 
    END.    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getCurrentUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCurrentUser Procedure
PROCEDURE getCurrentUser:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    
    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.
    IF NOT AVAILABLE users 
    THEN DO:
        /* Testing only */
        ASSIGN 
        cUserId = "mark"
        iSecurityLevel = 1000.
        
/*        ASSIGN              */
/*            oplError = TRUE.*/
/*        RETURN.             */
    END.
    ELSE ASSIGN 
        cUserId = users.user_id
        iSecurityLevel = users.securityLevel.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-GetPurgeListByGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPurgeListByGroup Procedure
PROCEDURE GetPurgeListByGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcGroupName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcPurgeList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCanPurge AS LOG NO-UNDO.
    
    FOR EACH ttPurgeByGroup WHERE 
        ttPurgeByGroup.ttcGroup EQ ipcGroupName:
            
        RUN checkPurgeSecurity (INPUT ttPurgeByGroup.ttcPurge, OUTPUT lCanPurge).
        IF NOT lCanPurge THEN ASSIGN 
            ipcPurgeList = ipcPurgeList + ttPurgeByGroup.ttcPurge + ",".
    END.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-initializeProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeProc Procedure
PROCEDURE initializeProc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUserError AS LOG NO-UNDO.
    
    ASSIGN
        cGroupList = 
            "EST,EST,EST" + "," +
            "ORD,ORD,ORD,ORD,ORD,ORD,ORD" + "," +
            "INV,INV,INV,INV,INV,INV,INV,INV" + "," +
            "JOB,JOB,JOB,JOB,JOB,JOB,JOB" + "," +
            "PUR,PUR" + "," +
            "FIN,FIN,FIN" + "," +
            "SYS,SYS,SYS,SYS,SYS,SYS"
        cPurgeList =
            "Purge Board in Style File" + "," +
            "Purge Quotes (NF8)" + "," +
            "Purge Old eItem Records (NM)" + "," +
            
            "Purge Zero Posted A/R (NF6)" + "," +
            "Purge Customers (NY))" + "," +
            "Purge Orders (NF7)" + "," +
            "Purge Purge Releases (NY9)" + "," +
            "Purge Paid A/R Invoices (NF#)" + "," +
            "Purge Orders Utility (NM)" + "," +
            "Purge Orders with no Lines (NM)" + "," +
            
            "Purge Finished Goods (NF1)" + "," +
            "Purge FG History" + "," +
            "Purge fg-rctd Records" + "," +
            "Purge Rqw Materials (NY3)" + "," +
            "FG History Consolidation (NM)" + "," +
            "Purge SS Load Tags (NM)" + "," +
            "Purge FG Spec Notes (NM)" + "," +
            "Purge FG Txns by Type (NM)" + "," +
            
            "Purge Employee Transactions (TF#)" + "," +
            "Purge Zero Time Transactions (TF%)" + "," +
            "Purge Touch Data" + "," +
            "Purge Jobs (NF!)" + "," +
            "Purge History by Job (NM)" + "," +
            "Purge Job Records with no Headers (NM)" + "," +
            "Job Purge Program (NM)" + "," +
            
            "Purge POs (NF9)" + "," +
            "Purge Paid A/P Invoices (NF$)" + "," +
            
            "Purge GL HIstory Details" + "," +
            "Consolidate GL Transactions" + "," +
            "Purge Adjustments" + "," +
            
            "Purge Report File (NY2)" + "," +
            "Purge Contacts" + "," +
            "Purge Notes" + "," +
            "Purge Audit History (NM)" + "," +
            "Purge Blank Record Keys (NM)" + "," +
            "Purge Orphan Records (NM)"
            .
    ASSIGN 
        cTableList =             
            "" + "," +
            "" + "," +
            "" + "," +
            
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            
            "" + "," +
            "" + "," +
            "" + "," +
            "job" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            
            "po" + "," +
            "" + "," +
            
            "" + "," +
            "" + "," +
            "" + "," +
            
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            ""
            .

    RUN getCurrentUser (OUTPUT lUserError).
    IF lUserError THEN DO:
        MESSAGE 
            "Failure to locate user in users table."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END. 
    
    RUN pBuildPurgesByGroup.
    
    RUN BuildDetailedPurgeParms ("Purge Jobs (NF!)").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-BuildDetailedPurgeParms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildDetailedPurgeParms Procedure
PROCEDURE BuildDetailedPurgeParms:
/*------------------------------------------------------------------------------
 Purpose:   Create Parms for additional purges here
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPurgeName AS CHARACTER NO-UNDO.

/*  The format of these parameter records is as follows:*/
/*      Purge Name (from NS8 or NM)                 */
/*      key field name                              */
/*      label                                       */
/*      data type                                   */
/*      data format                                 */
/*      range (YES) or fixed item (NO)              */
/*      beginning value (not required if fixed item)*/
/*      ending value                                */
            
    ASSIGN 
        cParm = "".

    CASE ipcPurgeName:
        WHEN "Purge Jobs (NF!)" THEN ASSIGN 
            cParm[1] = "Purge Jobs (NF!),close-date,Job CLOSE date is Less Than,Date,99/99/9999,NO,''," + fDateString(1)
            cParm[2] = "Purge Jobs (NF!),job-no,Job Range,Character,x(9),YES,0,9999999".
        WHEN "Purge POs (NF9)" THEN ASSIGN 
            cParm[1] = "Purge POs (NF9),po-no,PO Number,Integer,>>>>>>>9,YES,0,99999999"
            cParm[2] = "Purge POs (NF9),po-date,PO entered date,Date,99/99/9999,YES," + fDateString(40) + "," + fDateString(3)
            cParm[3] = "Purge POs (NF9),vend-no,Vendor,Character,x(20),YES,,zzzzzzzzzzzz"
            cParm[4] = "Purge POs (NF9),purgeIfLinked,Purge POs if Linked to Invoices/Receipts,logical,Yes/No,NO,,No"
            cParm[5] = "Purge POs (NF9),purgeClosedOnly,Purge ONLY Closed POs,logical,Yes/No,NO,,Yes"
            .
    END.

    RUN pBuildParmsByPurgeTT.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF



&IF DEFINED(EXCLUDE-outputGLaccountFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputGLaccountFile Procedure
PROCEDURE outputGLaccountFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputDir AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarning AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAmount AS DECIMAL NO-UNDO.
    
    OUTPUT STREAM sReport TO VALUE (ipcOutputDir + "\" + ipcFileName).
    
    PUT STREAM sReport UNFORMATTED 
        "Warning,Year,Period,Account,Journal,Txn Date,Amount,Currency,Description,Type,Created By,FileName,RecKey,Rowid" + CHR(10).
    FOR EACH ttGLHistList
        BREAK BY ttGLHistList.iYear
        BY ttGLHistList.iPeriod
        BY ttGLHistList.cAccount
        BY ttGLHistList.daTxnDate:
        
        IF FIRST-OF(ttGLHistList.cAccount) THEN
        DO:         
            cWarning = "".
            cAmount = 0.
            FIND FIRST account NO-LOCK
                 WHERE account.company EQ ipcCompany 
                 AND account.actnum EQ ttGLHistList.cAccount NO-ERROR .
            IF NOT AVAILABLE account THEN
            cWarning = " Invalid Account number".
            ELSE IF account.inactive THEN
            cWarning = " Account is inactive".              
        END. 
        
        cAmount = cAmount + ttGLHistList.deAmount.
        IF LAST-OF(ttGLHistList.cAccount) THEN
        DO:     
         PUT STREAM sReport UNFORMATTED
            cWarning + "," +
            STRING(ttGLHistList.iYear,"9999") + "," +
            STRING(ttGLHistList.iPeriod,"99") + "," +
            ttGLHistList.cAccount + "," +
            ttGLHistList.cJournal + "," +
            STRING(ttGLHistList.daTxnDate,"99/99/99") + "," +
            STRING(cAmount,"->>>>>>>>9.99") + "," +
            ttGLHistList.cCurrency + "," +
            REPLACE(ttGLHistList.cDescription,","," ") + "," +
            ttGLHistList.cType + "," +
            ttGLHistList.cCreatedBy + "," +
            ttGLHistList.cFileName + "," +   
            ttGLHistList.cReckey + ","
            STRING(ttGLHistList.rRowID) +
            CHR(10).
        END.    
    END.
    OUTPUT STREAM sReport CLOSE.
    EMPTY TEMP-TABLE ttFileList.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


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
                        IF iplPurge AND NOT iplCalledFromTrigger THEN
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
                    PUT STREAM datafiles UNFORMATTED DynExport(hdBuffer," ") SKIP.
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


&IF DEFINED(EXCLUDE-pDeleteRecordsByRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteRecordsByRowid Procedure
PROCEDURE pDeleteRecordsByRowid:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " NO-LOCK").
    hQuery:QUERY-OPEN().

    OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ipcFileName + ".d").
    FOR EACH ttPurgeList WHERE 
        ttPurgeList.cTable EQ ipcFileName TRANSACTION:
        hQuery:REPOSITION-TO-ROWID (ttPurgeList.rRowid).
        hBuffer:FIND-BY-ROWID(ttPurgeList.rRowid, EXCLUSIVE-LOCK).
        IF hBuffer:AVAILABLE THEN DO:
            PUT STREAM sDump UNFORMATTED dynExport(hBuffer, " ") SKIP.
            hBuffer:BUFFER-DELETE().
        END.
    END.
    OUTPUT STREAM sDump CLOSE.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pGetFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFieldList Procedure
PROCEDURE pGetFieldList:
/*------------------------------------------------------------------------------
 Purpose: Returns complete list of indexed fields for this table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER.
    DEFINE OUTPUT PARAMETER opcFieldList AS CHARACTER.

    FIND FIRST asi._file NO-LOCK WHERE 
    asi._file._file-name = ipcFileName 
        NO-ERROR.

    IF AVAILABLE asi._file THEN FOR EACH asi._index OF asi._file NO-LOCK, 
                                EACH asi._index-field OF asi._index NO-LOCK,
                                EACH asi._field OF asi._index-field NO-LOCK 
        BY asi._field._order:
        IF LOOKUP(_field._field-name,opcFieldList) EQ 0 THEN ASSIGN 
            opcFieldList = opcFieldList + asi._field._field-name + ",".
    END.

    ASSIGN 
        opcFieldList = TRIM(opcFieldList,",").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestGlhist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestGlhist Procedure
PROCEDURE pTestGlhist:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiYear AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPeriod AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE daPeriodStartDate AS DATE NO-UNDO.
    DEFINE VARIABLE daPeriodEndDate AS DATE NO-UNDO.
    DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
            
    DEFINE BUFFER bglHist FOR glHist.
    DEFINE BUFFER bPeriod FOR period.
    DEFINE BUFFER cPeriod FOR period.
    DEFINE BUFFER bf-glhist FOR glHist.
    
    FIND company NO-LOCK WHERE 
        company.company EQ ipcCompany
        NO-ERROR.
        
    FIND period NO-LOCK WHERE
        period.company EQ ipcCompany AND 
        period.yr EQ ipiYear AND 
        period.pnum EQ ipiPeriod
        NO-ERROR.
    /* There's not a period record for this year/period, so create one based on
       the latest year where this period number exists. Make sure the period we
       create here is CLOSED, as are all subledgers.  Let the create.trg assign rec_key. */
    IF NOT AVAILABLE period THEN DO:
        FIND LAST bPeriod NO-LOCK WHERE 
            bPeriod.company EQ ipcCompany AND 
            bPeriod.pnum EQ ipiPeriod
            NO-ERROR.
        FIND LAST cPeriod NO-LOCK WHERE 
            cPeriod.company EQ ipcCompany AND 
            cPeriod.pnum EQ IF ipiPeriod NE company.num-per THEN ipiPeriod + 1 ELSE 1 
            NO-ERROR.
        CREATE period.
        ASSIGN 
            period.company = ipcCompany
            period.yr = ipiYear
            period.pnum = bperiod.pnum
            period.pname = ""
            period.pst = DATE(MONTH(bperiod.pst), DAY(bperiod.pst), ipiYear) 
            /* This is tricky, because it has to account for Feb 29, so it gets the
               start date for the NEXT month, then subtracts 1 */       
            period.pend = DATE(MONTH(cperiod.pst), 1, ipiYear) - 1
            period.pstat = FALSE 
            period.subledgerAP = "C"
            period.subledgerPO = "C"
            period.subledgerOP = "C"
            period.subledgerWIP = "C"
            period.subledgerRM = "C"
            period.subledgerFG = "C"
            period.subledgerBR = "C"
            period.subledgerAR = "C"
            period.subledgerGL = "C"
            period.apClosedBy = USERID(LDBNAME(1))
            period.poClosedBy = USERID(LDBNAME(1))
            period.opClosedBy = USERID(LDBNAME(1))
            period.wipClosedBy = USERID(LDBNAME(1))
            period.rmClosedBy = USERID(LDBNAME(1))
            period.fgClosedBy = USERID(LDBNAME(1))
            period.brClosedBy = USERID(LDBNAME(1))
            period.arClosedBy = USERID(LDBNAME(1))
            period.glClosedBy = USERID(LDBNAME(1))
            period.apClosed = DATETIME(TODAY, MTIME)
            period.poClosed = DATETIME(TODAY, MTIME)
            period.opClosed = DATETIME(TODAY, MTIME)
            period.wipClosed = DATETIME(TODAY, MTIME)
            period.rmClosed = DATETIME(TODAY, MTIME)
            period.fgClosed = DATETIME(TODAY, MTIME)
            period.brClosed = DATETIME(TODAY, MTIME)
            period.arClosed = DATETIME(TODAY, MTIME)
            period.glClosed = DATETIME(TODAY, MTIME)
            .
    END.
    ASSIGN 
        daPeriodStartDate = period.pst
        daPeriodEndDate = period.pend.
    RELEASE period.
        
    FOR EACH bglhist NO-LOCK WHERE 
        bglhist.company EQ ipcCompany AND 
/*        bglhist.entryType NE "B" AND*/
            /* If the year and period are included in the gl-hist use them */ 
        (bglhist.yr EQ ipiYear AND bglhist.period EQ ipiPeriod) OR 
            /* or, if not, use the trans-date to find qualifying records.
               Note that we do NOT put the (assumed) year/period in the report. */
        (bglhist.yr EQ 0 AND bglhist.tr-date GE daPeriodStartDate AND bglhist.tr-date LE daPeriodEndDate):
            
        CREATE ttGLHistList.
        ASSIGN 
            ttGLHistList.lPosted         = YES
            ttGLHistList.iYear           = IF bglhist.glyear NE 0 THEN bglhist.glyear ELSE          /* First choice = record.glyear */ 
                                           IF bglhist.yr NE 0 THEN bglhist.yr ELSE                  /* Second choice = record.year */ 
                                           IF AVAILABLE period AND period.pnum NE 0 THEN period.yr ELSE /* Third choice = period.pnum */   
                                           YEAR(bglhist.tr-date)                                    /* finally, year of txn date */
            ttGLHistList.iPeriod         = bglhist.period
            ttGLHistList.cAccount        = bglhist.actnum
            ttGLHistList.cJournal        = bglhist.jrnl
            ttGLHistList.daTxnDate       = bglhist.tr-date
            ttGLHistList.deAmount        = IF bglhist.tr-amt EQ ? THEN 0 ELSE bglhist.tr-amt
            ttGLHistList.cCurrency       = bglhist.curr-code[1]
            ttGLHistList.cDescription    = bglhist.tr-dscr
            ttGLHistList.cType           = bglhist.entryType
            ttGLHistList.cCreatedBy      = bglhist.createdby
            ttGLHistList.cFileName       = "glhist"
            ttGLHistList.cReckey         = bglhist.rec_key
            ttGLHistList.rRowID          = ROWID(bglhist)
            .
        IF NOT bglhist.posted THEN
        DO:
           FIND FIRST bf-glhist EXCLUSIVE-LOCK
                WHERE ROWID(bf-glhist) EQ ROWID(bglhist) NO-ERROR.
           IF AVAILABLE bf-glhist THEN
           bf-glhist.posted = YES.
           RELEASE bf-glhist.
        END.
    END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestInvoice Procedure
PROCEDURE pTestInvoice:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyPrefix AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOG NO-UNDO.
    DEFINE VARIABLE lWarning AS LOG NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRule AS CHARACTER NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.

    ASSIGN
        hRecKey = ?
        hCompanyField = ?
        hCustNoField = ?
        hInvNoField = ?
        hXnoField = ?.
    
    /* Assign buffer-field names IF they are indexed fields */
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD(iCtr).
        /* Assign field names if exist in this table */        
        IF hTestField:NAME EQ "rec_key" THEN ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "company" THEN ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "cust-no" THEN ASSIGN
            hCustNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "inv-no" THEN ASSIGN
            hInvNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "x-no" THEN ASSIGN
            hXnoField = hBuffer:BUFFER-FIELD(iCtr).
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        (IF hCompanyField NE ? THEN (" AND " + ipcFileName + ".company EQ '" + cThisCompany + "'") ELSE "") + 
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        /* Company tests */
        IF hCompanyField NE ? THEN 
        DO:
            IF hCompanyField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Company"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST company WHERE 
                    company.company EQ hCompanyField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Company"
                        cMessage = cMessage + ",Company=" + hCompanyField:BUFFER-VALUE.  
        END.

        /* Cust-no tests */
        IF hCustNoField NE ? THEN 
        DO:
            IF hCustNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Cust-No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF hCustNoField:BUFFER-VALUE NE "" 
                    AND NOT CAN-FIND(FIRST cust WHERE 
                    cust.cust-no EQ hCustNoField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Cust-no"
                        cMessage = cMessage + ",Cust-no=" + hCustNoField:BUFFER-VALUE.
        END.

        /* Inv-no tests */
        IF hInvNoField NE ? THEN 
        DO:
            IF hInvNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank Inv No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF hInvNoField:BUFFER-VALUE NE 0
                    AND NOT CAN-FIND(FIRST inv-head WHERE 
                    inv-head.company EQ hCompanyField:BUFFER-VALUE AND 
                    inv-head.inv-no EQ hInvNoField:BUFFER-VALUE) 
                    AND NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND 
                    ar-inv.inv-no EQ hInvNoField:BUFFER-VALUE)THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid Inv No"
                        cMessage = cMessage + ",Inv-no=" + hInvNoField:BUFFER-VALUE. 
                 
        END.
        
        /* X-no tests */
        IF hXNoField NE ? THEN 
        DO:
            IF hXNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank X-no"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND     
                    ar-inv.x-no EQ hXNoField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid X-no"
                        cMessage = cMessage + ",X-no=" + hXNoField:BUFFER-VALUE.  
        END.
        
        IF lError 
            OR lWarning THEN 
        DO:
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName    = ipcFileName
                ttFileList.rRowID       = hBuffer:ROWID
                ttFileList.cRec_key     = hRecKey:BUFFER-VALUE
                ttFileList.cError       = IF lError THEN "Error" ELSE IF lWarning THEN "Warning" ELSE ""
                ttFileList.lPurge       = lError
                ttFileList.cRule        = SUBSTRING(cRule,2)
                ttFileList.cMessage     = SUBSTRING(cMessage,2)
                ttFileList.cKeyValues   = (IF hCompanyField NE ? THEN hCompanyField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hCustNoField NE ? THEN hCustNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hLocField NE ? THEN hLocField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hOrdNoField NE ? THEN hOrdNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hRNoField NE ? THEN hRNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBolNoField NE ? THEN hBolNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBNoField NE ? THEN hBNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hInvNoField NE ? THEN hInvNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hINoField NE ? THEN hINoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hXNoField NE ? THEN hXNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hActNumField NE ? THEN hActNumField:BUFFER-VALUE ELSE "")
                iErrorCount             = IF ttFileList.cError EQ "Error" THEN iErrorCount + 1 ELSE iErrorCount
                iWarningCount           = IF ttFileList.cError EQ "Warning" THEN iWarningCount + 1 ELSE iWarningCount
                .
                
        END.        
        ASSIGN 
            lError = FALSE 
            lWarning = FALSE  
            cRule = ""
            cMessage = ""
            iProcessedCount = iProcessedCount + 1.
        hQuery:GET-NEXT().
    END.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pTestBlankCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTestOneFile Procedure
PROCEDURE pTestOneFile:
/*------------------------------------------------------------------------------
 Purpose:   locates records with blank company code and adds to temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyPrefix AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOG NO-UNDO.
    DEFINE VARIABLE lWarning AS LOG NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRule AS CHARACTER NO-UNDO.
    
    /* Make a list of indexed fields in this table */
    FIND FIRST asi._file NO-LOCK WHERE 
        asi._file._file-name = ipcFileName 
        NO-ERROR.
    IF AVAILABLE asi._file THEN 
        FOR EACH asi._index OF asi._file NO-LOCK, 
            EACH asi._index-field OF asi._index NO-LOCK, 
            EACH asi._field OF asi._index-field NO-LOCK 
            BY asi._field._order:
            IF LOOKUP(_field._field-name,cFieldList) EQ 0 THEN ASSIGN 
                    cFieldList = cFieldList + asi._field._field-name + ",".
        END.
    ASSIGN 
        cFieldList = TRIM(cFieldList,",").

    CREATE BUFFER hBuffer FOR TABLE ipcFileName.
    CREATE QUERY hQuery.

    ASSIGN
        hRecKey = ?
        hActnumField = ?
        hBNoField = ?
        hBolNoField = ?
        hCompanyField = ?
        hCustNoField = ?
        hInoField = ?
        hInvNoField = ?
        hLocField = ?
        hOrdNoField = ?
        hPoNoField = ?
        hRnoField = ?
        hTestField = ?
        hXnoField = ?
        hJobField = ?
        hJobNoField = ?
        hJobNo2Field = ?.
    
    /* Assign buffer-field names IF they are indexed fields */
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD(iCtr).
        /* If this field is not in the list of indexed fields and isn't "rec_key", skip it */
        IF NOT CAN-DO(cFieldList,hTestField:NAME) 
        AND hTestField:NAME NE "rec_key" THEN NEXT.
        /* Assign field names if exist in this table */        
        IF hTestField:NAME EQ "rec_key" THEN ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "actnum" THEN ASSIGN
            hActnumField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "b-no" THEN ASSIGN
            hBNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "bol-no" THEN ASSIGN
            hBolNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "company" THEN ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "cust-no" THEN ASSIGN
            hCustNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "i-no" THEN ASSIGN
            hInoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "inv-no" THEN ASSIGN
            hInvNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "loc" THEN ASSIGN
            hLocField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "ord-no" THEN ASSIGN 
            hOrdNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "po-no" THEN ASSIGN 
            hPoNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "r-no" THEN ASSIGN 
            hRNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "x-no" THEN ASSIGN
            hXnoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job" THEN ASSIGN
            hJobField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job-no" THEN ASSIGN
            hJobNoField = hBuffer:BUFFER-FIELD(iCtr).
        ELSE IF hTestField:NAME EQ "job-no2" THEN ASSIGN
            hJobNo2Field = hBuffer:BUFFER-FIELD(iCtr).
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
                          ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
                          (IF hCompanyField NE ? THEN (" AND " + ipcFileName + ".company EQ '" + cThisCompany + "'") ELSE "") + 
                          " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        /* Account number tests */
        IF hActNumField NE ? THEN DO:
            IF hActNumField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = IF CAN-DO("ar-invl",ipcFileName) THEN TRUE ELSE lError 
                lWarning = IF NOT CAN-DO("ar-invl",ipcFileName) THEN TRUE ELSE lWarning 
                cRule = cRule + ",Blank Acct No"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST account WHERE 
                                 account.company EQ cThisCompany AND 
                                 account.actnum EQ hActNumField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Acct No"
                cMessage = cMessage + ",Acctnum=" + hActNumField:BUFFER-VALUE.  
        END.

        /* B-no tests */
        IF hBNoField NE ? THEN DO:
            IF hBNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-rcpth",ipcFileName) THEN ASSIGN 
                lWarning = TRUE 
                cRule = cRule + ",Blank B-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-bolh WHERE 
                                 oe-bolh.b-no EQ hBNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid B-no"
                cMessage = cMessage + ",B-no=" + hBNoField:BUFFER-VALUE.  
        END.

        /* BOL-no tests */
        IF hBolNoField NE ? THEN DO:
            IF hBolNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-bin,fg-rdtlh",ipcFileName) THEN ASSIGN 
                lWarning = TRUE 
                cRule = cRule + ",Blank BOL-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-bolh WHERE 
                                 oe-bolh.bol-no EQ hBolNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("ar-invl,fg-bin,fg-rdtlh,inv-head",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid BOL-no"
                cMessage = cMessage + ",BOL-no=" + hBolNoField:BUFFER-VALUE.  
        END.

        /* Company tests */
        IF hCompanyField NE ? THEN DO:
            IF hCompanyField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Company"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST company WHERE 
                                 company.company EQ hCompanyField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Company"
                cMessage = cMessage + ",Company=" + hCompanyField:BUFFER-VALUE.  
        END.

        /* Cust-no tests */
        IF hCustNoField NE ? THEN DO:
            IF hCustNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-bin,fg-rdtlh,item,itemfg,rm-bin,fg-rcpts,job-hdr",ipcFileName) THEN ASSIGN 
                lWarning = IF NOT CAN-DO("oe-rel,oe-relh,oe-rell,oe-bolh,oe-boll,oe-ord,oe-ordl,oe-ordm",ipcFileName) THEN TRUE ELSE lWarning 
                lError = IF CAN-DO("oe-rel,oe-relh,oe-rell,oe-bolh,oe-boll,oe-ord,oe-ordl,oe-ordm",ipcFileName) THEN TRUE ELSE lError 
                cRule = cRule + ",Blank Cust-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hCustNoField:BUFFER-VALUE NE "" 
            AND NOT CAN-FIND(FIRST cust WHERE 
                                 cust.cust-no EQ hCustNoField:BUFFER-VALUE) THEN ASSIGN 
                lError = IF NOT CAN-DO("item,itemfg",ipcFileName) THEN TRUE ELSE lError 
                lWarning = IF CAN-DO("item,itemfg",ipcFileName) THEN TRUE ELSE lWarning 
                cRule = cRule + ",Invalid Cust-no"
                cMessage = cMessage + ",Cust-no=" + hCustNoField:BUFFER-VALUE.
        END.

        /* I-no tests - Test blank i-no is complicated, as other records/fields come into play */
        IF hINoField NE ? THEN DO:
            IF hINoField:BUFFER-VALUE EQ "" 
            AND ipcFileName EQ "fg-rdtlh"
            AND NOT CAN-FIND(FIRST fg-rcpth WHERE 
                             fg-rcpth.company EQ hCompanyField:BUFFER-VALUE AND 
                             fg-rcpth.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                             USE-INDEX r-no) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE EQ "" 
            AND ipcFileName EQ "fg-rcpth"
            AND NOT CAN-FIND(FIRST fg-rdtlh WHERE 
                             fg-rdtlh.company EQ hCompanyField:BUFFER-VALUE AND 
                             fg-rdtlh.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                             USE-INDEX rm-rdtl) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE EQ "" 
            AND (ipcFileName EQ "oe-boll" OR ipcFileName EQ "oe-rell")
            AND NOT CAN-FIND(FIRST oe-ord WHERE 
                             oe-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                             oe-ord.ord-no EQ INTEGER(hOrdNoField:BUFFER-VALUE)
                             USE-INDEX ord-no) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank I-No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hINoField:BUFFER-VALUE NE "" THEN DO:
                IF NOT CAN-FIND(FIRST item WHERE 
                                     item.company EQ cThisCompany AND  
                                     item.i-no EQ hINoField:BUFFER-VALUE)
                    AND NOT CAN-FIND(FIRST itemfg WHERE 
                                     itemfg.company EQ cThisCompany AND 
                                     itemfg.i-no EQ hINoField:BUFFER-VALUE) THEN ASSIGN 
                    lError = IF NOT CAN-DO("item,itemfg-loc",ipcFileName) THEN TRUE ELSE lError 
                    lWarning = IF CAN-DO("item,itemfg-loc",ipcFileName) THEN TRUE ELSE lWarning 
                    cRule = cRule + ",Invalid Item No"
                    cMessage = cMessage + ",I-no=" + hINoField:BUFFER-VALUE.
/*                ELSE IF NOT CAN-DO("item,itemfg,itemfg-loc",ipcFileName) AND*/
/*                        (CAN-FIND(FIRST item WHERE                          */
/*                                 item.company EQ cThisCompany AND           */
/*                                 item.i-no EQ hINoField:BUFFER-VALUE AND    */
/*                                 item.stat EQ "I")                          */
/*                        OR CAN-FIND(FIRST itemfg WHERE                      */
/*                                itemfg.company = cThisCompany AND           */
/*                                itemfg.i-no EQ hINoField:BUFFER-VALUE AND   */
/*                                itemfg.stat EQ "I"))THEN ASSIGN             */
/*                    lWarning = TRUE                                         */
/*                    cRule = cRule + ",Inactive Item No"                     */
/*                    cMessage = cMessage + ",I-no=" + hINoField:BUFFER-VALUE.*/
                END.
        END.
        
        /* Inv-no tests */
        IF hInvNoField NE ? THEN DO:
            IF hInvNoField:BUFFER-VALUE EQ ""
            AND NOT CAN-DO("fg-bin",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Inv No"
                cMessage = cMessage + ",<blank>".
            ELSE IF hInvNoField:BUFFER-VALUE NE 0
                AND NOT CAN-FIND(FIRST inv-head WHERE 
                                 inv-head.company EQ hCompanyField:BUFFER-VALUE AND 
                                 inv-head.inv-no EQ hInvNoField:BUFFER-VALUE) 
                AND NOT CAN-FIND(FIRST ar-inv WHERE 
                                 ar-inv.company EQ hCompanyField:BUFFER-VALUE AND 
                                 ar-inv.inv-no EQ hInvNoField:BUFFER-VALUE)THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Inv No"
                cMessage = cMessage + ",Inv-no=" + hInvNoField:BUFFER-VALUE. 
                 
        END.
        
        /* Loc tests */
        IF hLocField NE ? THEN DO:
            IF hLocField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Warehouse"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST loc WHERE 
                                loc.company EQ hCompanyField:BUFFER-VALUE AND 
                                loc.loc EQ hLocField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid Warehouse"
                cMessage = cMessage + ",Loc=" + hLocField:BUFFER-VALUE.  
        END.
        
        /* Ord-no tests */
        IF hOrdNoField NE ? THEN DO:
            IF hOrdNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank Order No"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-ord WHERE 
                                oe-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                                oe-ord.ord-no EQ hOrdNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd,job-hdr",ipcFileName) 
            THEN ASSIGN 
                lError = IF NOT CAN-DO("ar-inv,ar-invl,ar-invm",ipcFileName) THEN TRUE ELSE lError 
                cRule = cRule + ",Invalid Order No"
                cMessage = cMessage + ",Ord-no=" + hOrdNoField:BUFFER-VALUE.  
        END.
        
        /* Po-no tests */
        IF hPoNoField NE ? THEN 
        DO:
            IF hPoNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("job-hdr",ipcFileName) THEN ASSIGN  
                    lError = TRUE 
                    cRule = cRule + ",Blank PO No"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST po-ord WHERE 
                    po-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                    po-ord.po-no EQ hPoNoField:BUFFER-VALUE) 
                    AND CAN-DO("po-ordl,po-ordl-add",ipcFileName) 
                    THEN ASSIGN 
                        lError = TRUE  
                        cRule = cRule + ",Invalid PO No"
                        cMessage = cMessage + ",Po-no=" + hPoNoField:BUFFER-VALUE.  
        END.

        /* R-no tests */
        IF hRNoField NE ? THEN DO:
            IF hRNoField:BUFFER-VALUE EQ "" 
            AND NOT CAN-DO("fg-rcpth,fg-rctd,fg-rdtlh,fg-rdtl,fg-rcpts",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank R-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST oe-relh WHERE 
                                oe-relh.company EQ hCompanyField:BUFFER-VALUE AND     
                                oe-relh.r-no EQ hRNoField:BUFFER-VALUE) 
            AND NOT CAN-DO("fg-rcpth,fg-rcpts,fg-rctd,fg-rdtlh,rm-rctd,rm-rcpt,oe-rel,inv-head",ipcFileName) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid R-no"
                cMessage = cMessage + ",R-no=" + hRNoField:BUFFER-VALUE.  
        END.
        
        /* X-no tests */
        IF hXNoField NE ? THEN DO:
            IF hXNoField:BUFFER-VALUE EQ "" THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Blank X-no"
                cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST ar-inv WHERE 
                    ar-inv.company EQ hCompanyField:BUFFER-VALUE AND     
                    ar-inv.x-no EQ hXNoField:BUFFER-VALUE) THEN ASSIGN 
                lError = TRUE 
                cRule = cRule + ",Invalid X-no"
                cMessage = cMessage + ",X-no=" + hXNoField:BUFFER-VALUE.  
        END.
        
        /* Job tests */
        IF hJobField NE ? THEN DO:
            IF hJobField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank job"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST job WHERE 
                    job.company EQ hCompanyField:BUFFER-VALUE AND     
                    job.job EQ hJobField:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid job"
                        cMessage = cMessage + ",job=" + hJobField:BUFFER-VALUE.  
        END.
        
        /* JobNo/JobNo2 tests */
        IF hJobNoField NE ? THEN DO:
            IF hJobField:BUFFER-VALUE EQ "" THEN ASSIGN 
                    lError = TRUE 
                    cRule = cRule + ",Blank job-no"
                    cMessage = cMessage + ",<blank>".
            ELSE IF NOT CAN-FIND(FIRST job WHERE 
                    job.company EQ hCompanyField:BUFFER-VALUE AND     
                    job.job-no EQ hJobNoField:BUFFER-VALUE AND 
                    job.job-no2 EQ hJobNo2Field:BUFFER-VALUE) THEN ASSIGN 
                        lError = TRUE 
                        cRule = cRule + ",Invalid job-no"
                        cMessage = cMessage + ",job-no=" + hJobNoField:BUFFER-VALUE + " and job-no2=" + hJobNo2Field:BUFFER-VALUE.  
        END.
        
        /* No matter what, don't delete fg-bin or rm-bin */
        IF CAN-DO("fg-bin,rm-bin",ipcFileName)
        AND lError THEN ASSIGN 
            lError = FALSE
            lWarning = TRUE.
            

        IF lError 
        OR lWarning THEN DO:
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName    = ipcFileName
                ttFileList.rRowID       = hBuffer:ROWID
                ttFileList.cRec_key     = hRecKey:BUFFER-VALUE
                ttFileList.cError       = IF lError THEN "Error" ELSE IF lWarning THEN "Warning" ELSE ""
                ttFileList.lPurge       = lError
                ttFileList.cRule        = SUBSTRING(cRule,2)
                ttFileList.cMessage     = SUBSTRING(cMessage,2)
                ttFileList.cKeyValues   = (IF hCompanyField NE ? THEN hCompanyField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hCustNoField NE ? THEN hCustNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hLocField NE ? THEN hLocField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hOrdNoField NE ? THEN hOrdNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hPoNoField NE ? THEN hPoNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hRNoField NE ? THEN hRNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBolNoField NE ? THEN hBolNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hBNoField NE ? THEN hBNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hInvNoField NE ? THEN hInvNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hINoField NE ? THEN hINoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hXNoField NE ? THEN hXNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hActNumField NE ? THEN hActNumField:BUFFER-VALUE ELSE "") + "," + 
                                          (IF hJobField NE ? THEN hJobField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hJobNoField NE ? THEN hJobNoField:BUFFER-VALUE ELSE "") + "," +
                                          (IF hJobNo2Field NE ? THEN hJobNo2Field:BUFFER-VALUE ELSE "")
                iErrorCount             = IF ttFileList.cError EQ "Error" THEN iErrorCount + 1 ELSE iErrorCount
                iWarningCount           = IF ttFileList.cError EQ "Warning" THEN iWarningCount + 1 ELSE iWarningCount
                .
                
        END.        
        ASSIGN 
            lError = FALSE 
            lWarning = FALSE  
            cRule = ""
            cMessage = ""
            iProcessedCount = iProcessedCount + 1.
        hQuery:GET-NEXT().
    END.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-purgeComplete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputOrphanFile Procedure
PROCEDURE outputOrphanFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT STREAM sReport TO VALUE (cOutputDir + "\" + "_PurgeReport.csv").
    
    PUT STREAM sReport UNFORMATTED 
        "Purge (Y/N)?,ErrLevel,Table Name,Reckey,Rowid,Company,Cust-no,Loc,Ord-no,Po-no,R-no,Bol-no,B-no,Inv-no,I-no,X-no,Acctnum,Job,Job-No,Job-No2" + CHR(10).
    FOR EACH ttFileList
        BY ttFileList.cError
        BY ttFileList.cFileName 
        BY ttFileList.cRec_key:
        PUT STREAM sReport UNFORMATTED
            (IF ttFileList.lPurge THEN "YES" ELSE "") + "," +
            ttFileList.cError + "," +
            ttFileList.cFileName + "," +   
            ttFileList.cRec_key + ","                    
            STRING(ttFileList.rRowID) + "," +                  
            ttFileList.cKeyValues + ",,"
            .
        DO iCtr = 1 TO NUM-ENTRIES(cRule):
            PUT STREAM sReport UNFORMATTED 
                ENTRY(iCtr,cRule) + "," + ENTRY(iCtr,cMessage) + ",".
        END.
        PUT STREAM sReport UNFORMATTED CHR(10).
    END.
    OUTPUT STREAM sReport CLOSE.
    EMPTY TEMP-TABLE ttFileList.
    
/*    IF lPurge THEN DO:                                                                        */
/*        FOR EACH ttFileList                                                                   */
/*            BREAK BY ttFileList.cFileName:                                                    */
/*                                                                                              */
/*            IF FIRST-OF(ttFileList.cFileName) THEN DO:                                        */
/*                OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ttFileList.cFileName + ".d").*/
/*                CREATE BUFFER hBuffer FOR TABLE ttFileList.cFileName.                         */
/*                hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).                                         */
/*                CREATE QUERY hQuery.                                                          */
/*                hQuery:ADD-BUFFER(hBuffer).                                                   */
/*            END.                                                                              */
/*                                                                                              */
/*            DO TRANSACTION:                                                                   */
/*                hQuery:QUERY-PREPARE("FOR EACH " + ttFileList.cFileName +                     */
/*                                     " WHERE ROWID(" + ttFileList.cFileName + ")" +           */
/*                                     " EQ TO-ROWID('" + STRING(ttFileList.rRowID) + "')").    */
/*                hQuery:QUERY-OPEN().                                                          */
/*                hQuery:GET-FIRST(EXCLUSIVE-LOCK).                                             */
/*                OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ttFileList.cFileName + ".d").*/
/*                IF hBuffer:AVAILABLE THEN DO:                                                 */
/*                    PUT STREAM sDump UNFORMATTED dynExport(hBuffer, " ") SKIP.                */
/*                    hBuffer:BUFFER-DELETE().                                                  */
/*                END.                                                                          */
/*                OUTPUT STREAM sDump CLOSE.                                                    */
/*            END.                                                                              */
/*                                                                                              */
/*            IF LAST-OF(ttFileList.cFileName) THEN DO:                                         */
/*                DELETE OBJECT hQuery.                                                         */
/*                DELETE OBJECT hBuffer.                                                        */
/*            END.                                                                              */
/*        END.                                                                                  */
/*    END.                                                                                      */
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeEitemRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeEitemRecords Procedure
PROCEDURE purgeEitemRecords:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE OUTPUT PARAMETER oplError AS LOG.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER.
    
    DEFINE VARIABLE cTempDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurgeDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurgeFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDirCreated AS LOG NO-UNDO.
    DEFINE VARIABLE cDirMsg AS CHARACTER NO-UNDO.
        
    RUN FileSys_GetTempDirectory (OUTPUT cTempDir).
    ASSIGN 
        cPurgeDir = cTempDir + "\eItemPurge" + STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99") + 
                                               STRING(DAY(TODAY),"99") +
                                               STRING(TIME,"99999").
    RUN FileSys_CreateDirectory (INPUT cPurgeDir,
                                 OUTPUT lDirCreated,
                                 OUTPUT cDirMsg).
    IF NOT lDirCreated THEN DO:
        ASSIGN 
            oplError = TRUE
            opcMessage = cDirMsg.
        RETURN.
    END.
    
    ASSIGN 
        opcMessage = cPurgeDir.
    
&SCOPED-DEFINE cFileToPurge e-item
    OUTPUT TO VALUE(cPurgeDir + "\e-item.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-item-vend
    OUTPUT TO VALUE(cPurgeDir + "\eitmvend.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-itemfg
    OUTPUT TO VALUE(cPurgeDir + "\e-itemfg.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPED-DEFINE cFileToPurge e-itemfg-vend
    OUTPUT TO VALUE(cPurgeDir + "\e-temfv.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
&SCOPEd-DEFINE cFileToPurge e-item-cust
    OUTPUT TO VALUE(cPurgeDir + "\e-item-c.d").
    FOR EACH {&cFileToPurge} WHERE 
        {&cFileToPurge}.company EQ (IF ipcCompany NE "All" THEN ipcCompany ELSE {&cFileToPurge}.company):
        EXPORT {&cFileToPurge}.
        DELETE {&cFileToPurge}.
    END.
    OUTPUT CLOSE.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeGLhistFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeGLhistFromFile Procedure
PROCEDURE purgeGLhistFromFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER.
    DEFINE INPUT PARAMETER ipcFileNameExt AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE OUTPUT PARAMETER oplError AS LOG.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER.
    
    DEFINE VARIABLE cHeaderList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTableCol AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRowidCol AS INTEGER NO-UNDO.
    DEFINE VARIABLE cRawRow AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE deSummaryAmount AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bGlhist FOR glhist.
    DISABLE TRIGGERS FOR LOAD OF glhist.
    
    ASSIGN 
        cOutputDir = REPLACE(ipcFileName,"\" + ipcFileNameExt ,"").
    
    IF SEARCH (ipcFileName) EQ ? THEN 
    DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate specified file name".
        RETURN.
    END.
    ELSE IF SEARCH (cOutputDir + "\" + ipcFileName + ".d") NE ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Records already deleted for table " + ipcFileName + " in this purge.".
        RETURN.
    END.
    ELSE DO:
        OUTPUT STREAM sDump TO VALUE (cOutputDir + "\glhist.d").
                   
        FOR EACH ttGLHistList 
            BREAK BY ttGLHistList.iYear
            BY ttGLHistList.iPeriod
            BY ttGLHistList.cAccount:
            /* Only on first by break */
            IF FIRST-OF(ttGLHistList.cAccount) 
            OR FIRST-OF(ttGLHistList.iPeriod)
            OR FIRST-OF(ttGLHistList.iYear) THEN ASSIGN 
                deSummaryAmount = 0.
            /* All records in group */
            ASSIGN 
                deSummaryAmount = deSummaryAmount + ttGLHistList.deAmount.
            FIND FIRST period NO-LOCK WHERE 
                period.company EQ ipcCompany AND
                period.pst LE ttGLHistList.daTxnDate AND 
                period.pend GE ttGLHistList.daTxnDate
                NO-ERROR.
            FIND FIRST bglHist EXCLUSIVE WHERE 
                ROWID(bglHist) EQ ttGLHistList.rRowid
                NO-ERROR.
            IF AVAILABLE bglHist THEN DO:
                EXPORT STREAM sDump bglHist.
                DELETE bglHist.
            END.
            /* Only on last of group */
            /* When creating the balance fwd txn, it's now safe to assign year/prd 
               because we created any missing ones during the test phase */
            IF LAST-OF(ttGLHistList.cAccount) THEN DO:
                CREATE bglHist.
                ASSIGN 
                    bglhist.actnum      = ttGLHistList.cAccount     
                    bglhist.company     = ipcCompany
                    bglhist.createdBy   = USERID(LDBNAME(1))
                    bglhist.createdDate = TODAY
                    bglhist.curr-code[1]= ttGLHistList.cCurrency
                    bglhist.documentID  = ""
                    bglhist.entryType   = "B"
                    bglhist.ex-rate     = 1
                    bglhist.glYear      = IF ttGLHistList.iYear NE 0 THEN ttGLHistList.iYear ELSE 
                                          IF AVAILABLE period THEN period.yr ELSE YEAR(ttGLHistList.daTxnDate)
                    bglhist.jrnl        = ttGLHistList.cJournal
                    bglhist.module      = ""
                    bglhist.period      = ttGLHistList.iPeriod
                    bglhist.posted      = YES
                    bglhist.postedBy    = USERID(LDBNAME(1))
                    bglhist.rec_key     = STRING(YEAR(TODAY),"9999") + 
                                          STRING(MONTH(TODAY),"99") + 
                                          STRING(DAY(TODAY),"99") + 
                                          STRING(TIME,"99999") + 
                                          STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
                    bglhist.sourceDate  = ?
                    bglhist.tr-amt      = deSummaryAmount
                    bglhist.tr-date     = IF AVAILABLE period THEN period.pend ELSE ttGLHistList.daTxnDate
                    bglhist.tr-dscr     = "Balance Forward Total"
                    bglhist.tr-num      = INTEGER(STRING(bglhist.glYear,"9999") + STRING(ttGLHistList.iPeriod,"99"))
                    bglhist.yr          = IF ttGLHistList.iYear NE 0 THEN ttGLHistList.iYear ELSE 
                                          IF AVAILABLE period THEN period.yr ELSE YEAR(ttGLHistList.daTxnDate)
                    .
            END.
        END.
            
            
    END.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeJobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeJobs Procedure
PROCEDURE purgeJobs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMode AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess AS LOG NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN Purge_SimulateAndPurgeJobRecords(
        BUFFER job,
        INPUT  NO,        /* Delete Records ? */
        INPUT  YES,         /* Create .csv files for child tables? */
        INPUT  NO,        /* Called from trigger?  */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ). 
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeOrphansFromFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeOrphansFromFile Procedure
PROCEDURE purgeOrphansFromFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER.
    DEFINE OUTPUT PARAMETER oplError AS LOG.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER.
    
    DEFINE VARIABLE cHeaderList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTableCol AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRowidCol AS INTEGER NO-UNDO.
    DEFINE VARIABLE cRawRow AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cOutputDir = REPLACE(ipcFileName,"\_PurgeReport.csv","").
    EMPTY TEMP-TABLE ttPurgeList.
    IF SEARCH (ipcFileName) EQ ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate specified file name".
        RETURN.
    END.
    ELSE IF SEARCH (cOutputDir + "\" + ipcFileName + ".d") NE ? THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Records already deleted for table " + ipcFileName + " in this purge.".
        RETURN.
    END.
    ELSE DO:
        INPUT FROM VALUE(ipcFileName).
        IMPORT UNFORMATTED cHeaderList.
        DO iCtr = 1 TO NUM-ENTRIES(cHeaderList):
            IF ENTRY(iCtr,cHeaderList) = "Table Name" THEN ASSIGN 
                iTableCol = iCtr.
            IF ENTRY(iCtr,cHeaderList) = "Rowid" THEN ASSIGN 
                iRowidCol = iCtr.
        END.
        INPUT CLOSE.
        IF iTableCol EQ 0 
        OR iRowidCol EQ 0 THEN DO:
            ASSIGN 
                oplError = TRUE 
                opcMessage = "Unable to locate table name or rowid column in specified file".
            RETURN.
        END.
        ELSE DO:
            INPUT FROM VALUE(ipcFileName).
            REPEAT:
                IMPORT UNFORMATTED cRawRow.
                IF ENTRY(1,cRawRow) BEGINS "Y" THEN DO:
                    CREATE ttPurgeList.
                    ASSIGN 
                        ttPurgeList.cTable = ENTRY(iTableCol,cRawRow)
                        ttPurgeList.rRowid = TO-ROWID(ENTRY(iRowidCol,cRawRow)).
                END.
            END.
            FOR EACH ttPurgeList BREAK BY ttPurgeList.cTable:
                IF FIRST-OF(ttPurgeList.cTable) THEN ASSIGN 
                    cTableList = cTableList + ttPurgeList.cTable + ",".
            END.
            ASSIGN 
                cTableList = TRIM(cTableList,",").
            DO iCtr = 1 TO NUM-ENTRIES(cTableList):
                RUN pDeleteRecordsByRowid (
                    ENTRY(iCtr,cTableList),
                    OUTPUT oplError,
                    OUTPUT opcMessage).
            END.
        END.
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
    
    IF NOT VALID-HANDLE(hOutputProcs) THEN 
        RUN system/OutputProcs.p PERSISTENT SET hOutputProcs.
            
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
                OUTPUT STREAM sReftable TO VALUE (cOutDir + "\DataFiles\reftable.d") APPEND.
            END.    
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company   EQ ip-bf-Job.company
                 AND reftable.loc       EQ ""
                 AND reftable.code      EQ STRING(ip-bf-Job.job,"999999999"):
                IF NOT iplCalledFromTrigger THEN
                EXPORT STREAM sReftable reftable.     
                DELETE reftable.
            END.
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "job.create-time"
                  AND reftable.company  EQ ip-bf-Job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(ip-bf-Job.job,"9999999999"):
                IF NOT iplCalledFromTrigger THEN
                EXPORT STREAM sReftable reftable.                      
                DELETE reftable.
            END.
            
            FOR EACH reftable EXCLUSIVE-LOCK
                WHERE reftable.reftable EQ "job.qty-changed"
                  AND reftable.company  EQ ip-bf-Job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(ip-bf-Job.job,"9999999999"):
                IF NOT iplCalledFromTrigger THEN
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
    
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeClearTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeClearTempTable Procedure
PROCEDURE purgeClearTempTable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
  EMPTY TEMP-TABLE ttGLHistList.
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-purgeProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeProcess Procedure
PROCEDURE purgeProcess:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputDir AS CHAR.
    
    DEFINE VARIABLE cFileList AS CHAR.

    FOR EACH ttPurgeList:
        IF cFileList EQ "" THEN ASSIGN 
            cFileList = ttPurgeList.cTable.
        ELSE IF LOOKUP(ttPurgeList.cTable,cFileList) EQ 0 THEN ASSIGN 
            cFileList = cFileList + "," + ttPurgeList.cTable.
    END.
    ASSIGN 
        cFileList = TRIM(cFileList,",")
        cOutputDir = ipcOutputDir.
    
    DO iCtr = 1 TO NUM-ENTRIES(cFileList):
        RUN pDeleteRecordsByRowid (
            ENTRY(iCtr,cFileList),
            OUTPUT lError,
            OUTPUT cMessage).
    END.            
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-testAccounts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testAccounts Procedure
PROCEDURE testAccounts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiYear AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPeriod AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputDir AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    OS-CREATE-DIR VALUE(ipcOutputDir).
    ASSIGN 
        cOutputDir = ipcOutputDir.
     
    RUN pTestGlhist (ipcCompany,
        ipiYear,
        ipiPeriod,
        OUTPUT oplError,
        OUTPUT opcMessage).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeOrphans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testOrphans Procedure
PROCEDURE testOrphans:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  Calling syntax:
             RUN testOrphans IN hPurge (cFileName,
                                        cDateLimit,
                                        cOutputDir,
                                        cThisCompany,
                                        OUTPUT iProcessed,
                                        OUTPUT lError,
                                        OUTPUT cMessage).
     
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDateLimit AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputDir AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiProcessedCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiErrorCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiWarningCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE daDateLimit AS DATE NO-UNDO.
    DEFINE VARIABLE cFieldNameList AS CHARACTER NO-UNDO.
    
    ASSIGN
        cThisCompany = ipcCompany
        daDateLimit = IF ipcDateLimit NE "" THEN DATE(ipcDateLimit) ELSE TODAY
        cRecKeyPrefix = STRING(YEAR(daDateLimit),"9999") +
                        STRING(MONTH(daDateLimit),"99") +
                        STRING(DAY(daDateLimit),"99")
        iProcessedCount = 0
        iErrorCount = 0
        iWarningCount = 0.
   
    OS-CREATE-DIR VALUE(ipcOutputDir).
    ASSIGN 
        cOutputDir = ipcOutputDir.
         
    IF CAN-DO("ar-inv,ar-invl,ar-invm,inv-head,inv-line,inv-misc",ipcFileName) THEN 
        RUN pTestInvoice (ipcFileName,
                          cRecKeyPrefix,
                          OUTPUT oplError,
                          OUTPUT opcMessage).
    ELSE 
        RUN pTestOneFile (ipcFileName,
                          cRecKeyPrefix,
                          OUTPUT oplError,
                          OUTPUT opcMessage).
    
    
    ASSIGN 
        opiProcessedCount = iProcessedCount
        opiErrorCount = iErrorCount
        opiWarningCount = iWarningCount.                  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF





/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  ( INPUT hRecord AS HANDLE , INPUT cDelim AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hFld     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtnt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArray   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLobname AS CHARACTER NO-UNDO.

    IF hRecord:TYPE <> "BUFFER" THEN
        RETURN ?.

    DO iCnt = 1 TO hRecord:NUM-FIELDS:

        ASSIGN 
            hFld = hRecord:BUFFER-FIELD(iCnt).

        /* Handle EXPORT for large objects by writing them out to .blb files. Omit this section in Progress 9 
         * EXPORT adds extra "" for output compatible with the INPUT statement.
         * Names for blobs are not guaranteed the same as the static EXPORT statement, IMPORT handles them correctly. 
        */
        IF hFld:DATA-TYPE = "clob" OR hFld:DATA-TYPE = "blob" THEN 
        DO:
            IF hFld:BUFFER-VALUE = ? THEN 
            DO:
                cResult = cResult + "?" + cDelim.
            END.
            ELSE 
            DO:
                cLobname = hFld:NAME +
                    (IF hFld:DATA-TYPE = "clob" THEN "!" + GET-CODEPAGES(hFld:BUFFER-VALUE) + "!" ELSE "") 
                    + hRecord:TABLE + "_" + STRING(hRecord:RECID) + ".blb".
                COPY-LOB FROM hFld:BUFFER-VALUE TO FILE cLobname NO-CONVERT.
                cResult = cResult + QUOTER(cLobname) + cDelim.
            END.
            NEXT.
        END.
      
        IF hFld:EXTENT = 0 THEN 
        DO:
            IF hFld:BUFFER-VALUE EQ ? THEN ASSIGN 
                cTmp = "?".
            ELSE 
         
                CASE hFld:DATA-TYPE:
                    WHEN "character" THEN 
                        cTmp = QUOTER(hFld:BUFFER-VALUE).
                    WHEN "raw"  THEN 
                        cTmp = '"' + STRING(hFld:BUFFER-VALUE) + '"'.
                    WHEN "datetime" OR 
                    WHEN "datetime-tz" THEN ASSIGN 
                        cTmp = STRING(YEAR(hFld:BUFFER-VALUE),"9999") 
                            + "-" + string(MONTH(hFld:BUFFER-VALUE),"99") 
                            + "-" + string(DAY(hFld:BUFFER-VALUE),"99") 
                            + "T" + substring(STRING(hFld:BUFFER-VALUE),12).
                    OTHERWISE  
                    cTmp = STRING(hFld:BUFFER-VALUE).
                END CASE.
         
            cResult = cResult + cTmp + cDelim.
        END.
        ELSE 
        DO:
            cArray = "".   
            DO iExtnt = 1 TO hFld:EXTENT:
                IF hFld:BUFFER-VALUE(iExtnt) EQ  ? THEN ASSIGN 
                    cTmp = "?".
                ELSE

                    CASE hFld:DATA-TYPE:
                        WHEN "character" THEN 
                            cTmp = QUOTER(hFld:BUFFER-VALUE(iExtnt)).
                        WHEN "raw"          THEN 
                            cTmp = '"' + STRING(hFld:BUFFER-VALUE(iExtnt)) + '"'.
                        WHEN "datetime" OR 
                        WHEN "datetime-tz" THEN 
                            cTmp = STRING(YEAR(hFld:BUFFER-VALUE(iExtnt)),"9999") 
                                + "-" + string(MONTH(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "-" + string(DAY(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "T" + substring(STRING(hFld:BUFFER-VALUE(iExtnt)),12).
                        OTHERWISE  
                        cTmp = STRING(hFld:BUFFER-VALUE(iExtnt)).
                    END CASE.

                cArray = cArray + cTmp + cDelim.
            END.
            cResult = cResult + RIGHT-TRIM(cArray,cDelim) + cDelim.
        END.
    END.
    RETURN RIGHT-TRIM(cResult,cDelim).
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-fDateString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateString Procedure
FUNCTION fDateString RETURNS CHARACTER 
  ( iYrOffset AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
		
    DEFINE VARIABLE result AS CHARACTER NO-UNDO.

    ASSIGN 
        RESULT = STRING(MONTH(TODAY),"99") + "/" +
                 STRING(DAY(TODAY),"99") + "/" +
                 STRING(YEAR(TODAY) - iYrOffset,"9999").
		
    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fEndPurge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEndPurge Procedure
FUNCTION fEndPurge RETURNS LOGICAL 
  ( INPUT cTable AS CHARACTER ):
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


&IF DEFINED(EXCLUDE-fGetDataDumpDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetDataDumpDir Procedure
FUNCTION fGetDataDumpDir RETURNS CHARACTER 
  ( INPUT ipcInitValue AS CHARACTER, INPUT ipcTable AS CHARACTER, OUTPUT oplError AS LOG, OUTPUT opcMessage AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDumpDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTestDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE jCtr AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCreateError AS LOG NO-UNDO.
    DEFINE VARIABLE cCreateMessage AS CHARACTER NO-UNDO.
    
    IF ipcTable EQ "" THEN DO:
        ASSIGN 
            FILE-INFO:FILE-NAME = ipcInitValue
            cTestDir = FILE-INFO:FULL-PATHNAME.
        DO iCtr = 1 TO NUM-ENTRIES(cTestDir,"\"):
            IF ENTRY(iCtr,cTestDir,"\") EQ "Environments"
            OR ENTRY(iCtr,cTestDir,"\") EQ "Repositories" THEN DO:
                DO jCtr = 1 TO iCtr - 1:
                    ASSIGN 
                        cDumpDir = cDumpDir + ENTRY(jCtr,cTestDir,"\") + "\".
                END.
                LEAVE.
            END.
        END.    
        ASSIGN 
            cDumpDir = cDumpDir + "Backups\Purges\".
        RUN filesys_createDirectory (INPUT cDumpDir, OUTPUT lCreateError, OUTPUT cCreateMessage).
        IF lCreateError THEN DO:
            
        END.            
    END.     
    ELSE DO:
    END. 
    
    RETURN cDumpDir.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fGetPurgeDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPurgeDir Procedure
FUNCTION fGetPurgeDir RETURNS CHARACTER 
  ( INPUT cTable AS CHARACTER  ):
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
  ( INPUT cTable AS CHARACTER ):
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
    ( INPUT cTable AS CHARACTER, INPUT cNames AS CHARACTER, INPUT cData AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOutline AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
        
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


