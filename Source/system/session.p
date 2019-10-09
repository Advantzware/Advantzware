&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : session.p
    Purpose     : Super Procedure for Session

    Syntax      : RUN system\session.p PERSISTENT SET hProc.
                  SESSION:ADD-SUPER-PROCEDURE (hProc).

    Description : session procedures

    Author(s)   : Ron Stark
    Created     : 7.6.2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupTitle        AS CHARACTER NO-UNDO INITIAL ?.
DEFINE VARIABLE cMnemonic           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgramID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE hMainMenuHandle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSysCtrlUsageHandle AS HANDLE    NO-UNDO.
DEFINE VARIABLE iParamValueID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPeriod             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSecure             AS LOGICAL   NO-UNDO.
/* cue card variables */
DEFINE VARIABLE lCueCardActive      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCueOrder           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNext               AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttSessionParam NO-UNDO
    FIELD sessionParam AS CHARACTER
    FIELD sessionValue AS CHARACTER
        INDEX sessionParam IS PRIMARY UNIQUE sessionParam
        .
{system/ttPermissions.i}
{system/ttSysCtrlUsage.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fCueCardActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCueCardActive Procedure
FUNCTION fCueCardActive RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetBeginSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetBeginSearch Procedure
FUNCTION sfGetBeginSearch RETURNS CHARACTER 
  ( INPUT ipcString AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfGetTtPermissionsHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetTtPermissionsHandle Procedure
FUNCTION sfGetTtPermissionsHandle RETURNS HANDLE 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfWebCharacters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfWebCharacters Procedure
FUNCTION sfWebCharacters RETURNS CHARACTER 
  (ipcWebString AS CHARACTER,
   ipiLevel AS INTEGER,
   ipcType AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfClearUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfClearUsage Procedure 
FUNCTION sfClearUsage RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetMainMenuHandle Procedure 
FUNCTION sfGetMainMenuHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetNextRecKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetNextRecKey Procedure 
FUNCTION sfGetNextRecKey RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetSysCtrlUsageHandle Procedure 
FUNCTION sfGetSysCtrlUsageHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetTtSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetTtSysCtrlUsageHandle Procedure 
FUNCTION sfGetTtSysCtrlUsageHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfSetMainMenuHandle Procedure 
FUNCTION sfSetMainMenuHandle RETURNS LOGICAL
  (iphMainMenuHandle AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfSetSysCtrlUsageHandle Procedure 
FUNCTION sfSetSysCtrlUsageHandle RETURNS LOGICAL
  (iphSysCtrlUsageHandle AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfUserSecurityLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfUserSecurityLevel Procedure 
FUNCTION sfUserSecurityLevel RETURNS INTEGER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
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

FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID("ASI")
     NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-spActivateCueCards) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spActivateCueCards Procedure
PROCEDURE spActivateCueCards:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO TRANSACTION:
        FOR EACH xCueCard EXCLUSIVE-LOCK
            WHERE xCueCard.user_id EQ USERID("ASI")
            :
            DELETE xCueCard.
        END. /* each xcuecard */
    END. /* do trans */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCheckTrackUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCheckTrackUsage Procedure
PROCEDURE spCheckTrackUsage:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMnemonic AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.

    IF CAN-FIND(FIRST prgrms
                WHERE prgrms.prgmname    EQ ipcPrgmName
                  AND prgrms.track_usage EQ YES) THEN DO:
        RUN spCreateAuditHdr ("TRACK","ASI",ipcPrgmName,ipcMnemonic,OUTPUT iAuditID).
        RUN spCreateAuditDtl (iAuditID,"",0,"","",NO).
    END. /* if lAuditRecalcQty */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCreateAuditDtl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreateAuditDtl Procedure
PROCEDURE spCreateAuditDtl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAuditID          AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcAuditField       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiAuditExtent      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcAuditBeforeValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAuditAfterValue  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplAuditIdxField    AS LOGICAL   NO-UNDO.
    
    CREATE AuditDtl.
    ASSIGN  
        AuditDtl.AuditID          = ipiAuditID
        AuditDtl.AuditField       = ipcAuditField
        AuditDtl.AuditExtent      = ipiAuditExtent
        AuditDtl.AuditBeforeValue = ipcAuditBeforeValue
        AuditDtl.AuditAfterValue  = ipcAuditAfterValue
        AuditDtl.AuditIdxField    = iplAuditIdxField
        . 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCreateAuditHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreateAuditHdr Procedure
PROCEDURE spCreateAuditHdr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAuditType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAuditDB    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAuditTable AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAuditKey   AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER opiAuditID    AS INTEGER   NO-UNDO. 

    CREATE AuditHdr.  
    ASSIGN  
        AuditHdr.AuditID       = NEXT-VALUE(Audit_Seq,Audit) 
        AuditHdr.AuditDateTime = NOW 
        AuditHdr.AuditType     = ipcAuditType
        AuditHdr.AuditDB       = ipcAuditDB
        AuditHdr.AuditTable    = ipcAuditTable
        AuditHdr.AuditUser     = USERID("ASI") 
        AuditHdr.AuditKey      = ipcAuditKey 
        opiAuditID             = AuditHdr.AuditID 
        . 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCreateSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreateSysCtrlUsage Procedure 
PROCEDURE spCreateSysCtrlUsage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcModule        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCharFld       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDateFld      AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecFld        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiIntFld        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplLogFld        AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescrip       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtUsageNow     AS DATETIME  NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCustVend      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustVendNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSeqNo         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipId        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubCategory   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSysCtrlID     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTypeCode      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStackTrace    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSecurityLevel AS INTEGER   NO-UNDO.

    IF sfUserSecurityLevel() LT ipiSecurityLevel THEN 
    ASSIGN 
        ipcCharFld    = ?
        ipdtDateFld   = ?
        ipdDecFld     = ?
        ipiIntFld     = ?
        iplLogFld     = ?
        ipcDescrip    = "Administratively Blocked from View"
        iplCustVend   = ?
        ipcCustVendNo = ?
        ipcShipID     = ?
        .
    FIND FIRST ttSysCtrlUsage
         WHERE ttSysCtrlUsage.company  EQ ipcCompany
           AND ttSysCtrlUsage.module   EQ ipcModule
           AND ttSysCtrlUsage.name     EQ ipcName
         NO-ERROR.
    IF NOT AVAILABLE ttSysCtrlUsage THEN
    CREATE ttSysCtrlUsage.
    ASSIGN 
        ttSysCtrlUsage.company      = ipcCompany
        ttSysCtrlUsage.module       = ipcModule
        ttSysCtrlUsage.name         = ipcName
        ttSysCtrlUsage.char-fld     = ipcCharFld
        ttSysCtrlUsage.date-fld     = ipdtDateFld
        ttSysCtrlUsage.dec-fld      = ipdDecFld
        ttSysCtrlUsage.int-fld      = ipiIntFld
        ttSysCtrlUsage.log-fld      = iplLogFld
        ttSysCtrlUsage.descrip      = ipcDescrip
        ttSysCtrlUsage.usageNow     = ipdtUsageNow
        ttSysCtrlUsage.category     = ipcCategory
        ttSysCtrlUsage.cust-vend    = iplCustVend
        ttSysCtrlUsage.cust-vend-no = ipcCustVendNo
        ttSysCtrlUsage.seqNo        = ipiSeqNo
        ttSysCtrlUsage.ship-id      = ipcShipID
        ttSysCtrlUsage.subCategory  = ipcSubCategory
        ttSysCtrlUsage.sysCtrlID    = ipiSysCtrlID
        ttSysCtrlUsage.typeCode     = ipcTypeCode
        ttSysCtrlUsage.stackTrace   = ipcStackTrace
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCreatettPermissions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreatettPermissions Procedure
PROCEDURE spCreateTtPermissions:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID    AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcMnemonic AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrder    AS INTEGER   NO-UNDO.

    FIND FIRST prgrms NO-LOCK
         WHERE ROWID(prgrms) EQ iprRowID
         NO-ERROR.
    IF NOT AVAILABLE prgrms THEN RETURN.
    FIND FIRST ttPermissions
         WHERE ttPermissions.sortMnemonic EQ ipcMnemonic
           AND ttPermissions.sortOrder    EQ ipiOrder
           AND ttPermissions.parentPrgm   EQ ipcParent
           AND ttPermissions.prgmName     EQ prgrms.prgmName
         NO-ERROR.
    IF AVAILABLE ttPermissions THEN RETURN.
    CREATE ttPermissions.
    ASSIGN
        ttPermissions.sortMnemonic = ipcMnemonic
        ttPermissions.sortOrder    = ipiOrder
        ttPermissions.mnemonic     = prgrms.mnemonic
        ttPermissions.parentPrgm   = ipcParent
        ttPermissions.prgmName     = prgrms.prgmName
        ttPermissions.prgTitle     = prgrms.prgTitle
        ttPermissions.can_run      = prgrms.can_run
        ttPermissions.can_create   = prgrms.can_create
        ttPermissions.can_delete   = prgrms.can_delete
        ttPermissions.can_update   = prgrms.can_update
        ttPermissions.groups       = ""
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spCueCardClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCueCardClose Procedure
PROCEDURE spCueCardClose:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    MESSAGE 
        "Inactivate ALL Cue Cards?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
    UPDATE lInactivateCueCards AS LOGICAL.
    IF lInactivateCueCards THEN DO TRANSACTION:
        FIND CURRENT users EXCLUSIVE-LOCK.
        users.showCueCard = NO.
        FIND CURRENT users NO-LOCK.
        RUN spInactivateCueCards (cueCard.cueType).
    END. /* if inactivate cue cards */
    IF lInactivateCueCards EQ ? THEN
    RETURN NO-APPLY.
    iCueOrder = 99999.
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCueCardFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCueCardFrame Procedure
PROCEDURE spCueCardFrame:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    iphWidget:MOVE-TO-TOP ().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spDynAuditField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spDynAuditField Procedure
PROCEDURE spDynAuditField:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFrameDB    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFrameFile  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFrameField AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAuditKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprRowID      AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMsg   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRunAudit   AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cRunAudit  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSubjectID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubjectID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lResults   AS LOGICAL   NO-UNDO.
    
    FOR EACH AuditHdr NO-LOCK
        WHERE AuditHdr.AuditDB    EQ ipcFrameDB
          AND AuditHdr.AuditTable EQ ipcFrameFile
          AND AuditHdr.AuditKey   EQ ipcAuditKey,
        FIRST AuditDtl OF AuditHdr NO-LOCK
        WHERE AuditDtl.AuditField EQ ipcFrameField
        :
        lResults = TRUE.
        LEAVE.
    END. /* each audithdr */
    RUN util/CheckModule.p ("ASI","Audit", NO, OUTPUT lContinue).
    IF lContinue THEN DO:
        IF CAN-FIND(FIRST AuditTbl
                    WHERE AuditTbl.AuditTable EQ ipcFrameFile
                      AND (AuditTbl.AuditCreate EQ YES
                       OR  AuditTbl.AuditDelete EQ YES
                       OR  AuditTbl.AuditUpdate EQ YES)) THEN DO:
            RUN sys/ref/nk1look.p (
                ipcCompany,"DynAuditField","L",NO,NO,"","",
                OUTPUT cRunAudit,OUTPUT lFound
                ).
            IF lFound AND cRunAudit EQ "yes" THEN DO:
                RUN sys/ref/nk1look.p (
                    ipcCompany,"DynAuditField","I",NO,NO,"","",
                    OUTPUT cSubjectID,OUTPUT lFound
                    ).
                iSubjectID = INTEGER(cSubjectID).
                IF iSubjectID NE 0 AND
                   CAN-FIND(FIRST dynSubject
                            WHERE dynSubject.subjectID EQ iSubjectID) THEN DO:
                    RUN spGetDynParamValue (iSubjectID, OUTPUT oprRowID, OUTPUT opcErrorMsg).
                    IF AVAILABLE dynParamValue THEN DO:
                        IF lResults THEN
                        DO TRANSACTION:
                            FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
                            ASSIGN
                                oplRunAudit  = YES
                                cLookupTitle = "Audit Field History for Database: " + ipcFrameDB
                                             + " - Table: " + ipcFrameFile
                                             + " - Field: " + ipcFrameField
                                             + " - Audit Key: " + ipcAuditKey
                                             .
                            DO idx = 1 TO EXTENT(dynParamValue.paramName):
                                IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
                                CASE dynParamValue.paramName[idx]:
                                    WHEN "AuditDB" THEN
                                    dynParamValue.paramValue[idx] = ipcFrameDB.
                                    WHEN "AuditTable" THEN
                                    dynParamValue.paramValue[idx] = ipcFrameFile.
                                    WHEN "AuditField" THEN
                                    dynParamValue.paramValue[idx] = ipcFrameField.
                                    WHEN "AuditKey" THEN
                                    dynParamValue.paramValue[idx] = ipcAuditKey.
                                END CASE.
                            END. /* do idx */
                            FIND CURRENT dynParamValue NO-LOCK.
                        END. /* if results exist */
                        ELSE
                        opcErrorMsg = "No Audit Field History Exists".
                    END. /* if avail */
                    ELSE
                    opcErrorMsg = "User Dynamic Parameter Value for Audit Field Lookup Record does not Exist".
                END. /* if can-find */
                ELSE
                opcErrorMsg = "Invalid Dynamic Subject ~""
                            + STRING(iSubjectID)
                            + "~" set in NK1 DynAuditField"
                            .
            END. /* nk1 turned on */
            ELSE
            opcErrorMsg = "NK1 DynAuditField is not activated to view Audit Field History".
        END. /* audit of table on */
        ELSE
        opcErrorMsg = "Auditing for Table ~""
                    + ipcFrameFile
                    + "~" not Enabled"
                    .
    END. /* if licensed */
    ELSE
    opcErrorMsg = "Viewing Audit Field History is not Licensed".
    
    IF opcErrorMsg NE "" AND lResults THEN
    opcErrorMsg = opcErrorMsg + CHR(10) + CHR(10)
                + "Audit Field History Exists for this Field"
                .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spGetDynParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetDynParamValue Procedure
PROCEDURE spGetDynParamValue:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprRowID     AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMsg  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bDynParamValue FOR dynParamValue.
    
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ipiSubjectID
           AND dynParamValue.user-id      EQ USERID("ASI")
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN DO:
        FIND FIRST bDynParamValue NO-LOCK
             WHERE bDynParamValue.subjectID    EQ ipiSubjectID
               AND bDynParamValue.user-id      EQ "_default"
               AND bDynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF AVAILABLE bDynParamValue THEN
        DO TRANSACTION:
            CREATE dynParamValue.
            BUFFER-COPY bDynParamValue TO dynParamValue
                ASSIGN
                    dynParamValue.user-id          = USERID("ASI")
                    dynParamValue.paramDescription = "User Default"
                    dynParamValue.outputFormat     = "Grid"
                    oprRowID                       = ROWID(dynParamValue)
                    .
            FIND CURRENT dynParamValue NO-LOCK.
        END. /* if avail */
        ELSE
        opcErrorMsg = "Default Dynamic Parameter Value for Audit Field Lookup Record does not Exist".
    END. /* if not avail */
    ELSE
    oprRowID = ROWID(dynParamValue).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spGetLookupTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetLookupTitle Procedure
PROCEDURE spGetLookupTitle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcLookupTitle AS CHARACTER NO-UNDO.
    
    ASSIGN
        opcLookupTitle = cLookupTitle
        cLookupTitle   = ?
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spGetSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetSession Procedure
PROCEDURE spGetSessionParam:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcSessionParam AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSessionValue AS CHARACTER NO-UNDO.
    
    FIND FIRST ttSessionParam
         WHERE ttSessionParam.sessionParam EQ ipcSessionParam
         NO-ERROR.
    IF AVAILABLE ttSessionParam THEN
    opcSessionValue = ttSessionParam.sessionValue.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spGetTaskFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetTaskFilter Procedure
PROCEDURE spGetTaskFilter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcMnemonic  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcProgramID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserID    AS CHARACTER NO-UNDO.
    
    ASSIGN
        opcMnemonic  = cMnemonic
        opcProgramID = cProgramID
        opcUserID    = cUserID
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spInactivateCueCards) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spInactivateCueCards Procedure
PROCEDURE spInactivateCueCards:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCueType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCueCard     FOR cueCard.
    DEFINE BUFFER bCueCardText FOR cueCardText.
    
    FOR EACH bCueCardText NO-LOCK,
        FIRST bCueCard NO-LOCK
        WHERE bCueCard.cueType EQ ipcCueType
        :
        IF CAN-FIND(FIRST xCueCard
                    WHERE xCueCard.user_id   EQ USERID("ASI")
                      AND xCueCard.cueType   EQ bCueCard.cueType
                      AND xCueCard.cueTextID EQ bCueCardText.cueTextID) THEN
        NEXT.
        CREATE xCueCard.
        ASSIGN
            xCueCard.user_id   = USERID("ASI")
            xCueCard.cueType   = bCueCard.cueType
            xCueCard.cueTextID = bCueCardText.cueTextID
            .
        RELEASE xCueCard.
    END. /* each bcuecardtext */

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spSendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSendEmail Procedure
PROCEDURE spSendEmail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcConfigID          AS INTEGER   NO-UNDO. /* Mandatory - This is configID value as stored in emailConfig table */
    DEFINE INPUT PARAMETER ipcRecipientsSendTo  AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.recipientsSendTo value */
    DEFINE INPUT PARAMETER ipcRecipientsReplyTo AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.recipientsReplyTo value */
    DEFINE INPUT PARAMETER ipcRecipientsSendCC  AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.recipientsSendCC value */
    DEFINE INPUT PARAMETER ipcRecipientsSendBCC AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.recipientsSendBCC value */
    DEFINE INPUT PARAMETER ipcSubject           AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.subject value */
    DEFINE INPUT PARAMETER ipcBody              AS CHARACTER NO-UNDO. /* Optional(Override) - This overrides emailConfig.body value */
    DEFINE INPUT PARAMETER ipcAttachment        AS CHARACTER NO-UNDO. /* Optional - This is the full file path to be attached */

    DEFINE VARIABLE cMail              AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE idx                AS INTEGER    NO-UNDO.
    DEFINE VARIABLE objOutlook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE objOutlookAttach   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE objOutlookMsg      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE objOutlookRecip    AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE cAttachments       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRecipientsSendTo  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRecipientsSendCC  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRecipientsSendBCC AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRecipientsReplyTo AS CHARACTER  NO-UNDO.
      
    ASSIGN
        FILE-INFO:FILE-NAME  = SEARCH("CMail.exe")
        cMail                = FILE-INFO:FULL-PATHNAME
        FILE-INFO:FILE-NAME  = ipcAttachment
        ipcAttachment        = FILE-INFO:FULL-PATHNAME
        .    
    FIND FIRST emailConfig NO-LOCK
         WHERE emailConfig.configID EQ ipcConfigID
           AND isActive
         NO-ERROR.        

    /* Sends email only if configID is available in emailConfig table */
    IF AVAILABLE emailConfig AND emailConfig.smtpServer NE "" THEN DO:
   
        /* If value for input recipientsinTo is null, then gets value from emailConfig table */
        IF ipcRecipientsSendTO EQ "" THEN
            ipcRecipientsSendTO = emailConfig.recipientsSendTO.
        
     /* If value for recipientsinTo is null in emailConfig table, then code execution stops */
        IF ipcRecipientsSendTO EQ "" THEN
            RETURN.
		
        /* If value for input recipientsinBCC is null, then gets value from emailConfig table */
        IF ipcRecipientsSendBCC EQ "" THEN
            ipcRecipientsSendBCC = emailConfig.recipientsSendBCC.
        
     /* If value for input recipientsinCC is null, then gets value from emailConfig table */			
        IF ipcRecipientsSendCC EQ "" THEN
            ipcRecipientsSendCC = emailConfig.recipientsSendCC.
        
     /* If value for input recipientsinReplyTo is null, then gets value from emailConfig table */			
        IF ipcRecipientsReplyTo EQ "" THEN
            ipcRecipientsReplyTo = emailConfig.recipientsReplyTo.
        
        ASSIGN
            ipcRecipientsSendTO  = TRIM(REPLACE(ipcRecipientsSendTO,";",","))
            ipcRecipientsSendCC  = TRIM(REPLACE(ipcRecipientsSendCC,";",","))
            ipcRecipientsSendBCC = TRIM(REPLACE(ipcRecipientsSendBCC,";",","))
            ipcRecipientsReplyTo = TRIM(REPLACE(ipcRecipientsReplyTo,";",","))
            .
            
     /* If value for input body is null, then gets value from emailConfig table */
        IF ipcBody EQ "" THEN
            ipcBody = emailConfig.body.
        
     /* If value for input subject is null, then gets value from emailConfig table */
        IF ipcSubject EQ "" THEN
            ipcSubject = emailConfig.subject.
        
     /* If value for input attachment is valid, then only attachment will be sent in email  */
        IF ipcAttachment NE ? THEN
            cAttachments = cAttachments + " -a:" + ipcAttachment.   
		
     /* cMail don't supports adding multiple recipients to a single (to/cc/bcc/reply-to) field. 
           Only one recipient can be added to a single (to/cc/bcc/reply-to) field. 
	 So,This setting may be required multiple times based on the number of receipients */
        DO idx = 1 TO NUM-ENTRIES(ipcRecipientsSendTO):
            cRecipientsSendTo = cRecipientsSendTo + " -to:" + ENTRY(idx,ipcRecipientsSendTO).
        END. /*do idx*/

        IF ipcRecipientsSendCC NE "" THEN
            DO idx = 1 TO NUM-ENTRIES(ipcRecipientsSendCC):
                cRecipientsSendCC = cRecipientsSendCC + " -cc:" + ENTRY(idx,ipcRecipientsSendCC).
            END. /* do idx */
            
        IF ipcRecipientsSendBCC NE "" THEN
            DO idx = 1 TO NUM-ENTRIES(ipcRecipientsSendBCC):
                cRecipientsSendBCC = cRecipientsSendBCC + " -bcc:" + ENTRY(idx,ipcRecipientsSendBCC).
            END. /* do idx */
            
        IF ipcRecipientsReplyTo NE "" THEN
            DO idx = 1 TO NUM-ENTRIES(ipcRecipientsReplyTo):
                cRecipientsReplyTo = cRecipientsReplyTo + " -reply-to:" + ENTRY(idx,ipcRecipientsReplyTo).
            END. /* do idx */

        cMail = cMail + " -host:"
              + emailConfig.smtpUser + ":" + emailConfig.smtpPassword
              + "@" + emailConfig.smtpServer + ":" + STRING(emailConfig.smtpPort)
              + " -starttls" + cAttachments
              + " ~"-subject:" + ipcSubject + "~""
              + " ~"-body:" + ipcBody + "~""
              + " -from:" + emailConfig.smtpUser
              + cRecipientsSendTo
              + cRecipientsSendCC
              + cRecipientsSendBCC
              + cRecipientsReplyTo
              .
        OS-COMMAND SILENT VALUE(cMail).
    END.
    ELSE DO:
        CREATE "Outlook.Application" objOutlook.
        objOutlookMsg = objOutlook:CreateItem(0).
        DO idx = 1 TO NUM-ENTRIES(ipcRecipientsSendTO):
            objOutlookRecip = objOutlookMsg:Recipients:Add(ENTRY(idx,ipcRecipientsSendTO)).
        END. /* do idx */
        objOutlookRecip:Type  = 1.
        objOutlookMsg:Subject = ipcSubject.
        objOutlookMsg:Body    = ipcBody.    
        objOutlookMsg:Attachments:Add(ipcAttachment).
        objOutlookRecip:Resolve.
        objOutlookMsg:Send.
      /*objOutlook:Quit().*/
        RELEASE OBJECT objOutlook.
        RELEASE OBJECT objOutlookMsg.
        RELEASE OBJECT objOutlookRecip.
    END. /* else */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spSetDontShowAgain) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetDontShowAgain Procedure
PROCEDURE spSetDontShowAgain:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    IF cueCard.cueType EQ "Message" THEN DO TRANSACTION:
        FIND CURRENT cueCardText EXCLUSIVE-LOCK.
        DELETE cueCardText.
    END. /* if message */
    ELSE DO TRANSACTION:
        CREATE xCueCard.
        ASSIGN
            xCueCard.user_id   = USERID("ASI")
            xCueCard.cueType   = cueCard.cueType
            xCueCard.cueTextID = cueCardText.cueTextID
            .
        RELEASE xCueCard.
    END. /* else */
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spSetDismiss) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetDismiss Procedure
PROCEDURE spSetDismiss:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bCueCard     FOR cueCard.
    DEFINE BUFFER bCueCardText FOR cueCardText.
    
    FOR EACH bCueCardText NO-LOCK
        WHERE bCueCardText.cueID EQ cueCardText.cueID,
        FIRST bCueCard NO-LOCK
        WHERE bCueCard.cueType   EQ cueCard.cueType
          AND bCueCard.cueID     EQ cueCardText.cueID
        :
        IF CAN-FIND(FIRST xCueCard
                    WHERE xCueCard.user_id   EQ USERID("ASI")
                      AND xCueCard.cueType   EQ bCueCard.cueType
                      AND xCueCard.cueTextID EQ bCueCardText.cueTextID) THEN
        NEXT.
        CREATE xCueCard.
        ASSIGN
            xCueCard.user_id   = USERID("ASI")
            xCueCard.cueType   = bCueCard.cueType
            xCueCard.cueTextID = bCueCardText.cueTextID
            .
        RELEASE xCueCard.
    END. /* each bcuecardtext */
    iCueOrder = 99999.
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spNextCue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spNextCue Procedure 
PROCEDURE spNextCue :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        iCueOrder = iCueOrder + 1
        lNext     = YES 
        .
    
    APPLY "U1":U TO iphWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spPrevCue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spPrevCue Procedure 
PROCEDURE spPrevCue :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        iCueOrder = iCueOrder - 1
        lNext     = NO
        .
    IF iCueOrder LT 1 THEN
    iCueOrder = ?.
    
    APPLY "U1":U TO iphWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spRunCueCard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spRunCueCard Procedure 
PROCEDURE spRunCueCard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCueType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphContainer AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iplActive    AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cCueCardPool        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCueCardFrame       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardRectangle   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hClose              AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardArrow       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardPrev        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardNext        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardText        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hDismiss            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hDontShowAgain      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dCol                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRow                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTitle              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iPosition           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrientation        AS CHARACTER NO-UNDO INITIAL ~
"default_LeftUp,default_Up,default_RightUp,default_Right,default_RightDown,default_Down,~
default_LeftDown,default_Left,information,default_SidebarCollapse,default_SidebarExpand".
    
    IF NOT VALID-HANDLE(iphContainer) THEN RETURN.
    IF NOT VALID-HANDLE(iphFrame) THEN RETURN.
    IF iphFrame:SENSITIVE EQ NO THEN RETURN.    
    IF lCueCardActive THEN RETURN.
    
    cCueCardPool = "CueCardPool" + STRING(TIME,"99999").
    DELETE WIDGET-POOL cCueCardPool NO-ERROR.
    CREATE WIDGET-POOL cCueCardPool NO-ERROR.

    /* check for active cue card */
    IF CAN-FIND(FIRST cueCard
                WHERE cueCard.cuePrgmName EQ ipcPrgmName
                  AND cueCard.cueType     EQ ipcCueType
                  AND cueCard.isActive    EQ YES) THEN 
    FOR EACH cueCard NO-LOCK
        WHERE cueCard.cuePrgmName EQ ipcPrgmName
          AND cueCard.cueType     EQ ipcCueType
          AND cueCard.isActive    EQ YES
        :
        ASSIGN
            iCueOrder = 1
            dTitle    = IF iphFrame:TITLE EQ ? THEN 0 ELSE 1
            lNext     = YES
            .
        /* check to be sure there are active cue card texts */
        IF CAN-FIND(FIRST cueCardText
                    WHERE cueCardText.cueID       EQ cueCard.cueID
                      AND cueCardText.cueType     EQ cueCard.cueType
                      AND cueCardText.isActive    EQ YES
                      AND (cueCardText.createdFor EQ USERID("ASI")
                       OR  cueCardText.createdFor EQ "")) THEN
        DO WHILE TRUE:
            /* done, no more cue card texts */
            IF iCueOrder EQ ? THEN LEAVE.
            FIND FIRST cueCardText NO-LOCK
                 WHERE cueCardText.cueID       EQ cueCard.cueID
                   AND cueCardText.cueType     EQ cueCard.cueType
                   AND cueCardText.cueOrder    EQ iCueOrder
                   AND (cueCardText.createdFor EQ USERID("ASI")
                    OR  cueCardText.createdFor EQ "")
                 NO-ERROR.
            /* if can't find, done, no more cue card texts */
            IF NOT AVAILABLE cueCardText THEN LEAVE.
            /* cue card not active, get next card in sequence */
            /* or check if user has blocked this cue card text */
            IF NOT cueCardText.isActive OR
              (iplActive EQ YES AND 
               CAN-FIND(FIRST xCueCard
                        WHERE xCueCard.user_id   EQ USERID("ASI")
                          AND xCueCard.cueType   EQ cueCardText.cueType
                          AND xCueCard.cueTextID EQ cueCardText.cueTextID)) THEN DO:
                IF lNext THEN
                RUN spNextCue (hMainMenuHandle).
                ELSE
                RUN spPrevCue (hMainMenuHandle).
                NEXT.
            END.
            /* check security level of user vs cue card security */
            IF sfUserSecurityLevel() LT cueCard.securityLevel THEN
            DO TRANSACTION:
                CREATE xCueCard.
                ASSIGN
                    xCueCard.user_id   = USERID("ASI")
                    xCueCard.cueType   = cueCard.cueType
                    xCueCard.cueTextID = cueCardText.cueTextID
                    .
                RELEASE xCueCard.
                NEXT.
            END. /* if securitylevel */
            /* calculate the cue card screen position */
            CASE cueCardText.cuePosition:
                /* arrows: 1=absolute */
                WHEN 1 THEN
                ASSIGN 
                    dCol = cueCardText.frameCol
                    dRow = cueCardText.frameRow - dTitle
                    .
                /* arrows: 2=width */
                WHEN 2 THEN 
                ASSIGN 
                    dCol = iphFrame:WIDTH - cueCardText.frameCol
                    dRow = cueCardText.frameRow - dTitle
                    .
                /* arrows: 3=height & width */
                WHEN 3 THEN 
                ASSIGN 
                    dCol = iphFrame:WIDTH  - cueCardText.frameCol
                    dRow = iphFrame:HEIGHT - cueCardText.frameRow - dTitle
                    .
                /* arrows: 4=height */
                WHEN 4 THEN  
                ASSIGN 
                    dCol = cueCardText.frameCol
                    dRow = iphFrame:HEIGHT - cueCardText.frameRow - dTitle
                    .
            END CASE.
            IF dRow LT 1 THEN dRow = 1.
            /* create cue card objects */
            /* FRAME */
            CREATE FRAME hCueCardFrame IN WIDGET-POOL cCueCardPool
                ASSIGN 
                    PARENT = iphContainer
                    FRAME = iphFrame
                    NAME = "CueCardFrame"
                    COL = dCol
                    ROW = dRow
                    HEIGHT = cueCardText.frameHeight
                    WIDTH = cueCardText.frameWidth
                    BOX = NO 
                    BGCOLOR = cueCardText.frameBGColor
                    FGCOLOR = cueCardText.frameFGColor
                    HIDDEN = NO 
                    SENSITIVE = YES
              TRIGGERS:
                ON ENTRY
                  PERSISTENT RUN spCueCardFrame IN THIS-PROCEDURE (hCueCardFrame).
              END TRIGGERS.
            hCueCardFrame:MOVE-TO-TOP().
            /* RECTANGLE */
            CREATE RECTANGLE hCueCardRectangle IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "CueCardRect"
                    COL = 1
                    ROW = 1
                    HEIGHT = hCueCardFrame:HEIGHT - .1
                    WIDTH = hCueCardFrame:WIDTH - .3
                    ROUNDED = YES
                    GRAPHIC-EDGE = YES
                    EDGE-PIXELS = 1
                    SENSITIVE = NO
                    HIDDEN = NO 
                    .
            hCueCardRectangle:MOVE-TO-TOP().
            /* CLOSE BUTTON */
            CREATE BUTTON hClose IN WIDGET-POOL cCueCardPool
                ASSIGN 
                    FRAME = hCueCardFrame
                    NAME = "btnClose"
                    COL = hCueCardFrame:WIDTH - 4.4 
                    ROW = 1.24
                    TOOLTIP = "Close Cue Card"
                    FLAT-BUTTON = YES
                    HEIGHT = 1
                    WIDTH = 4.2
                    HIDDEN = NO 
                    SENSITIVE = YES
              TRIGGERS:
                ON CHOOSE
                  PERSISTENT RUN spCueCardClose IN THIS-PROCEDURE (hMainMenuHandle).
              END TRIGGERS.
            hClose:LOAD-IMAGE("Graphics\16x16\delete.gif").
            hClose:MOVE-TO-TOP().
            /* ARROW IMAGE */
            CREATE IMAGE hCueCardArrow IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "ArrowImage"
                    COL = cueCardText.arrowCol
                    ROW = cueCardText.arrowRow
                    SENSITIVE = NO
                    HIDDEN = NO 
                    WIDTH = 7
                    HEIGHT = 1.67
                    TRANSPARENT = YES
                    .
            hCueCardArrow:LOAD-IMAGE("Graphics\24x24\" + ENTRY(cueCardText.cueOrientation,cOrientation) + ".gif").
            hCueCardArrow:MOVE-TO-TOP().
            /* PREVIOUS BUTTON */
            CREATE BUTTON hCueCardPrev IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "btnPrev"
                    COL = cueCardText.prevCol
                    ROW = cueCardText.prevRow
                    TOOLTIP = "Show Previous Cue Card"
                    FLAT-BUTTON = YES
                    SENSITIVE = YES 
                    HIDDEN = NO 
                    WIDTH = 5
                    HEIGHT = 1.19
              TRIGGERS:
                ON CHOOSE
                  PERSISTENT RUN spPrevCue IN THIS-PROCEDURE (hMainMenuHandle).
              END TRIGGERS.
            hCueCardPrev:LOAD-IMAGE("Graphics\24x24\" + ENTRY(10,cOrientation) + ".gif").
            hCueCardPrev:MOVE-TO-TOP().
            /* NEXT BUTTON */
            CREATE BUTTON hCueCardNext IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "btnNext"
                    COL = cueCardText.nextCol
                    ROW = cueCardText.nextRow
                    TOOLTIP = "Show Next Cue Card"
                    FLAT-BUTTON = YES
                    SENSITIVE = YES 
                    HIDDEN = NO 
                    WIDTH = 5
                    HEIGHT = 1.19
              TRIGGERS:
                ON CHOOSE
                  PERSISTENT RUN spNextCue IN THIS-PROCEDURE (hMainMenuHandle).
              END TRIGGERS.
            hCueCardNext:LOAD-IMAGE("Graphics\24x24\" + ENTRY(11,cOrientation) + ".gif").
            hCueCardNext:MOVE-TO-TOP().
            /* EDITOR */
            CREATE EDITOR hCueCardText IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "CueCardText"
                    FONT = cueCardText.textFont
                    COL = cueCardText.textCol
                    ROW = cueCardText.textRow
                    WIDTH = cueCardText.textWidth
                    HEIGHT = cueCardText.textHeight
                    BGCOLOR = cueCardText.frameBGColor
                    FGCOLOR = cueCardText.frameFGColor
                    SCROLLBAR-HORIZONTAL = NO
                    SCROLLBAR-VERTICAL = NO
                    WORD-WRAP = YES
                    READ-ONLY = YES
                    BOX = NO
                    SCREEN-VALUE = cueCardText.cueText
                    SENSITIVE = YES
                    HIDDEN = NO
                    .
            hCueCardText:MOVE-TO-TOP().
            /* TOGGLE BOX DON'T SHOW AGAIN */
            CREATE TOGGLE-BOX hDontShowAgain IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "DontShowAgain"
                    LABEL = "Don't Show Again"
                    FONT = cueCardText.dontShowAgainFont
                    COL = cueCardText.dontShowAgainCol
                    ROW = cueCardText.dontShowAgainRow
                    SENSITIVE = YES
                    HIDDEN = NO 
                    WIDTH = 24
                    HEIGHT = .81
                    SCREEN-VALUE = "NO"
              TRIGGERS:
                ON VALUE-CHANGED
                  PERSISTENT RUN spSetDontShowAgain IN THIS-PROCEDURE (hMainMenuHandle).
              END TRIGGERS.
            hDontShowAgain:MOVE-TO-TOP().
            ASSIGN
                hDontShowAgain:SENSITIVE = iplActive
                hDontShowAgain:HIDDEN    = NOT cueCard.enableDontShowAgain
                .
            /* TOGGLE BOX DISMISS */
            CREATE TOGGLE-BOX hDismiss IN WIDGET-POOL cCueCardPool
                ASSIGN
                    FRAME = hCueCardFrame
                    NAME = "Dismiss"
                    LABEL = "Dismiss This Cue Card Set"
                    FONT = cueCardText.dismissFont
                    COL = cueCardText.dismissCol
                    ROW = cueCardText.dismissRow
                    SENSITIVE = YES
                    HIDDEN = NO 
                    WIDTH = 34
                    HEIGHT = .81
                    SCREEN-VALUE = "NO"
              TRIGGERS:
                ON VALUE-CHANGED
                  PERSISTENT RUN spSetDismiss IN THIS-PROCEDURE (hMainMenuHandle).
              END TRIGGERS.
            hDismiss:MOVE-TO-TOP().
            ASSIGN 
                hDismiss:SENSITIVE = iplActive
                hDismiss:HIDDEN    = NOT cueCard.enableDismiss
                lCueCardActive     = YES
                .            
            WAIT-FOR "U1":U OF hMainMenuHandle.
            lCueCardActive = NO.            
            DELETE OBJECT hCueCardFrame.
        END. /* do while */
    END. /* each cuecard */
    DELETE WIDGET-POOL cCueCardPool NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spSetSessionParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetSessionParam Procedure
PROCEDURE spSetSessionParam:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcSessionParam AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSessionValue AS CHARACTER NO-UNDO.
    
    FIND FIRST ttSessionParam
         WHERE ttSessionParam.sessionParam EQ ipcSessionParam
         NO-ERROR.
    IF NOT AVAILABLE ttSessionParam THEN DO:
        CREATE ttSessionParam.
        ttSessionParam.sessionParam = ipcSessionParam.
    END. /* if not avail */
    ttSessionParam.sessionValue = ipcSessionValue.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spSetTaskFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetTaskFilter Procedure
PROCEDURE spSetTaskFilter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMnemonic  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProgramID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID    AS CHARACTER NO-UNDO.
    
    ASSIGN
        cMnemonic  = ipcMnemonic
        cProgramID = ipcProgramID
        cUserID    = ipcUserID
        .
    CASE cMnemonic:
        WHEN "BR" THEN
        cMnemonic = "SS".
        WHEN "DR" THEN
        cMnemonic = "DC".
        WHEN "ER" THEN
        cMnemonic = "EQ".
        WHEN "FR" THEN
        cMnemonic = "FT".
        WHEN "GR" THEN
        cMnemonic = "GL".
        WHEN "HR" THEN
        cMnemonic = "HS".
        WHEN "IR" THEN
        cMnemonic = "FG".
        WHEN "JR" THEN
        cMnemonic = "JC".
        WHEN "MR" THEN
        cMnemonic = "RM".
        WHEN "NR" THEN
        cMnemonic = "NS".
        WHEN "OR" THEN
        cMnemonic = "OE".
        WHEN "PR" THEN
        cMnemonic = "PO".
        WHEN "QL" THEN
        cMnemonic = "QR".
        WHEN "TR" THEN
        cMnemonic = "TS".
        WHEN "VR" THEN
        cMnemonic = "AP".
    END CASE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spTtPermissions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spTtPermissions Procedure
PROCEDURE spTtPermissions:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cMnemonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParent   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    FIND FIRST prgrms NO-LOCK
         WHERE ROWID(prgrms) EQ iprRowID
         NO-ERROR.
    IF NOT AVAILABLE prgrms OR CAN-DO("primflds.,sysCtrlU.,user_dir.",prgrms.prgmName) THEN
    RETURN.
    ASSIGN
        cMnemonic = prgrms.mnemonic
        cParent   = prgrms.prgmName
        .
    RUN spCreateTtPermissions (ROWID(prgrms), cMnemonic, cParent, idx).
    FOR EACH prgrms NO-LOCK
        WHERE CAN-DO(prgrms.mfgroup, cParent)
        :
        idx = idx + 1.
        RUN spCreateTtPermissions (ROWID(prgrms), cMnemonic, cParent, idx).
    END. /* each bprgrms */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCueCardActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCueCardActive Procedure
FUNCTION fCueCardActive RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN lCueCardActive.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetBeginSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetBeginSearch Procedure
FUNCTION sfGetBeginSearch RETURNS CHARACTER 
  ( INPUT ipcString AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iAstPos AS INTEGER NO-UNDO.
 
  iAstPos = INDEX(ipcString, "*").

  IF iAstPos GT 0 THEN 
     cResult = SUBSTRING(ipcString, 1, iAstPos - 1).
  ELSE
     cResult = ipcString.

  RETURN cResult.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfGetTtPermissionsHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetTtPermissionsHandle Procedure
FUNCTION sfGetTtPermissionsHandle RETURNS HANDLE 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN TEMP-TABLE ttPermissions:HANDLE.
    
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfWebCharacters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfWebCharacters Procedure
FUNCTION sfWebCharacters RETURNS CHARACTER 
  (ipcWebString AS CHARACTER, ipiLevel AS INTEGER, ipcType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: remove special characters with escape values
 Notes: 1=& (ampersand)
        2=' (single quote)
        3=" (double quote)
        4=< (less than)
        5=> (greater than)
        6=\ (back slash)
        7=/ (forward slash)
------------------------------------------------------------------------------*/
	DEFINE VARIABLE cWebString AS CHARACTER NO-UNDO.

    cWebString = ipcWebString.
    IF ipiLevel GE 1 THEN
    cWebString = REPLACE(cWebString,"~&",IF ipcType EQ "Web" THEN "~&amp;"  ELSE "").
    IF ipiLevel GE 2 THEN
    cWebString = REPLACE(cWebString,"~'",IF ipcType EQ "Web" THEN "~&apos;" ELSE "").
    IF ipiLevel GE 3 THEN
    cWebString = REPLACE(cWebString,"~"",IF ipcType EQ "Web" THEN "~&quot;" ELSE "").
    IF ipiLevel GE 4 THEN
    cWebString = REPLACE(cWebString,"<", IF ipcType EQ "Web" THEN "~&lt;"   ELSE "").
    IF ipiLevel GE 5 THEN
    cWebString = REPLACE(cWebString,">", IF ipcType EQ "Web" THEN "~&gt;"   ELSE "").
    IF ipiLevel GE 6 THEN
    cWebString = REPLACE(cWebString,"~\",IF ipcType EQ "Web" THEN "~\~\"    ELSE "").
    IF ipiLevel GE 7 THEN
    cWebString = REPLACE(cWebString,"~/",IF ipcType EQ "Web" THEN "~/~/"    ELSE "").

	RETURN cWebString.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfClearUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfClearUsage Procedure 
FUNCTION sfClearUsage RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttPermissions.
    EMPTY TEMP-TABLE ttSysCtrlUsage.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetMainMenuHandle Procedure 
FUNCTION sfGetMainMenuHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN hMainMenuHandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetNextRecKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetNextRecKey Procedure 
FUNCTION sfGetNextRecKey RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN STRING(YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(DAY(TODAY),"99")
         + STRING(TIME,"99999")
         + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
         .
         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetSysCtrlUsageHandle Procedure 
FUNCTION sfGetSysCtrlUsageHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        RETURN hSysCtrlUsageHandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetTtSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetTtSysCtrlUsageHandle Procedure 
FUNCTION sfGetTtSysCtrlUsageHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN TEMP-TABLE ttSysCtrlUsage:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfSetMainMenuHandle Procedure 
FUNCTION sfSetMainMenuHandle RETURNS LOGICAL
  (iphMainMenuHandle AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    hMainMenuHandle = iphMainMenuHandle.
    
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfSetSysCtrlUsageHandle Procedure 
FUNCTION sfSetSysCtrlUsageHandle RETURNS LOGICAL
  (iphSysCtrlUsageHandle AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    hSysCtrlUsageHandle = iphSysCtrlUsageHandle.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfUserSecurityLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfUserSecurityLevel Procedure 
FUNCTION sfUserSecurityLevel RETURNS INTEGER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*    RETURN 9999. /* do not remove, used by rstark to access subjectID 0 */*/
    RETURN IF AVAILABLE users THEN users.securityLevel ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
