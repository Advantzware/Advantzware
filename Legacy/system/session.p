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

DEFINE VARIABLE hSysCtrlUsageHandle AS HANDLE NO-UNDO.

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

&IF DEFINED(EXCLUDE-sfClearSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfClearSysCtrlUsage Procedure
FUNCTION sfClearTtSysCtrlUsage RETURNS LOGICAL 
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


&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetSysCtrlUsageHandle Procedure
FUNCTION sfGetTtSysCtrlUsageHandle RETURNS HANDLE 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfSetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfSetSysCtrlUsageHandle Procedure
FUNCTION sfSetSysCtrlUsageHandle RETURNS LOGICAL 
  (iphSysCtrlUsageHandle AS HANDLE ) FORWARD.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-pCreateSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreateSysCtrlUsage Procedure
PROCEDURE spCreateSysCtrlUsage:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcModule      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCharFld     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDateFld    AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecFld      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiIntFld      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplLogFld      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescrip     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtUsageNow   AS DATETIME  NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCustVend    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustVendNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSeqNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipId      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubCategory AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSysCtrlID   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTypeCode    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStackTrace  AS CHARACTER NO-UNDO.

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
/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-sfClearSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfClearSysCtrlUsage Procedure
FUNCTION sfClearTtSysCtrlUsage RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
	EMPTY TEMP-TABLE ttSysCtrlUsage.

	RETURN TRUE.

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


&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetSysCtrlUsageHandle Procedure
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

