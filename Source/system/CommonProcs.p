&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : CommonProcs.p
    Purpose     : Super Procedure for Common Procedures/Functions

    Syntax      : RUN system\CommonProcs.p PERSISTENT SET hProc.
                  SESSION:ADD-SUPER-PROCEDURE (hProc).

    Description : common procedures/functions

    Author(s)   : Ron Stark
    Created     : 11.5.2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE lUserAMPM           AS LOGICAL   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-sfCommon_GetNumberOfDaysInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_GetNumberOfDaysInMonth Procedure
FUNCTION sfCommon_GetNumberOfDaysInMonth RETURNS INTEGER 
  (ipcMonth AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF
&IF DEFINED(EXCLUDE-sfCommon_HideAMPM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_HideAMPM Procedure
FUNCTION sfCommon_HideAMPM RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_HourMax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_HourMax Procedure
FUNCTION sfCommon_HourMax RETURNS INTEGER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfCommon_HourMin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_HourMin Procedure
FUNCTION sfCommon_HourMin RETURNS INTEGER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfCommon_TimeDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_TimeDisplay Procedure
FUNCTION sfCommon_TimeDisplay RETURNS CHARACTER 
  (ipiTime AS INTEGER,
   iplClockTime AS LOGICAL,
   iplSeconds AS LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfCommon_UserAMPM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_UserAMPM Procedure
FUNCTION sfCommon_UserAMPM RETURNS LOGICAL 
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
lUserAMPM = AVAILABLE users AND users.AMPM.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-spCommon_ParseTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_ParseTime Procedure
PROCEDURE spCommon_ParseTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiTime    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iphHour    AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iphMinute  AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iphSeconds AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iphAMPM    AS HANDLE  NO-UNDO.

    DEFINE VARIABLE cTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx   AS INTEGER   NO-UNDO.

    ASSIGN
/*        idx   = IF lUserAMPM THEN 1  ELSE 0*/
        jdx   = IF lUserAMPM THEN 12 ELSE 24
        cTime = IF lUserAMPM THEN STRING(ipiTime,"HH:MM:SS AM")
                ELSE STRING(ipiTime,"HH:MM:SS")
              .    
    IF iphHour:TYPE EQ "COMBO-BOX" THEN DO:
        iphHour:LIST-ITEMS = " ".
        DO kdx = jdx TO idx BY -1:
            iphHour:LIST-ITEMS = iphHour:LIST-ITEMS + STRING(kdx,"99") + ",".
        END. /* do kdx */
        iphHour:LIST-ITEMS = TRIM(iphHour:LIST-ITEMS,",").
    END. /* if combo-box */

    IF VALID-HANDLE(iphHour) THEN
    iphHour:SCREEN-VALUE = SUBSTRING(cTime,1,2).
    IF VALID-HANDLE(iphMinute) THEN
    iphMinute:SCREEN-VALUE = SUBSTRING(cTime,4,2).
    IF VALID-HANDLE(iphSeconds) THEN
    iphSeconds:SCREEN-VALUE = SUBSTRING(cTime,7,2).
    IF VALID-HANDLE(iphAMPM) THEN DO:
        IF iphAMPM:TYPE EQ "BUTTON" THEN
        iphAMPM:LABEL = IF lUserAMPM THEN SUBSTRING(cTime,10,2) ELSE "".
        ELSE
        iphAMPM:SCREEN-VALUE = IF lUserAMPM THEN SUBSTRING(cTime,10,2) ELSE "".
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-sfCommon_GetNumberOfDaysInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_GetNumberOfDaysInMonth Procedure
FUNCTION sfCommon_GetNumberOfDaysInMonth RETURNS INTEGER 
    ( ipcMonth AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: return day in a month
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cDaya AS INTEGER EXTENT 12 INIT [31,28,31,30,31,30,31,31,30,31,30,31] NO-UNDO .
    DEFINE VARIABLE iReturn AS INTEGER NO-UNDO .

    IF ipcMonth NE 2 THEN iReturn = cDaya[ipcMonth].
    ELSE IF YEAR(TODAY) / 4   NE 0 THEN iReturn = cDaya[2].
    ELSE IF YEAR(TODAY) / 100 EQ 0 AND YEAR(TODAY) / 400 NE 0 THEN iReturn = cDaya[2].
    ELSE iReturn = cDaya[2] + 1.
 
    RETURN iReturn.   

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-sfCommon_HideAMPM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_HideAMPM Procedure
FUNCTION sfCommon_HideAMPM RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
	RETURN NOT lUserAMPM.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_HourMax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_HourMax Procedure
FUNCTION sfCommon_HourMax RETURNS INTEGER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
	RETURN IF lUserAMPM THEN 12 ELSE 24.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_HourMin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_HourMin Procedure
FUNCTION sfCommon_HourMin RETURNS INTEGER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
	RETURN IF lUserAMPM THEN 1 ELSE 0.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_TimeDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_TimeDisplay Procedure
FUNCTION sfCommon_TimeDisplay RETURNS CHARACTER 
  (ipiTime AS INTEGER, iplClockTime AS LOGICAL, iplSeconds AS LOGICAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFormat        AS CHARACTER NO-UNDO INITIAL "HH:MM".
	DEFINE VARIABLE opcTimeDisplay AS CHARACTER NO-UNDO.

    IF iplSeconds THEN
    cFormat = cFormat + ":SS".
    IF iplClockTime AND lUserAMPM THEN
    cFormat = cFormat + " AM".    
    opcTimeDisplay = STRING(ipiTime,cFormat).

    RETURN opcTimeDisplay.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_UserAMPM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_UserAMPM Procedure
FUNCTION sfCommon_UserAMPM RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
	RETURN lUserAMPM.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
