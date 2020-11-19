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

&IF DEFINED(EXCLUDE-sfCommon_CheckIntDecValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_CheckIntDecValue Procedure 
FUNCTION sfCommon_CheckIntDecValue RETURNS CHARACTER
  ( INPUT pcString AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_DateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_DateOptionDate Procedure 
FUNCTION sfCommon_DateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_DecimalTimeInHHMM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_DecimalDurationInHHMM Procedure
FUNCTION sfCommon_DecimalDurationInHHMM RETURNS CHARACTER 
  (ipdTimeInDecimal AS DECIMAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfCommon_GetDifferenceDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_GetDifferenceDays Procedure 
FUNCTION sfCommon_GetDifferenceDays RETURNS INTEGER
  ( ipdtTargetDate AS DATETIME, ipdtSourceDate AS DATETIME )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_GetNumberOfDaysInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_GetNumberOfDaysInMonth Procedure 
FUNCTION sfCommon_GetNumberOfDaysInMonth RETURNS INTEGER
    ( ipcMonth AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_GetWeekDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_GetWeekDay Procedure 
FUNCTION sfCommon_GetWeekDay RETURNS INTEGER
  ( ipdtDate AS DATETIME ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_GetWeekDayInText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_GetWeekDayInText Procedure 
FUNCTION sfCommon_GetWeekDayInText RETURNS CHARACTER
  ( INPUT ipiWeekDay AS INTEGER )  FORWARD.

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

&IF DEFINED(EXCLUDE-sfCommon_IsDateWeekend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_IsDateWeekend Procedure 
FUNCTION sfCommon_IsDateWeekend RETURNS LOGICAL
  ( ipdtDate AS DATETIME )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_SetDateOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_SetDateOptions Procedure 
FUNCTION sfCommon_SetDateOptions RETURNS LOGICAL
  ( iphDateOption AS HANDLE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_TimeDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfCommon_TimeDisplay Procedure 
FUNCTION sfCommon_TimeDisplay RETURNS CHARACTER
  (ipiTime AS INTEGER, iplClockTime AS LOGICAL, iplSeconds AS LOGICAL) FORWARD.

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


FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID("ASI")
     NO-ERROR.
lUserAMPM = AVAILABLE users AND users.AMPM.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pGetHoursFromDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetHoursFromDateTime Procedure 
PROCEDURE pGetHoursFromDateTime PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtInput  AS DATETIME    NO-UNDO.
    DEFINE OUTPUT PARAMETER opiHours   AS INTEGER     NO-UNDO.

    DEFINE VARIABLE iMinutes AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSeconds AS INTEGER     NO-UNDO.
    
    RUN pGetTimeUnitsFromDateTime (
        INPUT  ipdtInput,
        OUTPUT opiHours,
        OUTPUT iMinutes,
        OUTPUT iSeconds
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetTimeInGMT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTimeInGMT Procedure 
PROCEDURE pGetTimeInGMT PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdttzSystemTime AS DATETIME-TZ NO-UNDO.
    DEFINE OUTPUT PARAMETER opdttzGMTTime    AS DATETIME-TZ NO-UNDO.

    DEFINE VARIABLE iCurrentTimeZone         AS INTEGER     NO-UNDO.
        
    ASSIGN
        /* Fetch system time zone. Returns the minutes behind/ahead of GMT */
        iCurrentTimeZone = TIMEZONE(ipdttzSystemTime) * -1
        /* Substract/Add minutes to get the current GMT date and  time */
        opdttzGMTTime    = ADD-INTERVAL(ipdttzSystemTime, iCurrentTimeZone, "minutes")        
        /* Set the time zone to GMT */
        opdttzGMTTIme    = DATETIME-TZ(DATE(opdttzGMTTIme), MTIME(opdttzGMTTIme), 0)
        .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetTimeUnitsFromDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTimeUnitsFromDateTime Procedure 
PROCEDURE pGetTimeUnitsFromDateTime PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDateTime  AS DATETIME    NO-UNDO.
    DEFINE OUTPUT PARAMETER opiHours      AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiMinutes    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSeconds    AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE iTotSeconds AS INTEGER     NO-UNDO.

    iTotSeconds = TRUNCATE(MTIME(ipdtDateTime) / 1000, 0).        
    
    RUN pGetTimeUnitsFromTime (
        INPUT  iTotSeconds,
        OUTPUT opiHours,
        OUTPUT opiMinutes,
        OUTPUT opiSeconds        
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetTimeUnitsFromTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTimeUnitsFromTime Procedure 
PROCEDURE pGetTimeUnitsFromTime PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiTime    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiHours   AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiMinutes AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSeconds AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE cTime AS CHARACTER   NO-UNDO.
    
    cTime = STRING(ipiTime, "hh:mm:ss").
    
    ASSIGN
        opiHours   = INTEGER(SUBSTRING(cTime, 1, 2))
        opiMinutes = INTEGER(SUBSTRING(cTime, 4, 2))
        opiSeconds = INTEGER(SUBSTRING(cTime, 7, 2))
        NO-ERROR.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCommon_DateRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_DateRule Procedure 
PROCEDURE spCommon_DateRule :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDateRuleID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipTo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtBaseDate   AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER iprBaseRowID   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iprResultRowID AS ROWID     NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdtDate       AS DATE      NO-UNDO.

    DEFINE VARIABLE cBaseField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNK1Value   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDateRuleID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iDayOfWeek  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSkipDay    AS LOGICAL   NO-UNDO EXTENT 7.

    /* attempt to derive date rule id via nk1 */
    IF ipcDateRuleID EQ ? OR ipcDateRuleID EQ "" THEN DO:
        RUN sys/ref/nk1look.p (
            ipcCompany, "DateRule", "L", NO, NO, "", "",
            OUTPUT cNK1Value, OUTPUT lFound
            ).
        IF lFound AND cNK1Value EQ "YES" THEN DO:
            /* default date rule id for all customers */
            RUN sys/ref/nk1look.p (
                ipcCompany, "DateRule", "C", NO, NO, "", "",
                OUTPUT cDateRuleID, OUTPUT lFound
                ).
            RUN sys/ref/nk1look.p (
                ipcCompany, "DateRule", "L", YES, YES, ipcCustNo, ipcShipTo,
                OUTPUT cNK1Value, OUTPUT lFound
                ).
            IF lFound AND cNK1Value EQ "YES" THEN DO:
                /* date rule id for specific customer & ship to */
                RUN sys/ref/nk1look.p (
                    ipcCompany, "DateRule", "C", YES, YES, ipcCustNo, ipcShipTo,
                    OUTPUT cDateRuleID, OUTPUT lFound
                    ).
            END. /* if found */
        END. /* if yes */
    END. /* if ne ? */
    ELSE
    cDateRuleID = ipcDateRuleID.

    /* attempt to locate date rule using id */
    IF cDateRuleID NE ? AND cDateRuleID NE "" THEN
    FIND FIRST DateRules NO-LOCK
         WHERE DateRules.dateRuleID EQ cDateRuleID
         NO-ERROR.
    /* not date rule record, bail */
    IF NOT AVAILABLE DateRules THEN RETURN.

    /* if base row id provided, get date value from database */
    IF iprBaseRowID NE ? THEN DO:
        IF DateRules.baseTable EQ "" THEN RETURN.
        IF DateRules.baseField EQ "" THEN RETURN.
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ DateRules.baseTable
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN RETURN.
        FIND FIRST ASI._field OF ASI._file NO-LOCK
             WHERE ASI._field._field-name EQ DateRules.baseField
             NO-ERROR.
        IF NOT AVAILABLE ASI._field THEN RETURN.
        IF ASI._field._data-type NE "DATE" THEN RETURN.
    
        /* obtain the base table's field value */
        CREATE QUERY hQuery.
        CREATE BUFFER hBuffer FOR TABLE DateRules.baseTable.
        hQuery:ADD-BUFFER(hBuffer).
        hQuery:QUERY-PREPARE(
            "FOR EACH " + DateRules.baseTable + " NO-LOCK " +
            "WHERE ROWID(" + DateRules.baseTable + ") = TO-ROWID(~"" +
            STRING(iprBaseRowID) + "~")"
            ).
        hQuery:QUERY-OPEN().
        hTable = hQuery:GET-BUFFER-HANDLE(DateRules.baseTable).
        hQuery:GET-FIRST().
        cBaseField = hTable:BUFFER-FIELD(DateRules.baseField):BUFFER-VALUE() NO-ERROR.
        DELETE OBJECT hBuffer.
        DELETE OBJECT hQuery.
        dtDate = DATE(cBaseField) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN.
    END.
    ELSE
    dtDate = ipdtBaseDate.

    /* calculate date based on date rules */
    /* which week days to skip */
    DO idx = 1 TO 7:
        lSkipDay[idx] = SUBSTRING(DateRules.skipDays,idx,1) EQ "Y".
    END. /* do idx */
    /* add extra day if past the time limit */
    IF DateRules.skipTime GT 0 AND DateRules.skipTime LE TIME THEN
    dtDate = dtDate + 1.
    /* calculate ending date based on above week days to skip */
    DO idx = 1 TO DateRules.days:
        DO WHILE TRUE:
            dtDate = dtDate + 1.
            /* check if a holiday */
            IF SUBSTRING(DateRules.skipDays,8,1) EQ "Y" THEN
            IF CAN-FIND(FIRST reftable
                        WHERE reftable.reftable  EQ "Holiday"
                          AND DATE(reftable.loc) EQ dtDate) THEN
            NEXT.
            /* check day of week to see if should be skipped */
            IF NOT lSkipDay[WEEKDAY(dtDate)] THEN
            LEAVE.
        END. /* while */
    END. /* do idx */
    opdtDate = dtDate.

    /* check if result table should be updated */
    IF iprResultRowID EQ ? THEN RETURN.
    IF DateRules.resultTable EQ "" THEN RETURN.
    IF DateRules.resultField EQ "" THEN RETURN.
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name EQ DateRules.resultTable
         NO-ERROR.
    IF NOT AVAILABLE ASI._file THEN RETURN.
    FIND FIRST ASI._field OF ASI._file NO-LOCK
         WHERE ASI._field._field-name EQ DateRules.resultField
         NO-ERROR.
    IF NOT AVAILABLE ASI._field THEN RETURN.
    IF ASI._field._data-type NE "DATE" THEN RETURN.

    /* obtain the result table's field to be updated with result date */
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE DateRules.resultTable.
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE(
        "FOR EACH " + DateRules.resultTable + " EXCLUSIVE-LOCK " +
        "WHERE ROWID(" + DateRules.resultTable + ") = TO-ROWID(~"" +
        STRING(iprBaseRowID) + "~")"
        ).
    hQuery:QUERY-OPEN().
    hTable = hQuery:GET-BUFFER-HANDLE(DateRules.resultTable).
    DO TRANSACTION:
        hQuery:GET-FIRST().
        hTable:BUFFER-FIELD(DateRules.resultField):BUFFER-VALUE() = STRING(opdtDate).
    END. /* do trans */
    DELETE OBJECT hBuffer.
    DELETE OBJECT hQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCommon_GetCurrentGMTTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_GetCurrentGMTTime Procedure 
PROCEDURE spCommon_GetCurrentGMTTime :
/*------------------------------------------------------------------------------
 Purpose: Returns current GMT Time
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opdttzCurrentGMTTime AS DATETIME-TZ NO-UNDO.
    
    RUN pGetTimeInGMT(
        INPUT  NOW,
        OUTPUT opdttzCurrentGMTTime
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCommon_GetHoursFromDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_GetHoursFromDateTime Procedure 
PROCEDURE spCommon_GetHoursFromDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtInput  AS DATETIME    NO-UNDO.
    DEFINE OUTPUT PARAMETER opiHours   AS INTEGER     NO-UNDO.

    RUN pGetHoursFromDateTime (
        INPUT  ipdtInput,
        OUTPUT opiHours
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCommon_ParseTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_ParseTime Procedure 
PROCEDURE spCommon_ParseTime :
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
        cTime = REPLACE(cTime," ","0")
              .    
    IF iphHour:TYPE EQ "COMBO-BOX" THEN DO:
        iphHour:LIST-ITEMS = " ".
        DO kdx = jdx TO idx BY -1:
            iphHour:LIST-ITEMS = iphHour:LIST-ITEMS + STRING(kdx,"99") + ",".
        END. /* do kdx */
        iphHour:LIST-ITEMS = LEFT-TRIM(TRIM(iphHour:LIST-ITEMS,",")).
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

&IF DEFINED(EXCLUDE-spCommon_ValidateValueByDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_ValidateValueByDataType Procedure 
PROCEDURE spCommon_ValidateValueByDataType :
/*------------------------------------------------------------------------------
     Purpose: Validate if a given value can be converted to given data type.
              Returns error in case of failure
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipcValue    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE dDecimalValidator AS DECIMAL NO-UNDO.
    DEFINE VARIABLE daDateValidator   AS DATE    NO-UNDO.
    DEFINE VARIABLE lLogicalValidator AS LOGICAL NO-UNDO.

    CASE ipcDataType:
        WHEN "DECIMAL" OR
        WHEN "INTEGER" THEN DO:
            dDecimalValidator = DECIMAL(ipcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                oplValid = FALSE.
                RETURN.
            END.
        END.
        WHEN "DATE" THEN DO:
            daDateValidator = DATE(ipcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                oplValid = FALSE.
                RETURN.
            END.
        END.
        WHEN "LOGICAL" THEN DO:
            lLogicalValidator = LOGICAL(ipcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                oplValid = FALSE.
                RETURN.
            END.        
        END.
    END CASE.
    oplValid = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCommon_CheckTableLock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCommon_CheckPostingProcess Procedure
PROCEDURE spCommon_CheckPostingProcess:
/*------------------------------------------------------------------------------
     Purpose: Validate if a given value can be converted to given data type.
              Returns error in case of failure
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipcTableName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldInProcess AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldPostType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldUserId    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldDateTime  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTypeValue      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReleaseLock    AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldInProcess AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldPostType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldUserId    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldDateTime  AS CHARACTER NO-UNDO.
    
    define variable qh as handle no-undo.
    define variable bh as handle no-undo.
    define variable fhProcess as handle no-undo.
    define variable fhType as handle no-undo.
    define variable fhUser as handle no-undo.
    define variable fhDate as handle no-undo.
  
    DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
                
    cQueryString =  "for each " + ipcTableName + " WHERE " + ipcTableName + ".company = '" + ipcFieldCompany + "'"  .
               
    create buffer bh for table ipcTableName.
    create query qh.
    qh:set-buffers( bh ).
    qh:query-prepare( cQueryString ).
    qh:query-open.
  
    do transaction:
      qh:get-first( EXCLUSIVE-LOCK ).
      fhProcess = bh:buffer-field( ipcFieldInProcess ).
      fhType = bh:buffer-field( ipcFieldPostType ).
      fhUser = bh:buffer-field( ipcFieldUserId ).
      fhDate = bh:buffer-field( ipcFieldDateTime ).
      IF NOT iplReleaseLock THEN
      DO:     
        IF fhProcess:BUFFER-VALUE EQ NO THEN
        DO:            
           opcFieldInProcess = "No" .
           ASSIGN
                fhProcess:buffer-value = "Yes"
                fhType:buffer-value = ipcTypeValue
                fhUser:buffer-value = USERID(LDBNAME(1))
                fhDate:buffer-value = NOW .       
        END.
        ELSE IF fhProcess:BUFFER-VALUE EQ YES THEN
        DO:            
           ASSIGN
            opcFieldInProcess = "Yes" 
            opcFieldPostType = fhType:buffer-value
            opcFieldUserId =  fhUser:buffer-value
            opcFieldDateTime = fhDate:BUFFER-VALUE .    
        END.       
      END.
      ELSE IF iplReleaseLock THEN
      DO:
         ASSIGN
          fhProcess:buffer-value = "NO"
          fhType:buffer-value = ""
          fhUser:buffer-value = ""
          fhDate:buffer-value = "".  
      END.       
    end.

    DELETE object bh.
    DELETE object qh. 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-sfCommon_CheckIntDecValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_CheckIntDecValue Procedure 
FUNCTION sfCommon_CheckIntDecValue RETURNS CHARACTER
  ( INPUT pcString AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE iChar AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAsc AS INTEGER NO-UNDO.

    DEFINE VARIABLE cTemp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

    DO iChar = 1 TO LENGTH(pcString):
        ASSIGN cChar = SUBSTRING(pcString,iChar,1)
                        iAsc = ASC(cChar).

        IF iAsc GT 47 AND
             iAsc LT 58 THEN
           cTemp = cTemp + cChar.
    END.

    IF (cTemp GT "") EQ TRUE THEN
        RETURN cTemp.
    ELSE
        RETURN ?. /* If no integers in the string return the unknown value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_DateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_DateOptionDate Procedure 
FUNCTION sfCommon_DateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  convert date option into date based on input date
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtDate AS DATE NO-UNDO.

    CASE ipcDateOption:
        WHEN "Fixed Date" THEN
            dtDate = ipdtDate.
        WHEN "Current Date" THEN
            dtDate = TODAY.
        WHEN "Current Date -1" THEN
            dtDate = TODAY - 1.
        WHEN "Current Date +1" THEN
            dtDate = TODAY + 1.
        WHEN "Current Date -2" THEN
            dtDate = TODAY - 2.
        WHEN "Current Date +2" THEN
            dtDate = TODAY + 2.
        WHEN "Current Date -3" THEN
            dtDate = TODAY - 3.
        WHEN "Current Date +3" THEN
            dtDate = TODAY + 3.
        WHEN "Current Date -4" THEN
            dtDate = TODAY - 4.
        WHEN "Current Date +4" THEN
            dtDate = TODAY + 4.
        WHEN "Current Date -5" THEN
            dtDate = TODAY - 5.
        WHEN "Current Date +5" THEN
            dtDate = TODAY + 5.
        WHEN "Current Date -6" THEN
            dtDate = TODAY - 6.
        WHEN "Current Date +6" THEN
            dtDate = TODAY + 6.
        WHEN "Current Date -7" THEN
            dtDate = TODAY - 7.
        WHEN "Current Date +7" THEN
            dtDate = TODAY + 7.
        WHEN "Current Date -8" THEN
            dtDate = TODAY - 8.
        WHEN "Current Date +8" THEN
            dtDate = TODAY + 8.
        WHEN "Current Date -9" THEN
            dtDate = TODAY - 9.
        WHEN "Current Date +9" THEN
            dtDate = TODAY + 9.
        WHEN "Current Date -10" THEN
            dtDate = TODAY - 10.
        WHEN "Current Date +10" THEN
            dtDate = TODAY + 10.
        WHEN "Start of This Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).
        WHEN "End of This Month" THEN
            IF MONTH(TODAY) EQ 12 THEN
            dtDate = DATE(12,31,YEAR(TODAY)).
            ELSE
            dtDate = DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.
        WHEN "First Day of Last Month" THEN
            IF MONTH(TODAY) EQ 1 THEN
            dtDate = DATE(12,1,YEAR(TODAY) - 1).
            ELSE
            dtDate = DATE(MONTH(TODAY) - 1,1,YEAR(TODAY)).
        WHEN "Last Day of Last Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
        WHEN "Start of This Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY)).
        WHEN "End of This Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY)).
        WHEN "First Day of Last Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY) - 1).
        WHEN "Last Day of Last Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY) - 1).
        WHEN "Last Sunday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 1 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Last Monday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 2 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Last Tuesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 3 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Last Wednesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 4 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Last Thursday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 5 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Last Friday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 6 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Last Saturday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 7 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
        WHEN "Next Sunday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 1 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Next Monday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 2 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Next Tuesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 3 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Next Wednesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 4 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Next Thursday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 5 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Next Friday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 6 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Next Saturday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 7 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
        WHEN "Date Prior Month" THEN DO:
            dtDate = ipdtDate - 28.
            DO WHILE TRUE:
                IF (YEAR(dtDate)  LT YEAR(ipdtDate)   OR
                    MONTH(dtDate) LT MONTH(ipdtDate)) AND
                    DAY(dtDate)   LE DAY(ipdtDate)    THEN
                LEAVE.
                dtDate = dtDate - 1.
            END. /* do while */
        END. /* date prior month */
        WHEN "Date Prior Year" THEN DO:
            dtDate = ipdtDate - 365.
            DO WHILE TRUE:
                IF YEAR(dtDate)  LT YEAR(ipdtDate)  AND
                   MONTH(dtDate) EQ MONTH(ipdtDate) AND
                   DAY(dtDate)   LE DAY(ipdtDate)   THEN
                LEAVE.
                dtDate = dtDate - 1.
            END. /* do while */
        END. /* date prior year */
    END CASE.
        
    RETURN dtDate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_DecimalTimeInHHMM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_DecimalTimeInHHMM Procedure
FUNCTION sfCommon_DecimalDurationInHHMM RETURNS CHARACTER 
    (ipdTimeInDecimal AS DECIMAL):
    /*------------------------------------------------------------------------------
    Purpose:  Formats a duration time in decimal as "HH:MM"
    Notes:  Example:  12.5 hours => 12:30
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cTimeFormatted AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHours         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMinutes       AS CHARACTER NO-UNDO.
    
    cHours = STRING(TRUNC(ipdTimeInDecimal,0)).
    cMinutes = STRING(ROUND((ipdTimeInDecimal - TRUNC(ipdTimeInDecimal,0)) * 60,0),"99").
    
    cTimeFormatted = cHours + ":" + cMinutes.
    RETURN cTimeFormatted.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfCommon_GetDifferenceDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_GetDifferenceDays Procedure 
FUNCTION sfCommon_GetDifferenceDays RETURNS INTEGER
  ( ipdtTargetDate AS DATETIME, ipdtSourceDate AS DATETIME ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iDifferenceDays       AS INTEGER     NO-UNDO.
    
    iDifferenceDays = INTERVAL(ipdtTargetDate, ipdtSourceDate, "days").
    
    RETURN iDifferenceDays.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

&IF DEFINED(EXCLUDE-sfCommon_GetWeekDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_GetWeekDay Procedure 
FUNCTION sfCommon_GetWeekDay RETURNS INTEGER
  ( ipdtDate AS DATETIME ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lWeekday AS INTEGER NO-UNDO.

    lWeekday = WEEKDAY(ipdtDate).
    
    RETURN lWeekday.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_GetWeekDayInText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_GetWeekDayInText Procedure 
FUNCTION sfCommon_GetWeekDayInText RETURNS CHARACTER
  ( INPUT ipiWeekDay AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWeekDayChar AS CHARACTER NO-UNDO EXTENT 7.
    
    IF ipiWeekDay LT 1 OR ipiWeekDay GT 7 THEN
        RETURN "".

    ASSIGN
        cWeekDayChar[1] = "Sunday"
        cWeekDayChar[2] = "Monday"
        cWeekDayChar[3] = "Tuesday"
        cWeekDayChar[4] = "Wednesday"
        cWeekDayChar[5] = "Thursday"
        cWeekDayChar[6] = "Friday"
        cWeekDayChar[7] = "Saturday"
        .

    RETURN cWeekDayChar[ipiWeekDay].

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

&IF DEFINED(EXCLUDE-sfCommon_IsDateWeekend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_IsDateWeekend Procedure 
FUNCTION sfCommon_IsDateWeekend RETURNS LOGICAL
  ( ipdtDate AS DATETIME ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lIsWeekend AS LOGICAL NO-UNDO.

    lIsWeekend = sfCommon_GetWeekDay(ipdtDate) EQ 1 OR sfCommon_GetWeekDay(ipdtDate) EQ 7.
    
    RETURN lIsWeekend.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfCommon_SetDateOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfCommon_SetDateOptions Procedure 
FUNCTION sfCommon_SetDateOptions RETURNS LOGICAL
  ( iphDateOption AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDateOptions AS CHARACTER NO-UNDO INITIAL
"Fixed Date~
,Current Date~
,Current Date -1~
,Current Date +1~
,Current Date -2~
,Current Date +2~
,Current Date -3~
,Current Date +3~
,Current Date -4~
,Current Date +4~
,Current Date -5~
,Current Date +5~
,Current Date -6~
,Current Date +6~
,Current Date -7~
,Current Date +7~
,Current Date -8~
,Current Date +8~
,Current Date -9~
,Current Date +9~
,Current Date -10~
,Current Date +10~
,Start of This Month~
,End of This Month~
,First Day of Last Month~
,Last Day of Last Month~
,Start of This Year~
,End of This Year~
,First Day of Last Year~
,Last Day of Last Year~
,Last Sunday~
,Last Monday~
,Last Tuesday~
,Last Wednesday~
,Last Thursday~
,Last Friday~
,Last Saturday~
,Next Sunday~
,Next Monday~
,Next Tuesday~
,Next Wednesday~
,Next Thursday~
,Next Friday~
,Next Saturday~
".
    ASSIGN
        iphDateOption:LIST-ITEMS   = cDateOptions
        iphDateOption:INNER-LINES  = NUM-ENTRIES(cDateOptions)
        iphDateOption:SCREEN-VALUE = iphDateOption:ENTRY(1)
        .
    RETURN TRUE.

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

