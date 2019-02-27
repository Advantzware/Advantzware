/* pDynParamProcs.i - rstark - 2.18.2019 */

{AOA/includes/dynWidgets.i "{1}"}

PROCEDURE pCalendar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
    
    RUN nosweat/popupcal.w (OUTPUT calendarDate).
    IF calendarDate NE '' THEN
    iphWidget:SCREEN-VALUE = calendarDate.
    RETURN NO-APPLY.

END PROCEDURE.

PROCEDURE pDatePickList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget   AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphCalendar AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphPickList AS HANDLE NO-UNDO.
    
    ASSIGN
        iphWidget:SCREEN-VALUE = STRING(fDateOptionValue(
            iphPickList:SCREEN-VALUE,
            DATE(iphWidget:SCREEN-VALUE)
            ))
        iphWidget:READ-ONLY = iphPickList:SCREEN-VALUE NE "Fixed Date"
        iphWidget:PRIVATE-DATA = iphPickList:SCREEN-VALUE
        .
    IF VALID-HANDLE(iphCalendar) THEN
    iphCalendar:SENSITIVE = iphPickList:SCREEN-VALUE EQ "Fixed Date".

END PROCEDURE.

PROCEDURE pInitDynParameters :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    ASSIGN 
        hWidget = iphFrame:HANDLE 
        hWidget = hWidget:FIRST-CHILD 
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        CASE hWidget:TYPE:
            WHEN "COMBO-BOX" OR
            WHEN "RADIO-SET" OR
            WHEN "SELECTION-LIST" OR
            WHEN "TOGGLE-BOX" THEN
                APPLY "VALUE-CHANGED":U TO hWidget.
            WHEN "EDITOR" OR
            WHEN "FILL-IN" THEN
                APPLY "LEAVE":U TO hWidget.
        END CASE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

PROCEDURE pParamAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cAction  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParamID AS INTEGER   NO-UNDO.
    
    FIND FIRST ttAction
         WHERE ttACtion.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttAction THEN RETURN.
    iParamID = ttAction.paramID.
    FOR EACH ttAction
        WHERE ttAction.actionParamID EQ iParamID
          AND ttAction.action NE "",
        FIRST dynParam
        WHERE dynParam.paramID EQ ttAction.paramID
        :
        DO idx = 1 TO NUM-ENTRIES(ttAction.action):
            ASSIGN
                cValue  = ENTRY(1,ENTRY(idx,ttAction.action),":")
                cAction = ENTRY(2,ENTRY(idx,ttAction.action),":")
                .
            IF iphWidget:SCREEN-VALUE EQ cValue THEN
            CASE cAction:
                WHEN "DISABLE" THEN
                ttAction.paramWidget:READ-ONLY = YES.
                WHEN "ENABLE" THEN
                ttAction.paramWidget:READ-ONLY = NO.
                WHEN "HI" THEN
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = CHR(254).
                    WHEN "DECIMAL" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "99999999.99".
                    WHEN "DATE" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "12/31/2049".
                    WHEN "INTEGER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "99999999".
                END CASE.
                WHEN "LOW" THEN
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = CHR(32).
                    WHEN "DECIMAL" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "-99999999.99".
                    WHEN "DATE" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "1/1/1950".
                    WHEN "INTEGER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "-99999999".
                END CASE.
            END CASE.
        END. /* do idx */
    END. /* each ttaction */

END PROCEDURE.

PROCEDURE pRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = iphWidget:SCREEN-VALUE.
    RUN AOA/Recipients.w (INPUT-OUTPUT cRecipients).
    iphWidget:SCREEN-VALUE = cRecipients.

END PROCEDURE.

PROCEDURE pSaveDynParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFormat AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
            dynParamValue.outputFormat  = ipcOutputFormat
            dynParamValue.paramName     = ""
            dynParamValue.paramLabel    = ""
            dynParamValue.paramValue    = ""
            dynParamValue.paramDataType = ""
            dynParamValue.paramFormat   = ""
            hWidget = FRAME paramFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "BUTTON"    AND
               hWidget:TYPE NE "FRAME"     AND
               hWidget:TYPE NE "RECTANGLE" AND
               hWidget:TYPE NE "TEXT" THEN DO:
                ASSIGN
                    idx = idx + 1
                    dynParamValue.paramName[idx]     = hWidget:NAME
                    dynParamValue.paramLabel[idx]    = hWidget:LABEL
                    dynParamValue.paramValue[idx]    = hWidget:SCREEN-VALUE
                    dynParamValue.paramDataType[idx] = hWidget:DATA-TYPE
                    .
                IF CAN-DO("COMBO-BOX,FILL-IN",hWidget:TYPE) THEN
                dynParamValue.paramFormat[idx] = hWidget:FORMAT.
            END. /* if type */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            hWidget = FRAME outputFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF CAN-DO("EDITOR,TOGGLE-BOX",hWidget:TYPE) THEN
            ASSIGN
                idx = idx + 1
                dynParamValue.paramName[idx]     = hWidget:NAME
                dynParamValue.paramLabel[idx]    = hWidget:LABEL
                dynParamValue.paramValue[idx]    = hWidget:SCREEN-VALUE
                dynParamValue.paramDataType[idx] = hWidget:DATA-TYPE
                .
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */

END PROCEDURE.

PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.    

    RUN pParamAction (iphWidget).

    FIND FIRST ttAction
         WHERE ttACtion.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttAction THEN RETURN.
    
    IF ttAction.validateProc NE "" AND
       CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,ttAction.validateProc) THEN DO:
        RUN VALUE(ttAction.validateProc) (iphWidget).
        cErrorMsg = RETURN-VALUE.
        IF cErrorMsg NE "" THEN DO:
            MESSAGE cErrorMsg SKIP(1) "Correct this Entry?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Error"
            UPDATE lCorrect AS LOGICAL.
            IF lCorrect THEN DO:
                APPLY "ENTRY":U TO iphWidget.
                RETURN NO-APPLY.
            END. /* if lcorrect */
        END. /* if cerror */
    END. /* if validateProc */

END PROCEDURE.
