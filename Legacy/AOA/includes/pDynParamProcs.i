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
            WHEN "COMBO-BOX"      OR
            WHEN "RADIO-SET"      OR
            WHEN "SELECTION-LIST" OR
            WHEN "TOGGLE-BOX"     THEN
                APPLY "VALUE-CHANGED":U TO hWidget.
            WHEN "EDITOR"  OR
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
    
    DEFINE VARIABLE cAction    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cParamName AS CHARACTER NO-UNDO.
    
    FIND FIRST ttDynAction
         WHERE ttDynAction.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttDynAction THEN RETURN.
    
    cParamName = ttDynAction.paramName.
    FOR EACH ttDynAction
        WHERE ttDynAction.actionParamName EQ cParamName
          AND ttDynAction.action NE "",
        FIRST dynParam
        WHERE dynParam.paramID EQ ttDynAction.paramID
        :
        DO idx = 1 TO NUM-ENTRIES(ttDynAction.action):
            IF INDEX(ENTRY(idx,ttDynAction.action),":") EQ 0 THEN NEXT.
            ASSIGN
                cValue  = ENTRY(1,ENTRY(idx,ttDynAction.action),":")
                cAction = ENTRY(2,ENTRY(idx,ttDynAction.action),":")
                .
            IF iphWidget:SCREEN-VALUE EQ cValue THEN
            CASE cAction:
                WHEN "DISABLE" THEN
                ttDynAction.paramWidget:READ-ONLY = YES.
                WHEN "ENABLE" THEN
                ttDynAction.paramWidget:READ-ONLY = NO.
                WHEN "HI" THEN
                IF ttDynAction.paramWidget:FORMAT EQ "99:99" THEN
                ttDynAction.paramWidget:SCREEN-VALUE = "2400".
                ELSE
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = CHR(254).
                    WHEN "DECIMAL" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = ttDynAction.initialValue.
                    WHEN "DATE" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = "12/31/2049".
                    WHEN "INTEGER" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = ttDynAction.initialValue.
                END CASE.
                WHEN "LOW" THEN
                IF ttDynAction.paramWidget:FORMAT EQ "99:99" THEN
                ttDynAction.paramWidget:SCREEN-VALUE = "0000".
                ELSE
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = CHR(32).
                    WHEN "DECIMAL" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = ttDynAction.initialValue.
                    WHEN "DATE" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = "1/1/1950".
                    WHEN "INTEGER" THEN
                    ttDynAction.paramWidget:SCREEN-VALUE = ttDynAction.initialValue.
                END CASE.
            END CASE.
        END. /* do idx */
    END. /* each ttDynAction */

END PROCEDURE.

PROCEDURE pParamValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cErrorMsg  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamName AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttDynAction FOR ttDynAction.

    /* check and run action procedures */
    RUN pParamAction (iphWidget).

    FIND FIRST ttDynAction
         WHERE ttDynAction.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttDynAction THEN RETURN.

    IF ttDynAction.action NE ? AND INDEX(ttDynAction.action,"VALUE-CHANGED") NE 0 THEN DO:
        FOR EACH bttDynAction
            WHERE bttDynAction.actionParamName EQ ttDynAction.paramName
            :
            IF bttDynAction.validateProc NE "" AND
               CAN-DO(hDynValProc:INTERNAL-ENTRIES,bttDynAction.validateProc) THEN
            RUN VALUE(bttDynAction.validateProc) IN hDynValProc (
                ttDynAction.paramWidget,
                bttDynAction.paramWidget
                ).
        END. /* each bttdynaction */
    END. /* if value-changed */
    ELSE
    /* check and run validate procedures */
    IF ttDynAction.validateProc NE "" AND
       CAN-DO(hDynValProc:INTERNAL-ENTRIES,ttDynAction.validateProc) THEN DO:
        RUN VALUE(ttDynAction.validateProc) IN hDynValProc (iphWidget).
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

    /* check and run description procedures */
    cParamName = ttDynAction.paramName.
    FOR EACH ttDynAction
        WHERE ttDynAction.actionParamName EQ cParamName
        :
        IF ttDynAction.descriptionProc NE "" AND
           CAN-DO(hDynDescripProc:INTERNAL-ENTRIES,ttDynAction.descriptionProc) THEN DO:
            RUN VALUE(ttDynAction.descriptionProc) IN hDynDescripProc (iphWidget, ttDynAction.paramWidget).
        END. /* if descriptionProc */
    END. /* each ttDynAction */

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
            dynParamValue.securityLevel = IF dynParamValue.user-id EQ "_default" THEN 0
                                          ELSE DYNAMIC-FUNCTION("sfUserSecurityLevel")
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
            IF CAN-DO("EDITOR,RADIO-SET,TOGGLE-BOX",hWidget:TYPE) THEN
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
