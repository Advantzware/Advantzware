/* pDynParamProcs.i - rstark - 2.18.2019 */

{AOA/includes/dynWidgets.i "{1}" " "}

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
    
    DEFINE VARIABLE hWidget    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iSortOrder AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        EMPTY TEMP-TABLE ttParamOrder.
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
            dynParamValue.outputFormat  = ipcOutputFormat
            dynParamValue.securityLevel = IF dynParamValue.user-id EQ "_default" THEN 0
                                          ELSE IF dynParamValue.securityLevel EQ 0 THEN DYNAMIC-FUNCTION("sfUserSecurityLevel")
                                          ELSE dynParamValue.securityLevel
/*            /* rstark - remove when depricated */*/
/*            dynParamValue.paramName     = ""     */
/*            dynParamValue.paramLabel    = ""     */
/*            dynParamValue.paramValue    = ""     */
/*            dynParamValue.paramDataType = ""     */
/*            dynParamValue.paramFormat   = ""     */
            hWidget = FRAME paramFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        FIND CURRENT dynParamValue NO-LOCK.
        DO WHILE VALID-HANDLE(hWidget):
            IF NOT CAN-DO("BUTTON,FRAME,RECTANGLE,TEXT",hWidget:TYPE) THEN DO:
                CREATE ttParamOrder.
                ASSIGN
                    ttParamOrder.paramRow      = hWidget:ROW
                    ttParamOrder.paramCol      = hWidget:COL
                    ttParamOrder.paramName     = hWidget:NAME
                    ttParamOrder.paramLabel    = hWidget:LABEL
                    ttParamOrder.paramValue    = hWidget:SCREEN-VALUE
                    ttParamOrder.paramDataType = hWidget:DATA-TYPE
                    .
                IF CAN-DO("COMBO-BOX,FILL-IN",hWidget:TYPE) THEN
                ttParamOrder.paramFormat = hWidget:FORMAT.
            END. /* if type */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        FOR EACH dynValueParam EXCLUSIVE-LOCK
            WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
              AND dynValueParam.user-id      EQ dynParamValue.user-id
              AND dynValueParam.prgmName     EQ dynParamValue.prgmName
              AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
            :
            DELETE dynValueParam.
        END. /* each dynvalueparam */
        FOR EACH ttParamOrder:
            CREATE dynValueParam.
            ASSIGN
                iSortOrder                 = iSortOrder + 1
                dynValueParam.subjectID    = dynParamValue.subjectID
                dynValueParam.user-id      = dynParamValue.user-id
                dynValueParam.prgmName     = dynParamValue.prgmName
                dynValueParam.paramValueID = dynParamValue.paramValueID
                dynValueParam.sortOrder    = iSortOrder
                dynValueParam.paramName    = ttParamOrder.paramName
                dynValueParam.paramLabel   = ttParamOrder.paramLabel
                dynValueParam.paramValue   = ttParamOrder.paramValue
                dynValueParam.dataType     = ttParamOrder.paramdataType
                dynValueParam.paramFormat  = ttParamOrder.paramFormat
/*                /* rstark - remove when depricated */                        */
/*                dynParamValue.paramName[idx]     = ttParamOrder.paramName    */
/*                dynParamValue.paramLabel[idx]    = ttParamOrder.paramLabel   */
/*                dynParamValue.paramValue[idx]    = ttParamOrder.paramValue   */
/*                dynParamValue.paramDataType[idx] = ttParamOrder.paramdataType*/
/*                dynParamValue.paramFormat[idx]   = ttParamOrder.paramFormat  */
                .
        END. /* each ttparamorder */
&IF "{&program-id}" NE "dynBrowserParam." &THEN
        ASSIGN
            hWidget = FRAME outputFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF CAN-DO("EDITOR,RADIO-SET,TOGGLE-BOX",hWidget:TYPE) THEN DO:
                CREATE dynValueParam.
                ASSIGN
                    iSortOrder                 = iSortOrder + 1
                    dynValueParam.subjectID    = dynParamValue.subjectID
                    dynValueParam.user-id      = dynParamValue.user-id
                    dynValueParam.prgmName     = dynParamValue.prgmName
                    dynValueParam.paramValueID = dynParamValue.paramValueID
                    dynValueParam.sortOrder    = iSortOrder
                    dynValueParam.paramName    = hWidget:NAME
                    dynValueParam.paramLabel   = hWidget:LABEL
                    dynValueParam.paramValue   = hWidget:SCREEN-VALUE
                    dynValueParam.dataType     = hWidget:DATA-TYPE
/*                    /* rstark - remove when depricated */                  */
/*                    dynParamValue.paramName[idx]     = hWidget:NAME        */
/*                    dynParamValue.paramLabel[idx]    = hWidget:LABEL       */
/*                    dynParamValue.paramValue[idx]    = hWidget:SCREEN-VALUE*/
/*                    dynParamValue.paramDataType[idx] = hWidget:DATA-TYPE   */
                    .
            END. /* if hwidget:type */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        RELEASE dynValueParam.
&ENDIF        
    END. /* do trans */

END PROCEDURE.
