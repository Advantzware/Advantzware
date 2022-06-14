/* NK1toSetting.p - rstark - 11.4.2021 */

DISABLE TRIGGERS FOR LOAD OF settingType.

DEFINE INPUT  PARAMETER ipiCurrentVersion AS INTEGER NO-UNDO.

DEFINE VARIABLE companyContext      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iScopeID            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingTypeID      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSettingValue       AS CHARACTER NO-UNDO.
DEFINE VARIABLE nk1DataType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE nk1Name             AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingCategoryTags AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDataType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDefaultValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDescription  AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingPassword     AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingValidValues  AS CHARACTER NO-UNDO.

DEFINE VARIABLE oSetting AS system.Setting NO-UNDO.

oSetting = NEW system.Setting().

INPUT FROM VALUE(SEARCH("Documentation/NK1toSetting.csv")) NO-ECHO.
IMPORT ^. // header line
REPEAT:
    IMPORT DELIMITER ","
        nk1Name
        nk1DataType
        settingName
        settingDataType
        settingDescription
        settingValidValues
        settingDefaultValue
        companyContext
        settingCategoryTags
        settingPassword
        .
    FIND FIRST settingType EXCLUSIVE
         WHERE settingType.settingName EQ settingName
         NO-ERROR.
    IF NOT AVAILABLE settingType THEN DO:
        DO WHILE TRUE:
            iSettingTypeID = NEXT-VALUE(settingTypeID_seq).
            IF NOT CAN-FIND(FIRST settingType
                            WHERE settingType.settingTypeID EQ iSettingTypeID) THEN
            LEAVE.
        END. // do while
        CREATE settingType.
        ASSIGN
            settingDescription        = REPLACE(settingDescription,","," ")
            settingType.settingTypeID = iSettingTypeID
            settingType.settingName   = settingName
            settingType.description   = REPLACE(settingDescription,"|",",") + CHR(10) + "NK1=" + nk1Name
            settingType.dataType      = settingDataType
            settingType.validValues   = REPLACE(settingValidValues,"|",",")
            settingType.defaultValue  = REPLACE(settingDefaultValue,"|",",")
            settingType.categoryTags  = REPLACE(settingCategoryTags,"|",",")
            settingType.isPassword    = settingPassword EQ "Yes"
            settingType.rec_key       = STRING(YEAR(TODAY),"9999") + 
                                        STRING(MONTH(TODAY),"99") + 
                                        STRING(DAY(TODAY),"99") + 
                                        STRING(TIME,"99999") + 
                                        STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").
    END. // not can-find
    ELSE ASSIGN 
        settingType.description = SUBSTRING(settingType.description,1,INDEX(settingType.description,chr(10)) - 1)
        settingType.description = settingType.description + chr(10) + "NK1=" + nk1Name. 
        
    FOR EACH sys-ctrl EXCLUSIVE-LOCK
        WHERE sys-ctrl.name EQ nk1Name
        :
        sys-ctrl.isActive = NO.
        IF companyContext THEN DO:
            CASE nk1DataType:
                WHEN "Character" THEN
                cSettingValue = sys-ctrl.char-fld.
                WHEN "Date" THEN
                cSettingValue = STRING(sys-ctrl.date-fld,"99/99/9999").
                WHEN "Decimal" THEN
                cSettingValue = STRING(sys-ctrl.dec-fld).
                WHEN "Integer" THEN
                cSettingValue = STRING(sys-ctrl.int-fld).
                WHEN "Logical" THEN
                cSettingValue = STRING(sys-ctrl.log-fld,"YES/NO").
                WHEN "Description" THEN
                cSettingValue = sys-ctrl.descrip.
            END CASE.
            // convert when integer and/or decimal fields used as logicals
            IF nk1DataType NE settingDataType AND settingDataType EQ "Logical" THEN
            cSettingValue = STRING(cSettingValue EQ "1","YES/NO").
            // convert date & integer to datetime-tz
            IF nk1Datatype EQ "Date" AND settingDataType EQ "DateTime-TZ" THEN
            cSettingValue = STRING(DATETIME-TZ(sys-ctrl.date-fld, sys-ctrl.int-fld, TIMEZONE)).
            // if value same as default, don't create unique setting
            IF cSettingValue EQ settingType.defaultValue THEN NEXT.
            FIND FIRST scope NO-LOCK
                 WHERE scope.scopeTable  EQ "Company"
                   AND scope.scopeField1 EQ sys-ctrl.company
                 NO-ERROR.
            IF NOT AVAILABLE scope THEN DO:
                DO WHILE TRUE:
                    iScopeID = NEXT-VALUE(scopeID_seq).
                    IF NOT CAN-FIND(FIRST scope
                                    WHERE scope.scopeID EQ iScopeID) THEN
                    LEAVE.
                END. // do while
                CREATE scope.
                ASSIGN
                    scope.scopeID     = iScopeID
                    scope.scopeTable  = "Company"
                    scope.scopeField1 = sys-ctrl.company
                    .
            END. // if not avail
            iScopeID = scope.scopeID.
            FIND FIRST setting EXCLUSIVE-LOCK
                 WHERE setting.settingTypeID EQ settingType.settingTypeID
                   AND setting.settingName   EQ settingType.settingName
                   AND setting.scopeID       EQ iScopeID
                 NO-ERROR.
            IF NOT AVAILABLE setting THEN DO:
                DO WHILE TRUE:
                    iSettingID = NEXT-VALUE(settingID_seq).
                    IF NOT CAN-FIND(FIRST setting
                                    WHERE setting.settingID EQ iSettingID) THEN
                    LEAVE.
                END. // do while
                CREATE setting.
                ASSIGN
                    setting.settingTypeID = settingType.settingTypeID
                    setting.settingID     = iSettingID
                    setting.settingName   = settingType.settingName
                    setting.description   = settingType.description
                    setting.scopeID       = iScopeID
                    setting.settingValue  = cSettingValue
                    .
            END. // not avail
        END. // if companycontext
    END. // each sys-ctrl
END. // repeat
INPUT CLOSE.

IF ipiCurrentVersion LT 21041500 THEN
    RUN ipConvertInvoiceApprovalNK1s.

FINALLY:
    IF VALID-OBJECT (oSetting) THEN
        DELETE OBJECT oSetting. 
END FINALLY.
/* **********************  Internal Procedures  *********************** */

PROCEDURE ipConvertInvoiceApprovalNK1s:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cNK1List      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSettingValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSettingName  AS CHARACTER NO-UNDO.

    cNK1List = "ApplyInvoiceApprovals,"
             + "InvoiceApprovalBillNotes,"
             + "InvoiceApprovalExpectZero,"
             + "InvoiceApprovalFreightAmount,"
             + "InvoiceApprovalFreightTerms,"
             + "InvoiceApprovalInvoiceStatus,"
             + "InvoiceApprovalMiscCharge,"
             + "InvoiceApprovalOrderlineChange,"
             + "InvoiceApprovalPriceGTCost,"
             + "InvoiceApprovalTaxableCheck,"
             + "InvoiceApprovalTaxCalc".
    
    DO iIndex = 1 TO NUM-ENTRIES(cNK1List):
        cSettingName = ENTRY(iIndex, cNK1List).

        IF cSettingName = "ApplyInvoiceApprovals" THEN
            cSettingName = "InvoiceApproval".

        FOR EACH sys-ctrl NO-LOCK
            WHERE sys-ctrl.name EQ ENTRY(iIndex, cNK1List):
            IF sys-ctrl.log-fld THEN DO:
                cSettingValue = "Off".

                IF sys-ctrl.int-fld EQ 0 THEN
                    cSettingValue = "On".
                ELSE IF sys-ctrl.int-fld EQ 1 THEN
                    cSettingValue = "On Also During Post".
    
                oSetting:Update(cSettingName, "Company", sys-ctrl.company, "", "", cSettingValue).
            END.

            FOR EACH sys-ctrl-shipto NO-LOCK 
                WHERE sys-ctrl-ship.company EQ sys-ctrl.company
                  AND sys-ctrl-shipto.name  EQ sys-ctrl.name:

                cSettingValue = "Off".

                IF NOT sys-ctrl-shipto.log-fld THEN 
                    cSettingValue = "Off".
                ELSE IF sys-ctrl-shipto.int-fld EQ 0 THEN
                    cSettingValue = "On".
                ELSE IF sys-ctrl-shipto.int-fld EQ 1 THEN
                    cSettingValue = "On Also During Post".
                    
                oSetting:Update(cSettingName, "Customer", sys-ctrl-shipto.company, sys-ctrl-shipto.cust-vend-no, "", cSettingValue).
            END.
        END.
    END.
END PROCEDURE.
