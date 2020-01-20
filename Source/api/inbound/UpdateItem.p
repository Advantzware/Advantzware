/*------------------------------------------------------------------------
    File        : api\inbound\UpdateItem.p
    Purpose     : Updates itemfg with given key values for an item
    Syntax      :

    Description : Updates itemfg with given key values for an item

    Author(s)   : Mithun Porandla
    Created     : Thu Jan 16 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttInput.i}
    
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInput.
DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID  AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdCommonProcs AS HANDLE NO-UNDO.

RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCommonProcs).

RUN pUpdateInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF NOT oplSuccess THEN
    RETURN.
    
RUN pValidateInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF NOT oplSuccess THEN
    RETURN.

RUN pUpdateItem (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.    
        
IF NOT oplSuccess THEN
    RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetDbFieldAndDataType:
/*------------------------------------------------------------------------------
 Purpose: Procedure to get db field and data type of a given key
 Notes: This should return if a field is to be assigned with new values. For
        primary fields like company or i-no assinging should be blocked. 
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFieldName     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldDB       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldDataType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldProcess  AS LOGICAL   NO-UNDO.

    CASE ipcFieldName:
        WHEN "Company" THEN
            ASSIGN
                opcFieldProcess  = FALSE
                opcFieldDB       = "company"
                opcFieldDataType = "CHARACTER"
                .
        WHEN "ItemID" THEN
            ASSIGN
                opcFieldProcess  = FALSE
                opcFieldDB       = "i-no"
                opcFieldDataType = "CHARACTER"
                .
        WHEN "StackHeight" THEN
            ASSIGN
                opcFieldProcess  = TRUE
                opcFieldDB       = "stackHeight"
                opcFieldDataType = "INTEGER"
                .
        WHEN "PalletLength" THEN
            ASSIGN
                opcFieldProcess  = TRUE
                opcFieldDB       = "unitLength"
                opcFieldDataType = "DECIMAL"
                .
        WHEN "PalletWidth" THEN
            ASSIGN
                opcFieldProcess  = TRUE
                opcFieldDB       = "unitWidth"
                opcFieldDataType = "DECIMAL"
                .
        WHEN "PalletHeight" THEN
            ASSIGN
                opcFieldProcess  = TRUE
                opcFieldDB       = "unitHeight"
                opcFieldDataType = "DECIMAL"
                .                                                
    END CASE.
END PROCEDURE.

PROCEDURE pUpdateInputs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate the input keys and it's value
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidValue AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttInput FOR ttInput.
    
    FOR EACH ttInput
        BREAK BY ttInput.fieldSeq:
        
        IF NOT FIRST-OF(ttInput.fieldSeq) THEN
            NEXT.
        
        FOR EACH bf-ttInput 
            WHERE bf-ttInput.fieldSeq EQ ttInput.fieldSeq:
            /* Fetch the DB field. data type for the key field */
            RUN pGetDbFieldAndDataType (
                INPUT  bf-ttInput.fieldKey,
                OUTPUT bf-ttInput.fieldDB,
                OUTPUT bf-ttInput.fieldDataType,
                OUTPUT bf-ttInput.fieldProcess
                ) NO-ERROR.
            
            IF bf-ttInput.fieldDB EQ "" OR bf-ttInput.fieldDataType EQ "" THEN
                NEXT.
             
            IF bf-ttInput.fieldDataType EQ "CHARACTER" THEN
                NEXT.
            
            /* Validate if value of key field is valid */
            RUN spCommon_ValidateValueByDataType (
                INPUT  bf-ttInput.fieldValue,
                INPUT  bf-ttInput.fieldDataType,
                OUTPUT lValidValue
                ) NO-ERROR.
                
            IF NOT lValidValue THEN DO:
                ASSIGN
                    opcMessage = "Invalid " + bf-ttInput.fieldKey + " value " + bf-ttInput.fieldValue
                    oplSuccess = NO
                    .
                RETURN.    
            END.
        END.
    END.

    ASSIGN 
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pUpdateItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to update the fields into itemfg table. 
 Notes: This uses the dynamic buffer to find and update the input key fields
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttInput FOR ttInput.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    
    FOR EACH ttInput
        BREAK BY ttInput.fieldSeq:
        IF NOT FIRST-OF(ttInput.fieldSeq) THEN
            NEXT.
            
        FIND FIRST bf-ttInput
             WHERE bf-ttInput.fieldSeq EQ ttInput.fieldSeq
               AND bf-ttInput.fieldKey = "Company"
             NO-ERROR.
        IF AVAILABLE bf-ttInput THEN                
            ASSIGN
                bf-ttInput.success = TRUE
                cCompany           = bf-ttInput.fieldValue
                .
        
        FIND FIRST bf-ttInput
             WHERE bf-ttInput.fieldSeq EQ ttInput.fieldSeq
               AND bf-ttInput.fieldKey = "ItemID"
             NO-ERROR.
        IF AVAILABLE bf-ttInput THEN
            ASSIGN
                bf-ttInput.success = TRUE
                cItemID            = bf-ttInput.fieldValue
                .
        
        FIND FIRST bf-itemfg EXCLUSIVE-LOCK
             WHERE bf-itemfg.company = cCompany
               AND bf-itemfg.i-no    = cItemID
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN
            NEXT.
        
        /* Update the fields that are allowed to process */    
        FOR EACH bf-ttInput 
            WHERE bf-ttInput.fieldSeq      EQ ttInput.fieldSeq
              AND bf-ttInput.fieldDB       NE ""
              AND bf-ttInput.fieldDataType NE ""
              AND bf-ttInput.fieldProcess:           
            CASE bf-ttInput.fieldDataType:
                WHEN "CHARACTER" THEN
                    IF BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE NE bf-ttInput.fieldValue THEN
                        BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE = bf-ttInput.fieldValue NO-ERROR.
                WHEN "INTEGER" THEN
                    IF BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE NE INTEGER(bf-ttInput.fieldValue) THEN
                        BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE = INTEGER(bf-ttInput.fieldValue) NO-ERROR.
                WHEN "DECIMAL" THEN
                    IF BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE NE DECIMAL(bf-ttInput.fieldValue) THEN
                        BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE = DECIMAL(bf-ttInput.fieldValue) NO-ERROR.
                WHEN "DATE" THEN
                    IF BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE NE DATE(bf-ttInput.fieldValue) THEN
                        BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE = DATE(bf-ttInput.fieldValue) NO-ERROR.
                WHEN "LOGICAL" THEN
                    IF BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE NE LOGICAL(bf-ttInput.fieldValue) THEN
                        BUFFER bf-itemfg:BUFFER-FIELD(bf-ttInput.fieldDB):BUFFER-VALUE = LOGICAL(bf-ttInput.fieldValue) NO-ERROR.
            END CASE.
            
            IF NOT ERROR-STATUS:ERROR THEN
                bf-ttInput.success = TRUE.
        END.
        
        RELEASE bf-itemfg.
    END.

    ASSIGN 
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pValidateDBFieldValue PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure for itemfg field validations. These are field level validations
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFieldName     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldValue    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    oplValid   = TRUE.

    CASE ipcFieldName:
        WHEN "Company" THEN
            IF ipcFieldValue EQ "" OR 
                NOT CAN-FIND(FIRST company
                             WHERE company.company EQ ipcFieldValue) THEN
                ASSIGN
                    oplValid   = FALSE
                    opcMessage = "Invalid Company " + ipcFieldValue
                    .
        WHEN "ItemID" THEN
            IF ipcFieldValue EQ "" OR 
                NOT CAN-FIND(FIRST itemfg
                             WHERE itemfg.company EQ cCompany
                               AND itemfg.i-no    EQ ipcFieldValue) THEN
                ASSIGN
                    oplValid   = FALSE
                    opcMessage = "Invalid Item " + ipcFieldValue
                    .
        WHEN "StackHeight" THEN
            IF INTEGER(ipcFieldValue) GT 4 OR INTEGER(ipcFieldValue) LT 1 THEN
                ASSIGN
                    oplValid   = FALSE
                    opcMessage = "Stack Height must be in range of 1 and 4 for ItemID " + cItemID
                    .
        OTHERWISE
            oplValid = TRUE.                                              
    END CASE.
END PROCEDURE.

PROCEDURE pValidateInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validate the inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttInput FOR ttInput.
    
    FOR EACH ttInput
        BREAK BY ttInput.fieldSeq:
        IF NOT FIRST-OF(ttInput.fieldSeq) THEN
            NEXT.
            
        FIND FIRST bf-ttInput
             WHERE bf-ttInput.fieldSeq EQ ttInput.fieldSeq
               AND bf-ttInput.fieldKey = "Company"
             NO-ERROR.
        IF NOT AVAILABLE bf-ttInput THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Missing Company field"
                .
            RETURN.
        END.
        cCompany = bf-ttInput.fieldValue.
        
        FIND FIRST bf-ttInput
             WHERE bf-ttInput.fieldSeq EQ ttInput.fieldSeq
               AND bf-ttInput.fieldKey = "ItemID"
             NO-ERROR.
        IF NOT AVAILABLE bf-ttInput THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Missing ItemID field"
                .
            RETURN.
        END.            
        cItemID = bf-ttInput.fieldValue.
        
        /* Validate db field value */
        FOR EACH bf-ttInput 
            WHERE bf-ttInput.fieldSeq      EQ ttInput.fieldSeq
              AND bf-ttInput.fieldDB       NE ""
              AND bf-ttInput.fieldDataType NE "":
            RUN pValidateDBFieldValue (
                INPUT  bf-ttInput.fieldKey,
                INPUT  bf-ttInput.fieldValue,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.

            IF NOT oplSuccess THEN
                RETURN.
        END.
    END.

    ASSIGN 
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.
