/*------------------------------------------------------------------------
    File        : api/CommonAPIProcs.i
    Purpose     : This include file contains common functions and
                  procedure shared among Inbound and Outbound procedures

    Syntax      :

    Description : This include file contains common functions and
                  procedure shared among Inbound and Outbound procedures

    Author(s)   : Porandla Mithun
    Created     : Tue August 8th 10:01:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ************************  Function Prototypes ********************** */
FUNCTION fGetAPITransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ) FORWARD.

FUNCTION fGetClientTransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ) FORWARD.

/* **********************  Internal Procedures  *********************** */


PROCEDURE pUpdateDelimiter PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Replaces and trims the delimiters of the request data for a given data type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    
    IF ipcRequestDataType EQ "JSON" THEN
        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData, "$comma$", ",")
            ioplcRequestData = TRIM(ioplcRequestData, ",")
            .
    ELSE
        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData, "$linefeed$", "~n")
            ioplcRequestData = TRIM(ioplcRequestData, "~n")
            .
END PROCEDURE.

PROCEDURE updateRequestData:
/*------------------------------------------------------------------------------
 Purpose: Replaces the given key field with the value in the request data
 Notes: Below is the format for the key field to enter a format or data type in configuration.
        $keyfield|format|datatype|alignmentstype|trim|$
        Eg. $poID|>>>>>>>9|INT|$, $poNotes|X(30)|$, $poData|YYYYMMDD|DATE|$, $poID|>>>>>>>9|INT|L|$,
            $poID|>>>>>>>9|INT||TRIM|$
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcField         AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcValue         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldValuePrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValueSuffix AS CHARACTER NO-UNDO.        
    DEFINE VARIABLE cFormat           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormatType       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNextChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSourceString     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTargetString     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAlignmentStyle   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrim             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatAvail      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFormatTypeAvail  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAlignmentAvail   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTrimAvail        AS LOGICAL   NO-UNDO.
    
    ASSIGN
        cFieldValuePrefix = "$"
        cFieldValueSuffix = "$"
        .

    IF ipcValue EQ ? THEN
        ipcValue = "".
    
    /* This will add an escape character (\) before a (") so JSON won't throw error */
    ASSIGN
        ipcValue = REPLACE(ipcValue, '\','\\')
        ipcValue = REPLACE(ipcValue, '"','\"')
        .
    
    /* Replaces the key field with value in request data */
    ioplcRequestData = REPLACE(ioplcRequestData, cFieldValuePrefix + ipcField + cFieldValueSuffix, ipcValue).
    
    /* If the key field is available with a specific format or data type for conversion */
    DO WHILE ioplcRequestData MATCHES "*" + cFieldValuePrefix + ipcField + "|*|" + cFieldValueSuffix + "*":
        /* cNextChar fetches the character after the at the end of key field */
        ASSIGN
            cFormat         = ""
            cFormatType     = ""
            cAlignmentStyle = ""
            cSourceString   = ""
            cTrim           = ""
            cNextChar       = SUBSTRING(ioplcRequestData,INDEX(ioplcRequestData,cFieldValuePrefix + ipcField + "|") + LENGTH(cFieldValuePrefix + ipcField), 1)
            .
        
        ASSIGN
            lFormatAvail     = FALSE
            lFormatTypeAvail = FALSE
            lAlignmentAvail  = FALSE
            lTrimAvail       = FALSE
            .
            
        /* If $ do nothing, as it would have been already replaced */
        IF cNextChar EQ "$" THEN
            cFormat = "".
        /* If |, then a format exists */
        ELSE IF cNextChar EQ "|" THEN DO:
            iIndex = INDEX(ioplcRequestData,cFieldValuePrefix + ipcField + "|") + LENGTH(cFieldValuePrefix + ipcField) + 1.
            
            /* Code to retrieve the format */
            DO WHILE TRUE:
                IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                    LEAVE.
                
                ASSIGN
                    cFormat = cFormat + SUBSTRING(ioplcRequestData,iIndex,1)
                    iIndex  = iIndex + 1
                    .
            END.

            lFormatAvail = TRUE.
            
            /* Block to check if a data type exist */            
            iIndex = iIndex + 1.
            
            /* If the next character after the format is not $, then data type exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:     
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                        LEAVE.
                    
                    ASSIGN
                        cFormatType = cFormatType + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex      = iIndex + 1
                        .
                END.
                
                lFormatTypeAvail = TRUE.
            END.
            ELSE 
                iIndex = iIndex - 1.
            
            /* Block to check if a alignment style exist */
            iIndex = iIndex + 1.
  
            /* If the next character after the format is not $, then alignment style exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                        LEAVE.
  
                    ASSIGN
                        cAlignmentStyle = cAlignmentStyle + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lAlignmentAvail = TRUE.
            END. 
            ELSE
                iIndex = iIndex - 1.
                
            iIndex = iIndex + 1.    
            
            /* If the next character after the alignment is not $, then trim exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                        LEAVE.
  
                    ASSIGN
                        cTrim  = cTrim + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex = iIndex + 1
                        .
                END.
                
                lTrimAvail = TRUE.
            END.                    
        END.    
        ELSE
            cFormat = ?.    

        IF cFormatType NE "" THEN DO:
            IF cFormatType BEGINS "INT" THEN
                ASSIGN
                    cTargetString = STRING(INTEGER(ipcValue),cFormat)
                    cTargetString = TRIM(cTargetString)
                    NO-ERROR.
            ELSE IF cFormatType BEGINS "DEC" THEN
                ASSIGN
                    cTargetString = STRING(DECIMAL(ipcValue),cFormat)
                    cTargetString = TRIM(cTargetString)
                    NO-ERROR.
            ELSE IF cFormatType EQ "TIME" THEN
                RUN Format_Time (
                    INPUT  INTEGER(ipcValue),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType EQ "MTIME" THEN
                RUN Format_MTime (
                    INPUT  INTEGER(ipcValue),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType EQ "DATE" THEN
                RUN Format_DateTimeTZ (
                    INPUT  DATETIME-TZ(ipcValue),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType BEGINS "LOG" THEN
                ASSIGN
                    cTargetString = STRING(LOGICAL(ipcValue), cFormat)
                    cTargetString = TRIM(cTargetString)
                    .
        END.
        ELSE
            cTargetString = STRING(ipcValue,cFormat).

        IF cFormatType BEGINS "INT" OR cFormatType BEGINS "DEC" THEN DO:
            IF cAlignmentStyle EQ "L" THEN
                cTargetString = cTargetString + FILL(" ", LENGTH(cFormat) - LENGTH(cTargetString)).
            ELSE IF cAlignmentStyle EQ "R" THEN
                cTargetString = FILL(" ", LENGTH(cFormat) - LENGTH(cTargetString)) + cTargetString.
        END.

        /* If trim format exists them trim the target string */
        IF cTrim EQ "TRIM" THEN
            cTargetString = TRIM(cTargetString).
        
        IF lFormatAvail THEN        
            cSourceString = cSourceString + cFieldValuePrefix + ipcField + "|" + cFormat + "|".
        
        /* Constructing the string to replace with the formatted string */
        IF lFormatTypeAvail THEN
            cSourceString = cSourceString + cFormatType + "|".
        
        IF lAlignmentAvail THEN
            cSourceString = cSourceString + cAlignmentStyle + "|".

        IF lTrimAvail THEN
            cSourceString = cSourceString + cTrim + "|".

        cSourceString = cSourceString + cFieldValueSuffix.
                
        /* Replace the key field with format and data type with the value */
        IF cFormat NE ? AND cFormat NE "" THEN
            ioplcRequestData = REPLACE(ioplcRequestData,cSourceString, cTargetString).    
    END.    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetAPITransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Returns the next transaction counter value of APIInbound record
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    
    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID
         NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN
        RETURN bf-APIOutbound.transactionCounter.
END FUNCTION.

FUNCTION fGetClientTransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Returns the next transaction counter value of apiClient record
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    DEFINE BUFFER bf-apiClient   FOR apiClient.
    
    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID 
         NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN DO:
        FIND FIRST bf-apiClient NO-LOCK
             WHERE bf-apiClient.company  EQ bf-APIOutbound.company
               AND bf-apiClient.clientID EQ bf-APIOutbound.clientID
             NO-ERROR.
        IF AVAILABLE bf-apiClient THEN
            RETURN bf-apiClient.transactionCounter.
    END.
    
    /* If apiClient record not found */
    RETURN 0.
END FUNCTION.

