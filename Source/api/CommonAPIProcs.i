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
DEFINE VARIABLE hdFormatProcs AS HANDLE NO-UNDO.
RUN system/FormatProcs.p PERSISTENT SET hdFormatProcs.



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
        $keyfield|format|datatype|$
        Eg. $poID|>>>>>>>9|INT|$, $poNotes|X(30)|$, $poData|YYYYMMDD|DATE|$
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
            cFormat     = ""
            cFormatType = ""
            cNextChar   = SUBSTRING(ioplcRequestData,INDEX(ioplcRequestData,cFieldValuePrefix + ipcField) + LENGTH(cFieldValuePrefix + ipcField), 1)
            .
        
        /* If $ do nothing, as it would have been already replaced */
        IF cNextChar EQ "$" THEN
            cFormat = "".
        /* If |, then a format exists */
        ELSE IF cNextChar EQ "|" THEN DO:
            iIndex = INDEX(ioplcRequestData,cFieldValuePrefix + ipcField) + LENGTH(cFieldValuePrefix + ipcField) + 1.
            
            /* Code to retrieve the format */
            DO WHILE TRUE:
                IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                    LEAVE.
                
                ASSIGN
                    cFormat = cFormat + SUBSTRING(ioplcRequestData,iIndex,1)
                    iIndex  = iIndex + 1
                    .
            END.

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
            ELSE IF cFormatType EQ "DATE" THEN
                RUN Format_DateTimeTZ IN hdFormatProcs (
                    INPUT  DATETIME-TZ(ipcValue),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
        END.
        ELSE
            cTargetString = STRING(ipcValue,cFormat).
        
        IF cFormatType NE "" THEN
            cSourceString = cFieldValuePrefix + ipcField + "|" + cFormat + "|" + cFormatType + "|" + cFieldValueSuffix.
        ELSE
            cSourceString = cFieldValuePrefix + ipcField + "|" + cFormat + "|" + cFieldValueSuffix.
        
        /* Replace the key field with format and data type with the value */
        IF cFormat NE ? AND cFormat NE "" THEN
            ioplcRequestData = REPLACE(ioplcRequestData,cSourceString, cTargetString).    
    END.    
END PROCEDURE.

