
/*------------------------------------------------------------------------
    File        : JSONProcs.p
    Purpose     : 

    Syntax      :

    Description : JSON Handler

    Author(s)   : Porandla Mithun
    Created     : Mon Jun 24 07:57:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

{api/inbound/ttRequest.i}

FUNCTION fBeautifyJSON RETURNS LONGCHAR
    (iplcJSON AS LONGCHAR) FORWARD.

FUNCTION fFormatJSONFieldValue RETURNS CHARACTER PRIVATE
    (ipcFieldValue AS CHARACTER) FORWARD.
    
/* This is used for reading request JSON */
PROCEDURE ReadRequestData:
    DEFINE INPUT  PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRequest. 
    
    DEFINE VARIABLE iIndex1      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex2      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCounter     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParentID    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFieldValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldString AS CHARACTER NO-UNDO.

    /* Remove curly braces from request JSON string at the start and end */
    ASSIGN 
        iplcRequestData = REPLACE(iplcRequestData,'~{','')
        iplcRequestData = REPLACE(iplcRequestData,'}','')
        oplSuccess = YES
        .

    TEMP-TABLE-BLOCK:
    DO iIndex1 = 1 TO NUM-ENTRIES(iplcRequestData,','):   
        ASSIGN
            cFieldName  = ENTRY(1,ENTRY(iIndex1, iplcRequestData,','),":")
            cFieldValue = ENTRY(2,ENTRY(iIndex1, iplcRequestData,','),":")
            NO-ERROR.
            
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                oplSuccess = NO
                opcMessage = "Bad JSON Request"
                .
            LEAVE TEMP-TABLE-BLOCK.
        END.        

        DO iIndex2 = 1 TO NUM-ENTRIES(cFieldName, ".") BY 2:
            ASSIGN
                cFieldString = IF iIndex2 GE NUM-ENTRIES(cFieldName, ".") THEN
                                   cFieldValue
                               ELSE
                                   ENTRY(iIndex2 + 1, cFieldName, ".")
                .
 
            FIND FIRST ttRequest
                 WHERE ttRequest.fieldName   EQ REPLACE(ENTRY(iIndex2, cFieldName, "."),'"','')
                   AND ttRequest.fieldValue  EQ REPLACE(cFieldString,'"','')
                   AND ttRequest.fieldParent EQ iParentID
                 NO-ERROR.
            IF AVAILABLE ttRequest THEN DO:
                iParentID = ttRequest.fieldOrder.
                NEXT.
            END.
            
            iCounter = iCounter + 1.
                
            CREATE ttRequest.
            ASSIGN 
                ttRequest.fieldOrder  = iCounter
                ttRequest.fieldParent = iParentID
                ttRequest.fieldName   = ENTRY(iIndex2, cFieldName, ".")
                ttRequest.fieldValue  = cFieldString
                ttRequest.fieldName   = REPLACE(ttRequest.fieldName,'"','')
                ttRequest.fieldValue  = REPLACE(ttRequest.fieldValue,'"','')
                ttRequest.fieldValue  = fFormatJSONFieldValue(ttRequest.fieldValue)
                NO-ERROR.
                                
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    oplSuccess = NO
                    opcMessage = "Bad JSON Request"
                    .
                LEAVE TEMP-TABLE-BLOCK.
            END.
            
            iParentID = IF iIndex2 GE NUM-ENTRIES(cFieldName, ".") THEN
                            0
                        ELSE
                            iCounter.
        END.
    END.
END PROCEDURE.

PROCEDURE JSON_GetFieldValueByName:    
    DEFINE INPUT  PARAMETER ipcFieldName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttRequest FOR ttRequest.

    FIND FIRST bf-ttRequest
         WHERE bf-ttRequest.fieldName   EQ ipcFieldName
           AND bf-ttRequest.fieldParent EQ 0
         NO-ERROR.
    IF AVAILABLE bf-ttRequest THEN
        ASSIGN
            oplRecFound   = TRUE
            opcFieldValue = bf-ttRequest.fieldValue
            .
        
    RELEASE bf-ttRequest.
END PROCEDURE.

PROCEDURE JSON_GetFieldValueByNameAndParent:    
    DEFINE INPUT  PARAMETER ipcFieldName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttRequest FOR ttRequest.

    FIND FIRST bf-ttRequest
         WHERE bf-ttRequest.fieldName   EQ ipcFieldName 
           AND bf-ttRequest.fieldParent EQ ipiParentID
         NO-ERROR.
    IF AVAILABLE bf-ttRequest THEN
        ASSIGN
            oplRecFound   = TRUE
            opcFieldValue = bf-ttRequest.fieldValue
            .

    RELEASE bf-ttRequest.        
END PROCEDURE.

PROCEDURE JSON_GetFieldOrderByNameValueAndParent:    
    DEFINE INPUT  PARAMETER ipcFieldName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldValue AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opiFieldOrder AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttRequest FOR ttRequest.

    FIND FIRST bf-ttRequest
         WHERE bf-ttRequest.fieldName   EQ ipcFieldName 
           AND bf-ttRequest.fieldValue  EQ ipcFieldValue
           AND bf-ttRequest.fieldParent EQ ipiParentID
         NO-ERROR.
    IF AVAILABLE bf-ttRequest THEN
        ASSIGN
            oplRecFound   = TRUE
            opiFieldOrder = bf-ttRequest.fieldOrder
            .

    RELEASE bf-ttRequest.        
END PROCEDURE.
    
PROCEDURE JSON_GetRecordCountByNameAndParent:
    DEFINE INPUT  PARAMETER ipcFieldName   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID    AS INTEGER   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcRecordCount AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttRequest FOR ttRequest.
    
    FOR EACH bf-ttRequest
        WHERE bf-ttRequest.fieldName   EQ ipcFieldName 
          AND bf-ttRequest.fieldParent EQ ipiParentID:
        opcRecordCount = opcRecordCount + 1.  
    END.    

    RELEASE bf-ttRequest.    
END PROCEDURE.

PROCEDURE JSON_UpdateFieldValue:
    DEFINE INPUT-OUTPUT PARAMETER ioplcJSONData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcField      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcValue      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldValuePrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValueSuffix AS CHARACTER NO-UNDO.        
    
    ASSIGN
        cFieldValuePrefix = "$"
        cFieldValueSuffix = "$"
        .
        
    RUN JSON_EscapeExceptionalCharacters (
        INPUT-OUTPUT ipcValue
        ).  
    
    ioplcJSONData = REPLACE(ioplcJSONData, cFieldValuePrefix + ipcField + cFieldValueSuffix, ipcValue).
    
END PROCEDURE. 

PROCEDURE JSON_EscapeExceptionalCharacters:
    DEFINE INPUT-OUTPUT PARAMETER ipcFieldValue AS CHARACTER NO-UNDO.
    
    IF ipcFieldValue EQ ? THEN
        ipcFieldValue = "".
    
    /* This will add an escape character (\) before any JSON exceptional 
       characters (double quote and backward slash) so JSON parsing won't 
       throw error */
    ASSIGN
        ipcFieldValue = REPLACE(ipcFieldValue, '\','\\')
        ipcFieldValue = REPLACE(ipcFieldValue, '/','\/')
        ipcFieldValue = REPLACE(ipcFieldValue, '"','\"')
        .
END PROCEDURE. 

FUNCTION fBeautifyJSON RETURNS LONGCHAR
    (iplcJSON AS LONGCHAR):
    DEFINE VARIABLE cIndentation      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFormattedJSON    AS LONGCHAR    NO-UNDO.
    DEFINE VARIABLE cTemp             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iIndentationLevel AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCtr              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCtr2             AS INTEGER     NO-UNDO.

    ASSIGN
        cIndentation      = "  "
        iIndentationLevel = 0
        cFormattedJSON    = ""
        cTemp             = ""
        .

    DO iCtr = 1 TO LENGTH(iplcJSON):
        cTemp = SUBSTRING (iplcJSON, iCtr, 1).
        CASE cTemp:
            WHEN '\\' THEN DO:
                cFormattedJSON = cFormattedJSON + cTemp.
            END.
            WHEN '~{' OR WHEN  '[' THEN DO:
                ASSIGN
                    cFormattedJSON    = cFormattedJSON + cTemp
                    cFormattedJSON    = cFormattedJSON + "~n"
                    iIndentationLevel = iIndentationLevel + 1
                    .
                do iCtr2 = 1 TO iIndentationLevel:
                    cFormattedJSON = cFormattedJSON + cIndentation.
                END.
            END.
            WHEN '~}' OR WHEN ']' THEN DO:
                ASSIGN
                    cFormattedJSON    = cFormattedJSON + "~n"
                    IindentationLevel = iIndentationLevel - 1
                    .
                DO iCtr2 = 1 TO iIndentationLevel:
                    cFormattedJSON = cFormattedJSON + cIndentation.
                END.
    
                cFormattedJSON = cFormattedJSON + cTemp.
            END.
            WHEN ',' THEN DO:
                ASSIGN
                    cFormattedJSON = cFormattedJSON + cTemp
                    cFormattedJSON = cFormattedJSON + "~n"
                    .
                DO iCtr2 = 1 TO iIndentationLevel:
                    cFormattedJSON = cFormattedJSON + cIndentation.
                END.
                
            END.
            OTHERWISE
                cFormattedJSON = cFormattedJSON + cTemp.     
        END.
    END.
    RETURN cFormattedJSON.
END FUNCTION.    
    
FUNCTION fFormatJSONFieldValue RETURNS CHARACTER PRIVATE
    (ipcFieldValue AS CHARACTER):
    DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO.
    
    ASSIGN
        cFieldValue = REPLACE(ipcFieldValue,'<comma>',',')
        .
    
    RETURN cFieldValue.
END FUNCTION.    

