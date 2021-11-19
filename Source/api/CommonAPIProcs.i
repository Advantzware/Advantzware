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

DEFINE VARIABLE gcRequestDataType AS CHARACTER NO-UNDO.

/* ************************  Function Prototypes ********************** */
FUNCTION fGetAPITransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ) FORWARD.

FUNCTION fGetClientTransactionCounter RETURNS INTEGER PRIVATE
	( INPUT ipiAPIOutboundID AS INTEGER ) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pSetRequestDataType PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.

    gcRequestDataType = ipcRequestDataType.
         
END PROCEDURE.

PROCEDURE pUpdateRequestDataType PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    
    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID 
         NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN
        RUN pSetRequestDataType (
            INPUT bf-APIOutbound.requestDataType
            ).
         
END PROCEDURE.

PROCEDURE pUpdateDelimiter PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Replaces and trims the delimiters of the request data for a given data type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    
    RUN pUpdateDelimiterAndTrim (
        INPUT-OUTPUT ioplcRequestData,
        INPUT        ipcRequestDataType,
        INPUT        TRUE
        ).  
END PROCEDURE.

PROCEDURE pUpdateDelimiterWithoutTrim PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Replaces the delimiters of the request data for a given data type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    
    RUN pUpdateDelimiterAndTrim (
        INPUT-OUTPUT ioplcRequestData,
        INPUT        ipcRequestDataType,
        INPUT        FALSE
        ).  
END PROCEDURE.

PROCEDURE pUpdateDelimiterAndTrim PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Replaces and trims the delimiters of the request data for a given data type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iplTrim            AS LOGICAL   NO-UNDO.
    
    ASSIGN
        ioplcRequestData = REPLACE(ioplcRequestData, "$comma$", ",")
        ioplcRequestData = REPLACE(ioplcRequestData, "$linefeed$", "~n")    /* Replaces $linefeed$ with carriage return character */
        ioplcRequestData = REPLACE(ioplcRequestData, "$formfeed$", CHR(12)) /* Replaces $formfeed$ with Form Feed character (PAGE keyword) */
        .

    IF iplTrim THEN
        ASSIGN
            ioplcRequestData = TRIM(ioplcRequestData, ",")
            ioplcRequestData = TRIM(ioplcRequestData, "~n")
            ioplcRequestData = TRIM(ioplcRequestData, CHR(12))
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

    RUN Format_UpdateRequestData (
        INPUT-OUTPUT ioplcRequestData,
        INPUT        ipcField,
        INPUT        ipcValue,
        INPUT        gcRequestDataType
        ).            
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

