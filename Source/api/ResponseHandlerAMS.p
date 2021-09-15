/*------------------------------------------------------------------------
    File        : api/ResponseHandlerAMS.p
    Purpose     : Reads the response from AMS ScheduleOrder API

    Syntax      :

    Description : Reads the response from AMS ScheduleOrder API

    Author(s)   : Rajesh
    Created     : Tue Mar 16 07:33:22 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
       
{XMLOutput/ttNodes.i NEW}

DEFINE VARIABLE hdXMLProcs            AS HANDLE    NO-UNDO.
DEFINE VARIABLE lRecFound             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iAmsDataResponseOrder AS INTEGER   NO-UNDO.
DEFINE VARIABLE iResponseOrder        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStatusCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMessage         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponseText         AS CHARACTER NO-UNDO.

RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).

RUN XML_ReadToTT IN hdXMLProcs (
    INPUT iplcResponseData
    ) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN
        opcMessage = RETURN-VALUE
        oplSuccess = FALSE
        .
    RETURN.
END.

RUN XML_GetFieldOrderByName (
    INPUT  "AmsDataResponse",
    OUTPUT lRecFound,
    OUTPUT iAmsDataResponseOrder
    ).
IF NOT lRecFound THEN DO:
    ASSIGN 
        oplSuccess = FALSE
        opcMessage = "Missing 'AmsDataResponse' tag in response"
        .
    RETURN.    
END.

RUN XML_GetFieldOrderByNameAndParent (
    INPUT  "Response",
    INPUT  iAmsDataResponseOrder,
    OUTPUT lRecFound,
    OUTPUT iResponseOrder
    ).
IF NOT lRecFound THEN DO:
    ASSIGN 
        oplSuccess = FALSE
        opcMessage = "Missing 'Response' tag in response"
        .
    RETURN.    
END.

RUN XML_GetFieldValueByNameAndParent (
    INPUT  "Status",
    INPUT  iResponseOrder,
    OUTPUT lRecFound,
    OUTPUT cStatusCode 
    ).

RUN XML_GetFieldValueByNameAndParent (
    INPUT  "ErrorMessage",
    INPUT  iResponseOrder,
    OUTPUT lRecFound,
    OUTPUT cErrorMessage
    ).

RUN XML_GetFieldValueByNameAndParent (
    INPUT  "responseText",
    INPUT  iResponseOrder,
    OUTPUT lRecFound,
    OUTPUT cResponseText
    ).    

ASSIGN
    oplSuccess = cStatusCode EQ "0"
    opcMessage = IF oplSuccess THEN
                     cResponseText
                 ELSE
                     cErrorMessage
    .

FINALLY:
    IF VALID-HANDLE(hdXMLProcs) THEN DO:
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdXMLProcs).
        DELETE PROCEDURE hdXMLProcs.
    END.
END FINALLY.