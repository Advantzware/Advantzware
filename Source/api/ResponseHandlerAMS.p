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
DEFINE VARIABLE cInfoOrderList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorOrderList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarningOrderList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMessage         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarningMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMessageList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarningMessageList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoMessageList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex                AS INTEGER   NO-UNDO.

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

RUN XML_GetFieldOrderListByNameAndParent (
    INPUT  "Info",
    INPUT  iResponseOrder,
    OUTPUT cInfoOrderList
    ).    

DO iIndex = 1 TO NUM-ENTRIES(cInfoOrderList):
    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "Message",
        INPUT  INTEGER(ENTRY(iIndex, cInfoOrderList)),
        OUTPUT lRecFound,
        OUTPUT cInfoMessage
        ). 
    IF lRecFound THEN        
        cInfoMessageList = cInfoMessageList + "~n"
                         + cInfoMessage.     
END.

RUN XML_GetFieldOrderListByNameAndParent (
    INPUT  "Error",
    INPUT  iResponseOrder,
    OUTPUT cErrorOrderList
    ).    

DO iIndex = 1 TO NUM-ENTRIES(cErrorOrderList):
    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "Message",
        INPUT  INTEGER(ENTRY(iIndex, cErrorOrderList)),
        OUTPUT lRecFound,
        OUTPUT cErrorMessage
        ). 
    IF lRecFound THEN        
        cErrorMessageList = cErrorMessageList + "~n"
                          + cErrorMessage.     
END.

RUN XML_GetFieldOrderListByNameAndParent (
    INPUT  "Warning",
    INPUT  iResponseOrder,
    OUTPUT cWarningOrderList
    ).    

DO iIndex = 1 TO NUM-ENTRIES(cWarningOrderList):
    RUN XML_GetFieldValueByNameAndParent (
        INPUT  "Message",
        INPUT  INTEGER(ENTRY(iIndex, cWarningOrderList)),
        OUTPUT lRecFound,
        OUTPUT cWarningMessage
        ). 
    IF lRecFound THEN        
        cWarningMessageList = cWarningMessageList + "~n"
                            + cWarningMessage.     
END.

ASSIGN
    oplSuccess          = cStatusCode NE "-1"
    cInfoMessageList    = TRIM(cInfoMessageList, "~n")
    cWarningMessageList = TRIM(cWarningMessageList, "~n")
    cErrorMessageList   = TRIM(cErrorMessageList, "~n")
    opcMessage          = (IF cInfoMessageList NE "" THEN cInfoMessageList + "~n" ELSE "")
                        + (IF cErrorMessageList NE "" THEN cErrorMessageList + "~n" ELSE "")
                        + cWarningMessageList
    .

FINALLY:
    IF VALID-HANDLE(hdXMLProcs) THEN DO:
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdXMLProcs).
        DELETE PROCEDURE hdXMLProcs.
    END.
END FINALLY.