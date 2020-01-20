/*------------------------------------------------------------------------
    File        : api\inbound\handlers\UpdateItemRequestHandler.p
    Purpose     : Updates itemfg with given key values for an item

    Syntax      :

    Description : Updates itemfg with given key values for an item

    Author(s)   : Porandla Mithun
    Created     : Thu Jan 16 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdCommonProcs AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdJSONProcs   AS HANDLE  NO-UNDO.
DEFINE VARIABLE iResponseCode AS INTEGER NO-UNDO.

{api/inbound/ttRequest.i}
{api/inbound/ttInput.i}

/* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
   create trigger, only if session.p is running persistently, else will be populated with empty value.
   ( refer methods/triggers/create.i ) */

/* This will eventually move to setsession approach */
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

RUN system/TagProcs.p PERSISTENT SET hdTags.
SESSION:ADD-SUPER-PROCEDURE (hdTags).

{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs).

RUN pPrepareInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF oplSuccess THEN DO:
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
END.

IF oplSuccess THEN DO:
    RUN pGenerateResponseData (
        OUTPUT oplcResponseData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
END.

IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR.

iResponseCode = IF oplSuccess THEN
                    200
                ELSE
                    400.

/* Generate the response data in fixed format of response_code, response_message and response_data */
RUN JSON_GetResponseData (
    INPUT  iResponseCode,      /* Value which goes into response_code tag */
    INPUT  opcMessage,         /* Value which goes into response_message tag */
    INPUT  oplcResponseData,   /* Value which goes into response_data tag */
    OUTPUT oplcResponseData
    ) NO-ERROR.

/* Log the request to APIInboundEvent */
RUN api\CreateAPIInboundEvent.p (
    INPUT ipcRoute,
    INPUT iplcRequestData,
    INPUT oplcResponseData,
    INPUT oplSuccess,
    INPUT opcMessage,
    INPUT NOW,
    INPUT ipcRequestedBy,
    INPUT ipcRecordSource,
    INPUT ipcNotes,
    INPUT  "", /* PayloadID */
    OUTPUT opcAPIInboundEvent
    ).

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
DELETE PROCEDURE hdSession.

SESSION:REMOVE-SUPER-PROCEDURE (hdTags).
DELETE PROCEDURE hdTags.

PROCEDURE pPrepareInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch input data into temp-table ttCounts
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFieldKey        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValue      AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidValue      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iItemsCounter    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexFieldOrder AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iItemsFieldOrder AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTopLevelParent  AS INTEGER   NO-UNDO  INITIAL 0.
    DEFINE VARIABLE cFieldOrderList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iInputCounter    AS INTEGER   NO-UNDO.
    
    /* Get request data fields in a temp-table */
    RUN ReadRequestData IN hdJSONProcs (
        INPUT  iplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttRequest
        ).

    /* Fetch Requester */
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.

    /* Get the count of Counts records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Items",
        INPUT  iTopLevelParent,
        OUTPUT iItemsCounter
        ) NO-ERROR.
    IF iItemsCounter EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No records found to process items"
            .
        RETURN.
    END.

    /* Browse through all the Counts records */
    DO iIndex = 0 TO iItemsCounter - 1:
            
        /* Fetch the Items field order, which will be further used as
           parent to fetch it's child records */
        RUN JSON_GetFieldOrderByNameValueAndParent (
            INPUT  "Items",
            INPUT  STRING(iIndex),
            INPUT  iTopLevelParent,
            OUTPUT lRecFound,
            OUTPUT iItemsFieldOrder
            ) NO-ERROR.
       
        /* Fetch the field order list sequence of Item's childs */
        RUN JSON_GetFieldOrderListByParent (
            INPUT  iItemsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cFieldOrderList
            ) NO-ERROR.
        
        IF cFieldOrderList EQ "" THEN
            NEXT.

        iInputCounter = iInputCounter + 1.

        DO iIndexFieldOrder = 1 TO NUM-ENTRIES(cFieldOrderList):

            RUN JSON_GetNameAndValueByFieldOrder (
                INPUT  INTEGER(ENTRY(iIndexFieldOrder,cFieldOrderList)),
                OUTPUT lRecFound,
                OUTPUT cFieldKey,
                OUTPUT cFieldValue
                ) NO-ERROR.

            IF lRecFound THEN DO:
                CREATE ttInput.
                ASSIGN
                    ttInput.fieldSeq   = iInputCounter
                    ttInput.fieldKey   = cFieldKey
                    ttInput.fieldValue = cFieldValue
                    .
            END.
        END.                
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to process input data
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.
    
    RUN api\inbound\UpdateItem.p (
        INPUT-OUTPUT TABLE ttInput,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
                
    IF ERROR-STATUS:ERROR THEN
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
END PROCEDURE.

PROCEDURE pGenerateResponseData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to generate response data
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcResponseData1 AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcResponseData2 AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-ttInput FOR ttInput.
    
    FOR EACH ttInput
        BREAK BY ttInput.fieldSeq:
        IF NOT FIRST-OF(ttInput.fieldSeq) THEN
            NEXT.

        ASSIGN
            lcResponseData1 = lcResponseData1 + "," + "~{"
            lcResponseData2 = ""
            .

        FOR EACH bf-ttInput 
            WHERE bf-ttInput.fieldSeq      EQ ttInput.fieldSeq
              AND bf-ttInput.fieldDB       NE ""
              AND bf-ttInput.fieldDataType NE ""
              AND bf-ttInput.success:
            lcResponseData2 = lcResponseData2 + "," + '"' + bf-ttInput.fieldKey + '":"'
                           + bf-ttInput.fieldValue + '"'.
        END.

        ASSIGN
            lcResponseData2 = TRIM(lcResponseData2,",")
            lcResponseData1 = lcResponseData1 + lcResponseData2 + "}"
            .
    END.

    ASSIGN
        oplcResponseData = oplcResponseData + "," + lcResponseData1.
        oplcResponseData = TRIM(oplcResponseData, ",")
        .
    
    ASSIGN        
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.
