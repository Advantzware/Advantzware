/*------------------------------------------------------------------------
    File        : api/OutboundProcs.p
    Purpose     : Procedures related to Outbound API

    Syntax      :

    Description : Procedures related to Outbound API

    Author(s)   : Porandla Mithun
    Created     : Fri September 06 07:33:22 EDT 2019
    Notes       : This procedure should not be run as super procedure in SESSION.
                  The life cycle of this procedure should be limited to the calling
                  procedure only
  ----------------------------------------------------------------------*/
{api/ttArgs.i}

DEFINE TEMP-TABLE ttRequestData NO-UNDO
    FIELD company              AS CHARACTER
    FIELD location             AS CHARACTER
    FIELD apiID                AS CHARACTER
    FIELD clientID             AS CHARACTER
    FIELD triggerID            AS CHARACTER
    FIELD primaryID            AS CHARACTER
    FIELD eventDesc            AS CHARACTER
    FIELD tableList            AS CHARACTER
    FIELD rowidList            AS CHARACTER
    FIELD requestStatus        AS CHARACTER
    FIELD parentProgram        AS CHARACTER
    FIELD requestMessage       AS CHARACTER
    FIELD requestType          AS CHARACTER
    FIELD requestDataType      AS CHARACTER
    FIELD apiOutboundID        AS INT64
    FIELD apiOutboundTriggerID AS INT64
    FIELD apiOutboundEventID   AS INT64
    FIELD success              AS LOGICAL
    FIELD reTrigger            AS LOGICAL
    FIELD requestData          AS CLOB
    FIELD responseData         AS CLOB
    .

{api/ttAPIOutboundEvent.i}

DEFINE VARIABLE cRequestTypeList          AS CHARACTER NO-UNDO INITIAL "API,FTP,SAVE".
DEFINE VARIABLE cRequestVerbList          AS CHARACTER NO-UNDO INITIAL "POST,GET".
DEFINE VARIABLE cRequestDataTypeList      AS CHARACTER NO-UNDO INITIAL "JSON,XML,TXT,CSV".
DEFINE VARIABLE cRequestStatusInitialized AS CHARACTER NO-UNDO INITIAL "Initialized".
DEFINE VARIABLE cRequestStatusPrepared    AS CHARACTER NO-UNDO INITIAL "Prepared".
DEFINE VARIABLE cRequestStatusError       AS CHARACTER NO-UNDO INITIAL "Error".
DEFINE VARIABLE cRequestStatusSuccess     AS CHARACTER NO-UNDO INITIAL "Success".
DEFINE VARIABLE cRequestStatusFailed      AS CHARACTER NO-UNDO INITIAL "Failed".
DEFINE VARIABLE cRequestTypeAPI           AS CHARACTER NO-UNDO INITIAL "API".
DEFINE VARIABLE cRequestTypeFTP           AS CHARACTER NO-UNDO INITIAL "FTP".
DEFINE VARIABLE cRequestTypeSAVE          AS CHARACTER NO-UNDO INITIAL "SAVE".
DEFINE VARIABLE cLocValidationExceptions  AS CHARACTER NO-UNDO INITIAL "SendAdvancedShipNotice,SendFinishedGood". /* Should be comma (,) separated. loc.isAPIEnabled will not be validated for APIs in the list */
DEFINE VARIABLE cScopeTypeList            AS CHARACTER NO-UNDO INITIAL "Customer,Vendor,ShipTo".


/* **********************  Internal Procedures  *********************** */


PROCEDURE Outbound_CopyAPIDependencies:
/*------------------------------------------------------------------------------
 Purpose: Copies all API dependent table records and links to target API Outbound
          table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiSourceAPIOutboundID AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTargetAPIOutboundID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound-Source        FOR APIOutbound.
    DEFINE BUFFER bf-APIOutbound-Target        FOR APIOutbound.
    DEFINE BUFFER bf-APIOutboundDetail-Source  FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundDetail-Target  FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundTrigger-Source FOR APIOutboundTrigger.
    DEFINE BUFFER bf-APIOutboundTrigger-Target FOR APIOutboundTrigger.
      
    FIND FIRST bf-APIOutbound-Source NO-LOCK
         WHERE bf-APIOutbound-Source.apiOutboundID EQ ipiSourceAPIOutboundID
           NO-ERROR.
    IF NOT AVAILABLE bf-APIOutbound-Source THEN
        RETURN.
        
    FIND FIRST bf-APIOutbound-Target NO-LOCK
         WHERE bf-APIOutbound-Target.apiOutboundID EQ ipiTargetAPIOutboundID
           NO-ERROR.
    IF NOT AVAILABLE bf-APIOutbound-Target THEN
        RETURN.

    FOR EACH bf-APIOutboundDetail-Source NO-LOCK
        WHERE bf-APIOutboundDetail-Source.apiOutboundID EQ bf-APIOutbound-Source.apiOutboundID:
        FIND FIRST bf-APIOutboundDetail-Target NO-LOCK
             WHERE bf-APIOutboundDetail-Target.company  EQ bf-APIOutbound-Target.company
               AND bf-APIOutboundDetail-Target.apiID    EQ bf-APIOutbound-Target.apiID
               AND bf-APIOutboundDetail-Target.clientID EQ bf-APIOutbound-Target.clientID
               AND bf-APIOutboundDetail-Target.detailID EQ bf-APIOutboundDetail-Source.detailID
             NO-ERROR.
        IF NOT AVAILABLE bf-APIOutboundDetail-Target THEN DO:
            CREATE bf-APIOutboundDetail-Target.
            BUFFER-COPY bf-APIOutboundDetail-Source 
                EXCEPT bf-APIOutboundDetail-Source.apiID 
                       bf-APIOutboundDetail-Source.clientID 
                       bf-APIOutboundDetail-Source.apiOutboundID 
                       bf-APIOutboundDetail-Source.apiOutboundDetailID
                       bf-APIOutboundDetail-Source.rec_key
                TO bf-APIOutboundDetail-Target.
            ASSIGN
                bf-APIOutboundDetail-Target.apiID         = bf-APIOutbound-Target.apiID
                bf-APIOutboundDetail-Target.clientID      = bf-APIOutbound-Target.clientID
                bf-APIOutboundDetail-Target.apiOutboundID = bf-APIOutbound-Target.apiOutboundID
                .
        END.
    END.

    FOR EACH bf-APIOutboundTrigger-Source NO-LOCK
        WHERE bf-APIOutboundTrigger-Source.apiOutboundID EQ bf-APIOutbound-Source.apiOutboundID:
        FIND FIRST bf-APIOutboundTrigger-Target NO-LOCK
             WHERE bf-APIOutboundTrigger-Target.company   EQ bf-APIOutbound-Target.company
               AND bf-APIOutboundTrigger-Target.apiID     EQ bf-APIOutbound-Target.apiID
               AND bf-APIOutboundTrigger-Target.clientID  EQ bf-APIOutbound-Target.clientID
               AND bf-APIOutboundTrigger-Target.triggerID EQ bf-APIOutboundTrigger-Source.triggerID
             NO-ERROR.        
        IF NOT AVAILABLE bf-APIOutboundTrigger-Target THEN DO:
            CREATE bf-APIOutboundTrigger-Target.
            BUFFER-COPY bf-APIOutboundTrigger-Source 
                EXCEPT bf-APIOutboundTrigger-Source.apiID 
                       bf-APIOutboundTrigger-Source.clientID 
                       bf-APIOutboundTrigger-Source.apiOutboundID 
                       bf-APIOutboundTrigger-Source.apiOutboundTriggerID
                       bf-APIOutboundTrigger-Source.rec_key
                TO bf-APIOutboundTrigger-Target.
            ASSIGN
                bf-APIOutboundTrigger-Target.apiID         = bf-APIOutbound-Target.apiID
                bf-APIOutboundTrigger-Target.clientID      = bf-APIOutbound-Target.clientID
                bf-APIOutboundTrigger-Target.apiOutboundID = bf-APIOutbound-Target.apiOutboundID
                .
        END.
    END.
END PROCEDURE.

PROCEDURE Outbound_GetAPIID:
    /*------------------------------------------------------------------------------
     Purpose: Get Outbound API Sequence ID of given inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAPIOutboundID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.

    FIND FIRST APIOutbound NO-LOCK
          WHERE APIOutbound.company  EQ ipcCompany
            AND APIOutbound.apiID    EQ ipcAPIID
            AND APIOutbound.clientID EQ ipcClientID
          NO-ERROR.
    IF AVAILABLE APIOutbound AND
        NOT APIOutbound.Inactive THEN
        ASSIGN
            oplValid         = TRUE
            opcMessage       = "Success"
            opiAPIOutboundID = APIOutbound.apiOutboundID
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Outbound configuration for API ID ["
                       + ipcAPIID + "] is not available or inactive"
            .
END PROCEDURE.

PROCEDURE Outbound_GetAPIRequestType:
/*------------------------------------------------------------------------------
 Purpose: Returns request type of an API
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRequestType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    
    FIND FIRST bf-APIOutbound NO-LOCK
          WHERE bf-APIOutbound.company  EQ ipcCompany
            AND bf-APIOutbound.apiID    EQ ipcAPIID
            AND bf-APIOutbound.clientID EQ ipcClientID
          NO-ERROR.
    IF AVAILABLE bf-APIOutbound AND
        NOT bf-APIOutbound.Inactive THEN
        ASSIGN
            oplValid       = TRUE
            opcMessage     = "Success"
            opcRequestType = bf-APIOutbound.requestType
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Outbound configuration for API ID ["
                       + ipcAPIID + "] is not available or inactive"
            .
END PROCEDURE.

PROCEDURE Outbound_GetAPITriggerID:
    /*------------------------------------------------------------------------------
     Purpose: Get Outbound Trigger API Sequence ID of given inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID                AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    FIND FIRST APIOutboundTrigger NO-LOCK
         WHERE APIOutboundTrigger.company   EQ ipcCompany
           AND APIOutboundTrigger.apiID     EQ ipcAPIID
           AND APIOutboundTrigger.clientID  EQ ipcClientID
           AND APIOutboundTrigger.triggerID EQ ipcTriggerID
         NO-ERROR.
    IF AVAILABLE APIOutboundTrigger AND
        NOT APIOutboundTrigger.Inactive THEN
        ASSIGN
            oplValid                = TRUE
            opcMessage              = "Success"
            opiAPIOutboundTriggerID = APIOutbound.apiOutboundID
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Outbound Trigger configuration for Trigger ID ["
                       + ipcTriggerID + "] is not available or inactive"
            .
END PROCEDURE.

PROCEDURE Outbound_GetClientIDForScope:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientID  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-apiClientXref FOR apiClientXref.
    DEFINE BUFFER bf-apiClient     FOR apiClient.
     
    FIND FIRST bf-apiClientXref NO-LOCK
         WHERE bf-apiClientXref.company   EQ ipcCompany
           AND bf-apiClientXref.scopeID   EQ ipcScopeID
           AND bf-apiClientXref.scopeType EQ ipcScopeType
         NO-ERROR.
    IF AVAILABLE bf-apiClientXref THEN DO:
        FIND FIRST bf-apiClient NO-LOCK
             WHERE bf-apiClient.company  EQ bf-apiClientXref.company
               AND bf-apiClient.clientID EQ bf-apiClientXref.clientID
             NO-ERROR.
        IF AVAILABLE bf-apiClient THEN
            ASSIGN
                oplFound    = TRUE
                opcClientID = bf-apiClient.clientID
                .
    END.
END PROCEDURE.

PROCEDURE Outbound_GetRequestTypeList:
/*------------------------------------------------------------------------------
 Purpose: Returns the comma separated list of outbound request types
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRequestTypeList AS CHARACTER NO-UNDO.
    
    opcRequestTypeList = cRequestTypeList. 
END PROCEDURE.

PROCEDURE Outbound_GetRequestDataTypeList:
/*------------------------------------------------------------------------------
 Purpose: Returns the comma separated list of outbound request data types
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRequestDataTypeList AS CHARACTER NO-UNDO.
    
    opcRequestDataTypeList = cRequestDataTypeList.
END PROCEDURE.

PROCEDURE Outbound_GetRequestVerbList:
/*------------------------------------------------------------------------------
 Purpose: Returns the comma separated list of outbound request verbs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRequestVerbList AS CHARACTER NO-UNDO.
    
    opcRequestVerbList = cRequestVerbList.
END PROCEDURE.

PROCEDURE Outbound_GetScopeTypeList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcScopeTypeList AS CHARACTER NO-UNDO.
    
    opcScopeTypeList = cScopeTypeList.
END PROCEDURE.

PROCEDURE Outbound_PrepareAndExecuteForScope:
    /*----------------------------------------------------------vi--------------------
     Purpose: Public wrapper procedure to prepare request data and call Outbound API
              for a given scope id and scope Type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcScopeID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cClientID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lClientFound AS LOGICAL   NO-UNDO.
    
    RUN Outbound_GetClientIDForScope (
        INPUT  ipcCompany,
        INPUT  ipcScopeID,
        INPUT  ipcScopeType,
        OUTPUT lClientFound,
        OUTPUT cClientID
        ).
    
    IF NOT lClientFound THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Client not found for scope ID " + ipcScopeID + " and scope type " + ipcScopeType
            .
        RETURN. 
    END.    
        
    RUN pPrepareAndExecute (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  cClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  FALSE, /* Re-Trigger request */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.

END PROCEDURE.

PROCEDURE Outbound_ReTrigger:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to re-triggers the Outbound API request for 
              the given APIOutboundEventID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiAPIOutboundEventID AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage            AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.

    FIND FIRST bf-APIOutboundEvent NO-LOCK
         WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ipiAPIOutboundEventID
         NO-ERROR.

    IF AVAILABLE bf-APIOutboundEvent THEN DO:
        RUN pPrepareAndExecute (
            INPUT  bf-APIOutboundEvent.Company,
            INPUT  bf-APIOutboundEvent.LocationID,
            INPUT  bf-APIOutboundEvent.APIID,
            INPUT  bf-APIOutboundEvent.ClientID,
            INPUT  bf-APIOutboundEvent.sourceTriggerID,
            INPUT  "APIOutboundEvent",                   /* Pass APIOutboundEvent table in the list of tables */
            INPUT  STRING(ROWID(bf-APIOutboundEvent)),
            INPUT  bf-APIOutboundEvent.PrimaryID,
            INPUT  bf-APIOutboundEvent.EventDescription,
            INPUT  TRUE,  /* Re-Trigger request */
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
    END.

    RELEASE bf-APIOutboundEvent.
END PROCEDURE.

PROCEDURE Outbound_PrepareAndExecute:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to prepare request data and call Outbound API
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    RUN pPrepareAndExecute (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  FALSE, /* Re-Trigger request */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE Outbound_PrepareRequest:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to prepare request data
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    RUN pInitializeAndPrepareRequest (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  FALSE, /* Re-Trigger request */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE Outbound_Execute:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to call outbound API for the prepared 
              request data
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    RUN pExecute (
        INPUT  FALSE,  /* Re-Trigger request */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE Outbound_GetEvents:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to get the event IDs generated from preparing and calling
              outbound API
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttAPIOutboundEvent.

    /* This procedure returns ttAPIOutboundEvent as output */

    /* Here we collect all the event IDs before ttRequestData table data is reset */
    EMPTY TEMP-TABLE ttAPIOutboundEvent.
    FOR EACH ttRequestData:

        IF LOOKUP(ttRequestData.requestStatus, cRequestStatusInitialized + "," + cRequestStatusPrepared) NE 0 THEN
            NEXT.

        CREATE ttAPIOutboundEvent.
        ttAPIOutboundEvent.apiOutboundEventID = ttRequestData.apiOutboundEventID.
    END.
END PROCEDURE.

PROCEDURE Outbound_ResetContext:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAPIOutboundEvent.
    EMPTY TEMP-TABLE ttRequestData.
END.

PROCEDURE Outbound_ValidateClientID:
/*------------------------------------------------------------------------------
 Purpose: Validate clientID from apiClient table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-apiClient FOR apiClient.
    
    FIND FIRST bf-apiClient NO-LOCK
         WHERE bf-apiClient.company  EQ ipcCompany
           AND bf-apiClient.clientID EQ ipcClientID
         NO-ERROR.
    IF AVAILABLE bf-apiClient THEN
        ASSIGN
            oplValid   = TRUE
            opcMessage = "Success"
            .
    ELSE
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Invalid clientID '" + ipcClientID + "'"
            . 
END PROCEDURE.

PROCEDURE pInitializeRequest PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Initial procedure to validate and save input data into temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    IF ipcAPIID EQ "" THEN DO:
        ASSIGN
            opcMessage = "API ID cannot be empty"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    IF ipcTableList EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty value passed in table list"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    IF ipcROWIDList EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty value passed in ROW ID list"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    IF NUM-ENTRIES(ipcTableList) NE NUM-ENTRIES(ipcROWIDList) THEN DO:
        ASSIGN
            opcMessage = "Mismatch of number of entries in table and rowid list"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    IF ipcPrimaryID EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty value passed in Primary ID"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    RUN pPopulateRequestData (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  iplReTrigger,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pPopulateRequestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to populate request data for the given inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    IF iplReTrigger THEN DO:
        RUN pPopulateRequestDataForReTrigger (
            INPUT  ipcCompany,
            INPUT  ipcLocation,
            INPUT  ipcAPIID,
            INPUT  ipcClientID,
            INPUT  ipcTriggerID,
            INPUT  ipcPrimaryID,
            INPUT  ipcEventDescription,
            INPUT  ipcTableList,
            INPUT  ipcROWIDList,
            INPUT  iplReTrigger,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).

        RETURN.
    END.

    /* The following code will be executed if it is a fresh (NOT re-triggered) API call */
    FOR EACH APIOutbound NO-LOCK
       WHERE APIOutbound.company EQ ipcCompany
         AND APIOutbound.apiID   EQ ipcAPIID
         AND (IF ipcClientID EQ "" THEN
                  TRUE
              ELSE
                  APIOutbound.clientID EQ ipcClientID):

        IF APIOutbound.Inactive THEN
            NEXT.

        FIND FIRST APIOutboundTrigger NO-LOCK
             WHERE APIOutboundTrigger.apiOutboundID EQ APIOutbound.apiOutboundID
               AND APIOutboundTrigger.triggerID     EQ ipcTriggerID
               AND APIOutboundtrigger.Inactive      EQ FALSE
             NO-ERROR.
        IF NOT AVAILABLE APIOutboundTrigger THEN
            NEXT.

        RUN pCreateTTRequestData (
            INPUT ipcCompany,
            INPUT ipcLocation,
            INPUT ipcAPIID,
            INPUT APIOutbound.clientID,
            INPUT ipcTriggerID,
            INPUT ipcPrimaryID,
            INPUT ipcEventDescription,
            INPUT ipcTableList,
            INPUT ipcROWIDList,
            INPUT APIOutbound.requestType,
            INPUT APIOutbound.requestDataType,
            INPUT iplReTrigger,
            INPUT "",                                     /* Empty request data - Populated after Preparing request data */
            INPUT "",                                     /* Empty response data - Populated after Calling Outbound API */
            INPUT APIOutbound.apiOutboundID,
            INPUT APIOutboundTrigger.apiOutboundTriggerID,
            INPUT 0                                       /* apiOutboundEventID - Populates once an APIOutboundEvent record is created */
            ) NO-ERROR.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pPopulateRequestDataForReTrigger PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to populate request data for re-trigger events
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcRequestData  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER  NO-UNDO.

    EMPTY TEMP-TABLE ttArgs.

    DO iCount = 1 TO NUM-ENTRIES(ipcTableList):
        CREATE ttArgs.
        ASSIGN
            ttArgs.argType  = "ROWID"
            ttArgs.argKey   = ENTRY(iCount,ipcTableList)
            ttArgs.argValue = ENTRY(iCount,ipcROWIDList)
            .
    END.

    FOR EACH ttArgs
        WHERE ttArgs.argType EQ "ROWID"
          AND ttArgs.argKey  EQ "APIOutboundEvent"
          ,
          FIRST APIOutboundEvent NO-LOCK
          WHERE ROWID(APIOutboundEvent) = TO-ROWID(ttArgs.argValue):

        ASSIGN
            lcRequestData  = APIOutboundEvent.requestData
            lcResponseData = APIOutboundEvent.responseData
            .

        RUN pCreateTTRequestData (
            INPUT APIOutboundEvent.company,
            INPUT ipcLocation,
            INPUT APIOutboundEvent.apiID,
            INPUT APIOutboundEvent.clientID,
            INPUT APIOutboundEvent.sourceTriggerID,
            INPUT ipcPrimaryID,
            INPUT ipcEventDescription,
            INPUT ttArgs.argKey,
            INPUT ttArgs.argValue,
            INPUT "",                          /* Empty requestType - populates after validation with APIOutbound */
            INPUT "",                          /* Empty requestDataType - populates after validation with APIOutbound */
            INPUT iplReTrigger,
            INPUT lcRequestData,
            INPUT lcResponseData,
            INPUT 0,                           /* APIOutboundID - populates after validation with APIOutbound */
            INPUT 0,                           /* APIOutboundTriggerID - Only required to prepare request data. Not required in case of re-triggers as request data is already prepared*/
            INPUT APIOutboundEvent.apiOutboundEventID
            ) NO-ERROR.
    END.

    FOR EACH ttRequestData
        WHERE ttRequestData.requestStatus EQ cRequestStatusInitialized
          AND ttRequestData.reTrigger:

        ASSIGN
            ttRequestData.requestStatus = cRequestStatusPrepared
            lcRequestData               = ttRequestData.requestData
            lcResponseData              = ttRequestData.responseData
            .

        FIND FIRST APIOutbound NO-LOCK
             WHERE APIOutbound.company  EQ ttRequestData.company
               AND APIOutbound.apiID    EQ ttRequestData.apiID
               AND APIOutbound.clientID EQ ttRequestData.clientID
             NO-ERROR.
        IF AVAILABLE APIOutbound AND
           NOT APIOutbound.Inactive THEN DO:

            ASSIGN
                ttRequestData.apiOutboundID   = APIOutbound.apiOutboundID
                ttRequestData.requestType     = APIOutbound.requestType
                ttRequestData.requestDataType = APIOutbound.requestDataType
                .

            /* Validate if location is API enabled (see APIEnabled toggle box in I-F-4 screen) */
            RUN pValidateLocation (
                INPUT  ttRequestData.company,
                INPUT  ttRequestData.location,
                INPUT  ttRequestData.apiID,
                OUTPUT ttRequestData.success,
                OUTPUT ttRequestData.requestMessage
                ) NO-ERROR.

            IF ttRequestData.success AND NOT ERROR-STATUS:ERROR THEN
                NEXT.

            IF NOT ttRequestData.success THEN
                ttRequestData.requestStatus = cRequestStatusFailed.

            IF ERROR-STATUS:ERROR THEN
                ASSIGN
                    ttRequestData.requestMessage = ERROR-STATUS:GET-MESSAGE(1)
                    ttRequestData.requestStatus  = cRequestStatusError
                    .
        END.
        ELSE DO:
            ASSIGN
                ttRequestData.requestMessage = "API Outbound configuration for Outbound Sequence ID ["
                                             + APIOutboundEvent.apiID
                                             + "], is not available or inactive"
                ttRequestData.requestStatus  = cRequestStatusFailed
                .
        END.

        RUN api/CreateAPIOutboundEvent.p (
            INPUT  iplReTrigger,                        /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record */
            INPUT  ttRequestData.apiOutboundEventID,    /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
            INPUT  ttRequestData.company,
            INPUT  ttRequestData.location,
            INPUT  ttRequestData.apiID,
            INPUT  ttRequestData.clientID,
            INPUT  ttRequestData.triggerID,
            INPUT  ttRequestData.primaryID,
            INPUT  ttRequestData.eventDesc,
            INPUT  lcRequestData,
            INPUT  lcResponseData,
            INPUT  ttRequestData.parentProgram,
            INPUT  ttRequestData.success,
            INPUT  ttRequestData.requestMessage,
            INPUT  NOW,
            OUTPUT ttRequestData.apiOutboundEventID
            ) NO-ERROR.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pPrepareRequest PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to populate request data
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcRequestData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER  NO-UNDO.

    FOR EACH ttRequestData
        WHERE ttRequestData.requestStatus EQ cRequestStatusInitialized
          AND NOT ttRequestData.reTrigger:

        EMPTY TEMP-TABLE ttArgs.

        DO iCount = 1 TO NUM-ENTRIES(ttRequestData.tableList):
            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = ENTRY(iCount,ttRequestData.tableList)
                ttArgs.argValue = ENTRY(iCount,ttRequestData.rowidList)
                .
        END.

        ASSIGN
            ttRequestData.requestStatus = cRequestStatusPrepared
            lcRequestData               = ttRequestData.requestData
            .

        RUN api/PrepareOutboundRequest.p (
            INPUT  TABLE ttArgs,
            INPUT  ttRequestData.apiOutboundID,
            INPUT  ttRequestData.apiOutboundTriggerID,
            OUTPUT lcRequestData,
            OUTPUT ttRequestData.success,
            OUTPUT ttRequestData.requestMessage
            ) NO-ERROR.

        IF ttRequestData.success AND NOT ERROR-STATUS:ERROR THEN
            /* Validate location after preparing the request data */
            RUN pValidateLocation (
                INPUT  ttRequestData.company,
                INPUT  ttRequestData.location,
                INPUT  ttRequestData.apiID,
                OUTPUT ttRequestData.success,
                OUTPUT ttRequestData.requestMessage
                ) NO-ERROR.

        IF ttRequestData.success AND NOT ERROR-STATUS:ERROR THEN DO:
            ttRequestData.requestData = lcRequestData.

            NEXT.
        END.

        IF NOT ttRequestData.success THEN
            ttRequestData.requestStatus = cRequestStatusFailed.

        IF ERROR-STATUS:ERROR THEN
            ASSIGN
                ttRequestData.requestMessage = ERROR-STATUS:GET-MESSAGE(1)
                ttRequestData.requestStatus  = cRequestStatusError
                .

        RUN api/CreateAPIOutboundEvent.p (
            INPUT  ttRequestData.reTrigger,        /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record for the given apiOutboundEventID */
            INPUT  ?,                              /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
            INPUT  ttRequestData.company,
            INPUT  ttRequestData.location,
            INPUT  ttRequestData.apiID,
            INPUT  ttRequestData.clientID,
            INPUT  ttRequestData.triggerID,
            INPUT  ttRequestData.primaryID,
            INPUT  ttRequestData.eventDesc,
            INPUT  lcRequestData,
            INPUT  "",                             /* Response Data */
            INPUT  ttRequestData.parentProgram,
            INPUT  ttRequestData.success,
            INPUT  ttRequestData.requestMessage,
            INPUT  NOW,
            OUTPUT ttRequestData.apiOutboundEventID
            ) NO-ERROR.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pInitializeAndPrepareRequest PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to initialize and prepare request data
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    RUN pInitializeRequest (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  iplReTrigger,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF oplSuccess THEN
        RUN pPrepareRequest (
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE pExecute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to call the outbound APIs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplReTrigger  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcRequestData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcResponseData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iOutboundEventID AS INT64    NO-UNDO INITIAL ?.

    FOR EACH ttRequestData
        WHERE ttRequestData.requestStatus   EQ cRequestStatusPrepared
          AND ttRequestData.reTrigger       EQ iplReTrigger:

        lcRequestData = ttRequestData.requestData.

        /* Make the API call - We will have to exclude FTP and SAVE request type as those are handled in its customized handler */
        IF ttRequestData.requestType EQ cRequestTypeAPI THEN
            RUN api/CallOutBoundAPI.p (
                INPUT  ttRequestData.apiOutboundID,
                INPUT  lcRequestData,
                INPUT  ttRequestData.parentProgram,
                OUTPUT lcResponseData,
                OUTPUT ttRequestData.success,
                OUTPUT ttRequestData.requestMessage
                ) NO-ERROR.
        ELSE
            ASSIGN
                lcResponseData               = "Success"
                ttRequestData.success        = TRUE
                ttRequestData.requestMessage = "Success"
                .
                
        ttRequestData.requestStatus = cRequestStatusSuccess.

        IF NOT ttRequestData.success THEN
            ttRequestData.requestStatus = cRequestStatusFailed.

        IF ERROR-STATUS:ERROR THEN
            ASSIGN
                ttRequestData.requestMessage = ERROR-STATUS:GET-MESSAGE(1)
                ttRequestData.requestStatus  = cRequestStatusError
                .

        ttRequestData.responseData = lcResponseData.

        IF ttRequestData.reTrigger THEN
            iOutboundEventID = ttRequestData.apiOutboundEventID.

        RUN api/CreateAPIOutboundEvent.p (
            INPUT  ttRequestData.reTrigger,     /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record */
            INPUT  iOutboundEventID,            /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
            INPUT  ttRequestData.company,
            INPUT  ttRequestData.location,
            INPUT  ttRequestData.apiID,
            INPUT  ttRequestData.clientID,
            INPUT  ttRequestData.triggerID,
            INPUT  ttRequestData.primaryID,
            INPUT  ttRequestData.eventDesc,
            INPUT  lcRequestData,
            INPUT  lcResponseData,
            INPUT  ttRequestData.parentProgram,
            INPUT  ttRequestData.success,
            INPUT  ttRequestData.requestMessage,
            INPUT  NOW,
            OUTPUT ttRequestData.apiOutboundEventID
            ) NO-ERROR.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pPrepareAndExecute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplReTrigger        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    RUN pInitializeAndPrepareRequest (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcClientID,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  iplReTrigger, /* Re-Trigger request */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF oplSuccess THEN
        RUN pExecute (
            INPUT  iplReTrigger,      /* Re-Trigger request */
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        RETURN.
    END.
END PROCEDURE.

PROCEDURE pValidateLocation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validate location if API enabled
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    FIND FIRST loc NO-LOCK
         WHERE loc.company EQ ipcCompany
           AND loc.loc     EQ ipcLocation
         NO-ERROR.
    IF NOT AVAILABLE loc THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid location '"
                       + ipcLocation + "'"
            .
        RETURN.        
    END.
    
    /* Skip this validation where APIs added to cLocValidationExceptions list */
    IF NOT loc.isAPIEnabled AND LOOKUP(ipcAPIID, cLocValidationExceptions) EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "API Calls are not enabled for location '"
                       + ipcLocation + "'"
            .
        RETURN.
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pCreateTTRequestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAPIID                AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClientID             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTriggerID            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrimaryID            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEventDescription     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableList            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcROWIDList            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRequestType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRequestDataType      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplReTrigger            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplcRequestData         AS LONGCHAR  NO-UNDO.
    DEFINE INPUT PARAMETER iplcResponseData        AS LONGCHAR  NO-UNDO.
    DEFINE INPUT PARAMETER ipiAPIOutboundID        AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER ipiAPIOutboundTriggerID AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER ipiAPIOutboundEventID   AS INT64     NO-UNDO.

    DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.

    cParentProgram = IF NUM-ENTRIES(PROGRAM-NAME(2)," ") EQ 2 THEN
                         ENTRY(2, PROGRAM-NAME(2), " ")
                     ELSE
                         PROGRAM-NAME(2).

    CREATE ttRequestData.
    ASSIGN
        ttRequestData.company              = ipcCompany
        ttRequestData.location             = ipcLocation
        ttRequestData.apiID                = ipcAPIID
        ttRequestData.clientID             = ipcClientID
        ttRequestData.triggerID            = ipcTriggerID
        ttRequestData.primaryID            = ipcPrimaryID
        ttRequestData.eventDesc            = ipcEventDescription
        ttRequestData.tableList            = ipcTableList
        ttRequestData.rowidList            = ipcROWIDList
        ttRequestData.reTrigger            = iplReTrigger
        ttRequestData.requestType          = ipcRequestType
        ttRequestData.requestDataType      = ipcRequestDataType
        ttRequestData.apiOutboundID        = ipiAPIOutboundID
        ttRequestData.apiOutboundTriggerID = ipiAPIOutboundTriggerID
        ttRequestData.apiOutboundEventID   = ipiAPIOutboundEventID
        ttRequestData.requestData          = iplcRequestData
        ttRequestData.responseData         = iplcResponseData
        ttRequestData.parentProgram        = cParentProgram
        ttRequestData.requestStatus        = cRequestStatusInitialized
        .
END.

