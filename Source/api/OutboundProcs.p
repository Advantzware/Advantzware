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
USING System.SharedConfig.
  
{api/ttArgs.i}
{api/ttScopes.i}
{api/CommonAPIProcs.i}

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
DEFINE VARIABLE cLocValidationExceptions  AS CHARACTER NO-UNDO INITIAL "SendAdvancedShipNotice,SendFinishedGood,CalculateTax". /* Should be comma (,) separated. loc.isAPIEnabled will not be validated for APIs in the list */
DEFINE VARIABLE cScopeTypeList            AS CHARACTER NO-UNDO INITIAL "_ANY_,Customer,Vendor,ShipTo".
DEFINE VARIABLE cScopeTypeCustomer        AS CHARACTER NO-UNDO INITIAL "Customer".
DEFINE VARIABLE cScopeTypeVendor          AS CHARACTER NO-UNDO INITIAL "Vendor".
DEFINE VARIABLE cScopeTypeShipTo          AS CHARACTER NO-UNDO INITIAL "ShipTo".
DEFINE VARIABLE cAPIClientXrefAny         AS CHARACTER NO-UNDO INITIAL "_ANY_".
DEFINE VARIABLE scInstance                AS CLASS System.SharedConfig NO-UNDO.

/* **********************  Internal Procedures  *********************** */


PROCEDURE Outbound_CopyAPIDependencies:
/*------------------------------------------------------------------------------
 Purpose: Copies all API dependent table records and links to target API Outbound
          table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiSourceAPIOutboundID AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTargetAPIOutboundID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-Source-APIOutbound        FOR APIOutbound.
    DEFINE BUFFER bf-Target-APIOutbound        FOR APIOutbound.
    DEFINE BUFFER bf-Source-APIOutboundDetail  FOR APIOutboundDetail.
    DEFINE BUFFER bf-Target-APIOutboundDetail  FOR APIOutboundDetail.
    DEFINE BUFFER bf-Source-APIOutboundTrigger FOR APIOutboundTrigger.
    DEFINE BUFFER bf-Target-APIOutboundTrigger FOR APIOutboundTrigger.
      
    FIND FIRST bf-Source-APIOutbound NO-LOCK
         WHERE bf-Source-APIOutbound.apiOutboundID EQ ipiSourceAPIOutboundID
           NO-ERROR.
    IF NOT AVAILABLE bf-Source-APIOutbound THEN
        RETURN.
        
    FIND FIRST bf-Target-APIOutbound NO-LOCK
         WHERE bf-Target-APIOutbound.apiOutboundID EQ ipiTargetAPIOutboundID
           NO-ERROR.
    IF NOT AVAILABLE bf-Target-APIOutbound THEN
        RETURN.

    FOR EACH bf-Source-APIOutboundDetail NO-LOCK
        WHERE bf-Source-APIOutboundDetail.apiOutboundID EQ bf-Source-APIOutbound.apiOutboundID:
        FIND FIRST bf-Target-APIOutboundDetail NO-LOCK
             WHERE bf-Target-APIOutboundDetail.company  EQ bf-Target-APIOutbound.company
               AND bf-Target-APIOutboundDetail.apiID    EQ bf-Target-APIOutbound.apiID
               AND bf-Target-APIOutboundDetail.clientID EQ bf-Target-APIOutbound.clientID
               AND bf-Target-APIOutboundDetail.detailID EQ bf-Source-APIOutboundDetail.detailID
             NO-ERROR.
        IF NOT AVAILABLE bf-Target-APIOutboundDetail THEN DO:
            CREATE bf-Target-APIOutboundDetail.
            BUFFER-COPY bf-Source-APIOutboundDetail 
                EXCEPT bf-Source-APIOutboundDetail.apiID 
                       bf-Source-APIOutboundDetail.clientID 
                       bf-Source-APIOutboundDetail.apiOutboundID 
                       bf-Source-APIOutboundDetail.apiOutboundDetailID
                       bf-Source-APIOutboundDetail.rec_key
                TO bf-Target-APIOutboundDetail.
            ASSIGN
                bf-Target-APIOutboundDetail.apiID         = bf-Target-APIOutbound.apiID
                bf-Target-APIOutboundDetail.clientID      = bf-Target-APIOutbound.clientID
                bf-Target-APIOutboundDetail.apiOutboundID = bf-Target-APIOutbound.apiOutboundID
                .
        END.
    END.

    FOR EACH bf-Source-APIOutboundTrigger NO-LOCK
        WHERE bf-Source-APIOutboundTrigger.apiOutboundID EQ bf-Source-APIOutbound.apiOutboundID:
        FIND FIRST bf-Target-APIOutboundTrigger NO-LOCK
             WHERE bf-Target-APIOutboundTrigger.company   EQ bf-Target-APIOutbound.company
               AND bf-Target-APIOutboundTrigger.apiID     EQ bf-Target-APIOutbound.apiID
               AND bf-Target-APIOutboundTrigger.clientID  EQ bf-Target-APIOutbound.clientID
               AND bf-Target-APIOutboundTrigger.triggerID EQ bf-Source-APIOutboundTrigger.triggerID
             NO-ERROR.        
        IF NOT AVAILABLE bf-Target-APIOutboundTrigger THEN DO:
            CREATE bf-Target-APIOutboundTrigger.
            BUFFER-COPY bf-Source-APIOutboundTrigger 
                EXCEPT bf-Source-APIOutboundTrigger.apiID 
                       bf-Source-APIOutboundTrigger.clientID 
                       bf-Source-APIOutboundTrigger.apiOutboundID 
                       bf-Source-APIOutboundTrigger.apiOutboundTriggerID
                       bf-Source-APIOutboundTrigger.rec_key
                       bf-Source-APIOutboundTrigger.createBy
                       bf-Source-APIOutboundTrigger.createTime
                TO bf-Target-APIOutboundTrigger.
            ASSIGN
                bf-Target-APIOutboundTrigger.apiID         = bf-Target-APIOutbound.apiID
                bf-Target-APIOutboundTrigger.clientID      = bf-Target-APIOutbound.clientID
                bf-Target-APIOutboundTrigger.apiOutboundID = bf-Target-APIOutbound.apiOutboundID
                .
        END.
    END.
END PROCEDURE.

PROCEDURE Outbound_CreateAPIClient:
/*------------------------------------------------------------------------------
 Purpose: Creates an apiClient record for given inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-apiClient FOR apiClient.
    
    FIND FIRST bf-apiClient NO-LOCK
         WHERE bf-apiClient.company  EQ ipcCompany
           AND bf-apiClient.clientID EQ ipcClientID
         NO-ERROR.
    IF NOT AVAILABLE bf-apiClient THEN DO:
        CREATE bf-apiClient.
        ASSIGN
            bf-apiClient.company  = ipcCompany
            bf-apiClient.clientID = ipcClientID
            .
    END.
END PROCEDURE.

PROCEDURE Outbound_GetAPIClientTransCount:
/*------------------------------------------------------------------------------
 Purpose: Returns the transaction count value of the apiClient record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTransCount AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-apiClient FOR apiClient.

    FIND FIRST bf-apiClient EXCLUSIVE-LOCK
         WHERE bf-apiClient.company  EQ ipcCompany
           AND bf-apiClient.clientID EQ ipcClientID
         NO-ERROR.
    IF AVAILABLE bf-apiClient THEN
         opiTransCount = bf-apiClient.transactionCounter.
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
    
    DEFINE VARIABLE lAPIOutboundTestMode AS LOGICAL NO-UNDO.
    
    ASSIGN 
        scInstance           = SharedConfig:instance
        lAPIOutboundTestMode = LOGICAL(scInstance:GetValue("APIOutboundTestMode")) NO-ERROR
        .

    FIND FIRST APIOutbound NO-LOCK
          WHERE APIOutbound.company  EQ ipcCompany
            AND APIOutbound.apiID    EQ ipcAPIID
            AND APIOutbound.clientID EQ ipcClientID
          NO-ERROR.
    IF AVAILABLE APIOutbound AND
        (lAPIOutboundTestMode OR NOT APIOutbound.Inactive) THEN
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

PROCEDURE Outbound_GetAPITransCount:
/*------------------------------------------------------------------------------
 Purpose: Returns the transaction count value of the APIOutbound record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTransCount AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
        
    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.company  EQ ipcCompany
           AND bf-APIOutbound.apiID    EQ ipcAPIID
           AND bf-APIOutbound.clientID EQ ipcClientID 
         NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN
        opiTransCount = bf-APIOutbound.transactionCounter.   
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

    DEFINE VARIABLE lAPIOutboundTestMode AS LOGICAL NO-UNDO.
    
    ASSIGN 
        scInstance           = SharedConfig:instance
        lAPIOutboundTestMode = LOGICAL(scInstance:GetValue("APIOutboundTestMode")) NO-ERROR
        .

    FIND FIRST APIOutboundTrigger NO-LOCK
         WHERE APIOutboundTrigger.company   EQ ipcCompany
           AND APIOutboundTrigger.apiID     EQ ipcAPIID
           AND APIOutboundTrigger.clientID  EQ ipcClientID
           AND APIOutboundTrigger.triggerID EQ ipcTriggerID
         NO-ERROR.
    IF AVAILABLE APIOutboundTrigger AND
        (lAPIOutboundTestMode OR NOT APIOutboundTrigger.Inactive) THEN
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

PROCEDURE Outbound_GetAPITriggersList:
/*------------------------------------------------------------------------------
 Purpose: Procedure to return the list of available trigger for an api and client
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTriggerList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound        FOR APIOutbound.
    DEFINE BUFFER bf-APIOutboundTrigger FOR APIOutboundTrigger.
    
    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.company  EQ ipcCompany
           AND bf-APIOutbound.apiID    EQ ipcAPIID
           AND bf-APIOutbound.clientID EQ ipcClientID
           NO-ERROR.
    IF NOT AVAILABLE bf-APIOutbound THEN
        RETURN.

    FOR EACH bf-APIOutboundTrigger NO-LOCK
        WHERE bf-APIOutboundTrigger.apiOutboundID EQ bf-APIOutbound.apiOutboundID:
        opcTriggerList = opcTriggerList + "," + bf-APIOutboundTrigger.triggerID.
    END.
    
    opcTriggerList = TRIM(opcTriggerList,",").
    
END PROCEDURE.

PROCEDURE Outbound_IncrementAPITransactionCounter:
/*------------------------------------------------------------------------------
 Purpose: Increment the transaction counters of both APIOutbound and apiClient
          records for the given input
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAPIOutboundID AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    DEFINE BUFFER bf-apiClient   FOR apiClient.
        
    FIND FIRST bf-APIOutbound EXCLUSIVE-LOCK
         WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID 
         NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN DO:
        bf-APIOutbound.transactionCounter = bf-APIOutbound.transactionCounter + 1.
    
        FIND FIRST bf-apiClient EXCLUSIVE-LOCK
             WHERE bf-apiClient.company  EQ bf-APIOutbound.company
               AND bf-apiClient.clientID EQ bf-APIOutbound.clientID
             NO-ERROR.
        IF AVAILABLE bf-apiClient THEN
             bf-apiClient.transactionCounter = bf-apiClient.transactionCounter + 1.
    END.
END PROCEDURE.

PROCEDURE Outbound_IsApiScopeActive:
/*------------------------------------------------------------------------------
 Purpose: Check for Active Scope for a given API
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID       AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcScopeID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID   AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplScopeActive AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound        FOR APIOutbound.
    DEFINE BUFFER bf-APIOutboundTrigger FOR APIOutboundTrigger.
    DEFINE BUFFER bf-apiClient          FOR apiClient.
    
    /* The following code will be executed if it is a fresh (NOT re-triggered) API call */
    FOR EACH bf-APIOutbound NO-LOCK
       WHERE bf-APIOutbound.company EQ ipcCompany
         AND bf-APIOutbound.apiID   EQ ipcAPIID:
        IF bf-APIOutbound.Inactive THEN
            NEXT.

        FIND FIRST bf-APIOutboundTrigger NO-LOCK
             WHERE bf-APIOutboundTrigger.apiOutboundID EQ bf-APIOutbound.apiOutboundID
               AND bf-APIOutboundTrigger.triggerID     EQ ipcTriggerID
               AND bf-APIOutboundtrigger.Inactive      EQ FALSE
             NO-ERROR.
        IF NOT AVAILABLE bf-APIOutboundTrigger THEN
            NEXT.
        
        FIND FIRST bf-apiClient NO-LOCK
             WHERE bf-apiClient.company  EQ bf-APIOutbound.company
               AND bf-apiClient.clientID EQ bf-APIOutbound.clientID
             NO-ERROR.
        IF AVAILABLE bf-apiClient THEN DO:
            RUN pIsScopeActive (
                INPUT  bf-apiClient.company,
                INPUT  ipcLocation,
                INPUT  ipcAPIID,
                INPUT  bf-apiClient.clientID,
                INPUT  ipcTriggerID,
                INPUT  ipcScopeID,
                INPUT  ipcScopeType,
                OUTPUT oplScopeActive
                ).
            IF oplScopeActive THEN 
                RETURN.
        END.
    END.    

END PROCEDURE.

PROCEDURE Outbound_PrepareRequestForScope:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to prepare request data for a given scope id 
              and scope Type
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

    RUN pPrepareAndExecuteForScope (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcScopeID,
        INPUT  ipcScopeType,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  FALSE,  /* Execute. Send FALSE for just preparing the request data */
        OUTPUT oplSuccess,
        OUTPUT opcMessage        
        ).
END PROCEDURE.

PROCEDURE Outbound_PrepareAndExecuteForScope:
    /*------------------------------------------------------------------------------
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

    RUN pPrepareAndExecuteForScope (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        INPUT  ipcScopeID,
        INPUT  ipcScopeType,
        INPUT  ipcTriggerID,
        INPUT  ipcTableList,
        INPUT  ipcROWIDList,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  TRUE,  /* Execute. Send FALSE for just preparing the request data */
        OUTPUT oplSuccess,
        OUTPUT opcMessage        
        ).
END PROCEDURE.

PROCEDURE Outbound_ValidateLocation:
/*------------------------------------------------------------------------------
 Purpose: Validates the location 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    RUN pValidateLocation(
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        INPUT  ipcAPIID,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

END PROCEDURE.

PROCEDURE pPrepareAndExecuteForScope PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to prepare request data and call Outbound API
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
    DEFINE INPUT  PARAMETER iplExecute          AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lScopeActive AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound        FOR APIOutbound.
    DEFINE BUFFER bf-APIOutboundTrigger FOR APIOutboundTrigger.
    DEFINE BUFFER bf-apiClient          FOR apiClient.
    
    /* The following code will be executed if it is a fresh (NOT re-triggered) API call */
    FOR EACH bf-APIOutbound NO-LOCK
       WHERE bf-APIOutbound.company EQ ipcCompany
         AND bf-APIOutbound.apiID   EQ ipcAPIID:
        IF bf-APIOutbound.Inactive THEN
            NEXT.

        FIND FIRST bf-APIOutboundTrigger NO-LOCK
             WHERE bf-APIOutboundTrigger.apiOutboundID EQ bf-APIOutbound.apiOutboundID
               AND bf-APIOutboundTrigger.triggerID     EQ ipcTriggerID
               AND bf-APIOutboundtrigger.Inactive      EQ FALSE
             NO-ERROR.
        IF NOT AVAILABLE bf-APIOutboundTrigger THEN
            NEXT.
        
        FIND FIRST bf-apiClient NO-LOCK
             WHERE bf-apiClient.company  EQ bf-APIOutbound.company
               AND bf-apiClient.clientID EQ bf-APIOutbound.clientID
             NO-ERROR.
        IF AVAILABLE bf-apiClient THEN DO:
            RUN pIsScopeActive (
                INPUT  bf-apiClient.company,
                INPUT  ipcLocation,
                INPUT  ipcAPIID,
                INPUT  bf-apiClient.clientID,
                INPUT  ipcTriggerID,
                INPUT  ipcScopeID,
                INPUT  ipcScopeType,
                OUTPUT lScopeActive
                ).

            IF NOT lScopeActive THEN
                NEXT.

            IF AVAILABLE bf-apiClient THEN DO:
                RUN pInitializeAndPrepareRequest (
                    INPUT  ipcCompany,
                    INPUT  ipcLocation,
                    INPUT  ipcAPIID,
                    INPUT  bf-apiClient.clientID,
                    INPUT  ipcTriggerID,
                    INPUT  ipcTableList,
                    INPUT  ipcROWIDList,
                    INPUT  ipcPrimaryID,
                    INPUT  ipcEventDescription,
                    INPUT  FALSE, /* Re-Trigger request */
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ) NO-ERROR.
                
                /* Execute only if iplExecute flag is TRUE */
                IF oplSuccess AND iplExecute THEN
                    RUN pExecute (
                        INPUT  FALSE,      /* Re-Trigger request */
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
            END.             
        END.
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .   
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

PROCEDURE Outbound_UpdateGlobalFieldValues:
/*------------------------------------------------------------------------------
 Purpose: This procedure updates global fields in the request data that are 
          generic to the API 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAPITransactionCounter    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientTransactionCounter AS CHARACTER NO-UNDO.
    
    ASSIGN
        cAPITransactionCounter    = STRING(fGetAPITransactionCounter(ipiAPIOutboundID))
        cClientTransactionCounter = STRING(fGetClientTransactionCounter(ipiAPIOutboundID))
        .
        
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "APITransCounter", cAPITransactionCounter).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ClientTransCounter", cClientTransactionCounter).

END PROCEDURE.

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

PROCEDURE pGetApiClientXrefStatus PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Return apiClientXref records inactive flag status
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplScopeActive AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-apiClientXref FOR apiClientXref.
    
    /* Verify if a record is available for just the given inputs
       and return the inactive flag */
    FIND FIRST bf-apiClientXref NO-LOCK
         WHERE bf-apiClientXref.company   EQ ipcCompany
           AND bf-apiClientXref.apiID     EQ ipcAPIID
           AND bf-apiClientXref.clientID  EQ ipcClientID
           AND bf-apiClientXref.scopeID   EQ ipcScopeID
           AND bf-apiClientXref.scopeType EQ ipcScopeType
           AND bf-apiClientXref.triggerID EQ ipcTriggerID
         NO-ERROR.
    IF AVAILABLE bf-apiClientXref THEN DO:
        ASSIGN
            oplRecFound    = TRUE
            oplScopeActive = NOT bf-apiClientXref.inactive
            .
        RETURN.
    END.

END PROCEDURE.

PROCEDURE Outbound_GetAPIsForScopeID:
/*------------------------------------------------------------------------------
 Purpose: Returns the temp-table with list of api's active for the given scope
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttScopes.
    
    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    DEFINE BUFFER bf-apiClientXref FOR apiClientXref.
    
    FOR EACH bf-APIOutbound NO-LOCK
        WHERE bf-APIOutbound.company  EQ ipcCompany
          AND bf-APIOutbound.inactive EQ FALSE:
        FOR EACH bf-apiClientXref NO-LOCK
            WHERE bf-apiClientXref.company  EQ bf-APIOutbound.company
              AND bf-apiClientXref.apiID    EQ bf-APIOutbound.apiID
              AND bf-apiClientXref.clientID EQ bf-APIOutbound.clientID:
            IF ipcScopeType EQ cScopeTypeVendor THEN DO: 
                IF bf-apiClientXref.scopeType NE cScopeTypeVendor AND
                   bf-apiClientXref.scopeType NE cAPIClientXrefAny THEN
                NEXT.
                
                IF bf-apiClientXref.scopeID NE cAPIClientXrefAny AND 
                   bf-apiClientXref.scopeID NE ipcScopeID THEN
                NEXT.
            END.
            
            IF ipcScopeType EQ cScopeTypeCustomer THEN DO: 
                IF bf-apiClientXref.scopeType NE cScopeTypeCustomer AND
                   bf-apiClientXref.scopeType NE cScopeTypeShipTo AND
                   bf-apiClientXref.scopeType NE cAPIClientXrefAny THEN
                    NEXT.
                
                IF bf-apiClientXref.scopeID NE cAPIClientXrefAny AND 
                   NOT bf-apiClientXref.scopeID BEGINS ipcScopeID THEN
                NEXT.                
            END.
            
            CREATE ttScopes.
            ASSIGN
                ttScopes.company         = bf-apiClientXref.company
                ttScopes.apiID           = bf-apiClientXref.apiID
                ttScopes.clientID        = bf-apiClientXref.clientID
                ttScopes.scopeID         = IF bf-apiClientXref.scopeID EQ cAPIClientXrefAny THEN
                                               "ANY"
                                           ELSE
                                               bf-apiClientXref.scopeID
                ttScopes.scopeType       = IF bf-apiClientXref.scopeType EQ cAPIClientXrefAny THEN
                                               "ANY"
                                           ELSE
                                               bf-apiClientXref.scopeType
                ttScopes.triggerID       = IF bf-apiClientXref.triggerID EQ cAPIClientXrefAny THEN
                                               "ANY"
                                           ELSE
                                               bf-apiClientXref.triggerID
                ttScopes.inactive        = bf-apiClientXref.inactive
                ttScopes.riApiClientXref = ROWID(bf-apiClientXref)
                .

            IF bf-apiClientXref.scopeType EQ cScopeTypeCustomer THEN
                ttScopes.customerID = IF bf-apiClientXref.scopeID EQ cAPIClientXrefAny THEN
                                          "ANY"
                                      ELSE
                                          bf-apiClientXref.scopeID.

            IF bf-apiClientXref.scopeType EQ cScopeTypeShipTo THEN
                ASSIGN
                    ttScopes.customerID = IF bf-apiClientXref.scopeID EQ cAPIClientXrefAny THEN
                                              "ANY"
                                          ELSE
                                              ENTRY(1, bf-apiClientXref.scopeID, "|")
                    ttScopes.shipToiD   = IF bf-apiClientXref.scopeID EQ cAPIClientXrefAny THEN
                                              "ANY"
                                          ELSE
                                              ENTRY(2, bf-apiClientXref.scopeID, "|")
                    NO-ERROR.

            IF bf-apiClientXref.scopeType EQ cScopeTypeVendor THEN
                ttScopes.vendorID = IF bf-apiClientXref.scopeID EQ cAPIClientXrefAny THEN
                                        "ANY"
                                    ELSE
                                        bf-apiClientXref.scopeID.
        END.
    END.
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

PROCEDURE pIsScopeActive PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: For the given inputs, returns if a logical value to allow or skip
          api call 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplScopeActive AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cScopeID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScopeType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iRule      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumRules  AS INTEGER   NO-UNDO INITIAL 8.
    
    DO iRule = 1 TO iNumRules:
        /* All the rules below are prioritized to find the record with 
           most matching input values. Scope ID is given the top priority
           and, then trigger and scopeType */
        /* Verify if a record is available and return inactive flag status,
           for just the given inputs */
        IF iRule EQ 1 THEN
            ASSIGN
                cScopeID   = ipcScopeID
                cScopeType = ipcScopeType
                cTriggerID = ipcTriggerID
                .
        /* Verify if a record is available and return inactive flag status,
           for given scopeID, scopeType and for all ("_ALL_") the triggers */
        ELSE IF iRule EQ 2 THEN
            ASSIGN
                cScopeID   = ipcScopeID
                cScopeType = ipcScopeType
                cTriggerID = cAPIClientXrefAny
                .
        /* Verify if a record is available and return inactive flag status,
           for given scopeID, triggerID, for all ("_ALL_") the scopeTypes */
        ELSE IF iRule EQ 3 THEN
            ASSIGN
                cScopeID   = ipcScopeID
                cScopeType = cAPIClientXrefAny
                cTriggerID = ipcTriggerID
                .
        /* Verify if a record is available  and return inactive flag status,
           for given scopeID, for all ("_ALL_")  the scopeTypes, and for all
           the triggers */
        ELSE IF iRule EQ 4 THEN
            ASSIGN
                cScopeID   = ipcScopeID
                cScopeType = cAPIClientXrefAny
                cTriggerID = cAPIClientXrefAny
                .
        /* Verify if a record is available  and return inactive flag status,
           for all scopeIDs, input scopeType and input trigger ID*/
        ELSE IF iRule EQ 5 THEN
            ASSIGN
                cScopeID   = cAPIClientXrefAny
                cScopeType = ipcScopeType
                cTriggerID = ipcTriggerID
                .
        /* Verify if a record is available and return inactive flag status
           for all scopeIDs, and input scopeType and for all triggers*/
        ELSE IF iRule EQ 6 THEN
            ASSIGN
                cScopeID   = cAPIClientXrefAny
                cScopeType = ipcScopeType
                cTriggerID = cAPIClientXrefAny
                .
        /* Verify if a record is available and return inactive flag status
           for all scopeIDs, for all scopeType and input triggerID*/
        ELSE IF iRule EQ 7 THEN
            ASSIGN
                cScopeID   = cAPIClientXrefAny
                cScopeType = cAPIClientXrefAny
                cTriggerID = ipcTriggerID
                .
        /* Verify if a record is available and return inactive flag status
           for all scopeIDs, for all scopeType and for all triggers. If a 
           record is not found at this point no api call should be made for
           the input scope */
        ELSE IF iRule EQ 8 THEN
            ASSIGN
                cScopeID   = cAPIClientXrefAny
                cScopeType = cAPIClientXrefAny
                cTriggerID = cAPIClientXrefAny
                .

        RUN pGetApiClientXrefStatus (
            INPUT  ipcCompany,
            INPUT  ipcLocation,
            INPUT  ipcAPIID,
            INPUT  ipcClientID,
            INPUT  cTriggerID,
            INPUT  cScopeID,
            INPUT  cScopeType,
            OUTPUT lRecFound,
            OUTPUT oplScopeActive
            ).

        IF lRecFound THEN
            RETURN.
    END.  
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

