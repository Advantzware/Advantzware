/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 * The Developer of the Original Code is Progress Software.           *
 * The Original code is part of Progress Dynamics code provided as    *
 * open source code. The code has since been evolved by CW.           *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *  KIND, either express or implied.                                  *
 *                                                                    *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------------
    File   :   adm2/serviceadapter.p
    Purpose:   Data request service adapter interface
    Notes    - This sample service requests data in multiple entities by 
               ignoring requests for those who needs a join and relies on an 
               override in dataview to ensure that the ones that were 
               ignored are requested again separately with the necessary join
               added after the parent has been requesred.           
             - Multiple entities that are not linked are requested one by one 
               in the loop  
             - The adapter is responsible of starting and closing the service                  
------------------------------------------------------------------------------*/
/* ************************  Definitions  ********************** */
DEFINE VARIABLE ghService AS HANDLE NO-UNDO.

/* ************************  Function Prototypes ********************** */
FUNCTION getObjectType RETURNS CHARACTER 
  (  )  FORWARD .

FUNCTION getService RETURNS HANDLE PRIVATE 
  (  )  FORWARD .

FUNCTION destroyService RETURNS LOGICAL PRIVATE 
  (  )  FORWARD .

/* ***********************  Procedures ********************************* */
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:  Destroy     
------------------------------------------------------------------------------*/
  destroyService(). 
  DELETE PROCEDURE THIS-PROCEDURE .
END PROCEDURE .

PROCEDURE retrieveData :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve data or definitions for one or many Business Entities 
            The caller specifies the number of entities that need to be handled 
            by the request by defining a finite number of extents for the array 
            parameters.   
Parameters:  
     pcEntity       - Business Entity 
     pcTables       - Tables for which to retrieve data or definitions
     pcQueries      - List of queries that corresponds to the requested tables
     pcJoins        - List of join fields for the tables
     pcPositions    - How to find a single record in the tables  
     pcRequest      - The actual request for the tables   
     pcBatchContext - Context info for a batch request 
     plFillBatch    - Read a full batch size at end of result set
     pcNumRecords   - Batch size for the tables               
     phDataSet      - Prodataset handle 
     pcContext      - Other context    
     pcPrevContext  - Context for prev record that was NOT returned from tables
     pcNextContext  - Context for next record that was NOT returned from tables
                                        
  Notes: The comments has been minimized to focus on code.
         (see original stub in adm2) 
------------------------------------------------------------------------------*/

  DEFINE INPUT         PARAMETER pcEntity       AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcTables       AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcQueries      AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcJoins        AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcPositions    AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcRequests     AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT         PARAMETER pcBatchContext AS CHARACTER NO-UNDO.
  DEFINE INPUT         PARAMETER plFillBatch    AS LOGICAL   NO-UNDO.
  DEFINE INPUT-OUTPUT  PARAMETER pcNumRecords   AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT-OUTPUT  PARAMETER phDataset      AS HANDLE    NO-UNDO EXTENT. 
  DEFINE INPUT-OUTPUT  PARAMETER pcContext      AS CHARACTER NO-UNDO EXTENT.
  DEFINE OUTPUT        PARAMETER pcPrevContext  AS CHARACTER NO-UNDO EXTENT.
  DEFINE OUTPUT        PARAMETER pcNextContext  AS CHARACTER NO-UNDO EXTENT.

  EXTENT(pcPrevContext) = EXTENT(pcEntity).
  EXTENT(pcNextContext) = EXTENT(pcEntity).

  DEFINE VARIABLE hDataset AS HANDLE EXTENT 10 NO-UNDO.  
  DEFINE VARIABLE iEntity  AS INTEGER NO-UNDO.

  DO iEntity = 1 TO EXTENT(pcEntity):
      hDataset[iEntity]  = phDataset[iEntity].
  END.

  RUN support/proSIretrieve.p
          (pcEntity,
           pcTables,
           pcQueries,
           pcJoins,
           pcPositions,
           pcRequests,
           pcBatchContext,
           plFillBatch,
           0, /* value for StopAfter */
           INPUT-OUTPUT pcNumRecords,
           OUTPUT DATASET-HANDLE hDataset[1] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[2] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[3] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[4] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[5] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[6] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[7] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[8] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[9] APPEND, 
           OUTPUT DATASET-HANDLE hDataset[10] APPEND, 
           INPUT-OUTPUT pcContext,
           OUTPUT pcPrevContext,
           OUTPUT pcNextContext).
  
  DO iEntity = 1 TO EXTENT(pcEntity):
      phDataset[iEntity] = hDataset[iEntity].
  END.     

END PROCEDURE /* retrieveData */. 

PROCEDURE submitData :
/*------------------------------------------------------------------------------
  Purpose:     Submit of changes
  parameters:  
     pcEntity       - Business Entity 
     phDataSet      - Prodataset handle with changes only 
     pcContext      - Other context    
     Notes:        
------------------------------------------------------------------------------*/
  /* Business Entity reference */
  DEFINE INPUT         PARAMETER pcEntity     AS CHARACTER NO-UNDO EXTENT.
  DEFINE INPUT-OUTPUT  PARAMETER phDataset    AS HANDLE    NO-UNDO EXTENT.
  DEFINE INPUT-OUTPUT  PARAMETER pcContext    AS CHARACTER NO-UNDO EXTENT.

  /* we don't loop as we know the adm2 does not submit more than one */  
  RUN support/proSIsubmit.p (pcEntity[1],
                             INPUT-OUTPUT DATASET-HANDLE phDataset[1]) NO-ERROR . 
  
  IF ERROR-STATUS:ERROR THEN  
    RETURN ERROR IF RETURN-VALUE > '' THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1).  
  
  /* this service is single transaction, and does not set error flags  */  
  IF phdataset[1]:ERROR THEN
    RUN markRows (phDataset[1], 'REJECTED':U).
  
  RETURN.                              
END PROCEDURE .

/* receive message that adm2 is done (the data set is already deleted) */
PROCEDURE stopEntity:
  DEFINE INPUT PARAMETER phDataSet AS HANDLE.
  /* Relying in naming convention.. not so nice..  */
  
  DEFINE VARIABLE hService AS HANDLE NO-UNDO.
  ASSIGN hService = getService() .
  
  RUN stopEntity IN hService (substr(phdataset:name,2)).
END.

/* ************************  Function Implementations ***************** */
FUNCTION getObjectType RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose: To be used by designtime super destroy  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN "Service":U. 
END FUNCTION.

/* get service starts on first call  */
FUNCTION getService RETURNS HANDLE PRIVATE 
  (  ):
  IF NOT valid-handle(ghService) THEN 
  DO ON STOP UNDO, LEAVE:
    RUN support/service.p PERSISTENT SET ghService.  
  END.
  RETURN ghService. 
END.

FUNCTION destroyService RETURNS LOGICAL PRIVATE 
  (  ):
  DEFINE VARIABLE h AS HANDLE    NO-UNDO.  
  IF valid-handle(ghService) THEN 
  DO: 
    RUN destroyObject IN ghService.
    RETURN TRUE.
  END.   
  RETURN FALSE. 
END.

PROCEDURE markRows:
  /* marks all rows as REJECTED or ERROR 
     Use when the service does not do this with large transactions */ 
  DEFINE INPUT PARAMETER phDataSet AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pcFlag    AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iBuf   AS INTEGER NO-UNDO.
  DEFINE VARIABLE hBuf   AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBefore AS HANDLE  NO-UNDO.
  
  DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
  
  CREATE QUERY hQuery.
  DO ibuf = 1 TO phDataSet:NUM-BUFFERS:
    hBuf = phdataset:GET-BUFFER-HANDLE(iBuf).
    hBefore = hBuf:BEFORE-BUFFER.
    hQuery:SET-BUFFERS(hBefore).
    hQuery:QUERY-PREPARE("FOR EACH ":U + hBefore:name).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    DO WHILE NOT hQuery:QUERY-OFF-END :
      IF hBefore:ERROR = FALSE AND hBefore:REJECTED = FALSE THEN
      DO:
        IF pcFlag BEGINS 'reject':U THEN 
          hBefore:REJECTED = TRUE.
        ELSE
          hBefore:ERROR = TRUE. 
      END.     
       hQuery:GET-NEXT().
    END. /* do while not off end */ 
  END. /* ibuf loop */
  DELETE OBJECT hQuery.
END. /* procedure markRows */
  
  
