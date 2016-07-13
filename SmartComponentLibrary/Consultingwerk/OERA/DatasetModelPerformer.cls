/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : DatasetModelPerformer
    Purpose     : Performs the backend requests of the Dataset Model class 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sat Dec 21 23:41:54 CET 2013
    Notes       : This class has been introduced so that the TableModel
                  instances to not need to keep a reference to the 
                  Dataset Model and the Dataset Model will be cleaned up
                  by the garbage collector when no longer needed
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .  
USING Consultingwerk.Assertion.*       FROM PROPATH .  
USING Consultingwerk.OERA.*            FROM PROPATH .  
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH.
USING Consultingwerk.Util.*            FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.DatasetModelPerformer: 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this DatasetModel 
        Notes:   Only relevant when running on the client side (no databases connected)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the TempTable this DatasetModelPerformer is batching
                 on
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BatchingOnTable AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the BatchSize used when reading data from the Business 
                 Entity. 
        Notes:   Set to 0 to fetch all data and disable Batching
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER INITIAL 100 NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the primary Dataset owned by the DatasetController
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.  
    PRIVATE SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Name of the BusinessEntity.
        Notes:   This Property needs to be set by actual Dataset model implementation
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET.    
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the FetchDataRequest instance used to fetch the previous 
                 batch
        Notes:   Only valid when batching
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY FetchDataRequest AS FetchDataRequest NO-UNDO 
    GET.
    SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Returns is a next batch of records is available 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NextBatchAvailable AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the STOP-AFTER value used when reading data from the 
                 Business Entity. 
        Notes:   Set to 0 to disable STOP-AFTER
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY StopAfter AS INTEGER INITIAL 0 NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the DatasetModel is communicating with a dedicated
                 Business Entity instance
        Notes:   Required, when using the DatasetModel from within the actual Business 
                 Entity itself. As otherwise the read operations done through the 
                 DatasetModel instance to the Business Entity would clear the dataset
                 within the Business Entity. Only applicable, when UseInterface = 
                 ServiceInterface
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UseDedicatedBusinessEntity AS LOGICAL NO-UNDO 
    GET.
    SET (arg AS LOGICAL):
        IF THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceInterface THEN .

        ELSE IF arg <> FALSE THEN 
            UNDO, THROW NEW AppError ("Unable to use dedicated business entity when not using the Service Interface."{&TRAN}, 0) .
            
       ASSIGN THIS-OBJECT:UseDedicatedBusinessEntity = arg . 
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Interface this DatasetModel uses for Business 
                 Entity Requests
        Notes:   Default depends on NUM-DBS > 0, then ServiceInterface else ServiceAdapter
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UseInterface AS UseInterfaceEnum NO-UNDO 
    GET.
    SET (arg AS UseInterfaceEnum):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i arg """UseInterface"":U"} . 
        
        THIS-OBJECT:UseInterface = arg.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatasetModelPerformer class
        Notes:   
        @param phDataset The handle of the ProDataset from the model class
        @param pcEntityName The name of the Business Entity
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetModelPerformer (phDataset AS HANDLE,
	                                          pcEntityName AS CHARACTER):
		
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .
        
        ASSIGN THIS-OBJECT:EntityName    = pcEntityName 
               THIS-OBJECT:DatasetHandle = phDataset .  

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs Fetch Data request either using the Service Adapter or the 
                 Service Interface
        Notes:   
        @param poFetchDataRequest The FetchDataRequest instance with the request data
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformFetchDataRequest (poFetchDataRequest AS FetchDataRequest):
        
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
    
        DEFINE VARIABLE cEntityKey AS CHARACTER NO-UNDO.
    
        IF THIS-OBJECT:UseDedicatedBusinessEntity AND 
           THIS-OBJECT:UseInterface <> UseInterfaceEnum:ServiceInterface THEN 
            UNDO, THROW NEW UnableToUseDedicatedBusinessEntityWithoutServiceInterface ("PerformFetchDataRequest":U,
                                                                                       "Consultingwerk.OERA.DatasetModelPerformer":U) . 
        
        IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
            cEntityKey = SUBSTITUTE ("&1&2&3":U,
                                     THIS-OBJECT:EntityName,
                                     CHR(1),
                                     GUID) .
        ELSE 
            cEntityKey = THIS-OBJECT:EntityName .
        
        ASSIGN hDataset = THIS-OBJECT:DatasetHandle . 
        
        IF THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceAdapter THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:RetrieveData (THIS-OBJECT:AppServerPartition,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    poFetchDataRequest,
                                                                                    OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:FetchData (cEntityKey,
                                                            poFetchDataRequest,
                                                            OUTPUT DATASET-HANDLE hDataset /* No BY-REFERENCE when calling 
                                                                                              into ServiceInterface:FetchData, 
                                                                                              see comments there */) .

        IF ENTRY(1, poFetchDataRequest:NextContext, CHR(1)) > "":U THEN 
            ASSIGN THIS-OBJECT:NextBatchAvailable = TRUE 
                   THIS-OBJECT:FetchDataRequest   = poFetchDataRequest 
                   THIS-OBJECT:BatchingOnTable    = ENTRY (1, poFetchDataRequest:Tables). 
        ELSE 
            ASSIGN THIS-OBJECT:NextBatchAvailable = FALSE 
                   THIS-OBJECT:FetchDataRequest   = ? 
                   THIS-OBJECT:BatchingOnTable    = ? .

        FINALLY:
            IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
                ServiceManager:StopBusinessService (cEntityKey) . 
        END FINALLY.

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Fetches the next batch of data
	    Notes:   Invoked by the GetNext() method of the TableModel class when 
	             no local data is available in the model but additional batches are 
	             available on the backend
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID PerformNextBatchRequest ():
		
		DEFINE VARIABLE hDataset   AS HANDLE NO-UNDO .
        DEFINE VARIABLE hNextBatch AS HANDLE NO-UNDO .

        DEFINE VARIABLE cEntityKey AS CHARACTER NO-UNDO.
    
        IF NOT THIS-OBJECT:NextBatchAvailable OR NOT VALID-OBJECT (THIS-OBJECT:FetchDataRequest) THEN 
            UNDO, THROW NEW TableModelBatchingNotSupportedException ("Unable to perform fetch next batch request without Batch context."{&TRAN}, 0) .
    
        IF THIS-OBJECT:UseDedicatedBusinessEntity AND 
           THIS-OBJECT:UseInterface <> UseInterfaceEnum:ServiceInterface THEN 
            UNDO, THROW NEW UnableToUseDedicatedBusinessEntityWithoutServiceInterface ("PerformFetchDataRequest":U,
                                                                                       "Consultingwerk.OERA.DatasetModelPerformer":U) . 
        
        IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
            cEntityKey = SUBSTITUTE ("&1&2&3":U,
                                     THIS-OBJECT:EntityName,
                                     CHR(1),
                                     GUID) .
        ELSE 
            cEntityKey = THIS-OBJECT:EntityName .

        ASSIGN hDataset                             = THIS-OBJECT:DatasetHandle 
               THIS-OBJECT:FetchDataRequest:Context = THIS-OBJECT:FetchDataRequest:NextContext .

        IF THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceAdapter THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:RetrieveData (THIS-OBJECT:AppServerPartition,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    THIS-OBJECT:FetchDataRequest,
                                                                                    OUTPUT DATASET-HANDLE hNextBatch BY-REFERENCE) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:FetchData (cEntityKey,
                                                            THIS-OBJECT:FetchDataRequest,
                                                            OUTPUT DATASET-HANDLE hNextBatch /* No BY-REFERENCE when calling 
                                                                                                into ServiceInterface:FetchData, 
                                                                                                see comments there */) .

        hDataset:COPY-DATASET (hNextBatch, FALSE, TRUE) .

        IF ENTRY(1, THIS-OBJECT:FetchDataRequest:NextContext, CHR(1)) > "":U THEN 
            ASSIGN THIS-OBJECT:NextBatchAvailable = TRUE 
                   THIS-OBJECT:BatchingOnTable    = ENTRY (1, THIS-OBJECT:FetchDataRequest:Tables). 
        ELSE 
            ASSIGN THIS-OBJECT:NextBatchAvailable = FALSE 
                   THIS-OBJECT:FetchDataRequest   = ? 
                   THIS-OBJECT:BatchingOnTable    = ? .

        FINALLY:
            IF VALID-HANDLE (hNextBatch) THEN 
                DELETE OBJECT hNextBatch . 
            
            IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
                ServiceManager:StopBusinessService (cEntityKey) . 
        END FINALLY.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the backend
        Notes:   
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poQueryExpression The query expression to use for the first table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveData (pcTables AS CHARACTER,
                                     poQueryExpression AS IQueryExpression):
    
        DEFINE VARIABLE cQueryString      AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTables} .
        
        IF VALID-OBJECT (poQueryExpression) THEN 
            cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &2":U,
                                       ENTRY (1, pcTables),
                                       poQueryExpression:GetExpression()) .
        
        oFetchDataRequest = NEW FetchDataRequest (pcTables,
                                                  cQueryString,
                                                  THIS-OBJECT:BatchSize) .            

        oFetchDataRequest:StopAfter = THIS-OBJECT:StopAfter .

        THIS-OBJECT:PerformFetchDataRequest (oFetchDataRequest) .
 
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the backend
        Notes:   
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param pcQueryString The query string to use for the first table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveData (pcTables AS CHARACTER,
                                     pcQueryString AS CHARACTER):
    
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTables} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcQueryString} .
        
        oFetchDataRequest = NEW FetchDataRequest (pcTables,
                                                  pcQueryString,
                                                  THIS-OBJECT:BatchSize) .            

        oFetchDataRequest:StopAfter = THIS-OBJECT:StopAfter .

        THIS-OBJECT:PerformFetchDataRequest (oFetchDataRequest) .
 
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the backend
        Notes:   
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveData (poNamedQuery AS INamedQueryParameter):
    
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        oFetchDataRequest = NEW FetchDataRequest () .            

        ASSIGN oFetchDataRequest:NamedQuery = poNamedQuery 
               oFetchDataRequest:StopAfter  = THIS-OBJECT:StopAfter 
               oFetchDataRequest:NumRecords = THIS-OBJECT:BatchSize . 

        THIS-OBJECT:PerformFetchDataRequest (oFetchDataRequest) .
 
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the backend
        Notes:   
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveData (pcTables AS CHARACTER,
                                     poNamedQuery AS INamedQueryParameter):
    
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTables} .
        
        oFetchDataRequest = NEW FetchDataRequest (pcTables,
                                                  THIS-OBJECT:BatchSize) .            

        ASSIGN oFetchDataRequest:NamedQuery = poNamedQuery 
               oFetchDataRequest:StopAfter  = THIS-OBJECT:StopAfter .

        THIS-OBJECT:PerformFetchDataRequest (oFetchDataRequest) .
 
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the backend based on the given data-source 
                 ROWID (DB-ROWID)
        Notes:   DB-ROWID passed as comma-delimited list in case there are joined 
                 data-source queries
        @param pcTable The comma delimited list of the Table Names to retrieve
        @param pcSourceRowidIds The comma-delimited list of data-source rowids        
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RetrieveDataByRowid (pcTable AS CHARACTER,
                                            pcSourceRowidIds AS CHARACTER):
		
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTable} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcSourceRowidIds} .
        
        ListAssert:NumEntries (pcTable, 1) .

        oFetchDataRequest = NEW FetchDataRequest (pcTable,
                                                  1) .            

        oFetchDataRequest:StopAfter = THIS-OBJECT:StopAfter .
        oFetchDataRequest:Context   = pcSourceRowidIds .

        THIS-OBJECT:PerformFetchDataRequest (oFetchDataRequest) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs Save Changes request either using the Service Adapter or the
                 Service Interface 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges ():
        
        DEFINE VARIABLE hChangesDataset AS HANDLE       NO-UNDO .
        DEFINE VARIABLE hDataset        AS HANDLE       NO-UNDO .
        DEFINE VARIABLE cContext        AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE roRowids        AS ROWID EXTENT NO-UNDO .
    
        DEFINE VARIABLE cEntityKey AS CHARACTER NO-UNDO.
    
        IF THIS-OBJECT:UseDedicatedBusinessEntity AND 
           THIS-OBJECT:UseInterface <> UseInterfaceEnum:ServiceInterface THEN 
            UNDO, THROW NEW UnableToUseDedicatedBusinessEntityWithoutServiceInterface ("SaveChanges":U,
                                                                                       "Consultingwerk.OERA.DatasetModelPerformer":U) . 
    
        IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
            cEntityKey = SUBSTITUTE ("&1&2&3":U,
                                     THIS-OBJECT:EntityName,
                                     CHR(1),
                                     GUID) .
        ELSE 
            cEntityKey = THIS-OBJECT:EntityName .
        
        ASSIGN hDataset        = THIS-OBJECT:DatasetHandle
               roRowids        = DatasetHelper:GetDatasetRowids (hDataset) 
               hChangesDataset = DatasetHelper:GetChangesDataset (hDataset) . 

        IF THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceAdapter THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:SubmitData (THIS-OBJECT:AppServerPartition,
                                                                                  THIS-OBJECT:EntityName,
                                                                                  INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                                                                  INPUT-OUTPUT cContext) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:SaveChanges (cEntityKey,
                                                              INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE) .

        DatasetHelper:ThrowDatasetErrors (hChangesDataset) .

        DatasetHelper:SetTrackingChanges (hDataset, FALSE) .

        hChangesDataset:MERGE-CHANGES (hDataset, TRUE) .

        hDataset:ACCEPT-CHANGES () .

        FINALLY:
            IF VALID-HANDLE (hChangesDataset) THEN 
                DELETE OBJECT hChangesDataset . 

            IF THIS-OBJECT:UseDedicatedBusinessEntity THEN 
                ServiceManager:StopBusinessService (cEntityKey) . 
                
            IF EXTENT (roRowids) > 0 THEN 
                DatasetHelper:RepositionDatasetBuffers (hDataset, 
                                                        roRowids) .
        END FINALLY.

    END METHOD .

END CLASS.
