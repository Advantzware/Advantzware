/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessEntityResultsetMapper
    Purpose     : Allows mapping a Business Entity Resultset (ProDataset)
                  into a Temp-Table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 12 14:22:42 CET 2014
    Notes       : SCL-587
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*           FROM PROPATH . 
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH . 
USING Consultingwerk.Util.*      FROM PROPATH . 
USING Test.Akioma.*              FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityResultsetMapper: 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this class when retrieving data via the ServiceAdaptre 
        Notes:   Only relevant when running on the client side (no databases connected)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Fetches Records from a Business Entity and appends them based on 
                 the provided field mapping to the provided temp-table
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param pcBusinessEntity The Business Entity Name to retrieve records from 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param poQueryExpression The QueryExpression to use for the filter
        @param pcEntityTableMappings The fields to map for the entity table
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveDataAndMapResultset (pcBusinessEntity AS CHARACTER, 
                                                    pcEntityTable AS CHARACTER,
                                                    pcEntityView AS CHARACTER,
                                                    poQueryExpression AS IQueryExpression,
                                                    pcEntityTableMappings AS CHARACTER, 
                                                    INPUT-OUTPUT TABLE-HANDLE phTable):

        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO EXTENT 0.
         
        THIS-OBJECT:RetrieveDataAndMapResultset (pcBusinessEntity, 
                                                 pcEntityTable,
                                                 pcEntityView,
                                                 poQueryExpression,
                                                 pcEntityTableMappings, 
                                                 cMapping, 
                                                 INPUT-OUTPUT TABLE-HANDLE phTable BY-REFERENCE) .
                                                        
    END.    
    
    /*------------------------------------------------------------------------------
        Purpose: Fetches Records from a Business Entity and appends them based on 
                 the provided field mapping to the provided temp-table
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param pcBusinessEntity The Business Entity Name to retrieve records from 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param poQueryExpression The QueryExpression to use for the filter
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The comma delimited list fields to map for the entity view table (only the first one)
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveDataAndMapResultset (pcBusinessEntity AS CHARACTER, 
                                                    pcEntityTable AS CHARACTER,
                                                    pcEntityView AS CHARACTER,
                                                    poQueryExpression AS IQueryExpression,
                                                    pcEntityTableMappings AS CHARACTER, 
                                                    pcEntityViewMappings AS CHARACTER, 
                                                    INPUT-OUTPUT TABLE-HANDLE phTable):

        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO EXTENT 1.
         
        ASSIGN cMapping[1] = pcEntityViewMappings .
         
        THIS-OBJECT:RetrieveDataAndMapResultset (pcBusinessEntity, 
                                                 pcEntityTable,
                                                 pcEntityView,
                                                 poQueryExpression,
                                                 pcEntityTableMappings, 
                                                 cMapping, 
                                                 INPUT-OUTPUT TABLE-HANDLE phTable BY-REFERENCE) .
                                                        
    END.                                                         
                                                            
    /*------------------------------------------------------------------------------
        Purpose: Fetches Records from a Business Entity and appends them based on 
                 the provided field mapping to the provided temp-table
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param pcBusinessEntity The Business Entity Name to retrieve records from 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param poQueryExpression The QueryExpression to use for the filter
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The Character array of fields to map for the entity view tables, extent of the array needs to match the number of view tables
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RetrieveDataAndMapResultset (pcBusinessEntity AS CHARACTER, 
       	                                            pcEntityTable AS CHARACTER,
       	                                            pcEntityView AS CHARACTER,
        	                                        poQueryExpression AS IQueryExpression,
        	                                        pcEntityTableMappings AS CHARACTER, 
        	                                        pcEntityViewMappings AS CHARACTER EXTENT, 
        	                                        INPUT-OUTPUT TABLE-HANDLE phTable):
		
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hDataset AS HANDLE           NO-UNDO .

        ASSIGN oRequest = NEW FetchDataRequest (TRIM (SUBSTITUTE ("&1,&2":U, pcEntityTable, pcEntityView), ",":U),
                                                poQueryExpression:GetExpression()) . 
		
		IF NUM-DBS = 0 THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:RetrieveData (THIS-OBJECT:AppServerPartition,
                                                                                    pcBusinessEntity,
                                                                                    oRequest,
                                                                                    OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:FetchData (pcBusinessEntity,
                                                            oRequest,
                                                            OUTPUT DATASET-HANDLE hDataset /* No BY-REFERENCE when calling 
                                                                                              into ServiceInterface:FetchData, 
                                                                                              see comments there */) .

        THIS-OBJECT:MapResultsetIntoTempTable (hDataset,
                                               pcEntityTable,
                                               pcEntityView, 
                                               pcEntityTableMappings,
                                               pcEntityViewMappings,
                                               INPUT-OUTPUT TABLE-HANDLE phTable BY-REFERENCE) .
        
        FINALLY:
            IF VALID-HANDLE (hDataset) THEN  
                DELETE OBJECT hDataset .                 		  
        END FINALLY.                                 
                                 
    END METHOD .                                    

    /*------------------------------------------------------------------------------
        Purpose: Merges ProDataset records from the passed ProDataset to the provided 
                 target temp-table record
                 Supports joining multiple ProDataset records (parent/child) and field 
                 mapping.
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param phTempTableBuffer The buffer of a temp-table to copy the record to
        @param phQueryHandle The handle of the query that joins the EntityTable with the EntityView Tables. This query needs to be repositioned (by your code) to the proper result row
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The comma delimited list of fields to map for the entity view tables, extent of the array needs to match the number of view tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MapResultsetIntoTempTable (phTempTableBuffer AS HANDLE, 
                                                  phQueryHandle AS HANDLE, 
                                                  pcEntityTableMappings AS CHARACTER, 
                                                  pcEntityViewMappings AS CHARACTER):

        DEFINE VARIABLE cEntityViewMappings AS CHARACTER NO-UNDO EXTENT 1.

        ASSIGN cEntityViewMappings[1] = pcEntityViewMappings .

        THIS-OBJECT:MapResultsetIntoTempTable (phTempTableBuffer, 
                                               phQueryHandle, 
                                               pcEntityTableMappings, 
                                               cEntityViewMappings) .

    END.

    /*------------------------------------------------------------------------------
        Purpose: Merges ProDataset records from the passed ProDataset to the provided 
                 target temp-table record
                 Supports joining multiple ProDataset records (parent/child) and field 
                 mapping.
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param phTempTableBuffer The buffer of a temp-table to copy the record to
        @param phQueryHandle The handle of the query that joins the EntityTable with the EntityView Tables. This query needs to be repositioned (by the callers code) to the proper result row
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The Character array of fields to map for the entity view tables, extent of the array needs to match the number of view tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MapResultsetIntoTempTable (phTempTableBuffer AS HANDLE, 
                                                  phQueryHandle AS HANDLE, 
                                                  pcEntityTableMappings AS CHARACTER, 
                                                  pcEntityViewMappings AS CHARACTER EXTENT):
        
        DEFINE VARIABLE i             AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
        DEFINE VARIABLE hSourceBuffer AS HANDLE  NO-UNDO.
        
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phTempTableBuffer} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phQueryHandle WidgetTypeEnum:Query} .
        
        ASSIGN iCount = EXTENT (pcEntityViewMappings) .
        
        DO i = 2 TO phQueryHandle:NUM-BUFFERS:
            hSourceBuffer = phQueryHandle:GET-BUFFER-HANDLE (i) . 
                 
            IF hSourceBuffer:AVAILABLE THEN 
                phTempTableBuffer:BUFFER-COPY (hSourceBuffer, ?, pcEntityViewMappings[i - 1]) .
        END.
             
        phTempTableBuffer:BUFFER-COPY (phQueryHandle:GET-BUFFER-HANDLE (1), ?, pcEntityTableMappings) .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Appends records from the passed ProDataset to the provided temp-table.
                 Supports joining multiple ProDataset records (parent/child) and field 
                 mapping.
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param phDataset The handle of the ProDataset with the data to map to the temp-table 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param pcEntityTableMappings The fields to map for the entity table
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MapResultsetIntoTempTable (phDataset AS HANDLE, 
                                                  pcEntityTable AS CHARACTER,
                                                  pcEntityView AS CHARACTER,
                                                  pcEntityTableMappings AS CHARACTER, 
                                                  INPUT-OUTPUT TABLE-HANDLE phTable):  
                
        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO EXTENT 0.
         
        THIS-OBJECT:MapResultsetIntoTempTable (phDataset, 
                                               pcEntityTable,
                                               pcEntityView,
                                               pcEntityTableMappings, 
                                               cMapping, 
                                               INPUT-OUTPUT TABLE-HANDLE phTable BY-REFERENCE) . 
                                               
    END METHOD.                                                                                      

    /*------------------------------------------------------------------------------
        Purpose: Appends records from the passed ProDataset to the provided temp-table.
                 Supports joining multiple ProDataset records (parent/child) and field 
                 mapping.
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param phDataset The handle of the ProDataset with the data to map to the temp-table 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The comma delimited list of fields to map for the entity view table (only one)
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MapResultsetIntoTempTable (phDataset AS HANDLE, 
                                                  pcEntityTable AS CHARACTER,
                                                  pcEntityView AS CHARACTER,
                                                  pcEntityTableMappings AS CHARACTER, 
                                                  pcEntityViewMappings AS CHARACTER, 
                                                  INPUT-OUTPUT TABLE-HANDLE phTable):  
                
        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO EXTENT 1.
         
        ASSIGN cMapping[1] = pcEntityViewMappings .
         
        THIS-OBJECT:MapResultsetIntoTempTable (phDataset, 
                                               pcEntityTable,
                                               pcEntityView,
                                               pcEntityTableMappings, 
                                               cMapping, 
                                               INPUT-OUTPUT TABLE-HANDLE phTable BY-REFERENCE) . 
                                               
    END METHOD.                                                                                      
                                                                                         
    /*------------------------------------------------------------------------------
        Purpose: Appends records from the passed ProDataset to the provided temp-table.
                 Supports joining multiple ProDataset records (parent/child) and field 
                 mapping.
        Notes:   The Temp-Table is received as INPUT-OUTPUT Parameter to allow multiple
                 calls to this method with the same temp-table. This may be useful 
                 when multiple backend calls (queries to the same or different Business
                 Entity) should be added to the same view temp-table in a sequence. Like
                 when populating the AKIOMA SWAT tree of struct records.
        @param phDataset The handle of the ProDataset with the data to map to the temp-table 
        @param pcEntityTable The Entity Table 
        @param pcEntityView The comma delimited list of additional business entity tables
        @param pcEntityTableMappings The fields to map for the entity table
        @param pcEntityViewMappings The Character array of fields to map for the entity view tables, extent of the array needs to match the number of view tables
        @param phTable INPUT-OUTPUT BY-REFERENCE The result temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MapResultsetIntoTempTable (phDataset AS HANDLE, 
                                                  pcEntityTable AS CHARACTER,
                                                  pcEntityView AS CHARACTER,
                                                  pcEntityTableMappings AS CHARACTER, 
                                                  pcEntityViewMappings AS CHARACTER EXTENT, 
                                                  INPUT-OUTPUT TABLE-HANDLE phTable):                                    
                                   
        DEFINE VARIABLE oQueryBuilder AS ResultsetQueryBuilder NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE                NO-UNDO .
        DEFINE VARIABLE hSourceBuffer AS HANDLE                NO-UNDO .
        DEFINE VARIABLE iCount        AS INTEGER               NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER               NO-UNDO .
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .                                             
                                    
        oQueryBuilder = NEW ResultsetQueryBuilder () . 
        hQuery = oQueryBuilder:PrepareResultsetQuery (phDataset,
                                                      pcEntityTable,
                                                      pcEntityView,
                                                      TRUE) .
        hQuery:QUERY-OPEN () . 
        hQuery:GET-FIRST () .
                          
        ASSIGN iCount = NUM-ENTRIES (pcEntityView) .
            
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
                
            phTable:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE ()  . 
                
            DO i = 1 TO iCount:
                hSourceBuffer = phDataset:GET-BUFFER-HANDLE (ENTRY (i, pcEntityView)) . 
                    
                IF hSourceBuffer:AVAILABLE THEN 
                    phTable:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (hSourceBuffer, ?, pcEntityViewMappings[i]) .
            END.
                
            phTable:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (phDataset:GET-BUFFER-HANDLE (pcEntityTable), ?, pcEntityTableMappings) .
               
            hQuery:GET-NEXT () .        
        END.        
                 
        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .	
        END FINALLY.                                    

	END METHOD .

END CLASS.
