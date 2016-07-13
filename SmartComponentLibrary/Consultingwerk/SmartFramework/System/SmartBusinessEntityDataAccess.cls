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
    File        : SmartBusinessEntityDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 19:02:05 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/System/dsSmartBusinessEntity.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartBusinessEntity FOR SmartBusinessEntity .
    DEFINE PRIVATE DATA-SOURCE src_SmartBusinessEntityTable FOR SmartBusinessEntityTable .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* THIS-OBJECT:AssignSkipList = "<key fields>":U. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN DO: 
            THIS-OBJECT:AssignSkipDictionary:Add ("eSmartBusinessEntity":U, "BusinessEntityGuid":U).
            THIS-OBJECT:AssignSkipDictionary:Add ("eSmartBusinessEntityTable":U, "BusinessEntityTableGuid":U).
        END.
        
        THIS-OBJECT:TrackDeletions = TRUE . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSmartBusinessEntity:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartBusinessEntity:HANDLE,
                                      DATA-SOURCE src_SmartBusinessEntity:HANDLE, "
                                      BusinessEntityGuid,SmartBusinessEntity.BusinessEntityGuid,
                                      BusinessEntityPackage,SmartBusinessEntity.BusinessEntityPackage,
                                      BusinessEntityName,SmartBusinessEntity.BusinessEntityName,
                                      BusinessEntityPurpose,SmartBusinessEntity.BusinessEntityPurpose,
                                      DBRequired,SmartBusinessEntity.DBRequired,
                                      TrackDeletions,SmartBusinessEntity.TrackDeletions":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartBusinessEntityTable:HANDLE,
                                      DATA-SOURCE src_SmartBusinessEntityTable:HANDLE, "
                                      BusinessEntityTableGuid,SmartBusinessEntityTable.BusinessEntityTableGuid,
                                      BusinessEntityGuid,SmartBusinessEntityTable.BusinessEntityGuid,
                                      TableOrder,SmartBusinessEntityTable.TableOrder,
                                      TableName,SmartBusinessEntityTable.TableName,
                                      TableLabelSingular,SmartBusinessEntityTable.TableLabelSingular,
                                      TableLabelPlural,SmartBusinessEntityTable.TableLabelPlural,
                                      ShouldUseBatching,SmartBusinessEntityTable.ShouldUseBatching,
                                      BatchSize,SmartBusinessEntityTable.BatchSize,
                                      AllowDelete,SmartBusinessEntityTable.AllowDelete,
                                      AllowCreate,SmartBusinessEntityTable.AllowCreate,
                                      AllowUpdate,SmartBusinessEntityTable.AllowUpdate":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsSmartBusinessEntity DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsSmartBusinessEntity BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData, note that the callback methods need to be 
                 PUBLIC and need to have the Dataset defined as an INPUT Parameter
        Sample:  BUFFER eCustomer:SET-CALLBACK (Consultingwerk.DatasetBufferCallbackEnum:AfterFill,
                                                "eCustomerAfterRowFill":U) .                 
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSmartBusinessEntity:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartBusinessEntity:DETACH-DATA-SOURCE () .
        BUFFER eSmartBusinessEntityTable:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition) 
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
                 TO-DO: Provide code for alternative mapping                                             
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for             
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        
        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartBusinessEntity":U THEN
                cMap = DATA-SOURCE src_SmartBusinessEntity:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 
            WHEN "eSmartBusinessEntityTable":U THEN
                cMap = DATA-SOURCE src_SmartBusinessEntityTable:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

        END.
     
        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).                                                                      
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
                 TO-DO: Provide code to return the DATA-SOURCE root query string                                             
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):
    
        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartBusinessEntity":U THEN
                RETURN "FOR EACH SmartBusinessEntity INDEXED-REPOSITION":U. 
            WHEN "eSmartBusinessEntityTable":U THEN
                RETURN "FOR EACH SmartBusinessEntityTable INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
