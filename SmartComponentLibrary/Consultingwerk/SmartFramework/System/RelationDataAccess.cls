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
    File        : RelationDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.04.2013 21:42:53 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH . 
USING Consultingwerk.SmartFramework.*        FROM PROPATH . 
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.RelationDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/System/dsRelation.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartRelation FOR SmartRelation .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RelationDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
            THIS-OBJECT:AssignSkipList = "RelationGuid":U .
        
        THIS-OBJECT:TrackDeletions = TRUE . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsRelation:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        BUFFER eSmartRelation:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartRelation:HANDLE, "RelationGuid,SmartRelation.RelationGuid,ParentTableGUID,SmartRelation.ParentTableGUID,ChildTableGUID,SmartRelation.ChildTableGUID,RelationName,SmartRelation.RelationName,RelationDescription,SmartRelation.RelationDescription,RelationFieldPair,SmartRelation.RelationFieldPair,ParentRoleDescription,SmartRelation.ParentRoleDescription,ChildRoleDescription,SmartRelation.ChildRoleDescription,ParentCardinality,SmartRelation.ParentCardinality,ChildCardinality,SmartRelation.ChildCardinality,OnParentDelete,SmartRelation.OnParentDelete,DefaultParentRelation,SmartRelation.DefaultParentRelation,Inactive,SmartRelation.Inactive":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsRelation DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsRelation BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
        BUFFER eSmartRelation:SET-CALLBACK (DatasetBufferCallbackEnum:AfterRowFill,
                                            "eSmartRelationAfterFill":U) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsRelation:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartRelation:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: AFTER-FILL Callback for the eSmartRelation Table 
        Notes:   We're not using the SmartRelation itself for populating the Parent 
                 and Child table name to avoid chicken and egg situations
        @param dsRelation The Dataset 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID eSmartRelationAfterFill (DATASET dsRelation):
		
        DEFINE BUFFER SmartTable FOR SmartTable . 
        
        FIND SmartTable WHERE SmartTable.TableGUID = eSmartRelation.ParentTableGUID NO-ERROR . 

        IF AVAILABLE SmartTable THEN  
            ASSIGN eSmartRelation.ParentTableName = SUBSTITUTE ("&1.&2":U,
                                                                SmartTable.DatabaseName,
                                                                SmartTable.TableName) .

        FIND SmartTable WHERE SmartTable.TableGUID = eSmartRelation.ChildTableGUID NO-ERROR . 

        IF AVAILABLE SmartTable THEN  
            ASSIGN eSmartRelation.ChildTableName = SUBSTITUTE ("&1.&2":U,
                                                               SmartTable.DatabaseName,
                                                               SmartTable.TableName) .

	END METHOD .

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
            WHEN "eSmartRelation":U THEN
                cMap = DATA-SOURCE src_SmartRelation:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

        END.
     
        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).                                                                      
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):
    
        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartRelation":U THEN
                RETURN "FOR EACH SmartRelation INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
