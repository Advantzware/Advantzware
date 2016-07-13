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
    File        : TableDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 27.11.2012 23:44:55 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.TableDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/System/dsTable.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartTable FOR SmartTable .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
            THIS-OBJECT:AssignSkipList = "TableGUID":U .

        THIS-OBJECT:TrackDeletions = TRUE . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsTable:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartTable:HANDLE,
                                      DATA-SOURCE src_SmartTable:HANDLE, "
                                      TableGUID,SmartTable.TableGUID,
                                      TableName,SmartTable.TableName,
                                      DatabaseName,SmartTable.DatabaseName,
                                      TableDescription,SmartTable.TableDescription,
                                      UniqueKeyFields,SmartTable.UniqueKeyFields,
                                      UniqueKeyFormats,SmartTable.UniqueKeyFormats,
                                      UniqueKeySubstitute,SmartTable.UniqueKeySubstitute,
                                      AllowComments,SmartTable.AllowComments,
                                      AllowAttachments,SmartTable.AllowAttachments,
                                      DescriptionFields,SmartTable.DescriptionFields,
                                      DescriptionFormats,SmartTable.DescriptionFormats,
                                      DescriptionSubstitute,SmartTable.DescriptionSubstitute,
                                      TempTableName,SmartTable.TempTableName,
                                      KeyFieldAssignmentGUID,SmartTable.KeyFieldAssignmentGUID":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor                                                                        
        @param dsTable DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsTable BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsTable:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartTable:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetch data from the data source
        Notes:   
        @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS IFetchDataRequest):
		
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
          
        ASSIGN cQueryString               = poFetchDataRequest:Queries 
     
               /* Call into method ManipulateQueryString */
               cQueryString               = THIS-OBJECT:ManipulateQueryString(cQueryString)
               poFetchDataRequest:Queries = cQueryString .
		
		SUPER:FetchData (poFetchDataRequest) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the connected database in the GetDatabaseNamesResult instance
        Notes:   
        @param poGetDatabaseNamesResult The GetDatabaseNamesResult instance to return the data in
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetDatabaseNames (poGetDatabaseNamesResult AS GetDatabaseNamesResult):
		
		DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cDatabases AS CHARACTER NO-UNDO.

        Consultingwerk.Assertion.ObjectAssert:IsValid (poGetDatabaseNamesResult) .

        DO i = 1 TO NUM-DBS:
            ASSIGN cDatabases = ListHelper:InsertEntry (cDatabases, i, LDBNAME (i)) .
        END.
        
        poGetDatabaseNamesResult:DatabaseNames = cDatabases .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the connected database in the GetDatabaseNamesResult instance
        Notes:   
        @param poImportDatabaseTablesParameter The ImportDatabaseTablesParameter instance with the database name to process
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportDatabaseTables (poImportDatabaseTablesParameter AS ImportDatabaseTablesParameter):
        
        DEFINE VARIABLE cDatabases   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hFileBuffer  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cIndexFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFormats     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cSubstitute  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poImportDatabaseTablesParameter) .
        Consultingwerk.Assertion.DatabaseAssert:IsConnected (poImportDatabaseTablesParameter:DatabaseName) .
        
        CREATE BUFFER hFileBuffer 
            FOR TABLE SUBSTITUTE ("&1._file":U, 
                                  poImportDatabaseTablesParameter:DatabaseName) .
                       
        ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hFileBuffer,
                                                         "FOR EACH _file WHERE NOT _Hidden":U) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            
            IF NOT CAN-FIND (SmartTable WHERE SmartTable.DatabaseName = poImportDatabaseTablesParameter:DatabaseName
                                          AND SmartTable.TableName    = hFileBuffer::_file-name) THEN DO ON ERROR UNDO, THROW:
                                              
                CREATE BUFFER hBuffer 
                    FOR TABLE SUBSTITUTE ("&1.&2":U,
                                          poImportDatabaseTablesParameter:DatabaseName,
                                          hFileBuffer::_file-name) .
                                              
                CREATE SmartTable .
                ASSIGN SmartTable.DatabaseName     = poImportDatabaseTablesParameter:DatabaseName
                       SmartTable.TableName        = hFileBuffer::_file-name
                       SmartTable.TableDescription = hFileBuffer::_desc
                       SmartTable.TableGUID        = GUID 
                       
                       cIndexFields                = BufferHelper:UniqueKeyFields (hBuffer, TRUE).
                                 
                IF Consultingwerk.CharacterType:IsNullOrEmpty (cIndexFields) THEN 
                    ASSIGN cIndexFields = BufferHelper:UniqueKeyFields (hBuffer, FALSE) .
                                 
                ASSIGN cFormats                   = "":U 
                       cSubstitute                = "":U . 
                           
                DO i = 1 TO NUM-ENTRIES (cIndexFields):
                    
                    ASSIGN hField      = hBuffer:BUFFER-FIELD (ENTRY (i, cIndexFields)) 
                            
                           cFormats    = SUBSTITUTE ("&1~~&2":U, cFormats, hField:FORMAT)
                           cSubstitute = SUBSTITUTE ("&1~~&&&2":U, cSubstitute, i)  .
                END.
                                  
                ASSIGN SmartTable.UniqueKeyFields     = cIndexFields 
                       SmartTable.UniqueKeyFormats    = TRIM (cFormats, "~~":U)
                       SmartTable.UniqueKeySubstitute = TRIM (cSubstitute, "~~":U) .
                  
                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hBuffer) .		
                END FINALLY.
            END.
            
            hQuery:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hFileBuffer) .
        END FINALLY.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Mamipulates a Query String on eSmartTable.QualifiedTableName
        Notes:   
        @param pcQueryString The original query string
        @return The manipulated query string 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ManipulateQueryString (pcQueryString AS CHARACTER):
           
        DEFINE VARIABLE oQueryParser        AS Consultingwerk.QueryParser      NO-UNDO . 
        DEFINE VARIABLE oQueryExpression    AS Consultingwerk.IQueryExpression NO-UNDO . 
        DEFINE VARIABLE oFieldExpression    AS Consultingwerk.QueryExpression  NO-UNDO . 
        DEFINE VARIABLE cQualifiedTableName AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE cDbName             AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE cTableName          AS CHARACTER                       NO-UNDO .
                    
        oQueryParser = NEW Consultingwerk.QueryParser () .
        oQueryExpression = oQueryParser:ParseQueryString (pcQueryString) .
        oFieldExpression = Consultingwerk.QueryExpression:GetExpressionForField ("eSmartTable.QualifiedTableName":U, 
                                                                                 oQueryExpression) .                    
                    
        IF VALID-OBJECT (oFieldExpression) THEN DO:

            ASSIGN cQualifiedTableName = CAST (oFieldExpression:SecondOperand, Consultingwerk.CharacterHolder):Value 
            
                   cDbName             = ENTRY (1, cQualifiedTableName, ".":U) .
                   
            IF NUM-ENTRIES (cQualifiedTableName, ".":U) > 1 THEN                    
                ASSIGN cTableName = ENTRY (2, cQualifiedTableName, ".":U). 
                
            IF cTableName = "":U THEN DO:                             
                CAST (oFieldExpression:FirstOperand, Consultingwerk.BufferFieldName):FieldName = "DatabaseName":U .
                    
                RETURN "FOR EACH eSmartTable ":U + 
                       oQueryExpression:GetExpression() +
                       REPLACE (oQueryParser:ByClause, " BY eSmartTable.QualifiedTableName":U, " BY eSmartTable.DatabaseName":U) /* substitute the calc field in the BY clause */.  
            END.                
            ELSE 
                RETURN SUBSTITUTE ("for each eSmartTable where eSmartTable.DatabaseName = &2 AND eSmartTable.TableName &1 &3":U,
                                   STRING (oFieldExpression:Operator),
                                   QUOTER (cDbName), 
                                   QUOTER (cTableName)) .
        END.
        
        RETURN REPLACE (pcQueryString, " BY eSmartTable.QualifiedTableName":U, " BY eSmartTable.DatabaseName":U) /* substitute the calc field in the BY clause */.  
            
    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition) 
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for             
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
       
        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartTable":U THEN
                cMap = DATA-SOURCE src_SmartTable:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
            WHEN "eSmartTable":U THEN
                RETURN "FOR EACH SmartTable INDEXED-REPOSITION":U. 

        END. 
    END METHOD.

END CLASS.
