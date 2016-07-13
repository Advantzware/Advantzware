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
    File        : DatabaseSchemaDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 19:46:15 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH.
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaDataAccess 
    INHERITS DataAccess: 

    {Consultingwerk/BusinessEntityDesigner/Services/dsDatabaseSchema.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatabaseSchemaDataAccess class                                                                      
        Notes:                         
        @param phDataset The handle of the Database Schema dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseSchemaDataAccess (INPUT phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   
        @param dsDatabaseSchema DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsDatabaseSchema BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Not used in this class
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Not used in this class
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetch data from the data source
        Notes:    
        @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID FetchData (INPUT poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
                
        DEFINE VARIABLE oNumericFormat AS Consultingwerk.NumericFormat NO-UNDO .                 
        DEFINE VARIABLE oDateFormat    AS Consultingwerk.DateFormat    NO-UNDO .                 
                
        oNumericFormat = SessionHelper:GetNumericFormat() .
        oDateFormat    = SessionHelper:GetDateFormat() .

        SessionHelper:SetDefaultNumericFormat() .
        SessionHelper:SetDefaultDateFormat() .
                        
        EMPTY TEMP-TABLE eDatabase .                 
        EMPTY TEMP-TABLE eTable .                 
        EMPTY TEMP-TABLE eField .                 
        EMPTY TEMP-TABLE eIndex .                                                                         
                         
        DO i = 1 TO NUM-DBS:
            THIS-OBJECT:FetchSchema (LDBNAME (i)) .
        END.
                                
        FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .
            SessionHelper:SetDateFormat (oDateFormat) .		
        END FINALLY.                                
                                
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches schema definition for the given database                                                                         
        Notes:                                                        
        @param pcDatabase The name of the database                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FetchSchema (pcDatabase AS CHARACTER):
        
        DEFINE VARIABLE oStatusManager AS IStatusManager NO-UNDO . 
        DEFINE VARIABLE hFileBuffer    AS HANDLE         NO-UNDO .
        DEFINE VARIABLE hFileQuery     AS HANDLE         NO-UNDO .
        
        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN 
            oStatusManager:DisplayStatus (SUBSTITUTE ("Loading Database Schema... (&1)":U, pcDatabase)) .

        CREATE eDatabase .
        ASSIGN eDatabase.DatabaseName = pcDatabase .    

        CREATE BUFFER hFileBuffer FOR TABLE pcDatabase + "._file":U .
        hFileQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery 
                            (hFileBuffer, "FOR EACH _file WHERE NOT _Hidden":U) .
                
        DO WHILE NOT hFileQuery:QUERY-OFF-END:
            FetchTable (pcDatabase, hFileBuffer::_File-Name) .
            
            hFileQuery:GET-NEXT () .    
        END.                

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hFileQuery) .            
            GarbageCollectorHelper:DeleteObject (hFileBuffer) .            
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches schema definition for the given database table                                                                         
        Notes:                                                        
        @param pcDatabase The name of the database                
        @param pcFileName The name of the database table                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FetchTable (pcDatabase AS CHARACTER,
                                      pcFileName AS CHARACTER):
        
        DEFINE VARIABLE hBuffer        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cIndex         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j              AS INTEGER   NO-UNDO.
        
        DEFINE VARIABLE cFullTableName AS CHARACTER NO-UNDO.
        
        ASSIGN cFullTableName = SUBSTITUTE ("&1.&2":U,
                                            pcDatabase,
                                            pcFileName) .
        
        CREATE BUFFER hBuffer FOR TABLE cFullTableName . 
        
        CREATE eTable . 
        ASSIGN eTable.BusinessEntityName = pcDatabase 
               eTable.TempTableName      = cFullTableName 
               . 
        
        DO i = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN hField = hBuffer:BUFFER-FIELD (i) .
            
            CREATE eField . 
            ASSIGN eField.BusinessEntityName   = pcDatabase
                   eField.TempTableName        = cFullTableName 
                   eField.FieldName            = hField:NAME  
                   eField.FieldOrder           = i
                   eField.FieldDataType        = hField:DATA-TYPE 
                   eField.FieldDescription     = hField:LABEL  
                   eField.FieldInitial         = hField:DEFAULT-VALUE 
                   eField.FieldLabel           = hField:LABEL
                   eField.FieldFormat          = hField:FORMAT 
                   eField.FieldSerializeHidden = FALSE   
                   eField.FieldSerializeName   = hField:XML-NODE-NAME 
                   eField.FieldXmlDataType     = hField:XML-DATA-TYPE 
                   eField.FieldXmlNodeType     = hField:XML-NODE-TYPE 
                   eField.FieldExtent          = hField:EXTENT
                   eField.FieldCaseSensitive   = hField:CASE-SENSITIVE 
              .
              
              IF hField:DEFAULT-VALUE = ? THEN 
                  ASSIGN eField.FieldInitialUnknownValue = TRUE 
                         eField.FieldInitial             = "":U . 
              
              IF hField:DATA-TYPE = "DATE":U OR hField:DATA-TYPE BEGINS "DATETIME":U THEN 
                  ASSIGN eField.FieldInitial = hField:DEFAULT-STRING . 
        END.
        
        ASSIGN i      = 1
               cIndex = hBuffer:INDEX-INFORMAT (i) .  
        
        DO WHILE cIndex > "":U:
            
            /* Mike Fechner, Consultingwerk Ltd. 06.04.2014
               Index should have at least one field - avoid "default" index with no field */
            IF NUM-ENTRIES (cIndex) > 5 THEN DO:
                CREATE eIndex .
                ASSIGN eIndex.BusinessEntityName   = pcDatabase
                       eIndex.TempTableName        = cFullTableName 
                       eIndex.IndexOrder           = i 
                       eIndex.IndexName            = ENTRY (1, cIndex) 
                       eIndex.IndexUnique          = ENTRY (2, cIndex) = "1":U 
                       eIndex.IndexPrimary         = ENTRY (3, cIndex) = "1":U
                       eIndex.IndexWordIndex       = ENTRY (4, cIndex) = "1":U .
                        
                DO j = 6 TO NUM-ENTRIES (cIndex) BY 2:
                    ASSIGN eIndex.FieldNames = eIndex.FieldNames + ",":U + 
                                               ENTRY (j - 1, cIndex)
                           eIndex.FieldSort  = eIndex.FieldSort  + ",":U + 
                                               (IF ENTRY(j, cIndex) = "0":U THEN "ASCENDING":U ELSE "DESCENDING":U)
                           .
                END.        
                
                ASSIGN eIndex.FieldNames = TRIM(eIndex.FieldNames, ",":U)
                       eIndex.FieldSort  = TRIM(eIndex.FieldSort, ",":U) . 
            END. 
                       
            ASSIGN i                 = i + 1 
                   cIndex            = hBuffer:INDEX-INFORMAT (i) .  
        END.
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

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
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTE):
    END METHOD.

END CLASS.
