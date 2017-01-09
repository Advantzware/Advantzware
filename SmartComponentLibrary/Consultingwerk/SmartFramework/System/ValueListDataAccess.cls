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
    File        : ValueListDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 08.03.2013 15:44:37 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH . 
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.ValueListDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/System/dsValueList.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartValueList FOR SmartValueList .
    DEFINE PRIVATE DATA-SOURCE src_SmartValueListEntry FOR SmartValueListEntry .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValueListDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValueListDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartValueList":U, "ValueListGuid":U) .
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartValueListEntry":U, "ValueListEntryGuid":U) .

        THIS-OBJECT:TrackDeletions = TRUE . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsValueList:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        BUFFER eSmartValueList:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartValueList:HANDLE, "ValueListGuid,SmartValueList.ValueListGuid,ModuleGuid,SmartValueList.ModuleGuid,ValueListKey,SmartValueList.ValueListKey,ValueListDescription,SmartValueList.ValueListDescription":U) .
        BUFFER eSmartValueListEntry:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartValueListEntry:HANDLE, "ValueListEntryGuid,SmartValueListEntry.ValueListEntryGuid,ValueListGuid,SmartValueListEntry.ValueListGuid,ValueListEntryKey,SmartValueListEntry.ValueListEntryKey,ValueListEntryDescription,SmartValueListEntry.ValueListEntryDescription,ValueListEntryImageKey,SmartValueListEntry.ValueListEntryImageKey":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsValueList DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsValueList BIND):
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsValueList:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartValueList:DETACH-DATA-SOURCE () .
        BUFFER eSmartValueListEntry:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.
        
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
            WHEN "eSmartValueList":U THEN
                cMap = DATA-SOURCE src_SmartValueList:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 
            WHEN "eSmartValueListEntry":U THEN
                cMap = DATA-SOURCE src_SmartValueListEntry:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
            WHEN "eSmartValueList":U THEN
                RETURN "FOR EACH SmartValueList INDEXED-REPOSITION":U. 
            WHEN "eSmartValueListEntry":U THEN
                RETURN "FOR EACH SmartValueListEntry INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
