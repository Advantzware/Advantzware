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
    File        : ModuleDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 17:14:28 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.ModuleDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/Menu/dsModule.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE DATA-SOURCE src_SmartModule FOR SmartModule .


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModuleDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModuleDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
            AssignSkipList = "ModuleGuid":U .

        THIS-OBJECT:TrackDeletions = TRUE . 

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsModule:HANDLE, FALSE) .
        
        BUFFER eSmartModule:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartModule:HANDLE, "ModuleGuid,SmartModule.ModuleGuid,ModuleName,SmartModule.ModuleName,ModuleDescription,SmartModule.ModuleDescription":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor                                                                        
        @param dsModule DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsModule BIND):
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsModule:HANDLE, FALSE) .
    
        BUFFER eSmartModule:DETACH-DATA-SOURCE () .
        
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
        
        CASE pcTable:
            WHEN "eSmartModule":U THEN
                cMap = DATA-SOURCE src_SmartModule:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
    
        CASE pcTable:
            WHEN "eSmartModule":U THEN
                RETURN "FOR EACH SmartModule INDEXED-REPOSITION":U. 

        END. 
    END METHOD.

END CLASS.
