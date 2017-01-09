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
    File        : ObjectDefNamesDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 03.11.2013 13:19:55 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.RollbaseAdapter.*          FROM PROPATH .  
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH . 
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesDataAccess INHERITS DataAccess: 

    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDefNames.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE TEMP-TABLE eObjectDefNameTmp NO-UNDO 
        FIELD ObjectDefName AS CHARACTER FORMAT "X(60)":U LABEL "Integration Name":T
        INDEX Primary AS UNIQUE PRIMARY ObjectDefName ASCENDING
        .

    DEFINE DATA-SOURCE srcObjectDefNameTmp FOR eObjectDefNameTmp .  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectDefNamesDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectDefNamesDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* THIS-OBJECT:AssignSkipList = "<key fields>":U. */
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectDefNames:HANDLE, FALSE) .
        
        BUFFER eObjectDefName:ATTACH-DATA-SOURCE (DATA-SOURCE srcObjectDefNameTmp:HANDLE) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsObjectDefNames DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsObjectDefNames BIND):
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectDefNames:HANDLE, FALSE) .
    
        BUFFER eObjectDefName:DETACH-DATA-SOURCE () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetch data from the data source
        Notes:   Uses the RollbaseAdapter to retrieve from external server 
        @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):
		
        DEFINE VARIABLE oAdapter     AS IRollbaseAdapter     NO-UNDO . 
        DEFINE VARIABLE oCredentials AS IRollbaseCredentials NO-UNDO . 
        DEFINE VARIABLE cNames       AS CHARACTER            NO-UNDO EXTENT .
        DEFINE VARIABLE i            AS INTEGER              NO-UNDO.

        DATASET dsObjectDefNames:EMPTY-DATASET () . 
        
        EMPTY TEMP-TABLE eObjectDefNameTmp .
        
        oCredentials = {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseCredentials
                                                 "NEW Consultingwerk.RollbaseAdapter.RollbaseCredentials()"} .
        
        oAdapter = {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseAdapter
                                                 "NEW Consultingwerk.RollbaseAdapter.RollbaseAdapter()"} .

        cNames = oAdapter:GetObjectDefNames (PermissionEnum:View) .
    
        DO i = 1 TO EXTENT (cNames):
            CREATE eObjectDefNameTmp . 
            ASSIGN eObjectDefNameTmp.ObjectDefName  = cNames[i] .
        END.

        SUPER:FetchData (poFetchDataRequest) .

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
        
        CASE pcTable:
            WHEN "eObjectDefName":U THEN
                cMap = DATA-SOURCE srcObjectDefNameTmp:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 
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
    
        CASE pcTable:
            WHEN "eObjectDefName":U THEN
                RETURN "FOR EACH eObjectDefNameTmp INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
