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
    File        : SecurityAssignmentDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 17:49:49 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH . 
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityAssignmentDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/Authorization/dsSecurityAssignment.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartSecurityAssignment FOR SmartSecurityAssignment .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityAssignmentDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityAssignmentDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
             THIS-OBJECT:AssignSkipList = "SecurityAssignmentGuid":U.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSecurityAssignment:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        BUFFER eSmartSecurityAssignment:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartSecurityAssignment:HANDLE, "SecurityAssignmentGuid,SmartSecurityAssignment.SecurityAssignmentGuid,SecurityRealmGuid,SmartSecurityAssignment.SecurityRealmGuid,SecurityItemGuid,SmartSecurityAssignment.SecurityItemGuid,GroupGuid,SmartSecurityAssignment.GroupGuid,UserGuid,SmartSecurityAssignment.UserGuid,Restricted,SmartSecurityAssignment.Restricted,LoginCompanyGuid,SmartSecurityAssignment.LoginCompanyGuid":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsSecurityAssignment DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsSecurityAssignment BIND):
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSecurityAssignment:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartSecurityAssignment:DETACH-DATA-SOURCE () .
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
            WHEN "eSmartSecurityAssignment":U THEN
                cMap = DATA-SOURCE src_SmartSecurityAssignment:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
    
        DEFINE VARIABLE cCompanyGuid     AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oQueryParser     AS QueryParser      NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS IQueryExpression NO-UNDO .     
        DEFINE VARIABLE cQueryString     AS CHARACTER        NO-UNDO .
        
        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartSecurityAssignment":U THEN DO:
                ASSIGN cQueryString = ENTRY (1, THIS-OBJECT:FetchDataRequest:Queries, CHR(1)) . 
                
                IF cQueryString > "":U THEN DO:
                    oQueryParser = NEW QueryParser () . 
                    oQueryExpression = oQueryParser:ParseQueryString (cQueryString) .

                    /* No further need to filter on login company */                    
                    IF QueryExpression:HasExpressionForField ("eSmartSecurityAssignment.LoginCompanyGuid":U, oQueryExpression) THEN 
                        RETURN "FOR EACH SmartSecurityAssignment INDEXED-REPOSITION":U. 
                END.                
                
                ASSIGN cCompanyGuid = Consultingwerk.Framework.Session.SessionManager:LoginCompanyKey . 
                
                IF cCompanyGuid > "":U THEN 
                    RETURN SUBSTITUTE ("FOR EACH SmartSecurityAssignment WHERE SmartSecurityAssignment.LoginCompanyGuid = &1 OR SmartSecurityAssignment.LoginCompanyGuid = &2 INDEXED-REPOSITION":U,
                                       QUOTER (cCompanyGuid),
                                       QUOTER ("":U)) .
                ELSE  
                    RETURN "FOR EACH SmartSecurityAssignment INDEXED-REPOSITION":U.
            END.
        END CASE. 
        
    END METHOD.

END CLASS.
