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
    File        : MenuGroupDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:37:21 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.MenuGroupDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE DATA-SOURCE src_SmartMenuGroup FOR SmartMenuGroup .

    DEFINE PRIVATE VARIABLE cCurrentMenuGuid AS CHARACTER NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuGroupDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsMenuGroup:HANDLE, FALSE) .
        
        BUFFER eSmartMenuGroup:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartMenuGroup:HANDLE, "MenuGroupGuid,SmartMenuGroup.MenuGroupGuid,MenuGuid,SmartMenuGroup.MenuGuid,GroupGuid,SmartMenuGroup.GroupGuid":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor                                                                        
        @param dsMenuGroup DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsMenuGroup BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

        BUFFER eSmartMenuGroup:SET-CALLBACK ("AFTER-FILL":U, "eSmartMenuGroupAfterFill":U, THIS-OBJECT).
        BUFFER ePossibleGroup:SET-CALLBACK ("BEFORE-FILL":U, "ePossibleGroupBeforeFill":U, THIS-OBJECT).
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsMenuGroup:HANDLE, FALSE) .
    
        BUFFER eSmartMenuGroup:DETACH-DATA-SOURCE () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populate calculated fields
        Notes:   
        @param dsMenuGroup Input of the dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartMenuGroupAfterFill (DATASET dsMenuGroup):

        FOR EACH eSmartMenuGroup NO-LOCK:
            FIND FIRST SmartGroup WHERE SmartGroup.GroupGuid = eSmartMenuGroup.GroupGuid NO-LOCK NO-ERROR.
            
            IF AVAILABLE (SmartGroup) THEN DO:
                ASSIGN eSmartMenuGroup.GroupName= SmartGroup.GroupName.
                
                FIND FIRST SmartLoginCompany WHERE SmartLoginCompany.LoginCompanyGuid = SmartGroup.LoginCompanyGuid NO-LOCK NO-ERROR.
                
                IF AVAILABLE (SmartLoginCompany) THEN
                    ASSIGN eSmartMenuGroup.LoginCompanyName = SmartLoginCompany.LoginCompanyName.
                
                RELEASE SmartLoginCompany.
            END.
            
            RELEASE SmartUser.
            
        END.
        
        FOR EACH SmartGroup NO-LOCK:
            IF CAN-FIND (FIRST eSmartMenuGroup WHERE eSmartMenuGroup.GroupGuid = SmartGroup.GroupGuid) THEN NEXT.
            
            CREATE ePossibleGroup.
            BUFFER-COPY SmartGroup TO ePossibleGroup.
            
            ASSIGN 
                ePossibleGroup.MenuGroupGuid = GUID
                ePossibleGroup.MenuGuid      = cCurrentMenuGuid
                .

            FIND FIRST SmartLoginCompany WHERE SmartLoginCompany.LoginCompanyGuid = SmartGroup.LoginCompanyGuid NO-LOCK NO-ERROR.
            
            IF AVAILABLE (SmartLoginCompany) THEN
                ASSIGN ePossibleGroup.LoginCompanyName = SmartLoginCompany.LoginCompanyName.
            
            RELEASE SmartLoginCompany.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Call-Back method for the BEFORE-FILL event for the ePossibleGroup 
                 temp-table
        Notes:   Do nothing, just avoid errors from fil of temp-table without attached
                 source for the ePossibleGroup table
        @param dsMenuGroup The dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ePossibleGroupBeforeFill (DATASET dsMenuGroup):
        
        /* noop */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Extracts MenuGuid from the FetchDataRequest data to be able to 
                 fill the ePossibleGroup table.
        Notes:   
        @param poFetchDataRequest RequestData for the current FetchData process
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):
        
        IF LOOKUP ("eSmartMenuGroup.MenuGuid":U, poFetchDataRequest:Queries, " ":U) > 0 THEN 
            cCurrentMenuGuid = ENTRY (LOOKUP ("eSmartMenuGroup.MenuGuid":U, 
                                               poFetchDataRequest:Queries, 
                                               " ":U) + 2, 
                                       poFetchDataRequest:Queries, 
                                       " ":U).
        ELSE 
            cCurrentMenuGuid = "":U.
        
        cCurrentMenuGuid = TRIM (cCurrentMenuGuid, '"':U).
        
        SUPER:FetchData(INPUT poFetchDataRequest).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Values from ePossibleGroup will not be stored in the DB
        Notes:   
        @param phBuffer HANDLE of the BUFFER to be stored
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID SaveBuffer (INPUT phBuffer AS HANDLE):
        
        IF phBuffer:NAME = "ePossibleGroup":U THEN RETURN.

        SUPER:SaveBuffer(INPUT phBuffer).

    END METHOD .

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
            WHEN "eSmartMenuGroup":U THEN
                cMap = DATA-SOURCE src_SmartMenuGroup:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
            WHEN "eSmartMenuGroup":U THEN
                RETURN "FOR EACH SmartMenuGroup INDEXED-REPOSITION":U. 

        END. 
    END METHOD.

END CLASS.
