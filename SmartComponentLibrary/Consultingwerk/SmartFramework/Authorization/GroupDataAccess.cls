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
    File        : GroupDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:36:35 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH  .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.*    FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.GroupDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/Authorization/dsGroup.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE DATA-SOURCE src_SmartGroup FOR SmartGroup .


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GroupDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GroupDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
            THIS-OBJECT:AssignSkipList = "GroupGuid":U.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsGroup:HANDLE, FALSE) .
        
        BUFFER eSmartGroup:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartGroup:HANDLE, "GroupGuid,SmartGroup.GroupGuid,GroupName,SmartGroup.GroupName,ParentGroupGuid,SmartGroup.ParentGroupGuid,LoginCompanyGuid,SmartGroup.LoginCompanyGuid":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor  
        @param dsGroup DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsGroup BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
        BUFFER eSmartGroup:SET-CALLBACK ("AFTER-FILL":U, "eSmartGroupAfterFill":U, THIS-OBJECT).
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsGroup:HANDLE, FALSE) .
    
        BUFFER eSmartGroup:DETACH-DATA-SOURCE () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populate calculated fields
        Notes:   
        @param dsGroup Input of the dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartGroupAfterFill (DATASET dsGroup):
        
        DEFINE BUFFER bufSmartGroup        FOR SmartGroup.
        DEFINE BUFFER bufSmartLoginCompany FOR SmartLoginCompany .
        
        FOR EACH eSmartGroup EXCLUSIVE-LOCK:

            IF eSmartGroup.ParentGroupGuid > "":U THEN DO:              
                FIND FIRST bufSmartGroup WHERE bufSmartGroup.GroupGuid = eSmartGroup.ParentGroupGuid NO-LOCK NO-ERROR.
                
                IF AVAILABLE (bufSmartGroup) THEN DO:
                    IF bufSmartGroup.LoginCompanyGuid > "":U THEN DO:
                        FIND FIRST bufSmartLoginCompany WHERE bufSmartLoginCompany.LoginCompanyGuid = bufSmartGroup.LoginCompanyGuid NO-LOCK NO-ERROR.
                        
                        IF AVAILABLE (bufSmartLoginCompany) THEN 
                            ASSIGN eSmartGroup.ParentGroupName = SUBSTITUTE ("&1 (&2)":U, bufSmartGroup.GroupName, bufSmartLoginCompany.LoginCompanyName) .
                        ELSE 
                            ASSIGN eSmartGroup.ParentGroupName = SUBSTITUTE ("&1 (???)":U, bufSmartGroup.GroupName) .
                    END.
                    ELSE 
                        ASSIGN eSmartGroup.ParentGroupName = bufSmartGroup.GroupName.
                END.
                ELSE 
                    ASSIGN eSmartGroup.ParentGroupName = "???":U . 
                
                RELEASE bufSmartGroup.
            END.
            
            FIND FIRST bufSmartLoginCompany WHERE bufSmartLoginCompany.LoginCompanyGuid = eSmartGroup.LoginCompanyGuid NO-LOCK NO-ERROR.
            
            IF AVAILABLE (bufSmartLoginCompany) THEN 
                ASSIGN eSmartGroup.LoginCompanyName = bufSmartLoginCompany.LoginCompanyName.
            
            RELEASE bufSmartLoginCompany.
            
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validation method for eSmartGroup
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID eSmartGroupModifyEndTrans ():
        
        DEFINE VARIABLE oParameter AS GetParentGroupsParameter NO-UNDO . 
        DEFINE VARIABLE cEntityKey AS CHARACTER                NO-UNDO .
        
        DEFINE VARIABLE roRowid    AS ROWID                    NO-UNDO . 
        
        Consultingwerk.Util.BufferHelper:FindAfterBuffer (BUFFER eSmartGroupBefore:HANDLE) .
    
        /* SCL-1036: Existing groups: prevent getting it's own ancestor */
        IF eSmartGroup.GroupGuid > "":U AND eSmartGroup.ParentGroupGuid > "":U THEN DO ON ERROR UNDO, THROW:
            
            oParameter = NEW GetParentGroupsParameter (eSmartGroup.ParentGroupGuid) . 

            cEntityKey = SUBSTITUTE ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity&1&2":U, 
                                     CHR (1), GUID) . 

            roRowid = ROWID (eSmartGroup) .

            ServiceInterface:InvokeMethod (cEntityKey,  
                                           "GetParentGroups":U,
                                           INPUT-OUTPUT DATASET dsGroup,
                                           oParameter) .
                                
            IF ListHelper:EntryIsInList (oParameter:GroupGuid, 
                                         oParameter:ParentGroupGuids) THEN                                              
                UNDO, THROW NEW CircularGroupReferenceException ("A group cannot be it's own ancestor."{&TRAN},
                                                                 0) .
                
            CATCH err AS CircularGroupReferenceException:
                UNDO, THROW NEW CircularGroupReferenceException ("A group cannot be it's own ancestor."{&TRAN},
                                                                 0) .
            END CATCH.
            
            FINALLY:
                ServiceManager:StopBusinessService (cEntityKey) .       
            END FINALLY.

        END.     
        
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
            WHEN "eSmartGroup":U THEN
                cMap = DATA-SOURCE src_SmartGroup:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
            WHEN "eSmartGroup":U THEN
                RETURN "FOR EACH SmartGroup INDEXED-REPOSITION":U. 

        END. 
    END METHOD.

END CLASS.
