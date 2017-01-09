/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectTypeDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.10.2015 01:55:45 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Progress.Lang.*                                  FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH.

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ObjectTypeDataAccess 
    INHERITS SmartDataAccess
    USE-WIDGET-POOL: 

    { Consultingwerk/SmartFramework/Repository/Class/dsObjectType.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartObjectType FOR SmartObjectType .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectTypeDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectTypeDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
         THIS-OBJECT:AssignSkipList = "ObjectTypeGuid":U.

        /* Mike Fechner, Consultingwerk Ltd. 01.07.2014
           SCL-334: By default, prepare queries of joined data source 
                    queries. This behavior is the new default, and should
                    only be set to FALSE, when customers are already manipulating
                    the data-source query in code (e.g. QUERY-PREPARE in a BEFORE-FILL
                    call back) */
        THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE . 
        
        /* Allows to force applying the Source Default Query to child tables
           even when no filter expression was provided by the consumer */
        /* THIS-OBJECT:ForceApplyQueryOnChildTables = FALSE . */ 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectType:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartObjectType:HANDLE,
                                      DATA-SOURCE src_SmartObjectType:HANDLE, "
                                      ObjectTypeGuid,SmartObjectType.ObjectTypeGuid,
                                      ObjectTypeName,SmartObjectType.ObjectTypeName,
                                      ObjectTypeDescription,SmartObjectType.ObjectTypeDescription,
                                      DeploymentType,SmartObjectType.DeploymentType,
                                      StaticObject,SmartObjectType.StaticObject,
                                      ExtendsObjectTypeGuid,SmartObjectType.ExtendsObjectTypeGuid,
                                      CustomObjectTypeGuid,SmartObjectType.CustomObjectTypeGuid,
                                      TechnicalClassName,SmartObjectType.TechnicalClassName,
                                      ClassTypeGuid,SmartObjectType.ClassTypeGuid,
                                      ContainerType,SmartObjectType.ContainerType,
                                      StoreInstances,SmartObjectType.StoreInstances":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsObjectType DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsObjectType BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData, note that the callback methods need to be 
                 PUBLIC and need to have the Dataset defined as an INPUT Parameter.
                 Errors thrown from or raised in the FILL call-back methods of the 
                 DataAccess class (SET-CALLBACK) will stop the Dataset from getting 
                 filled / filled further. However errors thrown from the call-back 
                 methods are not thrown from the FILL() method further up into a 
                 catch block or to the client.  
                 See http://knowledgebase.progress.com/articles/Article/P188375
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectType:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartObjectType:DETACH-DATA-SOURCE () .        
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines the list of parent classes for the given class
        Notes:   The poPaarmeter:ObjectTypeName may contain an ObjectTypeName or ObjectTypeGuid
                 The search is first performed based on the ObjectTypeName 
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetParentClasses (poParameter AS GetParentClassesParameter):

        DEFINE BUFFER SmartObjectType FOR SmartObjectType .
        
        DEFINE VARIABLE cObjectTypeGuid AS CHARACTER NO-UNDO.

        ASSIGN poParameter:ParentObjectTypeGuids = "":U
               poParameter:ParentObjectTypeNames = "":U .

        FIND SmartObjectType WHERE SmartObjectType.ObjectTypeName = poParameter:ObjectTypeName NO-LOCK NO-ERROR . 
        
        IF NOT AVAILABLE (SmartObjectType) THEN 
            FIND SmartObjectType WHERE SmartObjectType.ObjectTypeGuid = poParameter:ObjectTypeName NO-LOCK NO-ERROR . 
        
        BufferAssert:IsAvailable (BUFFER SmartObjectType:HANDLE) .
        
        ASSIGN cObjectTypeGuid = SmartObjectType.ExtendsObjectTypeGuid .
        
        DO WHILE cObjectTypeGuid > "":U: 
            FIND SmartObjectType WHERE SmartObjectType.ObjectTypeGuid = cObjectTypeGuid NO-LOCK NO-ERROR . 
        
            IF NOT AVAILABLE SmartObjectType THEN 
                LEAVE . 
        
            ASSIGN poParameter:ParentObjectTypeGuids = poParameter:ParentObjectTypeGuids + 
                                                       (IF poParameter:ParentObjectTypeGuids > "":U THEN CHR(1) ELSE "":U) + 
                                                       SmartObjectType.ObjectTypeGuid 
                   poParameter:ParentObjectTypeNames = poParameter:ParentObjectTypeNames + 
                                                       (IF poParameter:ParentObjectTypeNames > "":U THEN ",":U ELSE "":U) + 
                                                       SmartObjectType.ObjectTypeName  
        
                   cObjectTypeGuid = SmartObjectType.ExtendsObjectTypeGuid .
        END.
        
        FINALLY:
            ErrorHelper:ResetErrorStatus () .		
        END FINALLY.
     
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
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .
            
        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartObjectType":U THEN
                cMap = DATA-SOURCE src_SmartObjectType:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

        END.
     
        ASSIGN iPos = LOOKUP(pcColumn, cMap) .
        
        IF iPos MODULO 2 = 1 THEN  
            RETURN ENTRY(LOOKUP(pcColumn,cMap) + 1,cMap).    
        ELSE 
            RETURN "":U . 

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
            WHEN "eSmartObjectType":U THEN
                RETURN "FOR EACH SmartObjectType INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
