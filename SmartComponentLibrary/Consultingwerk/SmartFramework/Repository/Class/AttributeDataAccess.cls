/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AttributeDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 12:50:30 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeDataAccess 
    INHERITS SmartDataAccess: 

    { Consultingwerk/SmartFramework/Repository/Class/dsAttribute.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE QUERY qry_SmartAttributeSmartAttribute FOR SmartAttribute, SmartAttributeGroup .
    DEFINE PRIVATE DATA-SOURCE src_SmartAttributeSmartAttribute FOR QUERY qry_SmartAttributeSmartAttribute .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN 
            THIS-OBJECT:AssignSkipList = "AttributeGuid":U.

        THIS-OBJECT:TrackDeletions = TRUE . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttribute:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        BUFFER eSmartAttribute:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartAttributeSmartAttribute:HANDLE, "AttributeGuid,SmartAttribute.AttributeGuid,AttributeGroupGuid,SmartAttribute.AttributeGroupGuid,AttributeLabel,SmartAttribute.AttributeLabel,TechnicalName,SmartAttribute.TechnicalName,AttributeDesription,SmartAttribute.AttributeDesription,RuntimeOnly,SmartAttribute.RuntimeOnly,VirtualProperty,SmartAttribute.VirtualProperty,ConstantLevel,SmartAttribute.ConstantLevel,LookupType,SmartAttribute.LookupType,LookupValues,SmartAttribute.LookupValues,PropertyType,SmartAttribute.PropertyType,RepositoryType,SmartAttribute.RepositoryType,SetServiceType,SmartAttribute.SetServiceType,PropertyOrEvent,SmartAttribute.PropertyOrEvent,AttributeGroupName,SmartAttributeGroup.AttributeGroupName":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsAttribute DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsAttribute BIND):
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

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttribute:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eSmartAttribute:DETACH-DATA-SOURCE () .        
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Logic performed after storing a new record in the db
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID eSmartAttributeCreateEndTrans ():
		
        ReassignAttributeGroupNames ().
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Logic performed after changing a record in the db
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartAttributeModifyEndTrans ():
        
        ReassignAttributeGroupNames ().
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Update the AttributeGroupName field for all records
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ReassignAttributeGroupNames ():
		
        FOR EACH eSmartAttribute ON ERROR UNDO, THROW:
        
            FIND FIRST SmartAttributeGroup WHERE SmartAttributeGroup.AttributeGroupGuid = eSmartAttribute.AttributeGroupGuid NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE SmartAttributeGroup THEN NEXT.
            
            eSmartAttribute.AttributeGroupName = SmartAttributeGroup.AttributeGroupName.
        END.
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
            WHEN "eSmartAttribute":U THEN
                cMap = DATA-SOURCE src_SmartAttributeSmartAttribute:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

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
            WHEN "eSmartAttribute":U THEN
                RETURN "FOR EACH SmartAttribute, FIRST SmartAttributeGroup OF SmartAttribute INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
