/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectMasterDataAccess
    Purpose     : Data Access component for Exporting and Importing ObjectMaster
                  data to .smartrepo fields
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 15:55:45
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.Export.* FROM PROPATH .
USING Consultingwerk.OERA.*                                    FROM PROPATH .
USING Consultingwerk.Util.*                                    FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Object/Export/dsObjectMaster.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartAttributeValue FOR SmartAttributeValue .
    DEFINE PRIVATE DATA-SOURCE src_SmartLink FOR SmartLink .
    DEFINE PRIVATE DATA-SOURCE src_SmartModifiedState FOR SmartModifiedState .
    DEFINE PRIVATE DATA-SOURCE src_SmartObjectInstance FOR SmartObjectInstance .
    DEFINE PRIVATE DATA-SOURCE src_SmartObjectMaster FOR SmartObjectMaster .
    DEFINE PRIVATE DATA-SOURCE src_SmartPage FOR SmartPage .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* THIS-OBJECT:AssignSkipList = "<key fields>":U. */


        /* Mike Fechner, Consultingwerk Ltd. 01.07.2014
           SCL-334: By default, prepare queries of joined data source
                    queries. This behavior is the new default, and should
                    only be set to FALSE, when customers are already manipulating
                    the data-source query in code (e.g. QUERY-PREPARE in a BEFORE-FILL
                    call back) */
        THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE .

        /* Allows to force applying the Source Default Query to child tables
           even when no filter expression was provided by the consumer */
        THIS-OBJECT:ForceApplyQueryOnChildTables = TRUE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectMaster:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartAttributeValue:HANDLE,
                                      DATA-SOURCE src_SmartAttributeValue:HANDLE, "
                                      AttributeValueGuid,SmartAttributeValue.AttributeValueGuid,
                                      ObjectTypeGuid,SmartAttributeValue.ObjectTypeGuid,
                                      ObjectMasterGuid,SmartAttributeValue.ObjectMasterGuid,
                                      ContainerObjectMasterGuid,SmartAttributeValue.ContainerObjectMasterGuid,
                                      ObjectInstanceGuid,SmartAttributeValue.ObjectInstanceGuid,
                                      ConstantValue,SmartAttributeValue.ConstantValue,
                                      AttributeLabel,SmartAttributeValue.AttributeLabel,
                                      CharacterValue,SmartAttributeValue.CharacterValue,
                                      IntegerValue,SmartAttributeValue.IntegerValue,
                                      Int64Value,SmartAttributeValue.Int64Value,
                                      DateValue,SmartAttributeValue.DateValue,
                                      DateTimeValue,SmartAttributeValue.DateTimeValue,
                                      DateTimeTzValue,SmartAttributeValue.DateTimeTzValue,
                                      DecimalValue,SmartAttributeValue.DecimalValue,
                                      LogicalValue,SmartAttributeValue.LogicalValue,
                                      RawValue,SmartAttributeValue.RawValue,
                                      AppliesAtRuntime,SmartAttributeValue.AppliesAtRuntime":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartLink:HANDLE,
                                      DATA-SOURCE src_SmartLink:HANDLE, "
                                      LinkGuid,SmartLink.LinkGuid,
                                      ContainerObjectMasterGuid,SmartLink.ContainerObjectMasterGuid,
                                      LinkTypeGuid,SmartLink.LinkTypeGuid,
                                      LinkName,SmartLink.LinkName,
                                      SourceObjectInstanceGuid,SmartLink.SourceObjectInstanceGuid,
                                      TargetObjectInstanceGuid,SmartLink.TargetObjectInstanceGuid":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartModifiedState:HANDLE,
                                      DATA-SOURCE src_SmartModifiedState:HANDLE, "
                                      ModifiedStateGuid,SmartModifiedState.ModifiedStateGuid,
                                      ModifiedStateTable,SmartModifiedState.ModifiedStateTable,
                                      ModifiedStateKeyFieldValues,SmartModifiedState.ModifiedStateKeyFieldValues,
                                      ModifiedStateTimeStamp,SmartModifiedState.ModifiedStateTimeStamp,
                                      ModifiedStateUser,SmartModifiedState.ModifiedStateUser":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartObjectInstance:HANDLE,
                                      DATA-SOURCE src_SmartObjectInstance:HANDLE, "
                                      ObjectInstanceGuid,SmartObjectInstance.ObjectInstanceGuid,
                                      ContainerObjectMasterGuid,SmartObjectInstance.ContainerObjectMasterGuid,
                                      ObjectMasterGuid,SmartObjectInstance.ObjectMasterGuid,
                                      ParentInstanceGuid,SmartObjectInstance.ParentInstanceGuid,
                                      LayoutPosition,SmartObjectInstance.LayoutPosition,
                                      InstanceName,SmartObjectInstance.InstanceName,
                                      InstanceDescription,SmartObjectInstance.InstanceDescription,
                                      PageGuid,SmartObjectInstance.PageGuid,
                                      ObjectSequence,SmartObjectInstance.ObjectSequence":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartObjectMaster:HANDLE,
                                      DATA-SOURCE src_SmartObjectMaster:HANDLE, "
                                      ObjectMasterGuid,SmartObjectMaster.ObjectMasterGuid,
                                      ObjectName,SmartObjectMaster.ObjectName,
                                      CustomizationResultGuid,SmartObjectMaster.CustomizationResultGuid,
                                      ObjectTypeGuid,SmartObjectMaster.ObjectTypeGuid,
                                      ModuleGuid,SmartObjectMaster.ModuleGuid,
                                      ObjectDescription,SmartObjectMaster.ObjectDescription,
                                      ObjectPackage,SmartObjectMaster.ObjectPackage,
                                      ObjectExtension,SmartObjectMaster.ObjectExtension,
                                      StaticObject,SmartObjectMaster.StaticObject,
                                      TemplateObject,SmartObjectMaster.TemplateObject,
                                      DeploymentType,SmartObjectMaster.DeploymentType,
                                      RunnableFromMenu,SmartObjectMaster.RunnableFromMenu,
                                      Disabled,SmartObjectMaster.Disabled,
                                      DesignTimeDataSourceGuid,SmartObjectMaster.DesignTimeDataSourceGuid":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartPage:HANDLE,
                                      DATA-SOURCE src_SmartPage:HANDLE, "
                                      PageGuid,SmartPage.PageGuid,
                                      ContainerObjectMasterGuid,SmartPage.ContainerObjectMasterGuid,
                                      PageSequence,SmartPage.PageSequence,
                                      PageLabel,SmartPage.PageLabel,
                                      SecurityToken,SmartPage.SecurityToken,
                                      EnableOnCreate,SmartPage.EnableOnCreate,
                                      EnableOnModify,SmartPage.EnableOnModify,
                                      EnableOnView,SmartPage.EnableOnView":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsObjectMaster DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsObjectMaster BIND):
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
                                                "eCustomerAfterFill":U) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsObjectMaster:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartAttributeValue:DETACH-DATA-SOURCE () .
        BUFFER eSmartLink:DETACH-DATA-SOURCE () .
        BUFFER eSmartModifiedState:DETACH-DATA-SOURCE () .
        BUFFER eSmartObjectInstance:DETACH-DATA-SOURCE () .
        BUFFER eSmartObjectMaster:DETACH-DATA-SOURCE () .
        BUFFER eSmartPage:DETACH-DATA-SOURCE () .
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
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartAttributeValue":U THEN
                cMap = DATA-SOURCE src_SmartAttributeValue:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartLink":U THEN
                cMap = DATA-SOURCE src_SmartLink:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartModifiedState":U THEN
                cMap = DATA-SOURCE src_SmartModifiedState:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartObjectInstance":U THEN
                cMap = DATA-SOURCE src_SmartObjectInstance:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartObjectMaster":U THEN
                cMap = DATA-SOURCE src_SmartObjectMaster:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartPage":U THEN
                cMap = DATA-SOURCE src_SmartPage:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

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
            WHEN "eSmartAttributeValue":U THEN
                RETURN "FOR EACH SmartAttributeValue":U.
            WHEN "eSmartLink":U THEN
                RETURN "FOR EACH SmartLink":U.
            WHEN "eSmartModifiedState":U THEN
                RETURN "for each SmartModifiedState where SmartModifiedState.ModifiedStateTable = ~"SmartObjectMaster~"":U.
            WHEN "eSmartObjectInstance":U THEN
                RETURN "FOR EACH SmartObjectInstance":U.
            WHEN "eSmartObjectMaster":U THEN
                RETURN "FOR EACH SmartObjectMaster":U.
            WHEN "eSmartPage":U THEN
                RETURN "FOR EACH SmartPage":U.

        END CASE .

    END METHOD.

END CLASS.
