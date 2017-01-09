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
    Purpose     : Data Access Component
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.05.2016 23:17:23
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Object/dsObjectMaster.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE BUFFER b_SmartObjectMaster FOR SmartObjectMaster .
    DEFINE PRIVATE BUFFER b_DataSource_SmartObjectMaster FOR SmartObjectMaster .

    DEFINE PRIVATE DATA-SOURCE src_SmartLink FOR SmartLink .
    DEFINE QUERY qry_SmartObjectInstanceSmartObje FOR SmartObjectInstance, b_SmartObjectMaster .
    DEFINE PRIVATE DATA-SOURCE src_SmartObjectInstanceSmartObje FOR QUERY qry_SmartObjectInstanceSmartObje .
    DEFINE QUERY qry_SmartObjectMasterSmartObject FOR SmartObjectMaster, SmartObjectType, SmartCustomizationResult, SmartModule, b_DataSource_SmartObjectMaster .
    DEFINE PRIVATE DATA-SOURCE src_SmartObjectMasterSmartObject FOR QUERY qry_SmartObjectMasterSmartObject .
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

        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartObjectMaster":U, "ObjectMasterGuid":U) .
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartLink":U, "LinkGuid":U) .
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartPage":U, "PageGuid":U) .
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartObjectInstance":U, "ObjectInstanceGuid":U) .

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
        THIS-OBJECT:AttachDataSource (BUFFER eSmartLink:HANDLE,
                                      DATA-SOURCE src_SmartLink:HANDLE, "
                                      LinkGuid,SmartLink.LinkGuid,
                                      ContainerObjectMasterGuid,SmartLink.ContainerObjectMasterGuid,
                                      LinkTypeGuid,SmartLink.LinkTypeGuid,
                                      LinkName,SmartLink.LinkName,
                                      SourceObjectInstanceGuid,SmartLink.SourceObjectInstanceGuid,
                                      TargetObjectInstanceGuid,SmartLink.TargetObjectInstanceGuid":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartObjectInstance:HANDLE,
                                      DATA-SOURCE src_SmartObjectInstanceSmartObje:HANDLE, "
                                      ObjectInstanceGuid,SmartObjectInstance.ObjectInstanceGuid,
                                      ContainerObjectMasterGuid,SmartObjectInstance.ContainerObjectMasterGuid,
                                      ObjectMasterGuid,SmartObjectInstance.ObjectMasterGuid,
                                      ObjectMasterName,b_SmartObjectMaster.ObjectName,
                                      ParentInstanceGuid,SmartObjectInstance.ParentInstanceGuid,
                                      LayoutPosition,SmartObjectInstance.LayoutPosition,
                                      InstanceName,SmartObjectInstance.InstanceName,
                                      InstanceDescription,SmartObjectInstance.InstanceDescription,
                                      PageGuid,SmartObjectInstance.PageGuid,
                                      ObjectSequence,SmartObjectInstance.ObjectSequence":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartObjectMaster:HANDLE,
                                      DATA-SOURCE src_SmartObjectMasterSmartObject:HANDLE, "
                                      ObjectMasterGuid,SmartObjectMaster.ObjectMasterGuid,
                                      ObjectName,SmartObjectMaster.ObjectName,
                                      CustomizationResultGuid,SmartObjectMaster.CustomizationResultGuid,
                                      CustomizationResultCode,SmartCustomizationResult.CustomizationResultCode,
                                      ObjectTypeGuid,SmartObjectMaster.ObjectTypeGuid,
                                      ObjectTypeName,SmartObjectType.ObjectTypeName,
                                      ModuleGuid,SmartObjectMaster.ModuleGuid,
                                      ModuleName,SmartModule.ModuleName,
                                      ObjectDescription,SmartObjectMaster.ObjectDescription,
                                      ObjectPackage,SmartObjectMaster.ObjectPackage,
                                      ObjectExtension,SmartObjectMaster.ObjectExtension,
                                      StaticObject,SmartObjectMaster.StaticObject,
                                      TemplateObject,SmartObjectMaster.TemplateObject,
                                      DeploymentType,SmartObjectMaster.DeploymentType,
                                      RunnableFromMenu,SmartObjectMaster.RunnableFromMenu,
                                      Disabled,SmartObjectMaster.Disabled,
                                      DesignTimeDataSourceGuid,SmartObjectMaster.DesignTimeDataSourceGuid,
                                      DesignTimeDataSourceName,b_DataSource_SmartObjectMaster.ObjectName,
                                      ContainerType,SmartObjectType.ContainerType":U) .
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

    /**
     * Purpose: This procedure is called from saveChanges in the DA
     *          to do the actual database commit and any transaction-related
     *          validation logic for a buffer.
     * Notes:
     * @param phBuffer The handle of the buffer to commit
     */
    METHOD OVERRIDE PROTECTED VOID CommitChanges (phBuffer AS HANDLE):

        DEFINE VARIABLE hBeforeBuff  AS HANDLE    NO-UNDO .

        SUPER:CommitChanges(phBuffer) .

        IF phBuffer:BEFORE-BUFFER:ERROR THEN
            RETURN .

        ASSIGN hBeforeBuff = phBuffer:BEFORE-BUFFER .

        IF hBeforeBuff:ROW-STATE = ROW-DELETED AND phBuffer:NAME = "eSmartObjectMaster":U THEN DO:

            THIS-OBJECT:ResetModifiedState ("SmartObjectMaster":U,
                                            hBeforeBuff::ObjectMasterGuid) .
        END.
        ELSE IF hBeforeBuff:ROW-STATE = ROW-DELETED THEN DO:
            THIS-OBJECT:TrackModifiedState ("SmartObjectMaster":U,
                                            hBeforeBuff::ContainerObjectMasterGuid) .
        END.
        ELSE IF hBeforeBuff:ROW-STATE = ROW-MODIFIED OR hBeforeBuff:ROW-STATE = ROW-CREATED THEN DO:

            phBuffer:FIND-BY-ROWID (hBeforeBuff:AFTER-ROWID) .

            IF phBuffer:NAME = "eSmartObjectMaster":U THEN
                THIS-OBJECT:TrackModifiedState ("SmartObjectMaster":U,
                                                phBuffer::ObjectMasterGuid) .
            ELSE
                THIS-OBJECT:TrackModifiedState ("SmartObjectMaster":U,
                                                phBuffer::ContainerObjectMasterGuid) .
        END.

    END METHOD .

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
        BUFFER eSmartLink:DETACH-DATA-SOURCE () .
        BUFFER eSmartObjectInstance:DETACH-DATA-SOURCE () .
        BUFFER eSmartObjectMaster:DETACH-DATA-SOURCE () .
        BUFFER eSmartPage:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the object master is a child of the given class
        Notes:
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ObjectIsA (poParameter AS ObjectIsAParameter):

        DEFINE VARIABLE oClassIsAParameter AS ClassIsAParameter NO-UNDO .
        DEFINE BUFFER b_ObjectMaster FOR SmartObjectMaster .

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        IF NOT CAN-FIND (FIRST SmartObjectMaster WHERE SmartObjectMaster.ObjectMasterGuid = poParameter:ObjectName) AND
           NOT CAN-FIND (FIRST SmartObjectMaster WHERE SmartObjectMaster.ObjectName = poParameter:ObjectName) THEN
            UNDO, THROW NEW InvalidObjectMasterException (poParameter:ObjectName) .

        FIND b_ObjectMaster WHERE b_ObjectMaster.ObjectName = poParameter:ObjectName NO-LOCK NO-ERROR .

        IF NOT AVAILABLE b_ObjectMaster THEN
            FIND b_ObjectMaster WHERE b_ObjectMaster.ObjectMasterGuid = poParameter:ObjectName NO-LOCK  .

        oClassIsAParameter = NEW ClassIsAParameter(b_ObjectMaster.ObjectTypeGuid, poParameter:ClassName) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U,
                                       "ClassIsA":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oClassIsAParameter) .

        poParameter:IsA = oClassIsAParameter:IsA .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
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
            WHEN "eSmartObjectMaster":U THEN
                cMap = DATA-SOURCE src_SmartObjectMasterSmartObject:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartPage":U THEN
                cMap = DATA-SOURCE src_SmartPage:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartObjectInstance":U THEN
                cMap = DATA-SOURCE src_SmartObjectInstanceSmartObje:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartLink":U THEN
                cMap = DATA-SOURCE src_SmartLink:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
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

        DEFINE VARIABLE oRepositoryService AS ISmartRepositoryService NO-UNDO .
        DEFINE VARIABLE cQueryString       AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cClasses           AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                 NO-UNDO .

        IF pcTable = "eSmartObjectMaster":U AND VALID-OBJECT (THIS-OBJECT:FetchDataRequest:NamedQuery) THEN DO:

            oRepositoryService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.ISmartRepositoryService "NEW SmartRepositoryService()"} .

            ASSIGN cQueryString = "FOR EACH SmartObjectMaster":U
                   cClasses     = oRepositoryService:GetChildClasses(THIS-OBJECT:FetchDataRequest:NamedQuery:Parameters:GetParameterValueAsCharacter("ObjectTypes":U)).

            DO i = 1 TO NUM-ENTRIES (cClasses):

                ASSIGN cQueryString = cQueryString + (IF i = 1 THEN " WHERE (":U ELSE " or ":U ) +
                                      SUBSTITUTE ("SmartObjectMaster.ObjectTypeGuid = &1":U, QUOTER (ENTRY (i, cClasses))) .

            END.

            ASSIGN cQueryString = cQueryString + ") , ~
    FIRST SmartObjectType WHERE SmartObjectType.ObjectTypeGuid = SmartObjectMaster.ObjectTypeGuid, ~
    FIRST SmartCustomizationResult WHERE SmartCustomizationResult.CustomizationResultGuid = SmartObjectMaster.CustomizationResultGuid OUTER-JOIN, ~
    FIRST SmartModule WHERE SmartModule.ModuleGuid = SmartObjectMaster.ModuleGuid, ~
    FIRST b_DataSource_SmartObjectMaster WHERE b_DataSource_SmartObjectMaster.ObjectMasterGuid = SmartObjectMaster.DesignTimeDataSourceGuid OUTER-JOIN":U .

            RETURN cQueryString .

        END.

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartObjectMaster":U THEN
                RETURN "FOR EACH SmartObjectMaster, ~
    FIRST SmartObjectType WHERE SmartObjectType.ObjectTypeGuid = SmartObjectMaster.ObjectTypeGuid, ~
    FIRST SmartCustomizationResult WHERE SmartCustomizationResult.CustomizationResultGuid = SmartObjectMaster.CustomizationResultGuid OUTER-JOIN, ~
    FIRST SmartModule WHERE SmartModule.ModuleGuid = SmartObjectMaster.ModuleGuid, ~
    FIRST b_DataSource_SmartObjectMaster WHERE b_DataSource_SmartObjectMaster.ObjectMasterGuid = SmartObjectMaster.DesignTimeDataSourceGuid OUTER-JOIN":U .
            WHEN "eSmartPage":U THEN
                RETURN "FOR EACH SmartPage":U.
            WHEN "eSmartObjectInstance":U THEN
                RETURN "FOR EACH SmartObjectInstance, FIRST b_SmartObjectMaster OF SmartObjectInstance":U.
            WHEN "eSmartLink":U THEN
                RETURN "FOR EACH SmartLink":U.
        END CASE .

    END METHOD.

END CLASS.
