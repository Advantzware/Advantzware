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
    File        : ObjectMasterBusinessEntity
    Purpose     : Business Entity for SmartObjectMaster
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.05.2016 23:17:23
    Notes       : When the Custom Context of the IFetchDataRequest instance
                  contains the "SkipCalculatedFields" entry, no calculated
                  fields will be populated
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.SmartFramework.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*      FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

@BusinessEntityTable (name="eSmartObjectMaster", MandatoryColumns="ObjectName", DefaultSort="BY eSmartObjectMaster.ObjectName") .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Object/dsObjectMaster.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ObjectMasterBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterBusinessEntity ():
        SUPER (DATASET dsObjectMaster:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDataAccess":U .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns an ObjectInstanceDetails instance with the details about the
     *          object instance provided by it's guid
     * Notes:
     * @param pcObjectInstanceGuid The guid of the object instance to return details from
     * @return The ObjectInstanceDetails structure with the details about the object instance
     */
    METHOD PROTECTED ObjectInstanceDetails GetObjectInstanceDetails (pcObjectInstanceGuid AS CHARACTER):

        DEFINE VARIABLE oReturn   AS ObjectInstanceDetails    NO-UNDO .
        DEFINE VARIABLE oObject   AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE cPageGuid AS CHARACTER                NO-UNDO .

        DEFINE BUFFER b_eSmartObjectInstance FOR eSmartObjectInstance .
        DEFINE BUFFER b_eSmartPage           FOR eSmartPage .

        ASSIGN oReturn = NEW ObjectInstanceDetails () .

        /* Populate Link instance details */
        IF pcObjectInstanceGuid = "":U THEN DO:
            ASSIGN oReturn:InstanceName       = "<Container>":U
                   oReturn:InstancePageNumber = 0 .
        END.
        ELSE DO:
            FIND b_eSmartObjectInstance WHERE b_eSmartObjectInstance.ObjectInstanceGuid = eSmartLink.SourceObjectInstanceGuid
                NO-ERROR .

            IF AVAILABLE b_eSmartObjectInstance THEN DO:
                ASSIGN oReturn:InstanceName = b_eSmartObjectInstance.InstanceName .

                IF b_eSmartObjectInstance.PageGuid = "":U THEN
                    oReturn:InstancePageNumber = 0 .
                ELSE DO:
                    FIND b_eSmartPage WHERE b_eSmartPage.PageGuid = b_eSmartObjectInstance.PageGuid NO-ERROR .

                    IF AVAILABLE b_eSmartPage THEN
                        ASSIGN oReturn:InstancePageNumber = b_eSmartPage.PageSequence .
                    ELSE DO:
                        oObject = NEW ObjectMasterDatasetModel () .
                        oObject:UseDedicatedBusinessEntity = TRUE .

                        oObject:SmartPage:Filter:PageGuid:EQ (b_eSmartObjectInstance.PageGuid):Run () .

                        ASSIGN oReturn:InstancePageNumber = oObject:SmartPage:PageSequenceWhenAvailable(?).
                    END.
                END.
            END.
            ELSE DO:
                oObject = NEW ObjectMasterDatasetModel () .
                oObject:UseDedicatedBusinessEntity = TRUE .

                oObject:SmartObjectInstance:Filter:ObjectInstanceGuid:EQ (pcObjectInstanceGuid):Run () .

                IF oObject:SmartObjectInstance:Available THEN DO:
                    ASSIGN oReturn:InstanceName       = oObject:SmartObjectInstance:InstanceName
                           cPageGuid                  = oObject:SmartObjectInstance:PageGuid .

                    FIND b_eSmartPage WHERE b_eSmartPage.PageGuid = cPageGuid NO-ERROR .

                    IF AVAILABLE b_eSmartPage THEN
                        ASSIGN oReturn:InstancePageNumber = b_eSmartPage.PageSequence .
                    ELSE DO:
                        oObject = NEW ObjectMasterDatasetModel () .
                        oObject:UseDedicatedBusinessEntity = TRUE .

                        oObject:SmartPage:Filter:PageGuid:EQ (cPageGuid):Run () .

                        ASSIGN oReturn:InstancePageNumber = oObject:SmartPage:PageSequenceWhenAvailable(?).
                    END.
                END.
                ELSE
                    ASSIGN oReturn:InstanceName       = "<invalid instance>":U
                           oReturn:InstancePageNumber = ? .
            END.
        END.

        RETURN oReturn .

    END METHOD.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Object.ObjectIsAParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns if the object master is a child of the given class
        Notes:
        @param dsObjectMaster INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ObjectIsA (INPUT-OUTPUT DATASET dsObjectMaster, poParameter AS Consultingwerk.SmartFramework.Repository.Object.ObjectIsAParameter):

        DEFINE VARIABLE oDataAccess AS ObjectMasterDataAccess NO-UNDO .

        ObjectAssert:IsValid (poParameter) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        oDataAccess = CAST (THIS-OBJECT:DataAccessObject, ObjectMasterDataAccess) .

        oDataAccess:ObjectIsA (poParameter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oInstanceDetails AS ObjectInstanceDetails NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:FetchDataRequest) AND
           LOOKUP ("SkipCalculatedFields":U, THIS-OBJECT:FetchDataRequest:CustomContext) > 0 THEN
            RETURN .

        FOR EACH eSmartLink ON ERROR UNDO, THROW:

            oInstanceDetails = THIS-OBJECT:GetObjectInstanceDetails (eSmartLink.SourceObjectInstanceGuid) .

            ASSIGN eSmartLink.SourceObjectName = oInstanceDetails:InstanceName
                   eSmartLink.SourcePageNumber = oInstanceDetails:InstancePageNumber .

            oInstanceDetails = THIS-OBJECT:GetObjectInstanceDetails (eSmartLink.TargetObjectInstanceGuid) .

            ASSIGN eSmartLink.TargetObjectName = oInstanceDetails:InstanceName
                   eSmartLink.TargetPageNumber = oInstanceDetails:InstancePageNumber .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        FOR EACH eSmartObjectMaster ON ERROR UNDO, THROW:

            ValidationHelper:ReadOnly (BUFFER eSmartObjectMaster:HANDLE, "ObjectMasterGuid":U, ?) .
            ValidationHelper:ReadOnly (BUFFER eSmartObjectMaster:HANDLE, "ObjectTypeGuid":U, ?) .

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartObjectMaster:HANDLE, "ObjectName":U, ?) .

            IF CharacterType:IsNullOrEmpty (eSmartObjectMaster.ObjectDescription) THEN
                eSmartObjectMaster.ObjectDescription = eSmartObjectMaster.ObjectName .
        END.

        FOR EACH eSmartPage ON ERROR UNDO, THROW:

            ValidationHelper:ReadOnly (BUFFER eSmartPage:HANDLE, "PageGuid":U, ?) .
            ValidationHelper:ReadOnly (BUFFER eSmartPage:HANDLE, "ContainerObjectMasterGuid":U, ?) .
            ValidationHelper:ReadOnly (BUFFER eSmartPage:HANDLE, "PageSequence":U, ?) .
            ValidationHelper:GT (BUFFER eSmartPage:HANDLE, "PageSequence":U, 0, ?) .

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartPage:HANDLE, "PageLabel":U, ?) .
        END.

        FOR EACH eSmartObjectInstance ON ERROR UNDO, THROW:

            ValidationHelper:ReadOnly (BUFFER eSmartObjectInstance:HANDLE, "ObjectMasterGuid":U, ?) .
            ValidationHelper:NE (BUFFER eSmartObjectInstance:HANDLE, "ObjectMasterGuid":U,
                                 eSmartObjectInstance.ContainerObjectMasterGuid,
                                 MessageFormatter:GetMessage (MessageGroups:SFR, 401)) .

            ValidationHelper:GT (BUFFER eSmartObjectInstance:HANDLE, "ObjectSequence":U, 0, ?) .

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartObjectInstance:HANDLE, "InstanceName":U, ?) .
        END.

        FOR EACH eSmartLink ON ERROR UNDO, THROW:

            ValidationHelper:ReadOnly (BUFFER eSmartLink:HANDLE, "LinkGuid":U, ?) .
            ValidationHelper:IsNotNullOrEmpty(BUFFER eSmartLink:HANDLE, "LinkName":U, ?).

        END.

    END METHOD.

END CLASS.
