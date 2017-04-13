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
    File        : ObjectTypeBusinessEntity
    Purpose     : Business Entity for SmartObjectType
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.10.2015 01:55:45
    Notes       : The CustomContext of the FetchDataRequest instance passed
                  in during a FetchData Request may contain the entry
                  "NoCalcFields" (comma delimited list). When this is passed
                  in, the Business Entity will not fill calculated fields
                  for performance reasons while fetching data
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Assertion.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*     FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityTable (name="eSmartObjectType", MandatoryColumns="ObjectTypeName", DefaultSort="BY eSmartObjectType.ObjectTypeName") .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Class/dsObjectType.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ObjectTypeBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectTypeBusinessEntity ():
        SUPER (DATASET dsObjectType:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeDataAccess":U .

    END CONSTRUCTOR.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.ClassIsAParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns if the child class is a child of the given parent class
        Notes:
        @param dsObjectType INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClassIsA (INPUT-OUTPUT DATASET dsObjectType,
                                 poParameter AS ClassIsAParameter):

        DEFINE VARIABLE oDataAccess AS ObjectTypeDataAccess NO-UNDO .

        ObjectAssert:IsValid (poParameter) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        oDataAccess = CAST (THIS-OBJECT:DataAccessObject, ObjectTypeDataAccess) .

        oDataAccess:ClassIsA (poParameter) .

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetChildClassesParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of child classes of the given classes
        Notes:   The parent classes are returned as well
        @param dsObjectType not used
        @param poParameter The GetChildClassesParameter instance with the comma delimited list of parent classes
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetChildClasses (INPUT-OUTPUT DATASET dsObjectType,
                                        poParameter AS GetChildClassesParameter):

        DEFINE VARIABLE cClasses           AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                 NO-UNDO .
        DEFINE VARIABLE oRequest           AS FetchDataRequest        NO-UNDO .
        DEFINE VARIABLE oRepositoryService AS ISmartRepositoryService NO-UNDO .

        oRepositoryService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.ISmartRepositoryService
                                                           "NEW SmartRepositoryService ()"} .

        ObjectAssert:IsValid (poParameter, "poParameter":U) .

        ASSIGN cClasses = oRepositoryService:EnsureGuids (poParameter:ObjectTypeGuids) .

        DO i = 1 TO NUM-ENTRIES (cClasses):

            oRequest = NEW FetchDataRequest ("eSmartObjectType":U,
                                             "for each eSmartObjectType where eSmartObjectType.ExtendsObjectTypeGuid = ":U + QUOTER (ENTRY (i, cClasses))) .

            THIS-OBJECT:FetchData (oRequest) .

            FOR EACH eSmartObjectType ON ERROR UNDO, THROW:
                ASSIGN cClasses = cClasses + ",":U + eSmartObjectType.ObjectTypeGuid .
            END.
        END .

        poParameter:ChildObjectTypeGuids = cClasses .

        FINALLY:
            DATASET dsObjectType:EMPTY-DATASET () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oObjectType AS ObjectTypeDatasetModel NO-UNDO .
        DEFINE VARIABLE oClassType  AS ClassTypeDatasetModel  NO-UNDO .

        /* SCL-1640 */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 16.10.2015
           Ability to opt-out calculated fields for performance during repository
           processing */
        IF VALID-OBJECT (THIS-OBJECT:FetchDataRequest) AND
           INDEX (THIS-OBJECT:FetchDataRequest:CustomContext, "NoCalcFields":U) > 0 THEN
            RETURN .

        oObjectType = NEW ObjectTypeDatasetModel () .
        oObjectType:UseDedicatedBusinessEntity = TRUE .

        oClassType = NEW ClassTypeDatasetModel () .

        FOR EACH eSmartObjectType BREAK BY eSmartObjectType.ExtendsObjectTypeGuid
            ON ERROR UNDO, THROW:

            IF FIRST-OF (eSmartObjectType.ExtendsObjectTypeGuid) THEN DO:
                oObjectType:SmartObjectType:Filter:Reset().
                oObjectType:SmartObjectType:Filter:ObjectTypeGuid:EQ (eSmartObjectType.ExtendsObjectTypeGuid):Run () .
            END.

            eSmartObjectType.ExtendsObjectTypeName = oObjectType:SmartObjectType:ObjectTypeNameWhenAvailable ("":U) .
        END.

        FOR EACH eSmartObjectType BREAK BY eSmartObjectType.CustomObjectTypeGuid
            ON ERROR UNDO, THROW:

            IF FIRST-OF (eSmartObjectType.CustomObjectTypeGuid) THEN DO:
                oObjectType:SmartObjectType:Filter:Reset().
                oObjectType:SmartObjectType:Filter:ObjectTypeGuid:EQ (eSmartObjectType.CustomObjectTypeGuid):Run () .
            END.

            eSmartObjectType.CustomObjectTypeName = oObjectType:SmartObjectType:ObjectTypeNameWhenAvailable ("":U) .
        END .

        FOR EACH eSmartObjectType BREAK BY eSmartObjectType.ClassTypeGuid
            ON ERROR UNDO, THROW:

            IF FIRST-OF (eSmartObjectType.ClassTypeGuid) THEN DO:
                oClassType:SmartClassType:Filter:Reset().
                oClassType:SmartClassType:Filter:ClassTypeGuid:EQ (eSmartObjectType.ClassTypeGuid):Run () .
            END.

            IF oClassType:SmartClassType:Available THEN
                eSmartObjectType.ClassTypeName = oClassType:SmartClassType:ClassTypeName .
            ELSE
                eSmartObjectType.ClassTypeName = "":U .
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

        DEFINE VARIABLE cEntityForTest    AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oGetParentClasses AS GetParentClassesParameter NO-UNDO .
        DEFINE VARIABLE oClassType        AS ClassTypeDatasetModel     NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        ASSIGN cEntityForTest = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U + CHR(1) + GUID .

        FOR EACH eSmartObjectType ON ERROR UNDO, THROW:

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartObjectType:HANDLE,
                                                           "ObjectTypeName":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Object Type"{&TRAN})) .

            IF eSmartObjectType.ObjectTypeName > "":U AND
               (INDEX (eSmartObjectType.ObjectTypeName, " ":U) > 0 OR
                INDEX (eSmartObjectType.ObjectTypeName, ",":U) > 0) THEN
                DatasetHelper:AddErrorString (BUFFER eSmartObjectType:HANDLE,
                                              MessageFormatter:GetMessage ("VALMSG":U, 2, "Object Type Name"{&TRAN}, eSmartObjectType.ObjectTypeName),
                                              "ObjectTypeName":U) .

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartObjectType:HANDLE,
                                                           "ObjectTypeDescription":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Object Type Description"{&TRAN})) .

            IF NOT eSmartObjectType.ExtendsObjectTypeGuid = "":U THEN DO:
                IF Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartObjectType:HANDLE,
                                                         "ExtendsObjectTypeGuid":U,
                                                         cEntityForTest,
                                                         "eSmartObjectType":U,
                                                         "for each eSmartObjectType where eSmartObjectType.ObjectTypeGuid = &1":U,
                                                         MessageFormatter:GetMessage ("VALMSG":U, 5, "Extends Object Type"{&TRAN}, "Object Type"{&TRAN})) THEN DO:

                    IF eSmartObjectType.ObjectTypeGuid = eSmartObjectType.ExtendsObjectTypeGuid THEN
                        DatasetHelper:AddErrorString (BUFFER eSmartObjectType:HANDLE,
                                                      MessageFormatter:GetMessage ("SFR":U, 400),
                                                      "ExtendsObjectTypeGuid":U) .

                    ELSE
                    IF eSmartObjectType.ObjectTypeGuid > "":U THEN DO:

                        oGetParentClasses = NEW GetParentClassesParameter (eSmartObjectType.ExtendsObjectTypeGuid) .

                        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
                            THIS-OBJECT:InitializeDataAccessObject () .

                        CAST (THIS-OBJECT:DataAccessObject, ObjectTypeDataAccess):GetParentClasses (oGetParentClasses) .

                        IF LOOKUP (eSmartObjectType.ObjectTypeGuid, oGetParentClasses:ParentObjectTypeGuids, CHR(1)) > 0 THEN
                            DatasetHelper:AddErrorString (BUFFER eSmartObjectType:HANDLE,
                                                          MessageFormatter:GetMessage ("SFR":U, 400),
                                                          "ExtendsObjectTypeGuid":U) .
                    END.
                END.
            END.

            IF NOT eSmartObjectType.CustomObjectTypeGuid = "":U THEN
                Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartObjectType:HANDLE,
                                                      "CustomObjectTypeGuid":U,
                                                      cEntityForTest,
                                                      "eSmartObjectType":U,
                                                      "for each eSmartObjectType where eSmartObjectType.ObjectTypeGuid = &1":U,
                                                      MessageFormatter:GetMessage ("VALMSG":U, 5, "Custom Object Type"{&TRAN}, "Object Type"{&TRAN})).

            IF eSmartObjectType.TechnicalClassName > "":U AND
               INDEX (eSmartObjectType.TechnicalClassName, " ":U) > 0 THEN
                DatasetHelper:AddErrorString (BUFFER eSmartObjectType:HANDLE,
                                              MessageFormatter:GetMessage ("VALMSG":U, 2, "Technical Class Name"{&TRAN}, eSmartObjectType.TechnicalClassName),
                                              "TechnicalClassName":U) .

            /* Default Class Type */
            IF CharacterType:IsNullOrEmpty(eSmartObjectType.ClassTypeGuid) THEN DO:
                oClassType = NEW ClassTypeDatasetModel() .
                oClassType:SmartClassType:Filter:ClassTypeName:EQ ("unspecified":U):Run () .

                IF oClassType:SmartClassType:Available THEN
                    eSmartObjectType.ClassTypeGuid = oClassType:SmartClassType:ClassTypeGuid .
            END.

            Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartObjectType:HANDLE,
                                                  "ClassTypeGuid":U,
                                                  "Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity":U,
                                                  "eSmartClassType":U,
                                                  "for each eSmartClassType where eSmartClassType.ClassTypeGuid = &1":U,
                                                  MessageFormatter:GetMessage ("VALMSG":U, 5, "Class Type"{&TRAN}, "Class Type"{&TRAN})).

            IF eSmartObjectType.ContainerType THEN DO:
                IF Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartObjectType:HANDLE,
                                                                  "ObjectTypeName":U,
                                                                  MessageFormatter:GetMessage ("VALMSG":U, 1, "Object Type"{&TRAN})) THEN

                Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartObjectType:HANDLE,
                                                       "StoreInstances":U,
                                                       "Master,Instance":U,
                                                       MessageFormatter:GetMessage ("VALMSG":U, 2, "Store Instances"{&TRAN}, eSmartObjectType.StoreInstances)) .
            END.
            ELSE
                ASSIGN eSmartObjectType.StoreInstances = "":U . /* no instances stored for non container objects */
        END.

        FINALLY:
            IF ServiceManager:IsRunning (cEntityForTest) THEN
                ServiceManager:StopBusinessService (cEntityForTest) .
        END FINALLY.

    END METHOD.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetParentClassesParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Determines the list of parent classes for the given class
        Notes:   The poParameter:ObjectTypeName may contain an ObjectTypeName or ObjectTypeGuid
                 The search is first performed based on the ObjectTypeName
                 The list of parent class Guid's is returned CHR(1) delimited. The list
                 of parent class names is returned comma delimited
        @param dsObjectType INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetParentClasses (INPUT-OUTPUT DATASET dsObjectType,
                                         poParameter AS GetParentClassesParameter):

        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        Assert:NotNullOrEmpty (poParameter:ObjectTypeName, "poParameter:ObjectTypeName":U) .

        IF poParameter:ObjectTypeName = "new":U THEN DO:
            ASSIGN poParameter:ParentObjectTypeGuids = "":U
                   poParameter:ParentObjectTypeNames = "":U .

            RETURN .
        END.

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        CAST (THIS-OBJECT:DataAccessObject, ObjectTypeDataAccess):GetParentClasses (poParameter) .

    END METHOD .

END CLASS.
