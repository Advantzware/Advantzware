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
    File        : AttributeValueBusinessEntity
    Purpose     : Business Entity for SmartAttributeValue
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.10.2015 20:22:29
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*      FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Class/dsAttributeValue.i }
    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i }

    DEFINE VARIABLE lPopulateCalculatedFields AS LOGICAL NO-UNDO INIT TRUE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttributeValueBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeValueBusinessEntity ():
        SUPER (DATASET dsAttributeValue:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.AttributeValueDataAccess":U .

    END CONSTRUCTOR.

    @InvokeMethod (template="", parameterClassName="Consultingwerk.ISerializable", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Fetches Object Instance Attributes
        Notes:   The input dataset contains a single buffer per object type to be retrieved
                 Each record must reference the object instance using the _ObjectInstanceGuid
                 field, ignores the eSmartObjectMaster, eSmartObjectInstance, eSmartLink and
                 eSmartPage temp-tables as those might be contained in the ProDataset for
                 storing links and pages
        @param phDataset INPUT-OUTPUT DATASET-HANDLE
        @param poParameter The Parameter Object for this method, ignored
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchObjectInstanceAttributes (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                      poParameter AS Consultingwerk.ISerializable):

        DEFINE VARIABLE iBuffer       AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oCacheFactory AS IRepositoryCacheTableFactory NO-UNDO .

        HandleAssert:ValidHandle(phDataset, "phDataset":U) .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW RepositoryCacheTableFactory()"} .

        bufferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:

            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (iBuffer) .

            IF LOOKUP (hBuffer:NAME, "eSmartObjectMaster,eSmartObjectInstance,eSmartLink,eSmartPage,_InstanceTypeMap":U) > 0 THEN
                NEXT bufferLoop .

            BufferAssert:HasField (hBuffer, "_ObjectMasterGuid":U) .

            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hBuffer, "for each ":U + hBuffer:NAME + " where _ObjectMasterGuid > '' AND _ObjectInstanceGuid > ''":U) .

            DO WHILE NOT hQuery:QUERY-OFF-END:
                THIS-OBJECT:FetchData (NEW FetchDataRequest ("eSmartAttributeValue":U,
                                                             SUBSTITUTE ("for each eSmartAttributeValue where eSmartAttributeValue.ContainerObjectMasterGuid = &1 AND eSmartAttributeValue.ObjectInstanceGuid = '' ~
                                                                                                           or eSmartAttributeValue.ObjectInstanceGuid = &2":U,
                                                                         QUOTER (hBuffer::_ObjectMasterGuid),
                                                                         QUOTER (hBuffer::_ObjectInstanceGuid)))) .

                attributeLoop1:
                FOR EACH eSmartAttributeValue WHERE eSmartAttributeValue.ObjectMasterGuid   = hBuffer::_ObjectMasterGuid
                                                AND eSmartAttributeValue.ObjectInstanceGuid = "":U:

                    hField = hBuffer:BUFFER-FIELD (eSmartAttributeValue.AttributeLabel) NO-ERROR .

                    IF NOT VALID-HANDLE (hField) THEN DO:
                        ERROR-STATUS:ERROR = FALSE NO-ERROR .
                        NEXT attributeLoop1 .
                    END .

                    CASE hField:DATA-TYPE:
                        WHEN "CHARACTER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                        WHEN "INTEGER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.IntegerValue . /* AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Integer Value":T */
                        WHEN "INT64":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.Int64Value . /* AS INT64 FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Int64 value":T */
                        WHEN "DATE":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateValue . /* AS DATE FORMAT "99/99/9999":U INIT ? LABEL "Date Value":T */
                        WHEN "DATETIME":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeValue . /* AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INIT ? LABEL "DateTime Value":T */
                        WHEN "DATETIME-TZ":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeTzValue . /* AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "DateTime-TZ Value":T */
                        WHEN "DECIMAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DecimalValue . /* AS DECIMAL FORMAT "->>,>>9.99":U INIT "0":U LABEL "Decimal Value":T */
                        WHEN "LOGICAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.LogicalValue . /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Logical Value":T */
                        WHEN "RAW":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.RawValue . /* AS RAW */
                        OTHERWISE
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                    END CASE .
                END.

                attributeLoop2:
                FOR EACH eSmartAttributeValue WHERE eSmartAttributeValue.ObjectInstanceGuid = hBuffer::_ObjectInstanceGuid:

                    hField = hBuffer:BUFFER-FIELD (eSmartAttributeValue.AttributeLabel) NO-ERROR .

                    IF NOT VALID-HANDLE (hField) THEN DO:
                        ERROR-STATUS:ERROR = FALSE NO-ERROR .
                        NEXT attributeLoop2 .
                    END .

                    CASE hField:DATA-TYPE:
                        WHEN "CHARACTER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                        WHEN "INTEGER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.IntegerValue . /* AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Integer Value":T */
                        WHEN "INT64":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.Int64Value . /* AS INT64 FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Int64 value":T */
                        WHEN "DATE":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateValue . /* AS DATE FORMAT "99/99/9999":U INIT ? LABEL "Date Value":T */
                        WHEN "DATETIME":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeValue . /* AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INIT ? LABEL "DateTime Value":T */
                        WHEN "DATETIME-TZ":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeTzValue . /* AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "DateTime-TZ Value":T */
                        WHEN "DECIMAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DecimalValue . /* AS DECIMAL FORMAT "->>,>>9.99":U INIT "0":U LABEL "Decimal Value":T */
                        WHEN "LOGICAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.LogicalValue . /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Logical Value":T */
                        WHEN "RAW":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.RawValue . /* AS RAW */
                        OTHERWISE
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                    END CASE .
                END.
                hQuery:GET-NEXT () .
            END.

            FINALLY:
                IF VALID-HANDLE (hQuery) THEN
                    DELETE OBJECT hQuery .
            END FINALLY.
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset}
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="", parameterClassName="Consultingwerk.ISerializable", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Fetches Object Master Attributes
        Notes:   The input dataset contains a single buffer per object type to be retrieved
                 Each record must reference the object master using the _ObjectMasterGuid
                 field, ignores the eSmartObjectMaster, eSmartObjectInstance, eSmartLink and
                 eSmartPage temp-tables as those might be contained in the ProDataset for
                 storing links and pages
        @param phDataset INPUT-OUTPUT DATASET-HANDLE
        @param poParameter The Parameter Object for this method, ignored
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchObjectMasterAttributes (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                    poParameter AS Consultingwerk.ISerializable):

        DEFINE VARIABLE iBuffer       AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oCacheFactory AS IRepositoryCacheTableFactory NO-UNDO .

        HandleAssert:ValidHandle(phDataset, "phDataset":U) .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW RepositoryCacheTableFactory()"} .

        bufferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:

            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (iBuffer) .

            IF LOOKUP (hBuffer:NAME, "eSmartObjectMaster,eSmartObjectInstance,eSmartLink,eSmartPage,_InstanceTypeMap":U) > 0 THEN
                NEXT bufferLoop .

            BufferAssert:HasField (hBuffer, "_ObjectMasterGuid":U) .

            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hBuffer, "for each ":U + hBuffer:NAME + " where _ObjectMasterGuid > ''":U) .

            DO WHILE NOT hQuery:QUERY-OFF-END:
                THIS-OBJECT:FetchData (NEW FetchDataRequest ("eSmartAttributeValue":U,
                                                             SUBSTITUTE ("for each eSmartAttributeValue where eSmartAttributeValue.ContainerObjectMasterGuid = &1 AND eSmartAttributeValue.ObjectInstanceGuid = ''":U,
                                                                         QUOTER (hBuffer::_ObjectMasterGuid)))) .

                attributeLoop:
                FOR EACH eSmartAttributeValue WHERE eSmartAttributeValue.ObjectMasterGuid = hBuffer::_ObjectMasterGuid:

                    hField = hBuffer:BUFFER-FIELD (eSmartAttributeValue.AttributeLabel) NO-ERROR .

                    IF NOT VALID-HANDLE (hField) THEN DO:
                        ERROR-STATUS:ERROR = FALSE NO-ERROR .
                        NEXT attributeLoop .
                    END .

                    CASE hField:DATA-TYPE:
                        WHEN "CHARACTER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                        WHEN "INTEGER":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.IntegerValue . /* AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Integer Value":T */
                        WHEN "INT64":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.Int64Value . /* AS INT64 FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Int64 value":T */
                        WHEN "DATE":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateValue . /* AS DATE FORMAT "99/99/9999":U INIT ? LABEL "Date Value":T */
                        WHEN "DATETIME":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeValue . /* AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INIT ? LABEL "DateTime Value":T */
                        WHEN "DATETIME-TZ":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DateTimeTzValue . /* AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "DateTime-TZ Value":T */
                        WHEN "DECIMAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.DecimalValue . /* AS DECIMAL FORMAT "->>,>>9.99":U INIT "0":U LABEL "Decimal Value":T */
                        WHEN "LOGICAL":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.LogicalValue . /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Logical Value":T */
                        WHEN "RAW":U THEN
                           hField:BUFFER-VALUE = eSmartAttributeValue.RawValue . /* AS RAW */
                        OTHERWISE
                           hField:BUFFER-VALUE = eSmartAttributeValue.CharacterValue . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                    END CASE .
                END.

                hQuery:GET-NEXT () .
            END.

            FINALLY:
                IF VALID-HANDLE (hQuery) THEN
                    DELETE OBJECT hQuery .
            END FINALLY.
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset}
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectInstanceDesignAttributesParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns the Design Time Attributes for the given SmartObjectInstance
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectInstanceDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                          poParameter AS GetObjectInstanceDesignAttributesParameter):

        DEFINE VARIABLE oRequest         AS FetchDataRequest         NO-UNDO .
        DEFINE VARIABLE oObjectInstance  AS ObjectMasterDatasetModel NO-UNDO .

        oObjectInstance = NEW ObjectMasterDatasetModel () .
        oObjectInstance:SmartObjectInstance:Filter:ObjectInstanceGuid:EQ(poParameter:ObjectInstanceGuid):Run () .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        THIS-OBJECT:GetObjectMasterDesignAttributes (DATASET dsDesignAttributeValue,
                                                     NEW GetObjectMasterDesignAttributesParameter (oObjectInstance:SmartObjectInstance:ObjectMasterGuid)) .

        FOR EACH eDesignAttributeValue WHERE eDesignAttributeValue.IsInherited = FALSE ON ERROR UNDO, THROW:
            ASSIGN eDesignAttributeValue.IsInherited   = TRUE
                   eDesignAttributeValue.InheritedFrom = "<Object Master>":U .
        END.

        oRequest = NEW FetchDataRequest ("eSmartAttributeValue":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectInstanceGuid = &1':U,
                                                     QUOTER (poParameter:ObjectInstanceGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        eSmartAttributeValueLoop:
        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            IF NOT CAN-FIND (eDesignAttributeValue WHERE eDesignAttributeValue.AttributeLabel = eSmartAttributeValue.AttributeLabel) THEN
                NEXT eSmartAttributeValueLoop .

            FIND eDesignAttributeValue WHERE eDesignAttributeValue.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .

            ASSIGN eDesignAttributeValue.IsInherited     = FALSE

                   eDesignAttributeValue.CharacterValue  = eSmartAttributeValue.CharacterValue
                   eDesignAttributeValue.IntegerValue    = eSmartAttributeValue.IntegerValue
                   eDesignAttributeValue.Int64Value      = eSmartAttributeValue.Int64Value
                   eDesignAttributeValue.DateValue       = eSmartAttributeValue.DateValue
                   eDesignAttributeValue.DateTimeValue   = eSmartAttributeValue.DateTimeValue
                   eDesignAttributeValue.DateTimeTzValue = eSmartAttributeValue.DateTimeTzValue
                   eDesignAttributeValue.DecimalValue    = eSmartAttributeValue.DecimalValue
                   eDesignAttributeValue.LogicalValue    = eSmartAttributeValue.LogicalValue
                   eDesignAttributeValue.RawValue        = eSmartAttributeValue.RawValue
                   .
        END.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterDesignAttributesParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns the Design Time Attributes for the given SmartObjectMaster
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectMasterDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                        poParameter AS GetObjectMasterDesignAttributesParameter):

        DEFINE VARIABLE oRequest         AS FetchDataRequest         NO-UNDO .
        DEFINE VARIABLE oObjectMaster    AS ObjectMasterDatasetModel NO-UNDO .

        oObjectMaster = NEW ObjectMasterDatasetModel (poParameter:ObjectMasterGuid) .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        THIS-OBJECT:GetObjectTypeDesignAttributes (DATASET dsDesignAttributeValue,
                                                   NEW GetObjectTypeDesignAttributesParameter (oObjectMaster:SmartObjectMaster:ObjectTypeGuid)) .

        FOR EACH eDesignAttributeValue WHERE eDesignAttributeValue.IsInherited = FALSE ON ERROR UNDO, THROW:
            ASSIGN eDesignAttributeValue.IsInherited   = TRUE
                   eDesignAttributeValue.InheritedFrom = oObjectMaster:SmartObjectMaster:ObjectTypeName .
        END.

        oRequest = NEW FetchDataRequest ("eSmartAttributeValue":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ContainerObjectMasterGuid  = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                     QUOTER (poParameter:ObjectMasterGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        eSmartAttributeValueLoop:
        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            IF NOT CAN-FIND (eDesignAttributeValue WHERE eDesignAttributeValue.AttributeLabel = eSmartAttributeValue.AttributeLabel) THEN
                NEXT eSmartAttributeValueLoop .

            FIND eDesignAttributeValue WHERE eDesignAttributeValue.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .

            ASSIGN eDesignAttributeValue.IsInherited     = FALSE

                   eDesignAttributeValue.CharacterValue  = eSmartAttributeValue.CharacterValue
                   eDesignAttributeValue.IntegerValue    = eSmartAttributeValue.IntegerValue
                   eDesignAttributeValue.Int64Value      = eSmartAttributeValue.Int64Value
                   eDesignAttributeValue.DateValue       = eSmartAttributeValue.DateValue
                   eDesignAttributeValue.DateTimeValue   = eSmartAttributeValue.DateTimeValue
                   eDesignAttributeValue.DateTimeTzValue = eSmartAttributeValue.DateTimeTzValue
                   eDesignAttributeValue.DecimalValue    = eSmartAttributeValue.DecimalValue
                   eDesignAttributeValue.LogicalValue    = eSmartAttributeValue.LogicalValue
                   eDesignAttributeValue.RawValue        = eSmartAttributeValue.RawValue
                   .
        END.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributesParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns the Design Time Attributes for the given SmartObjectType
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectTypeDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                      poParameter AS GetObjectTypeDesignAttributesParameter):

        DEFINE VARIABLE oRequest         AS FetchDataRequest          NO-UNDO .
        DEFINE VARIABLE oParentClasses   AS GetParentClassesParameter NO-UNDO .
        DEFINE VARIABLE oObjectTypeModel AS ObjectTypeDatasetModel    NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE iEntries         AS INTEGER                   NO-UNDO .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                     QUOTER (poParameter:ObjectTypeGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            CREATE eDesignAttributeValue.

            BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue .

            FIND eSmartAttribute WHERE eSmartAttribute.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .
            IF AVAILABLE eSmartAttribute THEN
                BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .

            ASSIGN eDesignAttributeValue.IsInherited = FALSE .

            FINALLY:
                ErrorHelper:ResetErrorStatus() .
            END FINALLY.
        END.

        oParentClasses = NEW GetParentClassesParameter (poParameter:ObjectTypeGuid) .

        oObjectTypeModel = NEW ObjectTypeDatasetModel () .
        oObjectTypeModel:GetParentClasses(oParentClasses) .

        ASSIGN iEntries = NUM-ENTRIES (oParentClasses:ParentObjectTypeGuids, CHR(1)) .

        DO i = 1 TO iEntries:
            oRequest = NEW FetchDataRequest ("*":U,
                                             SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U +
                                                                                       '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +
                                                                                       '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +
                                                                                       '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                         QUOTER (ENTRY (i, oParentClasses:ParentObjectTypeGuids, CHR(1)))),
                                             0) .

            THIS-OBJECT:FetchData (oRequest) .

            FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

                FIND eDesignAttributeValue WHERE eDesignAttributeValue.ObjectTypeGuid     = poParameter:ObjectTypeGuid
                                             AND eDesignAttributeValue.ObjectMasterGuid   = "":U
                                             AND eDesignAttributeValue.ObjectInstanceGuid = "":U
                                             AND eDesignAttributeValue.AttributeLabel     = eSmartAttributeValue.AttributeLabel NO-ERROR .

                /* Value not (yet) overridden by child class */
                IF NOT AVAILABLE eDesignAttributeValue THEN DO:
                    CREATE eDesignAttributeValue.

                    BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue
                        ASSIGN eDesignAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid.

                    FIND eSmartAttribute WHERE eSmartAttribute.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .
                    IF AVAILABLE eSmartAttribute THEN
                        BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .

                    ASSIGN eDesignAttributeValue.IsInherited   = TRUE
                           eDesignAttributeValue.InheritedFrom = ENTRY (LOOKUP (eSmartAttributeValue.ObjectTypeGuid,
                                                                                oParentClasses:ParentObjectTypeGuids,
                                                                                CHR (1)),
                                                                        oParentClasses:ParentObjectTypeNames) .
                END.
                ELSE DO:
                    IF eDesignAttributeValue.InheritedFrom = "":U THEN
                        ASSIGN eDesignAttributeValue.InheritedFrom = ENTRY (LOOKUP (eSmartAttributeValue.ObjectTypeGuid,
                                                                                    oParentClasses:ParentObjectTypeGuids,
                                                                                    CHR (1)),
                                                                            oParentClasses:ParentObjectTypeNames) .
                    /* When not yet Constant Value, apply Constant value
                       setting from parent class */
                    IF eDesignAttributeValue.ConstantValue = FALSE THEN
                        eDesignAttributeValue.ConstantValue = eSmartAttributeValue.ConstantValue .
                END.

                FINALLY:
                    ErrorHelper:ResetErrorStatus() .
                END FINALLY.
            END.
        END.

        FINALLY:
            DATASET dsAttributeValue:EMPTY-DATASET ().
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterAttributeValueParameter", datasetInput="false", datasetOutput="false") .
    /*------------------------------------------------------------------------------
        Purpose: Returns a single SmartAttributeValue record to the client
        Notes:
        @param hDataset INPUT-OUTPUT DATASET-HANDLE Not used
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectMasterAttributeValue (INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                      poParameter AS Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterAttributeValueParameter):

        DEFINE VARIABLE oRequest      AS IFetchDataRequest            NO-UNDO .
        DEFINE VARIABLE oCacheFactory AS IRepositoryCacheTableFactory NO-UNDO .
        DEFINE VARIABLE hCacheDataset AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oRepository   AS ISmartRepositoryService      NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE                       NO-UNDO .

        ASSIGN lPopulateCalculatedFields = FALSE .

        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectMasterGuid           = &1':U +
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U +
                                                                                   '  and eSmartAttributeValue.AttributeLabel             = &2':U,
                                                     QUOTER (poParameter:ObjectMasterGuid),
                                                     QUOTER (poParameter:AttributeLabel)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        FIND FIRST eSmartAttributeValue NO-ERROR  .

        IF AVAILABLE eSmartAttributeValue THEN
            ASSIGN poParameter:CharacterValue  = eSmartAttributeValue.CharacterValue
                   poParameter:IntegerValue    = eSmartAttributeValue.IntegerValue
                   poParameter:Int64Value      = eSmartAttributeValue.Int64Value
                   poParameter:DateValue       = eSmartAttributeValue.DateValue
                   poParameter:DateTimeValue   = eSmartAttributeValue.DateTimeValue
                   poParameter:DateTimeTzValue = eSmartAttributeValue.DateTimeTzValue
                   poParameter:DecimalValue    = eSmartAttributeValue.DecimalValue
                   poParameter:LogicalValue    = eSmartAttributeValue.LogicalValue
                   poParameter:RawValue        = eSmartAttributeValue.RawValue .

        ELSE DO:
            oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                          "NEW RepositoryCacheTableFactory()"} .
            oRepository = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.ISmartRepositoryService
                                                        "NEW SmartRepositoryService()"} .

            hCacheDataset = oCacheFactory:GetRepositoryCacheTables (GetAttributesForEnum:DesignTime,
                                                                    NEW CharacterList (oRepository:GetObjectTypeGuidOfMaster(poParameter:ObjectMasterGuid))) .

            IF BufferHelper:HasField(hCacheDataset:GET-BUFFER-HANDLE(1), poParameter:AttributeLabel) THEN DO:

                hField = hCacheDataset:GET-BUFFER-HANDLE(1):BUFFER-FIELD (poParameter:AttributeLabel) .

                CASE hField:DATA-TYPE:
                    WHEN "CHARACTER":U THEN
                        poParameter:CharacterValue = hField:DEFAULT-VALUE .
                    WHEN "INTEGER":U THEN
                        poParameter:IntegerValue = hField:DEFAULT-VALUE .
                    WHEN "INT64":U THEN
                        poParameter:Int64Value = hField:DEFAULT-VALUE .
                    WHEN "DATE":U THEN
                        poParameter:DateValue = hField:DEFAULT-VALUE .
                    WHEN "DATETIME":U THEN
                        poParameter:DateTimeValue = hField:DEFAULT-VALUE .
                    WHEN "DATETIME-TZ":U THEN
                        poParameter:DateTimeTzValue = hField:DEFAULT-VALUE .
                    WHEN "DECIMAL":U THEN
                        poParameter:DecimalValue = hField:DEFAULT-VALUE .
                    WHEN "LOGICAL":U THEN
                        poParameter:LogicalValue = hField:DEFAULT-VALUE .
                    WHEN "RAW":U THEN
                        poParameter:RawValue = hField:DEFAULT-VALUE .
                    OTHERWISE
                        UNDO, THROW NEW RepositoryException ("Invalid attribute type."{&TRAN}, 0) .

                END CASE .
            END.
            ELSE
                UNDO, THROW NEW RepositoryException (SUBSTITUTE ("Invalid attribute &1 for object type."{&TRAN},
                                                                 poParameter:AttributeLabel), 0) .
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hDataset} .

            ASSIGN lPopulateCalculatedFields = TRUE .

            ErrorHelper:ResetErrorStatus() .
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterDesignAttributeParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns a single SmartAttributeValue record to the client
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectMasterDesignAttribute (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                       poParameter AS Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterDesignAttributeParameter):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectMasterGuid           = &1':U +
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U +
                                                                                   '  and eSmartAttributeValue.AttributeLabel             = &2':U,
                                                     QUOTER (poParameter:ObjectMasterGuid),
                                                     QUOTER (poParameter:AttributeLabel)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            CREATE eDesignAttributeValue.

            BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue .

            FIND FIRST eSmartAttribute WHERE eSmartAttribute.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .
            IF AVAILABLE eSmartAttribute THEN
                BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .

            ASSIGN eDesignAttributeValue.IsInherited = FALSE .

            FINALLY:
                ErrorHelper:ResetErrorStatus() .
            END FINALLY.
        END.

        FINALLY:
            DATASET dsAttributeValue:EMPTY-DATASET ().
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributeParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns a single SmartAttributeValue record to the client
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectTypeDesignAttribute (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                     poParameter AS Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributeParameter):

        DEFINE VARIABLE oRequest         AS FetchDataRequest          NO-UNDO .
        DEFINE VARIABLE oObjectTypeModel AS ObjectTypeDatasetModel    NO-UNDO .

        oObjectTypeModel = NEW ObjectTypeDatasetModel () .
        oObjectTypeModel:SmartObjectType:Filter:ObjectTypeName:EQ (poParameter:ObjectTypeName):Run () .

        BufferAssert:IsAvailable (oObjectTypeModel:SmartObjectType:QueryHandle:GET-BUFFER-HANDLE (1)) .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U +
                                                                                   '  and eSmartAttributeValue.AttributeLabel             = &2':U,
                                                     QUOTER (oObjectTypeModel:SmartObjectType:ObjectTypeGuid),
                                                     QUOTER (poParameter:AttributeLabel)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            CREATE eDesignAttributeValue.

            BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue .

            FIND eSmartAttribute WHERE eSmartAttribute.AttributeLabel = eSmartAttributeValue.AttributeLabel NO-ERROR .
            IF AVAILABLE eSmartAttribute THEN
                BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .

            ASSIGN eDesignAttributeValue.IsInherited = FALSE .

            FINALLY:
                ErrorHelper:ResetErrorStatus() .
            END FINALLY.
        END.

        FINALLY:
            DATASET dsAttributeValue:EMPTY-DATASET ().
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oAttributeGroup AS AttributeGroupDatasetModel NO-UNDO .

        IF NOT lPopulateCalculatedFields THEN
            RETURN .

        oAttributeGroup = NEW AttributeGroupDatasetModel () .

        FOR EACH eSmartAttribute BREAK BY eSmartAttribute.AttributeGroupGuid:

            IF FIRST-OF (eSmartAttribute.AttributeGroupGuid) THEN DO:
                oAttributeGroup:SmartAttributeGroup:Filter:Reset ().
                oAttributeGroup:SmartAttributeGroup:Filter:AttributeGroupGuid:EQ (eSmartAttribute.AttributeGroupGuid):Run () .
            END.

            IF oAttributeGroup:SmartAttributeGroup:Available THEN
                ASSIGN eSmartAttribute.AttributeGroupName = oAttributeGroup:SmartAttributeGroup:AttributeGroupName .
           ELSE
                ASSIGN eSmartAttribute.AttributeGroupName = "":U .
        END.

    END METHOD.

    @InvokeMethod (template="", parameterClassName="Consultingwerk.ISerializable", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Stores Object Master Attributes
        Notes:   The input dataset contains a single buffer per object type to be stored
                 Each record must reference the object master using the _ObjectMasterGuid
                 field, ignores the eSmartLink and eSmartPage temp-tables as those might be
                 contained in the ProDataset for storing links and pages
        @param phDataset INPUT-OUTPUT DATASET-HANDLE
        @param poParameter The Parameter Object for this method, ignored
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObjectMasterAttributes (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                    poParameter AS Consultingwerk.ISerializable):

        DEFINE VARIABLE iBuffer       AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE iField        AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE oAttribute    AS AttributeDatasetModel        NO-UNDO .
        DEFINE VARIABLE oCacheFactory AS IRepositoryCacheTableFactory NO-UNDO .

        HandleAssert:ValidHandle(phDataset, "phDataset":U) .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW RepositoryCacheTableFactory()"} .

        oObjectMaster = NEW ObjectMasterDatasetModel() .
        oAttribute    = NEW AttributeDatasetModel() .

        bufferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:

            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (iBuffer) .

            IF hBuffer:NAME = "eSmartLink":U OR hBuffer:NAME = "eSmartPage":U THEN
                NEXT bufferLoop .

            BufferAssert:HasField(hBuffer, "_ObjectMasterGuid":U) .

            ASSIGN hQuery       = QueryHelper:CreatePreparedQuery (hBuffer) .

            DO WHILE NOT hQuery:QUERY-OFF-END:

                oObjectMaster:SmartObjectMaster:Filter:Reset () .
                oObjectMaster:SmartObjectMaster:Filter:ObjectMasterGuid:EQ (hBuffer::_ObjectMasterGuid):Run() .

                IF NOT oObjectMaster:SmartObjectMaster:Available THEN
                    UNDO, THROW NEW RepositoryException (SUBSTITUTE ("Invalid Object Master GUID: &1":U,
                                                                     hBuffer::_ObjectMasterGuid), 0) .

                THIS-OBJECT:FetchData (NEW FetchDataRequest ("eSmartAttributeValue":U,
                                                             SUBSTITUTE ("for each eSmartAttributeValue where eSmartAttributeValue.ObjectMasterGuid = &1":U,
                                                                         QUOTER (hBuffer::_ObjectMasterGuid)))) .

                THIS-OBJECT:TrackingChanges = TRUE .

                fieldLoop:
                DO iField = 1 TO hBuffer:NUM-FIELDS:

                    ASSIGN hField = hBuffer:BUFFER-FIELD (iField) .

                    IF hField:NAME BEGINS "_":U THEN
                        NEXT fieldLoop .

                    /* When the field's value is the initial value, we will delete the existing value */
                    IF hField:BUFFER-VALUE = hField:DEFAULT-VALUE AND
                       (BufferHelper:HasField (hBuffer, "_ExplicitAttributeValues":U) = FALSE OR
                        LOOKUP (hField:NAME, hBuffer::_ExplicitAttributeValues) = 0) THEN DO:

                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.AttributeLabel = hField:NAME) THEN DO:
                            FIND FIRST eSmartAttributeValue WHERE eSmartAttributeValue.AttributeLabel = hField:NAME NO-ERROR .
                            DELETE eSmartAttributeValue .
                        END.
                    END.
                    ELSE DO:
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.AttributeLabel = hField:NAME) THEN
                            FIND FIRST eSmartAttributeValue WHERE eSmartAttributeValue.AttributeLabel = hField:NAME NO-ERROR .
                        ELSE DO:
                            CREATE eSmartAttributeValue .
                            ASSIGN eSmartAttributeValue.AttributeLabel     = hField:NAME
                                   eSmartAttributeValue.AttributeValueGuid = ? .
                        END.

                        oAttribute:SmartAttribute:Filter:Reset () .
                        oAttribute:SmartAttribute:Filter:AttributeLabel:EQ (hField:NAME):Run () .

                        IF NOT oAttribute:SmartAttribute:Available THEN
                            UNDO, THROW NEW RepositoryException (SUBSTITUTE ("Invalid Attribute: &1":U,
                                                                             hField:NAME), 0) .

                        ASSIGN eSmartAttributeValue.ObjectTypeGuid            = oObjectMaster:SmartObjectMaster:ObjectTypeGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T */
                               eSmartAttributeValue.ObjectMasterGuid          = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T */
                               eSmartAttributeValue.ContainerObjectMasterGuid = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T */
                               eSmartAttributeValue.ObjectInstanceGuid        = "" /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T */
                               eSmartAttributeValue.ConstantValue             = oAttribute:SmartAttribute:ConstantLevel = "MASTER":U /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Constant Value":T */
                               eSmartAttributeValue.AttributeLabel            = oAttribute:SmartAttribute:AttributeLabel /* AS CHARACTER FORMAT "x(40)":U LABEL "Attribute Label":T */
                               eSmartAttributeValue.AppliesAtRuntime          = ? /* applies only to object type attributes */ /* AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "AppliesAtRuntime":T */
                               .

                        CASE oAttribute:SmartAttribute:RepositoryType:
                            WHEN "INTEGER":U THEN
                               eSmartAttributeValue.IntegerValue              = hField:BUFFER-VALUE . /* AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Integer Value":T */
                            WHEN "INT64":U THEN
                               eSmartAttributeValue.Int64Value                = hField:BUFFER-VALUE . /* AS INT64 FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Int64 value":T */
                            WHEN "DATE":U THEN
                               eSmartAttributeValue.DateValue                 = hField:BUFFER-VALUE . /* AS DATE FORMAT "99/99/9999":U INIT ? LABEL "Date Value":T */
                            WHEN "DATETIME":U THEN
                               eSmartAttributeValue.DateTimeValue             = hField:BUFFER-VALUE . /* AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INIT ? LABEL "DateTime Value":T */
                            WHEN "DATETIME-TZ":U THEN
                               eSmartAttributeValue.DateTimeTzValue           = hField:BUFFER-VALUE . /* AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "DateTime-TZ Value":T */
                            WHEN "DECIMAL":U THEN
                               eSmartAttributeValue.DecimalValue              = hField:BUFFER-VALUE . /* AS DECIMAL FORMAT "->>,>>9.99":U INIT "0":U LABEL "Decimal Value":T */
                            WHEN "LOGICAL":U THEN
                               eSmartAttributeValue.LogicalValue              = hField:BUFFER-VALUE . /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Logical Value":T */
                            WHEN "RAW":U THEN
                               eSmartAttributeValue.RawValue                  = hField:BUFFER-VALUE . /* AS RAW */
                            OTHERWISE
                               eSmartAttributeValue.CharacterValue            = hField:BUFFER-VALUE . /* AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T */
                        END CASE .
                    END.
                END.

                THIS-OBJECT:SaveChanges() .
                DatasetHelper:ThrowDatasetErrors(DATASET dsAttributeValue:HANDLE) .
                DATASET dsAttributeValue:ACCEPT-CHANGES () .

                hQuery:GET-NEXT () .
            END.
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset}
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.StoreObjectInstanceDesignAttributeParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Stores modified attribute values for an object instance
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObjectInstanceDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                            poParameter AS Consultingwerk.SmartFramework.Repository.Class.StoreObjectInstanceDesignAttributeParameter):

        DEFINE VARIABLE oRequest        AS FetchDataRequest         NO-UNDO .
        DEFINE VARIABLE oObjectInstance AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE oObjectMaster   AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE oAttribute      AS AttributeDatasetModel    NO-UNDO .

        DEFINE VARIABLE cObjectInstanceGuid        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cObjectMasterGuid          AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cObjectTypeGuid            AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cContainerObjectMasterGuid AS CHARACTER NO-UNDO .

        ObjectAssert:IsValid(poParameter, "poParameter":U) .

        ASSIGN oObjectInstance = NEW ObjectMasterDatasetModel () .

        oObjectInstance:SmartObjectInstance:Filter:ObjectInstanceGuid:EQ(poParameter:ObjectInstanceGuid):Run () .

        BufferAssert:IsAvailable (oObjectInstance:SmartObjectInstance) .

        ASSIGN oObjectMaster = NEW ObjectMasterDatasetModel(oObjectInstance:SmartObjectInstance:ObjectMasterGuid) .

        BufferAssert:IsAvailable (oObjectMaster:SmartObjectMaster) .

        ASSIGN cObjectInstanceGuid        = poParameter:ObjectInstanceGuid
               cObjectMasterGuid          = oObjectMaster:SmartObjectMaster:ObjectMasterGuid
               cObjectTypeGuid            = oObjectMaster:SmartObjectMaster:ObjectTypeGuid
               cContainerObjectMasterGuid = oObjectInstance:SmartObjectInstance:ContainerObjectMasterGuid .

        oRequest = NEW FetchDataRequest ("eSmartAttributeValue":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectInstanceGuid         = &1':U,
                                                     QUOTER (poParameter:ObjectInstanceGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        oAttribute = NEW AttributeDatasetModel() .

        THIS-OBJECT:TrackingChanges = TRUE .

        attributeLoop:
        FOR EACH eDesignAttributeValueBefore ON ERROR UNDO, THROW:

            CASE ROW-STATE (eDesignAttributeValueBefore):

                WHEN ROW-CREATED THEN
                    UNDO, THROW NEW Exception ("Unexcepted ROW-CREATED record in buffer eDesignAttributeValueBefore."{&TRAN}, 0) .

                WHEN ROW-MODIFIED THEN DO:

                    BufferHelper:FindAfterBuffer (BUFFER eDesignAttributeValueBefore:HANDLE) .

                    oAttribute:SmartAttribute:Filter:Reset () .
                    oAttribute:SmartAttribute:Filter:AttributeLabel:EQ (eDesignAttributeValue.AttributeLabel):Run () .

                    IF eDesignAttributeValue.IsInherited = FALSE THEN DO:
                        /* Update or create attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = oObjectInstance:SmartObjectInstance:ContainerObjectMasterGuid
                                                                  AND eSmartAttributeValue.ObjectInstanceGuid        = poParameter:ObjectInstanceGuid
                                                                  AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel) THEN
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = oObjectInstance:SmartObjectInstance:ContainerObjectMasterGuid
                                                        AND eSmartAttributeValue.ObjectInstanceGuid        = poParameter:ObjectInstanceGuid
                                                        AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                        ELSE DO:
                            CREATE eSmartAttributeValue .
                            ASSIGN eSmartAttributeValue.AttributeValueGuid = ? .
                        END.

                        BUFFER-COPY eDesignAttributeValue EXCEPT AttributeValueGuid TO eSmartAttributeValue

                            ASSIGN eSmartAttributeValue.ConstantValue             = oAttribute:SmartAttribute:ConstantLevel = "MASTER":U /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Constant Value":T */
                                   eSmartAttributeValue.AppliesAtRuntime          = ? /* applies only to object type attributes */ /* AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "AppliesAtRuntime":T */
                                   eSmartAttributeValue.ObjectInstanceGuid        = cObjectInstanceGuid
                                   eSmartAttributeValue.ObjectMasterGuid          = cObjectMasterGuid
                                   eSmartAttributeValue.ObjectTypeGuid            = cObjectTypeGuid
                                   eSmartAttributeValue.ContainerObjectMasterGuid = cContainerObjectMasterGuid
                                   .

                        RELEASE eSmartAttributeValue .
                    END.
                    ELSE DO:
                        /* Delete attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = oObjectInstance:SmartObjectInstance:ContainerObjectMasterGuid
                                                                  AND eSmartAttributeValue.ObjectInstanceGuid        = poParameter:ObjectInstanceGuid
                                                                  AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel) THEN DO:
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = oObjectInstance:SmartObjectInstance:ContainerObjectMasterGuid
                                                        AND eSmartAttributeValue.ObjectInstanceGuid        = poParameter:ObjectInstanceGuid
                                                        AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                            DELETE eSmartAttributeValue .
                        END.
                    END.
                END.

                WHEN ROW-DELETED THEN
                        UNDO, THROW NEW Exception ("Unexcepted ROW-DELETED record in buffer eDesignAttributeValueBefore."{&TRAN}, 0) .
            END CASE.

        END.

        THIS-OBJECT:SaveChanges() .

        DatasetHelper:ThrowDatasetErrors (DATASET dsAttributeValue:HANDLE) .

        FINALLY:
            THIS-OBJECT:TrackingChanges = FALSE .
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.StoreObjectMasterDesignAttributeParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Stores modified attribute values for an object master
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObjectMasterDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                          poParameter AS Consultingwerk.SmartFramework.Repository.Class.StoreObjectMasterDesignAttributeParameter):

        DEFINE VARIABLE oRequest      AS FetchDataRequest         NO-UNDO .
        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE oAttribute    AS AttributeDatasetModel    NO-UNDO .

        FOR EACH eDesignAttributeValue WHERE ROW-STATE (eDesignAttributeValue) = ROW-CREATED ON ERROR UNDO, THROW:
            ASSIGN eDesignAttributeValue.ObjectTypeGuid = poParameter:ObjectMasterGuid .
        END.

        oRequest = NEW FetchDataRequest ("eSmartAttributeValue":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ContainerObjectMasterGuid  = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                     QUOTER (poParameter:ObjectMasterGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        oObjectMaster = NEW ObjectMasterDatasetModel() .
        oObjectMaster:SmartObjectMaster:Filter:Reset () .
        oObjectMaster:SmartObjectMaster:Filter:ObjectMasterGuid:EQ (poParameter:ObjectMasterGuid):Run() .

        oAttribute = NEW AttributeDatasetModel() .

        THIS-OBJECT:TrackingChanges = TRUE .

        attributeLoop:
        FOR EACH eDesignAttributeValueBefore ON ERROR UNDO, THROW:

            CASE ROW-STATE (eDesignAttributeValueBefore):

                WHEN ROW-CREATED THEN DO:
                    BufferHelper:FindAfterBuffer (BUFFER eDesignAttributeValueBefore:HANDLE) .

                    oAttribute:SmartAttribute:Filter:Reset () .
                    oAttribute:SmartAttribute:Filter:AttributeLabel:EQ (eDesignAttributeValue.AttributeLabel):Run () .

                    CREATE eSmartAttributeValue.
                    BUFFER-COPY eDesignAttributeValue EXCEPT AttributeValueGuid TO eSmartAttributeValue

                        ASSIGN eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                               eSmartAttributeValue.ObjectInstanceGuid        = "":U
                               eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel
                               eSmartAttributeValue.ObjectTypeGuid            = oObjectMaster:SmartObjectMaster:ObjectTypeGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T */
                               eSmartAttributeValue.ObjectMasterGuid          = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T */
                               eSmartAttributeValue.ContainerObjectMasterGuid = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T */
                               eSmartAttributeValue.ObjectInstanceGuid        = "" /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T */
                               eSmartAttributeValue.ConstantValue             = oAttribute:SmartAttribute:ConstantLevel = "MASTER":U /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Constant Value":T */
                               eSmartAttributeValue.AppliesAtRuntime          = ? /* applies only to object type attributes */ /* AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "AppliesAtRuntime":T */
                               eSmartAttributeValue.AttributeValueGuid        = ?
                               .

                    RELEASE eSmartAttributeValue .
                END.

                WHEN ROW-MODIFIED THEN DO:

                    BufferHelper:FindAfterBuffer (BUFFER eDesignAttributeValueBefore:HANDLE) .

                    oAttribute:SmartAttribute:Filter:Reset () .
                    oAttribute:SmartAttribute:Filter:AttributeLabel:EQ (eDesignAttributeValue.AttributeLabel):Run () .

                    IF eDesignAttributeValue.IsInherited = FALSE THEN DO:
                        /* Update or create attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                                  AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                                  AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel) THEN
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                        AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                        AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                        ELSE DO:
                            CREATE eSmartAttributeValue .

                            ASSIGN eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                   eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                   eSmartAttributeValue.AttributeValueGuid        = ?
                                   eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel .
                        END.

                        BUFFER-COPY eDesignAttributeValue EXCEPT AttributeValueGuid TO eSmartAttributeValue

                            ASSIGN eSmartAttributeValue.ObjectTypeGuid            = oObjectMaster:SmartObjectMaster:ObjectTypeGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T */
                                   eSmartAttributeValue.ObjectMasterGuid          = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T */
                                   eSmartAttributeValue.ContainerObjectMasterGuid = oObjectMaster:SmartObjectMaster:ObjectMasterGuid /* AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T */
                                   eSmartAttributeValue.ObjectInstanceGuid        = "" /* AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T */
                                   eSmartAttributeValue.ConstantValue             = oAttribute:SmartAttribute:ConstantLevel = "MASTER":U /* AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Constant Value":T */
                                   eSmartAttributeValue.AppliesAtRuntime          = ? /* applies only to object type attributes */ /* AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "AppliesAtRuntime":T */
                                   .

                        RELEASE eSmartAttributeValue .
                    END.
                    ELSE DO:
                        /* Delete attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                                  AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                                  AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel) THEN DO:
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                        AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                        AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                            DELETE eSmartAttributeValue .
                        END.
                    END.
                END.

                WHEN ROW-DELETED THEN DO:
                    IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                              AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                              AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel) THEN DO:
                        FIND eSmartAttributeValue WHERE eSmartAttributeValue.ContainerObjectMasterGuid = poParameter:ObjectMasterGuid
                                                    AND eSmartAttributeValue.ObjectInstanceGuid        = "":U
                                                    AND eSmartAttributeValue.AttributeLabel            = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                        DELETE eSmartAttributeValue .
                    END.
                END.

            END CASE.

        END.

        THIS-OBJECT:SaveChanges() .

        DatasetHelper:ThrowDatasetErrors (DATASET dsAttributeValue:HANDLE) .

        FINALLY:
            THIS-OBJECT:TrackingChanges = FALSE .
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.StoreObjectTypeDesignAttributeParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Stores modified attribute values for an object type
        Notes:
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObjectTypeDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue,
                                                        poParameter AS Consultingwerk.SmartFramework.Repository.Class.StoreObjectTypeDesignAttributeParameter):

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .

        FOR EACH eDesignAttributeValue WHERE ROW-STATE (eDesignAttributeValue) = ROW-CREATED ON ERROR UNDO, THROW:
            ASSIGN eDesignAttributeValue.ObjectTypeGuId = poParameter:ObjectTypeGuid .
        END.

        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U +
                                                                                   '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                     QUOTER (poParameter:ObjectTypeGuid)),
                                         0) .

        THIS-OBJECT:FetchData (oRequest) .

        THIS-OBJECT:TrackingChanges = TRUE .

        FOR EACH eDesignAttributeValueBefore ON ERROR UNDO, THROW:

            CASE ROW-STATE (eDesignAttributeValueBefore):

                WHEN ROW-CREATED THEN DO:
                    BufferHelper:FindAfterBuffer (BUFFER eDesignAttributeValueBefore:HANDLE) .
                    CREATE eSmartAttributeValue.
                    BUFFER-COPY eDesignAttributeValue TO eSmartAttributeValue .

                    RELEASE eSmartAttributeValue .
                END.

                WHEN ROW-MODIFIED THEN DO:

                    BufferHelper:FindAfterBuffer (BUFFER eDesignAttributeValueBefore:HANDLE) .

                    IF eDesignAttributeValue.IsInherited = FALSE THEN DO:
                        /* Update or create attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                                  AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel) THEN
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                        AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                        ELSE DO:
                            CREATE eSmartAttributeValue .
                            ASSIGN eSmartAttributeValue.AttributeValueGuid = ?
                                   eSmartAttributeValue.ObjectTypeGuid     = poParameter:ObjectTypeGuid
                                   eSmartAttributeValue.AttributeLabel     = eDesignAttributeValueBefore.AttributeLabel .
                        END.

                        BUFFER-COPY eDesignAttributeValue EXCEPT AttributeValueGuid TO eSmartAttributeValue .

                        RELEASE eSmartAttributeValue .
                    END.
                    ELSE DO:
                        /* Delete attribute from this type */
                        IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                                  AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel) THEN DO:
                            FIND eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                        AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                            DELETE eSmartAttributeValue .
                        END.
                    END.
                END.

                WHEN ROW-DELETED THEN DO:
                    IF CAN-FIND (FIRST eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                              AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel) THEN DO:
                        FIND eSmartAttributeValue WHERE eSmartAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid
                                                    AND eSmartAttributeValue.AttributeLabel = eDesignAttributeValueBefore.AttributeLabel NO-ERROR.
                        DELETE eSmartAttributeValue .
                    END.
                END.

            END CASE.

        END.

        THIS-OBJECT:SaveChanges() .

        DatasetHelper:ThrowDatasetErrors (DATASET dsAttributeValue:HANDLE) .

        FINALLY:
            THIS-OBJECT:TrackingChanges = FALSE .
        END FINALLY.

    END METHOD .

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

    END METHOD.

END CLASS.
