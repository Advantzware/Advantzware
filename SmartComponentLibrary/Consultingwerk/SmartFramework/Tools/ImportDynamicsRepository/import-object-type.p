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
    File        : import-object-type.p
    Purpose     : Imports Dynamics Object Types

    Syntax      :

    Description :

    Author(s)   :
    Created     : Thu Oct 15 16:34:49 CEST 2015
    Notes       : Requires ICFDB connection during compilation
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.SmartFramework.Tools.ImportDynamicsRepository.* FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .

@SuppressUnusedWarnings.
DEFINE VARIABLE cID         AS CHARACTER NO-UNDO .
DEFINE VARIABLE iObjectType AS INTEGER   NO-UNDO .
DEFINE VARIABLE iAttribute  AS INTEGER   NO-UNDO .

/* ***************************  Main Block  *************************** */

FOR EACH SmartObjectType EXCLUSIVE-LOCK:
    DELETE SmartObjectType .
END.

FOR EACH SmartAttributeValue EXCLUSIVE-LOCK:
    DELETE SmartAttributeValue .
END.

&IF DBTYPE("ICFDB") = ? &THEN
MESSAGE "THIS PROGRAM NEEDS TO BE COMPILED WITH BOTH SmartDB and ICFDB connected." VIEW-AS ALERT-BOX.
&ELSE

FOR EACH gsc_object_type NO-LOCK ON ERROR UNDO, THROW:

    ASSIGN cID         = ObjValueHelper:ObjToId (gsc_object_type.object_type_obj)
           iObjectType = iObjectType + 1.

    FIND SmartObjectType WHERE SmartObjectType.ObjectTypeGuid = cID EXCLUSIVE-LOCK NO-ERROR .

    IF NOT AVAILABLE SmartObjectType THEN DO:
        ErrorHelper:ResetErrorStatus() .

        CREATE SmartObjectType.
        ASSIGN SmartObjectType.ObjectTypeGuid = cID
               SmartObjectType.StoreInstances = "":U
               SmartObjectType.ContainerType  = FALSE .
    END.

    ASSIGN SmartObjectType.ObjectTypeName        = gsc_object_type.object_type_code
           SmartObjectType.ObjectTypeDescription = gsc_object_type.object_type_description
           SmartObjectType.DeploymentType        = gsc_object_type.deployment_type
           SmartObjectType.StaticObject          = gsc_object_type.static_object .

    IF gsc_object_type.extends_object_type_obj > 0 THEN
        ASSIGN SmartObjectType.ExtendsObjectTypeGuid = ObjValueHelper:ObjToId (gsc_object_type.extends_object_type_obj) .

    IF gsc_object_type.custom_object_type_obj > 0 THEN
        ASSIGN SmartObjectType.CustomObjectTypeGuid = ObjValueHelper:ObjToId (gsc_object_type.custom_object_type_obj) .

    FOR EACH ryc_attribute_value WHERE ryc_attribute_value.object_type_obj     = gsc_object_type.object_type_obj
                                   AND ryc_attribute_value.smartobject_obj     = 0
                                   AND ryc_attribute_value.object_instance_obj = 0 NO-LOCK ON ERROR UNDO, THROW:

        ASSIGN cID        = ObjValueHelper:ObjToId (ryc_attribute_value.attribute_value_obj)
               iAttribute = iAttribute + 1 .

        FIND SmartAttributeValue WHERE SmartAttributeValue.AttributeValueGuid = cID EXCLUSIVE-LOCK NO-ERROR .

        IF NOT AVAILABLE SmartAttributeValue THEN DO:
            ErrorHelper:ResetErrorStatus() .

            CREATE SmartAttributeValue.
            ASSIGN SmartAttributeValue.AttributeValueGuid = cID .
        END.

        ASSIGN SmartAttributeValue.ObjectTypeGuid             = ObjValueHelper:ObjToId (ryc_attribute_value.object_type_obj)
               SmartAttributeValue.ObjectMasterGuid           = "":U
               SmartAttributeValue.ContainerObjectMasterGuid  = "":U
               SmartAttributeValue.ObjectInstanceGuid         = "":U
               SmartAttributeValue.ConstantValue              = ryc_attribute_value.constant_value
               SmartAttributeValue.AttributeLabel             = ryc_attribute_value.attribute_label
               SmartAttributeValue.CharacterValue             = ryc_attribute_value.character_value
               SmartAttributeValue.IntegerValue               = ryc_attribute_value.integer_value
               SmartAttributeValue.DateValue                  = ryc_attribute_value.date_value
               SmartAttributeValue.DateTimeValue              = ?
               SmartAttributeValue.DateTimeTzValue            = ?
               SmartAttributeValue.Int64Value                 = ?
               SmartAttributeValue.DecimalValue               = ryc_attribute_value.decimal_value
               SmartAttributeValue.LogicalValue               = ryc_attribute_value.logical_value
               SmartAttributeValue.RawValue                   = ryc_attribute_value.raw_value
/*                                                              = ryc_attribute_value.primary_smartobject_obj*/
/*                                                              = ryc_attribute_value.render_type_obj        */
               SmartAttributeValue.AppliesAtRuntime           = ryc_attribute_value.applies_at_runtime .

    END.
END .

&ENDIF

MESSAGE "Processed":U SKIP
        iObjectType "Object Types":U SKIP
        iAttribute  "Attributes":U
    VIEW-AS ALERT-BOX.

CATCH err AS Progress.Lang.Error:
    ErrorHelper:ShowErrorMessage (err) .
END CATCH.
