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
    File        : import-attributes.p
    Purpose     : Imports Dynamics Attributes

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 15 21:29:19 CEST 2015
    Notes       : Requires ICFDB connection during compilation
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.SmartFramework.Tools.ImportDynamicsRepository.* FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .

DEFINE VARIABLE cID AS CHARACTER NO-UNDO .  

DEFINE VARIABLE iAttributeGroup AS INTEGER NO-UNDO.
DEFINE VARIABLE iAttribute      AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

FUNCTION DataType RETURNS CHARACTER (piDataType AS INTEGER)
    FORWARD . 
   
&IF DBTYPE("ICFDB") = ? &THEN
MESSAGE "THIS PROGRAM NEEDS TO BE COMPILED WITH BOTH SmartDB and ICFDB connected." VIEW-AS ALERT-BOX.
&ELSE

FOR EACH ryc_attribute_group NO-LOCK ON ERROR UNDO, THROW:

    ASSIGN cID             = ObjValueHelper:ObjToId (ryc_attribute_group.attribute_group_obj) 
           iAttributeGroup = iAttributeGroup + 1 .

    FIND SmartAttributeGroup WHERE SmartAttributeGroup.AttributeGroupGuid = cID EXCLUSIVE-LOCK NO-ERROR . 

    IF NOT AVAILABLE SmartAttributeGroup THEN DO:
        ErrorHelper:ResetErrorStatus() .
        
        CREATE SmartAttributeGroup.
        ASSIGN SmartAttributeGroup.AttributeGroupGuid = cID . 
    END. 

    ASSIGN SmartAttributeGroup.AttributeGroupName        = "ICF-":U + ryc_attribute_group.attribute_group_name
           SmartAttributeGroup.AttributeGroupDescription = ryc_attribute_group.attribute_group_narrative .
END.

FOR EACH ryc_attribute NO-LOCK ON ERROR UNDO, THROW:

    ASSIGN cID        = ObjValueHelper:ObjToId (ryc_attribute.attribute_obj) 
           iAttribute = iAttribute + 1 .

    FIND SmartAttribute WHERE SmartAttribute.AttributeGuid = cID EXCLUSIVE-LOCK NO-ERROR . 

    IF NOT AVAILABLE SmartAttribute THEN DO:
        ErrorHelper:ResetErrorStatus() .
        
        CREATE SmartAttribute.
        ASSIGN SmartAttribute.AttributeGuid = cID . 
    END. 

    ASSIGN SmartAttribute.AttributeGroupGuid  = ObjValueHelper:ObjToId (ryc_attribute.attribute_group_obj)
           SmartAttribute.AttributeLabel      = ryc_attribute.attribute_label
           SmartAttribute.TechnicalName       = ryc_attribute.attribute_label
           SmartAttribute.AttributeDesription = ryc_attribute.attribute_narrative
           SmartAttribute.RuntimeOnly         = ryc_attribute.runtime_only 
           SmartAttribute.ConstantLevel       = ryc_attribute.constant_level
           SmartAttribute.LookupType          = ryc_attribute.lookup_type
           SmartAttribute.LookupValues        = ryc_attribute.lookup_value
           SmartAttribute.VirtualProperty     = ryc_attribute.design_only
           SmartAttribute.RepositoryType      = DataType (ryc_attribute.data_type)
           SmartAttribute.PropertyOrEvent     = TRUE .
               
END.

&ENDIF

MESSAGE "Processed":U SKIP 
        iAttributeGroup "Attribute Groups":U SKIP 
        iAttribute  "Attributes":U
    VIEW-AS ALERT-BOX.

CATCH err AS Progress.Lang.Error:
    ErrorHelper:ShowErrorMessage (err) .    
END CATCH.    

FUNCTION DataType RETURNS CHARACTER (piDataType AS INTEGER):
    
    CASE piDataType:

        WHEN 1  THEN RETURN UPPER ("Character":U) .
        WHEN 2  THEN RETURN UPPER ("Date":U) .
        WHEN 3  THEN RETURN UPPER ("Logical":U) .
        WHEN 4  THEN RETURN UPPER ("Integer":U) .
        WHEN 5  THEN RETURN UPPER ("Decimal":U) .
        WHEN 7  THEN RETURN UPPER ("Recid":U) .
        WHEN 8  THEN RETURN UPPER ("Raw":U) .
        WHEN 9  THEN RETURN UPPER ("Rowid":U) .
        WHEN 10 THEN RETURN UPPER ("Handle":U) .
        WHEN 11 THEN RETURN UPPER ("Memptr":U) .
        WHEN 14 THEN RETURN UPPER ("Com-handle":U) . 
   
   END CASE . 

END.
