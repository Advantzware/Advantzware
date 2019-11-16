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
    File        : eSmartAttributeValue.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.10.2015 20:22:29
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartAttributeValue{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartAttributeValueBefore{&SUFFIX} &ENDIF
    FIELD AttributeValueGuid AS CHARACTER FORMAT "x(36)":U LABEL "AttributeValueGuid":T
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T
    FIELD ObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T
    FIELD ConstantValue AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Constant Value":T
    FIELD AttributeLabel AS CHARACTER FORMAT "x(40)":U LABEL "Attribute Label":T
    FIELD CharacterValue AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T
    FIELD IntegerValue AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Integer Value":T
    FIELD Int64Value AS INT64 FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Int64 value":T
    FIELD DateValue AS DATE FORMAT "99/99/9999":U INIT ? LABEL "Date Value":T
    FIELD DateTimeValue AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INIT ? LABEL "DateTime Value":T
    FIELD DateTimeTzValue AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "DateTime-TZ Value":T
    FIELD DecimalValue AS DECIMAL FORMAT "->>,>>9.99":U INIT "0":U LABEL "Decimal Value":T
    FIELD LogicalValue AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Logical Value":T
    FIELD RawValue AS RAW
    FIELD AppliesAtRuntime AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "AppliesAtRuntime":T

    INDEX AttributeLabelObjectType AttributeLabel ASCENDING ObjectTypeGuid ASCENDING
    INDEX AttributeValueGuid AS UNIQUE PRIMARY AttributeValueGuid ASCENDING
    INDEX ContainerInstanceApplies ContainerObjectMasterGuid ASCENDING ObjectInstanceGuid ASCENDING AppliesAtRuntime ASCENDING
    INDEX ContainerObjectMasterGuid ContainerObjectMasterGuid ASCENDING
    INDEX ObjectInstanceLabel ObjectInstanceGuid ASCENDING AttributeLabel ASCENDING
    INDEX ObjectMasterGuid ObjectMasterGuid ASCENDING
    INDEX ObjectTypeMasterInstanceLabel AS UNIQUE ObjectTypeGuid ASCENDING ObjectMasterGuid ASCENDING ObjectInstanceGuid ASCENDING AttributeLabel ASCENDING

    .

    