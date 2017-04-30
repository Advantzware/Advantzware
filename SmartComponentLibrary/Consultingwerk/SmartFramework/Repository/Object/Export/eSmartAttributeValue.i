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
    File        : eSmartAttributeValue.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 02.01.2017 11:08:29
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="AttributeValueGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartAttributeValue{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartAttributeValueBefore{&SUFFIX} &ENDIF
    FIELD AttributeValueGuid AS CHARACTER FORMAT "x(36)":U LABEL "AttributeValueGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T XML-NODE-TYPE "attribute":U
    FIELD ConstantValue AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Constant Value":T XML-NODE-TYPE "attribute":U
    FIELD AttributeLabel AS CHARACTER FORMAT "x(40)":U LABEL "Attribute Label":T XML-NODE-TYPE "attribute":U
    FIELD CharacterValue AS CHARACTER FORMAT "x(80)":U LABEL "Character Value":T XML-NODE-TYPE "attribute":U
    FIELD IntegerValue AS INTEGER FORMAT "->,>>>,>>9":U INITIAL "0":U LABEL "Integer Value":T XML-NODE-TYPE "attribute":U
    FIELD Int64Value AS INT64 FORMAT "->,>>>,>>9":U INITIAL "0":U LABEL "Int64 value":T XML-NODE-TYPE "attribute":U
    FIELD DateValue AS DATE FORMAT "99/99/9999":U INITIAL ? LABEL "Date Value":T XML-NODE-TYPE "attribute":U
    FIELD DateTimeValue AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INITIAL ? LABEL "DateTime Value":T XML-NODE-TYPE "attribute":U
    FIELD DateTimeTzValue AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INITIAL ? LABEL "DateTime-TZ Value":T XML-NODE-TYPE "attribute":U
    FIELD DecimalValue AS DECIMAL FORMAT "->>,>>9.99":U INITIAL "0":U LABEL "Decimal Value":T XML-NODE-TYPE "attribute":U
    FIELD LogicalValue AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Logical Value":T XML-NODE-TYPE "attribute":U
    FIELD RawValue AS RAW XML-NODE-TYPE "attribute":U
    FIELD AppliesAtRuntime AS LOGICAL FORMAT "yes/no":U INITIAL "yes":U LABEL "AppliesAtRuntime":T XML-NODE-TYPE "attribute":U

    INDEX AttributeValueGuid AS UNIQUE PRIMARY AttributeValueGuid ASCENDING

    .

