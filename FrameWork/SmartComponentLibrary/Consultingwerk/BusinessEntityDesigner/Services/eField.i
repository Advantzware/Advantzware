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
    File        : eField.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 01.12.2016 14:24:54
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="TempTableName,FieldName").

DEFINE {&ACCESS} TEMP-TABLE {&PREFIX}eField{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eFieldBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityName AS CHARACTER FORMAT "X(8)":U
    FIELD TempTableName AS CHARACTER FORMAT "X(8)":U
    FIELD FieldName AS CHARACTER FORMAT "X(8)":U
    FIELD FieldOrder AS INTEGER FORMAT ">>>,>>9":U
    FIELD FieldDataType AS CHARACTER FORMAT "X(8)":U
    FIELD FieldDescription AS CHARACTER FORMAT "X(8)":U
    FIELD FieldInitial AS CHARACTER FORMAT "X(8)":U
    FIELD FieldInitialUnknownValue AS LOGICAL FORMAT "yes/no":U LABEL "Initial Value Unknown":T
    FIELD FieldLabel AS CHARACTER FORMAT "X(8)":U
    FIELD FieldFormat AS CHARACTER FORMAT "X(8)":U
    FIELD FieldSerializeHidden AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD FieldSerializeName AS CHARACTER FORMAT "X(8)":U
    FIELD FieldXmlDataType AS CHARACTER FORMAT "X(8)":U
    FIELD FieldXmlNodeType AS CHARACTER FORMAT "X(8)":U
    FIELD FieldXmlNodeName AS CHARACTER FORMAT "X(8)":U
    FIELD FieldSource AS CHARACTER FORMAT "X(8)":U
    FIELD FieldExtent AS INTEGER FORMAT ">>9":U INIT "0":U
    FIELD FieldGetAccess AS CHARACTER FORMAT "X(8)":U INIT "PUBLIC":U LABEL "&Get Access":T
    FIELD FieldSetAccess AS CHARACTER FORMAT "X(8)":U INIT "PUBLIC":U LABEL "&Set Access":T
    FIELD FieldCalcExpression AS CHARACTER FORMAT "X(800)":U LABEL "Calc Expression":T
    FIELD FieldCaseSensitive AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U LABEL "Case Sensitive":T
    FIELD FieldCustomControlName AS CHARACTER FORMAT "X(80)":U LABEL "Field Custom Control Name":T
    FIELD FieldValidationInstance AS CLOB LABEL "Validation Instance":T
    FIELD FieldSemanticType AS CHARACTER FORMAT "X(20)":U LABEL "Field Semantic Type":T
    FIELD FieldSkipAssignment AS LOGICAL FORMAT "yes/no":U LABEL "Add field to AssignSkipDictionary":T

    INDEX FieldName AS UNIQUE PRIMARY TempTableName ASCENDING FieldName ASCENDING
    INDEX Order TempTableName ASCENDING FieldOrder ASCENDING

    .

