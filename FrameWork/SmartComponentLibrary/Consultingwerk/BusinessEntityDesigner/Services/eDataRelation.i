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
    File        : eDataRelation.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06.11.2016 18:48:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="DataRelationName").

DEFINE {&ACCESS} TEMP-TABLE eDataRelation{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eDataRelationBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityName AS CHARACTER FORMAT "X(8)":U
    FIELD DataRelationName AS CHARACTER FORMAT "X(8)":U
    FIELD ParentTempTableName AS CHARACTER FORMAT "X(8)":U
    FIELD ChildTempTableName AS CHARACTER FORMAT "X(8)":U
    FIELD RelationFields AS CHARACTER FORMAT "X(8)":U
    FIELD DataRelationDescription AS CHARACTER FORMAT "X(8)":U
    FIELD DataRelationReposition AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD DataRelationNested AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD DataRelationForeignKeyHidden AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD DataRelationNotActive AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD DataRelationRecursive AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD ChildCardinality AS CHARACTER FORMAT "X(4)":U INIT "MANY":U LABEL "Child Cardinality":T
    FIELD ChildPropertyName AS CHARACTER FORMAT "X(80)":U LABEL "Child Property Name":T

    INDEX DataRelationName AS UNIQUE PRIMARY DataRelationName ASCENDING
    INDEX Tables AS UNIQUE ParentTempTableName ASCENDING ChildTempTableName ASCENDING
    INDEX BusinessEntity BusinessEntityName ASCENDING DataRelationName ASCENDING
    INDEX ChildTempTableName ChildTempTableName ASCENDING DataRelationNotActive ASCENDING

    .

    