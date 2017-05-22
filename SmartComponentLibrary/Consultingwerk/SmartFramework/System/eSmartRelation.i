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
    File        : eSmartRelation.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 24.03.2016 13:07:15
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.RelationBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartRelation{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartRelationBefore{&SUFFIX} &ENDIF
    FIELD RelationGuid AS CHARACTER FORMAT "x(36)":U LABEL "RelationGuid":T
    FIELD ParentTableGUID AS CHARACTER FORMAT "x(36)":U LABEL "ParentTableGUID":T
    FIELD ParentTableName AS CHARACTER FORMAT "X(80)":U
    FIELD ChildTableGUID AS CHARACTER FORMAT "x(36)":U LABEL "ChildTableGUID":T
    FIELD ChildTableName AS CHARACTER FORMAT "X(80)":U
    FIELD RelationName AS CHARACTER FORMAT "x(80)":U LABEL "Name":T
    FIELD RelationDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD RelationFieldPair AS CHARACTER FORMAT "x(80)":U LABEL "RelationFieldPair(s)":T
    FIELD ParentRoleDescription AS CHARACTER FORMAT "x(80)":U LABEL "Parent Role Description":T
    FIELD ChildRoleDescription AS CHARACTER FORMAT "x(80)":U LABEL "Child Role Description":T
    FIELD ParentCardinality AS CHARACTER FORMAT "x(3)":U LABEL "Parent Cardinality":T
    FIELD ChildCardinality AS CHARACTER FORMAT "x(3)":U LABEL "Child Cardinality":T
    FIELD OnParentDelete AS CHARACTER FORMAT "x(8)":U LABEL "On Parent Delete":T
    FIELD DefaultParentRelation AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Default Parent Relation":T
    FIELD SmartAttachments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Attachments":T
    FIELD SmartComments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Comments":T
    FIELD SmartRecordKey AS CHARACTER FORMAT "X(80)":U
    FIELD Inactive AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Inactive":T

    INDEX ChildTableGuid ChildTableGUID ASCENDING
    INDEX ParentTableGuid ParentTableGUID ASCENDING
    INDEX RelationGuid AS UNIQUE PRIMARY RelationGuid ASCENDING
    INDEX RelationName RelationName ASCENDING
    INDEX ParentTableName ParentTableName ASCENDING
    INDEX ChildTableName ChildTableName ASCENDING
    INDEX DefaultParentRelation ChildTableName ASCENDING DefaultParentRelation ASCENDING

    .

    