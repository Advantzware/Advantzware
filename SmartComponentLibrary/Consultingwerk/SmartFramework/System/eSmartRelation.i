/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    Created     : 26.06.2014 16:23:48
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartRelation NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartRelationBefore &ENDIF
    FIELD RelationGuid AS CHARACTER FORMAT "x(36)":U LABEL "RelationGuid":T SERIALIZE-NAME "RelationGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentTableGUID AS CHARACTER FORMAT "x(36)":U LABEL "ParentTableGUID":T SERIALIZE-NAME "ParentTableGUID":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentTableName AS CHARACTER FORMAT "X(80)":U
    FIELD ChildTableGUID AS CHARACTER FORMAT "x(36)":U LABEL "ChildTableGUID":T SERIALIZE-NAME "ChildTableGUID":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ChildTableName AS CHARACTER FORMAT "X(80)":U
    FIELD RelationName AS CHARACTER FORMAT "x(80)":U LABEL "Name":T SERIALIZE-NAME "RelationName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD RelationDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T SERIALIZE-NAME "RelationDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD RelationFieldPair AS CHARACTER FORMAT "x(80)":U LABEL "RelationFieldPair(s)":T SERIALIZE-NAME "RelationFieldPair":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentRoleDescription AS CHARACTER FORMAT "x(80)":U LABEL "Parent Role Description":T SERIALIZE-NAME "ParentRoleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ChildRoleDescription AS CHARACTER FORMAT "x(80)":U LABEL "Child Role Description":T SERIALIZE-NAME "ChildRoleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentCardinality AS CHARACTER FORMAT "x(3)":U LABEL "Parent Cardinality":T SERIALIZE-NAME "ParentCardinality":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ChildCardinality AS CHARACTER FORMAT "x(3)":U LABEL "Child Cardinality":T SERIALIZE-NAME "ChildCardinality":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD OnParentDelete AS CHARACTER FORMAT "x(8)":U LABEL "On Parent Delete":T SERIALIZE-NAME "OnParentDelete":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD DefaultParentRelation AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Default Parent Relation":T SERIALIZE-NAME "DefaultParentRelation":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U
    FIELD SmartAttachments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Attachments":T
    FIELD SmartComments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Comments":T
    FIELD SmartRecordKey AS CHARACTER FORMAT "X(80)":U
    FIELD Inactive AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Inactive":T SERIALIZE-NAME "Inactive":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U

    INDEX ChildTableGuid ChildTableGUID ASCENDING
    INDEX ParentTableGuid ParentTableGUID ASCENDING
    INDEX RelationGuid AS UNIQUE PRIMARY RelationGuid ASCENDING
    INDEX RelationName RelationName ASCENDING
    INDEX ParentTableName ParentTableName ASCENDING Inactive ASCENDING
    INDEX ChildTableName ChildTableName ASCENDING
    INDEX DefaultParentRelation ChildTableName ASCENDING DefaultParentRelation ASCENDING

    .
