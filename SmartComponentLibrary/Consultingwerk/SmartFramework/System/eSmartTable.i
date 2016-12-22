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
    File        : eSmartTable.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.10.2015 11:25:48
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.TableBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartTable{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartTableBefore{&SUFFIX} &ENDIF
    FIELD TableGUID AS CHARACTER FORMAT "x(36)":U LABEL "TableGUID":T
    FIELD TableName AS CHARACTER FORMAT "x(20)":U LABEL "Table Name":T
    FIELD DatabaseName AS CHARACTER FORMAT "x(20)":U LABEL "Database Name":T
    FIELD TableDescription AS CHARACTER FORMAT "x(400)":U LABEL "Table Description":T
    FIELD UniqueKeyFields AS CHARACTER FORMAT "x(80)":U LABEL "Unique Key Fields":T
    FIELD UniqueKeyFormats AS CHARACTER FORMAT "x(80)":U LABEL "Unique Key Formats":T
    FIELD UniqueKeySubstitute AS CHARACTER FORMAT "x(80)":U LABEL "Unique Key Substitute":T
    FIELD AllowComments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Comments allowed":T
    FIELD AllowAttachments AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Attachments Allowed":T
    FIELD DescriptionFields AS CHARACTER FORMAT "x(80)":U LABEL "Description Fields":T
    FIELD DescriptionFormats AS CHARACTER FORMAT "x(80)":U LABEL "Description Formats":T
    FIELD DescriptionSubstitute AS CHARACTER FORMAT "x(80)":U LABEL "Description Substitute":T
    FIELD QualifiedTableName AS CHARACTER FORMAT "X(80)":U LABEL "Qualified Table Name":T
    FIELD TempTableName AS CHARACTER FORMAT "x(80)":U LABEL "Temp Table Name":T
    FIELD KeyFieldAssignmentGUID AS CHARACTER FORMAT "x(36)":U LABEL "KeyFieldAssignmentGUID":T

    INDEX DatabaseTable AS UNIQUE PRIMARY DatabaseName ASCENDING TableName ASCENDING
    INDEX TableGUID AS UNIQUE TableGUID ASCENDING

    .

    