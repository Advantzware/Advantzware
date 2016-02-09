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
    File        : eTable.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2015 15:13:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eTable{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eTableBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityName AS CHARACTER FORMAT "X(8)":U
    FIELD TempTableName AS CHARACTER FORMAT "X(8)":U
    FIELD SourceTableNames AS CHARACTER FORMAT "X(8)":U
    FIELD SourceBufferNames AS CHARACTER FORMAT "X(8)":U
    FIELD SourceDefaultQuery AS CHARACTER FORMAT "X(8)":U
    FIELD SourceBufferKeys AS CHARACTER FORMAT "X(8)":U LABEL "Source Buffer Keys":T
    FIELD TempTablePurpose AS CHARACTER FORMAT "X(8)":U
    FIELD TempTableDescription AS CHARACTER FORMAT "X(8)":U
    FIELD TempTablePath AS CHARACTER FORMAT "X(8)":U
    FIELD TempTableBeforeName AS CHARACTER FORMAT "X(8)":U
    FIELD LayoutColumn AS INTEGER FORMAT ">>>,>>9":U
    FIELD LayoutRow AS INTEGER FORMAT ">>>,>>9":U
    FIELD LayoutWidth AS INTEGER FORMAT ">>>,>>9":U
    FIELD LayoutHeight AS INTEGER FORMAT ">>>,>>9":U
    FIELD NamespaceUri AS CHARACTER FORMAT "X(8)":U
    FIELD NamespacePrefix AS CHARACTER FORMAT "X(8)":U
    FIELD XmlNodeName AS CHARACTER FORMAT "X(8)":U
    FIELD SerializeName AS CHARACTER FORMAT "X(8)":U
    FIELD NoBeforeTable AS LOGICAL FORMAT "yes/no":U INIT "false":U LABEL "No Before Table (read-only)":T
    FIELD EntityPackageName AS CHARACTER FORMAT "X(80)":U LABEL "Table Package Name":T
    FIELD EntityClassName AS CHARACTER FORMAT "X(80)":U LABEL "Table Class Name":T
    FIELD DeletedFields AS CHARACTER FORMAT "X(60)":U LABEL "Deleted Fields":T

    INDEX TempTableName AS UNIQUE PRIMARY TempTableName ASCENDING

    .

    