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
    File        : eSmartBusinessEntityTable.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.09.2015 14:10:59
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartBusinessEntityTable{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartBusinessEntityTableBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityTableGuid AS CHARACTER FORMAT "x(36)":U LABEL "BusinessEntity Table Guid":T
    FIELD BusinessEntityGuid AS CHARACTER FORMAT "x(36)":U LABEL "BusinessEntity Guid":T
    FIELD TableOrder AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Table Order":T
    FIELD TableName AS CHARACTER FORMAT "x(60)":U LABEL "Name":T
    FIELD TableLabelSingular AS CHARACTER FORMAT "x(60)":U LABEL "Table Label Singular":T
    FIELD TableLabelPlural AS CHARACTER FORMAT "x(60)":U LABEL "Table Label Plural":T
    FIELD ShouldUseBatching AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Should use Batching":T
    FIELD BatchSize AS INTEGER FORMAT "->,>>>,>>9":U INIT "100":U LABEL "Batch Size":T
    FIELD AllowDelete AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Allow Delete":T
    FIELD AllowCreate AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Allow Create":T
    FIELD AllowUpdate AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Allow Update":T

    INDEX BusinessEntity AS UNIQUE BusinessEntityGuid ASCENDING TableOrder ASCENDING
    INDEX SmartBusinessEntityTable AS UNIQUE PRIMARY BusinessEntityTableGuid ASCENDING
    INDEX TableName AS UNIQUE BusinessEntityGuid ASCENDING TableName ASCENDING

    .

    