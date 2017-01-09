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
    File        : eIndex.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06.11.2016 18:48:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="TempTableName,IndexName").

DEFINE {&ACCESS} TEMP-TABLE eIndex{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eIndexBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityName AS CHARACTER FORMAT "X(8)":U
    FIELD TempTableName AS CHARACTER FORMAT "X(8)":U
    FIELD IndexOrder AS INTEGER FORMAT ">>>,>>9":U
    FIELD IndexName AS CHARACTER FORMAT "X(8)":U
    FIELD IndexUnique AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD IndexPrimary AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD IndexWordIndex AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U
    FIELD FieldNames AS CHARACTER FORMAT "X(8)":U
    FIELD FieldSort AS CHARACTER FORMAT "X(8)":U
    FIELD DefaultSearchCodeGeneration AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U

    INDEX IndexName AS UNIQUE PRIMARY TempTableName ASCENDING IndexName ASCENDING
    INDEX Order TempTableName ASCENDING IndexOrder ASCENDING
    INDEX DefaultSearchCodeGeneration DefaultSearchCodeGeneration ASCENDING

    .

    