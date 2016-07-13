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
    File        : eBusinessEntity.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2015 15:13:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eBusinessEntity{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eBusinessEntityBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityName AS CHARACTER FORMAT "X(8)":U
    FIELD BusinessEntityPurpose AS CHARACTER FORMAT "X(8)":U
    FIELD BusinessEntityDescription AS CHARACTER FORMAT "X(8)":U
    FIELD BusinessEntityPackage AS CHARACTER FORMAT "X(8)":U
    FIELD DataAccessName AS CHARACTER FORMAT "X(8)":U
    FIELD DataAccessPackage AS CHARACTER FORMAT "X(8)":U
    FIELD DatasetControllerName AS CHARACTER FORMAT "X(8)":U
    FIELD DatasetControllerPackage AS CHARACTER FORMAT "X(8)":U
    FIELD DatasetName AS CHARACTER FORMAT "X(8)":U
    FIELD DatasetPath AS CHARACTER FORMAT "X(8)":U
    FIELD DefaultTablePath AS CHARACTER FORMAT "X(8)":U
    FIELD DefaultTablePrefix AS CHARACTER FORMAT "X(8)":U INIT "e":U
    FIELD DefaultTableSuffix AS CHARACTER FORMAT "X(8)":U
    FIELD DefaultBeforeTablePrefix AS CHARACTER FORMAT "X(8)":U INIT "e":U
    FIELD DefaultBeforeTableSuffix AS CHARACTER FORMAT "X(8)":U INIT "Before":U
    FIELD GenerateBusinessEntity AS LOGICAL FORMAT "yes/no":U INIT "TRUE":U LABEL "Generate Business Entity":T
    FIELD GenerateDataAccess AS LOGICAL FORMAT "yes/no":U INIT "TRUE":U LABEL "Generate Data Access Class":T
    FIELD GenerateDatasetController AS LOGICAL FORMAT "yes/no":U INIT "TRUE":U LABEL "Generate Dataset Controller":T
    FIELD DistinctTempTableIncludeFiles AS LOGICAL FORMAT "yes/no":U INIT "TRUE":U LABEL "Use Distinct Include Files for Temp-Table Definition":T

    INDEX BusinessEntityName AS UNIQUE PRIMARY BusinessEntityName ASCENDING

    .

    