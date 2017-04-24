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
    File        : eDataRelationProperties.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06.11.2016 18:48:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="DataRelationName,PropertyName").

DEFINE {&ACCESS} TEMP-TABLE eDataRelationProperties{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eDataRelationPropertiesBefore{&SUFFIX} &ENDIF
    FIELD DataRelationName AS CHARACTER FORMAT "X(80)":U LABEL "Data Relation":T
    FIELD PropertyName AS CHARACTER FORMAT "X(80)":U LABEL "Property Name":T
    FIELD PropertyValue AS CHARACTER FORMAT "X(80)":U LABEL "Property Value":T

    INDEX DataRelationProperties AS UNIQUE PRIMARY DataRelationName ASCENDING PropertyName ASCENDING

    .

    