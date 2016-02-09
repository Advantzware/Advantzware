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
    File        : eSmartBusinessEntity.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.09.2015 14:10:59
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartBusinessEntity{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartBusinessEntityBefore{&SUFFIX} &ENDIF
    FIELD BusinessEntityGuid AS CHARACTER FORMAT "x(36)":U LABEL "BusinessEntity Guid":T
    FIELD BusinessEntityPackage AS CHARACTER FORMAT "x(60)":U LABEL "Package":T
    FIELD BusinessEntityName AS CHARACTER FORMAT "x(60)":U LABEL "Name":T
    FIELD BusinessEntityPurpose AS CHARACTER FORMAT "x(60)":U LABEL "Purpose":T
    FIELD DBRequired AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "DB Required":T
    FIELD TrackDeletions AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Track Deletions":T

    INDEX BusinessEntityName AS UNIQUE BusinessEntityPackage ASCENDING BusinessEntityName ASCENDING
    INDEX SmartBusinessEntity AS UNIQUE PRIMARY BusinessEntityGuid ASCENDING

    .

    