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
    File        : eBusinessEntityProperties.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2015 15:13:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eBusinessEntityProperties{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eBusinessEntityPropertiesBefore{&SUFFIX} &ENDIF
    FIELD PropertyName AS CHARACTER FORMAT "X(80)":U LABEL "Property Name":T
    FIELD PropertyValue AS CHARACTER FORMAT "X(80)":U LABEL "Property Value":T

    INDEX BusinessEntityProperties AS UNIQUE PRIMARY PropertyName ASCENDING

    .

    