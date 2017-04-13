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
    File        : eSmartAttributeGroup.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 20.05.2014 17:41:22
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartAttributeGroup NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartAttributeGroupBefore &ENDIF
    FIELD AttributeGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "AttributeGroupGuid":T SERIALIZE-NAME "AttributeGroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttributeGroupName AS CHARACTER FORMAT "x(80)":U LABEL "Attribute Group":T SERIALIZE-NAME "AttributeGroupName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttributeGroupDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T SERIALIZE-NAME "AttributeGroupDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX AttributeGroupGuid AS UNIQUE PRIMARY AttributeGroupGuid ASCENDING
    INDEX AttributeGroupName AS UNIQUE AttributeGroupName ASCENDING

    .
