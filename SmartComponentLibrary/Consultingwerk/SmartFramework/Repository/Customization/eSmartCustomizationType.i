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
    File        : eSmartCustomizationType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 11:03:28
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartCustomizationType NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartCustomizationTypeBefore &ENDIF
    FIELD CustomizationTypeGuid AS CHARACTER FORMAT "x(36)":U SERIALIZE-NAME "CustomizationTypeGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD CustomizationTypeCode AS CHARACTER FORMAT "x(80)":U LABEL "Customization Type":T SERIALIZE-NAME "CustomizationTypeCode":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD CustomizationTypeDescription AS CHARACTER FORMAT "x(80)":U LABEL "Description":T SERIALIZE-NAME "CustomizationTypeDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ServiceTypeName AS CHARACTER FORMAT "x(80)":U LABEL "Service Type Name":T SERIALIZE-NAME "ServiceTypeName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ServiceMethodName AS CHARACTER FORMAT "x(80)":U LABEL "Method Name":T SERIALIZE-NAME "ServiceMethodName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX CustomizationTypeCode AS UNIQUE CustomizationTypeGuid ASCENDING
    INDEX CustomizationTypeDescription CustomizationTypeDescription ASCENDING
    INDEX CustomizationTypeGuid AS UNIQUE PRIMARY CustomizationTypeGuid ASCENDING

    .
