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
    File        : eSmartFieldSecurityItem.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 18.08.2014 14:15:17
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartFieldSecurityItem NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartFieldSecurityItemBefore &ENDIF
    FIELD FieldSecurityItemGuid AS CHARACTER FORMAT "x(36)":U LABEL "FieldSecurityItemGuid":T SERIALIZE-NAME "FieldSecurityItemGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD FieldName AS CHARACTER FORMAT "x(60)":U LABEL "Field Name":T SERIALIZE-NAME "FieldName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityKey AS CHARACTER FORMAT "x(60)":U LABEL "Security Key":T SERIALIZE-NAME "SecurityKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ObjectName AS CHARACTER FORMAT "x(60)":U LABEL "Object Name":T SERIALIZE-NAME "ObjectName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD Description AS CHARACTER FORMAT "x(60)":U LABEL "Description":T SERIALIZE-NAME "Description":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD FieldNameCombined AS CHARACTER FORMAT "X(60)":U LABEL "Field Name Combined":T

    INDEX FieldName AS UNIQUE FieldName ASCENDING SecurityKey ASCENDING ObjectName ASCENDING
    INDEX FieldSecurityItemGuid AS UNIQUE PRIMARY FieldSecurityItemGuid ASCENDING

    .
