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
    File        : eSmartSecurityToken.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2014 20:51:22
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartSecurityToken NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSecurityTokenBefore &ENDIF
    FIELD SecurityTokenGuid AS CHARACTER FORMAT "x(36)":U LABEL "SecurityTokenGuid":T SERIALIZE-NAME "SecurityTokenGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityTokenCode AS CHARACTER FORMAT "x(10)":U LABEL "Token Code":T SERIALIZE-NAME "SecurityTokenCode":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityTokenDescription AS CHARACTER FORMAT "x(80)":U LABEL "Token Description":T SERIALIZE-NAME "SecurityTokenDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX SecurityTokenCode AS UNIQUE SecurityTokenCode ASCENDING
    INDEX SecurityTokenGuid AS UNIQUE PRIMARY SecurityTokenGuid ASCENDING

    .
