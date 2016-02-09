/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartModule.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 06.04.2014 13:09:37
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartModule NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartModuleBefore &ENDIF
    FIELD ModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModuleGuid":T SERIALIZE-NAME "ModuleGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ModuleName AS CHARACTER FORMAT "x(30)":U LABEL "ModuleName":T SERIALIZE-NAME "ModuleName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ModuleDescription AS CHARACTER FORMAT "x(100)":U LABEL "ModuleDescription":T SERIALIZE-NAME "ModuleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductGuid AS CHARACTER FORMAT "x(36)":U LABEL "ProductGuid":T SERIALIZE-NAME "ProductGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductCode AS CHARACTER FORMAT "x(20)":U LABEL "Code":T

    INDEX ModuleGuid AS UNIQUE PRIMARY ModuleGuid ASCENDING

    .
