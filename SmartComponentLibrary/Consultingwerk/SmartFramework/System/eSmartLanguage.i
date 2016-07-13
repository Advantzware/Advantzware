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
    File        : eSmartLanguage.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 20.09.2013 09:09:43
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartLanguage NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLanguageBefore &ENDIF
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T SERIALIZE-NAME "LanguageGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageName AS CHARACTER FORMAT "x(20)":U LABEL "LanguageName":T SERIALIZE-NAME "LanguageName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageImage AS CHARACTER FORMAT "x(40)":U LABEL "LanguageImage":T
    FIELD LanguageIsoCode AS CHARACTER FORMAT "X(8)":U LABEL "ISO-Code":T
    FIELD RcodeLanguage AS CHARACTER FORMAT "x(30)":U LABEL "RCode Language":T SERIALIZE-NAME "RcodeLanguage":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX LanguageGuid AS UNIQUE PRIMARY LanguageGuid ASCENDING
    INDEX LanguageIsoCode AS UNIQUE LanguageIsoCode ASCENDING

    .
