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
    File        : ttLanguage.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 23:03:06 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttLanguage NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLanguageBefore &ENDIF
    FIELD LanguageKey AS CHARACTER FORMAT "x(36)":U LABEL "LanguageKey":T SERIALIZE-NAME "LanguageKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageName AS CHARACTER FORMAT "x(20)":U LABEL "LanguageName":T SERIALIZE-NAME "LanguageName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageImage AS CHARACTER FORMAT "x(40)":U LABEL "LanguageImage":T
    FIELD LanguageIsoCode AS CHARACTER FORMAT "X(8)":U LABEL "ISO-Code":T

    INDEX LanguageKey AS UNIQUE PRIMARY LanguageKey ASCENDING
    INDEX LanguageIsoCode AS UNIQUE LanguageIsoCode ASCENDING

    .
