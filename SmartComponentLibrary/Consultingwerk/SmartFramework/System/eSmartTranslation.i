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
    File        : eSmartTranslation.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 16.01.2013 09:55:45
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartTranslation NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartTranslationBefore &ENDIF
    FIELD TranslationGuid AS CHARACTER FORMAT "x(36)":U LABEL "TranslationGuid":T SERIALIZE-NAME "TranslationGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TranslationScope AS CHARACTER FORMAT "x(70)":U LABEL "Scope":T SERIALIZE-NAME "TranslationScope":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TranslationKey AS CHARACTER FORMAT "x(70)":U LABEL "Key":T SERIALIZE-NAME "TranslationKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T SERIALIZE-NAME "LanguageGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD OriginalString AS CHARACTER FORMAT "x(70)":U LABEL "OriginalString":T SERIALIZE-NAME "OriginalString":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TranslatedString AS CHARACTER FORMAT "x(70)":U LABEL "TranslatedString":T SERIALIZE-NAME "TranslatedString":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX ScopeKeyLanguage TranslationScope ASCENDING TranslationKey ASCENDING LanguageGuid ASCENDING
    INDEX ScopeOriginalStringLanguage TranslationScope ASCENDING OriginalString ASCENDING LanguageGuid ASCENDING
    INDEX TranslationGuid AS UNIQUE PRIMARY TranslationGuid ASCENDING

    .
