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
    File        : eSmartUiTranslation.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:40:10
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartUiTranslation NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartUiTranslationBefore &ENDIF
    FIELD UiTranslationGuid AS CHARACTER FORMAT "x(36)":U LABEL "UiTranslationGuid":T SERIALIZE-NAME "UiTranslationGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T SERIALIZE-NAME "LanguageGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ObjectName AS CHARACTER FORMAT "x(60)":U LABEL "Container Object Name":T SERIALIZE-NAME "ObjectName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ResourceKey AS CHARACTER FORMAT "x(60)":U LABEL "Resource Key":T SERIALIZE-NAME "ResourceKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD OriginalString AS CHARACTER FORMAT "x(70)":U LABEL "OriginalString":T SERIALIZE-NAME "OriginalString":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TranslatedString AS CHARACTER FORMAT "x(70)":U LABEL "TranslatedString":T SERIALIZE-NAME "TranslatedString":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX LanguageObjectOriginal LanguageGuid ASCENDING ObjectName ASCENDING OriginalString ASCENDING ResourceKey ASCENDING
    INDEX LanguageObjectResource LanguageGuid ASCENDING ObjectName ASCENDING ResourceKey ASCENDING
    INDEX UiTranslationGuid AS UNIQUE PRIMARY UiTranslationGuid ASCENDING

    .
