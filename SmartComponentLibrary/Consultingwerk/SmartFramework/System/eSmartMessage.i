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
    File        : eSmartMessage.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 09.01.2013 08:02:19
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartMessage NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartMessageBefore &ENDIF
    FIELD MessageGuid AS CHARACTER FORMAT "x(36)":U LABEL "MessageGuid":T SERIALIZE-NAME "MessageGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MessageGroup AS CHARACTER FORMAT "x(8)":U LABEL "Message Group":T SERIALIZE-NAME "MessageGroup":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MessageNumber AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "MessageNumber":T SERIALIZE-NAME "MessageNumber":U XML-DATA-TYPE "int":U XML-NODE-TYPE "ELEMENT":U
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T SERIALIZE-NAME "LanguageGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MessageText AS CHARACTER FORMAT "x(70)":U LABEL "MessageText":T SERIALIZE-NAME "MessageText":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MessageDetail AS CHARACTER FORMAT "x(70)":U LABEL "MessageDetail":T SERIALIZE-NAME "MessageDetail":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MessageType AS CHARACTER FORMAT "x(8)":U LABEL "MessageType":T SERIALIZE-NAME "MessageType":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX MessageGroupNumberLanguage AS UNIQUE MessageGroup ASCENDING MessageNumber ASCENDING LanguageGuid ASCENDING
    INDEX MessageGuid AS UNIQUE PRIMARY MessageGuid ASCENDING

    .
