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
    File        : eSmartLinkType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 10:27:57
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartLinkType NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLinkTypeBefore &ENDIF
    FIELD LinkTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkTypeGuid":T SERIALIZE-NAME "LinkTypeGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LinkName AS CHARACTER FORMAT "x(80)":U LABEL "Link Name":T SERIALIZE-NAME "LinkName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SourcePropertyName AS CHARACTER FORMAT "x(80)":U LABEL "Source Property Name":T SERIALIZE-NAME "SourcePropertyName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SourceHandshakeMethod AS CHARACTER FORMAT "x(80)":U LABEL "Source Handshake Method":T SERIALIZE-NAME "SourceHandshakeMethod":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TargetPropertyName AS CHARACTER FORMAT "x(80)":U LABEL "Target Property Name":T SERIALIZE-NAME "TargetPropertyName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD TargetHandshakeMethod AS CHARACTER FORMAT "x(80)":U LABEL "Target Handshake Method":T SERIALIZE-NAME "TargetHandshakeMethod":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX LinkName AS UNIQUE LinkName ASCENDING
    INDEX LinkTypeGuid AS UNIQUE PRIMARY LinkTypeGuid ASCENDING

    .
