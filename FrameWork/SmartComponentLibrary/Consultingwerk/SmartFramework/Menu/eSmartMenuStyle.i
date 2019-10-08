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
    File        : eSmartMenuStyle.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 09.08.2013 08:55:19
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartMenuStyle NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartMenuStyleBefore &ENDIF
    FIELD MenuStyleGuid AS CHARACTER FORMAT "x(36)":U LABEL "MenuStyleGuid":T SERIALIZE-NAME "MenuStyleGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuStyleCode AS CHARACTER FORMAT "x(8)":U LABEL "Menu Style Code":T SERIALIZE-NAME "MenuStyleCode":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuStyleDescription AS CHARACTER FORMAT "x(80)":U LABEL "Description":T SERIALIZE-NAME "MenuStyleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX MenuStyleCode AS UNIQUE MenuStyleCode ASCENDING
    INDEX MenuStyleGuid AS UNIQUE PRIMARY MenuStyleGuid ASCENDING

    .
