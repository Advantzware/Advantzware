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
    File        : ePossibleGroup.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories
    Created     : 02.11.2012 13:24:59
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ePossibleGroup NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ePossibleGroupBefore &ENDIF
    FIELD MenuGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "MenuGroupGuid":T SERIALIZE-NAME "MenuGroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuGuid AS CHARACTER FORMAT "x(36)":U LABEL "MenuGuid":T SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD GroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "GroupGuid":T SERIALIZE-NAME "GroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD GroupName AS CHARACTER FORMAT "x(20)":U LABEL "GroupName":T
    FIELD LoginCompanyName AS CHARACTER FORMAT "x(20)":U LABEL "LoginCompanyName":T

    INDEX GroupId GroupGuid ASCENDING
    INDEX MenuGroupGuid AS UNIQUE PRIMARY MenuGroupGuid ASCENDING

    .
