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
    File        : eSmartUserGroup.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 25.07.2013 22:06:23
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartUserGroup NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartUserGroupBefore &ENDIF
    FIELD UserGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGroupGuid":T SERIALIZE-NAME "UserGroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD GroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "GroupGuid":T SERIALIZE-NAME "GroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD UserGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGuid":T SERIALIZE-NAME "UserGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD UserName AS CHARACTER FORMAT "x(20)":U LABEL "UserName":T
    FIELD GroupName AS CHARACTER FORMAT "x(20)":U LABEL "GroupName":T
    FIELD ParentGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "ParentGroupGuid":T

    INDEX UserGroupGuid AS UNIQUE PRIMARY UserGroupGuid ASCENDING

    .
