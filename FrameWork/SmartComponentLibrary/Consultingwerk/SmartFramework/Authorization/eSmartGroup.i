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
    File        : eSmartGroup.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories
    Created     : 12.10.2012 18:06:53
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartGroup NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartGroupBefore &ENDIF
    FIELD GroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "GroupGuid":T SERIALIZE-NAME "GroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD GroupName AS CHARACTER FORMAT "x(20)":U LABEL "GroupName":T SERIALIZE-NAME "GroupName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "ParentGroupGuid":T SERIALIZE-NAME "ParentGroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyGuid AS CHARACTER FORMAT "x(36)":U LABEL "LoginCompanyGuid":T SERIALIZE-NAME "LoginCompanyGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentGroupName AS CHARACTER FORMAT "X(40)":U LABEL "Parent Group Name":T
    FIELD LoginCompanyName AS CHARACTER FORMAT "x(20)":U LABEL "LoginCompanyName":T
    
    INDEX GroupGuid AS UNIQUE PRIMARY GroupGuid ASCENDING
    INDEX LoginCompanyGuid LoginCompanyGuid ASCENDING
    INDEX ParentGroupGuid ParentGroupGuid ASCENDING

    .
