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
    File        : eSmartSecurityAssignment.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16.04.2014 08:24:02
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartSecurityAssignment NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSecurityAssignmentBefore &ENDIF
    FIELD SecurityAssignmentGuid AS CHARACTER FORMAT "x(36)":U LABEL "SecurityAssignmentGuid":T SERIALIZE-NAME "SecurityAssignmentGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityRealmGuid AS CHARACTER FORMAT "x(36)":U LABEL "SecurityRealmGuid":T SERIALIZE-NAME "SecurityRealmGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityItemGuid AS CHARACTER FORMAT "x(36)":U LABEL "Security Item":T SERIALIZE-NAME "SecurityItemGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD GroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "GroupGuid":T SERIALIZE-NAME "GroupGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD UserGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGuid":T SERIALIZE-NAME "UserGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD Restricted AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Restricted":T SERIALIZE-NAME "Restricted":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityRealmCode AS CHARACTER FORMAT "x(3)":U LABEL "Realm Code":T
    FIELD SecurityRealmDescription AS CHARACTER FORMAT "x(80)":U LABEL "Realm Description":T
    FIELD SecurityItemDescription AS CHARACTER FORMAT "X(80)":U LABEL "Security Item":T
    FIELD OwnerName AS CHARACTER FORMAT "X(80)":U LABEL "User or Group":T
    FIELD OwnerType AS CHARACTER FORMAT "X(1)":U LABEL "Type":T
    FIELD GroupName AS CHARACTER FORMAT "x(20)":U LABEL "GroupName":T
    FIELD UserName AS CHARACTER FORMAT "x(20)":U LABEL "UserName":T
    FIELD LoginCompanyGuid AS CHARACTER FORMAT "x(36)":U LABEL "LoginCompanyGuid":T SERIALIZE-NAME "LoginCompanyGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX SecurityAssignmentGuid AS UNIQUE PRIMARY SecurityAssignmentGuid ASCENDING
    INDEX SecurityRealmItemGroup SecurityRealmGuid ASCENDING SecurityItemGuid ASCENDING GroupGuid ASCENDING UserGuid ASCENDING
    INDEX SecurityRealmItemUser SecurityRealmGuid ASCENDING SecurityItemGuid ASCENDING UserGuid ASCENDING GroupGuid ASCENDING

    .
