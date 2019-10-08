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
    File        : eSmartSecurityRealm.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16.04.2014 07:45:11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartSecurityRealm NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSecurityRealmBefore &ENDIF
    FIELD SecurityRealmGuid AS CHARACTER FORMAT "x(36)":U LABEL "SecurityRealmGuid":T SERIALIZE-NAME "SecurityRealmGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityRealmCode AS CHARACTER FORMAT "x(3)":U LABEL "Realm Code":T SERIALIZE-NAME "SecurityRealmCode":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityRealmDescription AS CHARACTER FORMAT "x(80)":U LABEL "Realm Description":T SERIALIZE-NAME "SecurityRealmDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD SecurityRealmService AS CHARACTER FORMAT "x(80)":U LABEL "Security Realm Service":T
    FIELD DefaultRestricted AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Default Restricted":T SERIALIZE-NAME "DefaultRestricted":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U
    FIELD CompanyDependent AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Assignment Company Dependent":T SERIALIZE-NAME "CompanyDependent":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U

    INDEX SecurityRealmCode AS UNIQUE SecurityRealmCode ASCENDING
    INDEX SecurityRealmGuid AS UNIQUE PRIMARY SecurityRealmGuid ASCENDING

    .
