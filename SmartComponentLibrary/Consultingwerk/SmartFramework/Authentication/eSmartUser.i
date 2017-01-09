/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartUser.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 12.06.2015 01:48:48
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authentication.UserBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartUser{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartUserBefore{&SUFFIX} &ENDIF
    FIELD UserGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGuid":T
    FIELD UserName AS CHARACTER FORMAT "x(20)":U LABEL "UserName":T
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T
    FIELD UserFullName AS CHARACTER FORMAT "x(40)":U LABEL "UserFullName":T
    FIELD UserEmail AS CHARACTER FORMAT "x(40)":U LABEL "UserEmail":T
    FIELD UserPassword AS CHARACTER FORMAT "x(20)":U LABEL "UserPassword":T
    FIELD UserPasswordChangedDate AS DATE FORMAT "99/99/99":U LABEL "UserPasswordChangedDate":T
    FIELD UserLastLogin AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U LABEL "UserLastLogin":T
    FIELD LoginCompanyGuid AS CHARACTER FORMAT "x(36)":U LABEL "LoginCompanyGuid":T
    FIELD ManagerUserGuid AS CHARACTER FORMAT "x(36)":U LABEL "ManagerUserGuid":T
    FIELD UserSecurityId AS INTEGER FORMAT ">,>>>,>>9":U INIT ? LABEL "User Security Id":T
    FIELD LanguageIsoCode AS CHARACTER FORMAT "X(8)":U LABEL "Language ISO Code":T
    FIELD LanguageName AS CHARACTER FORMAT "X(40)":U LABEL "Language Name":T
    FIELD ManagerUserName AS CHARACTER FORMAT "X(40)":U LABEL "Manager Name":T
    FIELD ManagerUserFullName AS CHARACTER FORMAT "X(40)":U LABEL "Manager FullName":T
    FIELD LoginCompanyName AS CHARACTER FORMAT "x(20)":U LABEL "LoginCompanyName":T

    INDEX ManagerUserGuid ManagerUserGuid ASCENDING
    INDEX UserGuid AS UNIQUE PRIMARY UserGuid ASCENDING
    INDEX UserNameLoginCompanyGuid AS UNIQUE UserName ASCENDING LoginCompanyGuid ASCENDING
    INDEX UserSecurityId AS UNIQUE UserSecurityId ASCENDING

    .

    