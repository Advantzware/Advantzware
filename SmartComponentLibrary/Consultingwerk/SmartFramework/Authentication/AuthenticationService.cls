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
    File        : AuthenticationService
    Purpose     : Provides Authentication using the SmartDB User Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 07 01:56:50 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.*     FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.AuthenticationService
    IMPLEMENTS IAuthenticationService:

    { Consultingwerk/SmartFramework/Authentication/dsUser.i }

    /*------------------------------------------------------------------------------
        Purpose: Changes the Password of a user
        Notes:
        @param pcUserName The name of the user to change the password for
        @param pcLoginCompanyGuid The company the user belongs too
        @param pcCurrentPassword The current password
        @param pcNewPassword The new password to be set
        @param pcNewPasswordVerified The repeated new password
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ChangePassword (pcUserName AS CHARACTER,
                                       pcLoginCompanyGuid AS CHARACTER,
                                       pcCurrentPassword AS CHARACTER,
                                       pcNewPassword AS CHARACTER,
                                       pcNewPasswordVerified AS CHARACTER):

        DEFINE VARIABLE oConfigurationProvider   AS IConfigurationProvider  NO-UNDO .
        DEFINE VARIABLE oUserCredentials         AS IUserCredentials        NO-UNDO .
        DEFINE VARIABLE oChangePasswordParameter AS ChangePasswordParameter NO-UNDO .
        DEFINE VARIABLE hDataset                 AS HANDLE                  NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i
                                    Consultingwerk.Framework.IConfigurationProvider
                                    "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .

        oUserCredentials = NEW Consultingwerk.Framework.UserCredentials (pcUserName,
                                                                         pcCurrentPassword,
                                                                         pcLoginCompanyGuid) .

        IF NOT Consultingwerk.Framework.Session.SessionManager:UserName > "" THEN
            ValidateUser (oUserCredentials).

        IF pcNewPassword = "":U THEN
            UNDO, THROW NEW SmartException ("SFR":U, 300).

        IF NOT pcNewPassword = pcNewPasswordVerified THEN
            UNDO, THROW NEW SmartException ("SFR":U, 301).

        IF pcCurrentPassword = pcNewPassword THEN
            UNDO, THROW NEW SmartException ("SFR":U, 302).


        oChangePasswordParameter = NEW ChangePasswordParameter (oUserCredentials,
                                                                pcNewPassword) .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                       "ChangeSmartUserPassword":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oChangePasswordParameter) .

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oUserCredentials).
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oChangePasswordParameter).
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hDataset).
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates the user credentials and updates the Session Manager with
                 the user information
        Notes:
        @param poCredentials The Credentials to be validated
        @return Logical value indicating if the user has been authenficated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateUser (poCredentials AS IUserCredentials):

        DEFINE VARIABLE oRequest     AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE cPasswordEnc AS CHARACTER        NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poCredentials,
                                                       "User Credentials"{&TRAN}) .

        IF poCredentials:Password > "":U THEN
            ASSIGN cPasswordEnc = ENCODE (poCredentials:Password) .

        /* Mike Fechner, Consultingwerk Ltd. 07.09.2014
           First Authentication Attempd: User and Login Company */
        oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                         SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2 AND eSmartUser.UserPassword = &3":U,
                                                     QUOTER (poCredentials:UserName),
                                                     QUOTER (poCredentials:CompanyKey),
                                                     QUOTER (cPasswordEnc)),
                                         1) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsUser) .

        FIND FIRST eSmartUser NO-ERROR .

        /* Mike Fechner, Consultingwerk Ltd. 07.09.2014
           Second Authentication Attempd: User with no specific Login Company */
        IF NOT AVAILABLE eSmartUser THEN DO:
            oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                             SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2 AND eSmartUser.UserPassword = &3":U,
                                                         QUOTER (poCredentials:UserName),
                                                         QUOTER ("":U),
                                                         QUOTER (cPasswordEnc)),
                                             1) .

            FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                           "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                           oRequest,
                                                           OUTPUT DATASET dsUser) .

            FIND FIRST eSmartUser NO-ERROR .
        END.

        IF NOT AVAILABLE eSmartUser THEN
            RETURN FALSE .

        ASSIGN
            SessionManager:UserName        = poCredentials:UserName
            SessionManager:UserKey         = eSmartUser.UserGuid
            SessionManager:LoginCompanyKey = poCredentials:CompanyKey .

        RETURN TRUE.

    END METHOD .

END CLASS.
