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
    File        : DatabaseAuthenticationService
    Purpose     : Provides server side Authentication  using the database
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
USING Consultingwerk.SmartFramework.Exceptions.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .  
USING Consultingwerk.OERA.*                          FROM PROPATH . 
USING Consultingwerk.Util.*                          FROM PROPATH . 
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.DatabaseAuthenticationService 
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

        IF NOT oConfigurationProvider:GetValue ("authenticationDb":U) > "":U THEN 
            UNDO, THROW NEW AppError ("No database provided for setting authenticationDb in the applicationsettings!"{&TRAN}, 0).

        oUserCredentials = NEW Consultingwerk.Framework.UserCredentials (pcUserName,
                                                                         pcCurrentPassword,
                                                                         pcLoginCompanyGuid) .

        IF Consultingwerk.Framework.Session.SessionManager:UserName > "" THEN DO:
            IF USERID (oConfigurationProvider:GetValue ("authenticationDb":U)) <> Consultingwerk.Framework.Session.SessionManager:UserName THEN
                UNDO, THROW NEW AppError ("Users can only change their own password!"{&TRAN}, 0).
        END.
        ELSE 
            /* login to DB */
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
                                                       "ChangeDatabasePassword":U,
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

        DEFINE VARIABLE oAuthenticationParameter AS AuthenticationParameter NO-UNDO . 
        DEFINE VARIABLE hDataset                 AS HANDLE                  NO-UNDO .
        DEFINE VARIABLE hPrincipal               AS HANDLE                  NO-UNDO .
        
        oAuthenticationParameter = NEW AuthenticationParameter (poCredentials) . 

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                       "PerformDatabaseAuthentication":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oAuthenticationParameter) .

        CREATE CLIENT-PRINCIPAL hPrincipal .
         
        hPrincipal:IMPORT-PRINCIPAL (oAuthenticationParameter:ClientPrincipal) NO-ERROR .
        		
        ErrorHelper:ResetErrorStatus() .        		
        		
        IF hPrincipal:LOGIN-STATE = "LOGIN":U THEN DO:
            ASSIGN 
                SessionManager:UserName         = oAuthenticationParameter:UserName 
                SessionManager:LoginCompanyName = oAuthenticationParameter:CompanyName 
                SessionManager:ClientPrincipal  = oAuthenticationParameter:ClientPrincipal . 
            
            RETURN TRUE .        		
        END.        		        		
		
		RETURN FALSE.

        CATCH err AS Progress.Lang.Error:
/*            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .*/
            
        	RETURN FALSE . 	
        END CATCH.
        
        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hPrincipal) .		
        END FINALLY.

	END METHOD .

END CLASS.
