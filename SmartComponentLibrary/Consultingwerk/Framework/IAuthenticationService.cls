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
    File        : IAuthenticationService
    Purpose     : Interface for authentication systems
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 07 01:40:08 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IAuthenticationService:  
  
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
                                       pcNewPasswordVerified AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Validates the user credentials and updates the Session Manager with 
                 the user information
        Notes:   
        @param poCredentials The Credentials to be validated
        @return Logical value indicating if the user has been authenficated
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL ValidateUser (poCredentials AS IUserCredentials).

END INTERFACE.