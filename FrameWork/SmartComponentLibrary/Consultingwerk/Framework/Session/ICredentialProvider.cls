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
    File        : ICredentialProvider
    Purpose     : Provides support for custom authentication methods
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 30 07:45:04 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.*         FROM PROPATH .  
USING Consultingwerk.Framework.Session.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

INTERFACE Consultingwerk.Framework.Session.ICredentialProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the current user credentials
        Notes:   
        @return The IUserCredentials instance with the current credentials to be passed to the IAuthenticationService
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IUserCredentials GetUserCredentials ().

END INTERFACE.