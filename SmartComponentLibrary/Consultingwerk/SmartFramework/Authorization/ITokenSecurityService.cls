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
    File        : ITokenSecurityService
    Purpose     : Interface for classes providing token security information 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 18 21:33:01 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.Authorization.ITokenSecurityService:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns if the given security token is restricted
        Notes:   When the token is unknown on the current system, the ? is returned
        @param pcSecurityTokenCode The code of the security token
        @return Logical value indicating if the given token is restricted for the current user, TRUE means the user does not have access to the token
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsTokenRestricted (pcSecurityTokenCode AS CHARACTER).

END INTERFACE.
