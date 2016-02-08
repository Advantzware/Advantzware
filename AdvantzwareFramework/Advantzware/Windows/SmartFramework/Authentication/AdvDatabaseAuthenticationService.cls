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
    File        : DatabaseAuthenticationService
    Purpose     : Extension to the SmartFramework DB Auth Service, integrates
                  nosweat company and location assignment after successful
                  authentication
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 08 16:49:10 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Advantzware.Windows.SmartFramework.Authentication.* FROM PROPATH .  
USING Consultingwerk.SmartFramework.Authentication.*      FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Advantzware.Windows.SmartFramework.Authentication.AdvDatabaseAuthenticationService 
    INHERITS DatabaseAuthenticationService: 

    /*------------------------------------------------------------------------------
        Purpose: Validates the user credentials and updates the Session Manager with 
                 the user information
        Notes:   
        @param poCredentials The Credentials to be validated
        @return Logical value indicating if the user has been authenficated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LOGICAL ValidateUser (poCredentials AS IUserCredentials):

        IF SUPER:ValidateUser (poCredentials) THEN DO:
            RUN Advantzware/Windows/SmartFramework/Menu/nosweat-after-auth.p . 

            RETURN TRUE . 
        END.
        ELSE 
            RETURN FALSE . 
    
    END METHOD .  

END CLASS.
