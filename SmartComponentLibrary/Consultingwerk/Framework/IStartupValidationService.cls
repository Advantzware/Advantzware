/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IStartupValidationService
    Purpose     : Interface for services that are used during the startup
                  of a session (client, AppServer, REST AppServer) to
                  validate a session. Misconfigurated sessions can be
                  stopped from starting by throwing an error from the
                  service method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 28 08:07:35 CEST 2016
    Notes       : Implementations of this Interface may throw the
                  StartupValidationFailedException
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IStartupValidationService:

    /**
     * Purpose: Validates a session startup
     * Notes:   Expected to throw an error, when the session startup is
     *          cancelled due to misconfiguration
     */
    METHOD PUBLIC VOID ValidateSessionStartup().

END INTERFACE.
