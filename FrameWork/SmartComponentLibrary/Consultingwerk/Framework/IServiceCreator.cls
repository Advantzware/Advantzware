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
    File        : IServiceCreator
    Purpose     : Supports lazy loading of service requested from the
                  Service Container (Consultingwerk/get-service.i)
    Syntax      :
    Description : Classes implementing the IServiceCreator interface may
                  act as a factory for services loaded at first request
                  rather than during application startup. The ServiceContainer
                  will call into the IServiceCreator registered as a service
                  to instantiate the service. As a logical consequence,
                  classes implementing the IServiceCreator interface can
                  never be used as an actual service - but that's a limitation
                  which is considered to be acceptable
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 15 13:22:41 CEST 2015
    Notes       : http://msdn.microsoft.com/de-de/library/system.componentmodel.design.servicecreatorcallback(v=vs.110).aspx
                  SCL-573
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IServiceCreator:

    /**
     * Purpose: Creates a service instance
     * Notes:   Factory method for services referenced by the provided service type
     *          (typically an interface)
     * @param poType The service type to create an instance of
     * @return The created service instance
     */
    METHOD PUBLIC Progress.Lang.Object CreateInstance (poType AS Progress.Lang.Class).

END INTERFACE.
