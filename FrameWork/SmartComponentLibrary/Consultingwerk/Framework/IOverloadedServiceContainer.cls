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
    File        : IOverloadedServiceContainer
    Purpose     : Interface for IServiceContainer implementations that
                  support overloading
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 15 20:11:10 CEST 2016
    Notes       : The overloading service container is queried by the current
                  instance before querying the servives of that service container
                  itself
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH.
USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.IOverloadedServiceContainer:

    /**
     * Purpose: Gets and sets the reference to the overloading service container
     * Notes:   The overloading service container is queried by the current
     *          instance before querying the servives of that service container
     *          itself
     */
    DEFINE PUBLIC PROPERTY OverloadingServiceContainer AS IServiceContainer NO-UNDO
    GET.
    SET.

END INTERFACE.
