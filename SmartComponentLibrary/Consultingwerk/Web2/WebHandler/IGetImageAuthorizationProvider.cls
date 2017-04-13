/**********************************************************************
 * Copyright (C) 2006-2017 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IGetImageAuthorizationProvider
    Purpose     : Interface for services providing authorization for the
                  GetImageWebHandler
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 17 12:04:44 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Web2.WebHandler.IGetImageAuthorizationProvider:

    /**
     * Purpose: Returns is the GetImage operation for the given image file
     *          is restrictred
     * Notes:
     * @param pcFileName The relative file name of the requested image file
     * @return Logical value indicating if the get image operation is restricted (TRUE)
     */
    METHOD PUBLIC LOGICAL IsGetImageRestricted(pcFileName AS CHARACTER) .

END INTERFACE.
