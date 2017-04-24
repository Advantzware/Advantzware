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
    File        : IGridCustomProvider
    Purpose     : Interface for providers of custom grid layouts. These
                  services can be registered using the method
                  IGridLayoutProvider:RegisterGridCustomProvider ()
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 18 21:35:14 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

INTERFACE Consultingwerk.Web2.Services.SmartViews.IGridCustomProvider:

    /**
     * Purpose: Returns the Json Object describing the Grid
     * Notes:
     * @param pcCustomViewName The name of the view
     * @return The JSON Object describing the Grid
     */
    METHOD PUBLIC JsonObject GetFragment (pcCustomViewName AS CHARACTER) .

END INTERFACE.
