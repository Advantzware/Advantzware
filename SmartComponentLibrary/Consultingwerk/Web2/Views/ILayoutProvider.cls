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
    File        : ILayoutProvider
    Purpose     : Interface for the service that provides a Layout 
                  (for a Container)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 14:37:34 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

INTERFACE Consultingwerk.Web2.Views.ILayoutProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Returns a Layout to the web browser 
        Notes:   
        @param pcLayoutId The Id of the layout to return
        @param pcJSON The JSON structure of the layout 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetLayout (pcLayoutId AS CHARACTER, 
                                  OUTPUT pcJSON AS CHARACTER) . 
  
END INTERFACE.
