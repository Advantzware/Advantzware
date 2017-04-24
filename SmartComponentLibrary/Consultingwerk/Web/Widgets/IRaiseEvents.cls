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
    File        : IRaiseEvents
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 27 20:56:09 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Web.Widgets.IRaiseEvents:  
  
    /*------------------------------------------------------------------------------
        Purpose: Raises Events from a Web Control
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RaiseEvents ().

END INTERFACE.