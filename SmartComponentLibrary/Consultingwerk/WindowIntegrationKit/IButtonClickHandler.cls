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
    File        : IButtonClickHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 31 13:12:43 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.WindowIntegrationKit.IButtonClickHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Handles the Click event of UltraButtons that replaced ABL Buttons
        Notes:   
        @param poControl The Control with the Click event
        @param phWidget The related ABL Widget
        @return Logical Value indicating if the Click event has been handled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleClickEvent (poControl AS System.Windows.Forms.Control, 
                                            phWidget AS HANDLE).

END INTERFACE.