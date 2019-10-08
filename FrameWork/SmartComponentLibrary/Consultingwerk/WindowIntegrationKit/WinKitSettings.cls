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
    File        : WinKitSettings
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 10 10:31:11 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Consultingwerk.WindowIntegrationKit.WinKitSettings: 

    /*------------------------------------------------------------------------------
        Purpose: Turn on/off embedded windows                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY WinKitActive AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Disable instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WinKitSettings ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.