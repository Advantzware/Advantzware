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
    File        : WinUser
    Purpose     : winuser.h constants / windows API
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 29 22:03:01 CEST 2011
    Notes       : Based on C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Include\WinUser.h
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.API.WinUser FINAL: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WinUser ():
        SUPER ().
        
    END CONSTRUCTOR.

    { Consultingwerk/Windows/API/winuser_consts.i }

END CLASS.