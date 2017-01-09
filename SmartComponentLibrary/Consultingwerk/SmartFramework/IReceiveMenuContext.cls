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
    File        : IReceiveMenuContext
    Purpose     : Interface for classes launched from the menu structrue
                  that provide the method ReceiveMenuContext to receive 
                  the context of the menu item that launched the current
                  function 
    Syntax      :   
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Dec 20 23:42:34 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.IReceiveMenuContext:  
  
    /*------------------------------------------------------------------------------
        Purpose: Provides the context of the menu item that launched the current
                 function to the called instance
        Notes:   May be an object of the MenuContext class that provides information 
                 about the Label of the menu item that launched the current function
        @param poContext The context of the menu function that launched the current object
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ReceiveMenuContext (poContext AS Progress.Lang.Object).

END INTERFACE.