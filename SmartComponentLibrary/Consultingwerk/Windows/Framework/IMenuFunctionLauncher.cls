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
    File        : IMenuFunctionLauncher
    Purpose     : Defines functioanlity required for serices that can
                  launch menu functions based on the Menu Function Key
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 08 00:55:35 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.* FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.IMenuFunctionLauncher:

    /*------------------------------------------------------------------------------
        Purpose: Event launched whenever a menu function is launched
        Notes:   This can be used for logging or LRU tracking purposes etc.
        @param sender The reference to the object that raised the event
        @param e The MenuFunctionLaunchedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuFunctionLaunched SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                             e AS MenuFunctionLaunchedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Launches a Menu Function from the Menu
        Notes:
        @param pcMenuFunctionKey The key of the Menu Function
        @param poContext An optional reference to a context (could be the Control that launches the function)
        @return The reference to the launched menu function
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object LaunchMenuFunction (pcMenuFunctionKey AS CHARACTER,
                                                           poContext AS Progress.Lang.Object) .

END INTERFACE.
