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
    File        : MenuContext
    Purpose     : Allows passing of the menu context to a Form Launched
                  from the Menu
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 20 23:28:05 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.MenuContext:

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the object that launched the menu function
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LaunchedFrom AS Progress.Lang.Object NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the menu function launched
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuFunctionKey AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the title of the menu item that launched the menu function
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuTitle AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuContext class
        Notes:
        @param pcMenuFunctionKey The key of the menu function that is launched
        @param pcMenuTitle The tile of the menu function that is launched
        @param poLaunchedFrom The reference to the object that launched the menu function
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuContext (pcMenuFunctionKey AS CHARACTER,
                                    pcMenuTitle AS CHARACTER,
                                    poLaunchedFrom AS Progress.Lang.Object):
        SUPER ().

        ASSIGN THIS-OBJECT:MenuFunctionKey = pcMenuFunctionKey
               THIS-OBJECT:MenuTitle       = pcMenuTitle
               THIS-OBJECT:LaunchedFrom    = poLaunchedFrom .

    END CONSTRUCTOR.

END CLASS.
