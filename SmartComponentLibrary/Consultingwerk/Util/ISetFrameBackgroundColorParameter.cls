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
    File        : SetFrameBackgroundColorParameter
    Purpose     : Interface for the parameter object for the
                  WidgetHelper:SetFrameBackgroundColor method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 11 16:17:03 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

INTERFACE Consultingwerk.Util.ISetFrameBackgroundColorParameter:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for BROWSE widgets BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BrowseColor AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for BROWSE widgets
                 LABEL-BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BrowseColumnColor AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FRAME and WINDOW
                 widgets BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Color AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if LITERAL or TEXT widgets BGCOLOR will be set to
                 piColor as well
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColorLiterals AS LOGICAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FILL-IN, EDITOR,
                 COMBO-BOX widgets BGCOLOR when disabled
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DisabledtextColor AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if WidgetHelper:SetFrameBackground should call the
                 Win32 API LockWindowUpdate to prevent flickering on the screen
        Notes:   Default is FALSE to avoid interfering with potentially existing code
                 to avoid window flickering
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LockWindowUpdate AS LOGICAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the parent FRAME or WINDOW
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Parent AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for RADIO-SET widgets BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RadiosetColor AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FILL-IN, EDITOR,
                 COMBO-BOX widgets BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TextboxColor AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets The color number to be used for TOGGLE-BOX widgets BGCOLOR
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToggleBoxColor AS INTEGER NO-UNDO
    GET.
    SET.

END INTERFACE.
