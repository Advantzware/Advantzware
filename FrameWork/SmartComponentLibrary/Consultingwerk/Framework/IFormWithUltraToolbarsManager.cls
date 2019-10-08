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
    File        : IFormWithUltraToolbarsManager
    Purpose     : Interface defining properties required to render ABL
                  MENU-BAR strcutures as UltraToolbarsManager menus using
                  the Consultingwerk.Util.UltraToolbarsManager
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 24 15:13:18 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

{ Consultingwerk/products.i }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .
&ENDIF

INTERFACE Consultingwerk.Framework.IFormWithUltraToolbarsManager:

    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the ABL MENU-BAR widget associated with this
                 Form / the owned UltraToolbarsManager
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MENU-BAR AS WIDGET-HANDLE NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the MENU-BAR should be rendered should be rendered
                 as a Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuBarAsToolbar AS LOGICAL NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if MENU-ITEM widgets should be rendered should be rendered
                 as a Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuItemsAsToolbar AS LOGICAL NO-UNDO
    GET .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the Reference to the UltraToolbarsManager owned by the Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToolbarsManager AS UltraToolbarsManager NO-UNDO
    GET .
&ENDIF
END INTERFACE.
