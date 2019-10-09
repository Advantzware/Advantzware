/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IMainMenuRenderer
    Purpose     : Defines functioanlity required for classes that can
                  render the main menu and ribbon tabs
    Syntax      :
    Description : For a reference implementation please see class
                  Consultingwerk.Windows.Framework.Controls.MainMenuRenderer
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu May 26 14:41:09 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*         FROM ASSEMBLY .
USING Infragistics.Win.UltraWinExplorerBar.*      FROM ASSEMBLY .
USING System.Windows.Forms.*                      FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.Controls.IMainMenuRenderer:

    /*------------------------------------------------------------------------------
        Purpose: Render or refreshes the SmartFramework Menu displayed in an
                 ultraExplorerBar and ultraToolbarsManager instance
        Notes:
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
        @param poUltraToolbarsManager The UltraToolbarsManager instance that owns the RibonTab
        @param poForm The form instance the menu is rendered to.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RenderMenuStructure (poUltraExplorerBar AS UltraExplorerBar,
                                            poUltraToolbarsManager AS UltraToolbarsManager,
                                            poForm AS Form).

END INTERFACE.