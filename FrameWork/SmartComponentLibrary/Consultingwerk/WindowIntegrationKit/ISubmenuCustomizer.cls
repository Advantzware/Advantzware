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
    File        : ISubmenuCustomizer
    Purpose     : Allows customization of rendered SUB-MENU items. Typically
                  used to resolve duplicate "File" submenu's when for the 
                  first time formerly indepent application windows get 
                  integrated/menu merged with an MDI container 
                  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 30 10:21:46 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.ISubmenuCustomizer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Customizes the properties of a new create PopupMenuTool in relation
                 to a SUBMENU widget handle                                                                        
        Notes:   
        @param phWidget The handle of the SUBMENU widget
        @param poPopupMenuTool The reference to the PopupMenuTool rendered for the SUBMENU
        @return Return TRUE when changes have been made, otherwise FALSE                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL AssignPopupMenuToolProperties (phWidget AS HANDLE, 
                                                         poPopupMenuTool AS PopupMenuTool).

END INTERFACE.