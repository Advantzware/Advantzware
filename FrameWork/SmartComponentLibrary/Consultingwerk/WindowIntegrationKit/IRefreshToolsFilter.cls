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
    File        : IRefreshToolsFilter
    Purpose     : Allows filtering out widgets/tools from the UltraToolbarsHelper:RefreshTools   
                  API functionality
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 01 15:11:32 EST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.WindowIntegrationKit.IRefreshToolsFilter:  
  
    /*------------------------------------------------------------------------------
       Purpose: Allows filtering out widgets/tools from the UltraToolbarsHelper:RefreshTools   
                API functionality
       Notes:
       @param poTool The reference to the current base Tool
       @param phWidget The handle of the represented ABL widget (menu item, button, ...)
       @return Logical value indicating if the Widget should be filtered out or not (FALSE will skip the current widget)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FilterTool (poTool AS Infragistics.Win.UltraWinToolbars.ToolBase,
                                      phWidget AS HANDLE).

END INTERFACE. 