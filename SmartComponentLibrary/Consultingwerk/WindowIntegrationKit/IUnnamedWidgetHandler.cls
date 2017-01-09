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
    File        : IUnnamedWidgetHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 25 19:31:37 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY . 
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Handles unnamed widgets found during the rendering of menu's and
                 toolbars to UltraToolbarsHandler                                                                       
        Notes:   May return alternative widget name - or ERROR
                 When alternative widget name is used, it's recommended to assign 
                 this to the widget:NAME as well
        @param phWidget The unnamed widget that needs to be handled
        @param phProcedure The owning procedure handle; may be used for call backs
        @param poToolbarsManager The owning UltraToolbarsManager instance (where the ButtonTool should have been created)
        @return The tool name (key) to use for the unnamed widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER HandleUnnamedWidget (phWidget AS HANDLE,
                                                 phProcedure AS HANDLE,
                                                 poToolbarsManager AS UltraToolbarsManager).

END INTERFACE.