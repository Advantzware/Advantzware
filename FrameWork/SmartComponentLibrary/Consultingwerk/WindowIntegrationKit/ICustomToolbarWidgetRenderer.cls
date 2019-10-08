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
    File        : ICustomToolbarWidgetRenderer
    Purpose     : Interface supporting the rendering of custom ABL Widgets  
                  in the Infragistics Toolbar by the UltraToolbarsHelper                
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 23 15:49:33 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.WindowIntegrationKit.ICustomToolbarWidgetRenderer:  

    /*------------------------------------------------------------------------------
        Purpose: Indicates the start of the rendering of a new RibbonGroup                                                                      
        Notes:   Provides transactional support with EndToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poRibbonGroup The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginRibbonGroup (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                         poRibbonGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup) .

    /*------------------------------------------------------------------------------
        Purpose: Indicates the start of the rendering of a new Toolbar                                                                      
        Notes:   Provides transactional support with EndToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginToolbar (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                     poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbar) .
  
    /*------------------------------------------------------------------------------
        Purpose: Indicates the end of the rendering of a new RibbonGroup                                                                      
        Notes:   Provides transactional support with BeginToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poRibbonGroup The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndRibbonGroup (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                       poRibbonGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup) .
                                       
    /*------------------------------------------------------------------------------
        Purpose: Indicates the end of the rendering of a new Toolbar                                                                      
        Notes:   Provides transactional support with BeginToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndToolbar (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                   poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbar) .

    /*------------------------------------------------------------------------------
        Purpose: Indicates of the provided widget should be rendered by the ICustomToolbarWidgetRendered                                                                      
        Notes:   This method provides the ability to control which widgets will (later) 
                 be passed to the method RenderWidget                                                             
        @param phWidget The handle of the Widget that should be tested
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @return True when the Widget should be passed to RenderWidget, False when not
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FilterWidget (phWidget AS HANDLE, 
                                        poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager).

    /*------------------------------------------------------------------------------
        Purpose: Renders the provided widget with the ICustomToolbarWidgetRendered                                                                      
        Notes:   This method provides the ability to control which widgets will (later) 
                 be passed to the method RenderWidget                                                           
        @param phWidget The handle of the Widget that should be tested
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
        @return The Infragistics ToolBase instance that was created for the ABL Widget (root tool instance) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.UltraWinToolbars.ToolBase RenderWidget (phWidget AS HANDLE, 
                                                                           poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                                                           poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbar).

    /*------------------------------------------------------------------------------
        Purpose: Renders the provided widget with the ICustomToolbarWidgetRendered                                                                      
        Notes:   This method provides the ability to control which widgets will (later) 
                 be passed to the method RenderWidget                                                           
        @param phWidget The handle of the Widget that should be tested
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poRibbonGroup The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
        @return The Infragistics ToolBase instance that was created for the ABL Widget (root tool instance) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.UltraWinToolbars.ToolBase RenderWidget (phWidget AS HANDLE, 
                                                                           poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                                                           poRibbonGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup).

END INTERFACE.