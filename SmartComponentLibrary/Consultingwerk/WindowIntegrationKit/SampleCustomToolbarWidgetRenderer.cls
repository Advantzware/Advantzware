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
    File        : SampleCustomToolbarWidgetRenderer
    Purpose     : Sample ICustomToolbarWidgetRenderer implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 23 16:16:23 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY . 
USING Progress.Lang.*.


CLASS Consultingwerk.WindowIntegrationKit.SampleCustomToolbarWidgetRenderer
    IMPLEMENTS Consultingwerk.WindowIntegrationKit.ICustomToolbarWidgetRenderer: 

    DEFINE VARIABLE lFirst AS LOGICAL NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Indicates the start of the rendering of a new RibbonGroup                                                                      
        Notes:   Provides transactional support with EndToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poRibbonGroup The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginRibbonGroup (poToolbarsManager AS UltraToolbarsManager, 
                                         poRibbonGroup AS RibbonGroup):
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Indicates the start of the rendering of a new Toolbar                                                                      
        Notes:   Provides transactional support with EndToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginToolbar (poToolbarsManager AS UltraToolbarsManager,
                                     poToolbar AS UltraToolbar):
        
        ASSIGN lFirst = TRUE  .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Indicates the end of the rendering of a new RibbonGroup                                                                      
        Notes:   Provides transactional support with BeginToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poRibbonGroup The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndRibbonGroup (poToolbarsManager AS UltraToolbarsManager, 
                                       poRibbonGroup AS RibbonGroup):
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Indicates the end of the rendering of a new Toolbar                                                                      
        Notes:   Provides transactional support with BeginToolbar                          
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndToolbar (poToolbarsManager AS UltraToolbarsManager,
                                   poToolbar AS UltraToolbar):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Indicates of the provided widget should be rendered by the ICustomToolbarWidgetRendered                                                                      
        Notes:   This method provides the ability to control which widgets will (later) 
                 be passed to the method RenderWidget                                                           
        @param phWidget The handle of the Widget that should be tested
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @return True when the Widget should be passed to RenderWidget, False when not
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FilterWidget (phWidget AS HANDLE, 
                                        poToolbarsManager AS UltraToolbarsManager):
        
        IF phWidget:TYPE <> Consultingwerk.WidgetTypeEnum:FillIn THEN 
            RETURN FALSE . 
    
        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Indicates of the provided widget should be rendered by the ICustomToolbarWidgetRendered                                                                      
        Notes:   This method provides the ability to control which widgets will (later) 
                 be passed to the method RenderWidget                                                           
        @param phWidget The handle of the Widget that should be tested
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance used for rendering the Toolbar
        @param poToolbar The Infragistics.Win.UltraWinToolbars.UltraToolbar instance used for rendering the Toolbar
        @return The Infragistics ToolBase instance that was created for the ABL Widget (root tool instance) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ToolBase RenderWidget (phWidget AS HANDLE, 
                                         poToolbarsManager AS UltraToolbarsManager, 
                                         poToolbar AS UltraToolbar):
        
        DEFINE VARIABLE oTextBoxTool  AS TextBoxTool NO-UNDO . 
        DEFINE VARIABLE oTextBoxToolI AS TextBoxTool NO-UNDO . 

        DEFINE VARIABLE oSpacer       AS LabelTool   NO-UNDO .
        DEFINE VARIABLE oSpacerI      AS LabelTool   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWidget Consultingwerk.WidgetTypeEnum:FillIn} .

        IF lFirst THEN DO:
            ASSIGN oSpacer  = NEW LabelTool (SUBSTITUTE ("&1_space":U, phWidget:NAME)) 
                   oSpacerI = NEW LabelTool (SUBSTITUTE ("&1_space":U, phWidget:NAME)) .
         
            poToolbarsManager:Tools:Add (oSpacer) .
            oSpacer:SharedProps:Width = 50 . 
            oSpacer:SharedProps:ShowInCustomizer = FALSE  .
            poToolbar:Tools:Add (oSpacerI) .       
        END.    
                                                          
        ASSIGN oTextBoxTool  = NEW TextBoxTool (phWidget:NAME) 
               oTextBoxToolI = NEW TextBoxTool (phWidget:NAME) .                                                 
        
        poToolbarsManager:Tools:Add (oTextBoxTool) .

        ASSIGN 
            oTextBoxTool:SharedProps:Width             = 30 
            oTextBoxTool:SharedProps:Enabled           = phWidget:SENSITIVE 
            oTextBoxTool:Text                          = phWidget:SCREEN-VALUE 
            oTextBoxToolI:InstanceProps:IsFirstInGroup = lFirst .

        poToolbar:Tools:Add (oTextBoxToolI) .

        ASSIGN lFirst = FALSE . 

        RETURN oTextBoxTool .

    END METHOD.

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
                                                                           poRibbonGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
        
        DEFINE VARIABLE oTextBoxTool  AS TextBoxTool NO-UNDO . 
        DEFINE VARIABLE oTextBoxToolI AS TextBoxTool NO-UNDO . 

        DEFINE VARIABLE oSpacer       AS LabelTool   NO-UNDO .
        DEFINE VARIABLE oSpacerI      AS LabelTool   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWidget Consultingwerk.WidgetTypeEnum:FillIn} .
                                            
        ASSIGN oTextBoxTool  = NEW TextBoxTool (phWidget:NAME) 
               oTextBoxToolI = NEW TextBoxTool (phWidget:NAME) .                                                 
        
        poToolbarsManager:Tools:Add (oTextBoxTool) .

        ASSIGN 
            oTextBoxTool:SharedProps:Width             = 30 
            oTextBoxTool:SharedProps:Enabled           = phWidget:SENSITIVE 
            oTextBoxTool:Text                          = phWidget:SCREEN-VALUE 
            oTextBoxToolI:InstanceProps:IsFirstInGroup = lFirst .

        poRibbonGroup:Tools:Add (oTextBoxToolI) .

        ASSIGN lFirst = FALSE . 

        RETURN oTextBoxTool .
        
    END METHOD .

END CLASS.
