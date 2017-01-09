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
    File        : EmbeddedWindowForm
    Purpose     : Default IEmbeddedWindowForm implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 14:12:13 CET 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.*  FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Helper.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*          FROM ASSEMBLY .  
USING Progress.Lang.*                              FROM PROPATH .
USING System.Drawing.*                             FROM ASSEMBLY .     
USING System.Windows.Forms.*                       FROM ASSEMBLY .

CLASS Consultingwerk.WindowIntegrationKit.Forms.EmbeddedWindowForm 
    INHERITS EmbeddedWindowFormDesigner 
    IMPLEMENTS IEmbeddedWindowForm, IFormWithUltraToolbarsManager
        USE-WIDGET-POOL :

    DEFINE VARIABLE WinContainer AS Progress.Windows.WindowContainer NO-UNDO.
        
    DEFINE PRIVATE VARIABLE deltaCol AS INTEGER NO-UNDO . 
    DEFINE PRIVATE VARIABLE deltaRow AS INTEGER NO-UNDO .         
    
    DEFINE VARIABLE iWindowStartWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWindowStartHeight AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE oTimer          AS System.Windows.Forms.Timer NO-UNDO . 
    DEFINE VARIABLE oToolClickTimer AS System.Windows.Forms.Timer NO-UNDO .
    
    DEFINE VARIABLE oHideButtonsPanel AS Infragistics.Win.Misc.UltraPanel NO-UNDO . 
    
    /*------------------------------------------------------------------------------
       Purpose: Returns a new instance of an EmbeddedWindowForm. 
       Notes:                                                                                                                                                                                                                                                               
    ------------------------------------------------------------------------------*/      
    CONSTRUCTOR PUBLIC EmbeddedWindowForm ():
  
       SUPER ().
       
       THIS-OBJECT:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1 . 
       
       ASSIGN THIS-OBJECT:WindowContainerColOffset = WinKitForms:DefaultWindowContainerColOffset
              THIS-OBJECT:WindowContainerRowOffset = WinKitForms:DefaultWindowContainerRowOffset .
       
       IF VALID-OBJECT(THIS-OBJECT:ToolbarsManager) THEN DO:
           THIS-OBJECT:ToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .
           THIS-OBJECT:ToolbarsManager:BeforeToolDropdown:Subscribe (ToolDropDownHandler) .
       END.
       
       THIS-OBJECT:FormClosing:Subscribe(FormClosingHandler) .
       
       oTimer = NEW System.Windows.Forms.Timer () .
       oTimer:Interval = 1 . 
       oTimer:Enabled = FALSE .
       oTimer:Tick:Subscribe (TimerTickHandler) .

       oToolClickTimer = NEW System.Windows.Forms.Timer () .
       oToolClickTimer:Interval = 1 . 
       oToolClickTimer:Enabled = FALSE .
       oToolClickTimer:Tick:Subscribe (ToolClickTimerTickHandler) .
       
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
		Purpose: Closes the Form after unsubscribing it's own FormClosing event handle  																	  
		Notes:   Invoked from src/winkit/closewindow.i after the ABL window WINDOW-CLOSE
		         event handler has not cancelled the closing of the WINDOW and Form     																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CloseFromCode ():
		
		THIS-OBJECT:FormClosing:Unsubscribe(FormClosingHandler) .
        
        THIS-OBJECT:Close () .

 	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the Progress.Windows.WindowContainer 
                 instance and embeds the given window widget in that instance - 
                 but does not parent the WindowContainer instance in the Form 
                 (THIS-OBJECT). This is delayed till later.    
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.
        @param phWindow The handle of the Window to embed                                                                         
    ------------------------------------------------------------------------------*/    
     METHOD PUBLIC VOID EmbedWindow (phWindow AS HANDLE):         
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        
         ASSIGN 
             iWindowStartHeight         = phWindow:HEIGHT-PIXELS
             iWindowStartWidth          = phWindow:WIDTH-PIXELS  
             THIS-OBJECT:EmbeddedWindow = phWindow . 
                
        WinContainer = NEW Progress.Windows.WindowContainer( ).
        
        THIS-OBJECT:WindowContainers:Add (WinContainer) .
        
        THIS-OBJECT:ClientSize = NEW Size (phWindow:WIDTH-PIXELS + (THIS-OBJECT:ClientSize:Width - 
                                                                    THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:ClientSize:Width), 
                                           phWindow:HEIGHT-PIXELS +  (THIS-OBJECT:ClientSize:Height - 
                                                                      THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:ClientSize:Height)).
        WinContainer:Size = NEW Size(phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        WinContainer:MinimumSize = NEW Size(phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        
        WinContainer:EmbeddedWindow = phWindow.       
        
        /* Mike Fechner, Consultingwerk Ltd. 23.09.2009
           Ability to set a logical offset for the location of the WindowContainer 
           within this .NET Form. Enables "hiding" of the empty space resulting
           that results from a hidden toolbar  */             
           
        RepositionWindowContainer () .   

        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        WinContainer:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).

     END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finalize the embedding of an ABL window widget by parenting 
                 the Progress.Windows.WindowContainer instance in the Form
                 (THIS-OBJECT). Sets the Form Text property and applies the 
                 window-resized event to the window widget.     
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FinalizeEmbedding ():
        
        DEFINE VARIABLE oPanel  AS System.Windows.Forms.Panel NO-UNDO .
        DEFINE VARIABLE hWindow AS HANDLE                     NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow 
               oPanel  = NEW System.Windows.Forms.Panel () 
               
               THIS-OBJECT:Finalized = TRUE .
        
        /* Mike Fechner, Consultingwerk Ltd. 16.11.2010
           Overlay the WindowContainer with a Panel to avoid flickering */
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:SuspendLayout() .
        oPanel:Dock = DockStyle:Fill .
/*        oPanel:BackColor = System.Drawing.Color:Red .*/
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Controls:Add (oPanel) .
        oPanel:BringToFront() .
        
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:SuspendLayout() .
        
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Dock = System.Windows.Forms.DockStyle:None.
        
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Size = NEW System.Drawing.Size (WinContainer:WIDTH + WinContainer:LEFT,
                                                                                          WinContainer:HEIGHT + WinContainer:Top ) . 
        
        WinContainer:Parent = THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel.

        WinContainer:Size = NEW System.Drawing.Size (
                MAX(THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Width + deltaCol, 0),
                MAX(THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Height + deltaRow + THIS-OBJECT:WindowContainerHeightOffset, 0)).
        
        IF THIS-OBJECT:WindowContainerHeightOffset > 0 THEN DO:

            oHideButtonsPanel = NEW  Infragistics.Win.Misc.UltraPanel () .
            oHideButtonsPanel:SIZE = NEW System.Drawing.Size(THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Width,
                                                             3000) .
                                                             
            oHideButtonsPanel:TOP = THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Height + 3 .
           
            oHideButtonsPanel:BringToFront() .
           
            EmbeddedWindowFormDesigner_Fill_Panel:Controls:Add (oHideButtonsPanel) .                                                            
            
        END. 
        
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.

        THIS-OBJECT:Height = THIS-OBJECT:Height -
                             THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Height + 
                             iWindowStartHeight - 
                             THIS-OBJECT:WindowContainerRowOffset .
      
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:ResumeLayout(TRUE) .

        THIS-OBJECT:TEXT = hWindow:TITLE .
        
        APPLY "window-resized":U TO hWindow .            
        
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:Controls:Remove (oPanel) .
        THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel:ResumeLayout() .
        
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the FormClosing event of the Form. When a window
                 is embedded, the FormClosing event will be cancelled and the 
                 WINDOW-CLOSE event to the window will be applied.                                                                         
        Notes:   The window is responsible for deleting the Form.  
                 {src/winkit/closewindow.i} can be used in the ON CLOSE OF THIS-
                 PROCEDURE trigger for that purpose   
        @param sender The reference to the object that raised the event
        @param e The FormClosingEventArgs with the data for this event                                                                                      
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FormClosingHandler (sender AS System.Object, 
                                           e AS FormClosingEventArgs):

        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
                
        /* Mike Fechner, Consultingwerk Ltd. 22.02.2012 
           When the WINDOW Widget is in-SENSITIVE, cancel the FormClosing
           Event in any case */
        IF VALID-HANDLE (hWindow) AND hWindow:SENSITIVE = FALSE THEN DO:
           e:Cancel = TRUE .
           RETURN .
        END.
                               
       IF Progress.Util.EnumHelper:AreEqual(e:CloseReason,
                                            CloseReason:UserClosing) THEN 
           IF VALID-HANDLE(hWindow) THEN DO:
               e:Cancel = TRUE .    
        
               oTimer:ENABLED = TRUE .                          

               /* Mike Fechner, Consultingwerk Ltd. 24.02.2012 - WinKit PoC
                  APPLY window-close moved to the Timer-Tick event handler,
                  so that the .NET Form has finished it's Close event handler, 
                  when the ABL WINDOW-CLOSE behaviour is started. */

/*                 APPLY "window-close":U TO hWindow .                             */
           END.                     

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Moves the UltraPanel used to hide the section of the embedded window
                 truncated from the bottom (WindowContainerHeightOffset) during a 
                 resizing operation that makes the Form larger in height                                                                     
        Notes:   Invoke this method from the WINDOW-RESIZED trigger of the ABL 
                 window
        @param piTop The distance in pixels from the Top of the screen                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MoveHideButtonPanel (piTop AS INTEGER):
        
        IF VALID-OBJECT (oHideButtonsPanel) THEN DO:
            oHideButtonsPanel:Location = NEW System.Drawing.Point (0, piTop) . 
    
            
            oHideButtonsPanel:Size = NEW System.Drawing.Size (3000, 3000) . 

            oHideButtonsPanel:BringToFront() .
        
        END. 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Activated event                                                                      
        Notes:   Delegate the Activated Event to the Embedded Window (ENTRY)                                                                       
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnActivated (e AS System.EventArgs):
        
        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        
        SUPER:OnActivated (e) .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        IF VALID-HANDLE (hWindow) THEN 
            APPLY "ENTRY":U TO hWindow .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosed event  
        Notes:   
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnFormClosed (e AS System.Windows.Forms.FormClosedEventArgs):

        IF VALID-OBJECT (oToolClickTimer) THEN DO:
            oToolClickTimer:Enabled = FALSE . 
            oToolClickTimer:Tick:Unsubscribe (ToolClickTimerTickHandler) .
            
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oToolClickTimer) .
        END.
		
		SUPER:OnFormClosed(INPUT e).

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Don't forward sizing events when the window is minimized                                                                      
        Notes:                                                                        
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnSizeChanged (e AS System.EventArgs):

        IF Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.FormWindowState:Minimized,
                                              THIS-OBJECT:WindowState) THEN . 
        ELSE 
            SUPER:OnSizeChanged (INPUT e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the state of all overlay buttons in the IEmbeddedWindowForm                                                                      
        Notes: 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshButtons ():
        
        DEFINE VARIABLE oWindowContainer AS System.Windows.Forms.Control . 
        
        Consultingwerk.Util.WidgetHelper:RefreshButtons (THIS-OBJECT:ClientArea) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adjusts the position of the WindowContainer based on the Col- and
                 Row-Offset                                                                  
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID RepositionWindowContainer ():

        IF NOT VALID-OBJECT (WinContainer) THEN
            RETURN . 
        
        WinContainer:Location = NEW System.Drawing.Point (0 - deltaCol, 0 - deltaRow) . 
        WinContainer:Size = NEW System.Drawing.Size (WinContainer:Width + deltaCol, WinContainer:Height + deltaRow) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Event Handler for the Tick Event of the Timer Component                                                                                                                                                                                                                                                                        
       Notes:                                                          
       @param sender The event sender
       @param e The .NET default System.EventArgs for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TimerTickHandler (sender AS System.Object,
                                            e AS System.EventArgs):
                   
       DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
                   
        IF VALID-HANDLE (THIS-OBJECT:EmbeddedWindow) THEN DO:
            
            ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow . 
            
            APPLY "WINDOW-CLOSE":U TO hWindow .
        END.
                   
        CATCH err AS Progress.Lang.Error :
            /* noop */         
        END CATCH.                                                      

        FINALLY:
            oTimer:Enabled = FALSE .                               
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the ToolClick Event of the UltraToolbarsManager                                                                                                                                                                                                                                                                              
        Notes:   Applies CHOOSE to the base menu item
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/      
    METHOD PRIVATE VOID ToolClickHandler (sender AS System.Object,
                                          e AS ToolClickEventArgs):
                                                                                              
        DEFINE VARIABLE hMenuItem   AS HANDLE NO-UNDO.
        DEFINE VARIABLE hFrame      AS HANDLE NO-UNDO.
        DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
    
        THIS-OBJECT:UpdateControlsForEventHandler () .
        
        ASSIGN hMenuItem = WIDGET-HANDLE(STRING(THIS-OBJECT:ToolbarsManager:Tools[e:Tool:Key]:Tag)) .
            
        IF VALID-HANDLE(hMenuItem) THEN 
        CASE hMenuItem:TYPE:
            WHEN "MENU-ITEM":U THEN DO:
                CASE hMenuItem:SUBTYPE:
                    WHEN "NORMAL":U THEN DO:
                        IF TYPE-OF (e:Tool, StateButtonTool) 
                           AND CAN-QUERY (hMenuItem, "TOGGLE-BOX":U) 
                           AND hMenuItem:TOGGLE-BOX = TRUE 
                           AND NOT Consultingwerk.Util.UltratoolbarsHelper:RefreshingToolbar
                        THEN DO:
                            
                            hMenuItem:CHECKED = CAST (e:Tool, StateButtonTool):Checked .
                            
                            APPLY "VALUE-CHANGED":U TO hMenuItem .
                        END.
                        ELSE 
                            APPLY "CHOOSE":U TO hMenuItem .
                    END.
                    OTHERWISE 
                        MESSAGE SUBSTITUTE ("ToolClick event for MENU-ITEM of sub-type &1 not yet implemented."{&TRAN},
                                            hMenuItem:SUBTYPE) .
                END CASE .                          
            END .
            WHEN "BUTTON":U THEN DO:
                oToolClickTimer:Tag = BOX (STRING (hMenuItem)) .
                oToolClickTimer:Enabled = TRUE . 
            END.
            OTHERWISE MESSAGE SUBSTITUTE ("ToolClick event for MENU-ITEM of type &1 not yet implemented."{&TRAN},
                                          hMenuItem:TYPE) .             
        END CASE.  
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the ToolClickTimer component 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ToolClickTimerTickHandler (sender AS System.Object,
	                                               e AS System.EventArgs):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
		
		oToolClickTimer:Enabled = FALSE . 

        ASSIGN hWidget = WIDGET-HANDLE (UNBOX (oToolClickTimer:Tag)) NO-ERROR . 
        
        oToolClickTimer:Tag = ? .
        
        IF NOT VALID-HANDLE (hWidget) THEN 
            RETURN . 

        APPLY "CHOOSE":U TO hWidget .

	END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Handles the BeforeToolDropDown event of the UltraToolbarsManager
       Notes:   Delegates action to the MENU-DROP Event of the Progress SUB-MENU widget 
       @param sender The reference to the object that raised the event
       @param e The BeforeToolDropdownEventArgs with the data for this event                                                                                                                                                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolDropDownHandler (sender AS System.Object,
                                               e AS Infragistics.Win.UltraWinToolbars.BeforeToolDropdownEventArgs):
           
        DEFINE VARIABLE cHandle AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hMenu   AS HANDLE NO-UNDO.
           
        ASSIGN cHandle = UNBOX (e:Tool:SharedProps:RootTool:Tag) 
               hMenu   = WIDGET-HANDLE (cHandle) .
           
        IF VALID-HANDLE (hMenu) AND 
            hMenu:TYPE = Consultingwerk.WidgetTypeEnum:SubMenu AND
            hMenu:SENSITIVE THEN DO:
                  
            APPLY "MENU-DROP":U TO hMenu .
       END.
                   
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Update Form properties with Properties from the WindowWidget                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID UpdateWindowAttributes ():
        
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
                
        DEFINE VARIABLE hWindow  AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hWidget  AS HANDLE                       NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
                
        IF VALID-HANDLE (hWindow) THEN DO:
            ASSIGN THIS-OBJECT:Text    = hWindow:TITLE 

                   THIS-OBJECT:ToolbarsManager:Enabled = hWindow:SENSITIVE 
                   
/*                   THIS-OBJECT:Enabled = hWindow:SENSITIVE*/
                   .  
            /* Mike Fechner, Consultingwerk Ltd. 22.02.2012 - WinKit PoC
               Controls that overlay ABL Widgets shall be Enabled, when both the Window 
               and the associated Widget are enabled */
            {Consultingwerk/foreach.i "'System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Control>'" oPair in this-object:OverlayControls}

                ASSIGN oControl = oPair:Value .

                IF TYPE-OF (oControl, Consultingwerk.WindowIntegrationKit.Controls.IOverlayControl) THEN 
                    ASSIGN hWidget = CAST (oControl, Consultingwerk.WindowIntegrationKit.Controls.IOverlayControl):WidgetHandle .
                ELSE 
                    NEXT .  
            
                oControl:Enabled = hWindow:SENSITIVE AND hWidget:SENSITIVE .
            END.
        END.

    END METHOD.
         
    /*------------------------------------------------------------------------------
        Purpose: Returns the parent control for Controls placed in this Form      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY ClientArea AS System.Windows.Forms.Control NO-UNDO 
    GET:
        RETURN THIS-OBJECT:EmbeddedWindowFormDesigner_Fill_Panel .        
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Col offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                           
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowContainerColOffset AS INTEGER NO-UNDO 
    GET():
        RETURN deltaCol . 
    END GET.
    SET(INPUT arg AS INTEGER):
        ASSIGN deltaCol = arg .
        
        RepositionWindowContainer () .
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Number of pixels from the Bottom of the Embedded ABL 
                 Window Widget that should be hidden by making the WindowContainer larger
                 than the Form's client area 
        Notes:   
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY WindowContainerHeightOffset AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Row offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                           
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY WindowContainerRowOffset AS INTEGER NO-UNDO 
    GET():
        RETURN deltaRow .
    END GET.
    SET(INPUT arg AS INTEGER):
        ASSIGN deltaRow = arg .
        
        RepositionWindowContainer () . 
    END SET.
 
END CLASS.
