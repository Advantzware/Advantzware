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
    File        : IEmbeddedWindowForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 08 10:05:22 CET 2008
    Notes       : 
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the main client are of the IEmbeddedWindowForm 
        Notes:   Read-Only                                                                         
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClientArea AS System.Windows.Forms.Control NO-UNDO 
    GET .
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the embedded window widget (ABL window) 
        Notes:   Read-Only, set by EmbedWindow method                                                                         
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EmbeddedWindow AS WIDGET-HANDLE NO-UNDO 
    GET .  
  
    /*------------------------------------------------------------------------------
        Purpose: Flags if the IEmbeddedWindowForm has already been finalized
        Notes:   Prevents duplicate execution of embedfinalize.i
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Finalized AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a comma-delimited list of all FRAME handles in a Window
        Notes:   This may be useful when FRAME's are realized (and made visible) after
                 the Window is shown. In this case it may be difficult to query the 
                 FRAME handles from the Window widget-tree
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FrameHandles AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Controls if the Form should become an MdiChild of not. 
        Notes:                                                                            
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MakeMdiChild AS LOGICAL NO-UNDO  
    GET.
    SET.
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the MENU-BAR of the embedded window 
        Notes:   This property is required, as the embedding seems to remove the 
                 reference to the menu-bar, but the menu-bar is still available
                 (the legacy ABL code).                                       
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY MENU-BAR AS WIDGET-HANDLE NO-UNDO 
    GET .  
    SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if whole MENU-BAR of the window should be rendered as 
                 a toolbar instance 
        Notes:   This is an optional property . 
                 The initial value is FALSE .                                 
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY MenuBarAsToolbar AS LOGICAL NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if MENU-ITEM Type Menu-Items of the Main-Menubar
                 (Windows MENU-BAR) should be rendered as a separate toolbar instance 
        Notes:   This is an optional property . 
                 The initial value is FALSE .                                     
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY MenuItemsAsToolbar AS LOGICAL NO-UNDO 
    GET . 
    SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Allows the definition of a minimum height in Chars for the embedded
                 ABL window 
        Notes:   This property is not required by the standard WinKit functionality 
                 and is reserved for customization                               
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY MinHeightChars AS DECIMAL NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows the definition of a minimum width in Chars for the embedded
                 ABL window 
        Notes:   This property is not required by the standard WinKit functionality 
                 and is reserved for customization                               
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY MinWidthChars AS DECIMAL NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Keeps the original window title (as entered in the AppBuilder) in a 
        Notes:   This is useful if this string shoudl be used for a standard caption 
                 of Ribbon Tabs or similar and during the initalization runtime 
                 specific attributes (like the client or user) are added to the window title.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalWindowTitle AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: A Dictionary of Overlay Controls  
        Notes:   OverlayControls is not a Dictionary with the STRING of the associated
                 widgets handle as the key and the reference to the .NET Control as
                 the value                                
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY OverlayControls AS "System.Collections.Generic.Dictionary<System.String,System.Windows.Forms.Control>":U NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: A reference to the procedure that's owning this 
                 EmbeddedWindowBaseForm instance  
        Notes:                                 
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY ProcedureHandle AS HANDLE NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if the embedded window Form should be shown as a Dialog 
        Notes:   Typically supresses the call of the Show() method in the embedfinalize.i
                 and the call to the Dispose() method in closewindow.i                                
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY ShowAsDialog AS LOGICAL NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraToolbarsManager instance contained
                 in the Embedded Window Form 
        Notes:                                 
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY ToolbarsManager AS UltraToolbarsManager
    GET .  
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the default Row offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                              
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY WindowContainerRowOffset AS INTEGER NO-UNDO 
    GET.
    SET.

     /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Col offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                           
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowContainerColOffset AS INTEGER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: A collection of the Progress.Windows.WindowsContainer controls used
                 by this Form  
        Notes:                                 
    ------------------------------------------------------------------------------*/  
    DEFINE PUBLIC PROPERTY WindowContainers AS "System.Collections.Generic.List<Progress.Windows.WindowContainer>":U NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Registry Key used for storing this Forms
                 window position                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowPositionRegistryKey AS CHARACTER NO-UNDO 
    GET.
    SET.
        
    /*------------------------------------------------------------------------------
        Purpose: Requests closing of the embedded window                                                                      
        Notes:   
        @param lOk Logical value indicating if the windows were closed                                                                          
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID CloseEmbeddedWindows (OUTPUT lOk AS LOGICAL). 
                
    /*------------------------------------------------------------------------------
        Purpose: Closes the Form after unsubscribing it's own FormClosing event handle                                                                        
        Notes:   Invoked from src/winkit/closewindow.i after the ABL window WINDOW-CLOSE
                 event handler has not cancelled the closing of the WINDOW and Form                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseFromCode ().
                
    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the Progress.Windows.WindowContainer 
                 instance and embeds the given window widget in that instance - 
                 but does not parent the WindowContainer instance in the Form 
                 (THIS-OBJECT). This is delayed till later.    
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.
        @param phWindow The handle of the WINDOW widget to embed                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EmbedWindow (phWindow AS HANDLE) .
  
    /*------------------------------------------------------------------------------
        Purpose: Toggles Sensitivity of OverlayControls based on the sensitive status
                 of the corresponding Widget/Frame                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableDisableOverlayControls () .
      
    /*------------------------------------------------------------------------------
        Purpose: Finalize the embedding of an ABL window widget by parenting 
                 the Progress.Windows.WindowContainer instance in the Form
                 (THIS-OBJECT). Sets the Form Text property and applies the 
                 window-resized event to the window widget.     
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FinalizeEmbedding () .  
  
	/*------------------------------------------------------------------------------
	    Purpose: Returns an indeterminate array of the HANDLEs of the embedded Windows
	    Notes:   
	    @return The array of window HANDLE's
	------------------------------------------------------------------------------*/
	METHOD PUBLIC HANDLE EXTENT GetEmbeddedWindows ().

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the state of all overlay buttons in the IEmbeddedWindowForm                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshButtons () .  
  
    /*------------------------------------------------------------------------------
        Purpose: Update Form properties with Properties from the WindowWidget                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID UpdateWindowAttributes () .
  
    /*------------------------------------------------------------------------------
        Purpose: Toggles Visibily of OverlayControls based on the view/hide status
                 of the corresponding Widget/Frame                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ViewHideOverlayControls ().
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the WindowContainer that contains the widget                                                                     
        Notes:
        @param phWidget The handle of the WINDOW widget to locate
        @return The reference to the WindowContainer that contains the WINDOW                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Windows.WindowContainer WindowContainerOfWidget (phWidget AS HANDLE).
          
END INTERFACE.