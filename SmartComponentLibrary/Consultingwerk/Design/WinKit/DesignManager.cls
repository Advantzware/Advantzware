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
    File        : DesignManager
    Purpose     : Provides a Notify Icon in the System Tray that allows
                  starting the Toolbar & Ribbon Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 11 14:26:47 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING System.Windows.Forms.*                      FROM ASSEMBLY .

CLASS Consultingwerk.Design.WinKit.DesignManager: 

    DEFINE PRIVATE STATIC VARIABLE oNotifyIcon AS NotifyIcon NO-UNDO .  

    {Consultingwerk/Design/WinKit/ttEmbeddedWindows.i &ACCESS = "PRIVATE STATIC"}    

    /*------------------------------------------------------------------------------
        Purpose: Closes the Design Manager                                                                        
        Notes:    Removes the Notify Icon from the System Tray                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Close ():
        
        IF VALID-OBJECT (oNotifyIcon) THEN DO:
            oNotifyIcon:Visible = FALSE .
            oNotifyIcon:Dispose () .
            oNotifyIcon = ? .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Designer Manager                                                                        
        Notes:   Constructs notify icon and context menu                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Initialize ():
        
        DEFINE VARIABLE oMenuItem AS System.Windows.Forms.MenuItem NO-UNDO .
        
        IF NOT VALID-OBJECT (oNotifyIcon) THEN DO:
            FILE-INFORMATION:FILE-NAME = "Consultingwerk/Design/WinKit/monitor_brush.ico":U .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN 
                UNDO, THROW NEW AppError ("Unable to find icon monitor_brush.ico for NotifyIcon."{&TRAN}, 0) .            
            
            oNotifyIcon = NEW NotifyIcon () .
            oNotifyIcon:Icon = NEW System.Drawing.Icon (FILE-INFORMATION:FULL-PATHNAME) .
            oNotifyIcon:Visible = TRUE .
            
            oNotifyIcon:ShowBalloonTip (10000, 
                                        "WinKit Designer"{&TRAN}, 
                                        "Right click here for WinKit Desgin options"{&TRAN}, 
                                        ToolTipIcon:Info) .
                                        
            oNotifyIcon:ContextMenu = NEW ContextMenu () .
            oMenuItem = oNotifyIcon:ContextMenu:MenuItems:Add ("Toolbar and Ribbon Designer"{&TRAN}) .
            oMenuItem:Tag = "ToolbarRibbon":U .
            oMenuItem:Click:Subscribe (MenuItemClick) .                                    
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Toolbar and Ribbon Designer                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID InvokeToolbarRibbonDesigner ():
        
        DEFINE VARIABLE oForm AS Consultingwerk.Design.WinKit.EmbeddedWindowFormSelectorForm NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
        
        LocateEmbeddedWindows () .

        oForm = NEW Consultingwerk.Design.WinKit.EmbeddedWindowFormSelectorForm () .
        oForm:ReceiveEmbeddedWindows (TABLE ttEmbeddedWindows) . 
        
        WAIT-FOR oForm:ShowDialog () SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult,
                                              DialogResult:Ok) THEN DO:
                                                  
            IF VALID-OBJECT (oForm:SelectedEmbeddedWindowForm) AND 
               VALID-OBJECT (oForm:SelectedEmbeddedWindowForm:ToolbarsManager) THEN 
               
               Consultingwerk.Design.WinKit.Toolbar.ToolbarDesignerForm:ShowDesigner 
                   (oForm:SelectedEmbeddedWindowForm:ToolbarsManager,
                    oForm:SelectedEmbeddedWindowForm:ProcedureHandle) .
        END.                                                 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Searches for all current running embedded windows                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID LocateEmbeddedWindows ():
        
        DEFINE VARIABLE oEmbeddedWindows AS IEmbeddedWindowForm  NO-UNDO .
        DEFINE VARIABLE oObject          AS Progress.Lang.Object NO-UNDO . 

        EMPTY TEMP-TABLE ttEmbeddedWindows . 

        ASSIGN oObject = SESSION:FIRST-OBJECT .
        
        DO WHILE VALID-OBJECT (oObject):
            
            IF TYPE-OF (oObject, Progress.Windows.Form) AND 
               TYPE-OF (oObject, IEmbeddedWindowForm) THEN DO:
                   
                   ASSIGN oEmbeddedWindows = CAST (oObject, IEmbeddedWindowForm) . 
                   
                   CREATE ttEmbeddedWindows.
                   ASSIGN ttEmbeddedWindows.EmbeddedWindow  = oEmbeddedWindows
                          ttEmbeddedWindows.WindowTitle     = CAST (oObject, Progress.Windows.Form):Text
                          ttEmbeddedWindows.ClassName       = oObject:GetClass():TypeName
                          ttEmbeddedWindows.ProcedureName   = oEmbeddedWindows:ProcedureHandle:FILE-NAME
                       NO-ERROR  .                   
            END.
            
            oObject = oObject:NEXT-SIBLING . 
            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MenuItemClick event of the notify icon context
                 menu                                                                        
        Notes:       
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC VOID MenuItemClick (sender AS System.Object,
                                              e AS System.EventArgs):
        
        DEFINE VARIABLE cTag AS CHARACTER NO-UNDO.
        
        ASSIGN cTag = CAST (sender, MenuItem):Tag:ToString () . 
        
        CASE cTag:
            WHEN "ToolbarRibbon":U THEN
                InvokeToolbarRibbonDesigner () . 
        END CASE.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DesignManager class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DesignManager ():

        IF VALID-OBJECT (oNotifyIcon) THEN DO:
            oNotifyIcon:VISIBLE = FALSE .
            oNotifyIcon:Dispose () .
        END.

    END DESTRUCTOR.

END CLASS.