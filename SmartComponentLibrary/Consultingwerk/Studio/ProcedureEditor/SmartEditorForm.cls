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
    File        : SmartEditorForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 25 20:33:45 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING com.joanju.proparse.NodeTypes           FROM ASSEMBLY .
USING Consultingwerk.Forms.*                  FROM PROPATH .
USING Consultingwerk.Studio.ProcedureEditor.* FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.*        FROM PROPATH .
USING Consultingwerk.Util.*                   FROM PROPATH . 
USING Consultingwerk.Windows.Controls.*       FROM PROPATH . 
USING org.prorefactor.core.JPNode             FROM ASSEMBLY .
USING org.prorefactor.treeparser.*            FROM ASSEMBLY .
USING Progress.Lang.*                         FROM PROPATH .
USING Infragistics.Win.UltraWinTree.*         FROM ASSEMBLY . 

CLASS Consultingwerk.Studio.ProcedureEditor.SmartEditorForm 
    INHERITS BaseForm: 
	
    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartEditorForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE timer2 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabbedMdiManager1 AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTreeWithSearchControl1 AS Consultingwerk.Windows.Controls.UltraTreeWithSearchControl NO-UNDO.

    DEFINE VARIABLE oEditors    AS "System.Collections.Generic.Dictionary<System.String, System.Windows.Forms.Form>":U   NO-UNDO .

    DEFINE VARIABLE oParseUnits AS "System.Collections.Generic.Dictionary<System.String, System.Object>":U NO-UNDO .

    DEFINE PUBLIC PROPERTY Nodes AS "System.Collections.Generic.Dictionary<System.Int32, org.prorefactor.core.JPNode>":U NO-UNDO 
    GET.
    PRIVATE SET. 
    
    DEFINE VARIABLE oEventImage    AS System.Drawing.Image NO-UNDO . 
    DEFINE VARIABLE oMethodImage   AS System.Drawing.Image NO-UNDO .
    DEFINE VARIABLE oPropertyImage AS System.Drawing.Image NO-UNDO . 
    
    DEFINE PROTECTED TEMP-TABLE ttMethods NO-UNDO 
        FIELD NodeNum    AS INTEGER 
        FIELD MethodName AS CHARACTER 
        INDEX MethodName MethodName . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartEditorForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartEditorForm ():
		
        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.":U &THEN          
        ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 .
        ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = ? . 
        &ENDIF

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        Consultingwerk.Framework.FrameworkSettings:MdiContainer = THIS-OBJECT . 
        Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = TRUE . 

        oEditors = NEW "System.Collections.Generic.Dictionary<System.String, System.Windows.Forms.Form>" () . 
        oParseUnits = NEW "System.Collections.Generic.Dictionary<System.String, System.Object>" () .
        
        THIS-OBJECT:Nodes = NEW "System.Collections.Generic.Dictionary<System.Int32, org.prorefactor.core.JPNode>" (). 
        
        ultraTreeWithSearchControl1:TreeNodeSelected:Subscribe (TreeNodeSelectedHandler) .
        
        oEventImage    = ImageHelper:Load ("Consultingwerk/Studio/ProcedureEditor/Images/event.png":U) .
        oMethodImage   = ImageHelper:Load ("Consultingwerk/Studio/ProcedureEditor/Images/method.png":U) .
        oPropertyImage = ImageHelper:Load ("Consultingwerk/Studio/ProcedureEditor/Images/property.png":U) .
        
        THIS-OBJECT:ultraToolbarsManager1:Tools ["Insert Method":U]:SharedProps:ShortCut
            = KeyboardHelper:ShortcutFromKeyCombination ("SHIFT":U, "ALT":U, "M":U) . 
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs a syntax check 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CheckSyntax ():
		
		DEFINE VARIABLE oEditor AS ProcedureEditorForm NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            RETURN . 
            
        oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                        ProcedureEditorForm) .

        oEditor:CheckSyntax () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Closes all open editors and when successful the SmartEditorForm itself
        Notes:   
        @return Logical value indicating if the editor was closed 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL CloseAll ():
		
        DEFINE VARIABLE oChildForm AS System.Windows.Forms.Form NO-UNDO .
        
        DO WHILE THIS-OBJECT:MdiChildren:Length > 0:

            oChildForm = CAST (THIS-OBJECT:MdiChildren:GetValue (0),
                               System.Windows.Forms.Form) .  

            oChildForm:Close () .

            IF VALID-OBJECT (oChildForm) AND oChildForm:Visible THEN DO: 
                RETURN FALSE .
            END.
        END.        

        THIS-OBJECT:Close () . 

        RETURN TRUE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deletes a temporary file
        Notes:   
        @param pcFileName The name of the temporary file
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DeleteTemporaryFile (pcFileName AS CHARACTER):
		
		DEFINE VARIABLE ex AS Consultingwerk.Exceptions.Exception NO-UNDO . 
		
		OS-DELETE VALUE (pcFileName) .
		
		IF OS-ERROR > 0 THEN 
		    UNDO, THROW NEW Consultingwerk.Exceptions.OsErrorException (OS-ERROR) .
		    
		CATCH err AS Progress.Lang.Error:
        	ex = NEW Consultingwerk.Exceptions.Exception (err, SUBSTITUTE ("Unable to delete &1"{&TRAN}, 
        	                                                               pcFileName), 0) .
        		
            ErrorHelper:ShowErrorMessage (err) .         		
        END CATCH.    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays status in the status bar                                                                       
        Notes:                                    
        @param pcStatus The status to display                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (pcStatus AS CHARACTER):
        
        THIS-OBJECT:ultraStatusBar1:Panels["status":U]:Text = pcStatus . 
    
        PROCESS EVENTS . 
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handle for the FormClosedHandler event of the Procedure Editor 
                 Forms                                                                        
        Notes:        
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event                                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EditorFormClosedHandler (sender AS System.Object,
                                                   e AS System.Windows.Forms.FormClosedEventArgs):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .
        
        oForm = CAST (sender, System.Windows.Forms.Form) . 
        
        IF oEditors:ContainsValue (oForm) THEN DO: 
            {Consultingwerk/foreach.i '"System.Collections.Generic.KeyValuePair<System.String, System.Windows.Forms.Form>"' oPair in oEditors}
            
                IF oPair:Value = oForm THEN DO:
                    oEditors:Remove (oPair:Key) . 
                    
                    LEAVE .                   
                END. 
            END.
        END . 

        oForm:FormClosed:Unsubscribe (EditorFormClosedHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines and sets the Enabled status of the Save button 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID EvaluateSaveEnabled ():

        DEFINE VARIABLE lModified AS LOGICAL             NO-UNDO.
        DEFINE VARIABLE oEditor   AS ProcedureEditorForm NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            ASSIGN lModified = FALSE . 
        ELSE DO:            
            oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                            ProcedureEditorForm) .        
		
		    lModified = oEditor:IsModified .
		END.
		    
        THIS-OBJECT:ultraToolbarsManager1:Tools["save":U]:SharedProps:Enabled = lModified .

        ASSIGN lModified = FALSE .
        
        {Consultingwerk/foreach.i System.Windows.Forms.Form oForm in THIS-OBJECT:MdiChildren}
            oEditor = CAST (oForm,
                            ProcedureEditorForm) .        
            
            IF oEditor:IsModified = TRUE THEN DO: 
                lModified = TRUE .
                LEAVE .
            END.
        END .

        THIS-OBJECT:ultraToolbarsManager1:Tools["save all":U]:SharedProps:Enabled = lModified .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the currently active file
        Notes:   
        @return The name of the currently active file 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetCurrentFileName ():
		
        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Form>"" 
                                  oPair in oEditors}
            
            IF oPair:Value = THIS-OBJECT:ActiveMdiChild THEN 
                RETURN oPair:Key .
        END.            
            
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a JPNode by a node number                                                                       
        Notes:                                    
        @param piNodeNum The node number to return
        @return The refernce to the specified JPNode                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JPNode GetJPNode (piNodeNum AS INTEGER):
        
        RETURN THIS-OBJECT:Nodes[piNodeNum] .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Insert Method":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Search Outline":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Check Syntax":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Outline View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Parse Automatically":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Parse":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("PopupMenuTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("PopupMenuTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool1 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool1 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool2 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool2 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Check Syntax":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Search Outline":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Insert Method":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool3 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool3 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Parse Automatically":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Parse":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("f7f39d56-4d67-4996-997a-94a04f9047a4":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("16bd581f-4e71-40a4-910e-b1dd546cd7fa":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("f7f39d56-4d67-4996-997a-94a04f9047a4":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.ProcedureEditor.SmartEditorForm":U).
        THIS-OBJECT:ultraTreeWithSearchControl1 = NEW Consultingwerk.Windows.Controls.UltraTreeWithSearchControl().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartEditorFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:timer2 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTreeWithSearchControl1 */
        /*  */
        THIS-OBJECT:ultraTreeWithSearchControl1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraTreeWithSearchControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraTreeWithSearchControl1:Name = "ultraTreeWithSearchControl1":U.
        THIS-OBJECT:ultraTreeWithSearchControl1:Size = NEW System.Drawing.Size(254, 406).
        THIS-OBJECT:ultraTreeWithSearchControl1:TabIndex = 10.
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "File":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool9.
        arrayvar0[3] = buttonTool10.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Source":U.
        buttonTool7:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool5:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar1[1] = buttonTool7.
        arrayvar1[2] = buttonTool5.
        arrayvar1[3] = buttonTool3.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        ribbonGroup3:Caption = "Outline View":U.
        stateButtonTool1:Checked = TRUE.
        stateButtonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = stateButtonTool1.
        arrayvar2[2] = buttonTool13.
        ribbonGroup3:Tools:AddRange(arrayvar2).
        ribbonGroup4:Caption = "View":U.
        popupMenuTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = popupMenuTool1.
        ribbonGroup4:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 4 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        arrayvar4[3] = ribbonGroup3.
        arrayvar4[4] = ribbonGroup4.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool2:SharedPropsInternal:Caption = "Open":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        popupMenuTool2:SharedPropsInternal:Caption = "Editor":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = mdiWindowListTool1.
        popupMenuTool2:Tools:AddRange(arrayvar6).
        mdiWindowListTool2:SharedPropsInternal:Caption = "MDIWindowListTool1":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool4:SharedPropsInternal:Caption = "Check Syntax":U.
        buttonTool4:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:ShiftF2.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool6:SharedPropsInternal:Caption = "Search Outline":U.
        buttonTool6:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:CtrlO.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool8:SharedPropsInternal:Caption = "Insert Method":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool11:SharedPropsInternal:AppearancesLarge:Appearance = appearance11.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool11:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool11:SharedPropsInternal:Caption = "Save":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool12:SharedPropsInternal:Caption = "Save All":U.
        stateButtonTool3:Checked = TRUE.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        stateButtonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        stateButtonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        stateButtonTool3:SharedPropsInternal:Caption = "Parse Automatically":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesLarge:Appearance = appearance17.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool14:SharedPropsInternal:Caption = "Parse":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 10 NO-UNDO.
        arrayvar7[1] = buttonTool2.
        arrayvar7[2] = popupMenuTool2.
        arrayvar7[3] = mdiWindowListTool2.
        arrayvar7[4] = buttonTool4.
        arrayvar7[5] = buttonTool6.
        arrayvar7[6] = buttonTool8.
        arrayvar7[7] = buttonTool11.
        arrayvar7[8] = buttonTool12.
        arrayvar7[9] = stateButtonTool3.
        arrayvar7[10] = buttonTool14.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar7).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_SmartEditorForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 162).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:Name = "m_SmartEditorForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 424).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartEditorForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(755, 162).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:Name = "m_SmartEditorForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 424).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartEditorForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:Name = "m_SmartEditorForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(763, 162).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartEditorForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 586).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartEditorForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(763, 0).
        THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:Office2007.
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        dockableControlPane1:Control = THIS-OBJECT:ultraTreeWithSearchControl1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(47, 186, 184, 356).
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Outline":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar8[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar8).
        dockAreaPane1:Size = NEW System.Drawing.Size(254, 424).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 1 NO-UNDO.
        arrayvar9[1] = dockAreaPane1.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar9).
        THIS-OBJECT:ultraDockManager1:DragIndicatorStyle = Infragistics.Win.UltraWinDock.DragIndicatorStyle:VisualStudio2008Vista.
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_SmartEditorFormUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Name = "m_SmartEditorFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 424).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft:TabIndex = 5.
        /*  */
        /* m_SmartEditorFormUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(755, 162).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Name = "m_SmartEditorFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 424).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight:TabIndex = 6.
        /*  */
        /* m_SmartEditorFormUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Name = "m_SmartEditorFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(747, 0).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop:TabIndex = 7.
        /*  */
        /* m_SmartEditorFormUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(8, 586).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Name = "m_SmartEditorFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(747, 0).
        THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom:TabIndex = 8.
        /*  */
        /* m_SmartEditorFormAutoHideControl */
        /*  */
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:Name = "m_SmartEditorFormAutoHideControl":U.
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:Size = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:m_SmartEditorFormAutoHideControl:TabIndex = 9.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(259, 424).
        THIS-OBJECT:windowDockingArea1:TabIndex = 11.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:ultraTreeWithSearchControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(254, 424).
        THIS-OBJECT:dockableWindow1:TabIndex = 20.
        /*  */
        /* ultraTabbedMdiManager1 */
        /*  */
        THIS-OBJECT:ultraTabbedMdiManager1:AllowNestedTabGroups = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:MdiParent = THIS-OBJECT.
        THIS-OBJECT:ultraTabbedMdiManager1:ViewStyle = Infragistics.Win.UltraWinTabbedMdi.ViewStyle:Office2007.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "cls":U.
        THIS-OBJECT:openFileDialog1:Filter = "ABL Source (*.cls; *.p; *.w; *.i)|*.cls;*.p;*.i;*.w|All files (*.*)|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 586).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "status":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 1 NO-UNDO.
        arrayvar10[1] = ultraStatusPanel1.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar10).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(763, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 17.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Enabled = TRUE.
        THIS-OBJECT:timer1:Interval = 1000.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* timer2 */
        /*  */
        THIS-OBJECT:timer2:Enabled = TRUE.
        THIS-OBJECT:timer2:Interval = 10000.
        THIS-OBJECT:timer2:Tick:Subscribe(THIS-OBJECT:timer2_Tick).
        /*  */
        /* SmartEditorForm */
        /*  */
        THIS-OBJECT:AllowDrop = TRUE.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(763, 609).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartEditorForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "SmartEditorForm":U.
        THIS-OBJECT:Text = "Smart Editor":U.
        THIS-OBJECT:WindowPositionRegistryKey = "SmartEditorForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:windowDockingArea1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Inserts a method into the source code 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InsertMethod ():
		
		DEFINE VARIABLE cMethodName AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cTemplate   AS CHARACTER NO-UNDO .
		DEFINE VARIABLE lcCode      AS LONGCHAR  NO-UNDO .
		
		IF Consultingwerk.Util.InputPromptHelper:PromptForCharacterValue ("Insert Method"{&TRAN},
		                                                                  "Enter the name of the new method to insert:"{&TRAN},
		                                                                  NEW Consultingwerk.Framework.NotEmptyInputValidator(),
		                                                                  INPUT-OUTPUT cMethodName) = TRUE THEN . 
		ELSE 
		    RETURN . 

        cTemplate = "Consultingwerk\Studio\ProcedureEditor\Templates\method.template":U . 

        COPY-LOB FROM FILE cTemplate TO lcCode .

        ASSIGN lcCode = SUBSTITUTE (lcCode, cMethodName) .

        THIS-OBJECT:InsertMethod (cMethodName, STRING (lcCode)) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Inserts a method into the current class
        Notes:   Locates the position by alphabeth
        @param pcMethodName The name of the method to insert
        @param pcCode The complete code of the method to insert (including comments and the METHOD statement)
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InsertMethod (pcMethodName AS CHARACTER,
	                                 pcCode AS CHARACTER):

        DEFINE VARIABLE oEditor    AS ProcedureEditorForm NO-UNDO . 
        DEFINE VARIABLE oNode      AS JPNode              NO-UNDO . 
        DEFINE VARIABLE oClassNode AS JPNode              NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            RETURN . 
            
        oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                        ProcedureEditorForm) .

        FILE-INFO:FILE-NAME = THIS-OBJECT:GetCurrentFileName() .
        IF NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN 
            RETURN .

        THIS-OBJECT:Save () .

        IF THIS-OBJECT:Reparse () = TRUE THEN .
        ELSE RETURN . 
		
        THIS-OBJECT:PopulateMethodTempTable () .

        FIND LAST ttMethods WHERE ttMethods.MethodName < pcMethodName NO-ERROR .

        IF AVAILABLE ttMethods THEN DO:
            oNode = THIS-OBJECT:GetJPNode (ttMethods.NodeNum):lastChild() .
        
            oEditor:InsertText (pcCode, oNode:getLine () + 1) .
        END.
        ELSE DO:
            {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Form>"" 
                                      oPair2 in oEditors}
                
                IF oPair2:Value = THIS-OBJECT:ActiveMdiChild THEN DO:

                    IF NOT VALID-OBJECT (oParseUnits [oPair2:Key]) THEN 
                        UNDO, THROW NEW AppError ("Unable to insert method, as the file could not be parsed."{&TRAN}, 0) .

                    oClassNode = ProparseHelper:FindChildNodeOfNodeType (CAST (oParseUnits [oPair2:Key], ParseUnit):getTopNode(),
                                                                         "CLASS":U) . 
                
                    LEAVE . 
                END.
            END.
            
            IF VALID-OBJECT (oClassNode) THEN DO:
                oNode = oClassNode:lastChild () .            

                oEditor:InsertText (pcCode, oNode:getLine ()) .
            END.
        END.
        
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:Reparse () .
        
            FINALLY:
                THIS-OBJECT:Save () .    	
            END FINALLY.
        END.            
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event 
        Notes:   
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):
		
		DEFINE VARIABLE oChildForm AS System.Windows.Forms.Form NO-UNDO .
		
		SUPER:OnFormClosing (e) .

        IF NOT e:Cancel THEN DO:
    
            THIS-OBJECT:StoreSettings (STRING (UltraToolbarsHelper:GetStateButtonToolChecked (ultraToolbarsManager1, "Parse Automatically":U)), 
                                       "ParseAutomatically":U) . 
            
            DO WHILE THIS-OBJECT:MdiChildren:Length > 0:

                oChildForm = CAST (THIS-OBJECT:MdiChildren:GetValue (0),
                                   System.Windows.Forms.Form) .  
    
                oChildForm:Close () .

                IF VALID-OBJECT (oChildForm) AND oChildForm:Visible THEN DO: 
                    e:Cancel = TRUE .
                    
                    RETURN .
                END.
            END.        
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MdiChildActivate event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnMdiChildActivate (e AS System.EventArgs):
		
		SUPER:OnMdiChildActivate(INPUT e).

        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Form>"" 
                                  oPair in oEditors}
            
            IF oPair:Value = THIS-OBJECT:ActiveMdiChild THEN DO:
            
                IF oParseUnits:ContainsKey (oPair:Key) AND VALID-OBJECT (oParseUnits [oPair:Key]) THEN DO: 
                    THIS-OBJECT:PopulateOutline (CAST (oParseUnits [oPair:Key], ParseUnit)) .
            
                    RETURN .
                END. 
            END.
        END.

        THIS-OBJECT:Nodes:Clear () . 
        
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTreeWithSearchControl1:UltraTree:Nodes) .
        
        FINALLY:
            THIS-OBJECT:EvaluateSaveEnabled () .    		
        END FINALLY.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnShown event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		DEFINE VARIABLE cParseAutomatically AS CHARACTER NO-UNDO.
		
		SUPER:OnShown (e) . 

        PROCESS EVENTS . 
	
	    ASSIGN cParseAutomatically = THIS-OBJECT:LoadSettings ("ParseAutomatically":U) .
		
		IF cParseAutomatically = "no":U THEN DO:
		    UltraToolbarsHelper:SetStateButtonToolChecked (ultraToolbarsManager1, "Parse Automatically":U, FALSE) . 
		    THIS-OBJECT:SetTimersEnabled (TRUE) .
		END.
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the File Open dialog  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OpenFile ():
		
		DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
		
		WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult .
		
		{Consultingwerk/check-dialogresult-ok.i oDialogResult}   

        THIS-OBJECT:OpenFile (openFileDialog1:FileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the given file
        Notes:   
        @param pcFileName The name of the file to open 
        @return The reference to the ProcedureEditorForm
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ProcedureEditorForm OpenFile (pcFileName AS CHARACTER):
		
        DEFINE VARIABLE oEditorForm   AS ProcedureEditorForm NO-UNDO . 
        DEFINE VARIABLE oParseUnit    AS ParseUnit           NO-UNDO . 

        DEFINE VARIABLE pw_Window     AS HANDLE              NO-UNDO.
        DEFINE VARIABLE pw_Editor     AS HANDLE              NO-UNDO.
        DEFINE VARIABLE p_Cursor_Line AS INTEGER             NO-UNDO.
        
        DEFINE VARIABLE lcSource      AS LONGCHAR            NO-UNDO . 
        
        THIS-OBJECT:ultraToolbarsManager1:Tools["save":U]:SharedProps:Enabled = FALSE  .
        
        IF oEditors:ContainsKey (pcFileName) THEN DO:
            
            oEditorForm = CAST (oEditors[pcFileName], 
                                ProcedureEditorForm) NO-ERROR .
                                
            oEditorForm:Activate () .                                   
        END.

        IF NOT VALID-OBJECT (oEditorForm) THEN         
            RUN adecommx/_pwmain.p ("":U, 
                                    pcFileName,
                                    "":U, 
                                    OUTPUT oEditorForm) .   
        
        IF NOT oEditors:ContainsValue (oEditorForm) THEN DO:
            oEditors:Add (pcFileName, oEditorForm) .
            oEditorForm:FormClosed:Subscribe (EditorFormClosedHandler) .
        END.
        
        RUN adecomm/_pwgetwh.p (oEditorForm:EmbeddedWindow, OUTPUT pw_Window).
        RUN adecomm/_pwgeteh.p (INPUT pw_Window , OUTPUT pw_Editor).

        /* Read-Only file? */
        FILE-INFO:FILE-NAME = pcFileName . 
        IF NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN 
            pw_Editor:READ-ONLY = TRUE .  

        /* Disable the standard file-drop of the procedure editor window */
        ASSIGN pw_Window:DROP-TARGET = FALSE . 
        
        APPLY "ENTRY":U TO pw_Editor .   

        DO ON ERROR UNDO, THROW:

            SESSION:SET-WAIT-STATE ("general":U) .
            
            THIS-OBJECT:DisplayStatus ("Initializing Proparse environment."{&TRAN}) . 
            
            Consultingwerk.Studio.Proparse.ProparseHelper:SetProparseEnvironment() . 
    
            THIS-OBJECT:DisplayStatus ("Processing database schema."{&TRAN}) . 
    
            Consultingwerk.Studio.Proparse.ProparseHelper:ExportDatabaseSchema () .
            
            THIS-OBJECT:DisplayStatus ("Initializing Proparse."{&TRAN}) . 
            
            Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .
    
            THIS-OBJECT:DisplayStatus ("Parsing source code."{&TRAN}) . 
            
            COPY-LOB FROM FILE pcFileName TO lcSource . 
            
            oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseString (lcSource) .
    
            IF oParseUnits:ContainsKey (pcFileName) THEN 
                oParseUnits:Remove (pcFileName) .
    
            oParseUnits:Add (pcFileName, oParseUnit) .
    
            THIS-OBJECT:DisplayStatus ("Populating treeview."{&TRAN}) . 
    
            THIS-OBJECT:PopulateOutline (oParseUnit) .
    
            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .                
            END CATCH.
    
            FINALLY:
                THIS-OBJECT:DisplayStatus ("":U) . 
                
                SESSION:SET-WAIT-STATE ("":U) .
            END FINALLY.
        END.

        RETURN oEditorForm .
                
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the ClassNode 
        Notes:   
        @param poClassName The JPNode for the Class Node
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ParseClassMembers (poClassName AS JPNode):
		
        DEFINE VARIABLE oCodeBlock  AS JPNode        NO-UNDO .   
        DEFINE VARIABLE oNode       AS JPNode        NO-UNDO .
        DEFINE VARIABLE oID         AS JPNode        NO-UNDO .    
        DEFINE VARIABLE oTreeNode   AS UltraTreeNode NO-UNDO .
		DEFINE VARIABLE cSignature  AS CHARACTER     NO-UNDO .

        DEFINE VARIABLE oParentNode AS UltraTreeNode NO-UNDO . 
		
		oCodeBlock = ProparseHelper:FindChildNodeOfNodeType (poClassName,
                                                             "Code_block":U) .

        IF NOT VALID-OBJECT (oCodeBlock) THEN 
            RETURN . 

        oNode = oCodeBlock:firstchild () .
        
        DO WHILE VALID-OBJECT (oNode):

            oTreeNode = ? . 

            CASE NodeTypes:getTypeName(oNode:getType()):
                WHEN "CONSTRUCTOR":U THEN DO:
                    oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Constructors":U) .
                    
                    IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Constructors":U, "Constructors":U) .
                        oParentNode:Expanded = TRUE  . 
                    END.
                    
                    oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                  "TYPE_NAME":U) .
                                    
                    ASSIGN cSignature = ProparseHelper:ParseParameterList (oNode) .                                   
                                    
                    oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .      
                    oTreeNode:LeftImages:Add (oMethodImage) .                              
                    
                    IF cSignature > "":U THEN 
                        oTreeNode:Text = SUBSTITUTE ("&1 (&2)":U,
                                                     oID:getText (),
                                                     cSignature) . 
                    ELSE 
                        oTreeNode:Text = oID:getText () .
                END.
                WHEN "DEFINE":U THEN DO:
                    IF ProparseHelper:HasChildNodeOfNodeType (oNode, "PROPERTY":U) THEN DO:
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Properties":U) .
                        
                        IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                            oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Properties":U, "Properties":U) .
                            oParentNode:Expanded = TRUE  . 
                        END.
                        
                        oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                      "ID":U) .
                                        
                        oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .                                    
                        oTreeNode:LeftImages:Add (oPropertyImage) .                              
                        oTreeNode:Text = oID:getText () .
                    END.    
                    ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "EVENT":U) THEN DO:
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Events":U) .
                        
                        IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                            oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Events":U, "Events":U) .
                            oParentNode:Expanded = TRUE  . 
                        END.
                        
                        oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                      "ID":U) .
                                        
                        oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .     
                        oTreeNode:LeftImages:Add (oEventImage) .                              
                        oTreeNode:Text = oID:getText () .
                    END.                        
                END.
                WHEN "METHOD":U THEN DO:
                    oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Methods":U) .
                    
                    IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Methods":U, "Methods":U) .
                        oParentNode:Expanded = TRUE  . 
                    END.
                    
                    oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                  "ID":U) .
                                    
                    ASSIGN cSignature = ProparseHelper:ParseParameterList (oNode) .                                   
                                    
                    oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .  
                    oTreeNode:LeftImages:Add (oMethodImage) .                              
                    
                    IF cSignature > "":U THEN 
                        oTreeNode:Text = SUBSTITUTE ("&1 (&2)":U,
                                                     oID:getText (),
                                                     cSignature) . 
                    ELSE 
                        oTreeNode:Text = oID:getText () .
                END.
            END CASE . 

            IF VALID-OBJECT (oTreeNode) THEN     
                THIS-OBJECT:Nodes:ADD (oNode:getNodeNum (), oNode) .
    
            oNode = oNode:nextSibling () .    
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the ProcedureNode 
        Notes:   
        @param poProcedureNode The JPNode for the Procedure Node
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ParseProcedureMembers (poProcedureNode AS JPNode):
		
        DEFINE VARIABLE oNode       AS JPNode        NO-UNDO .
        DEFINE VARIABLE oID         AS JPNode        NO-UNDO .    
        DEFINE VARIABLE oTreeNode   AS UltraTreeNode NO-UNDO .

        DEFINE VARIABLE oParentNode AS UltraTreeNode NO-UNDO . 
        
        oNode = poProcedureNode:firstchild () .
        
        DO WHILE VALID-OBJECT (oNode):

            oTreeNode = ? . 

            CASE NodeTypes:getTypeName(oNode:getType()):
                WHEN "PROCEDURE":U THEN DO:
                    oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Procedures":U) .
                    
                    IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Procedures":U, "Procedures":U) .
                        oParentNode:Expanded = TRUE  . 
                    END.
                    
                    oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                  "ID":U) .
                                    
                    oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .      
                    oTreeNode:LeftImages:Add (oMethodImage) .                              
                    oTreeNode:Text = oID:getText () .
                END.
                WHEN "FUNCTION":U THEN DO:
                    oParentNode = ultraTreeWithSearchControl1:UltraTree:GetNodeByKey ("Functions":U) .
                    
                    IF NOT VALID-OBJECT (oParentNode) THEN DO: 
                        oParentNode = ultraTreeWithSearchControl1:UltraTree:Nodes:Add ("Functions":U, "Functions":U) .
                        oParentNode:Expanded = TRUE  . 
                    END.
                    
                    oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                                  "ID":U) .
                                    
                    oTreeNode = oParentNode:Nodes:Add (STRING (oNode:getNodeNum ())) .  
                    oTreeNode:LeftImages:Add (oPropertyImage) .          
                    
                    IF ProparseHelper:HasChildNodeOfNodeType (oNode, "FORWARDS":U) THEN 
                        oTreeNode:Text = SUBSTITUTE ("&1 FORWARD":U, oID:getText()) .                                            
                    ELSE 
                        oTreeNode:Text = oID:getText () .
                END.
            END CASE . 

            IF VALID-OBJECT (oTreeNode) THEN     
                THIS-OBJECT:Nodes:ADD (oNode:getNodeNum (), oNode) .
    
            oNode = oNode:nextSibling () .    
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the current editor contents 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ParseSourceCode ():
		
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        THIS-OBJECT:SetTimersEnabled (FALSE) .
        
        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            RETURN . 

        ASSIGN cFileName = THIS-OBJECT:GetCurrentFileName () .
        
        FILE-INFO:FILE-NAME = cFileName .
        IF NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN RETURN .
        
        THIS-OBJECT:DisplayStatus ("Parsing source code..."{&TRAN}) .
        
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:Reparse () .

            IF oParseUnits:ContainsKey (cFileName) THEN 
                THIS-OBJECT:PopulateOutline (CAST (oParseUnits [cFileName], ParseUnit)) .         

            FINALLY:
                THIS-OBJECT:Save () .       
            END FINALLY.
        END.
        
        THIS-OBJECT:DisplayStatus ("") .

        CATCH err AS Progress.Lang.Error:
            THIS-OBJECT:DisplayStatus (err:GetMessage (1)) .        
        END CATCH.

        FINALLY:
            THIS-OBJECT:SetTimersEnabled (TRUE) .       
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the temp-table with the current class methods 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateMethodTempTable ():
		
        DEFINE VARIABLE oID        AS JPNode              NO-UNDO .
		
        EMPTY TEMP-TABLE ttMethods .
        
        /* Locate the node to insert the method before */
        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.Int32,org.prorefactor.core.JPNode>"" 
                                  oPair in THIS-OBJECT:Nodes}

            IF NodeTypes:getTypeName(oPair:Value:getType()) = "METHOD":U THEN DO:
                CREATE ttMethods . 
                
                oID = ProparseHelper:FindChildNodeOfNodeType (oPair:Value,
                                                              "ID":U) .
                
                ASSIGN ttMethods.NodeNum    = oPair:Key 
                       ttMethods.MethodName = oID:getText () .
            END.
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Outline View
        Notes:   
        @param poParseUnit The ParseUnit of the File to parse
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateOutline (poParseUnit AS ParseUnit):
		
		DEFINE VARIABLE oClassNode AS JPNode NO-UNDO .
		
		THIS-OBJECT:Nodes:Clear () . 
		
		GarbageCollectorHelper:ClearUltraTreeNodes (ultraTreeWithSearchControl1:UltraTree:Nodes) .
		
		ultraTreeWithSearchControl1:UltraTree:Override:Sort = Infragistics.Win.UltraWinTree.SortType:Ascending .
		ultraTreeWithSearchControl1:UltraTree:AutoScrollHorizontal = Infragistics.Win.DefaultableBoolean:False .
		    
		oClassNode = ProparseHelper:FindChildNodeOfNodeType (poParseUnit:getTopNode(),
		                                                     "CLASS":U) . 

        IF VALID-OBJECT (oClassNode) THEN 
            THIS-OBJECT:ParseClassMembers (oClassNode) . 
		ELSE 
            THIS-OBJECT:ParseProcedureMembers (poParseUnit:getTopNode()) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Reparses the current editor contents 
        Notes:   
        @return Logical value indicating if the file could be parsed
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED LOGICAL Reparse ():
		
		DEFINE VARIABLE cTempFile  AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFileName  AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oParseUnit AS ParseUnit           NO-UNDO . 
        DEFINE VARIABLE oEditor    AS ProcedureEditorForm NO-UNDO . 
        DEFINE VARIABLE lcSource   AS LONGCHAR            NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            RETURN FALSE . 
            
        oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                        ProcedureEditorForm) .

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (oEditor:EmbeddedWindow:Hwnd) .

        THIS-OBJECT:Nodes:Clear () .

        ASSIGN cFileName = GetCurrentFileName () . 

        IF oParseUnits:ContainsKey (cFileName) THEN 
            oParseUnits:Remove (cFileName) .

        ASSIGN cTempFile = oEditor:SaveToTemporaryFile () .

        IF cFileName > "":U THEN .
        ELSE   
            RETURN FALSE . 

        SESSION:SET-WAIT-STATE ("general":U) .

        THIS-OBJECT:DisplayStatus ("Initializing Proparse environment."{&TRAN}) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:SetProparseEnvironment() . 

        THIS-OBJECT:DisplayStatus ("Processing database schema."{&TRAN}) . 

        Consultingwerk.Studio.Proparse.ProparseHelper:ExportDatabaseSchema () .
        
        THIS-OBJECT:DisplayStatus ("Initializing Proparse."{&TRAN}) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .

        THIS-OBJECT:DisplayStatus ("Parsing source code."{&TRAN}) . 
        
        COPY-LOB FROM FILE cTempFile TO lcSource . 
        
        oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseString (lcSource) .

        ASSIGN cFileName = GetCurrentFileName () . 
        
        IF oParseUnits:ContainsKey (cFileName) THEN 
            oParseUnits:Remove (cFileName) .
        oParseUnits:Add (cFileName, oParseUnit) .

        THIS-OBJECT:DisplayStatus ("Populating treeview."{&TRAN}) . 
    
        THIS-OBJECT:PopulateOutline (oParseUnit) .
        
        RETURN TRUE . 
        
        FINALLY:
            THIS-OBJECT:DisplayStatus ("":U) . 
            
            SESSION:SET-WAIT-STATE ("":U) .
            
            THIS-OBJECT:DeleteTemporaryFile (cTempFile) .
            
            Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
            
        END FINALLY.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the current editors 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID Save ():
		
        DEFINE VARIABLE oEditor AS ProcedureEditorForm NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN 
            RETURN . 
            
        oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                        ProcedureEditorForm) .

        oEditor:Save () .
        
        THIS-OBJECT:EvaluateSaveEnabled () .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves all open editors 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveAll ():
        
        DEFINE VARIABLE oEditor AS ProcedureEditorForm NO-UNDO . 

        {Consultingwerk/foreach.i System.Windows.Forms.Form oForm in this-object:MdiChildren}

            oEditor = CAST (oForm,
                            ProcedureEditorForm) .

            oEditor:Save () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DragDrop event
        Notes:   
        @param e The DragEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnDragDrop (e AS System.Windows.Forms.DragEventArgs):
        
        DEFINE VARIABLE cFiles AS CHARACTER NO-UNDO EXTENT .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .

        
        SUPER:OnDragDrop (e) .

        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            
            cFiles = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data) .
            
            DO i = 1 TO EXTENT (cFiles):
                THIS-OBJECT:OpenFile (cFiles[i]) .               
            END.
        END. 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DragEnter event
        Notes:   
        @param e The DragEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnDragEnter (e AS System.Windows.Forms.DragEventArgs):
		
		SUPER:OnDragEnter (e) .

        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN 
            e:Effect = System.Windows.Forms.DragDropEffects:Copy .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enables or Disables the Timer Components 
        Notes:   
        @param plEnabled Logical value indicating if the timers should be enabled or not
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetTimersEnabled (plEnabled AS LOGICAL):
		
		ASSIGN timer1:Enabled = plEnabled
		       timer2:Enabled = plEnabled AND UltraToolbarsHelper:GetStateButtonToolChecked (ultraToolbarsManager1, "Parse Automatically":U).   

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer component
        Notes:   Evaluates the Save button state
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID timer1_Tick (sender AS System.Object, 
	                                 e AS System.EventArgs):
		
		THIS-OBJECT:EvaluateSaveEnabled () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer component
        Notes:   Reparses the current file for the outline view
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID timer2_Tick (sender AS System.Object,
	                                 e AS System.EventArgs):
		
        THIS-OBJECT:ParseSourceCode () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the treeNodeSelected event of the ultraTreeWithSearchControl1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The TreeNodeSelectedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID TreeNodeSelectedHandler (sender AS System.Object,
	                                               e AS TreeNodeSelectedEventArgs):
		
        DEFINE VARIABLE iKey          AS INTEGER             NO-UNDO.
        DEFINE VARIABLE oNode         AS JPNode              NO-UNDO . 
        DEFINE VARIABLE cFileName     AS CHARACTER           NO-UNDO.
        DEFINE VARIABLE oEditorForm   AS ProcedureEditorForm NO-UNDO . 

        DEFINE VARIABLE pw_Window     AS HANDLE              NO-UNDO.
        DEFINE VARIABLE pw_Editor     AS HANDLE              NO-UNDO.
        DEFINE VARIABLE p_Cursor_Line AS INTEGER             NO-UNDO.
        		
		iKey = INTEGER (e:ItemKey) NO-ERROR  . 	
		
		IF iKey > 0 THEN DO: 
            oNode = THIS-OBJECT:GetJPNode (iKey) .
            
            ASSIGN oEditorForm = CAST (THIS-OBJECT:ActiveMdiChild, ProcedureEditorForm) .
                        
            RUN adecomm/_pwgetwh.p (oEditorForm:EmbeddedWindow, OUTPUT pw_Window).
            RUN adecomm/_pwgeteh.p (INPUT pw_Window , OUTPUT pw_Editor).
                            
            ASSIGN p_Cursor_Line = oNode:getLine() . 
    
            IF VALID-HANDLE (pw_Editor) THEN DO: 
                IF pw_Editor:EMPTY THEN RETURN.
                /* If entered line is greater than number of lines in buffer, then
                   move to EOF. */
                IF p_Cursor_Line > pw_Editor:NUM-LINES
                    THEN pw_Editor:MOVE-TO-EOF().
                ELSE DO:
                /* If entered line 0, goto line 1. */
                    IF ( p_Cursor_Line < 1 ) THEN ASSIGN p_Cursor_Line = 1.
            
                    ASSIGN pw_Editor:CURSOR-LINE = p_Cursor_Line.
                    
                    IF oNode:getColumn () > 0 THEN 
                        pw_Editor:CURSOR-CHAR = oNode:getColumn () . 
                END.          
                
                APPLY "ENTRY":U TO pw_Editor .   
            END.
		END .
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:   
        @param sender The reference to the event that raised this event
        @param e The ToolClickEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
	                                                     e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
		
		CASE e:Tool:Key:
		    
		    WHEN "open":U THEN 
		        THIS-OBJECT:OpenFile () .
		    
		    WHEN "save":U THEN 
		        THIS-OBJECT:Save () .
		        
		    WHEN "save all":U THEN 
		        THIS-OBJECT:SaveAll () .
		        		    
		    WHEN "check syntax":U THEN 
		        THIS-OBJECT:CheckSyntax () .
		     
		    WHEN "search outline":U THEN 
		        THIS-OBJECT:ultraTreeWithSearchControl1:FocusSearchField() .
		     
		    WHEN "Insert Method":U THEN 
		        THIS-OBJECT:InsertMethod () .
		     
		    WHEN "Parse Automatically":U THEN 
		        THIS-OBJECT:SetTimersEnabled (TRUE) .

            WHEN "Parse":U THEN 
                THIS-OBJECT:ParseSourceCode () . 
		     
		END CASE . 

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartEditorForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartEditorForm ():

    END DESTRUCTOR .
    
END CLASS.
