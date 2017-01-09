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
    File        : ToolbarDesignerForm
    Purpose     : Provides a user interface to customize an Infragistics 
                  Toolbar or Ribbon
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 08 21:20:50 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Design.WinKit.Toolbar.* .
USING Progress.Lang.*.
USING Progress.Windows.Form.
USING System.* .
USING System.Collections.* .
USING System.Collections.Generic.* .
USING System.ComponentModel.* .
USING System.ComponentModel.Design.* .
USING System.Data.* .
USING System.Drawing.* .
USING System.Drawing.Design.* .
USING System.Text.* .
USING System.Windows.Forms.* .

{Consultingwerk/products.i}

CLASS Consultingwerk.Design.WinKit.Toolbar.ToolbarDesignerForm INHERITS Form: 
	
    DEFINE PRIVATE VARIABLE m_ToolbarDesignerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ToolbarDesignerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ToolbarDesignerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE propertyGrid1 AS System.Windows.Forms.PropertyGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ToolbarDesignerForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

    DEFINE VARIABLE oDesignSurface             AS System.ComponentModel.Design.DesignSurface             NO-UNDO . 
    DEFINE VARIABLE oDesignView                AS System.Windows.Forms.Control                           NO-UNDO .
    DEFINE VARIABLE oDesignerHost              AS System.ComponentModel.Design.IDesignerHost             NO-UNDO .
    DEFINE VARIABLE oSelectionService          AS System.ComponentModel.Design.ISelectionService         NO-UNDO .
    DEFINE VARIABLE oTemplateForm              AS Progress.Windows.Form                                  NO-UNDO .
    DEFINE VARIABLE oToolbar                   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO . 
    DEFINE VARIABLE oToolbarDesigner           AS System.ComponentModel.Design.ComponentDesigner         NO-UNDO .  
    DEFINE VARIABLE oCustomizeVerb             AS System.ComponentModel.Design.DesignerVerb              NO-UNDO .     
    DEFINE VARIABLE oCustomPropertiesPagesVerb AS System.ComponentModel.Design.DesignerVerb              NO-UNDO .    
    DEFINE VARIABLE oCurrentLicenseContext     AS System.ComponentModel.LicenseContext                   NO-UNDO .	
		
    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user presses Save                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC EVENT AfterSaveLayout DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Cancelable event that raised when the user presses Save                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeSaveLayout DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Retruns the reference to the UltraToolbarsManager instance used 
                 on the design canvas.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DesignToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the reference to the UltraToolbarsManager that was
                 used as the starting point (the toolbar used in the actual application)                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LoadFromToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the procedure handle used to build the default file 
                 name for saving the toolbar design                                                                      
        Notes:   Can be unknown at runtime                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ProcedureHandle AS HANDLE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolbarDesignerForm class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ToolbarDesignerForm ():
		
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
		Purpose: Builds the name for the toolbar XML file
		Notes:  																	  
        @return The proposed file name for the toolbar definition
	------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER BuildToolbarFileName ():
		
		DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
		
		IF VALID-HANDLE (THIS-OBJECT:ProcedureHandle) THEN DO:
		    
		    FILE-INFO:FILE-NAME = THIS-OBJECT:ProcedureHandle:FileName .
		    
		    cFileName = FILE-INFO:FULL-PATHNAME .  
	   
	        cFileName = SUBSTRING (cFileName, 1, R-INDEX (cFileName, ".":U) - 1) + "_toolbar.xml":U .
		END. 
		ELSE 
		    cFileName = "_toolbar.xml":U .  
		  
		RETURN cFileName .   
		  
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Closes the Form  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CloseForm ():
		
		DEFINE VARIABLE lOk AS LOGICAL NO-UNDO INIT TRUE .
		
		MESSAGE "Do you want to save the current toolbar design?"{&TRAN}
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
            TITLE THIS-OBJECT:Text
            UPDATE lOk .
            
        IF lOk THEN SaveLayout () .
        
        IF lOk = ? THEN RETURN . 
        
        THIS-OBJECT:Close() .    

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("DesignerToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Layout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Load Layout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Customize Tools":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Properties Pages":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Ribbon":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Group":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Layout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Customize Tools":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Properties Pages":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Ribbon":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Group":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Load Layout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Design.WinKit.Toolbar.ToolbarDesignerForm":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
        THIS-OBJECT:propertyGrid1 = NEW System.Windows.Forms.PropertyGrid().
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
        THIS-OBJECT:splitContainer1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:WindowsVista.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool7:InstanceProps:IsFirstInGroup = TRUE.
        stateButtonTool1:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 8 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool11.
        arrayvar0[3] = buttonTool2.
        arrayvar0[4] = buttonTool3.
        arrayvar0[5] = buttonTool7.
        arrayvar0[6] = buttonTool8.
        arrayvar0[7] = stateButtonTool1.
        arrayvar0[8] = buttonTool13.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Settings:FillEntireRow = Infragistics.Win.DefaultableBoolean:True.
        ultraToolbar1:Settings:ToolDisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        ultraToolbar1:Settings:UseLargeImages = Infragistics.Win.DefaultableBoolean:True.
        ultraToolbar1:Text = "DesignerToolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance11.
        buttonTool4:SharedPropsInternal:Caption = "Save Layout":U.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        buttonTool5:SharedPropsInternal:Caption = "Close":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
        buttonTool6:SharedPropsInternal:Caption = "Cancel":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance14.
        buttonTool9:SharedPropsInternal:Caption = "Customize":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
        buttonTool10:SharedPropsInternal:Caption = "Custom Properties":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        stateButtonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance16.
        stateButtonTool2:SharedPropsInternal:Caption = "Ribbon":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesLarge:Appearance = appearance18.
        buttonTool14:SharedPropsInternal:Caption = "New Group":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesLarge:Appearance = appearance17.
        buttonTool12:SharedPropsInternal:Caption = "Load Layout":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 8 NO-UNDO.
        arrayvar2[1] = buttonTool4.
        arrayvar2[2] = buttonTool5.
        arrayvar2[3] = buttonTool6.
        arrayvar2[4] = buttonTool9.
        arrayvar2[5] = buttonTool10.
        arrayvar2[6] = stateButtonTool2.
        arrayvar2[7] = buttonTool14.
        arrayvar2[8] = buttonTool12.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ToolbarDesignerForm_Fill_Panel */
        /*  */
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 46).
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Name = "ToolbarDesignerForm_Fill_Panel":U.
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:Size = NEW System.Drawing.Size(911, 300).
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* splitContainer1 */
        /*  */
        THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
        THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:splitContainer1:Name = "splitContainer1":U.
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:propertyGrid1).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(911, 300).
        THIS-OBJECT:splitContainer1:SplitterDistance = 628.
        THIS-OBJECT:splitContainer1:TabIndex = 0.
        /*  */
        /* propertyGrid1 */
        /*  */
        THIS-OBJECT:propertyGrid1:CausesValidation = FALSE.
        THIS-OBJECT:propertyGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:propertyGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:propertyGrid1:Name = "propertyGrid1":U.
        THIS-OBJECT:propertyGrid1:Size = NEW System.Drawing.Size(279, 300).
        THIS-OBJECT:propertyGrid1:TabIndex = 0.
        THIS-OBJECT:propertyGrid1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* m_ToolbarDesignerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 46).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:Name = "m_ToolbarDesignerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 300).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ToolbarDesignerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(911, 46).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:Name = "m_ToolbarDesignerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 300).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ToolbarDesignerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:Name = "m_ToolbarDesignerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(911, 46).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 346).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:Name = "m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(911, 0).
        THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "xml":U.
        THIS-OBJECT:saveFileDialog1:Filter = "Toolbar Designer Files (*_toolbar.xml)|*_toolbar.xml|XML Files (*.xml)|*.xml|All Files (*.*)|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save Toolbar Design As":U.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "xml":U.
        THIS-OBJECT:openFileDialog1:FileName = "":U.
        THIS-OBJECT:openFileDialog1:Filter = "Toolbar Designer Files (*_toolbar.xml)|*_toolbar.xml|XML Files (*.xml)|*.xml|All Files (*.*)|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* ToolbarDesignerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(911, 346).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ToolbarDesignerForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ToolbarDesignerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ToolbarDesignerForm":U.
        THIS-OBJECT:Text = "Consultingwerk WinKit Toolbar & Ribbon Designer":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ToolbarDesignerForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Initializes the Design Environment 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitializeDesignEnvironment(  ):
		

        oCurrentLicenseContext = System.ComponentModel.LicenseManager:CurrentContext .
        		
        System.ComponentModel.LicenseManager:CurrentContext = NEW Consultingwerk.Design.LicenseContext () . 
		
		oDesignSurface = NEW System.ComponentModel.Design.DesignSurface (Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U)).
		
        /* Get the service container */
        DEFINE VARIABLE oServiceContainer AS System.ComponentModel.Design.IServiceContainer NO-UNDO .
        
        oServiceContainer = CAST (oDesignSurface:GetService(Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IServiceContainer":U)), 
                                  System.ComponentModel.Design.IServiceContainer).

        oDesignerHost = CAST (oDesignSurface:GetService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.IDesignerHost":U)),
                             System.ComponentModel.Design.IDesignerHost) .
        
        oServiceContainer:AddService(Progress.Util.TypeHelper:GetType ("System.Windows.Forms.PropertyGrid":U), 
                                     THIS-OBJECT:propertyGrid1) .

        /* Mike Fechner, Consultingwerk Ltd. 28.09.2012
           Use Consultingwerk's selection service in 10.2A/10.2B */		
		IF PROVERSION BEGINS "10.2":U THEN DO:
            oSelectionService = NEW Consultingwerk.Design.SelectionService (oDesignerHost).  
            oSelectionService:SelectionChanged:Subscribe (SelectionChangedHandler) .
                         
            oServiceContainer:RemoveService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.ISelectionService":U)).
                
            oServiceContainer:AddService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.ISelectionService":U),
                                         oSelectionService).
        END.
        ELSE DO:
           /* Mike Fechner, Consultingwerk Ltd. 25.02.2012
               In OpenEdge 11 the UltraWinToolbar Designer seems to have issues with our own ABL based Selection Service
               So we are going to keep using the .NET Default */
            oSelectionService = CAST (oServiceContainer:GetService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.ISelectionService":U)),
                                      System.ComponentModel.Design.ISelectionService) .             
            oSelectionService:SelectionChanged:Subscribe (SelectionChangedHandler) .
        END.
		
		oServiceContainer:AddService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.IDesignerEventService":U),
		                             NEW Consultingwerk.Design.DesignerEventService(oDesignerHost)) .
		
        oTemplateForm = CAST (oDesignerHost:RootComponent, Progress.Windows.Form) .
        /* Set the default properties of our base template form */
        oTemplateForm:Text = "Toolbar Design Form"{&TRAN}.
		oTemplateForm:Dock = System.Windows.Forms.DockStyle:Fill . 
		oTemplateForm:IsMdiContainer = TRUE . 
        
        oDesignView = CAST (oDesignSurface:View, Control).
        oDesignView:Dock = DockStyle:Fill.
        splitContainer1:Panel1:Controls:Add(oDesignView).		
		

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Begins designing the Toolbar  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InitializeToolbarDesign ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
		
		oToolbar = CAST (oDesignerHost:CreateComponent(Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinToolbars.UltraToolbarsManager":U), 
		                                               "ultraToolbarsManager1":U),
                         Infragistics.Win.UltraWinToolbars.UltraToolbarsManager)  .

        THIS-OBJECT:DesignToolbar = oToolbar . 

        oToolbar:DockWithinContainer = oTemplateForm . 

        oToolbarDesigner = CAST (oDesignerHost:GetDesigner (oToolbar),
                                 System.ComponentModel.Design.ComponentDesigner) .

        DO i = 0 TO oToolbarDesigner:Verbs:Count - 1:
            
            IF oToolbarDesigner:Verbs[i]:Text BEGINS "Customize":U THEN 
                oCustomizeVerb = oToolbarDesigner:Verbs[i] .
            ELSE IF oToolbarDesigner:Verbs[i]:Text BEGINS "Custom Pro":U THEN 
                oCustomPropertiesPagesVerb = oToolbarDesigner:Verbs[i] . 
        END.
    
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Invokes the Infragistics "Customize" Dialog by invoking the 
		         Designer verb 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InvokeCustomize ():
		
		oCustomizeVerb:Invoke () .

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Invokes the Infragistics "Custom Properties" Dialog by invoking the 
                 Designer verb  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InvokeCustomProperties ():
		
		oCustomPropertiesPagesVerb:Invoke () .

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Prompts for a file name and loads a Toolbar Dialog  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LoadLayout ():
		
		DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO . 
		
		WAIT-FOR openFileDialog1:ShowDialog() SET oDialogResult . 

        IF Progress.Util.EnumHelper:AreNotEqual (oDialogResult, DialogResult:Ok) THEN 
            RETURN . 
            
        oToolbar:LoadFromXml(openFileDialog1:FileName) .
        
        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Loads a Toolbar from the EmbeddedWindowForm 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LoadToolbarDesign ():
		
		DEFINE VARIABLE oStream AS System.IO.MemoryStream NO-UNDO . 
		
		oStream = NEW System.IO.MemoryStream () .
		
		THIS-OBJECT:LoadFromToolbar:SaveAsXml(oStream, FALSE) .
		
		oStream:Seek(0, System.IO.SeekOrigin:Begin) .
		
		oToolbar:LoadFromXml(oStream) .
		oStream:Close().
		
		DELETE OBJECT oStream .

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Creates a new RibbonGroup 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID NewRibbonGroup ():
		
		DEFINE VARIABLE oForm AS Consultingwerk.Design.WinKit.Toolbar.NewRibbonGroupForm NO-UNDO . 
		DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
		DEFINE VARIABLE oGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO . 
		
		DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
		
		IF NOT VALID-OBJECT (oToolbar:Ribbon:SelectedTab) THEN
		    RETURN .
		
		oForm = NEW Consultingwerk.Design.WinKit.Toolbar.NewRibbonGroupForm () .
		
		oForm:ParentTab = oToolbar:Ribbon:SelectedTab .
		
		WAIT-FOR oForm:ShowDialog () SET oDialogResult . 		
		
		IF Progress.Util.EnumHelper:AreEqual (oDialogResult,
		                                      System.Windows.Forms.DialogResult:OK) THEN DO:
	
	        oForm:GetValues (OUTPUT cKey, OUTPUT cText) .
	      
	        oGroup = oToolbar:Ribbon:SelectedTab:Groups:Add (cKey) .   
	      
            IF cText > "":U THEN 
	           oGroup:Caption = cText .   
        END. 
        
        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Raises the AfterSaveLayout event  																	  
		Notes:  																	  
		@param e The System.EventArgs with the data for this event																  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OnAfterSaveLayout (e AS System.EventArgs):
		
		IF NOT VALID-OBJECT (e) THEN 
		    e = System.EventArgs:Empty .  
		
		THIS-OBJECT:AfterSaveLayout:Publish (THIS-OBJECT, e) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterSaveLayout event                                                                     
        Notes:                                                                        
        @param e The CAncelEventArgs with the data for this event                                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeSaveLayout (e AS System.ComponentModel.CancelEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeSaveLayout must be raised with a valid CancelEventArgs instance."{&TRAN}, 0) .  
        
        THIS-OBJECT:BeforeSaveLayout:Publish (THIS-OBJECT, e) .

    END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                        
		Notes:  																	  
        @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
		
		SUPER:OnLoad(INPUT e).

        InitializeDesignEnvironment () .
        InitializeToolbarDesign () .

        IF VALID-OBJECT (THIS-OBJECT:LoadFromToolbar) THEN 
            LoadToolbarDesign () .

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Prompts for a file name and saves the Layout  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SaveLayout ():
		
        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO . 
        DEFINE VARIABLE e  AS System.ComponentModel.CancelEventArgs NO-UNDO . 
        
        e = NEW System.ComponentModel.CancelEventArgs () .
        
        OnBeforeSaveLayout (e) .
        
        IF VALID-OBJECT (e) AND e:Cancel THEN 
            RETURN . 
        
        saveFileDialog1:FileName = BuildToolbarFileName () .
        
        WAIT-FOR saveFileDialog1:ShowDialog() SET oDialogResult . 

        IF Progress.Util.EnumHelper:AreNotEqual (oDialogResult, DialogResult:Ok) THEN 
            RETURN . 
            
        oToolbar:SaveAsXml(saveFileDialog1:FileName, FALSE) .

        OnAfterSaveLayout (System.EventArgs:Empty) .
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectionChanged event                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectionChangedHandler (sender AS System.Object,
                                                   e AS System.EventArgs):
        
            DEFINE VARIABLE selectedComponents AS ICollection NO-UNDO .
            DEFINE VARIABLE oObjects AS System.Object EXTENT .
            DEFINE VARIABLE i AS INTEGER NO-UNDO.
            DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator NO-UNDO .

            IF propertyGrid1:SelectedObjects:Length > 0 AND 
               propertyGrid1:SelectedObjects:GetValue(0) = oToolbar THEN 
                RETURN . 

            IF VALID-OBJECT (oSelectionService) THEN DO:
                selectedComponents = oSelectionService:GetSelectedComponents()  .

                IF selectedComponents:Count > 0 THEN DO:

                    EXTENT (oObjects)= selectedComponents:Count .

                    oEnumerator = selectedComponents:GetEnumerator () .
                    oEnumerator:Reset() .
                    
                    DO i = 1 TO EXTENT (oObjects):
                        oEnumerator:MoveNext() .
                        
                        oObjects[i] = oEnumerator:Current .
                    END.

                    propertyGrid1:SelectedObjects = oObjects.
                END.
                ELSE
                    propertyGrid1:SelectedObjects = ? .
            END.

    END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Launches the ToolbarDesignerForm 																	  
		Notes:  																	  
		@param poToolbarsManager The source UltraToolbarsManager instance (from the IEmbeddedWindowForm)
		@param phProcedure The handle of the ABL source procedure									  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID ShowDesigner (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
	                                        phProcedure AS HANDLE):
		
        DEFINE VARIABLE oInstance AS ToolbarDesignerForm NO-UNDO . 
        
        oInstance = NEW Consultingwerk.Design.WinKit.Toolbar.ToolbarDesignerForm () . 
        oInstance:ProcedureHandle = phProcedure . 
        oInstance:LoadFromToolbar = poToolbarsManager .
		WAIT-FOR oInstance:ShowDialog() .
		
		oInstance:Dispose () NO-ERROR .
		
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Toggles the visibility of the Ribbon  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ToggleRibbon ():
		
		DEFINE VARIABLE oStateButton AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO . 
		
		oStateButton = CAST (ultraToolbarsManager1:Tools["Ribbon":U], 
		                     Infragistics.Win.UltraWinToolbars.StateButtonTool) .
		                     
        oToolbar:Ribbon:Visible = oStateButton:CHECKED .		                     

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
			Notes:  																	  
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                        
	------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
		
		CASE e:Tool:Key:
		    
		    WHEN "Ribbon":U THEN 
		        ToggleRibbon () .
		    WHEN "New Group":U THEN
		        NewRibbonGroup () .
		    WHEN "Custom Properties Pages":U THEN 
		        InvokeCustomProperties() .
            WHEN "Customize Tools":U THEN 
                InvokeCustomize() . 
            WHEN "Load Layout":U THEN 
                LoadLayout () .
            WHEN "Save Layout":U THEN 
                SaveLayout () .
            WHEN "Cancel":U THEN 
                THIS-OBJECT:Close() .
            WHEN "Close":U THEN 
                CloseForm () . 
		END CASE.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the ToolbarDesignerForm class
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC ToolbarDesignerForm ( ):

        System.ComponentModel.LicenseManager:CurrentContext = oCurrentLicenseContext .

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
