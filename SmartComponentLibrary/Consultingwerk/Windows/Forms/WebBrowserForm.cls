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
    File        : WebBrowserForm
    Purpose     : Container for the .NET WebBrowser Control
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 02 00:12:33 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Windows.Form.


USING Consultingwerk.Windows.Forms.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.Forms.WebBrowserForm INHERITS Form: 

    DEFINE PRIVATE VARIABLE m_WebBrowserForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebBrowserForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebBrowserForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebBrowserForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE webBrowser1 AS System.Windows.Forms.WebBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE WebBrowserForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WebBrowserForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC WebBrowserForm ():
		
        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

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
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Back":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Forward":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Stop":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Web Browser":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Back":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Forward":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Stop":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Back":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Forward":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Stop":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Forms.WebBrowserForm":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:WebBrowserForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:webBrowser1 = NEW System.Windows.Forms.WebBrowser().
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:WebBrowserForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U).
        ribbonTab1:Caption = resources:GetString("ribbonTab1.Caption":U).
        ribbonGroup1:Caption = resources:GetString("ribbonGroup1.Caption":U).
        ribbonGroup1:KeyTip = resources:GetString("ribbonGroup1.KeyTip":U).
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool7:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool8:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool7.
        arrayvar0[3] = buttonTool8.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar1[1] = ribbonGroup1.
        ribbonTab1:Groups:AddRange(arrayvar1).
        ribbonTab1:KeyTip = resources:GetString("ribbonTab1.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar2[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:KeyTip = resources:GetString("ultraToolbar1.KeyTip":U).
        buttonTool11:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar3[1] = buttonTool9.
        arrayvar3[2] = buttonTool10.
        arrayvar3[3] = buttonTool11.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar3).
        ultraToolbar1:Text = resources:GetString("ultraToolbar1.Text":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar4[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar4).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool3:SharedPropsInternal:Caption = resources:GetString("resource.Caption":U).
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool4:SharedPropsInternal:Caption = resources:GetString("resource.Caption1":U).
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        appearance6:TextHAlignAsString = resources:GetString("appearance6.TextHAlignAsString":U).
        appearance6:TextVAlignAsString = resources:GetString("appearance6.TextVAlignAsString":U).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool6:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar5[1] = buttonTool3.
        arrayvar5[2] = buttonTool4.
        arrayvar5[3] = buttonTool6.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* WebBrowserForm_Fill_Panel */
        /*  */
        THIS-OBJECT:WebBrowserForm_Fill_Panel:AccessibleDescription = resources:GetString("WebBrowserForm_Fill_Panel.AccessibleDescription":U).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:AccessibleName = resources:GetString("WebBrowserForm_Fill_Panel.AccessibleName":U).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Anchor = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:AutoSize = System.Convert:ToBoolean(resources:GetObject("WebBrowserForm_Fill_Panel.AutoSize":U)).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:BackgroundImageLayout = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* WebBrowserForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:AccessibleDescription = resources:GetString("WebBrowserForm_Fill_Panel.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:AccessibleName = resources:GetString("WebBrowserForm_Fill_Panel.ClientArea.AccessibleName":U).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:webBrowser1).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:Font = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Dock = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Font = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.Font":U), System.Drawing.Font).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ImeMode = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Location = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.Location":U), System.Drawing.Point).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:MaximumSize = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Name = "WebBrowserForm_Fill_Panel":U.
        THIS-OBJECT:WebBrowserForm_Fill_Panel:RightToLeft = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:Size = CAST(resources:GetObject("WebBrowserForm_Fill_Panel.Size":U), System.Drawing.Size).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:TabIndex = System.Convert:ToInt32(resources:GetObject("WebBrowserForm_Fill_Panel.TabIndex":U)).
        /*  */
        /* webBrowser1 */
        /*  */
        THIS-OBJECT:webBrowser1:AccessibleDescription = resources:GetString("webBrowser1.AccessibleDescription":U).
        THIS-OBJECT:webBrowser1:AccessibleName = resources:GetString("webBrowser1.AccessibleName":U).
        THIS-OBJECT:webBrowser1:AllowWebBrowserDrop = FALSE.
        THIS-OBJECT:webBrowser1:Anchor = CAST(resources:GetObject("webBrowser1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:webBrowser1:Dock = CAST(resources:GetObject("webBrowser1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:webBrowser1:Location = CAST(resources:GetObject("webBrowser1.Location":U), System.Drawing.Point).
        THIS-OBJECT:webBrowser1:MaximumSize = CAST(resources:GetObject("webBrowser1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:webBrowser1:MinimumSize = CAST(resources:GetObject("webBrowser1.MinimumSize":U), System.Drawing.Size).
        THIS-OBJECT:webBrowser1:Name = "webBrowser1":U.
        THIS-OBJECT:webBrowser1:Size = CAST(resources:GetObject("webBrowser1.Size":U), System.Drawing.Size).
        THIS-OBJECT:webBrowser1:TabIndex = System.Convert:ToInt32(resources:GetObject("webBrowser1.TabIndex":U)).
        THIS-OBJECT:webBrowser1:DocumentCompleted:Subscribe(THIS-OBJECT:webBrowser1_DocumentCompleted).
        THIS-OBJECT:webBrowser1:Navigated:Subscribe(THIS-OBJECT:webBrowser1_Navigated).
        THIS-OBJECT:webBrowser1:Navigating:Subscribe(THIS-OBJECT:webBrowser1_Navigating).
        /*  */
        /* m_WebBrowserForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:AccessibleDescription = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Left.AccessibleDescription":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:AccessibleName = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Left.AccessibleName":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:Anchor = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:BackgroundImage = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:BackgroundImageLayout = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:ImeMode = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:Location = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:MaximumSize = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:Name = "m_WebBrowserForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:RightToLeft = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:Size = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Left.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_WebBrowserForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:AccessibleDescription = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Right.AccessibleDescription":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:AccessibleName = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Right.AccessibleName":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:Anchor = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:BackgroundImage = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:BackgroundImageLayout = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:ImeMode = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:Location = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:MaximumSize = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:Name = "m_WebBrowserForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:RightToLeft = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:Size = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Right.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_WebBrowserForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:AccessibleDescription = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Top.AccessibleDescription":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:AccessibleName = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Top.AccessibleName":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:Anchor = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:BackgroundImage = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:BackgroundImageLayout = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:ImeMode = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:Location = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:MaximumSize = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:Name = "m_WebBrowserForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:RightToLeft = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:Size = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Top.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_WebBrowserForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:AccessibleDescription = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.AccessibleDescription":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:AccessibleName = resources:GetString("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.AccessibleName":U).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:Anchor = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:BackgroundImage = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:BackgroundImageLayout = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:ImeMode = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:Location = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:MaximumSize = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:Name = "m_WebBrowserForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:RightToLeft = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:Size = CAST(resources:GetObject("m_WebBrowserForm_Toolbars_Dock_Area_Bottom.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* WebBrowserForm */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:WebBrowserForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebBrowserForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "WebBrowserForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:WebBrowserForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Navigates to the given web location 
        Notes:   
        @param pcUri The web location to navigate to
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Navigate (pcUri AS CHARACTER):
		
		THIS-OBJECT:webBrowser1:Navigate (pcUri) .
        THIS-OBJECT:Text = pcUri . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the WebBrowserControl
        Notes:   
        @param sender The reference to the object that raised the ToolClick event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
	                                                     e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
		
		CASE e:Tool:Key:
		    
		    WHEN "Stop":U THEN 
		        webBrowser1:Stop () . 
            WHEN "Back":U THEN 
                webBrowser1:GoBack () . 
            WHEN "Forward":U THEN 
                webBrowser1:GoForward () . 
		    
		END CASE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DocumentCompleted event of the WebBrowserControl
        Notes:   
        @param sender The reference to the object that raised the DocumentCompleted event
        @param e The WebBrowserDocumentCompletedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID webBrowser1_DocumentCompleted (sender AS System.Object, 
	                                                   e AS System.Windows.Forms.WebBrowserDocumentCompletedEventArgs):
		
		THIS-OBJECT:Text = webBrowser1:Document:Title .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Navigated event of the WebBrowserControl
        Notes:   
        @param sender The reference to the object that raised the Navigated event
        @param e The WebBrowserNavigatedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID webBrowser1_Navigated (sender AS System.Object, 
	                                           e AS System.Windows.Forms.WebBrowserNavigatedEventArgs):
		
		ultraToolbarsManager1:Tools ["Stop":U]:SharedProps:Enabled = FALSE . 

        ultraToolbarsManager1:Tools ["Back":U]:SharedProps:Enabled = webBrowser1:CanGoBack . 
        ultraToolbarsManager1:Tools ["Forward":U]:SharedProps:Enabled = webBrowser1:CanGoForward . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Navigating event of the WebBrowserControl
        Notes:   
        @param sender The reference to the object that raised the Navigating event
        @param e The WebBrowserNavigatedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID webBrowser1_Navigating (sender AS System.Object, 
	                                            e AS System.Windows.Forms.WebBrowserNavigatingEventArgs):
		
        ultraToolbarsManager1:Tools ["Stop":U]:SharedProps:Enabled = TRUE . 

	END METHOD .

END CLASS.
