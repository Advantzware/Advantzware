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
    File        : SmartGenericLookupDialogDesigner
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 14 09:59:59 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.* FROM ASSEMBLY .
USING Progress.Windows.Form FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartGenericLookupDialogDesigner 
    INHERITS Form : 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PROTECTED VARIABLE button2 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PROTECTED VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE Form_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PROTECTED VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
        
    CONSTRUCTOR PUBLIC SmartGenericLookupDialogDesigner (  ):
        SUPER().
        InitializeComponent().
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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance151 AS Infragistics.Win.Appearance NO-UNDO.
        appearance151 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS System.Resources.ResXResourceSet NO-UNDO.
        resources = Progress.Util.ResourceHelper:Load("Consultingwerk\SmartComponents\Support\SmartGenericLookupDialogDesigner.resx", PROPATH).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance164 AS Infragistics.Win.Appearance NO-UNDO.
        appearance164 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance165 AS Infragistics.Win.Appearance NO-UNDO.
        appearance165 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance166 AS Infragistics.Win.Appearance NO-UNDO.
        appearance166 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance155 AS Infragistics.Win.Appearance NO-UNDO.
        appearance155 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance152 AS Infragistics.Win.Appearance NO-UNDO.
        appearance152 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance153 AS Infragistics.Win.Appearance NO-UNDO.
        appearance153 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance154 AS Infragistics.Win.Appearance NO-UNDO.
        appearance154 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance163 AS Infragistics.Win.Appearance NO-UNDO.
        appearance163 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance158 AS Infragistics.Win.Appearance NO-UNDO.
        appearance158 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance157 AS Infragistics.Win.Appearance NO-UNDO.
        appearance157 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance156 AS Infragistics.Win.Appearance NO-UNDO.
        appearance156 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance160 AS Infragistics.Win.Appearance NO-UNDO.
        appearance160 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance162 AS Infragistics.Win.Appearance NO-UNDO.
        appearance162 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance161 AS Infragistics.Win.Appearance NO-UNDO.
        appearance161 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance159 AS Infragistics.Win.Appearance NO-UNDO.
        appearance159 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:button2 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:Form_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:Form_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 400).
        THIS-OBJECT:panel1:Name = "panel1".
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(630, 31).
        THIS-OBJECT:panel1:TabIndex = 4.
        /*  */
        /* button2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button2:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button2:Location = NEW System.Drawing.Point(446, 3).
        THIS-OBJECT:button2:Name = "button2".
        THIS-OBJECT:button2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button2:TabIndex = 3.
        THIS-OBJECT:button2:Text = "OK".
        THIS-OBJECT:button2:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button2:UseVisualStyleBackColor = TRUE.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(543, 3).
        THIS-OBJECT:button1:Name = "button1".
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 2.
        THIS-OBJECT:button1:Text = "Cancel".
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:AddNavigationTools = FALSE.
        THIS-OBJECT:smartToolbarController1:AddTableIOTools = FALSE.
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar0[1] = buttonTool3.
        arrayvar0[2] = buttonTool4.
        arrayvar0[3] = buttonTool5.
        arrayvar0[4] = buttonTool17.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "Navigation Toolbar".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar1).
        appearance151:Image = CAST(resources:GetObject("appearance151.Image"), System.Object).
        buttonTool2:SharedProps:AppearancesSmall:Appearance = appearance151.
        buttonTool2:SharedProps:Caption = "First record".
        buttonTool2:SharedProps:CustomizerCaption = "First".
        appearance164:Image = CAST(resources:GetObject("appearance164.Image"), System.Object).
        buttonTool18:SharedProps:AppearancesSmall:Appearance = appearance164.
        buttonTool18:SharedProps:Caption = "Previous record".
        buttonTool18:SharedProps:CustomizerCaption = "Previous".
        appearance165:Image = CAST(resources:GetObject("appearance165.Image"), System.Object).
        buttonTool19:SharedProps:AppearancesSmall:Appearance = appearance165.
        buttonTool19:SharedProps:Caption = "Next record".
        buttonTool19:SharedProps:CustomizerCaption = "Next".
        appearance166:Image = CAST(resources:GetObject("appearance166.Image"), System.Object).
        buttonTool20:SharedProps:AppearancesSmall:Appearance = appearance166.
        buttonTool20:SharedProps:Caption = "Last record".
        buttonTool20:SharedProps:CustomizerCaption = "Last".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool2.
        arrayvar2[2] = buttonTool18.
        arrayvar2[3] = buttonTool19.
        arrayvar2[4] = buttonTool20.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar2).
        /*  */
        /* Form_Fill_Panel */
        /*  */
        THIS-OBJECT:Form_Fill_Panel:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:Form_Fill_Panel:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Form_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:Form_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:Form_Fill_Panel:Location = NEW System.Drawing.Point(0, 23).
        THIS-OBJECT:Form_Fill_Panel:Name = "Form_Fill_Panel".
        THIS-OBJECT:Form_Fill_Panel:Size = NEW System.Drawing.Size(630, 431).
        THIS-OBJECT:Form_Fill_Panel:TabIndex = 0.
        /*  */
        /* _Form_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 23).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Name = "_Form_Toolbars_Dock_Area_Left".
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 431).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* _Form_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(630, 23).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Name = "_Form_Toolbars_Dock_Area_Right".
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 431).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* _Form_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Name = "_Form_Toolbars_Dock_Area_Top".
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(630, 23).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* _Form_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 454).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Name = "_Form_Toolbars_Dock_Area_Bottom".
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(630, 0).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        appearance155:BackColor = System.Drawing.SystemColors:Window.
        appearance155:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance155.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance152:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance152:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance152:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance152:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance152.
        appearance153:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance153.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance154:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance154:BackColor2 = System.Drawing.SystemColors:Control.
        appearance154:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance154:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance154.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance163:BackColor = System.Drawing.SystemColors:Window.
        appearance163:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance163.
        appearance158:BackColor = System.Drawing.SystemColors:Highlight.
        appearance158:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance158.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance157:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance157.
        appearance156:BorderColor = System.Drawing.Color:Silver.
        appearance156:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance156.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance160:BackColor = System.Drawing.SystemColors:Control.
        appearance160:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance160:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance160:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance160:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance160.
        appearance162:TextHAlignAsString = "Left".
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance162.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance161:BackColor = System.Drawing.SystemColors:Window.
        appearance161:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance161.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        appearance159:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance159.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartDataBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1".
        THIS-OBJECT:smartDataBrowser1:Size = NEW System.Drawing.Size(630, 400).
        THIS-OBJECT:smartDataBrowser1:TabIndex = 5.
        THIS-OBJECT:smartDataBrowser1:Text = "smartDataBrowser1".
        /*  */
        /* SmartGenericLookupDialogDesigner */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(630, 454).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:Form_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Name = "SmartGenericLookupDialogDesigner".
        THIS-OBJECT:Text = "SmartGenericLookupDialogDesigner".
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:Form_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartGenericLookupDialogDesigner ( ):
        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.
    END DESTRUCTOR.

END CLASS.