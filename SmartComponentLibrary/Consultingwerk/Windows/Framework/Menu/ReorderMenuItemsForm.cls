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
    File        : ReorderMenuItemsForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri Feb 15 18:15:31 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH.
USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .
USING System.Windows.Forms.* FROM ASSEMBLY .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.ReorderMenuItemsForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top    AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right  AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                       AS System.ComponentModel.IContainer                        NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1                            AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager  NO-UNDO.
    DEFINE PRIVATE VARIABLE ReorderMenuItemsForm_Fill_Panel                  AS Infragistics.Win.Misc.UltraPanel                        NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1                                       AS Infragistics.Win.UltraWinTree.UltraTree                 NO-UNDO.

    CONSTRUCTOR PUBLIC ReorderMenuItemsForm ():

        SUPER ().
        InitializeComponent ().

        THIS-OBJECT:StoreFormPosition = FALSE.

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Node in the NodesCollection
        Notes:
        @param pcNodeKey The Key of the new Node
        @param pcNodeText The Label of the Node
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddListEntry (pcNodeKey  AS CHARACTER,
                                     pcNodeText AS CHARACTER):

        ultraTree1:Nodes:Add (pcNodeKey, pcNodeText).

        IF ultraTree1:Nodes:Count = 1 THEN DO:
            ultraTree1:Nodes[0]:Selected = TRUE.
            ultraTree1:ActiveNode = ultraTree1:Nodes[0].
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the NodesCollection Index of the Node identified by the
                 NodeKey
        Notes:
        @param pcNodeKey Key of the Node
        @return INTEGER The NodesCollection Index
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER GetIndexOfEntry (pcNodeKey AS CHARACTER):

        RETURN ultraTree1:Nodes:IndexOf(pcNodeKey).

    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Up":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Down":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Ok":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("DataTab":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ReorderGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Up":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Down":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ActivityGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Ok":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Up":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Down":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Ok":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.ReorderMenuItemsForm":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar0[1] = buttonTool9.
        arrayvar0[2] = buttonTool12.
        arrayvar0[3] = buttonTool13.
        arrayvar0[4] = buttonTool11.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar0).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar1[1] = buttonTool10.
        arrayvar1[2] = buttonTool2.
        ribbonGroup1:Tools:AddRange(arrayvar1).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        buttonTool5:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool6:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = buttonTool5.
        arrayvar2[2] = buttonTool6.
        ribbonGroup2:Tools:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar3[1] = ribbonGroup1.
        arrayvar3[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar4[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        resources:ApplyResources(buttonTool3:SharedPropsInternal, "buttonTool3.SharedPropsInternal":U).
        buttonTool3:ForceApplyResources = "SharedPropsInternal":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        resources:ApplyResources(buttonTool4:SharedPropsInternal, "buttonTool4.SharedPropsInternal":U).
        buttonTool4:ForceApplyResources = "SharedPropsInternal":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        resources:ApplyResources(buttonTool7:SharedPropsInternal, "buttonTool7.SharedPropsInternal":U).
        buttonTool7:ForceApplyResources = "SharedPropsInternal":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        resources:ApplyResources(buttonTool8:SharedPropsInternal, "buttonTool8.SharedPropsInternal":U).
        buttonTool8:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar5[1] = buttonTool3.
        arrayvar5[2] = buttonTool4.
        arrayvar5[3] = buttonTool7.
        arrayvar5[4] = buttonTool8.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ReorderMenuItemsForm_Fill_Panel */
        /*  */
        /*  */
        /* ReorderMenuItemsForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel, "ReorderMenuItemsForm_Fill_Panel":U).
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:Name = "ReorderMenuItemsForm_Fill_Panel":U.
        /*  */
        /* ultraTree1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTree1, "ultraTree1":U).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        m_override1:SelectionType = Infragistics.Win.UltraWinTree.SelectType:SingleAutoDrag.
        THIS-OBJECT:ultraTree1:Override = m_override1.
        /*  */
        /* m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left, "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:Name = "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right, "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:Name = "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top, "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:Name = "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom, "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:Name = "m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ReorderMenuItemsForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ReorderMenuItemsForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "ReorderMenuItemsForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ReorderMenuItemsForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves a Node one position up or down
        Notes:
        @param pcAction The direction (Up / Down) to move the Node
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID MoveNode (pcAction AS CHARACTER):
        DEFINE VARIABLE cNodeKey AS CHARACTER NO-UNDO.

        IF NOT ultraTree1:SelectedNodes:Count = 1 THEN
            RETURN.

        cNodeKey = ultraTree1:SelectedNodes[0]:Key.

        CASE pcAction:
            WHEN "Up":U THEN DO:
                IF ultraTree1:SelectedNodes[0]:Index = 0 THEN
                    RETURN.

                ultraTree1:SelectedNodes[0]:Reposition (ultraTree1:Nodes, ultraTree1:SelectedNodes[0]:PrevVisibleNode:Index).
            END.
            WHEN "Down":U THEN DO:
                IF ultraTree1:SelectedNodes[0]:Index = ultraTree1:Nodes:Count - 1 THEN
                    RETURN.

                ultraTree1:SelectedNodes[0]:Reposition (ultraTree1:Nodes, ultraTree1:SelectedNodes[0]:NextVisibleNode:Index).
            END.
        END CASE.

        ultraTree1:SelectedNodes:Clear ().
        ultraTree1:GetNodeByKey (cNodeKey):Selected = TRUE.
        ultraTree1:ActiveNode = ultraTree1:GetNodeByKey (cNodeKey).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handler for the ToolClick Event of the smartToolbarController
        Notes:
        @param sender The source of the event
        @param e A ToolClickEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "Up":U THEN MoveNode ("Up":U).
            WHEN "Down":U THEN MoveNode ("Down":U).

            WHEN "Ok":U THEN THIS-OBJECT:DialogResult = DialogResult:OK.
            WHEN "Cancel":U THEN THIS-OBJECT:DialogResult = DialogResult:Cancel.
        END CASE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ReorderMenuItemsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ReorderMenuItemsForm ():

    END DESTRUCTOR .

END CLASS.
