/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartMenuSecurityMaintenanceForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu May 22 13:20:21 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*       FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH . 
USING Consultingwerk.Windows.Framework.Authorization.*    FROM PROPATH .
USING Consultingwerk.Windows.Util.*                       FROM PROPATH . 
USING Infragistics.Win.UltraWinTree.*                     FROM ASSEMBLY .  
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i} 

CLASS Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceForm 
    INHERITS SmartWindowForm: 
	
    DEFINE PRIVATE VARIABLE m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top    AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                  NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right  AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                  NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                  NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                  NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                                   AS System.ComponentModel.IContainer                                         NO-UNDO.
    DEFINE PRIVATE VARIABLE smartMenuSecurityMaintenanceControl1 AS Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1                                      AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController     NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartMenuSecurityMaintenanceForm_Fill_Panel                  AS Infragistics.Win.Misc.UltraPanel                                         NO-UNDO.

    DEFINE VARIABLE oOwnerList AS CharacterDictionary NO-UNDO.
    
    {Consultingwerk/SmartFramework/Authorization/dsAuthorizationMaintenance.i}

    /*------------------------------------------------------------------------------
        Purpose: Initializes a new instance of the SmartMenuSecurityMaintenanceForm
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartMenuSecurityMaintenanceForm ():
	    
        SUPER ().
        InitializeComponent ().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:smartMenuSecurityMaintenanceControl1:HasPendingChangesChanged:Subscribe (HasPendingChangesChangedHandler) .

        THIS-OBJECT:HasPendingChangesChangedHandler (smartMenuSecurityMaintenanceControl1, 
                                                     Consultingwerk.EventArgs:Empty) .
    
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
        
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the HasPendingChanges event of the 
                 SmartMenuSecurityMaintenance user control
        Notes:   Enables and disables the Cancel and Save button  
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID HasPendingChangesChangedHandler (sender AS Progress.Lang.Object,
	                                                       e AS Consultingwerk.EventArgs):
		
		ASSIGN smartToolbarController1:Tools["Save Settings":U]:SharedProps:Enabled = smartMenuSecurityMaintenanceControl1:HasPendingChanges
		       smartToolbarController1:Tools["Cancel":U]:SharedProps:Enabled = smartMenuSecurityMaintenanceControl1:HasPendingChanges 
               smartToolbarController1:Tools["Reload":U]:SharedProps:Enabled = NOT smartMenuSecurityMaintenanceControl1:HasPendingChanges 
                                                                               .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the components of the form instance
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
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open Users and Groups":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Settings":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open Users and Groups":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Settings":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartMenuSecurityMaintenanceControl1 = NEW Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceControl().
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool7:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool2:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool7.
        arrayvar0[3] = buttonTool2.
        arrayvar0[4] = buttonTool3.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar1[1] = ribbonGroup1.
        ribbonTab1:Groups:AddRange(arrayvar1).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar2[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar2).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool4:SharedPropsInternal:Caption = resources:GetString("resource.Caption":U).
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool5:SharedPropsInternal:Caption = resources:GetString("resource.Caption1":U).
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool6:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        resources:ApplyResources(buttonTool8:SharedPropsInternal, "buttonTool8.SharedPropsInternal":U).
        buttonTool8:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar3[1] = buttonTool4.
        arrayvar3[2] = buttonTool5.
        arrayvar3[3] = buttonTool6.
        arrayvar3[4] = buttonTool8.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar3).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* SmartMenuSecurityMaintenanceForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartMenuSecurityMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartMenuSecurityMaintenanceControl1).
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel, "SmartMenuSecurityMaintenanceForm_Fill_Panel":U).
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:Name = "SmartMenuSecurityMaintenanceForm_Fill_Panel":U.
        /*  */
        /* smartMenuSecurityMaintenanceControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartMenuSecurityMaintenanceControl1, "smartMenuSecurityMaintenanceControl1":U).
        THIS-OBJECT:smartMenuSecurityMaintenanceControl1:Name = "smartMenuSecurityMaintenanceControl1":U.
        /*  */
        /* m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left, "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right, "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top, "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom, "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartMenuSecurityMaintenanceForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuSecurityMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartMenuSecurityMaintenanceForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartMenuSecurityMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Override of the OnShown Method to first show the form and then 
                 initiate reading data.
        Notes:   
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e).

        PROCESS EVENTS . 

        IF NOT SelectUserAndGroups () THEN RETURN.

        THIS-OBJECT:smartMenuSecurityMaintenanceControl1:OpenUserAndGroupAuthorization (oOwnerList) .
        
        CATCH ple AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Display a dialog to select Users and Groups and make use of this 
                 selection to display security settings.
        Notes:   
        @return Indicator LOGICAL that shows if the selection was changed 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LOGICAL SelectUserAndGroups ():
        
        DEFINE VARIABLE oSelectionForm AS SmartUserAndGroupSelectionForm    NO-UNDO.
        DEFINE VARIABLE oDialogResult  AS System.Windows.Forms.DialogResult NO-UNDO.
        
        oSelectionForm = NEW Consultingwerk.Windows.Framework.Authorization.SmartUserAndGroupSelectionForm ().

        IF VALID-OBJECT (oOwnerList) THEN 
            oSelectionForm:SelectedUserAndGroups = oOwnerList.
        
        FormHelper:ShowDialog (oSelectionForm, OUTPUT oDialogResult) .
        
        IF NOT oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN RETURN FALSE.
        
        oOwnerList = oSelectionForm:SelectedUserAndGroups.

        RETURN TRUE.
        
        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oSelectionForm).
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the ToolClick event of the ToolbarController
        Notes:   
        @param sender The source of the event
        @param e A CancelableEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object, 
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        DEFINE VARIABLE oResult AS DialogResultEnum NO-UNDO . 

        CASE e:Tool:Key :
            WHEN "Save Settings":U THEN
                THIS-OBJECT:smartMenuSecurityMaintenanceControl1:StoreAuthorizationData ().

            WHEN "Open Users and Groups":U THEN DO:
                IF NOT THIS-OBJECT:SelectUserAndGroups () THEN RETURN.

                THIS-OBJECT:smartMenuSecurityMaintenanceControl1:OpenUserAndGroupAuthorization (oOwnerList) .
            END.
            WHEN "Cancel":U THEN DO:
                oResult = MessageFormHelper:AskQuestion ("Are you sure that you want to cancel pending updates to the security settings?"{&TRAN},
                                                         THIS-OBJECT:Text) .
        
                IF oResult = DialogResultEnum:DialogResultNo THEN 
                    RETURN . 

                THIS-OBJECT:smartMenuSecurityMaintenanceControl1:Cancel () .
            END. 
            WHEN "Reload":U THEN DO:
                THIS-OBJECT:smartMenuSecurityMaintenanceControl1:Reload () .
            END.
        END CASE.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartMenuSecurityMaintenanceForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenuSecurityMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
