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
    File        : GenericConnectionProfileForm
    Purpose     : Provides a generic user interface for working with 
                  the Consultingwerk.Framework.ConfigurationSettingsManager
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 11 20:30:45 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                    FROM PROPATH . 
USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH . 
USING Consultingwerk.Forms.BaseForm       FROM PROPATH .
USING Consultingwerk.AWSManager.*         FROM PROPATH .  
USING Consultingwerk.AWSManager.Forms.*   FROM PROPATH .  
USING Consultingwerk.Windows.*            FROM PROPATH . 
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .
USING System.Windows.Forms.*              FROM ASSEMBLY . 

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Forms.GenericConnectionProfileForm INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE m_ConnectionProfileForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConnectionProfileForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConnectionProfileForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ConnectionProfileForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    
    DEFINE VARIABLE lOpenConnectionMode AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Event raised to assign the settings form a Form to the Settings instance
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AssignSettingFromFormEventArgs with the data for this event              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AssignSettingFromForm SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                              e AS AssignSettingFromFormEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when a new List View Item has been created
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SettingListViewItemCreatedEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SettingListViewItemCreated SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                   e AS SettingListViewItemCreatedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when to create the Edit Dialog
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ShowEditDialogEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ShowEditDialog SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                       e AS ShowEditDialogEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns the GUID of the selected connection profile 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedProfileGuid AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Sub Item Columns collection  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SubItemColumns AS UltraListViewSubItemColumnsCollection NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ConnectionProfileForm 
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GenericConnectionProfileForm ():
        
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:StoreFormPosition = FALSE . 
        THIS-OBJECT:SubItemColumns = ultraListView1:SubItemColumns .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Edits the current profile 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EditProfile ():
        
        DEFINE VARIABLE oForm          AS System.Windows.Forms.Form    NO-UNDO .
        DEFINE VARIABLE oConfigManager AS ConfigurationSettingsManager NO-UNDO .  
        DEFINE VARIABLE oSetting       AS Progress.Lang.Object         NO-UNDO . 
        DEFINE VARIABLE oDialogResult  AS DialogResult                 NO-UNDO .                
        DEFINE VARIABLE eShowEditDialogEventArgs AS ShowEditDialogEventArgs NO-UNDO .                 
        DEFINE VARIABLE eAssignSettingFromFormEventArgs AS AssignSettingFromFormEventArgs NO-UNDO . 
                
        oConfigManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ConfigurationSettingsManager} .
    
        IF ultraListView1:SelectedItems:Count > 0 THEN 
            oSetting = oConfigManager:GetSettingByGUID (ultraListView1:SelectedItems[0]:Key)  . 
        ELSE
            RETURN .
                    
        eShowEditDialogEventArgs = NEW ShowEditDialogEventArgs (oSetting) .
        
        OnShowEditDialog (eShowEditDialogEventArgs) .

        IF eShowEditDialogEventArgs:Cancel OR NOT VALID-OBJECT (eShowEditDialogEventArgs:Form) THEN RETURN . 
                    
        WAIT-FOR eShowEditDialogEventArgs:Form:ShowDialog (THIS-OBJECT) SET oDialogResult . 
                        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult}
         
        ASSIGN eAssignSettingFromFormEventArgs = NEW AssignSettingFromFormEventArgs (eShowEditDialogEventArgs:Form, 
                                                                                     oSetting) .
         
        OnAssignSettingFromForm (eAssignSettingFromFormEventArgs) . 
        
        IF eAssignSettingFromFormEventArgs:Cancel THEN RETURN . 
        
        OnSettingListViewItemCreated (NEW SettingListViewItemCreatedEventArgs (ultraListView1:SelectedItems[0], 
                                                                               oSetting)) . 
         
        oConfigManager:StoreToSettingsService () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AssignSettingFromForm event
        Notes:   
        @param e The AssignSettingFromFormEventArgs with the data for this event              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAssignSettingFromForm (e AS AssignSettingFromFormEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AssignSettingFromForm":U) .
        
        THIS-OBJECT:AssignSettingFromForm:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:   
        @param e The FormClosingEventArgs with th edata for this event 
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

        DEFINE VARIABLE oConfigManager AS ConfigurationSettingsManager NO-UNDO .  
        
        oConfigManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ConfigurationSettingsManager} .
    
        IF ultraListView1:SelectedItems:Count > 0 THEN 
            THIS-OBJECT:SelectedProfileGuid = ultraListView1:SelectedItems[0]:Key . 
        ELSE 
            THIS-OBJECT:SelectedProfileGuid = "":U . 

        IF lOpenConnectionMode = FALSE THEN 
            oConfigManager:StoreToSettingsService () .
                
        SUPER:OnFormClosing (INPUT e).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE oDictionary      AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO.
        DEFINE VARIABLE oConfigManager   AS ConfigurationSettingsManager                             NO-UNDO .  
        DEFINE VARIABLE oItem            AS Infragistics.Win.UltraWinListView.UltraListViewItem      NO-UNDO .                 
    
        DEFINE VARIABLE oSettingsService AS ISettingsService                                         NO-UNDO .
        DEFINE VARIABLE oSetting         AS Progress.Lang.Object                                     NO-UNDO . 

        DEFINE VARIABLE cView            AS CHARACTER                                                NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                                                  NO-UNDO .                
                
        SUPER:OnLoad (e) .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        DO ON ERROR UNDO, THROW:
            ASSIGN cView = oSettingsService:GetSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                                        "View":U) .
            
            IF cView > "":U THEN 
                ultraListView1:View = {Consultingwerk/parse-enum.i Infragistics.Win.UltraWinListView.UltraListViewStyle cView} .   
                                   
            CATCH err AS Progress.Lang.Error :
                /* noop */    
            END CATCH.
        END.

        oConfigManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ConfigurationSettingsManager} .

        oDictionary = oConfigManager:GetSettings() .

        DO i = 1 TO oDictionary:Count:
            
            oItem = NEW Infragistics.Win.UltraWinListView.UltraListViewItem (ENTRY (i, oDictionary:Keys, oDictionary:KeyDelimiter)) .
            oItem:Value = oDictionary:GetValue (oItem:Key) .
            
            oSetting = oConfigManager:GetSettingByGUID (oItem:Key) .

            THIS-OBJECT:ultraListView1:Items:Add (oItem) .

            OnSettingListViewItemCreated (NEW SettingListViewItemCreatedEventArgs (oItem, oSetting)) .
        END.

        ultraListView1_ItemSelectionChanged (ultraListView1, ?) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .     
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SettingListViewItemCreated event
        Notes:   
        @param e The SettingListViewItemCreatedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSettingListViewItemCreated (e AS SettingListViewItemCreatedEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "SettingListViewItemCreated":U) .
        
        THIS-OBJECT:SettingListViewItemCreated:Publish (THIS-OBJECT,  
                                                        e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnShowEditDialog event 
        Notes:   
        @param e The ShowEditDialogEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnShowEditDialog (e AS ShowEditDialogEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ShowEditDialog":U) .

        THIS-OBJECT:ShowEditDialog:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OpenConnectionShowDialog ():
        
        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
        
        lOpenConnectionMode = TRUE .
        
        THIS-OBJECT:Text = "Open Connection"{&TRAN} . 
        
        ultraToolbarsManager1:Tools["Edit Profile":U]:SharedProps:Visible = FALSE . 
        ultraToolbarsManager1:Tools["Remove Profile":U]:SharedProps:Visible = FALSE . 
        ultraToolbarsManager1:Tools["Cancel":U]:SharedProps:Visible = TRUE . 
        
        WAIT-FOR THIS-OBJECT:ShowDialog () SET oDialogResult .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OpenMaintenanceShowDialog ():
        
        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
    
        lOpenConnectionMode = FALSE . 
        
        THIS-OBJECT:Text = "Manage Connections"{&TRAN} . 
        
        ultraToolbarsManager1:Tools["Edit Profile":U]:SharedProps:Visible = TRUE . 
        ultraToolbarsManager1:Tools["Remove Profile":U]:SharedProps:Visible = TRUE . 
        ultraToolbarsManager1:Tools["Cancel":U]:SharedProps:Visible = FALSE .
        
        WAIT-FOR THIS-OBJECT:ShowDialog () SET oDialogResult .
        
    END METHOD .

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
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Profile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove Profile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Spacer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Ok":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Details":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("List":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Tiles":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Ok":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Spacer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("List":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Details":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Tiles":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Profile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove Profile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Forms.GenericConnectionProfileForm":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:WindowsVista.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool6:InstanceProps:IsFirstInGroup = TRUE.
        labelTool2:InstanceProps:Spring = Infragistics.Win.DefaultableBoolean:True.
        labelTool2:InstanceProps:Width = 250.
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool3:InstanceProps:Spring = Infragistics.Win.DefaultableBoolean:True.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar0[1] = popupMenuTool1.
        arrayvar0[2] = buttonTool6.
        arrayvar0[3] = buttonTool8.
        arrayvar0[4] = labelTool2.
        arrayvar0[5] = buttonTool3.
        arrayvar0[6] = buttonTool1.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Settings:FillEntireRow = Infragistics.Win.DefaultableBoolean:True.
        ultraToolbar1:Settings:GrabHandleStyle = Infragistics.Win.UltraWinToolbars.GrabHandleStyle:None.
        ultraToolbar1:Text = "UltraToolbar1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        popupMenuTool3:SharedPropsInternal:Caption = "View":U.
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool10:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar2[1] = buttonTool9.
        arrayvar2[2] = buttonTool5.
        arrayvar2[3] = buttonTool10.
        popupMenuTool3:Tools:AddRange(arrayvar2).
        buttonTool2:SharedPropsInternal:Caption = "&Cancel":U.
        buttonTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool4:SharedPropsInternal:Caption = "&Ok":U.
        buttonTool4:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool7:SharedPropsInternal:Caption = "List":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool11:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool11:SharedPropsInternal:Caption = "Details":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool12:SharedPropsInternal:Caption = "Tiles":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool13:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool13:SharedPropsInternal:Caption = "Edit Profile":U.
        buttonTool13:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool14:SharedPropsInternal:Caption = "Remove Profile":U.
        buttonTool14:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 9 NO-UNDO.
        arrayvar3[1] = popupMenuTool3.
        arrayvar3[2] = buttonTool2.
        arrayvar3[3] = buttonTool4.
        arrayvar3[4] = labelTool1.
        arrayvar3[5] = buttonTool7.
        arrayvar3[6] = buttonTool11.
        arrayvar3[7] = buttonTool12.
        arrayvar3[8] = buttonTool13.
        arrayvar3[9] = buttonTool14.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar3).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ConnectionProfileForm_Fill_Panel */
        /*  */
        /*  */
        /* ConnectionProfileForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 30).
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:Name = "ConnectionProfileForm_Fill_Panel":U.
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:Size = NEW System.Drawing.Size(569, 285).
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:MainColumn:Text = "Description":U.
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(569, 285).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:ultraListView1:ViewSettingsTiles:ItemSize = NEW System.Drawing.Size(300, 32).
        THIS-OBJECT:ultraListView1:ItemDoubleClick:Subscribe(THIS-OBJECT:ultraListView1_ItemDoubleClick).
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemSelectionChanged).
        THIS-OBJECT:ultraListView1:KeyDown:Subscribe(THIS-OBJECT:ultraListView1_KeyDown).
        /*  */
        /* m_ConnectionProfileForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 30).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:Name = "m_ConnectionProfileForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 285).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ConnectionProfileForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(569, 30).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:Name = "m_ConnectionProfileForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 285).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ConnectionProfileForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:Name = "m_ConnectionProfileForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(569, 30).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(225)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(246))).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 315).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:Name = "m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(569, 0).
        THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* GenericConnectionProfileForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(569, 315).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ConnectionProfileForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConnectionProfileForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "GenericConnectionProfileForm":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Connection Profiles":U.
        THIS-OBJECT:WindowPositionRegistryKey = "GenericConnectionProfileForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ConnectionProfileForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the selected profile 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveProfile ():
        
        DEFINE VARIABLE oConfigManager AS ConfigurationSettingsManager NO-UNDO .
        DEFINE VARIABLE oDialogResult  AS DialogResultEnum             NO-UNDO .
        
        IF ultraListView1:SelectedItems:Count = 0 THEN 
            RETURN . 
    
        oDialogResult = Consultingwerk.Util.MessageFormHelper:AskQuestion ("Are you sure that you want to remove the selected profile?"{&TRAN}, 
                                                                           "Remove Profile"{&TRAN}) .
               
        IF oDialogResult = DialogResultEnum:DialogResultYes THEN . 
        ELSE 
            RETURN .                
               
        oConfigManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ConfigurationSettingsManager} .
            
        oConfigManager:RemoveInstance (ultraListView1:SelectedItems[0]:Key) .

        ultraListView1:Items:Remove (ultraListView1:SelectedItems[0]) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemDoubleClick event of the ultraListView1  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemDoubleClickEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemDoubleClick (sender AS System.Object, 
                                                        e AS Infragistics.Win.UltraWinListView.ItemDoubleClickEventArgs):
        
        IF lOpenConnectionMode = FALSE THEN 
            THIS-OBJECT:EditProfile () .
        ELSE DO:
            THIS-OBJECT:DialogResult = DialogResult:OK .
            THIS-OBJECT:Close () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the ultraListView1  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemSelectionChanged (sender AS System.Object, 
                                                             e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):
        
        IF lOpenConnectionMode = TRUE THEN 
            ultraToolbarsManager1:Tools["Ok":U]:SharedProps:Enabled = ultraListView1:SelectedItems:Count > 0 . 

        ultraToolbarsManager1:Tools["Edit Profile":U]:SharedProps:Enabled = ultraListView1:SelectedItems:Count > 0 . 
        ultraToolbarsManager1:Tools["Remove Profile":U]:SharedProps:Enabled = ultraListView1:SelectedItems:Count > 0 . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the ultraListView1  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_KeyDown (sender AS System.Object, e AS System.Windows.Forms.KeyEventArgs):
        
        IF ultraListView1:SelectedItems:Count > 0 AND 
           (Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) OR
            Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return)) THEN DO: 
             
            IF lOpenConnectionMode = FALSE THEN 
                THIS-OBJECT:EditProfile () .
            ELSE DO: 
                THIS-OBJECT:DialogResult = DialogResult:OK .
                THIS-OBJECT:Close () .
            END. 
        END . 
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Escape) THEN DO:
            THIS-OBJECT:DialogResult = DialogResult:Cancel .
            THIS-OBJECT:Close () .
        END. 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        CASE e:Tool:Key:
            WHEN "Cancel":U THEN DO:
                THIS-OBJECT:DialogResult = DialogResult:Cancel .
                THIS-OBJECT:Close () .
            END.
            
            WHEN "Ok":U THEN DO:
                THIS-OBJECT:DialogResult = DialogResult:Ok .
                THIS-OBJECT:Close () .
            END.
            
            WHEN "Edit Profile":U THEN 
                EditProfile () .
                
            WHEN "Remove Profile":U THEN 
                RemoveProfile () .
            
            WHEN "List":U THEN 
                ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List .

            WHEN "Details":U THEN 
                ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details .

            WHEN "Tiles":U THEN 
                ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Tiles .
        END CASE . 

        oSettingsService:StoreSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                       "View":U, 
                                       ultraListView1:View:ToString()) .

    END METHOD .

    DESTRUCTOR PUBLIC GenericConnectionProfileForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
