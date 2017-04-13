/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartObjectMasterViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 25.05.2016 14:59:05
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_CustomizationResultGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ModuleGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectPackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectExtension_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_ObjectExtension AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_StaticObject AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_TemplateObject AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_DeploymentType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_DeploymentType AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_RunnableFromMenu AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_Disabled AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectMaster_DesignTimeDataSourceGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupCustomizationType AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupDesignTimeDataSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupModule AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupObjectType AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectMasterViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectMasterViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:SetControlEnabled (lookupObjectType,
                                       lookupModule,
                                       lookupDesignTimeDataSource,
                                       lookupCustomizationType,
                                       ControlEnabledEnum:Add) .

    END CONSTRUCTOR.

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartObjectMaster":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartObjectMaster_ObjectName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectMaster_CustomizationResultGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ModuleGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectMaster_StaticObject = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectMaster_TemplateObject = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectMaster_DeploymentType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectMaster_DeploymentType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectMaster_Disabled = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectMaster_DesignTimeDataSourceGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:lookupObjectType = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:lookupModule = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:lookupDesignTimeDataSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:lookupCustomizationType = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectExtension, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_StaticObject, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_TemplateObject, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_DeploymentType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_Disabled, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupObjectType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupModule, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupDesignTimeDataSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupCustomizationType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 18 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CustomizationResultGuid":U, "Customization Result":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("CustomizationResultCode":U, "Customization Result Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("ObjectTypeName":U, "Object Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "Module Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("ObjectDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("ObjectPackage":U, "Object Package":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("ObjectExtension":U, "Extension":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("StaticObject":U, "Static Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("TemplateObject":U, "Template Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("DeploymentType":U, "Deployment Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("RunnableFromMenu":U, "Runnable from Menu":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("Disabled":U, "Disabled":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("DesignTimeDataSourceGuid":U, "Design Time Data Source":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("DesignTimeDataSourceName":U, "Design Time Data Source Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartObjectMaster_ObjectName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ObjectName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectName_Label, "eSmartObjectMaster_ObjectName_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectName_Label:Name = "eSmartObjectMaster_ObjectName_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectName, "eSmartObjectMaster_ObjectName":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectName":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_ObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectMaster_ObjectName:Name = "eSmartObjectMaster_ObjectName":U.
        /*  */
        /* eSmartObjectMaster_CustomizationResultGuid_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_CustomizationResultGuid_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_CustomizationResultGuid_Label, "eSmartObjectMaster_CustomizationResultGuid_Label":U).
        THIS-OBJECT:eSmartObjectMaster_CustomizationResultGuid_Label:Name = "eSmartObjectMaster_CustomizationResultGuid_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectTypeGuid_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ObjectTypeGuid_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectTypeGuid_Label, "eSmartObjectMaster_ObjectTypeGuid_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectTypeGuid_Label:Name = "eSmartObjectMaster_ObjectTypeGuid_Label":U.
        /*  */
        /* eSmartObjectMaster_ModuleGuid_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ModuleGuid_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ModuleGuid_Label, "eSmartObjectMaster_ModuleGuid_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ModuleGuid_Label:Name = "eSmartObjectMaster_ModuleGuid_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectDescription_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectDescription_Label, "eSmartObjectMaster_ObjectDescription_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription_Label:Name = "eSmartObjectMaster_ObjectDescription_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectDescription, "eSmartObjectMaster_ObjectDescription":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectDescription":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectMaster_ObjectDescription:Name = "eSmartObjectMaster_ObjectDescription":U.
        /*  */
        /* eSmartObjectMaster_ObjectPackage_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectPackage_Label, "eSmartObjectMaster_ObjectPackage_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage_Label:Name = "eSmartObjectMaster_ObjectPackage_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectPackage */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectPackage, "eSmartObjectMaster_ObjectPackage":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectPackage":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectMaster_ObjectPackage:Name = "eSmartObjectMaster_ObjectPackage":U.
        /*  */
        /* eSmartObjectMaster_ObjectExtension_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectExtension_Label, "eSmartObjectMaster_ObjectExtension_Label":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension_Label:Name = "eSmartObjectMaster_ObjectExtension_Label":U.
        /*  */
        /* eSmartObjectMaster_ObjectExtension */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_ObjectExtension, "eSmartObjectMaster_ObjectExtension":U).
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectExtension":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectMaster_ObjectExtension:Name = "eSmartObjectMaster_ObjectExtension":U.
        /*  */
        /* eSmartObjectMaster_StaticObject */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_StaticObject, "eSmartObjectMaster_StaticObject":U).
        THIS-OBJECT:eSmartObjectMaster_StaticObject:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "StaticObject":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_StaticObject:Name = "eSmartObjectMaster_StaticObject":U.
        THIS-OBJECT:eSmartObjectMaster_StaticObject:UseMnemonics = TRUE.
        /*  */
        /* eSmartObjectMaster_TemplateObject */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_TemplateObject, "eSmartObjectMaster_TemplateObject":U).
        THIS-OBJECT:eSmartObjectMaster_TemplateObject:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TemplateObject":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_TemplateObject:Name = "eSmartObjectMaster_TemplateObject":U.
        THIS-OBJECT:eSmartObjectMaster_TemplateObject:UseMnemonics = TRUE.
        /*  */
        /* eSmartObjectMaster_DeploymentType_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_DeploymentType_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_DeploymentType_Label, "eSmartObjectMaster_DeploymentType_Label":U).
        THIS-OBJECT:eSmartObjectMaster_DeploymentType_Label:Name = "eSmartObjectMaster_DeploymentType_Label":U.
        /*  */
        /* eSmartObjectMaster_DeploymentType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_DeploymentType, "eSmartObjectMaster_DeploymentType":U).
        THIS-OBJECT:eSmartObjectMaster_DeploymentType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DeploymentType":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_DeploymentType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectMaster_DeploymentType:Name = "eSmartObjectMaster_DeploymentType":U.
        /*  */
        /* eSmartObjectMaster_RunnableFromMenu */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu, "eSmartObjectMaster_RunnableFromMenu":U).
        THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RunnableFromMenu":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu:Name = "eSmartObjectMaster_RunnableFromMenu":U.
        THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu:UseMnemonics = TRUE.
        /*  */
        /* eSmartObjectMaster_Disabled */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_Disabled, "eSmartObjectMaster_Disabled":U).
        THIS-OBJECT:eSmartObjectMaster_Disabled:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Disabled":U, TRUE)).
        THIS-OBJECT:eSmartObjectMaster_Disabled:Name = "eSmartObjectMaster_Disabled":U.
        THIS-OBJECT:eSmartObjectMaster_Disabled:UseMnemonics = TRUE.
        /*  */
        /* eSmartObjectMaster_DesignTimeDataSourceGuid_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectMaster_DesignTimeDataSourceGuid_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectMaster_DesignTimeDataSourceGuid_Label, "eSmartObjectMaster_DesignTimeDataSourceGuid_Label":U).
        THIS-OBJECT:eSmartObjectMaster_DesignTimeDataSourceGuid_Label:Name = "eSmartObjectMaster_DesignTimeDataSourceGuid_Label":U.
        /*  */
        /* lookupObjectType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupObjectType, "lookupObjectType":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:lookupObjectType:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:lookupObjectType:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectTypeGuid":U, TRUE)).
        THIS-OBJECT:lookupObjectType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectTypeName":U, TRUE)).
        THIS-OBJECT:lookupObjectType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupObjectType:LookupAdditionalFields = "":U.
        THIS-OBJECT:lookupObjectType:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:lookupObjectType:LookupBrowserFields = "ObjectTypeName,ObjectTypeDescription,ExtendsObjectTypeName,ContainerType,StaticObject,TechnicalClassName":U.
        THIS-OBJECT:lookupObjectType:LookupCascade = FALSE.
        THIS-OBJECT:lookupObjectType:LookupControls = "eSmartObjectMaster_StaticObject":U.
        THIS-OBJECT:lookupObjectType:LookupDialogClassName = "":U.
        THIS-OBJECT:lookupObjectType:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:lookupObjectType:LookupDialogFilterFields = "ObjectTypeName,ObjectTypeDescription":U.
        THIS-OBJECT:lookupObjectType:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupObjectType:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:lookupObjectType:LookupDialogQuerySort = "BY eSmartObjectType.ObjectTypeName":U.
        THIS-OBJECT:lookupObjectType:LookupDialogQueryString = "":U.
        THIS-OBJECT:lookupObjectType:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:lookupObjectType:LookupEntityTable = "eSmartObjectType":U.
        THIS-OBJECT:lookupObjectType:LookupEntityView = "":U.
        THIS-OBJECT:lookupObjectType:LookupFields = "StaticObject":U.
        THIS-OBJECT:lookupObjectType:LookupKeyField = "ObjectTypeName":U.
        THIS-OBJECT:lookupObjectType:LookupKeyValue = "":U.
        THIS-OBJECT:lookupObjectType:LookupKeyValueColumn = "ObjectTypeGuid":U.
        THIS-OBJECT:lookupObjectType:LookupQuerySort = "":U.
        THIS-OBJECT:lookupObjectType:LookupQueryString = "FOR EACH eSmartObjectType WHERE eSmartObjectType.ObjectTypeName BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupObjectType:LookupTimerDelay = 750.
        THIS-OBJECT:lookupObjectType:LookupTimerDisabled = FALSE.
        THIS-OBJECT:lookupObjectType:Name = "lookupObjectType":U.
        THIS-OBJECT:lookupObjectType:OwningGrid = ?.
        /*  */
        /* lookupModule */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupModule, "lookupModule":U).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:lookupModule:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:lookupModule:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModuleGuid":U, TRUE)).
        THIS-OBJECT:lookupModule:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModuleName":U, TRUE)).
        THIS-OBJECT:lookupModule:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupModule:LookupAdditionalFields = "":U.
        THIS-OBJECT:lookupModule:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:lookupModule:LookupBrowserFields = "ModuleName,ModuleDescription,ProductCode":U.
        THIS-OBJECT:lookupModule:LookupCascade = FALSE.
        THIS-OBJECT:lookupModule:LookupControls = "":U.
        THIS-OBJECT:lookupModule:LookupDialogClassName = "":U.
        THIS-OBJECT:lookupModule:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:lookupModule:LookupDialogFilterFields = "ModuleName,ModuleDescription":U.
        THIS-OBJECT:lookupModule:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupModule:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:lookupModule:LookupDialogQuerySort = "BY eSmartModule.ModuleName":U.
        THIS-OBJECT:lookupModule:LookupDialogQueryString = "":U.
        THIS-OBJECT:lookupModule:LookupEntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:lookupModule:LookupEntityTable = "eSmartModule":U.
        THIS-OBJECT:lookupModule:LookupEntityView = "":U.
        THIS-OBJECT:lookupModule:LookupFields = "":U.
        THIS-OBJECT:lookupModule:LookupKeyField = "ModuleName":U.
        THIS-OBJECT:lookupModule:LookupKeyValue = "":U.
        THIS-OBJECT:lookupModule:LookupKeyValueColumn = "ModuleGuid":U.
        THIS-OBJECT:lookupModule:LookupQuerySort = "":U.
        THIS-OBJECT:lookupModule:LookupQueryString = "FOR EACH eSmartModule WHERE eSmartModule.ModuleName BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupModule:LookupTimerDelay = 750.
        THIS-OBJECT:lookupModule:LookupTimerDisabled = FALSE.
        THIS-OBJECT:lookupModule:Name = "lookupModule":U.
        THIS-OBJECT:lookupModule:OwningGrid = ?.
        /*  */
        /* lookupDesignTimeDataSource */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupDesignTimeDataSource, "lookupDesignTimeDataSource":U).
        editorButton3:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton3, "editorButton3":U).
        THIS-OBJECT:lookupDesignTimeDataSource:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:lookupDesignTimeDataSource:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DesignTimeDataSourceGuid":U, TRUE)).
        THIS-OBJECT:lookupDesignTimeDataSource:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DesignTimeDataSourceName":U, TRUE)).
        THIS-OBJECT:lookupDesignTimeDataSource:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupAdditionalFields = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupBrowserFields = "ObjectName,ObjectDescription,ObjectTypeName,ObjectPackage":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupCascade = FALSE.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupControls = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogClassName = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogFilterFields = "ObjectName,ObjectDescription":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogQuerySort = "BY eSmartObjectMaster.ObjectName":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupDialogQueryString = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupEntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupEntityView = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupFields = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupKeyField = "ObjectName":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupKeyValue = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupKeyValueColumn = "ObjectMasterGuid":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupQuerySort = "":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupQueryString = "FOR EACH eSmartObjectMaster WHERE eSmartObjectMaster.ObjectName BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupTimerDelay = 750.
        THIS-OBJECT:lookupDesignTimeDataSource:LookupTimerDisabled = FALSE.
        THIS-OBJECT:lookupDesignTimeDataSource:Name = "lookupDesignTimeDataSource":U.
        THIS-OBJECT:lookupDesignTimeDataSource:OwningGrid = ?.
        /*  */
        /* lookupCustomizationType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupCustomizationType, "lookupCustomizationType":U).
        editorButton4:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton4, "editorButton4":U).
        THIS-OBJECT:lookupCustomizationType:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:lookupCustomizationType:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationResultGuid":U, TRUE)).
        THIS-OBJECT:lookupCustomizationType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationResultCode":U, TRUE)).
        THIS-OBJECT:lookupCustomizationType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupCustomizationType:LookupAdditionalFields = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupBrowserFields = "CustomizationResultCode,CustomizationResultDescription,CustomizationTypeCode":U.
        THIS-OBJECT:lookupCustomizationType:LookupCascade = FALSE.
        THIS-OBJECT:lookupCustomizationType:LookupControls = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupDialogClassName = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:lookupCustomizationType:LookupDialogFilterFields = "CustomizationResultCode,CustomizationResultDescription,CustomizationTypeCode":U.
        THIS-OBJECT:lookupCustomizationType:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupCustomizationType:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:lookupCustomizationType:LookupDialogQuerySort = "BY eSmartCustomizationResult.CustomizationResultCode":U.
        THIS-OBJECT:lookupCustomizationType:LookupDialogQueryString = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity":U.
        THIS-OBJECT:lookupCustomizationType:LookupEntityTable = "eSmartCustomizationResult":U.
        THIS-OBJECT:lookupCustomizationType:LookupEntityView = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupFields = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupKeyField = "CustomizationResultCode":U.
        THIS-OBJECT:lookupCustomizationType:LookupKeyValue = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupKeyValueColumn = "CustomizationResultGuid":U.
        THIS-OBJECT:lookupCustomizationType:LookupQuerySort = "":U.
        THIS-OBJECT:lookupCustomizationType:LookupQueryString = "FOR EACH eSmartCustomizationResult WHERE eSmartCustomizationResult.CustomizationResultCode BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupCustomizationType:LookupTimerDelay = 750.
        THIS-OBJECT:lookupCustomizationType:LookupTimerDisabled = FALSE.
        THIS-OBJECT:lookupCustomizationType:Name = "lookupCustomizationType":U.
        THIS-OBJECT:lookupCustomizationType:OwningGrid = ?.
        /*  */
        /* SmartObjectMasterViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lookupCustomizationType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lookupDesignTimeDataSource).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lookupModule).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lookupObjectType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_CustomizationResultGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ModuleGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectPackage_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectPackage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectExtension_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_ObjectExtension).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_StaticObject).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_TemplateObject).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_DeploymentType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_DeploymentType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_Disabled).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectMaster_DesignTimeDataSourceGuid_Label).
        THIS-OBJECT:Name = "SmartObjectMasterViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_ObjectExtension, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_StaticObject, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_TemplateObject, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_DeploymentType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_RunnableFromMenu, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectMaster_Disabled, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupObjectType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupModule, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupDesignTimeDataSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupCustomizationType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartObjectMasterViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartObjectMasterViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
