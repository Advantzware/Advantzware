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
    File        : SmartAttributeViewerControl
    Purpose     : Viewer for maintenance of SmartAttribute records
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 11:42:55
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Framework.*               FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartAttributeViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components                                AS System.ComponentModel.IContainer                                               NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1         AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_AttributeLabel_Label      AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_AttributeLabel            AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_TechnicalName_Label       AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_TechnicalName             AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_AttributeDesription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_AttributeDesription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_RuntimeOnly               AS Infragistics.Win.UltraWinEditors.UltraCheckEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_VirtualProperty           AS Infragistics.Win.UltraWinEditors.UltraCheckEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_ConstantLevel_Label       AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_LookupType_Label          AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_LookupValues_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_LookupValues              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_PropertyType_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_PropertyType              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_RepositoryType_Label      AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_SetServiceType_Label      AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttribute_SetServiceType            AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartComboEditor1                         AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor                 NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor_LookupType               AS Infragistics.Win.UltraWinEditors.UltraComboEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor_ConstantLevel            AS Infragistics.Win.UltraWinEditors.UltraComboEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor_RepositoryType           AS Infragistics.Win.UltraWinEditors.UltraComboEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabelAttributeGroup                  AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraOptionSet1                           AS Infragistics.Win.UltraWinEditors.UltraOptionSet                                NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartAttributeViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartAttributeViewerControl ():

        SUPER().
        InitializeComponent ().

        PopulateComboBoxes ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initialize the newly added record
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID AddRecord ():

        DEFINE VARIABLE oValueListItem AS Infragistics.Win.ValueListItem NO-UNDO.

        SUPER:AddRecord ().

        IF THIS-OBJECT:smartComboEditor1:Items:Count > 0 THEN
            THIS-OBJECT:smartComboEditor1:SelectedIndex = 0.

        oValueListItem = THIS-OBJECT:ultraComboEditor_ConstantLevel:ValueList:FindByDataValue ("":U).
        IF VALID-OBJECT (oValueListItem) THEN
            THIS-OBJECT:ultraComboEditor_ConstantLevel:ValueList:SelectedIndex = oValueListItem:ListIndex.

        oValueListItem = THIS-OBJECT:ultraComboEditor_LookupType:ValueList:FindByDataValue ("":U).
        IF VALID-OBJECT (oValueListItem) THEN
            THIS-OBJECT:ultraComboEditor_LookupType:ValueList:SelectedIndex = oValueListItem:ListIndex.

        IF THIS-OBJECT:ultraComboEditor_RepositoryType:Items:Count > 0 THEN
            THIS-OBJECT:ultraComboEditor_RepositoryType:SelectedIndex = 0.

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartAttribute":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartAttributeViewerControl":U).
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
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_AttributeLabel = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_TechnicalName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_AttributeDesription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttribute_RuntimeOnly = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartAttribute_VirtualProperty = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_LookupType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_LookupValues_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_LookupValues = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttribute_PropertyType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_PropertyType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttribute_SetServiceType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraComboEditor_ConstantLevel = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraComboEditor_LookupType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabelAttributeGroup = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:ultraOptionSet1 = NEW Infragistics.Win.UltraWinEditors.UltraOptionSet().
        THIS-OBJECT:ultraComboEditor_RepositoryType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_AttributeLabel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_TechnicalName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_AttributeDesription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_RuntimeOnly, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_VirtualProperty, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_LookupValues, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_PropertyType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttribute_SetServiceType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor_ConstantLevel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor_LookupType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraOptionSet1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor_RepositoryType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.AttributeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartAttribute":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 15 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("AttributeGuid":U, "AttributeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("AttributeGroupGuid":U, "AttributeGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("AttributeLabel":U, "Attribute Label":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("TechnicalName":U, "Technical Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("AttributeDesription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("RuntimeOnly":U, "Runtime Only":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("VirtualProperty":U, "Virtual":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("ConstantLevel":U, "Constant Level":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("LookupType":U, "Lookup Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("LookupValues":U, "Lookup Values":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("PropertyType":U, "Property Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("RepositoryType":U, "Repository Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("SetServiceType":U, "SET Service Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("PropertyOrEvent":U, "PropertyOrEvent":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("AttributeGroupName":U, "Attribute Group":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartAttribute_AttributeLabel_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_AttributeLabel_Label, "eSmartAttribute_AttributeLabel_Label":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Name = "eSmartAttribute_AttributeLabel_Label":U.
        /*  */
        /* eSmartAttribute_AttributeLabel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_AttributeLabel, "eSmartAttribute_AttributeLabel":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeLabel":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Name = "eSmartAttribute_AttributeLabel":U.
        /*  */
        /* eSmartAttribute_TechnicalName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_TechnicalName_Label, "eSmartAttribute_TechnicalName_Label":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Name = "eSmartAttribute_TechnicalName_Label":U.
        /*  */
        /* eSmartAttribute_TechnicalName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_TechnicalName, "eSmartAttribute_TechnicalName":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TechnicalName":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_TechnicalName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_TechnicalName:Name = "eSmartAttribute_TechnicalName":U.
        /*  */
        /* eSmartAttribute_AttributeDesription_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_AttributeDesription_Label, "eSmartAttribute_AttributeDesription_Label":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Name = "eSmartAttribute_AttributeDesription_Label":U.
        /*  */
        /* eSmartAttribute_AttributeDesription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_AttributeDesription, "eSmartAttribute_AttributeDesription":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeDesription":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Name = "eSmartAttribute_AttributeDesription":U.
        /*  */
        /* eSmartAttribute_RuntimeOnly */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_RuntimeOnly, "eSmartAttribute_RuntimeOnly":U).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RuntimeOnly":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Name = "eSmartAttribute_RuntimeOnly":U.
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:UseMnemonics = TRUE.
        /*  */
        /* eSmartAttribute_VirtualProperty */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_VirtualProperty, "eSmartAttribute_VirtualProperty":U).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "VirtualProperty":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Name = "eSmartAttribute_VirtualProperty":U.
        THIS-OBJECT:eSmartAttribute_VirtualProperty:UseMnemonics = TRUE.
        /*  */
        /* eSmartAttribute_ConstantLevel_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_ConstantLevel_Label, "eSmartAttribute_ConstantLevel_Label":U).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Name = "eSmartAttribute_ConstantLevel_Label":U.
        /*  */
        /* eSmartAttribute_LookupType_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_LookupType_Label, "eSmartAttribute_LookupType_Label":U).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Name = "eSmartAttribute_LookupType_Label":U.
        /*  */
        /* eSmartAttribute_LookupValues_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_LookupValues_Label, "eSmartAttribute_LookupValues_Label":U).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Name = "eSmartAttribute_LookupValues_Label":U.
        /*  */
        /* eSmartAttribute_LookupValues */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_LookupValues, "eSmartAttribute_LookupValues":U).
        THIS-OBJECT:eSmartAttribute_LookupValues:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LookupValues":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_LookupValues:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_LookupValues:Name = "eSmartAttribute_LookupValues":U.
        /*  */
        /* eSmartAttribute_PropertyType_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_PropertyType_Label, "eSmartAttribute_PropertyType_Label":U).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Name = "eSmartAttribute_PropertyType_Label":U.
        /*  */
        /* eSmartAttribute_PropertyType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_PropertyType, "eSmartAttribute_PropertyType":U).
        THIS-OBJECT:eSmartAttribute_PropertyType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PropertyType":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_PropertyType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_PropertyType:Name = "eSmartAttribute_PropertyType":U.
        /*  */
        /* eSmartAttribute_RepositoryType_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_RepositoryType_Label, "eSmartAttribute_RepositoryType_Label":U).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Name = "eSmartAttribute_RepositoryType_Label":U.
        /*  */
        /* eSmartAttribute_SetServiceType_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_SetServiceType_Label, "eSmartAttribute_SetServiceType_Label":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Name = "eSmartAttribute_SetServiceType_Label":U.
        /*  */
        /* eSmartAttribute_SetServiceType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartAttribute_SetServiceType, "eSmartAttribute_SetServiceType":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SetServiceType":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_SetServiceType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_SetServiceType:Name = "eSmartAttribute_SetServiceType":U.
        /*  */
        /* ultraComboEditor_ConstantLevel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor_ConstantLevel, "ultraComboEditor_ConstantLevel":U).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ConstantLevel":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Name = "ultraComboEditor_ConstantLevel":U.
        /*  */
        /* ultraComboEditor_LookupType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor_LookupType, "ultraComboEditor_LookupType":U).
        THIS-OBJECT:ultraComboEditor_LookupType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LookupType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_LookupType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_LookupType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_LookupType:Name = "ultraComboEditor_LookupType":U.
        /*  */
        /* ultraLabelAttributeGroup */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabelAttributeGroup:Appearance = appearance10.
        resources:ApplyResources(THIS-OBJECT:ultraLabelAttributeGroup, "ultraLabelAttributeGroup":U).
        THIS-OBJECT:ultraLabelAttributeGroup:Name = "ultraLabelAttributeGroup":U.
        /*  */
        /* smartComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartComboEditor1, "smartComboEditor1":U).
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeGroupGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        /*  */
        /* ultraOptionSet1 */
        /*  */
        THIS-OBJECT:ultraOptionSet1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraOptionSet1:CheckedIndex = 0.
        THIS-OBJECT:ultraOptionSet1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PropertyOrEvent":U, TRUE)).
        valueListItem1:DataValue = TRUE.
        resources:ApplyResources(valueListItem1, "valueListItem1":U).
        valueListItem1:ForceApplyResources = "":U.
        valueListItem2:DataValue = FALSE.
        resources:ApplyResources(valueListItem2, "valueListItem2":U).
        valueListItem2:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        THIS-OBJECT:ultraOptionSet1:Items:AddRange(arrayvar1).
        resources:ApplyResources(THIS-OBJECT:ultraOptionSet1, "ultraOptionSet1":U).
        THIS-OBJECT:ultraOptionSet1:Name = "ultraOptionSet1":U.
        THIS-OBJECT:ultraOptionSet1:ValueChanged:Subscribe(THIS-OBJECT:ultraOptionSet1_ValueChanged).
        /*  */
        /* ultraComboEditor_RepositoryType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor_RepositoryType, "ultraComboEditor_RepositoryType":U).
        THIS-OBJECT:ultraComboEditor_RepositoryType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RepositoryType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_RepositoryType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_RepositoryType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_RepositoryType:Name = "ultraComboEditor_RepositoryType":U.
        /*  */
        /* SmartAttributeViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor_RepositoryType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraOptionSet1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabelAttributeGroup).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor_LookupType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor_ConstantLevel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_AttributeLabel_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_AttributeLabel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_TechnicalName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_TechnicalName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_AttributeDesription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_AttributeDesription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_RuntimeOnly).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_VirtualProperty).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_ConstantLevel_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_LookupType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_LookupValues_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_LookupValues).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_PropertyType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_PropertyType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_RepositoryType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_SetServiceType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttribute_SetServiceType).
        THIS-OBJECT:Name = "SmartAttributeViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_AttributeLabel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_TechnicalName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_AttributeDesription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_RuntimeOnly, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_VirtualProperty, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_LookupValues, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_PropertyType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttribute_SetServiceType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor_ConstantLevel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor_LookupType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraOptionSet1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor_RepositoryType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fill the comboboxes of this viewer with values
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID PopulateComboBoxes ():

        DEFINE VARIABLE oValueListProvider AS IValueListProvider NO-UNDO.

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        oValueListProvider = {Consultingwerk/get-service.i
                              Consultingwerk.Windows.Framework.IValueListProvider
                              "NEW Consultingwerk.Windows.Framework.System.ValueListProvider ()"}.

        THIS-OBJECT:ultraComboEditor_ConstantLevel:ValueList = oValueListProvider:ValueListFromKey ("RyConstantLevel":U).

        THIS-OBJECT:ultraComboEditor_LookupType:ValueList = oValueListProvider:ValueListFromKey ("RyLookupType":U).

        THIS-OBJECT:ultraComboEditor_RepositoryType:ValueList = oValueListProvider:ValueListFromKey ("RyRepositoryType":U).

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /**
     * Purpose: Event handler for the ValueChanged event of the ultraOptionSet1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraOptionSet1_ValueChanged (sender AS System.Object,
                                                      e AS System.EventArgs):

        IF ultraOptionSet1:Enabled = FALSE THEN RETURN .

        IF UNBOX (ultraOptionSet1:Value) = FALSE THEN DO:

            IF eSmartAttribute_PropertyType:Enabled THEN
                eSmartAttribute_PropertyType:Text = "CHARACTER":U .
            IF ultraComboEditor_RepositoryType:Enabled THEN
                ultraComboEditor_RepositoryType:Value = "CHARACTER":U .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartAttributeViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartAttributeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
