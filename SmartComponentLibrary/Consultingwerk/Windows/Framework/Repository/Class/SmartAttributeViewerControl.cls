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
    Purpose     : 
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartAttributeViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartAttribute":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
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
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = resources:GetString("smartBusinessEntityBindingSource1.BindTo":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = resources:GetString("smartBusinessEntityBindingSource1.EntityJoin":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = resources:GetString("smartBusinessEntityBindingSource1.EntityName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = resources:GetString("smartBusinessEntityBindingSource1.EntityTable":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityView = resources:GetString("smartBusinessEntityBindingSource1.EntityView":U).
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
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:AccessibleDescription = resources:GetString("eSmartAttribute_AttributeLabel_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:AccessibleName = resources:GetString("eSmartAttribute_AttributeLabel_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_AttributeLabel_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Dock = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Font = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Location = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Name = "eSmartAttribute_AttributeLabel_Label":U.
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Size = CAST(resources:GetObject("eSmartAttribute_AttributeLabel_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_AttributeLabel_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_AttributeLabel_Label:Text = resources:GetString("eSmartAttribute_AttributeLabel_Label.Text":U).
        /*  */
        /* eSmartAttribute_AttributeLabel */
        /*  */
        THIS-OBJECT:eSmartAttribute_AttributeLabel:AccessibleDescription = resources:GetString("eSmartAttribute_AttributeLabel.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:AccessibleName = resources:GetString("eSmartAttribute_AttributeLabel.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Anchor = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_AttributeLabel.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeLabel":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Dock = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Font = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:ImeMode = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Location = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:MaximumSize = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Name = "eSmartAttribute_AttributeLabel":U.
        THIS-OBJECT:eSmartAttribute_AttributeLabel:RightToLeft = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:Size = CAST(resources:GetObject("eSmartAttribute_AttributeLabel.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeLabel:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_AttributeLabel.TabIndex":U)).
        /*  */
        /* eSmartAttribute_TechnicalName_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:AccessibleDescription = resources:GetString("eSmartAttribute_TechnicalName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:AccessibleName = resources:GetString("eSmartAttribute_TechnicalName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_TechnicalName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Dock = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Font = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Location = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Name = "eSmartAttribute_TechnicalName_Label":U.
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Size = CAST(resources:GetObject("eSmartAttribute_TechnicalName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_TechnicalName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_TechnicalName_Label:Text = resources:GetString("eSmartAttribute_TechnicalName_Label.Text":U).
        /*  */
        /* eSmartAttribute_TechnicalName */
        /*  */
        THIS-OBJECT:eSmartAttribute_TechnicalName:AccessibleDescription = resources:GetString("eSmartAttribute_TechnicalName.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName:AccessibleName = resources:GetString("eSmartAttribute_TechnicalName.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_TechnicalName:Anchor = CAST(resources:GetObject("eSmartAttribute_TechnicalName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_TechnicalName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_TechnicalName.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_TechnicalName:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_TechnicalName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_TechnicalName:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_TechnicalName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_TechnicalName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TechnicalName":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_TechnicalName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_TechnicalName:Dock = CAST(resources:GetObject("eSmartAttribute_TechnicalName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_TechnicalName:Font = CAST(resources:GetObject("eSmartAttribute_TechnicalName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_TechnicalName:ImeMode = CAST(resources:GetObject("eSmartAttribute_TechnicalName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_TechnicalName:Location = CAST(resources:GetObject("eSmartAttribute_TechnicalName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_TechnicalName:MaximumSize = CAST(resources:GetObject("eSmartAttribute_TechnicalName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_TechnicalName:Name = "eSmartAttribute_TechnicalName":U.
        THIS-OBJECT:eSmartAttribute_TechnicalName:RightToLeft = CAST(resources:GetObject("eSmartAttribute_TechnicalName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_TechnicalName:Size = CAST(resources:GetObject("eSmartAttribute_TechnicalName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_TechnicalName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_TechnicalName.TabIndex":U)).
        /*  */
        /* eSmartAttribute_AttributeDesription_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:AccessibleDescription = resources:GetString("eSmartAttribute_AttributeDesription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:AccessibleName = resources:GetString("eSmartAttribute_AttributeDesription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_AttributeDesription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Dock = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Font = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Location = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Name = "eSmartAttribute_AttributeDesription_Label":U.
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Size = CAST(resources:GetObject("eSmartAttribute_AttributeDesription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_AttributeDesription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription_Label:Text = resources:GetString("eSmartAttribute_AttributeDesription_Label.Text":U).
        /*  */
        /* eSmartAttribute_AttributeDesription */
        /*  */
        THIS-OBJECT:eSmartAttribute_AttributeDesription:AccessibleDescription = resources:GetString("eSmartAttribute_AttributeDesription.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:AccessibleName = resources:GetString("eSmartAttribute_AttributeDesription.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Anchor = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_AttributeDesription.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeDesription":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Dock = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Font = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:ImeMode = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Location = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:MaximumSize = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_AttributeDesription.Multiline":U)).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Name = "eSmartAttribute_AttributeDesription":U.
        THIS-OBJECT:eSmartAttribute_AttributeDesription:RightToLeft = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:Size = CAST(resources:GetObject("eSmartAttribute_AttributeDesription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_AttributeDesription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_AttributeDesription.TabIndex":U)).
        /*  */
        /* eSmartAttribute_RuntimeOnly */
        /*  */
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:AccessibleDescription = resources:GetString("eSmartAttribute_RuntimeOnly.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:AccessibleName = resources:GetString("eSmartAttribute_RuntimeOnly.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Anchor = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_RuntimeOnly.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RuntimeOnly":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Dock = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Font = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:ImeMode = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Location = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:MaximumSize = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Name = "eSmartAttribute_RuntimeOnly":U.
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:RightToLeft = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Size = CAST(resources:GetObject("eSmartAttribute_RuntimeOnly.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_RuntimeOnly.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:Text = resources:GetString("eSmartAttribute_RuntimeOnly.Text":U).
        THIS-OBJECT:eSmartAttribute_RuntimeOnly:UseMnemonics = TRUE.
        /*  */
        /* eSmartAttribute_VirtualProperty */
        /*  */
        THIS-OBJECT:eSmartAttribute_VirtualProperty:AccessibleDescription = resources:GetString("eSmartAttribute_VirtualProperty.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:AccessibleName = resources:GetString("eSmartAttribute_VirtualProperty.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Anchor = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_VirtualProperty.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "VirtualProperty":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Dock = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Font = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:ImeMode = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Location = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:MaximumSize = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Name = "eSmartAttribute_VirtualProperty":U.
        THIS-OBJECT:eSmartAttribute_VirtualProperty:RightToLeft = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Size = CAST(resources:GetObject("eSmartAttribute_VirtualProperty.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_VirtualProperty.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:Text = resources:GetString("eSmartAttribute_VirtualProperty.Text":U).
        THIS-OBJECT:eSmartAttribute_VirtualProperty:UseMnemonics = TRUE.
        /*  */
        /* eSmartAttribute_ConstantLevel_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:AccessibleDescription = resources:GetString("eSmartAttribute_ConstantLevel_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:AccessibleName = resources:GetString("eSmartAttribute_ConstantLevel_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_ConstantLevel_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Dock = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Font = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Location = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Name = "eSmartAttribute_ConstantLevel_Label":U.
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Size = CAST(resources:GetObject("eSmartAttribute_ConstantLevel_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_ConstantLevel_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_ConstantLevel_Label:Text = resources:GetString("eSmartAttribute_ConstantLevel_Label.Text":U).
        /*  */
        /* eSmartAttribute_LookupType_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_LookupType_Label:AccessibleDescription = resources:GetString("eSmartAttribute_LookupType_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:AccessibleName = resources:GetString("eSmartAttribute_LookupType_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartAttribute_LookupType_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_LookupType_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Dock = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Font = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Location = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Name = "eSmartAttribute_LookupType_Label":U.
        THIS-OBJECT:eSmartAttribute_LookupType_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Size = CAST(resources:GetObject("eSmartAttribute_LookupType_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_LookupType_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_LookupType_Label:Text = resources:GetString("eSmartAttribute_LookupType_Label.Text":U).
        /*  */
        /* eSmartAttribute_LookupValues_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:AccessibleDescription = resources:GetString("eSmartAttribute_LookupValues_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:AccessibleName = resources:GetString("eSmartAttribute_LookupValues_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance6:TextHAlignAsString = resources:GetString("appearance6.TextHAlignAsString":U).
        appearance6:TextVAlignAsString = resources:GetString("appearance6.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Appearance = appearance6.
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_LookupValues_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Dock = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Font = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Location = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Name = "eSmartAttribute_LookupValues_Label":U.
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Size = CAST(resources:GetObject("eSmartAttribute_LookupValues_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_LookupValues_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_LookupValues_Label:Text = resources:GetString("eSmartAttribute_LookupValues_Label.Text":U).
        /*  */
        /* eSmartAttribute_LookupValues */
        /*  */
        THIS-OBJECT:eSmartAttribute_LookupValues:AccessibleDescription = resources:GetString("eSmartAttribute_LookupValues.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_LookupValues:AccessibleName = resources:GetString("eSmartAttribute_LookupValues.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_LookupValues:Anchor = CAST(resources:GetObject("eSmartAttribute_LookupValues.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_LookupValues:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_LookupValues.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_LookupValues:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_LookupValues.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_LookupValues:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_LookupValues.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_LookupValues:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LookupValues":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_LookupValues:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_LookupValues:Dock = CAST(resources:GetObject("eSmartAttribute_LookupValues.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_LookupValues:Font = CAST(resources:GetObject("eSmartAttribute_LookupValues.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_LookupValues:ImeMode = CAST(resources:GetObject("eSmartAttribute_LookupValues.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_LookupValues:Location = CAST(resources:GetObject("eSmartAttribute_LookupValues.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_LookupValues:MaximumSize = CAST(resources:GetObject("eSmartAttribute_LookupValues.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupValues:Name = "eSmartAttribute_LookupValues":U.
        THIS-OBJECT:eSmartAttribute_LookupValues:RightToLeft = CAST(resources:GetObject("eSmartAttribute_LookupValues.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_LookupValues:Size = CAST(resources:GetObject("eSmartAttribute_LookupValues.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_LookupValues:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_LookupValues.TabIndex":U)).
        /*  */
        /* eSmartAttribute_PropertyType_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:AccessibleDescription = resources:GetString("eSmartAttribute_PropertyType_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:AccessibleName = resources:GetString("eSmartAttribute_PropertyType_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance7:TextHAlignAsString = resources:GetString("appearance7.TextHAlignAsString":U).
        appearance7:TextVAlignAsString = resources:GetString("appearance7.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Appearance = appearance7.
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_PropertyType_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Dock = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Font = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Location = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Name = "eSmartAttribute_PropertyType_Label":U.
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Size = CAST(resources:GetObject("eSmartAttribute_PropertyType_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_PropertyType_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_PropertyType_Label:Text = resources:GetString("eSmartAttribute_PropertyType_Label.Text":U).
        /*  */
        /* eSmartAttribute_PropertyType */
        /*  */
        THIS-OBJECT:eSmartAttribute_PropertyType:AccessibleDescription = resources:GetString("eSmartAttribute_PropertyType.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_PropertyType:AccessibleName = resources:GetString("eSmartAttribute_PropertyType.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_PropertyType:Anchor = CAST(resources:GetObject("eSmartAttribute_PropertyType.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_PropertyType:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_PropertyType.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_PropertyType:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_PropertyType.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_PropertyType:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_PropertyType.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_PropertyType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PropertyType":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_PropertyType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_PropertyType:Dock = CAST(resources:GetObject("eSmartAttribute_PropertyType.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_PropertyType:Font = CAST(resources:GetObject("eSmartAttribute_PropertyType.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_PropertyType:ImeMode = CAST(resources:GetObject("eSmartAttribute_PropertyType.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_PropertyType:Location = CAST(resources:GetObject("eSmartAttribute_PropertyType.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_PropertyType:MaximumSize = CAST(resources:GetObject("eSmartAttribute_PropertyType.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_PropertyType:Name = "eSmartAttribute_PropertyType":U.
        THIS-OBJECT:eSmartAttribute_PropertyType:RightToLeft = CAST(resources:GetObject("eSmartAttribute_PropertyType.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_PropertyType:Size = CAST(resources:GetObject("eSmartAttribute_PropertyType.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_PropertyType:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_PropertyType.TabIndex":U)).
        /*  */
        /* eSmartAttribute_RepositoryType_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:AccessibleDescription = resources:GetString("eSmartAttribute_RepositoryType_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:AccessibleName = resources:GetString("eSmartAttribute_RepositoryType_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance8:TextHAlignAsString = resources:GetString("appearance8.TextHAlignAsString":U).
        appearance8:TextVAlignAsString = resources:GetString("appearance8.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Appearance = appearance8.
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_RepositoryType_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Dock = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Font = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Location = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Name = "eSmartAttribute_RepositoryType_Label":U.
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Size = CAST(resources:GetObject("eSmartAttribute_RepositoryType_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_RepositoryType_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_RepositoryType_Label:Text = resources:GetString("eSmartAttribute_RepositoryType_Label.Text":U).
        /*  */
        /* eSmartAttribute_SetServiceType_Label */
        /*  */
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:AccessibleDescription = resources:GetString("eSmartAttribute_SetServiceType_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:AccessibleName = resources:GetString("eSmartAttribute_SetServiceType_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Anchor = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance9:TextHAlignAsString = resources:GetString("appearance9.TextHAlignAsString":U).
        appearance9:TextVAlignAsString = resources:GetString("appearance9.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Appearance = appearance9.
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_SetServiceType_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Dock = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Font = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:ImeMode = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Location = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:MaximumSize = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Name = "eSmartAttribute_SetServiceType_Label":U.
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:RightToLeft = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Size = CAST(resources:GetObject("eSmartAttribute_SetServiceType_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_SetServiceType_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttribute_SetServiceType_Label:Text = resources:GetString("eSmartAttribute_SetServiceType_Label.Text":U).
        /*  */
        /* eSmartAttribute_SetServiceType */
        /*  */
        THIS-OBJECT:eSmartAttribute_SetServiceType:AccessibleDescription = resources:GetString("eSmartAttribute_SetServiceType.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType:AccessibleName = resources:GetString("eSmartAttribute_SetServiceType.AccessibleName":U).
        THIS-OBJECT:eSmartAttribute_SetServiceType:Anchor = CAST(resources:GetObject("eSmartAttribute_SetServiceType.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttribute_SetServiceType:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttribute_SetServiceType.AutoSize":U)).
        THIS-OBJECT:eSmartAttribute_SetServiceType:BackgroundImage = CAST(resources:GetObject("eSmartAttribute_SetServiceType.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttribute_SetServiceType:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttribute_SetServiceType.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttribute_SetServiceType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SetServiceType":U, TRUE)).
        THIS-OBJECT:eSmartAttribute_SetServiceType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttribute_SetServiceType:Dock = CAST(resources:GetObject("eSmartAttribute_SetServiceType.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttribute_SetServiceType:Font = CAST(resources:GetObject("eSmartAttribute_SetServiceType.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttribute_SetServiceType:ImeMode = CAST(resources:GetObject("eSmartAttribute_SetServiceType.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttribute_SetServiceType:Location = CAST(resources:GetObject("eSmartAttribute_SetServiceType.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttribute_SetServiceType:MaximumSize = CAST(resources:GetObject("eSmartAttribute_SetServiceType.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_SetServiceType:Name = "eSmartAttribute_SetServiceType":U.
        THIS-OBJECT:eSmartAttribute_SetServiceType:RightToLeft = CAST(resources:GetObject("eSmartAttribute_SetServiceType.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttribute_SetServiceType:Size = CAST(resources:GetObject("eSmartAttribute_SetServiceType.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttribute_SetServiceType:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttribute_SetServiceType.TabIndex":U)).
        /*  */
        /* ultraComboEditor_ConstantLevel */
        /*  */
        THIS-OBJECT:ultraComboEditor_ConstantLevel:AccessibleDescription = resources:GetString("ultraComboEditor_ConstantLevel.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:AccessibleName = resources:GetString("ultraComboEditor_ConstantLevel.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Anchor = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor_ConstantLevel.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:BackgroundImage = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ConstantLevel":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Dock = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Font = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:ImeMode = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Location = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:MaximumSize = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Name = "ultraComboEditor_ConstantLevel":U.
        THIS-OBJECT:ultraComboEditor_ConstantLevel:RightToLeft = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:Size = CAST(resources:GetObject("ultraComboEditor_ConstantLevel.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_ConstantLevel:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor_ConstantLevel.TabIndex":U)).
        /*  */
        /* ultraComboEditor_LookupType */
        /*  */
        THIS-OBJECT:ultraComboEditor_LookupType:AccessibleDescription = resources:GetString("ultraComboEditor_LookupType.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor_LookupType:AccessibleName = resources:GetString("ultraComboEditor_LookupType.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor_LookupType:Anchor = CAST(resources:GetObject("ultraComboEditor_LookupType.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor_LookupType:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor_LookupType.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor_LookupType:BackgroundImage = CAST(resources:GetObject("ultraComboEditor_LookupType.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor_LookupType:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor_LookupType.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor_LookupType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LookupType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_LookupType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_LookupType:Dock = CAST(resources:GetObject("ultraComboEditor_LookupType.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor_LookupType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_LookupType:Font = CAST(resources:GetObject("ultraComboEditor_LookupType.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor_LookupType:ImeMode = CAST(resources:GetObject("ultraComboEditor_LookupType.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor_LookupType:Location = CAST(resources:GetObject("ultraComboEditor_LookupType.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor_LookupType:MaximumSize = CAST(resources:GetObject("ultraComboEditor_LookupType.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_LookupType:Name = "ultraComboEditor_LookupType":U.
        THIS-OBJECT:ultraComboEditor_LookupType:RightToLeft = CAST(resources:GetObject("ultraComboEditor_LookupType.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor_LookupType:Size = CAST(resources:GetObject("ultraComboEditor_LookupType.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_LookupType:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor_LookupType.TabIndex":U)).
        /*  */
        /* ultraLabelAttributeGroup */
        /*  */
        THIS-OBJECT:ultraLabelAttributeGroup:AccessibleDescription = resources:GetString("ultraLabelAttributeGroup.AccessibleDescription":U).
        THIS-OBJECT:ultraLabelAttributeGroup:AccessibleName = resources:GetString("ultraLabelAttributeGroup.AccessibleName":U).
        THIS-OBJECT:ultraLabelAttributeGroup:Anchor = CAST(resources:GetObject("ultraLabelAttributeGroup.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance10:TextHAlignAsString = resources:GetString("appearance10.TextHAlignAsString":U).
        appearance10:TextVAlignAsString = resources:GetString("appearance10.TextVAlignAsString":U).
        THIS-OBJECT:ultraLabelAttributeGroup:Appearance = appearance10.
        THIS-OBJECT:ultraLabelAttributeGroup:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraLabelAttributeGroup.AutoSize":U)).
        THIS-OBJECT:ultraLabelAttributeGroup:BackgroundImage = CAST(resources:GetObject("ultraLabelAttributeGroup.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraLabelAttributeGroup:BackgroundImageLayout = CAST(resources:GetObject("ultraLabelAttributeGroup.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraLabelAttributeGroup:Dock = CAST(resources:GetObject("ultraLabelAttributeGroup.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraLabelAttributeGroup:Font = CAST(resources:GetObject("ultraLabelAttributeGroup.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraLabelAttributeGroup:ImeMode = CAST(resources:GetObject("ultraLabelAttributeGroup.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraLabelAttributeGroup:Location = CAST(resources:GetObject("ultraLabelAttributeGroup.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraLabelAttributeGroup:MaximumSize = CAST(resources:GetObject("ultraLabelAttributeGroup.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabelAttributeGroup:Name = "ultraLabelAttributeGroup":U.
        THIS-OBJECT:ultraLabelAttributeGroup:RightToLeft = CAST(resources:GetObject("ultraLabelAttributeGroup.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraLabelAttributeGroup:Size = CAST(resources:GetObject("ultraLabelAttributeGroup.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabelAttributeGroup:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraLabelAttributeGroup.TabIndex":U)).
        THIS-OBJECT:ultraLabelAttributeGroup:Text = resources:GetString("ultraLabelAttributeGroup.Text":U).
        /*  */
        /* smartComboEditor1 */
        /*  */
        THIS-OBJECT:smartComboEditor1:AccessibleDescription = resources:GetString("smartComboEditor1.AccessibleDescription":U).
        THIS-OBJECT:smartComboEditor1:AccessibleName = resources:GetString("smartComboEditor1.AccessibleName":U).
        THIS-OBJECT:smartComboEditor1:Anchor = CAST(resources:GetObject("smartComboEditor1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartComboEditor1:AppServerPartition = resources:GetString("smartComboEditor1.AppServerPartition":U).
        THIS-OBJECT:smartComboEditor1:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartComboEditor1.AutoSize":U)).
        THIS-OBJECT:smartComboEditor1:BackgroundImage = CAST(resources:GetObject("smartComboEditor1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartComboEditor1:BackgroundImageLayout = CAST(resources:GetObject("smartComboEditor1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeGroupGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DisplayFields = resources:GetString("smartComboEditor1.DisplayFields":U).
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DisplaySubstitute = resources:GetString("smartComboEditor1.DisplaySubstitute":U).
        THIS-OBJECT:smartComboEditor1:Dock = CAST(resources:GetObject("smartComboEditor1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:EntityName = resources:GetString("smartComboEditor1.EntityName":U).
        THIS-OBJECT:smartComboEditor1:EntityTable = resources:GetString("smartComboEditor1.EntityTable":U).
        THIS-OBJECT:smartComboEditor1:FlagText = resources:GetString("smartComboEditor1.FlagText":U).
        THIS-OBJECT:smartComboEditor1:FlagValue = resources:GetString("smartComboEditor1.FlagValue":U).
        THIS-OBJECT:smartComboEditor1:Font = CAST(resources:GetObject("smartComboEditor1.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartComboEditor1:ImageField = resources:GetString("smartComboEditor1.ImageField":U).
        THIS-OBJECT:smartComboEditor1:ImageKeyField = resources:GetString("smartComboEditor1.ImageKeyField":U).
        THIS-OBJECT:smartComboEditor1:ImageKeyFieldSubstitute = resources:GetString("smartComboEditor1.ImageKeyFieldSubstitute":U).
        THIS-OBJECT:smartComboEditor1:ImeMode = CAST(resources:GetObject("smartComboEditor1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartComboEditor1:KeyField = resources:GetString("smartComboEditor1.KeyField":U).
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Location = CAST(resources:GetObject("smartComboEditor1.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartComboEditor1:MaximumSize = CAST(resources:GetObject("smartComboEditor1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        THIS-OBJECT:smartComboEditor1:QueryString = resources:GetString("smartComboEditor1.QueryString":U).
        THIS-OBJECT:smartComboEditor1:RightToLeft = CAST(resources:GetObject("smartComboEditor1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartComboEditor1:Size = CAST(resources:GetObject("smartComboEditor1.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartComboEditor1:TabIndex = System.Convert:ToInt32(resources:GetObject("smartComboEditor1.TabIndex":U)).
        /*  */
        /* ultraOptionSet1 */
        /*  */
        THIS-OBJECT:ultraOptionSet1:AccessibleDescription = resources:GetString("ultraOptionSet1.AccessibleDescription":U).
        THIS-OBJECT:ultraOptionSet1:AccessibleName = resources:GetString("ultraOptionSet1.AccessibleName":U).
        THIS-OBJECT:ultraOptionSet1:Anchor = CAST(resources:GetObject("ultraOptionSet1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraOptionSet1:BackgroundImage = CAST(resources:GetObject("ultraOptionSet1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraOptionSet1:BackgroundImageLayout = CAST(resources:GetObject("ultraOptionSet1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraOptionSet1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraOptionSet1:CheckedIndex = 0.
        THIS-OBJECT:ultraOptionSet1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PropertyOrEvent":U, TRUE)).
        THIS-OBJECT:ultraOptionSet1:Dock = CAST(resources:GetObject("ultraOptionSet1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraOptionSet1:Font = CAST(resources:GetObject("ultraOptionSet1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraOptionSet1:ImeMode = CAST(resources:GetObject("ultraOptionSet1.ImeMode":U), System.Windows.Forms.ImeMode).
        valueListItem1:DataValue = TRUE.
        valueListItem1:DisplayText = resources:GetString("valueListItem1.DisplayText":U).
        valueListItem2:DataValue = FALSE.
        valueListItem2:DisplayText = resources:GetString("valueListItem2.DisplayText":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        THIS-OBJECT:ultraOptionSet1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraOptionSet1:Location = CAST(resources:GetObject("ultraOptionSet1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraOptionSet1:MaximumSize = CAST(resources:GetObject("ultraOptionSet1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraOptionSet1:Name = "ultraOptionSet1":U.
        THIS-OBJECT:ultraOptionSet1:RightToLeft = CAST(resources:GetObject("ultraOptionSet1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraOptionSet1:Size = CAST(resources:GetObject("ultraOptionSet1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraOptionSet1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraOptionSet1.TabIndex":U)).
        THIS-OBJECT:ultraOptionSet1:Text = resources:GetString("ultraOptionSet1.Text":U).
        /*  */
        /* ultraComboEditor_RepositoryType */
        /*  */
        THIS-OBJECT:ultraComboEditor_RepositoryType:AccessibleDescription = resources:GetString("ultraComboEditor_RepositoryType.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor_RepositoryType:AccessibleName = resources:GetString("ultraComboEditor_RepositoryType.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor_RepositoryType:Anchor = CAST(resources:GetObject("ultraComboEditor_RepositoryType.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor_RepositoryType:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor_RepositoryType.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor_RepositoryType:BackgroundImage = CAST(resources:GetObject("ultraComboEditor_RepositoryType.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor_RepositoryType:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor_RepositoryType.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor_RepositoryType:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RepositoryType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor_RepositoryType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor_RepositoryType:Dock = CAST(resources:GetObject("ultraComboEditor_RepositoryType.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor_RepositoryType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor_RepositoryType:Font = CAST(resources:GetObject("ultraComboEditor_RepositoryType.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor_RepositoryType:ImeMode = CAST(resources:GetObject("ultraComboEditor_RepositoryType.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor_RepositoryType:Location = CAST(resources:GetObject("ultraComboEditor_RepositoryType.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor_RepositoryType:MaximumSize = CAST(resources:GetObject("ultraComboEditor_RepositoryType.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_RepositoryType:Name = "ultraComboEditor_RepositoryType":U.
        THIS-OBJECT:ultraComboEditor_RepositoryType:RightToLeft = CAST(resources:GetObject("ultraComboEditor_RepositoryType.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor_RepositoryType:Size = CAST(resources:GetObject("ultraComboEditor_RepositoryType.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor_RepositoryType:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor_RepositoryType.TabIndex":U)).
        /*  */
        /* SmartAttributeViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
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
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartAttributeViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
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
		
		IF THIS-OBJECT:DesignTime THEN RETURN.
		
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
