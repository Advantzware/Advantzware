/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartObjectTypeViewerControl
    Purpose     : SmartViewer for SmartObjectType Maintenance
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06.10.2015 13:00:57
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTypeViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE eSmartObjectType_ExtendsObjectTypeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ObjectTypeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ObjectTypeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ObjectTypeDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ObjectTypeDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_DeploymentType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_DeploymentType AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_StaticObject AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ExtendsObjectTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_CustomObjectTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_TechnicalClassName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_TechnicalClassName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ClassTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_ContainerType AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectType_StoreInstances_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectTypeViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
        
        THIS-OBJECT:SetControlEnabled (eSmartObjectType_ObjectTypeName, 
                                       ControlEnabledEnum:Add) .
                
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Enables controls of the SmartViewerControl
        Notes:   Conditionally enabled the Store Instances combo box   
        @param pcFields The EnableFieldsEnum value indicating if the fields are enabled for Add or Update 
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID EnableFields (INPUT pcFields AS CHARACTER):
		
		SUPER:EnableFields(INPUT pcFields).

        THIS-OBJECT:VerifyStoreInstances () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the eSmartObjectType_ContainerType
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID eSmartObjectType_ContainerType_CheckedChanged (sender AS System.Object, 
                                                                       e AS System.EventArgs):

        DEFINE VARIABLE cStackTrace AS CHARACTER NO-UNDO.
		
		THIS-OBJECT:VerifyStoreInstances () .

        IF NOT BindingSourceHelper:IsBindingChangingValue() THEN DO:
            
            IF eSmartObjectType_ContainerType:Checked THEN 
                ASSIGN ultraComboEditor1:Value = ultraComboEditor1:ValueList:ValueListItems[1]:DataValue .
            ELSE 
                ASSIGN ultraComboEditor1:Value = ultraComboEditor1:ValueList:ValueListItems[0]:DataValue .
        END.
            
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartObjectType":U).
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_ObjectTypeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_DeploymentType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectType_StaticObject = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_TechnicalClassName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_ContainerType = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityLookup2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_ObjectTypeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_ObjectTypeDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_DeploymentType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_StaticObject, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_TechnicalClassName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_ContainerType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartObjectType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 14 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ObjectTypeName":U, "Object Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ObjectTypeDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("DeploymentType":U, "Deployment Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("StaticObject":U, "Static Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("ExtendsObjectTypeGuid":U, "Extends Object Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ExtendsObjectTypeName":U, "Extends Object Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("CustomObjectTypeGuid":U, "Customized by Object Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("CustomObjectTypeName":U, "Custom Object Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("TechnicalClassName":U, "Technical Class Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("ClassTypeGuid":U, "Class Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("ClassTypeName":U, "Class Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("ContainerType":U, "Container":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("StoreInstances":U, "Store instances for":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartObjectType_ObjectTypeName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:Location = NEW System.Drawing.Point(20, 24).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:Name = "eSmartObjectType_ObjectTypeName_Label":U.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:Size = NEW System.Drawing.Size(68, 14).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:TabIndex = 0.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label:Text = "Object Type:":U.
        /*  */
        /* eSmartObjectType_ObjectTypeName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectTypeName":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:Location = NEW System.Drawing.Point(172, 20).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:Name = "eSmartObjectType_ObjectTypeName":U.
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:Size = NEW System.Drawing.Size(163, 21).
        THIS-OBJECT:eSmartObjectType_ObjectTypeName:TabIndex = 1.
        /*  */
        /* eSmartObjectType_ObjectTypeDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:Location = NEW System.Drawing.Point(20, 50).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:Name = "eSmartObjectType_ObjectTypeDescription_Label":U.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:TabIndex = 4.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label:Text = "Description:":U.
        /*  */
        /* eSmartObjectType_ObjectTypeDescription */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectTypeDescription":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:Location = NEW System.Drawing.Point(172, 46).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:Name = "eSmartObjectType_ObjectTypeDescription":U.
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:eSmartObjectType_ObjectTypeDescription:TabIndex = 5.
        /*  */
        /* eSmartObjectType_DeploymentType_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:Location = NEW System.Drawing.Point(20, 76).
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:Name = "eSmartObjectType_DeploymentType_Label":U.
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:Size = NEW System.Drawing.Size(96, 14).
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:TabIndex = 6.
        THIS-OBJECT:eSmartObjectType_DeploymentType_Label:Text = "Deployment Type:":U.
        /*  */
        /* eSmartObjectType_DeploymentType */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_DeploymentType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_DeploymentType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DeploymentType":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_DeploymentType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectType_DeploymentType:Location = NEW System.Drawing.Point(172, 72).
        THIS-OBJECT:eSmartObjectType_DeploymentType:Name = "eSmartObjectType_DeploymentType":U.
        THIS-OBJECT:eSmartObjectType_DeploymentType:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:eSmartObjectType_DeploymentType:TabIndex = 7.
        /*  */
        /* eSmartObjectType_StaticObject */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:eSmartObjectType_StaticObject:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_StaticObject:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_StaticObject:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "StaticObject":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_StaticObject:Location = NEW System.Drawing.Point(341, 23).
        THIS-OBJECT:eSmartObjectType_StaticObject:Name = "eSmartObjectType_StaticObject":U.
        THIS-OBJECT:eSmartObjectType_StaticObject:Size = NEW System.Drawing.Size(85, 17).
        THIS-OBJECT:eSmartObjectType_StaticObject:TabIndex = 2.
        THIS-OBJECT:eSmartObjectType_StaticObject:Text = "Static Object":U.
        THIS-OBJECT:eSmartObjectType_StaticObject:UseMnemonics = TRUE.
        /*  */
        /* eSmartObjectType_ExtendsObjectTypeGuid_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:Location = NEW System.Drawing.Point(20, 103).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:Name = "eSmartObjectType_ExtendsObjectTypeGuid_Label":U.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:Size = NEW System.Drawing.Size(112, 14).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:TabIndex = 8.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label:Text = "Extends Object Type:":U.
        /*  */
        /* eSmartObjectType_CustomObjectTypeGuid_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:Location = NEW System.Drawing.Point(20, 129).
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:Name = "eSmartObjectType_CustomObjectTypeGuid_Label":U.
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:Size = NEW System.Drawing.Size(146, 14).
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:TabIndex = 10.
        THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label:Text = "Customized by Object Type:":U.
        /*  */
        /* eSmartObjectType_TechnicalClassName_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:Appearance = appearance6.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:Location = NEW System.Drawing.Point(20, 184).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:Name = "eSmartObjectType_TechnicalClassName_Label":U.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:Size = NEW System.Drawing.Size(121, 14).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:TabIndex = 14.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label:Text = "Technical Class Name:":U.
        /*  */
        /* eSmartObjectType_TechnicalClassName */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TechnicalClassName":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:Location = NEW System.Drawing.Point(172, 180).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:Name = "eSmartObjectType_TechnicalClassName":U.
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:eSmartObjectType_TechnicalClassName:TabIndex = 15.
        /*  */
        /* eSmartObjectType_ClassTypeGuid_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:Appearance = appearance7.
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:Location = NEW System.Drawing.Point(20, 157).
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:Name = "eSmartObjectType_ClassTypeGuid_Label":U.
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:TabIndex = 12.
        THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label:Text = "Class Type:":U.
        /*  */
        /* eSmartObjectType_ContainerType */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:eSmartObjectType_ContainerType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ContainerType:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_ContainerType:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ContainerType":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_ContainerType:Location = NEW System.Drawing.Point(432, 23).
        THIS-OBJECT:eSmartObjectType_ContainerType:Name = "eSmartObjectType_ContainerType":U.
        THIS-OBJECT:eSmartObjectType_ContainerType:Size = NEW System.Drawing.Size(70, 17).
        THIS-OBJECT:eSmartObjectType_ContainerType:TabIndex = 3.
        THIS-OBJECT:eSmartObjectType_ContainerType:Text = "Container":U.
        THIS-OBJECT:eSmartObjectType_ContainerType:UseMnemonics = TRUE.
        THIS-OBJECT:eSmartObjectType_ContainerType:CheckedChanged:Subscribe(THIS-OBJECT:eSmartObjectType_ContainerType_CheckedChanged).
        /*  */
        /* eSmartObjectType_StoreInstances_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:Appearance = appearance8.
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:Location = NEW System.Drawing.Point(20, 211).
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:Name = "eSmartObjectType_StoreInstances_Label":U.
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:Size = NEW System.Drawing.Size(102, 14).
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:TabIndex = 16.
        THIS-OBJECT:eSmartObjectType_StoreInstances_Label:Text = "Store instances for:":U.
        /*  */
        /* eSmartObjectType_ExtendsObjectTypeName */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ExtendsObjectTypeName":U, TRUE)).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:Enabled = FALSE.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:Location = NEW System.Drawing.Point(172, 99).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:Name = "eSmartObjectType_ExtendsObjectTypeName":U.
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName:TabIndex = 9.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "ShowLookupDialog":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomObjectTypeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomObjectTypeGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:Location = NEW System.Drawing.Point(172, 126).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "ObjectTypeName,ObjectTypeDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "ObjectTypeName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogTitle = "Lookup Custom Class":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartObjectType":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "ObjectTypeName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "ObjectTypeGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartObjectType WHERE eSmartObjectType.ObjectTypeName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:smartBusinessEntityLookup1:TabIndex = 11.
        /*  */
        /* smartBusinessEntityLookup2 */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Key = "ShowLookupDialog":U.
        editorButton2:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ClassTypeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ClassTypeGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup2:Location = NEW System.Drawing.Point(172, 153).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserFields = "ClassTypeName,AvmType,ClassTypeDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterFields = "ClassTypeName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogTitle = "Lookup Class Type":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityTable = "eSmartClassType":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyField = "ClassTypeName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValueColumn = "ClassTypeGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = "FOR EACH eSmartClassType WHERE eSmartClassType.ClassTypeName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup2:Name = "smartBusinessEntityLookup2":U.
        THIS-OBJECT:smartBusinessEntityLookup2:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup2:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:smartBusinessEntityLookup2:TabIndex = 13.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "StoreInstances":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "":U.
        valueListItem1:DisplayText = "not applicable":U.
        valueListItem2:DataValue = "MASTER":U.
        valueListItem2:DisplayText = "Master":U.
        valueListItem3:DataValue = "INSTANCE":U.
        valueListItem3:DisplayText = "Instance":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        arrayvar1[3] = valueListItem3.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(172, 207).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(330, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 17.
        /*  */
        /* SmartObjectTypeViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ObjectTypeName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ObjectTypeName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ObjectTypeDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ObjectTypeDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_DeploymentType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_DeploymentType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_StaticObject).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_CustomObjectTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_TechnicalClassName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_TechnicalClassName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ClassTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ContainerType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_StoreInstances_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName).
        THIS-OBJECT:Name = "SmartObjectTypeViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(505, 386).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_ObjectTypeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_ObjectTypeDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_DeploymentType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_StaticObject, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_TechnicalClassName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_ContainerType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectType_ExtendsObjectTypeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verfifies Enabled state of the Store Instances Combo   
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID VerifyStoreInstances ():
		
        IF eSmartObjectType_ContainerType:Checked THEN 
            THIS-OBJECT:SetControlEnabled (ultraComboEditor1, 
                                           ControlEnabledEnum:Update) . 
        ELSE 
            THIS-OBJECT:SetControlEnabled (ultraComboEditor1, 
                                           ControlEnabledEnum:Never) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartObjectTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartObjectTypeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
