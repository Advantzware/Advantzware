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
    File        : DataRelationViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 16 22:34:33 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.DataRelationViewerControl INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_RelationFields AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataRelationViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataRelationViewerControl ():

        SUPER().

        InitializeComponent().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

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
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eDataRelation":U).
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
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
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(512, 330).
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildCardinality":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "ONE":U.
        valueListItem1:DisplayText = "One":U.
        valueListItem2:DataValue = "MANY":U.
        valueListItem2:DisplayText = "Many":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        arrayvar0[2] = valueListItem2.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(150, 215).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(358, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 14.
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eDataRelation":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 13 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "BusinessEntityName":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("DataRelationName":U, "DataRelationName":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("ParentTempTableName":U, "ParentTempTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("ChildTempTableName":U, "ChildTempTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("RelationFields":U, "RelationFields":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("DataRelationDescription":U, "DataRelationDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("DataRelationReposition":U, "DataRelationReposition":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[8] = NEW Progress.Data.ColumnPropDesc("DataRelationNested":U, "DataRelationNested":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[9] = NEW Progress.Data.ColumnPropDesc("DataRelationForeignKeyHidden":U, "DataRelationForeignKeyHidden":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[10] = NEW Progress.Data.ColumnPropDesc("DataRelationNotActive":U, "DataRelationNotActive":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[11] = NEW Progress.Data.ColumnPropDesc("DataRelationRecursive":U, "DataRelationRecursive":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[12] = NEW Progress.Data.ColumnPropDesc("ChildCardinality":U, "Child Cardinality":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[13] = NEW Progress.Data.ColumnPropDesc("ChildPropertyName":U, "Child Property Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:Size = NEW System.Drawing.Size(359, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName:TabIndex = 1.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Appearance = appearance1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationReposition":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Location = NEW System.Drawing.Point(149, 111).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Size = NEW System.Drawing.Size(359, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:TabIndex = 8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:Text = "&Reposition":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Appearance = appearance2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationNested":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Location = NEW System.Drawing.Point(149, 137).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Size = NEW System.Drawing.Size(82, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:TabIndex = 9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Text = "&Nested":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:UseMnemonics = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:CheckedChanged:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested_CheckedChanged).
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:Appearance = appearance3.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:Location = NEW System.Drawing.Point(6, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:Size = NEW System.Drawing.Size(109, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName_Label:Text = "&Data Relation Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Appearance = appearance4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationForeignKeyHidden":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Location = NEW System.Drawing.Point(236, 137).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Size = NEW System.Drawing.Size(272, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:TabIndex = 10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Text = "&Foreign Key Hidden":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:UseMnemonics = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:CheckedChanged:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden_CheckedChanged).
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_RelationFields */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RelationFields":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Location = NEW System.Drawing.Point(150, 84).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Name = "smartBusinessEntityBindingSource1_eDataRelation_RelationFields":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Size = NEW System.Drawing.Size(359, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:TabIndex = 7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Appearance = appearance5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationNotActive":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Location = NEW System.Drawing.Point(150, 163).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Size = NEW System.Drawing.Size(358, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:TabIndex = 11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:Text = "N&ot Active":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:Appearance = appearance6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:Location = NEW System.Drawing.Point(6, 88).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:Size = NEW System.Drawing.Size(82, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:TabIndex = 6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_Label:Text = "&Relation Fields:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Appearance = appearance7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationRecursive":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Location = NEW System.Drawing.Point(150, 189).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Size = NEW System.Drawing.Size(358, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:TabIndex = 12.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Text = "&Recursive":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:UseMnemonics = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:CheckedChanged:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive_CheckedChanged).
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:Appearance = appearance8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:Location = NEW System.Drawing.Point(6, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:Size = NEW System.Drawing.Size(138, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName_Label:Text = "&Parent Temp-Table Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildTempTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:Location = NEW System.Drawing.Point(150, 57).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:Name = "smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:Size = NEW System.Drawing.Size(359, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:TabIndex = 5.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentTempTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:Name = "smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:Size = NEW System.Drawing.Size(359, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName:TabIndex = 3.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:Appearance = appearance9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:Location = NEW System.Drawing.Point(6, 61).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:Size = NEW System.Drawing.Size(131, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName_Label:Text = "&Child Temp-Table Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:Appearance = appearance10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:Location = NEW System.Drawing.Point(6, 219).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:Size = NEW System.Drawing.Size(90, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:TabIndex = 13.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildCardinality_Label:Text = "&Child Cardinality:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:Appearance = appearance11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:Location = NEW System.Drawing.Point(6, 246).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:Name = "smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:Size = NEW System.Drawing.Size(112, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:TabIndex = 15.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName_Label:Text = "&Child Property Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildPropertyName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Location = NEW System.Drawing.Point(150, 242).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Name = "smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Size = NEW System.Drawing.Size(358, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:TabIndex = 16.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(1, 22).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(512, 330).
        /*  */
        /* smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataRelationDescription":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Name = "smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:Size = NEW System.Drawing.Size(512, 330).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription:TabIndex = 0.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(514, 353).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Data-Relation":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "Description":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar2[1] = ultraTab1.
        arrayvar2[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar2).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(512, 330).
        /*  */
        /* DataRelationViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName.
        THIS-OBJECT:Name = "DataRelationViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(514, 353).
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationReposition, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationNotActive, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ParentTempTableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_DataRelationDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object,
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
            AND THIS-OBJECT:Enabled THEN DO:

&IF NOT PROVERSION BEGINS "10" &THEN
            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
&ELSE
            /* Mike Fechner, Consultingwerk Ltd. 17.08.2015
               DYNAMIC-INVOKE seems to crash here on 10.2B */
            CASE sender:
                WHEN smartBusinessEntityBindingSource1_eDataRelation_RelationFields
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eDataRelation_RelationFields_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE .
&ENDIF
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the
                 smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden_CheckedChanged (sender AS System.Object,
                                                                                                                     e AS System.EventArgs):

            IF smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Checked THEN
                smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Checked = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the
                 smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested_CheckedChanged (sender AS System.Object,
                                                                                                           e AS System.EventArgs):

        IF THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:FieldsEnabled OR
           THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:ModifyingData THEN DO:

            IF smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Checked THEN
                smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Checked = FALSE .
            ELSE
                smartBusinessEntityBindingSource1_eDataRelation_DataRelationForeignKeyHidden:Checked = FALSE .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the
                 smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive_CheckedChanged (sender AS System.Object,
                                                                                                              e AS System.EventArgs):

        IF THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:FieldsEnabled OR
           THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:ModifyingData THEN DO:

            IF smartBusinessEntityBindingSource1_eDataRelation_DataRelationRecursive:Checked THEN
                smartBusinessEntityBindingSource1_eDataRelation_DataRelationNested:Checked = FALSE .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the
                 smartBusinessEntityBindingSource1_eDataRelation_RelationFields
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eDataRelation_RelationFields_EditorButtonClick (sender AS System.Object,
                                                                                                          e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oForm         AS Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm NO-UNDO .

        DEFINE VARIABLE hParentTable  AS HANDLE                                                               NO-UNDO .
        DEFINE VARIABLE hChildTable   AS HANDLE                                                               NO-UNDO .
        DEFINE VARIABLE hDataset      AS HANDLE                                                               NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE                                                               NO-UNDO.
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                                    NO-UNDO .
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Form                                            NO-UNDO .

        oForm = NEW Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm () .

        ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource,
                                Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter):Dataset
               hBuffer = hDataset::eTable .

        CREATE BUFFER hParentTable FOR TABLE hBuffer .
        CREATE BUFFER hChildTable FOR TABLE hBuffer .

        hParentTable:FIND-FIRST (SUBSTITUTE ("WHERE BusinessEntityName = '&1' AND TempTableName = '&2'":U,
                                 THIS-OBJECT:SmartDataSource:GetFieldValues("BusinessEntityName":U),
                                 THIS-OBJECT:SmartDataSource:GetFieldValues("ParentTempTableName":U))) .

        hChildTable:FIND-FIRST (SUBSTITUTE ("WHERE BusinessEntityName = '&1' AND TempTableName = '&2'":U,
                                THIS-OBJECT:SmartDataSource:GetFieldValues("BusinessEntityName":U),
                                THIS-OBJECT:SmartDataSource:GetFieldValues("ChildTempTableName":U))) .

        oForm:Dataset = hDataset .
        oForm:DeleteOnFormClosed = FALSE .
        oForm:ParentTableBuffer = hParentTable .
        oForm:ChildTableBuffer = hChildTable .
        oForm:RelationFields = smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Text .

        oParent = THIS-OBJECT:FindForm() .

        WAIT-FOR oForm:ShowDialog (oParent) SET oDialogResult.

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN
            ASSIGN smartBusinessEntityBindingSource1_eDataRelation_RelationFields:Text = oForm:RelationFields
                   THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hParentTable) .
            GarbageCollectorHelper:DeleteObject (hChildTable) .
            GarbageCollectorHelper:DeleteObject (oForm) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraComboEditor1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (sender AS System.Object,
                                                        e AS System.EventArgs):

        DEFINE VARIABLE cChildTable   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPropertyName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hDataset      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.

        IF NOT CAST (sender, System.Windows.Forms.Control):Enabled THEN
            RETURN .

        /* Only populate child relation name when previously set */
        IF smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Text > "":U THEN .
            ELSE RETURN .

        ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource,
                                Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter):Dataset
               hBuffer = hDataset::eBusinessEntity .

        CREATE BUFFER hBuffer FOR TABLE hBuffer .

        ASSIGN cChildTable = smartBusinessEntityBindingSource1_eDataRelation_ChildTempTableName:Text .

        hBuffer:FIND-FIRST () .

        IF hBuffer::DefaultTablePrefix > "":U AND cChildTable BEGINS hBuffer::DefaultTablePrefix THEN
            cPropertyName = SUBSTRING (cChildTable, 1 + LENGTH (hBuffer::DefaultTablePrefix, "CHARACTER":U), -1, "CHARACTER":U) NO-ERROR .

        IF NOT cPropertyName > "":U THEN
            ASSIGN cPropertyName = cChildTable .

        IF UNBOX (ultraComboEditor1:Value) = "MANY":U THEN
            ASSIGN cPropertyName = cPropertyName + "s":U .

        ASSIGN smartBusinessEntityBindingSource1_eDataRelation_ChildPropertyName:Text = cPropertyName .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DataRelationViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DataRelationViewerControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
