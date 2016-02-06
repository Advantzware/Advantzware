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
    File        : SmartBusinessEntityTableViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 20:05:46
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartBusinessEntityTableViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableOrder_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableOrder AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableLabelSingular_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableLabelSingular AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableLabelPlural_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_TableLabelPlural AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_ShouldUseBatching_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_ShouldUseBatching AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_BatchSize_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_BatchSize AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowDelete_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowDelete AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowCreate_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowCreate AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowUpdate_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntityTable_AllowUpdate AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityTableViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityTableViewerControl ():
                
        SUPER().
        InitializeComponent().
        
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartBusinessEntityTable":U).
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
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_TableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartBusinessEntityTable":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 11 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityTableGuid":U, "BusinessEntity Table Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("BusinessEntityGuid":U, "BusinessEntity Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("TableOrder":U, "Table Order":U, Progress.Data.DataType:INTEGER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("TableName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("TableLabelSingular":U, "Table Label Singular":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("TableLabelPlural":U, "Table Label Plural":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ShouldUseBatching":U, "Should use Batching":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("BatchSize":U, "Batch Size":U, Progress.Data.DataType:INTEGER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("AllowDelete":U, "Allow Delete":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("AllowCreate":U, "Allow Create":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("AllowUpdate":U, "Allow Update":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartBusinessEntityTable_TableOrder_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Location = NEW System.Drawing.Point(20, 132).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Name = "eSmartBusinessEntityTable_TableOrder_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Size = NEW System.Drawing.Size(68, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:TabIndex = 1.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Text = "Table Order:":U.
        /*  */
        /* eSmartBusinessEntityTable_TableOrder */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableOrder":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:Location = NEW System.Drawing.Point(147, 128).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:Name = "eSmartBusinessEntityTable_TableOrder":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:Size = NEW System.Drawing.Size(75, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:TabIndex = 4.
        /*  */
        /* eSmartBusinessEntityTable_TableName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Location = NEW System.Drawing.Point(20, 25).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Name = "eSmartBusinessEntityTable_TableName_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:TabIndex = 3.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Text = "Name:":U.
        /*  */
        /* eSmartBusinessEntityTable_TableName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableName":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:Location = NEW System.Drawing.Point(147, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:Name = "eSmartBusinessEntityTable_TableName":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:TabIndex = 0.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelSingular_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Location = NEW System.Drawing.Point(20, 52).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Name = "eSmartBusinessEntityTable_TableLabelSingular_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Size = NEW System.Drawing.Size(111, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:TabIndex = 5.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Text = "Table Label Singular:":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelSingular */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableLabelSingular":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:Location = NEW System.Drawing.Point(147, 48).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:Name = "eSmartBusinessEntityTable_TableLabelSingular":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:TabIndex = 1.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelPlural_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Location = NEW System.Drawing.Point(20, 78).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Name = "eSmartBusinessEntityTable_TableLabelPlural_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Size = NEW System.Drawing.Size(99, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:TabIndex = 7.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Text = "Table Label Plural:":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelPlural */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableLabelPlural":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:Location = NEW System.Drawing.Point(147, 74).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:Name = "eSmartBusinessEntityTable_TableLabelPlural":U.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:TabIndex = 2.
        /*  */
        /* eSmartBusinessEntityTable_ShouldUseBatching_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Location = NEW System.Drawing.Point(20, 158).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Name = "eSmartBusinessEntityTable_ShouldUseBatching_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Size = NEW System.Drawing.Size(111, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:TabIndex = 9.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Text = "Should use Batching:":U.
        /*  */
        /* eSmartBusinessEntityTable_ShouldUseBatching */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ShouldUseBatching":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:Location = NEW System.Drawing.Point(147, 155).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:Name = "eSmartBusinessEntityTable_ShouldUseBatching":U.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:TabIndex = 5.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_BatchSize_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Appearance = appearance6.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Location = NEW System.Drawing.Point(20, 105).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Name = "eSmartBusinessEntityTable_BatchSize_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Size = NEW System.Drawing.Size(61, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:TabIndex = 11.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Text = "Batch Size:":U.
        /*  */
        /* eSmartBusinessEntityTable_BatchSize */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BatchSize":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:Location = NEW System.Drawing.Point(147, 101).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:Name = "eSmartBusinessEntityTable_BatchSize":U.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:Size = NEW System.Drawing.Size(75, 21).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:TabIndex = 3.
        /*  */
        /* eSmartBusinessEntityTable_AllowDelete_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Appearance = appearance7.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Location = NEW System.Drawing.Point(20, 184).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Name = "eSmartBusinessEntityTable_AllowDelete_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Size = NEW System.Drawing.Size(70, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:TabIndex = 13.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Text = "Allow Delete:":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowDelete */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowDelete":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:Location = NEW System.Drawing.Point(147, 181).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:Name = "eSmartBusinessEntityTable_AllowDelete":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:TabIndex = 6.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_AllowCreate_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Appearance = appearance8.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Location = NEW System.Drawing.Point(20, 210).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Name = "eSmartBusinessEntityTable_AllowCreate_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Size = NEW System.Drawing.Size(72, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:TabIndex = 15.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Text = "Allow Create:":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowCreate */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowCreate":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:Location = NEW System.Drawing.Point(147, 207).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:Name = "eSmartBusinessEntityTable_AllowCreate":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:TabIndex = 7.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_AllowUpdate_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Appearance = appearance9.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Location = NEW System.Drawing.Point(20, 236).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Name = "eSmartBusinessEntityTable_AllowUpdate_Label":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Size = NEW System.Drawing.Size(74, 14).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:TabIndex = 17.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Text = "Allow Update:":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowUpdate */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowUpdate":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:Location = NEW System.Drawing.Point(147, 233).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:Name = "eSmartBusinessEntityTable_AllowUpdate":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:TabIndex = 8.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:UseMnemonics = TRUE.
        /*  */
        /* SmartBusinessEntityTableViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate).
        THIS-OBJECT:Name = "SmartBusinessEntityTableViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(312, 271).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartBusinessEntityTableViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityTableViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
