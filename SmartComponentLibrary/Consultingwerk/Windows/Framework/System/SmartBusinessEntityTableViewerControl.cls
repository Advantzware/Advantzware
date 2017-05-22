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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartBusinessEntityTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartBusinessEntityTableViewerControl":U).
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
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartBusinessEntityTable":U.
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
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label, "eSmartBusinessEntityTable_TableOrder_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder_Label:Name = "eSmartBusinessEntityTable_TableOrder_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_TableOrder */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableOrder":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableOrder, "eSmartBusinessEntityTable_TableOrder":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableOrder:Name = "eSmartBusinessEntityTable_TableOrder":U.
        /*  */
        /* eSmartBusinessEntityTable_TableName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label, "eSmartBusinessEntityTable_TableName_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName_Label:Name = "eSmartBusinessEntityTable_TableName_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_TableName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableName, "eSmartBusinessEntityTable_TableName":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableName":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableName:Name = "eSmartBusinessEntityTable_TableName":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelSingular_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label, "eSmartBusinessEntityTable_TableLabelSingular_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular_Label:Name = "eSmartBusinessEntityTable_TableLabelSingular_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelSingular */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular, "eSmartBusinessEntityTable_TableLabelSingular":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableLabelSingular":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelSingular:Name = "eSmartBusinessEntityTable_TableLabelSingular":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelPlural_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label, "eSmartBusinessEntityTable_TableLabelPlural_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural_Label:Name = "eSmartBusinessEntityTable_TableLabelPlural_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_TableLabelPlural */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural, "eSmartBusinessEntityTable_TableLabelPlural":U).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableLabelPlural":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntityTable_TableLabelPlural:Name = "eSmartBusinessEntityTable_TableLabelPlural":U.
        /*  */
        /* eSmartBusinessEntityTable_ShouldUseBatching_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label, "eSmartBusinessEntityTable_ShouldUseBatching_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching_Label:Name = "eSmartBusinessEntityTable_ShouldUseBatching_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_ShouldUseBatching */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ShouldUseBatching":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching, "eSmartBusinessEntityTable_ShouldUseBatching":U).
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:Name = "eSmartBusinessEntityTable_ShouldUseBatching":U.
        THIS-OBJECT:eSmartBusinessEntityTable_ShouldUseBatching:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_BatchSize_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label, "eSmartBusinessEntityTable_BatchSize_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize_Label:Name = "eSmartBusinessEntityTable_BatchSize_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_BatchSize */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BatchSize":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_BatchSize, "eSmartBusinessEntityTable_BatchSize":U).
        THIS-OBJECT:eSmartBusinessEntityTable_BatchSize:Name = "eSmartBusinessEntityTable_BatchSize":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowDelete_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label, "eSmartBusinessEntityTable_AllowDelete_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete_Label:Name = "eSmartBusinessEntityTable_AllowDelete_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowDelete */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowDelete":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete, "eSmartBusinessEntityTable_AllowDelete":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:Name = "eSmartBusinessEntityTable_AllowDelete":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowDelete:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_AllowCreate_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label, "eSmartBusinessEntityTable_AllowCreate_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate_Label:Name = "eSmartBusinessEntityTable_AllowCreate_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowCreate */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowCreate":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate, "eSmartBusinessEntityTable_AllowCreate":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:Name = "eSmartBusinessEntityTable_AllowCreate":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowCreate:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntityTable_AllowUpdate_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label, "eSmartBusinessEntityTable_AllowUpdate_Label":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate_Label:Name = "eSmartBusinessEntityTable_AllowUpdate_Label":U.
        /*  */
        /* eSmartBusinessEntityTable_AllowUpdate */
        /*  */
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowUpdate":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate, "eSmartBusinessEntityTable_AllowUpdate":U).
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:Name = "eSmartBusinessEntityTable_AllowUpdate":U.
        THIS-OBJECT:eSmartBusinessEntityTable_AllowUpdate:UseMnemonics = TRUE.
        /*  */
        /* SmartBusinessEntityTableViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
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
