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
    File        : SmartTableViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 27.11.2012 23:46:09
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartTableViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartTable_TempTableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDatabaseName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDatabaseName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTableDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTableDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeyFields_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeyFields AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeyFormats_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeyFormats AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeySubstitute_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbUniqueKeySubstitute AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAllowComments AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAllowAttachments AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolTipManager1 AS Infragistics.Win.UltraWinToolTip.UltraToolTipManager NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTableViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTableViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        ultraComboEditor1:ValueList = ValueListHelper:FromBusinessEntity ("":U,
                                                                          "Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity":U,
                                                                          "eSmartKeyFieldAssignmentType":U,
                                                                          "for each eSmartKeyFieldAssignmentType":U,
                                                                          "KeyFieldAssignmentGUID":U,
                                                                          "KeyFieldAssignmentKey,KeyFieldAssignmentDescription":U,
                                                                          "&1 (&2)":U,
                                                                          "":U,
                                                                          "none":U) .
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartTableViewerControl":U).
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
        DEFINE VARIABLE ultraToolTipInfo1 AS Infragistics.Win.UltraWinToolTip.UltraToolTipInfo NO-UNDO.
        ultraToolTipInfo1 = NEW Infragistics.Win.UltraWinToolTip.UltraToolTipInfo("Defines the detaulf temp-table name used in the Business Entity Designer excluding the prefix like ~"e~" or ~"tt~"":U, Infragistics.Win.ToolTipImage:Info, ?, Infragistics.Win.DefaultableBoolean:Default).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbTableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbDatabaseName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbDatabaseName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbTableDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTableDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbUniqueKeyFields_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbUniqueKeyFields = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbUniqueKeyFormats_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbUniqueKeyFormats = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbUniqueKeySubstitute_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbUniqueKeySubstitute = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAllowComments = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:tbAllowAttachments = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraToolTipManager1 = NEW Infragistics.Win.UltraWinToolTip.UltraToolTipManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDatabaseName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTableDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbUniqueKeyFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbUniqueKeyFormats, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbUniqueKeySubstitute, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAllowComments, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAllowAttachments, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.TableBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartTable":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 15 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("TableGUID":U, "TableGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TableName":U, "Table Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("DatabaseName":U, "Database Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("TableDescription":U, "Table Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("UniqueKeyFields":U, "Unique Key Fields":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("UniqueKeyFormats":U, "Unique Key Formats":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("UniqueKeySubstitute":U, "Unique Key Substitute":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("AllowComments":U, "Comments allowed":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("AllowAttachments":U, "Attachments Allowed":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("DescriptionFields":U, "Description Fields":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("DescriptionFormats":U, "Description Formats":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("DescriptionSubstitute":U, "Description Substitute":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("QualifiedTableName":U, "Qualified Table Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("TempTableName":U, "Temp Table Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("KeyFieldAssignmentGUID":U, "KeyFieldAssignmentGUID":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbTableName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbTableName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:tbTableName_Label, "tbTableName_Label":U).
        THIS-OBJECT:tbTableName_Label:Name = "tbTableName_Label":U.
        /*  */
        /* tbTableName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbTableName, "tbTableName":U).
        THIS-OBJECT:tbTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableName":U, TRUE)).
        THIS-OBJECT:tbTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTableName:Name = "tbTableName":U.
        /*  */
        /* tbDatabaseName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbDatabaseName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbDatabaseName_Label, "tbDatabaseName_Label":U).
        THIS-OBJECT:tbDatabaseName_Label:Name = "tbDatabaseName_Label":U.
        /*  */
        /* tbDatabaseName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbDatabaseName, "tbDatabaseName":U).
        THIS-OBJECT:tbDatabaseName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatabaseName":U, TRUE)).
        THIS-OBJECT:tbDatabaseName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbDatabaseName:Name = "tbDatabaseName":U.
        /*  */
        /* tbTableDescription_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbTableDescription_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:tbTableDescription_Label, "tbTableDescription_Label":U).
        THIS-OBJECT:tbTableDescription_Label:Name = "tbTableDescription_Label":U.
        /*  */
        /* tbTableDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbTableDescription, "tbTableDescription":U).
        THIS-OBJECT:tbTableDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableDescription":U, TRUE)).
        THIS-OBJECT:tbTableDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTableDescription:Name = "tbTableDescription":U.
        /*  */
        /* tbUniqueKeyFields_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeyFields_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeyFields_Label, "tbUniqueKeyFields_Label":U).
        THIS-OBJECT:tbUniqueKeyFields_Label:Name = "tbUniqueKeyFields_Label":U.
        /*  */
        /* tbUniqueKeyFields */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeyFields, "tbUniqueKeyFields":U).
        THIS-OBJECT:tbUniqueKeyFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeyFields":U, TRUE)).
        THIS-OBJECT:tbUniqueKeyFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeyFields:Name = "tbUniqueKeyFields":U.
        /*  */
        /* tbUniqueKeyFormats_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeyFormats_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeyFormats_Label, "tbUniqueKeyFormats_Label":U).
        THIS-OBJECT:tbUniqueKeyFormats_Label:Name = "tbUniqueKeyFormats_Label":U.
        /*  */
        /* tbUniqueKeyFormats */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeyFormats, "tbUniqueKeyFormats":U).
        THIS-OBJECT:tbUniqueKeyFormats:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeyFormats":U, TRUE)).
        THIS-OBJECT:tbUniqueKeyFormats:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeyFormats:Name = "tbUniqueKeyFormats":U.
        /*  */
        /* tbUniqueKeySubstitute_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeySubstitute_Label, "tbUniqueKeySubstitute_Label":U).
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Name = "tbUniqueKeySubstitute_Label":U.
        /*  */
        /* tbUniqueKeySubstitute */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbUniqueKeySubstitute, "tbUniqueKeySubstitute":U).
        THIS-OBJECT:tbUniqueKeySubstitute:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeySubstitute":U, TRUE)).
        THIS-OBJECT:tbUniqueKeySubstitute:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeySubstitute:Name = "tbUniqueKeySubstitute":U.
        /*  */
        /* tbAllowComments */
        /*  */
        THIS-OBJECT:tbAllowComments:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowComments":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:tbAllowComments, "tbAllowComments":U).
        THIS-OBJECT:tbAllowComments:Name = "tbAllowComments":U.
        THIS-OBJECT:tbAllowComments:UseMnemonics = TRUE.
        /*  */
        /* tbAllowAttachments */
        /*  */
        THIS-OBJECT:tbAllowAttachments:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowAttachments":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:tbAllowAttachments, "tbAllowAttachments":U).
        THIS-OBJECT:tbAllowAttachments:Name = "tbAllowAttachments":U.
        THIS-OBJECT:tbAllowAttachments:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionFields":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionFormats":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute, "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionSubstitute":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Appearance = appearance10.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label, "smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_TempTableName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName, "smartBusinessEntityBindingSource1_eSmartTable_TempTableName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:Name = "smartBusinessEntityBindingSource1_eSmartTable_TempTableName":U.
        ultraToolTipInfo1:ToolTipImage = Infragistics.Win.ToolTipImage:Info.
        resources:ApplyResources(ultraToolTipInfo1, "ultraToolTipInfo1":U).
        THIS-OBJECT:ultraToolTipManager1:SetUltraToolTip(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName, ultraToolTipInfo1).
        /*  */
        /* ultraToolTipManager1 */
        /*  */
        THIS-OBJECT:ultraToolTipManager1:ContainingControl = THIS-OBJECT.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "KeyFieldAssignmentGUID":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance11.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* SmartTableViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTableName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTableName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDatabaseName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDatabaseName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTableDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTableDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeyFields_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeyFields).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeyFormats_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeyFormats).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeySubstitute).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAllowComments).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAllowAttachments).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbUniqueKeySubstitute_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName).
        THIS-OBJECT:Name = "SmartTableViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDatabaseName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTableDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbUniqueKeyFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbUniqueKeyFormats, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbUniqueKeySubstitute, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAllowComments, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAllowAttachments, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartTableViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
