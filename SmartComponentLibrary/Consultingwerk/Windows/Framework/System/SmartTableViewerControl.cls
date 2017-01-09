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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartTable":U).
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
        THIS-OBJECT:tbTableName_Label:AutoSize = TRUE.
        THIS-OBJECT:tbTableName_Label:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:tbTableName_Label:Name = "tbTableName_Label":U.
        THIS-OBJECT:tbTableName_Label:Size = NEW System.Drawing.Size(69, 14).
        THIS-OBJECT:tbTableName_Label:TabIndex = 1.
        THIS-OBJECT:tbTableName_Label:Text = "Table Name:":U.
        /*  */
        /* tbTableName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableName":U, TRUE)).
        THIS-OBJECT:tbTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTableName:Location = NEW System.Drawing.Point(130, 3).
        THIS-OBJECT:tbTableName:Name = "tbTableName":U.
        THIS-OBJECT:tbTableName:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:tbTableName:TabIndex = 2.
        /*  */
        /* tbDatabaseName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbDatabaseName_Label:Appearance = appearance2.
        THIS-OBJECT:tbDatabaseName_Label:AutoSize = TRUE.
        THIS-OBJECT:tbDatabaseName_Label:Location = NEW System.Drawing.Point(3, 33).
        THIS-OBJECT:tbDatabaseName_Label:Name = "tbDatabaseName_Label":U.
        THIS-OBJECT:tbDatabaseName_Label:Size = NEW System.Drawing.Size(89, 14).
        THIS-OBJECT:tbDatabaseName_Label:TabIndex = 3.
        THIS-OBJECT:tbDatabaseName_Label:Text = "Database Name:":U.
        /*  */
        /* tbDatabaseName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatabaseName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatabaseName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatabaseName":U, TRUE)).
        THIS-OBJECT:tbDatabaseName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbDatabaseName:Location = NEW System.Drawing.Point(130, 29).
        THIS-OBJECT:tbDatabaseName:Name = "tbDatabaseName":U.
        THIS-OBJECT:tbDatabaseName:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:tbDatabaseName:TabIndex = 4.
        /*  */
        /* tbTableDescription_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbTableDescription_Label:Appearance = appearance3.
        THIS-OBJECT:tbTableDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:tbTableDescription_Label:Location = NEW System.Drawing.Point(3, 59).
        THIS-OBJECT:tbTableDescription_Label:Name = "tbTableDescription_Label":U.
        THIS-OBJECT:tbTableDescription_Label:Size = NEW System.Drawing.Size(96, 14).
        THIS-OBJECT:tbTableDescription_Label:TabIndex = 5.
        THIS-OBJECT:tbTableDescription_Label:Text = "Table Description:":U.
        /*  */
        /* tbTableDescription */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTableDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTableDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TableDescription":U, TRUE)).
        THIS-OBJECT:tbTableDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTableDescription:Location = NEW System.Drawing.Point(130, 55).
        THIS-OBJECT:tbTableDescription:Multiline = TRUE.
        THIS-OBJECT:tbTableDescription:Name = "tbTableDescription":U.
        THIS-OBJECT:tbTableDescription:Size = NEW System.Drawing.Size(278, 107).
        THIS-OBJECT:tbTableDescription:TabIndex = 6.
        /*  */
        /* tbUniqueKeyFields_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeyFields_Label:Appearance = appearance4.
        THIS-OBJECT:tbUniqueKeyFields_Label:AutoSize = TRUE.
        THIS-OBJECT:tbUniqueKeyFields_Label:Location = NEW System.Drawing.Point(3, 172).
        THIS-OBJECT:tbUniqueKeyFields_Label:Name = "tbUniqueKeyFields_Label":U.
        THIS-OBJECT:tbUniqueKeyFields_Label:Size = NEW System.Drawing.Size(99, 14).
        THIS-OBJECT:tbUniqueKeyFields_Label:TabIndex = 7.
        THIS-OBJECT:tbUniqueKeyFields_Label:Text = "Unique Key Fields:":U.
        /*  */
        /* tbUniqueKeyFields */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeyFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeyFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeyFields":U, TRUE)).
        THIS-OBJECT:tbUniqueKeyFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeyFields:Location = NEW System.Drawing.Point(130, 168).
        THIS-OBJECT:tbUniqueKeyFields:Name = "tbUniqueKeyFields":U.
        THIS-OBJECT:tbUniqueKeyFields:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:tbUniqueKeyFields:TabIndex = 8.
        /*  */
        /* tbUniqueKeyFormats_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeyFormats_Label:Appearance = appearance5.
        THIS-OBJECT:tbUniqueKeyFormats_Label:AutoSize = TRUE.
        THIS-OBJECT:tbUniqueKeyFormats_Label:Location = NEW System.Drawing.Point(3, 198).
        THIS-OBJECT:tbUniqueKeyFormats_Label:Name = "tbUniqueKeyFormats_Label":U.
        THIS-OBJECT:tbUniqueKeyFormats_Label:Size = NEW System.Drawing.Size(110, 14).
        THIS-OBJECT:tbUniqueKeyFormats_Label:TabIndex = 9.
        THIS-OBJECT:tbUniqueKeyFormats_Label:Text = "Unique Key Formats:":U.
        /*  */
        /* tbUniqueKeyFormats */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeyFormats:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeyFormats:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeyFormats":U, TRUE)).
        THIS-OBJECT:tbUniqueKeyFormats:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeyFormats:Location = NEW System.Drawing.Point(130, 194).
        THIS-OBJECT:tbUniqueKeyFormats:Name = "tbUniqueKeyFormats":U.
        THIS-OBJECT:tbUniqueKeyFormats:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:tbUniqueKeyFormats:TabIndex = 10.
        /*  */
        /* tbUniqueKeySubstitute_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Appearance = appearance6.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:AutoSize = TRUE.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Location = NEW System.Drawing.Point(3, 224).
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Name = "tbUniqueKeySubstitute_Label":U.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Size = NEW System.Drawing.Size(119, 14).
        THIS-OBJECT:tbUniqueKeySubstitute_Label:TabIndex = 11.
        THIS-OBJECT:tbUniqueKeySubstitute_Label:Text = "Unique Key Substitute:":U.
        /*  */
        /* tbUniqueKeySubstitute */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeySubstitute:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbUniqueKeySubstitute:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UniqueKeySubstitute":U, TRUE)).
        THIS-OBJECT:tbUniqueKeySubstitute:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbUniqueKeySubstitute:Location = NEW System.Drawing.Point(130, 220).
        THIS-OBJECT:tbUniqueKeySubstitute:Name = "tbUniqueKeySubstitute":U.
        THIS-OBJECT:tbUniqueKeySubstitute:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:tbUniqueKeySubstitute:TabIndex = 12.
        /*  */
        /* tbAllowComments */
        /*  */
        THIS-OBJECT:tbAllowComments:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowComments":U, TRUE)).
        THIS-OBJECT:tbAllowComments:Location = NEW System.Drawing.Point(130, 375).
        THIS-OBJECT:tbAllowComments:Name = "tbAllowComments":U.
        THIS-OBJECT:tbAllowComments:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:tbAllowComments:TabIndex = 23.
        THIS-OBJECT:tbAllowComments:Text = "Comments allowed":U.
        THIS-OBJECT:tbAllowComments:UseMnemonics = TRUE.
        /*  */
        /* tbAllowAttachments */
        /*  */
        THIS-OBJECT:tbAllowAttachments:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AllowAttachments":U, TRUE)).
        THIS-OBJECT:tbAllowAttachments:Location = NEW System.Drawing.Point(130, 402).
        THIS-OBJECT:tbAllowAttachments:Name = "tbAllowAttachments":U.
        THIS-OBJECT:tbAllowAttachments:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:tbAllowAttachments:TabIndex = 24.
        THIS-OBJECT:tbAllowAttachments:Text = "Attachments allowed":U.
        THIS-OBJECT:tbAllowAttachments:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Appearance = appearance7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Location = NEW System.Drawing.Point(3, 277).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Size = NEW System.Drawing.Size(97, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:TabIndex = 15.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields_Label:Text = "&Description Fields:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionFields":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Location = NEW System.Drawing.Point(130, 273).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFields:TabIndex = 16.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Appearance = appearance8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Location = NEW System.Drawing.Point(3, 301).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Size = NEW System.Drawing.Size(109, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:TabIndex = 17.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats_Label:Text = "&Description Formats:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionFormats":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Location = NEW System.Drawing.Point(130, 298).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionFormats:TabIndex = 18.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Appearance = appearance9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Location = NEW System.Drawing.Point(3, 325).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Size = NEW System.Drawing.Size(118, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:TabIndex = 19.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute_Label:Text = "&Description Substitute:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DescriptionSubstitute":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Location = NEW System.Drawing.Point(130, 322).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Name = "smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_DescriptionSubstitute:TabIndex = 20.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Appearance = appearance10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Location = NEW System.Drawing.Point(3, 352).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Name = "smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Size = NEW System.Drawing.Size(101, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:TabIndex = 21.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName_Label:Text = "&Temp Table Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartTable_TempTableName */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:Location = NEW System.Drawing.Point(130, 348).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:Name = "smartBusinessEntityBindingSource1_eSmartTable_TempTableName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName:TabIndex = 22.
        ultraToolTipInfo1:ToolTipImage = Infragistics.Win.ToolTipImage:Info.
        ultraToolTipInfo1:ToolTipText = "Defines the detaulf temp-table name used in the Business Entity Designer excluding the prefix like ~"e~" or ~"tt~"":U.
        THIS-OBJECT:ultraToolTipManager1:SetUltraToolTip(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartTable_TempTableName, ultraToolTipInfo1).
        /*  */
        /* ultraToolTipManager1 */
        /*  */
        THIS-OBJECT:ultraToolTipManager1:ContainingControl = THIS-OBJECT.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "KeyFieldAssignmentGUID":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(130, 247).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 25.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance11.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 251).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(124, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 26.
        THIS-OBJECT:ultraLabel1:Text = "Auto Assign Key Fields:":U.
        /*  */
        /* SmartTableViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
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
        THIS-OBJECT:Margin = NEW System.Windows.Forms.Padding(4).
        THIS-OBJECT:Name = "SmartTableViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(411, 451).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
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
