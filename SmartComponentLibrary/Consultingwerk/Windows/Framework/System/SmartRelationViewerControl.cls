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
    File        : SmartRelationViewerControl
    Purpose     : Maintenance Object for the SmartRelation Table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.04.2013 21:51:46
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartRelationViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE cbParentCardinality AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbOnParentDelete AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbChildCardinality AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE eSmartRelation_Inactive AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParentTableGUID_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbChildTableGUID_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationFieldPair_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelationFieldPair AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParentRoleDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParentRoleDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbChildRoleDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbChildRoleDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOnParentDelete_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartRelationViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartRelationViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        cbParentCardinality:ValueList:ValueListItems:Clear () .
        cbParentCardinality:ValueList:ValueListItems:Add ("1":U, "exacly 1"{&TRAN}) .
        cbParentCardinality:ValueList:ValueListItems:Add ("01":U, "0 or 1"{&TRAN}) .
        cbParentCardinality:ValueList:ValueListItems:Add ("01n":U, "0, 1 or n"{&TRAN}) .
        cbParentCardinality:ValueList:ValueListItems:Add ("1n":U, "1 or n"{&TRAN}) .

        cbChildCardinality:ValueList:ValueListItems:Clear () .
        cbChildCardinality:ValueList:ValueListItems:Add ("01n":U, "0, 1 or n"{&TRAN}) .
        cbChildCardinality:ValueList:ValueListItems:Add ("1":U, "exacly 1"{&TRAN}) .
        cbChildCardinality:ValueList:ValueListItems:Add ("01":U, "0 or 1"{&TRAN}) .
        cbChildCardinality:ValueList:ValueListItems:Add ("1n":U, "1 or n"{&TRAN}) .

        cbOnParentDelete:ValueList:ValueListItems:Clear () .
        cbOnParentDelete:ValueList:ValueListItems:Add ("DoNothing":U, "Do nothing"{&TRAN}) .
        cbOnParentDelete:ValueList:ValueListItems:Add ("Cascade":U, "Cascade"{&TRAN}) .
        cbOnParentDelete:ValueList:ValueListItems:Add ("Nullify":U, "Nullify"{&TRAN}) .
        cbOnParentDelete:ValueList:ValueListItems:Add ("Restrict":U, "Restrict"{&TRAN}) .
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartRelation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartRelationViewerControl":U).
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
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbParentTableGUID_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbChildTableGUID_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbRelationName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbRelationName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbRelationDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbRelationDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbRelationFieldPair_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbRelationFieldPair = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbParentRoleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbParentRoleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbChildRoleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbChildRoleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbOnParentDelete_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbParentCardinality = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbChildCardinality = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbOnParentDelete = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityLookup2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartRelation_Inactive = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbRelationName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbRelationDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbRelationFieldPair, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbParentRoleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbChildRoleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbParentCardinality, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbChildCardinality, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbOnParentDelete, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartRelation_Inactive, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.RelationBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartRelation":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 18 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("RelationGuid":U, "RelationGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ParentTableGUID":U, "ParentTableGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ParentTableName":U, "ParentTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ChildTableGUID":U, "ChildTableGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ChildTableName":U, "ChildTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("RelationName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("RelationDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("RelationFieldPair":U, "RelationFieldPair(s)":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("ParentRoleDescription":U, "Parent Role Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("ChildRoleDescription":U, "Child Role Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("ParentCardinality":U, "Parent Cardinality":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("ChildCardinality":U, "Child Cardinality":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("OnParentDelete":U, "On Parent Delete":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("DefaultParentRelation":U, "Default Parent Relation":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("SmartAttachments":U, "Attachments":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("SmartComments":U, "Comments":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("SmartRecordKey":U, "SmartRecordKey":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("Inactive":U, "Inactive":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbParentTableGUID_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbParentTableGUID_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:tbParentTableGUID_Label, "tbParentTableGUID_Label":U).
        THIS-OBJECT:tbParentTableGUID_Label:Name = "tbParentTableGUID_Label":U.
        /*  */
        /* tbChildTableGUID_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbChildTableGUID_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbChildTableGUID_Label, "tbChildTableGUID_Label":U).
        THIS-OBJECT:tbChildTableGUID_Label:Name = "tbChildTableGUID_Label":U.
        /*  */
        /* tbRelationName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbRelationName_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:tbRelationName_Label, "tbRelationName_Label":U).
        THIS-OBJECT:tbRelationName_Label:Name = "tbRelationName_Label":U.
        /*  */
        /* tbRelationName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbRelationName, "tbRelationName":U).
        THIS-OBJECT:tbRelationName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RelationName":U, TRUE)).
        THIS-OBJECT:tbRelationName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbRelationName:Name = "tbRelationName":U.
        /*  */
        /* tbRelationDescription_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbRelationDescription_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:tbRelationDescription_Label, "tbRelationDescription_Label":U).
        THIS-OBJECT:tbRelationDescription_Label:Name = "tbRelationDescription_Label":U.
        /*  */
        /* tbRelationDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbRelationDescription, "tbRelationDescription":U).
        THIS-OBJECT:tbRelationDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RelationDescription":U, TRUE)).
        THIS-OBJECT:tbRelationDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbRelationDescription:Name = "tbRelationDescription":U.
        THIS-OBJECT:tbRelationDescription:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        /*  */
        /* tbRelationFieldPair_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbRelationFieldPair_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:tbRelationFieldPair_Label, "tbRelationFieldPair_Label":U).
        THIS-OBJECT:tbRelationFieldPair_Label:Name = "tbRelationFieldPair_Label":U.
        /*  */
        /* tbRelationFieldPair */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbRelationFieldPair, "tbRelationFieldPair":U).
        THIS-OBJECT:tbRelationFieldPair:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RelationFieldPair":U, TRUE)).
        THIS-OBJECT:tbRelationFieldPair:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbRelationFieldPair:Name = "tbRelationFieldPair":U.
        /*  */
        /* tbParentRoleDescription_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbParentRoleDescription_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:tbParentRoleDescription_Label, "tbParentRoleDescription_Label":U).
        THIS-OBJECT:tbParentRoleDescription_Label:Name = "tbParentRoleDescription_Label":U.
        /*  */
        /* tbParentRoleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbParentRoleDescription, "tbParentRoleDescription":U).
        THIS-OBJECT:tbParentRoleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentRoleDescription":U, TRUE)).
        THIS-OBJECT:tbParentRoleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbParentRoleDescription:Name = "tbParentRoleDescription":U.
        /*  */
        /* tbChildRoleDescription_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbChildRoleDescription_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:tbChildRoleDescription_Label, "tbChildRoleDescription_Label":U).
        THIS-OBJECT:tbChildRoleDescription_Label:Name = "tbChildRoleDescription_Label":U.
        /*  */
        /* tbChildRoleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbChildRoleDescription, "tbChildRoleDescription":U).
        THIS-OBJECT:tbChildRoleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildRoleDescription":U, TRUE)).
        THIS-OBJECT:tbChildRoleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbChildRoleDescription:Name = "tbChildRoleDescription":U.
        /*  */
        /* tbOnParentDelete_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbOnParentDelete_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:tbOnParentDelete_Label, "tbOnParentDelete_Label":U).
        THIS-OBJECT:tbOnParentDelete_Label:Name = "tbOnParentDelete_Label":U.
        /*  */
        /* cbParentCardinality */
        /*  */
        THIS-OBJECT:cbParentCardinality:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentCardinality":U, TRUE)).
        THIS-OBJECT:cbParentCardinality:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbParentCardinality:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:cbParentCardinality, "cbParentCardinality":U).
        THIS-OBJECT:cbParentCardinality:Name = "cbParentCardinality":U.
        /*  */
        /* cbChildCardinality */
        /*  */
        THIS-OBJECT:cbChildCardinality:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildCardinality":U, TRUE)).
        THIS-OBJECT:cbChildCardinality:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbChildCardinality:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:cbChildCardinality, "cbChildCardinality":U).
        THIS-OBJECT:cbChildCardinality:Name = "cbChildCardinality":U.
        /*  */
        /* cbOnParentDelete */
        /*  */
        THIS-OBJECT:cbOnParentDelete:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "OnParentDelete":U, TRUE)).
        THIS-OBJECT:cbOnParentDelete:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbOnParentDelete:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:cbOnParentDelete, "cbOnParentDelete":U).
        THIS-OBJECT:cbOnParentDelete:Name = "cbOnParentDelete":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentTableGUID":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "DatabaseName,TableName,TableDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "DatabaseName,TableName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS,BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.System.TableBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartTable":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "QualifiedTableName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "TableGUID":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartTable WHERE eSmartTable.QualifiedTableName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        /*  */
        /* smartBusinessEntityLookup2 */
        /*  */
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ChildTableGUID":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup2, "smartBusinessEntityLookup2":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserFields = "DatabaseName,TableName,TableDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterFields = "DatabaseName,TableName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterOperators = "BEGINS,BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityName = "Consultingwerk.SmartFramework.System.TableBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityTable = "eSmartTable":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyField = "QualifiedTableName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValueColumn = "TableGUID":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = "FOR EACH eSmartTable WHERE eSmartTable.QualifiedTableName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:Name = "smartBusinessEntityLookup2":U.
        THIS-OBJECT:smartBusinessEntityLookup2:OwningGrid = ?.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation, "smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultParentRelation":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation:Name = "smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation":U.
        /*  */
        /* eSmartRelation_Inactive */
        /*  */
        THIS-OBJECT:eSmartRelation_Inactive:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Inactive":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartRelation_Inactive, "eSmartRelation_Inactive":U).
        THIS-OBJECT:eSmartRelation_Inactive:Name = "eSmartRelation_Inactive":U.
        /*  */
        /* SmartRelationViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbOnParentDelete).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbChildCardinality).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbParentCardinality).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbParentTableGUID_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbChildTableGUID_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationFieldPair_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbRelationFieldPair).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbParentRoleDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbParentRoleDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbChildRoleDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbChildRoleDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbOnParentDelete_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartRelation_Inactive).
        THIS-OBJECT:Name = "SmartRelationViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbRelationName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbRelationDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbRelationFieldPair, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbParentRoleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbChildRoleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbParentCardinality, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbChildCardinality, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbOnParentDelete, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartRelation_DefaultParentRelation, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartRelation_Inactive, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartRelationViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartRelationViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
