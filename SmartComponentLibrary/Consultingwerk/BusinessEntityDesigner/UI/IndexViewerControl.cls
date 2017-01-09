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
    File        : IndexViewerControl
    Purpose     : Viewer Control for the eIndex Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 23 21:09:05 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*      FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.IndexViewerControl INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE eIndex_DefaultSearchCodeGeneration AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_IndexWordIndex AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_IndexUnique AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_IndexPrimary AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_IndexName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_IndexName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_FieldSort_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_FieldSort AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_FieldNames_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eIndex_FieldNames AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MemberTableViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IndexViewerControl ():

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eIndex":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eIndex_DefaultSearchCodeGeneration, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:eIndex_DefaultSearchCodeGeneration).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(1, 22).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(625, 213).
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_FieldSort */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldSort":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:Location = NEW System.Drawing.Point(150, 135).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:Name = "smartBusinessEntityBindingSource1_eIndex_FieldSort":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:Size = NEW System.Drawing.Size(472, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:TabIndex = 8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort:Leave:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Leave).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eIndex":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "BusinessEntityName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TempTableName":U, "TempTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("IndexOrder":U, "IndexOrder":U, Progress.Data.DataType:INTEGER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("IndexName":U, "IndexName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("IndexUnique":U, "IndexUnique":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("IndexPrimary":U, "IndexPrimary":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("IndexWordIndex":U, "IndexWordIndex":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("FieldNames":U, "FieldNames":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("FieldSort":U, "FieldSort":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("DefaultSearchCodeGeneration":U, "DefaultSearchCodeGeneration":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_FieldSort_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:Appearance = appearance1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:Location = NEW System.Drawing.Point(3, 139).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:Name = "smartBusinessEntityBindingSource1_eIndex_FieldSort_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:Size = NEW System.Drawing.Size(56, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:TabIndex = 7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort_Label:Text = "&Field Sort:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_IndexName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:Appearance = appearance2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:Name = "smartBusinessEntityBindingSource1_eIndex_IndexName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName_Label:Text = "&Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_FieldNames */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldNames":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:Location = NEW System.Drawing.Point(150, 108).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:Name = "smartBusinessEntityBindingSource1_eIndex_FieldNames":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:Size = NEW System.Drawing.Size(472, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:TabIndex = 6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames:Leave:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Leave).
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_IndexName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "IndexName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:Name = "smartBusinessEntityBindingSource1_eIndex_IndexName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:Size = NEW System.Drawing.Size(472, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName:TabIndex = 1.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_FieldNames_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:Appearance = appearance3.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:Location = NEW System.Drawing.Point(3, 112).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:Name = "smartBusinessEntityBindingSource1_eIndex_FieldNames_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:Size = NEW System.Drawing.Size(71, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:TabIndex = 5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames_Label:Text = "&Field Names:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_IndexWordIndex */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "IndexWordIndex":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:Location = NEW System.Drawing.Point(150, 82).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:Name = "smartBusinessEntityBindingSource1_eIndex_IndexWordIndex":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:Text = "&Word-Index":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_IndexUnique */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "IndexUnique":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:Name = "smartBusinessEntityBindingSource1_eIndex_IndexUnique":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:Text = "&Unique":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eIndex_IndexPrimary */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "IndexPrimary":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:Location = NEW System.Drawing.Point(150, 56).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:Name = "smartBusinessEntityBindingSource1_eIndex_IndexPrimary":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:TabIndex = 3.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:Text = "&Primary":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary:UseMnemonics = TRUE.
        /*  */
        /* eIndex_DefaultSearchCodeGeneration */
        /*  */
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:AutoSize = TRUE.
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultSearchCodeGeneration":U, TRUE)).
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:Location = NEW System.Drawing.Point(150, 162).
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:Name = "eIndex_DefaultSearchCodeGeneration":U.
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:Size = NEW System.Drawing.Size(472, 20).
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:TabIndex = 9.
        THIS-OBJECT:eIndex_DefaultSearchCodeGeneration:Text = "Default Search Code Generation":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(627, 236).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "Index":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar1).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(625, 213).
        /*  */
        /* IndexViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Name = "IndexViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(556, 291).
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldSort, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_FieldNames, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexWordIndex, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexUnique, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eIndex_IndexPrimary, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eIndex_DefaultSearchCodeGeneration, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the
                 smartBusinessEntityBindingSource1_eIndex_FieldNames Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eIndex_FieldNames_EditorButtonClick (sender AS System.Object,
                                                                                               e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oIndexFieldsForm AS IndexFieldsForm                   NO-UNDO .
        DEFINE VARIABLE oDialogResult    AS System.Windows.Forms.DialogResult NO-UNDO .

        oIndexFieldsForm = NEW IndexFieldsForm (THIS-OBJECT:SmartDataSource:GetFieldValues ("BusinessEntityName":U),
                                                THIS-OBJECT:SmartDataSource:GetFieldValues ("TempTableName":U),
                                                THIS-OBJECT:SmartDataSource:GetFieldValues ("IndexName":U),
                                                smartBusinessEntityBindingSource1_eIndex_FieldNames:Text,
                                                smartBusinessEntityBindingSource1_eIndex_FieldSort:Text) .

        WAIT-FOR oIndexFieldsForm:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ASSIGN smartBusinessEntityBindingSource1_eIndex_FieldNames:Text = oIndexFieldsForm:IndexFields
               smartBusinessEntityBindingSource1_eIndex_FieldSort:Text  = oIndexFieldsForm:IndexSort
               THIS-OBJECT:SmartTableIOState                            = TableIOStateEnum:ModifyingData .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Leave event for the Index FieldNames field
        Notes:
        @param sender The reference to the object that raised this error
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eIndex_FieldNames_Leave (sender AS System.Object,
                                                                                   e AS System.EventArgs):

        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cSort   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .

        IF NOT smartBusinessEntityBindingSource1_eIndex_FieldSort:Enabled THEN
            RETURN .

        ASSIGN cFields = UNBOX (smartBusinessEntityBindingSource1_eIndex_FieldNames:Value)
               cSort   = UNBOX (smartBusinessEntityBindingSource1_eIndex_FieldSort:Value) .

         ASSIGN cFields = ListHelper:RemoveEmptyEntries (cFields)
                cSort   = ListHelper:RemoveEmptyEntries (cSort) .

        IF CharacterType:IsNullOrEmpty (cFields) THEN
            ASSIGN cSort = "":U .
        ELSE IF CharacterType:IsNotNullOrEmpty (cFields) AND CharacterType:IsNullOrEmpty (cSort) THEN DO:

            ASSIGN cSort = TRIM (FILL ("ASCENDING,":U, NUM-ENTRIES (cFields)), ",":U) .

            IF cSort > "":U THEN
                smartBusinessEntityBindingSource1_eIndex_FieldSort:Value = BOX (cSort) .

            RETURN .
        END.
        ELSE DO:
            IF NUM-ENTRIES (cSort) < NUM-ENTRIES (cFields) THEN DO:
                ASSIGN cSort = cSort + FILL (",ASCENDING":U, NUM-ENTRIES (cFields) - NUM-ENTRIES (cSort)) .
            END.
            ELSE IF NUM-ENTRIES (cSort) > NUM-ENTRIES (cFields) THEN DO:
                DO i = NUM-ENTRIES (cSort) TO NUM-ENTRIES (cFields) + 1 BY -1:
                    ASSIGN cSort = ListHelper:RemoveEntry (cSort, i) .
                END.
            END.

            DO i = 1 TO NUM-ENTRIES (cSort):
                IF ENTRY (i, cSort) BEGINS "D":U THEN
                    ASSIGN ENTRY (i, cSort) = "DESCENDING":U .
                ELSE
                    ASSIGN ENTRY (i, cSort) = "ASCENDING":U .
            END.
        END.

        smartBusinessEntityBindingSource1_eIndex_FieldNames:Value = BOX (cFields) .
        smartBusinessEntityBindingSource1_eIndex_FieldSort:Value = BOX (cSort) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MemberTableViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC IndexViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
