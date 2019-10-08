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
    File        : MemberTableViewerControl
    Purpose     : Viewer Control for the eTable Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 12:25:42 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*  FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*              FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*               FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM ASSEMBLY .
USING System.Windows.Forms.*                           FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.MemberTableViewerControl INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE m_MemberTableViewerControl_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_MemberTableViewerControl_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_MemberTableViewerControl_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE eTable_DeletedFields_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eTable_DeletedFields AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_EntityPackageName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_EntityClassName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_EntityClassName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_NoBeforeTable AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SerializeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SerializeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_NamespaceUri AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_NamespacePrefix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceBufferKeys AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceBufferNames AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_SourceTableNames AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_XmlNodeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTablePurpose AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTablePath_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTablePath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTableName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTableName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTableDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eTable_TempTableBeforeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MemberTableViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MemberTableViewerControl ():

        SUPER().
        InitializeComponent().

        THIS-OBJECT:SetControlEnabled (THIS-OBJECT:ultraTabControl1, Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Always) .

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eTable":U).
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Source Buffer Keys":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Key Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ROWID":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.MemberTableViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Key Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ROWID":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eTable_DeletedFields_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eTable_DeletedFields = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eTable_DeletedFields, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:eTable_DeletedFields_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:eTable_DeletedFields).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 22).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(579, 499).
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:Appearance = appearance1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:Location = NEW System.Drawing.Point(4, 240).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:Name = "smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:Size = NEW System.Drawing.Size(140, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:TabIndex = 15.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys_Label:Text = "&Source Buffer Keys (Context Menu for help):":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceBufferKeys */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys, "Source Buffer Keys":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourceBufferKeys":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Location = NEW System.Drawing.Point(150, 238).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Name = "smartBusinessEntityBindingSource1_eTable_SourceBufferKeys":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Size = NEW System.Drawing.Size(426, 67).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:TabIndex = 16.
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eTable":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 22 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "BusinessEntityName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TempTableName":U, "TempTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SourceTableNames":U, "SourceTableNames":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("SourceBufferNames":U, "SourceBufferNames":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("SourceDefaultQuery":U, "SourceDefaultQuery":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("SourceBufferKeys":U, "Source Buffer Keys":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("TempTablePurpose":U, "TempTablePurpose":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("TempTableDescription":U, "TempTableDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("TempTablePath":U, "TempTablePath":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("TempTableBeforeName":U, "TempTableBeforeName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("LayoutColumn":U, "LayoutColumn":U, Progress.Data.DataType:INTEGER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("LayoutRow":U, "LayoutRow":U, Progress.Data.DataType:INTEGER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("LayoutWidth":U, "LayoutWidth":U, Progress.Data.DataType:INTEGER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("LayoutHeight":U, "LayoutHeight":U, Progress.Data.DataType:INTEGER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("NamespaceUri":U, "NamespaceUri":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("NamespacePrefix":U, "NamespacePrefix":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("XmlNodeName":U, "XmlNodeName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("SerializeName":U, "SerializeName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[19] = NEW Progress.Data.ColumnPropDesc("NoBeforeTable":U, "No Before Table (read-only)":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[20] = NEW Progress.Data.ColumnPropDesc("EntityPackageName":U, "Table Package Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[21] = NEW Progress.Data.ColumnPropDesc("EntityClassName":U, "Table Class Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[22] = NEW Progress.Data.ColumnPropDesc("DeletedFields":U, "Deleted Fields":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_NoBeforeTable */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "NoBeforeTable":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:Location = NEW System.Drawing.Point(416, 87).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:Name = "smartBusinessEntityBindingSource1_eTable_NoBeforeTable":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:Size = NEW System.Drawing.Size(161, 17).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:TabIndex = 8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable:Text = "No Before Table (read-only)":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTableName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTableName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Name = "smartBusinessEntityBindingSource1_eTable_TempTableName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Validating:Subscribe(THIS-OBJECT:TempTableNameValidatingHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:Appearance = appearance5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:Location = NEW System.Drawing.Point(4, 167).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:Name = "smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:Size = NEW System.Drawing.Size(116, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:TabIndex = 13.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery_Label:Text = "&Source Default Query:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:Appearance = appearance6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:Location = NEW System.Drawing.Point(3, 115).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:Name = "smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:Size = NEW System.Drawing.Size(114, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:TabIndex = 9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames_Label:Text = "&Source Table Names:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:AlwaysInEditMode = TRUE.
        appearance19:BackColor = System.Drawing.Color:White.
        appearance19:BackColor2 = System.Drawing.Color:White.
        appearance19:FontData:Name = "Courier New":U.
        appearance19:FontData:SizeInPoints = Progress.Util.CastUtil:ToSingle(11).
        appearance19:ForeColor = System.Drawing.Color:Black.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Appearance = appearance19.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourceDefaultQuery":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Location = NEW System.Drawing.Point(150, 165).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Name = "smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:Size = NEW System.Drawing.Size(426, 67).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery:TabIndex = 14.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTableName_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:Appearance = appearance7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:Name = "smartBusinessEntityBindingSource1_eTable_TempTableName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:Size = NEW System.Drawing.Size(101, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName_Label:Text = "&Temp Table Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceTableNames */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourceTableNames":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:Location = NEW System.Drawing.Point(150, 111).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:Name = "smartBusinessEntityBindingSource1_eTable_SourceTableNames":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:TabIndex = 10.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTableBeforeName */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTableBeforeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Location = NEW System.Drawing.Point(150, 84).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Name = "smartBusinessEntityBindingSource1_eTable_TempTableBeforeName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Size = NEW System.Drawing.Size(260, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:TabIndex = 7.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:Appearance = appearance8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:Location = NEW System.Drawing.Point(3, 88).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:Name = "smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:Size = NEW System.Drawing.Size(137, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:TabIndex = 6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName_Label:Text = "&Temp Table Before Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:Appearance = appearance9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:Location = NEW System.Drawing.Point(3, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:Name = "smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:Size = NEW System.Drawing.Size(113, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose_Label:Text = "&Temp Table Purpose:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTablePath */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTablePath":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:Location = NEW System.Drawing.Point(150, 57).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:Name = "smartBusinessEntityBindingSource1_eTable_TempTablePath":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:TabIndex = 5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTablePurpose */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTablePurpose":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:Name = "smartBusinessEntityBindingSource1_eTable_TempTablePurpose":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose:TabIndex = 3.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTablePath_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:Appearance = appearance10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:Location = NEW System.Drawing.Point(3, 61).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:Name = "smartBusinessEntityBindingSource1_eTable_TempTablePath_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:Size = NEW System.Drawing.Size(94, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_Label:Text = "&Temp Table Path:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:Appearance = appearance11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:Location = NEW System.Drawing.Point(4, 142).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:Name = "smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:Size = NEW System.Drawing.Size(115, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:TabIndex = 11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames_Label:Text = "&Source Buffer Names:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SourceBufferNames */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourceBufferNames":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Location = NEW System.Drawing.Point(150, 138).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Name = "smartBusinessEntityBindingSource1_eTable_SourceBufferNames":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:TabIndex = 12.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label */
        /*  */
        appearance12:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:Appearance = appearance12.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:Location = NEW System.Drawing.Point(4, 315).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:Name = "smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:Size = NEW System.Drawing.Size(90, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:TabIndex = 17.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri_Label:Text = "Namespace &URI:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_NamespaceUri */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "NamespaceUri":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:Location = NEW System.Drawing.Point(150, 311).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:Name = "smartBusinessEntityBindingSource1_eTable_NamespaceUri":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri:TabIndex = 18.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label */
        /*  */
        appearance13:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:Appearance = appearance13.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:Location = NEW System.Drawing.Point(4, 342).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:Name = "smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:Size = NEW System.Drawing.Size(100, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:TabIndex = 19.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix_Label:Text = "Namespace &Prefix:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_NamespacePrefix */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "NamespacePrefix":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:Location = NEW System.Drawing.Point(150, 338).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:Name = "smartBusinessEntityBindingSource1_eTable_NamespacePrefix":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix:TabIndex = 20.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label */
        /*  */
        appearance14:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:Appearance = appearance14.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:Location = NEW System.Drawing.Point(7, 368).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:Name = "smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:Size = NEW System.Drawing.Size(90, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:TabIndex = 21.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName_Label:Text = "&Xml Node Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_XmlNodeName */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "XmlNodeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:Location = NEW System.Drawing.Point(150, 364).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:Name = "smartBusinessEntityBindingSource1_eTable_XmlNodeName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName:TabIndex = 22.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SerializeName_Label */
        /*  */
        appearance15:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:Appearance = appearance15.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:Location = NEW System.Drawing.Point(7, 395).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:Name = "smartBusinessEntityBindingSource1_eTable_SerializeName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:Size = NEW System.Drawing.Size(84, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:TabIndex = 23.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName_Label:Text = "Seriali&ze Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_SerializeName */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SerializeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:Location = NEW System.Drawing.Point(150, 391).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:Name = "smartBusinessEntityBindingSource1_eTable_SerializeName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName:TabIndex = 24.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label */
        /*  */
        appearance16:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:Appearance = appearance16.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:Location = NEW System.Drawing.Point(7, 422).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:Name = "smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:Size = NEW System.Drawing.Size(116, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:TabIndex = 25.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName_Label:Text = "Table &Package Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_EntityPackageName */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EntityPackageName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:Location = NEW System.Drawing.Point(150, 418).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:Name = "smartBusinessEntityBindingSource1_eTable_EntityPackageName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName:TabIndex = 26.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_EntityClassName_Label */
        /*  */
        appearance17:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:Appearance = appearance17.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:Location = NEW System.Drawing.Point(7, 448).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:Name = "smartBusinessEntityBindingSource1_eTable_EntityClassName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:Size = NEW System.Drawing.Size(100, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:TabIndex = 27.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName_Label:Text = "Table &Class Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_EntityClassName */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EntityClassName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:Location = NEW System.Drawing.Point(150, 444).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:Name = "smartBusinessEntityBindingSource1_eTable_EntityClassName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName:TabIndex = 28.
        /*  */
        /* eTable_DeletedFields_Label */
        /*  */
        appearance18:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eTable_DeletedFields_Label:Appearance = appearance18.
        THIS-OBJECT:eTable_DeletedFields_Label:AutoSize = TRUE.
        THIS-OBJECT:eTable_DeletedFields_Label:Location = NEW System.Drawing.Point(7, 475).
        THIS-OBJECT:eTable_DeletedFields_Label:Name = "eTable_DeletedFields_Label":U.
        THIS-OBJECT:eTable_DeletedFields_Label:Size = NEW System.Drawing.Size(80, 14).
        THIS-OBJECT:eTable_DeletedFields_Label:TabIndex = 29.
        THIS-OBJECT:eTable_DeletedFields_Label:Text = "&Deleted Fields:":U.
        /*  */
        /* eTable_DeletedFields */
        /*  */
        DEFINE VARIABLE nestedvar15 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar15 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eTable_DeletedFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar15, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eTable_DeletedFields:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DeletedFields":U, TRUE)).
        THIS-OBJECT:eTable_DeletedFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:eTable_DeletedFields:Location = NEW System.Drawing.Point(150, 471).
        THIS-OBJECT:eTable_DeletedFields:Name = "eTable_DeletedFields":U.
        THIS-OBJECT:eTable_DeletedFields:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:eTable_DeletedFields:TabIndex = 30.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(579, 474).
        /*  */
        /* smartBusinessEntityBindingSource1_eTable_TempTableDescription */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TempTableDescription":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:Name = "smartBusinessEntityBindingSource1_eTable_TempTableDescription":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:Size = NEW System.Drawing.Size(579, 474).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription:TabIndex = 0.
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
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(581, 522).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Temp-Table":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "Description":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar1[1] = ultraTab1.
        arrayvar1[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar1).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(579, 499).
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        popupMenuTool1:SharedPropsInternal:Caption = "Source Buffer Keys":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar2[1] = buttonTool4.
        arrayvar2[2] = buttonTool5.
        arrayvar2[3] = buttonTool6.
        popupMenuTool1:Tools:AddRange(arrayvar2).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool1:SharedPropsInternal:Caption = "Clear":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool2:SharedPropsInternal:Caption = "Key Fields":U.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool3:SharedPropsInternal:Caption = "ROWID":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar3[1] = popupMenuTool1.
        arrayvar3[2] = buttonTool1.
        arrayvar3[3] = buttonTool2.
        arrayvar3[4] = buttonTool3.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar3).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_MemberTableViewerControl_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:Name = "m_MemberTableViewerControl_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 522).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_MemberTableViewerControl_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(581, 0).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:Name = "m_MemberTableViewerControl_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 522).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_MemberTableViewerControl_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:Name = "m_MemberTableViewerControl_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(581, 0).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 522).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:Name = "m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(581, 0).
        THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* MemberTableViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_MemberTableViewerControl_Toolbars_Dock_Area_Top).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName.
        THIS-OBJECT:Name = "MemberTableViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(581, 522).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NoBeforeTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceDefaultQuery, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePurpose, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespaceUri, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_NamespacePrefix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_XmlNodeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SerializeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityPackageName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_EntityClassName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eTable_DeletedFields, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
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
                WHEN smartBusinessEntityBindingSource1_eTable_TempTablePath
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE .
&ENDIF
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the Source Buffer Key phrase to Buffer Fields
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetKeysFields ():

        DEFINE VARIABLE cKeys    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cBuffers AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTables  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFields  AS CHARACTER NO-UNDO .

        DEFINE VARIABLE hBuffer  AS HANDLE    NO-UNDO .

        IF NOT THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Enabled OR
           THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:ReadOnly THEN
            RETURN .

        ASSIGN cBuffers = THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Text
               cTables  = THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceTableNames:Text .

        IF cBuffers > "":U THEN .
        ELSE DO:
            MessageFormHelper:ShowMessage ("Can only assign Keys when temp-table has source buffers.":U,
                                           "Business Entity Designer":U,
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        IF NUM-ENTRIES (cBuffers) <> NUM-ENTRIES (cTables) THEN DO:
            MessageFormHelper:ShowMessage ("Buffer and Table settings do not match.":U,
                                           "Business Entity Designer":U,
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        IF NUM-ENTRIES (cBuffers) = 1 THEN DO:
            DO ON ERROR UNDO, THROW:

                CREATE BUFFER hBuffer FOR TABLE ENTRY (1, cTables) .

                ASSIGN cFields = BufferHelper:UniqueKeyFields (hBuffer, FALSE, FALSE) .

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hBuffer) .
                END FINALLY.
            END.

            IF cFields > "":U THEN
                cKeys = SUBSTITUTE ("KEYS (&1)":U, cFields) .
            ELSE
                cKeys = "KEYS (<PLEASE SPECIFY>)":U .
        END.
        ELSE
            DO i = 1 TO NUM-ENTRIES (cBuffers):
                DO ON ERROR UNDO, THROW:

                    CREATE BUFFER hBuffer FOR TABLE ENTRY (i, cTables) .

                    ASSIGN cFields = BufferHelper:UniqueKeyFields (hBuffer, FALSE, FALSE) .

                    FINALLY:
                        GarbageCollectorHelper:DeleteObject (hBuffer) .
                    END FINALLY.
                END.

                IF cFields > "":U THEN
                    cKeys = cKeys + SUBSTITUTE ("&1 KEYS (&2), ":U,
                                                ENTRY (i, cBuffers),
                                                cFields) .
                ELSE
                    cKeys = cKeys + SUBSTITUTE ("&1 KEYS (<PLEASE SPECIFY>), ":U,
                                                ENTRY (i, cBuffers)) .
            END.

        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Text = TRIM (cKeys, ", ":U) .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Source Buffer Key phrase to ROWID
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetKeysRowid ():

        DEFINE VARIABLE cKeys    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cBuffers AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .

        IF NOT THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Enabled OR
           THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:ReadOnly THEN
            RETURN .

        ASSIGN cBuffers = THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferNames:Text .

        IF cBuffers > "":U THEN .
        ELSE DO:
            MessageFormHelper:ShowMessage ("Can only assign Keys when temp-table has source buffers.":U,
                                           "Business Entity Designer":U,
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        IF NUM-ENTRIES (cBuffers) = 1 THEN
            cKeys = "KEYS (ROWID)":U .
        ELSE
            DO i = 1 TO NUM-ENTRIES (cBuffers):
                cKeys = cKeys + SUBSTITUTE ("&1 KEYS (ROWID), ":U,
                                            ENTRY (i, cBuffers)) .
            END.

        THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Text = TRIM (cKeys, ", ":U) .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the
                 smartBusinessEntityBindingSource1_eTable_TempTablePath instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eTable_TempTablePath_EditorButtonClick
                (sender AS System.Object,
                 e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        DEFINE VARIABLE oFolderBrowserDialog AS Consultingwerk.Windows.Util.Forms.FolderBrowserForm NO-UNDO .

        FILE-INFORMATION:FILE-NAME = ".":U .

        oFolderBrowserDialog = NEW Consultingwerk.Windows.Util.Forms.FolderBrowserForm () .
        oFolderBrowserDialog:RootFolder = FILE-INFORMATION:FULL-PATHNAME .

        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO:
            FILE-INFORMATION:FILE-NAME = ".":U .

            IF oFolderBrowserDialog:SelectedFolder BEGINS FILE-INFORMATION:FULL-PATHNAME THEN
                ASSIGN THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTablePath:Text =
                        REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, "/":U)
                       THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into relative path."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Validating event of the TempTableName field
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID TempTableNameValidatingHandler (sender AS System.Object,
                                                        e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE cBeforeTableName AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Focused OR
           NOT THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Enabled THEN
            RETURN .

        IF THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Text =
           THIS-OBJECT:smartDataSource:GetFieldValues ("TempTableName":U) THEN
              RETURN .

        ASSIGN cBeforeTableName = CAST (CAST (THIS-OBJECT:SmartDataSource, SmartDatasetChildAdapter):DatasetController,
                                        BusinessEntityDatasetController):BeforeTableName
                            (THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableName:Text) .

        IF cBeforeTableName <> THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Text THEN DO:

            IF MessageFormHelper:AskQuestion ("Update before table name to match temp-table name?":U) = DialogResultEnum:DialogResultYes THEN
                THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_TempTableBeforeName:Text = cBeforeTableName .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "Clear":U THEN DO:
                IF NOT THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Enabled OR
                   THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:ReadOnly THEN
                    RETURN .

                THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Text = "":U .

                IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            END.
            WHEN "Key Fields":U THEN DO:
                THIS-OBJECT:SetKeysFields () .
            END.
            WHEN "ROWID":U THEN DO:
                THIS-OBJECT:SetKeysRowid () .
            END.
        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates the values
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID VisualValidate ():

        IF THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Text > "":U AND NOT
           THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys:Text MATCHES "*KEYS*":U THEN

            UNDO, THROW NEW ClientValidationError ("Source Buffer Keys phrase must contain ~"KEYS~" keyword!"{&TRAN}, 0,
                                                   THIS-OBJECT:smartBusinessEntityBindingSource1_eTable_SourceBufferKeys) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MemberTableViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC MemberTableViewerControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
