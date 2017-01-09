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
    File        : BusinessEntityViewerControl
    Purpose     : Viewer Control for the eBusinessEntity table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 12:23:19 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*                FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                            FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                             FROM ASSEMBLY .
USING Infragistics.Win.UltraWinTabControl.*                          FROM ASSEMBLY .
USING Progress.Lang.*                                                FROM PROPATH .
USING System.Windows.Forms.*                                         FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityViewerControl INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE eBusinessEntity_DistinctTempTableIncludeFiles AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eBusinessEntity_GenerateDatasetController AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eBusinessEntity_GenerateDataAccess AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eBusinessEntity_GenerateBusinessEntity AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraTabControl
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TabControl AS UltraTabControl NO-UNDO
    GET:
        RETURN ultraTabControl1 .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityViewerControl ():

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eBusinessEntity":U).
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
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
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
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton5 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton5 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
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
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl4:SuspendLayout().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateDataAccess, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateDatasetController, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(606, 321).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Leave:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Leave).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 19 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "BusinessEntityName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPurpose":U, "BusinessEntityPurpose":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("BusinessEntityDescription":U, "BusinessEntityDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPackage":U, "BusinessEntityPackage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("DataAccessName":U, "DataAccessName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("DataAccessPackage":U, "DataAccessPackage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("DatasetControllerName":U, "DatasetControllerName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("DatasetControllerPackage":U, "DatasetControllerPackage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("DatasetName":U, "DatasetName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("DatasetPath":U, "DatasetPath":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("DefaultTablePath":U, "DefaultTablePath":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("DefaultTablePrefix":U, "DefaultTablePrefix":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("DefaultTableSuffix":U, "DefaultTableSuffix":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("DefaultBeforeTablePrefix":U, "DefaultBeforeTablePrefix":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("DefaultBeforeTableSuffix":U, "DefaultBeforeTableSuffix":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("GenerateBusinessEntity":U, "Generate Business Entity":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("GenerateDataAccess":U, "Generate Data Access Class":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("GenerateDatasetController":U, "Generate Dataset Controller":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[19] = NEW Progress.Data.ColumnPropDesc("DistinctTempTableIncludeFiles":U, "Use Distinct Include Files for Temp-Table Definition":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:Appearance = appearance1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:Location = NEW System.Drawing.Point(5, 220).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:Size = NEW System.Drawing.Size(104, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:TabIndex = 14.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName_Label:Text = "&Data Access Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:Appearance = appearance2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:Location = NEW System.Drawing.Point(5, 170).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:Size = NEW System.Drawing.Size(80, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:TabIndex = 12.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName_Label:Text = "&Dataset Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataAccessName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Location = NEW System.Drawing.Point(150, 216).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:TabIndex = 15.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:Appearance = appearance3.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:Location = NEW System.Drawing.Point(5, 247).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:Size = NEW System.Drawing.Size(118, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:TabIndex = 16.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_Label:Text = "&Data Access Package:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:Appearance = appearance4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:Location = NEW System.Drawing.Point(5, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:Size = NEW System.Drawing.Size(118, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Label:Text = "&Business Entity Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataAccessPackage":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Location = NEW System.Drawing.Point(150, 243).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:TabIndex = 17.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatasetName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Location = NEW System.Drawing.Point(150, 166).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:TabIndex = 13.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityPackage":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Location = NEW System.Drawing.Point(150, 57).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:TabIndex = 5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Leave:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Leave).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton3:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatasetPath":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Location = NEW System.Drawing.Point(150, 139).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:TabIndex = 11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:Appearance = appearance5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:Location = NEW System.Drawing.Point(5, 61).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:Size = NEW System.Drawing.Size(132, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Label:Text = "&Business Entity Package:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:Appearance = appearance6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:Location = NEW System.Drawing.Point(5, 143).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:Size = NEW System.Drawing.Size(73, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:TabIndex = 10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_Label:Text = "&Dataset Path:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:Appearance = appearance7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:Location = NEW System.Drawing.Point(5, 88).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:Size = NEW System.Drawing.Size(132, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:TabIndex = 6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName_Label:Text = "&Dataset Controller Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:Appearance = appearance8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:Location = NEW System.Drawing.Point(5, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:Size = NEW System.Drawing.Size(130, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose_Label:Text = "&Business Entity Purpose:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatasetControllerName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Location = NEW System.Drawing.Point(150, 84).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:TabIndex = 7.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton4:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DatasetControllerPackage":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Location = NEW System.Drawing.Point(150, 111).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:TabIndex = 9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:Appearance = appearance9.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:Location = NEW System.Drawing.Point(5, 115).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:Size = NEW System.Drawing.Size(146, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:TabIndex = 8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_Label:Text = "&Dataset Controller Package:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityPurpose":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Size = NEW System.Drawing.Size(453, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:TabIndex = 3.
        /*  */
        /* eBusinessEntity_DistinctTempTableIncludeFiles */
        /*  */
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:AutoSize = TRUE.
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DistinctTempTableIncludeFiles":U, TRUE)).
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:Location = NEW System.Drawing.Point(150, 193).
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:Name = "eBusinessEntity_DistinctTempTableIncludeFiles":U.
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:Size = NEW System.Drawing.Size(279, 17).
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:TabIndex = 3.
        THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles:Text = "Use Distinct Include Files for Temp-Table Definition":U.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(606, 321).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityDescription":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:Size = NEW System.Drawing.Size(606, 321).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription:TabIndex = 0.
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix).
        THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(1, 22).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(606, 321).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton5:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:ButtonsRight:Add(editorButton5).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultTablePath":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Size = NEW System.Drawing.Size(419, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultBeforeTableSuffix":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:Location = NEW System.Drawing.Point(150, 111).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:Size = NEW System.Drawing.Size(419, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix:TabIndex = 9.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:Appearance = appearance10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:Size = NEW System.Drawing.Size(101, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_Label:Text = "&Default Table Path:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:Appearance = appearance11.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:Location = NEW System.Drawing.Point(3, 115).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:Size = NEW System.Drawing.Size(142, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:TabIndex = 8.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix_Label:Text = "&Default Before Table Suffix:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label */
        /*  */
        appearance12:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:Appearance = appearance12.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:Location = NEW System.Drawing.Point(3, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:Size = NEW System.Drawing.Size(107, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix_Label:Text = "&Default Table Prefix:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultBeforeTablePrefix":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:Location = NEW System.Drawing.Point(150, 84).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:Size = NEW System.Drawing.Size(419, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix:TabIndex = 7.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultTablePrefix":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:Size = NEW System.Drawing.Size(419, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix:TabIndex = 3.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label */
        /*  */
        appearance13:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:Appearance = appearance13.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:Location = NEW System.Drawing.Point(3, 88).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:Size = NEW System.Drawing.Size(143, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:TabIndex = 6.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix_Label:Text = "&Default Before Table Prefix:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label */
        /*  */
        appearance14:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:Appearance = appearance14.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:Location = NEW System.Drawing.Point(3, 61).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:Size = NEW System.Drawing.Size(106, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix_Label:Text = "&Default Table Suffix:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultTableSuffix":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:Location = NEW System.Drawing.Point(150, 57).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:Name = "smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:Size = NEW System.Drawing.Size(419, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix:TabIndex = 5.
        /*  */
        /* ultraTabPageControl4 */
        /*  */
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity).
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:eBusinessEntity_GenerateDataAccess).
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:eBusinessEntity_GenerateDatasetController).
        THIS-OBJECT:ultraTabPageControl4:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4":U.
        THIS-OBJECT:ultraTabPageControl4:Size = NEW System.Drawing.Size(606, 321).
        /*  */
        /* eBusinessEntity_GenerateBusinessEntity */
        /*  */
        appearance15:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:Appearance = appearance15.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:AutoSize = TRUE.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GenerateBusinessEntity":U, TRUE)).
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:Location = NEW System.Drawing.Point(41, 6).
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:Name = "eBusinessEntity_GenerateBusinessEntity":U.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:Size = NEW System.Drawing.Size(149, 17).
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:TabIndex = 3.
        THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity:Text = "Generate Business Entity":U.
        /*  */
        /* eBusinessEntity_GenerateDataAccess */
        /*  */
        appearance16:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:Appearance = appearance16.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:AutoSize = TRUE.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GenerateDataAccess":U, TRUE)).
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:Location = NEW System.Drawing.Point(41, 29).
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:Name = "eBusinessEntity_GenerateDataAccess":U.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:Size = NEW System.Drawing.Size(166, 17).
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:TabIndex = 5.
        THIS-OBJECT:eBusinessEntity_GenerateDataAccess:Text = "Generate Data Access Class":U.
        /*  */
        /* eBusinessEntity_GenerateDatasetController */
        /*  */
        appearance17:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:Appearance = appearance17.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:AutoSize = TRUE.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GenerateDatasetController":U, TRUE)).
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:Location = NEW System.Drawing.Point(41, 52).
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:Name = "eBusinessEntity_GenerateDatasetController":U.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:Size = NEW System.Drawing.Size(163, 17).
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:TabIndex = 7.
        THIS-OBJECT:eBusinessEntity_GenerateDatasetController:Text = "Generate Dataset Controller":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl4).
        THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(608, 344).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Business Entity":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "Description":U.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        ultraTab3:Text = "Temp-Table Defaults":U.
        ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl4.
        ultraTab4:Text = "Generate":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
        arrayvar1[1] = ultraTab1.
        arrayvar1[2] = ultraTab2.
        arrayvar1[3] = ultraTab3.
        arrayvar1[4] = ultraTab4.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar1).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(606, 321).
        /*  */
        /* BusinessEntityViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName.
        THIS-OBJECT:Name = "BusinessEntityViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(608, 344).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eBusinessEntity_DistinctTempTableIncludeFiles, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityDescription, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl3:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTableSuffix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultBeforeTablePrefix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePrefix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTableSuffix, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl4:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl4:PerformLayout().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateBusinessEntity, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateDataAccess, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eBusinessEntity_GenerateDatasetController, System.ComponentModel.ISupportInitialize):EndInit().
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
                WHEN smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .

                WHEN smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .

                WHEN smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .

                WHEN smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .

                WHEN smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath
                    THEN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE .
&ENDIF
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the VisualValidating event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnVisualValidating (INPUT e AS System.EventArgs):

        SUPER:OnVisualValidating(INPUT e).

        IF smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Focused THEN
            smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Leave (smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName,
                                                                                        System.EventArgs:Empty) .

        IF smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Focused THEN
            smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Leave (smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage,
                                                                                           System.EventArgs:Empty) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Leave event of the smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName_Leave (sender AS System.Object,
                                                                                                    e AS System.EventArgs):

        DEFINE VARIABLE oNameHandler           AS ICustomBusinessEntityNameHandler NO-UNDO .

        DEFINE VARIABLE cBusinessEntityName    AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE cDatasetControllerType AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE cBusinessEntityPurpose AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE cDatasetName           AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE cDataAccessName        AS CHARACTER                        NO-UNDO.

        IF THIS-OBJECT:CancellingUpdate THEN
            RETURN .

        oNameHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ICustomBusinessEntityNameHandler
                                                     "NEW CustomBusinessEntityNameHandler ()"} .

        ASSIGN cBusinessEntityName    = smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Text
               cDatasetControllerType = smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Text
               cBusinessEntityPurpose = smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Text
               cDatasetName           = smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Text
               cDataAccessName        = smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Text
            .

        IF NOT oNameHandler:DetermineBusinessEntityNameFields (INPUT-OUTPUT cBusinessEntityName,
                                                               INPUT-OUTPUT cDatasetControllerType,
                                                               INPUT-OUTPUT cBusinessEntityPurpose,
                                                               INPUT-OUTPUT cDatasetName,
                                                               INPUT-OUTPUT cDataAccessName) THEN
            RETURN .

        ASSIGN smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityName:Text    = cBusinessEntityName
               smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerName:Text = cDatasetControllerType
               smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPurpose:Text = cBusinessEntityPurpose
               smartBusinessEntityBindingSource1_eBusinessEntity_DatasetName:Text           = cDatasetName
               smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessName:Text        = cDataAccessName .

        IF NOT THIS-OBJECT:SavingChanges THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_EditorButtonClick
                    (sender AS System.Object,
                     e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE cRelative     AS CHARACTER                         NO-UNDO .

        DEFINE VARIABLE oFolderBrowserDialog AS Consultingwerk.Windows.Util.Forms.FolderBrowserForm NO-UNDO .

        FILE-INFORMATION:FILE-NAME = ".":U .

        oFolderBrowserDialog = NEW Consultingwerk.Windows.Util.Forms.FolderBrowserForm () .
        oFolderBrowserDialog:RootFolder = FILE-INFORMATION:FULL-PATHNAME .

        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO:
            FILE-INFORMATION:FILE-NAME = ".":U .

            ASSIGN cRelative = FileHelper:RelativeFileName(oFolderBrowserDialog:SelectedFolder) .

            IF SUBSTRING (cRelative, 2, 1, "CHARACTER":U) = ":":U OR cRelative BEGINS "~\~\":U OR cRelative BEGINS "/":U OR cRelative BEGINS ".":U THEN
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into package."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
            ELSE DO:
                THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Text =
                  REPLACE (cRelative, "~\":U, ".":U) .

                smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Leave
                  (smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage, System.EventArgs:Empty) .

                IF NOT THIS-OBJECT:SavingChanges THEN
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Leave event of the smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage_Leave (sender AS System.Object,
                                                                                                       e AS System.EventArgs ):

        DEFINE VARIABLE oPackageHandler           AS ICustomBusinessEntityPackageNameHandler NO-UNDO .

        DEFINE VARIABLE cBusinessEntityPackage    AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cDatasetControllerPackage AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cDatasetPath              AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cDefaultTablePath         AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cDataAccessPackage        AS CHARACTER                               NO-UNDO .

        IF THIS-OBJECT:CancellingUpdate THEN
            RETURN .

        oPackageHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ICustomBusinessEntityPackageNameHandler
                                                        "NEW CustomBusinessEntityPackageNameHandler ()"} .

        ASSIGN cBusinessEntityPackage    = smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Text
               cDatasetControllerPackage = smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Text
               cDatasetPath              = smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Text
               cDefaultTablePath         = smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Text
               cDataAccessPackage        = smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Text
                .

        IF NOT oPackageHandler:DetermineBusinessEntityPackageFields (INPUT-OUTPUT cBusinessEntityPackage,
                                                                     INPUT-OUTPUT cDatasetControllerPackage,
                                                                     INPUT-OUTPUT cDatasetPath,
                                                                     INPUT-OUTPUT cDefaultTablePath,
                                                                     INPUT-OUTPUT cDataAccessPackage) THEN
            RETURN .

        ASSIGN  smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage:Text    = cBusinessEntityPackage
                smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Text = cDatasetControllerPackage
                smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Text              = cDatasetPath
                smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Text         = cDefaultTablePath
                smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Text        = cDataAccessPackage .

        IF NOT THIS-OBJECT:SavingChanges THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
                .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage_EditorButtonClick
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
                ASSIGN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage:Text =
                        REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, ".":U)
                       THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into package."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage_EditorButtonClick
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
                ASSIGN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage:Text =
                        REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, ".":U)
                       THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into package."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath_EditorButtonClick
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
                ASSIGN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetPath:Text =
                        REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, "/":U)
                       THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into relative path."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath_EditorButtonClick
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
                ASSIGN THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DefaultTablePath:Text =
                        SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U)
                       THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into relative path."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates a Package Label
        Notes:   Used by the VisualValidate Method
        @param poControl The UltraTextEditor to validate the value from
        @param pcPackageLabel The label of the package for the validation message
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ValidatePackageName (poControl AS UltraTextEditor,
                                               pcPackageLabel AS CHARACTER):

        DEFINE VARIABLE cPackage AS CHARACTER NO-UNDO.

        ASSIGN cPackage = poControl:Text .

        IF cPackage > "":U THEN .
        ELSE
            UNDO, THROW NEW ClientValidationError (SUBSTITUTE ("&1 may not be empty."{&TRAN}, pcPackageLabel), 0, poControl) .

        IF INDEX (cPackage, " ":U) > 0 THEN
            UNDO, THROW NEW ClientValidationError (SUBSTITUTE ("&1 may not contain spaces."{&TRAN}, pcPackageLabel), 0, poControl) .

        IF INDEX (cPackage, "/":U) > 0 THEN
            UNDO, THROW NEW ClientValidationError (SUBSTITUTE ("&1 may not contain slashes."{&TRAN}, pcPackageLabel), 0, poControl) .

        IF INDEX (cPackage, "~\":U) > 0 THEN
            UNDO, THROW NEW ClientValidationError (SUBSTITUTE ("&1 may not contain back slashes."{&TRAN}, pcPackageLabel), 0, poControl) .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates the values
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID VisualValidate ():

        ValidatePackageName (THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage, "Business Entity Package"{&TRAN}) .
        ValidatePackageName (THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DataAccessPackage, "Data Access Object Package"{&TRAN}) .
        ValidatePackageName (THIS-OBJECT:smartBusinessEntityBindingSource1_eBusinessEntity_DatasetControllerPackage, "Dataset Controller Package"{&TRAN}) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityViewerControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.