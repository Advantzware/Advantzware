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
    File        : WebViewerDesignerForm
    Purpose     : Designer form for a Dynamic Web Viewer Control
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 08 15:51:29 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Design.SmartComponents.*.
USING Consultingwerk.SmartComponents.Base.*.
USING Consultingwerk.Design.SmartComponents.Viewer.* .
USING Consultingwerk.Forms.BaseForm.
USING Consultingwerk.SmartComponents.Implementation.* .
USING Consultingwerk.Util.* .
USING Progress.Lang.*.

CLASS Consultingwerk.Web.Studio.Viewer.WebViewerDesignerForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingSourceFieldsControl1 AS Consultingwerk.Design.SmartComponents.BindingSourceFieldsControl NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow2 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE propertySheet1 AS Consultingwerk.Design.SmartComponents.PropertySheet NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE smartViewerDesignerControl1 AS Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea2 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WebViewerDesignerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WebViewerDesignerForm ():


        SUPER().
        InitializeComponent().

        THIS-OBJECT:smartViewerDesignerControl1:FieldCreated:Subscribe
            (FieldCreatedHandler) .

        /* Mike Fechner, Consultingwerk Ltd. 12.09.2011
           Initialize Visual Designer */
        smartViewerDesignerControl1:CreatePropertySheet = FALSE .
        smartViewerDesignerControl1:CreateToolboxForm = FALSE .

        smartViewerDesignerControl1:BindingSource = bindingSource1 .
        bindingSourceFieldsControl1:BindingSource = bindingSource1 .


        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Handles the creation of new fields on the Design canvas
        Notes:
        @param sender The event sender
        @param e The FieldCreatedEventArgs instance of the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FieldCreatedHandler (sender AS Progress.Lang.Object,
                                               e AS FieldCreatedEventArgs):

        IF TYPE-OF (e:Control, System.Windows.Forms.TextBox) THEN
            CAST (e:Control, System.Windows.Forms.TextBox):MultiLine = TRUE .


        IF TYPE-OF (e:Control, System.Windows.Forms.CheckBox) THEN DO:

            {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in bindingSource1:TableSchema:Columns}

                IF oColumn:ColumnName = e:Control:Name THEN DO:
                    e:Control:Text = oColumn:ColumnLabel.

                    LEAVE.
                END.
            END.
        END.

    END METHOD.

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
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("transaction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Design":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save Design":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("f92dd2cf-c208-4ec4-8a47-22d4ea75b034":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("99ff5049-cdda-4136-9f89-1bfe41fd734c":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("f92dd2cf-c208-4ec4-8a47-22d4ea75b034":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedBottom, NEW System.Guid("67e6b050-9389-4671-9503-f88bc6c27b82":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("6728cf8d-f987-4b5f-96f3-d345dc88d7ce":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("67e6b050-9389-4671-9503-f88bc6c27b82":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eCustomer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("eSalesrep":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Web.Studio.Viewer.WebViewerDesignerForm":U).
        THIS-OBJECT:bindingSourceFieldsControl1 = NEW Consultingwerk.Design.SmartComponents.BindingSourceFieldsControl().
        THIS-OBJECT:propertySheet1 = NEW Consultingwerk.Design.SmartComponents.PropertySheet().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:smartViewerDesignerControl1 = NEW Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:windowDockingArea2 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow2 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea2:SuspendLayout().
        THIS-OBJECT:dockableWindow2:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSourceFieldsControl1 */
        /*  */
        THIS-OBJECT:bindingSourceFieldsControl1:BindingSource = ?.
        THIS-OBJECT:bindingSourceFieldsControl1:DesignerHost = ?.
        THIS-OBJECT:bindingSourceFieldsControl1:Location = NEW System.Drawing.Point(0, 20).
        THIS-OBJECT:bindingSourceFieldsControl1:Name = "bindingSourceFieldsControl1":U.
        THIS-OBJECT:bindingSourceFieldsControl1:Size = NEW System.Drawing.Size(140, 387).
        THIS-OBJECT:bindingSourceFieldsControl1:TabIndex = 14.
        /*  */
        /* propertySheet1 */
        /*  */
        THIS-OBJECT:propertySheet1:Location = NEW System.Drawing.Point(0, 20).
        THIS-OBJECT:propertySheet1:Name = "propertySheet1":U.
        THIS-OBJECT:propertySheet1:SelectionService = ?.
        THIS-OBJECT:propertySheet1:Size = NEW System.Drawing.Size(581, 94).
        THIS-OBJECT:propertySheet1:TabIndex = 26.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        arrayvar1[2] = buttonTool3.
        arrayvar1[3] = buttonTool4.
        arrayvar1[4] = buttonTool5.
        arrayvar1[5] = buttonTool6.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Navigation":U.
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool8.
        arrayvar2[2] = buttonTool9.
        arrayvar2[3] = buttonTool10.
        arrayvar2[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Maintenance":U.
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool12.
        arrayvar3[2] = buttonTool13.
        arrayvar3[3] = buttonTool14.
        arrayvar3[4] = buttonTool15.
        arrayvar3[5] = buttonTool16.
        arrayvar3[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        ribbonGroup3:Caption = "Transaction":U.
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar4[1] = buttonTool18.
        arrayvar4[2] = buttonTool19.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        ribbonGroup4:Caption = "ribbonGroup1":U.
        buttonTool48:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = buttonTool48.
        ribbonGroup4:Tools:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 4 NO-UNDO.
        arrayvar6[1] = ribbonGroup1.
        arrayvar6[2] = ribbonGroup2.
        arrayvar6[3] = ribbonGroup3.
        arrayvar6[4] = ribbonGroup4.
        ribbonTab1:Groups:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar7[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar8[1] = buttonTool20.
        arrayvar8[2] = buttonTool21.
        arrayvar8[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar8).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:ScenicRibbon.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar9[1] = buttonTool23.
        arrayvar9[2] = buttonTool24.
        arrayvar9[3] = buttonTool25.
        arrayvar9[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar10[1] = buttonTool27.
        arrayvar10[2] = buttonTool28.
        arrayvar10[3] = buttonTool29.
        arrayvar10[4] = buttonTool30.
        arrayvar10[5] = buttonTool31.
        arrayvar10[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar11[1] = buttonTool33.
        arrayvar11[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar11).
        ultraToolbar3:Text = "Transaction Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar12[1] = ultraToolbar1.
        arrayvar12[2] = ultraToolbar2.
        arrayvar12[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar12).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool35:SharedPropsInternal:Caption = "First record":U.
        buttonTool35:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = "First record":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool36:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool36:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool37:SharedPropsInternal:Caption = "Next record":U.
        buttonTool37:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool38:SharedPropsInternal:Caption = "Last record":U.
        buttonTool38:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool39:SharedPropsInternal:Caption = "Add record":U.
        buttonTool39:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool40:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool40:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool41:SharedPropsInternal:Caption = "Update record":U.
        buttonTool41:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool42:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool42:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        buttonTool43:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool43:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool44:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool44:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool45:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool46:SharedPropsInternal:Caption = "Commit":U.
        buttonTool46:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance14.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool47:SharedPropsInternal:Caption = "Revert":U.
        buttonTool47:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        buttonTool49:SharedPropsInternal:Caption = "Save Design":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 14 NO-UNDO.
        arrayvar13[1] = buttonTool35.
        arrayvar13[2] = buttonTool36.
        arrayvar13[3] = buttonTool37.
        arrayvar13[4] = buttonTool38.
        arrayvar13[5] = buttonTool39.
        arrayvar13[6] = buttonTool40.
        arrayvar13[7] = buttonTool41.
        arrayvar13[8] = buttonTool42.
        arrayvar13[9] = buttonTool43.
        arrayvar13[10] = buttonTool44.
        arrayvar13[11] = buttonTool45.
        arrayvar13[12] = buttonTool46.
        arrayvar13[13] = buttonTool47.
        arrayvar13[14] = buttonTool49.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar13).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* m_WebViewerDesignerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(223)), System.Convert:ToInt32(System.Convert:ToByte(233)), System.Convert:ToInt32(System.Convert:ToByte(245))).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 153).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:Name = "m_WebViewerDesignerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 407).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_WebViewerDesignerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(223)), System.Convert:ToInt32(System.Convert:ToByte(233)), System.Convert:ToInt32(System.Convert:ToByte(245))).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(734, 153).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:Name = "m_WebViewerDesignerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 407).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_WebViewerDesignerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(223)), System.Convert:ToInt32(System.Convert:ToByte(233)), System.Convert:ToInt32(System.Convert:ToByte(245))).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:Name = "m_WebViewerDesignerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(742, 153).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(223)), System.Convert:ToInt32(System.Convert:ToByte(233)), System.Convert:ToInt32(System.Convert:ToByte(245))).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 560).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:Name = "m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(742, 8).
        THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:VisualStudio2008.
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        dockAreaPane1:DockedBefore = NEW System.Guid("67e6b050-9389-4671-9503-f88bc6c27b82":U).
        dockableControlPane1:Control = THIS-OBJECT:bindingSourceFieldsControl1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(86, 156, 224, 500).
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "bindingSourceFieldsControl1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar14[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar14).
        dockAreaPane1:Size = NEW System.Drawing.Size(140, 407).
        dockableControlPane2:Control = THIS-OBJECT:propertySheet1.
        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(11, 435, 420, 119).
        dockableControlPane2:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane2:Text = "propertySheet1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar15[1] = dockableControlPane2.
        dockAreaPane2:Panes:AddRange(arrayvar15).
        dockAreaPane2:Size = NEW System.Drawing.Size(581, 114).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 2 NO-UNDO.
        arrayvar16[1] = dockAreaPane1.
        arrayvar16[2] = dockAreaPane2.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar16).
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:UnpinnedTabStyle = Infragistics.Win.UltraWinTabs.TabStyle:Office2010Ribbon.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:VisualStudio2008.
        /*  */
        /* m_WebViewerDesignerFormUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(8, 153).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Name = "m_WebViewerDesignerFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 407).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft:TabIndex = 4.
        /*  */
        /* m_WebViewerDesignerFormUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(734, 153).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Name = "m_WebViewerDesignerFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 407).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight:TabIndex = 5.
        /*  */
        /* m_WebViewerDesignerFormUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(8, 153).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Name = "m_WebViewerDesignerFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(726, 0).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop:TabIndex = 6.
        /*  */
        /* m_WebViewerDesignerFormUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(8, 560).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Name = "m_WebViewerDesignerFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(726, 0).
        THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom:TabIndex = 7.
        /*  */
        /* m_WebViewerDesignerFormAutoHideControl */
        /*  */
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:Name = "m_WebViewerDesignerFormAutoHideControl":U.
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:Size = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl:TabIndex = 8.
        /*  */
        /* smartViewerDesignerControl1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartViewerDesignerControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartViewerDesignerControl1:BackColor = System.Drawing.Color:Gray.
        THIS-OBJECT:smartViewerDesignerControl1:BindingSource = ?.
        THIS-OBJECT:smartViewerDesignerControl1:DesignerToolbar = ?.
        THIS-OBJECT:smartViewerDesignerControl1:Location = NEW System.Drawing.Point(8, 155).
        THIS-OBJECT:smartViewerDesignerControl1:Name = "smartViewerDesignerControl1":U.
        THIS-OBJECT:smartViewerDesignerControl1:Size = NEW System.Drawing.Size(563, 277).
        THIS-OBJECT:smartViewerDesignerControl1:SourceControl = ?.
        THIS-OBJECT:smartViewerDesignerControl1:TabIndex = 13.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(589, 153).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(145, 407).
        THIS-OBJECT:windowDockingArea1:TabIndex = 15.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:bindingSourceFieldsControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(5, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(140, 407).
        THIS-OBJECT:dockableWindow1:TabIndex = 32.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar17 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar17.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar18[1] = NEW Progress.Data.ColumnPropDesc("SalesRep":U, "Sales Rep":U, Progress.Data.DataType:CHARACTER).
        arrayvar18[2] = NEW Progress.Data.ColumnPropDesc("RepName":U, "Rep Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar18[3] = NEW Progress.Data.ColumnPropDesc("Region":U, ?, Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar18.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
        arrayvar19[1] = tableDesc2.
        tableDesc1:ChildTables = arrayvar19.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Progress.Data.ColumnPropDesc EXTENT 19 NO-UNDO.
        arrayvar20[1] = NEW Progress.Data.ColumnPropDesc("CustNum":U, "Cust Num":U, Progress.Data.DataType:INTEGER).
        arrayvar20[2] = NEW Progress.Data.ColumnPropDesc("Country":U, "Country":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[3] = NEW Progress.Data.ColumnPropDesc("Name":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[4] = NEW Progress.Data.ColumnPropDesc("Address":U, "Address":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[5] = NEW Progress.Data.ColumnPropDesc("Address2":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[6] = NEW Progress.Data.ColumnPropDesc("City":U, "City":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[7] = NEW Progress.Data.ColumnPropDesc("State":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[8] = NEW Progress.Data.ColumnPropDesc("PostalCode":U, "Postal Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[9] = NEW Progress.Data.ColumnPropDesc("Contact":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[10] = NEW Progress.Data.ColumnPropDesc("Phone":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[11] = NEW Progress.Data.ColumnPropDesc("SalesRep":U, "Sales Rep":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[12] = NEW Progress.Data.ColumnPropDesc("CreditLimit":U, "Credit Limit":U, Progress.Data.DataType:DECIMAL).
        arrayvar20[13] = NEW Progress.Data.ColumnPropDesc("Balance":U, ?, Progress.Data.DataType:DECIMAL).
        arrayvar20[14] = NEW Progress.Data.ColumnPropDesc("Terms":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[15] = NEW Progress.Data.ColumnPropDesc("Discount":U, ?, Progress.Data.DataType:INTEGER).
        arrayvar20[16] = NEW Progress.Data.ColumnPropDesc("Comments":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[17] = NEW Progress.Data.ColumnPropDesc("Fax":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar20[18] = NEW Progress.Data.ColumnPropDesc("EmailAddress":U, "Email":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[19] = NEW Progress.Data.ColumnPropDesc("Flags":U, "Notes":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar20.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* windowDockingArea2 */
        /*  */
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow2).
        THIS-OBJECT:windowDockingArea2:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:windowDockingArea2:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea2:Location = NEW System.Drawing.Point(8, 441).
        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2":U.
        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea2:Size = NEW System.Drawing.Size(581, 119).
        THIS-OBJECT:windowDockingArea2:TabIndex = 27.
        /*  */
        /* dockableWindow2 */
        /*  */
        THIS-OBJECT:dockableWindow2:Controls:Add(THIS-OBJECT:propertySheet1).
        THIS-OBJECT:dockableWindow2:Location = NEW System.Drawing.Point(0, 5).
        THIS-OBJECT:dockableWindow2:Name = "dockableWindow2":U.
        THIS-OBJECT:dockableWindow2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow2:Size = NEW System.Drawing.Size(581, 114).
        THIS-OBJECT:dockableWindow2:TabIndex = 33.
        /*  */
        /* WebViewerDesignerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(742, 568).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartViewerDesignerControl1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_WebViewerDesignerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "WebViewerDesignerForm":U.
        THIS-OBJECT:Text = "SmartComponents.Web Viewer Designer":U.
        THIS-OBJECT:WindowPositionRegistryKey = "WebViewerDesignerForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:windowDockingArea1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:windowDockingArea2:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow2:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The default .NET System.EventArgs
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

/*        DEFINE VARIABLE oBPMViewer AS DynamicWorkflowItemViewerControl NO-UNDO .*/
        DEFINE VARIABLE oDesignAdapter AS SmartTempTableAdapter NO-UNDO .

/*        THIS-OBJECT:Text = SUBSTITUTE ("Design: &1 / &2",              */
/*                                       THIS-OBJECT:ProcessTemplateName,*/
/*                                       THIS-OBJECT:WorkStepName) .     */

        SUPER:OnLoad (e) .

        smartViewerDesignerControl1:DesignerToolbar = smartToolbarController1 .

        propertySheet1:Designer = smartViewerDesignerControl1 .

/*        IF NOT THIS-OBJECT:NewDesign THEN DO:                                                                */
/*                                                                                                             */
/*            oBPMViewer = NEW DynamicWorkflowItemViewerControl () .                                           */
/*            oBPMViewer:ProcessTemplateName = THIS-OBJECT:ProcessTemplateName .                               */
/*            oBPMViewer:WorkStepName = THIS-OBJECT:WorkStepName .                                             */
/*            oBPMViewer:SubstituteTypes = FALSE .                                                             */
/*            oBPMViewer:LoadSettings () .                                                                     */
/*                                                                                                             */
/*            oDesignAdapter = NEW SmartTempTableAdapter () .                                                  */
/*            oDesignAdapter:TempTableHandle = THIS-OBJECT:TemplateDataset:GET-BUFFER-HANDLE (1):TABLE-HANDLE .*/
/*            oDesignAdapter:RetrieveData () .                                                                 */
/*            oBPMViewer:SmartDataSource = oDesignAdapter .                                                    */
/*                                                                                                             */
/*            oBPMViewer:SubstituteDynamicControls = smartViewerDesignerControl1:SubstituteDynamicControls .   */
/*                                                                                                             */
/*            oBPMViewer:InitializeControls () .                                                               */
/*                                                                                                             */
/*            smartViewerDesignerControl1:SourceControl = oBPMViewer  .                                        */
/*        END.                                                                                                 */

        smartViewerDesignerControl1:Initialize() .

        /* Mike Fechner, Consultingwerk Ltd. 16.09.2011
           Set SourceControl after SmartViewerDesignerControl:Initialize() to
           avoid copying the control from here */
        smartViewerDesignerControl1:SourceControl = THIS-OBJECT .

        smartViewerDesignerControl1:ServiceContainer:AddService(Progress.Util.TypeHelper:GetType("System.Drawing.Design.IToolboxService":U),
                                                                bindingSourceFieldsControl1) .
        bindingSourceFieldsControl1:DesignerHost = smartViewerDesignerControl1:DesignerHost .

        propertySheet1:SelectionService = CAST (smartViewerDesignerControl1:ServiceContainer:GetService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.ISelectionService":U)),
                                                System.ComponentModel.Design.ISelectionService) .

        /* Hide Components Area */
        smartViewerDesignerControl1:Size = NEW System.Drawing.Size (THIS-OBJECT:smartViewerDesignerControl1:Width,
                                                                    THIS-OBJECT:smartViewerDesignerControl1:Height + 80) .

/*        bindingSourceFieldsControl1:InsertSeparator () .                                                                         */
/*                                                                                                                                 */
/*        bindingSourceFieldsControl1:AddCustomToolboxItem ("Groupbox",                                                            */
/*                                                          "Consultingwerk/BPM/Images/shape_square.png",                          */
/*                                                          Progress.Util.TypeHelper:GetType ("System.Windows.Forms.GroupBox")) .  */
/*                                                                                                                                 */
/*        bindingSourceFieldsControl1:AddCustomToolboxItem ("Tabfolder",                                                           */
/*                                                          "Consultingwerk/BPM/Images/folder_closed.png",                         */
/*                                                          Progress.Util.TypeHelper:GetType ("System.Windows.Forms.TabControl")) .*/

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        THIS-OBJECT:bindingSourceFieldsControl1:DesignerControl = smartViewerDesignerControl1.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the design to an XML file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveDesign ():

        THIS-OBJECT:smartViewerDesignerControl1:Serialize (OUTPUT TABLE ttViewerDesign) .

        TEMP-TABLE ttViewerDesign:WRITE-XML ("file":U,
                                             "Consultingwerk/SmartComponentsDemo/Web/customer.viewer":U,
                                             TRUE) .

        THIS-OBJECT:smartViewerDesignerControl1:ShutDown () .
        THIS-OBJECT:Close () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "Save Design":U THEN
                SaveDesign () .

         END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the WebViewerDesignerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC WebViewerDesignerForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
