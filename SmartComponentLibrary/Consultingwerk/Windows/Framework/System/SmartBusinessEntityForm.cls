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
    File        : SmartBusinessEntityForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 19:06:59
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.System.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*     FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Consultingwerk.Framework.Enum.*           FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartBusinessEntityForm INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSourceMain AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSourceChild AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityTableViewerControl AS Consultingwerk.Windows.Framework.System.SmartBusinessEntityTableViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityViewerControl AS Consultingwerk.Windows.Framework.System.SmartBusinessEntityViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowserSmartBusinessEntity AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowserTable AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDatasetChildAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartBusinessEntityForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter2 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    DEFINE VARIABLE lCreatingRecord AS LOGICAL NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityForm ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartBusinessEntityForm":U).
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
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartBusinessEntityTable":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityTableGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TableOrder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TableName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TableLabelSingular":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TableLabelPlural":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ShouldUseBatching":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AllowDelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AllowCreate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AllowUpdate":U).
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
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartBusinessEntityTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartBusinessEntity":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityPackage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityPurpose":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DBRequired":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TrackDeletions":U).
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
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("eSmartBusinessEntity":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityViewerControl = NEW Consultingwerk.Windows.Framework.System.SmartBusinessEntityViewerControl().
        THIS-OBJECT:smartBusinessEntityAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartDataBrowserTable = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:smartBusinessEntityBindingSourceChild = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartDatasetChildAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraSplitter2 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartBusinessEntityTableViewerControl = NEW Consultingwerk.Windows.Framework.System.SmartBusinessEntityTableViewerControl().
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:smartBusinessEntityBindingSourceMain = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityViewerControl, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowserTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSourceChild, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDatasetChildAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityTableViewerControl, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowserSmartBusinessEntity, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSourceMain, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityViewerControl).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl1, "ultraTabPageControl1":U).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        /*  */
        /* smartBusinessEntityViewerControl */
        /*  */
        THIS-OBJECT:smartBusinessEntityViewerControl:ActivateCurrentGridRowDuringEdit = TRUE.
        THIS-OBJECT:smartBusinessEntityViewerControl:AllowCloseOfUnmodifiedNewRecord = TRUE.
        THIS-OBJECT:smartBusinessEntityViewerControl:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityViewerControl, "smartBusinessEntityViewerControl":U).
        THIS-OBJECT:smartBusinessEntityViewerControl:FocusControlOnAdd = ?.
        THIS-OBJECT:smartBusinessEntityViewerControl:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartBusinessEntityViewerControl:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter.
        THIS-OBJECT:smartBusinessEntityViewerControl:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartBusinessEntityViewerControl:LinkTableIOSource = THIS-OBJECT:smartToolbarController.
        THIS-OBJECT:smartBusinessEntityViewerControl:Name = "smartBusinessEntityViewerControl":U.
        THIS-OBJECT:smartBusinessEntityViewerControl:ReadOnlyDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityViewerControl:SaveMode = FALSE.
        /*  */
        /* smartBusinessEntityAdapter */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter:AppServerPartition = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartBusinessEntityAdapter:DatasetControllerType = "Consultingwerk.Windows.Framework.System.SmartBusinessEntityDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter:EntityJoin = "NO":U.
        THIS-OBJECT:smartBusinessEntityAdapter:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter:EntityTable = "eSmartBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter:EntityView = "eSmartBusinessEntityTable":U.
        THIS-OBJECT:smartBusinessEntityAdapter:FetchLastOnServer = TRUE.
        THIS-OBJECT:smartBusinessEntityAdapter:ForeignFields = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController.
        THIS-OBJECT:smartBusinessEntityAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter:QuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter:SmartDataSourceActive = TRUE.
        THIS-OBJECT:smartBusinessEntityAdapter:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:smartBusinessEntityAdapter:AfterUpdateRecord:Subscribe(THIS-OBJECT:smartBusinessEntityAdapter1_AfterUpdateRecord).
        THIS-OBJECT:smartBusinessEntityAdapter:BeforeUpdateRecord:Subscribe(THIS-OBJECT:smartBusinessEntityAdapter1_BeforeUpdateRecord).
        /*  */
        /* smartToolbarController */
        /*  */
        THIS-OBJECT:smartToolbarController:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        arrayvar1[2] = buttonTool3.
        arrayvar1[3] = buttonTool4.
        arrayvar1[4] = buttonTool5.
        arrayvar1[5] = buttonTool6.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
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
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar6[1] = buttonTool20.
        arrayvar6[2] = buttonTool21.
        arrayvar6[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar6).
        THIS-OBJECT:smartToolbarController:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar7[1] = buttonTool23.
        arrayvar7[2] = buttonTool24.
        arrayvar7[3] = buttonTool25.
        arrayvar7[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool27.
        arrayvar8[2] = buttonTool28.
        arrayvar8[3] = buttonTool29.
        arrayvar8[4] = buttonTool30.
        arrayvar8[5] = buttonTool31.
        arrayvar8[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool33.
        arrayvar9[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController:Toolbars:AddRange(arrayvar10).
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        resources:ApplyResources(buttonTool35:SharedPropsInternal, "buttonTool35.SharedPropsInternal":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:ForceApplyResources = "SharedPropsInternal":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        resources:ApplyResources(buttonTool36:SharedPropsInternal, "buttonTool36.SharedPropsInternal":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:ForceApplyResources = "SharedPropsInternal":U.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        resources:ApplyResources(buttonTool37:SharedPropsInternal, "buttonTool37.SharedPropsInternal":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:ForceApplyResources = "SharedPropsInternal":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance28.
        resources:ApplyResources(buttonTool38:SharedPropsInternal, "buttonTool38.SharedPropsInternal":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:ForceApplyResources = "SharedPropsInternal":U.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance29.
        resources:ApplyResources(buttonTool39:SharedPropsInternal, "buttonTool39.SharedPropsInternal":U).
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:ForceApplyResources = "SharedPropsInternal":U.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        resources:ApplyResources(buttonTool40:SharedPropsInternal, "buttonTool40.SharedPropsInternal":U).
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:ForceApplyResources = "SharedPropsInternal":U.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        resources:ApplyResources(buttonTool41:SharedPropsInternal, "buttonTool41.SharedPropsInternal":U).
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:ForceApplyResources = "SharedPropsInternal":U.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        resources:ApplyResources(buttonTool42:SharedPropsInternal, "buttonTool42.SharedPropsInternal":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:ForceApplyResources = "SharedPropsInternal":U.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance33.
        resources:ApplyResources(buttonTool43:SharedPropsInternal, "buttonTool43.SharedPropsInternal":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:ForceApplyResources = "SharedPropsInternal":U.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        resources:ApplyResources(buttonTool44:SharedPropsInternal, "buttonTool44.SharedPropsInternal":U).
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:ForceApplyResources = "SharedPropsInternal":U.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance35.
        resources:ApplyResources(buttonTool45:SharedPropsInternal, "buttonTool45.SharedPropsInternal":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool45:ForceApplyResources = "SharedPropsInternal":U.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance36.
        appearance37:Image = CAST(resources:GetObject("appearance37.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance37.
        resources:ApplyResources(buttonTool46:SharedPropsInternal, "buttonTool46.SharedPropsInternal":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:ForceApplyResources = "SharedPropsInternal":U.
        appearance38:Image = CAST(resources:GetObject("appearance38.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance38.
        appearance39:Image = CAST(resources:GetObject("appearance39.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance39.
        resources:ApplyResources(buttonTool47:SharedPropsInternal, "buttonTool47.SharedPropsInternal":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar11[1] = buttonTool35.
        arrayvar11[2] = buttonTool36.
        arrayvar11[3] = buttonTool37.
        arrayvar11[4] = buttonTool38.
        arrayvar11[5] = buttonTool39.
        arrayvar11[6] = buttonTool40.
        arrayvar11[7] = buttonTool41.
        arrayvar11[8] = buttonTool42.
        arrayvar11[9] = buttonTool43.
        arrayvar11[10] = buttonTool44.
        arrayvar11[11] = buttonTool45.
        arrayvar11[12] = buttonTool46.
        arrayvar11[13] = buttonTool47.
        THIS-OBJECT:smartToolbarController:Tools:AddRange(arrayvar11).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowserTable).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraSplitter2).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityTableViewerControl).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl2, "ultraTabPageControl2":U).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        /*  */
        /* smartDataBrowserTable */
        /*  */
        THIS-OBJECT:smartDataBrowserTable:DataSource = THIS-OBJECT:smartBusinessEntityBindingSourceChild.
        THIS-OBJECT:smartDataBrowserTable:DeselectRowsOnActivate = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Appearance = appearance1.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn6:Header, "ultraGridColumn6.Header":U).
        ultraGridColumn6:Header:VisiblePosition = 0.
        ultraGridColumn6:Hidden = TRUE.
        ultraGridColumn6:ForceApplyResources = "Header":U.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn7:Header, "ultraGridColumn7.Header":U).
        ultraGridColumn7:Header:VisiblePosition = 1.
        ultraGridColumn7:Hidden = TRUE.
        ultraGridColumn7:ForceApplyResources = "Header":U.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn8:Header, "ultraGridColumn8.Header":U).
        ultraGridColumn8:Header:VisiblePosition = 2.
        ultraGridColumn8:ForceApplyResources = "Header":U.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn9:Header, "ultraGridColumn9.Header":U).
        ultraGridColumn9:Header:VisiblePosition = 3.
        ultraGridColumn9:ForceApplyResources = "Header":U.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn10:Header, "ultraGridColumn10.Header":U).
        ultraGridColumn10:Header:VisiblePosition = 4.
        ultraGridColumn10:Hidden = TRUE.
        ultraGridColumn10:ForceApplyResources = "Header":U.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn11:Header, "ultraGridColumn11.Header":U).
        ultraGridColumn11:Header:VisiblePosition = 5.
        ultraGridColumn11:Hidden = TRUE.
        ultraGridColumn11:ForceApplyResources = "Header":U.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn12:Header, "ultraGridColumn12.Header":U).
        ultraGridColumn12:Header:VisiblePosition = 6.
        ultraGridColumn12:Hidden = TRUE.
        ultraGridColumn12:ForceApplyResources = "Header":U.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn13:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn13:Header, "ultraGridColumn13.Header":U).
        ultraGridColumn13:Header:VisiblePosition = 7.
        ultraGridColumn13:Hidden = TRUE.
        ultraGridColumn13:ForceApplyResources = "Header":U.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn14:Header, "ultraGridColumn14.Header":U).
        ultraGridColumn14:Header:VisiblePosition = 8.
        ultraGridColumn14:ForceApplyResources = "Header":U.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn15:Header, "ultraGridColumn15.Header":U).
        ultraGridColumn15:Header:VisiblePosition = 9.
        ultraGridColumn15:ForceApplyResources = "Header":U.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn16:Header, "ultraGridColumn16.Header":U).
        ultraGridColumn16:Header:VisiblePosition = 10.
        ultraGridColumn16:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 11 NO-UNDO.
        arrayvar12[1] = ultraGridColumn6.
        arrayvar12[2] = ultraGridColumn7.
        arrayvar12[3] = ultraGridColumn8.
        arrayvar12[4] = ultraGridColumn9.
        arrayvar12[5] = ultraGridColumn10.
        arrayvar12[6] = ultraGridColumn11.
        arrayvar12[7] = ultraGridColumn12.
        arrayvar12[8] = ultraGridColumn13.
        arrayvar12[9] = ultraGridColumn14.
        arrayvar12[10] = ultraGridColumn15.
        arrayvar12[11] = ultraGridColumn16.
        ultraGridBand1:Columns:AddRange(arrayvar12).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        resources:ApplyResources(appearance10, "appearance10":U).
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowserTable:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowserTable, "smartDataBrowserTable":U).
        THIS-OBJECT:smartDataBrowserTable:LinkDataSource = THIS-OBJECT:smartDatasetChildAdapter.
        THIS-OBJECT:smartDataBrowserTable:Name = "smartDataBrowserTable":U.
        THIS-OBJECT:smartDataBrowserTable:SettingsKey = "f6ac1e71-9178-7dae-d513-c6cfd44f28d0":U.
        /*  */
        /* smartBusinessEntityBindingSourceChild */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:EntityTable = "eSmartBusinessEntityTable":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Progress.Data.ColumnPropDesc EXTENT 11 NO-UNDO.
        arrayvar13[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityTableGuid":U, "BusinessEntity Table Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[2] = NEW Progress.Data.ColumnPropDesc("BusinessEntityGuid":U, "BusinessEntity Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[3] = NEW Progress.Data.ColumnPropDesc("TableOrder":U, "Table Order":U, Progress.Data.DataType:INTEGER).
        arrayvar13[4] = NEW Progress.Data.ColumnPropDesc("TableName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[5] = NEW Progress.Data.ColumnPropDesc("TableLabelSingular":U, "Table Label Singular":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[6] = NEW Progress.Data.ColumnPropDesc("TableLabelPlural":U, "Table Label Plural":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[7] = NEW Progress.Data.ColumnPropDesc("ShouldUseBatching":U, "Should use Batching":U, Progress.Data.DataType:LOGICAL).
        arrayvar13[8] = NEW Progress.Data.ColumnPropDesc("BatchSize":U, "Batch Size":U, Progress.Data.DataType:INTEGER).
        arrayvar13[9] = NEW Progress.Data.ColumnPropDesc("AllowDelete":U, "Allow Delete":U, Progress.Data.DataType:LOGICAL).
        arrayvar13[10] = NEW Progress.Data.ColumnPropDesc("AllowCreate":U, "Allow Create":U, Progress.Data.DataType:LOGICAL).
        arrayvar13[11] = NEW Progress.Data.ColumnPropDesc("AllowUpdate":U, "Allow Update":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar13.
        THIS-OBJECT:smartBusinessEntityBindingSourceChild:TableSchema = tableDesc1.
        /*  */
        /* smartDatasetChildAdapter */
        /*  */
        THIS-OBJECT:smartDatasetChildAdapter:AppServerPartition = "":U.
        THIS-OBJECT:smartDatasetChildAdapter:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartDatasetChildAdapter:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartDatasetChildAdapter:EntityTable = "eSmartBusinessEntityTable":U.
        THIS-OBJECT:smartDatasetChildAdapter:ForeignFields = "BusinessEntityGuid,BusinessEntityGuid":U.
        THIS-OBJECT:smartDatasetChildAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter.
        THIS-OBJECT:smartDatasetChildAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:smartDatasetChildAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController.
        THIS-OBJECT:smartDatasetChildAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:smartDatasetChildAdapter:QuerySort = "":U.
        THIS-OBJECT:smartDatasetChildAdapter:SmartDataSourceActive = TRUE.
        THIS-OBJECT:smartDatasetChildAdapter:ThrowErrorsFromRetrieveData = FALSE.
        /*  */
        /* ultraSplitter2 */
        /*  */
        THIS-OBJECT:ultraSplitter2:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter2, "ultraSplitter2":U).
        THIS-OBJECT:ultraSplitter2:Name = "ultraSplitter2":U.
        THIS-OBJECT:ultraSplitter2:RestoreExtent = 263.
        /*  */
        /* smartBusinessEntityTableViewerControl */
        /*  */
        THIS-OBJECT:smartBusinessEntityTableViewerControl:ActivateCurrentGridRowDuringEdit = TRUE.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:AllowCloseOfUnmodifiedNewRecord = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityTableViewerControl, "smartBusinessEntityTableViewerControl":U).
        THIS-OBJECT:smartBusinessEntityTableViewerControl:FocusControlOnAdd = ?.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:LinkDataSource = THIS-OBJECT:smartDatasetChildAdapter.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:LinkTableIOSource = THIS-OBJECT:smartToolbarController.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:Name = "smartBusinessEntityTableViewerControl":U.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:ReadOnlyDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityTableViewerControl:SaveMode = FALSE.
        /*  */
        /* SmartBusinessEntityForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartBusinessEntityForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartDataBrowserSmartBusinessEntity).
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel, "SmartBusinessEntityForm_Fill_Panel":U).
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:Name = "SmartBusinessEntityForm_Fill_Panel":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        resources:ApplyResources(THIS-OBJECT:ultraTabControl1, "ultraTabControl1":U).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        ultraTab1:Key = "TabSmartBusinessEntity":U.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        resources:ApplyResources(ultraTab1, "ultraTab1":U).
        ultraTab1:ForceApplyResources = "":U.
        ultraTab2:Key = "TabSmartBusinessEntityTable":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        resources:ApplyResources(ultraTab2, "ultraTab2":U).
        ultraTab2:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar14[1] = ultraTab1.
        arrayvar14[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar14).
        THIS-OBJECT:ultraTabControl1:ActiveTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_ActiveTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage1, "ultraTabSharedControlsPage1":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:Cursor = System.Windows.Forms.Cursors:VSplit.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 307.
        /*  */
        /* smartDataBrowserSmartBusinessEntity */
        /*  */
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DataSource = THIS-OBJECT:smartBusinessEntityBindingSourceMain.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DeselectRowsOnActivate = TRUE.
        appearance13:BackColor = System.Drawing.SystemColors:Window.
        appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Appearance = appearance13.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn17:Header, "ultraGridColumn17.Header":U).
        ultraGridColumn17:Header:VisiblePosition = 0.
        ultraGridColumn17:Hidden = TRUE.
        ultraGridColumn17:ForceApplyResources = "Header":U.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn18:Header, "ultraGridColumn18.Header":U).
        ultraGridColumn18:Header:VisiblePosition = 1.
        ultraGridColumn18:ForceApplyResources = "Header":U.
        ultraGridColumn19:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn19:Header, "ultraGridColumn19.Header":U).
        ultraGridColumn19:Header:VisiblePosition = 2.
        ultraGridColumn19:ForceApplyResources = "Header":U.
        ultraGridColumn20:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn20:Header, "ultraGridColumn20.Header":U).
        ultraGridColumn20:Header:VisiblePosition = 3.
        ultraGridColumn20:Hidden = TRUE.
        ultraGridColumn20:ForceApplyResources = "Header":U.
        ultraGridColumn21:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn21:Header, "ultraGridColumn21.Header":U).
        ultraGridColumn21:Header:VisiblePosition = 4.
        ultraGridColumn21:ForceApplyResources = "Header":U.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn1:Header, "ultraGridColumn1.Header":U).
        ultraGridColumn1:Header:VisiblePosition = 5.
        ultraGridColumn1:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:TriStateCheckBox.
        ultraGridColumn1:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS System.Object EXTENT 6 NO-UNDO.
        arrayvar15[1] = ultraGridColumn17.
        arrayvar15[2] = ultraGridColumn18.
        arrayvar15[3] = ultraGridColumn19.
        arrayvar15[4] = ultraGridColumn20.
        arrayvar15[5] = ultraGridColumn21.
        arrayvar15[6] = ultraGridColumn1.
        ultraGridBand2:Columns:AddRange(arrayvar15).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance14:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:GroupByBox:Appearance = appearance14.
        appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance16:BackColor2 = System.Drawing.SystemColors:Control.
        appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:MaxRowScrollRegions = 1.
        appearance17:BackColor = System.Drawing.SystemColors:Window.
        appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:ActiveCellAppearance = appearance17.
        appearance18:BackColor = System.Drawing.SystemColors:Highlight.
        appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:ActiveRowAppearance = appearance18.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance19:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:CardAreaAppearance = appearance19.
        appearance20:BorderColor = System.Drawing.Color:Silver.
        appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:CellAppearance = appearance20.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:CellPadding = 0.
        appearance21:BackColor = System.Drawing.SystemColors:Control.
        appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance21:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:GroupByRowAppearance = appearance21.
        resources:ApplyResources(appearance22, "appearance22":U).
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:HeaderAppearance = appearance22.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance23:BackColor = System.Drawing.SystemColors:Window.
        appearance23:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:RowAppearance = appearance23.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowserSmartBusinessEntity, "smartDataBrowserSmartBusinessEntity":U).
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:Name = "smartDataBrowserSmartBusinessEntity":U.
        THIS-OBJECT:smartDataBrowserSmartBusinessEntity:SettingsKey = "c934ffc2-f007-d8bd-0214-4426a025837d":U.
        /*  */
        /* smartBusinessEntityBindingSourceMain */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:EntityTable = "eSmartBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:NoLOBs = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar16 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar16.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar17[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityGuid":U, "BusinessEntity Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[2] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPackage":U, "Package":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[3] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[4] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPurpose":U, "Purpose":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[5] = NEW Progress.Data.ColumnPropDesc("DBRequired":U, "DB Required":U, Progress.Data.DataType:LOGICAL).
        arrayvar17[6] = NEW Progress.Data.ColumnPropDesc("TrackDeletions":U, "Track Deletions":U, Progress.Data.DataType:LOGICAL).
        tableDesc2:Columns = arrayvar17.
        THIS-OBJECT:smartBusinessEntityBindingSourceMain:TableSchema = tableDesc2.
        /*  */
        /* m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left, "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:Name = "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController.
        /*  */
        /* m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right, "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:Name = "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController.
        /*  */
        /* m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top, "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:Name = "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController.
        /*  */
        /* m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom, "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController.
        /*  */
        /* SmartBusinessEntityForm */
        /*  */
        THIS-OBJECT:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartBusinessEntityForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartBusinessEntityForm":U.
        THIS-OBJECT:PrimaryDataSource = THIS-OBJECT:smartBusinessEntityAdapter.
        THIS-OBJECT:PrimaryDataTarget = THIS-OBJECT:smartBusinessEntityAdapter.
        THIS-OBJECT:PrimaryNavigationSource = THIS-OBJECT:smartToolbarController.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartBusinessEntityViewerControl, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowserTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSourceChild, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDatasetChildAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityTableViewerControl, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartBusinessEntityForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowserSmartBusinessEntity, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSourceMain, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown Event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() .

        THIS-OBJECT:smartBusinessEntityAdapter:RetrieveData () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:  Create all BusinessEntity Tables
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityAdapter1_AfterUpdateRecord (sender AS System.Object,
                                                                       e      AS Consultingwerk.SmartComponents.Implementation.AfterUpdateRecordEventArgs):

        DEFINE VARIABLE oMessageProvider   AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO.
        DEFINE VARIABLE oMessage           AS Consultingwerk.SmartFramework.Message          NO-UNDO.
        DEFINE VARIABLE oDatasetController AS SmartBusinessEntityDatasetController           NO-UNDO.

        IF NOT lCreatingRecord THEN
            RETURN.

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        IF VALID-OBJECT (oMessageProvider) THEN
            DO ON ERROR UNDO, THROW:
                oMessage = oMessageProvider:GetMessage ("SFR":U, 101) .

                IF MessageFormHelper:ShowMessage (oMessage) = DialogResultEnum:DialogResultYes THEN DO:

                    IF VALID-OBJECT (THIS-OBJECT:smartBusinessEntityAdapter:DatasetController) AND
                       TYPE-OF (THIS-OBJECT:smartBusinessEntityAdapter:DatasetController, SmartBusinessEntityDatasetController) THEN
                        oDatasetController = CAST (THIS-OBJECT:smartBusinessEntityAdapter:DatasetController, SmartBusinessEntityDatasetController).

                    IF NOT VALID-OBJECT (oDatasetController) THEN
                        UNDO, THROW NEW AppError (oMessageProvider:GetMessage ("SFR":U, 1):Text, 0).

                    oDatasetController:CreateSmartBusinessEntityTable (SUBSTITUTE ("&1.&2":U,
                                                                                   smartBusinessEntityAdapter:GetFieldValues ("BusinessEntityPackage":U),
                                                                                   smartBusinessEntityAdapter:GetFieldValues ("BusinessEntityName":U))).

                END.

                CATCH err AS Progress.Lang.Error :
                    ErrorHelper:ShowErrorMessage (err) .
                END CATCH.
            END.

        FINALLY:
            ASSIGN lCreatingRecord = FALSE.
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Save creating record state
        Notes:
        @param sender The reference to the object that raised the event
        @param e The BeforeUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityAdapter1_BeforeUpdateRecord (sender AS System.Object,
                                                                        e      AS Consultingwerk.SmartComponents.Implementation.BeforeUpdateRecordEventArgs):

        ASSIGN lCreatingRecord = smartBusinessEntityAdapter:CreatingRecord .

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Bind the toolbar navigation target
     Notes:
     @param sender The reference to the object that raised the event
     @param e The ActiveTabChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTabControl1_ActiveTabChanged (sender AS System.Object,
                                                           e      AS Infragistics.Win.UltraWinTabControl.ActiveTabChangedEventArgs):

        CASE e:TAB:Key:
            WHEN "TabSmartBusinessEntity":U THEN DO:
                smartToolbarController:ActivateSmartNavigationTarget (smartBusinessEntityAdapter) .
                smartToolbarController:ActivateSmartTableIOTarget (smartBusinessEntityViewerControl) .
            END.
            WHEN "TabSmartBusinessEntityTable":U THEN DO:
                smartToolbarController:ActivateSmartNavigationTarget (smartDatasetChildAdapter) .
                smartToolbarController:ActivateSmartTableIOTarget (smartBusinessEntityTableViewerControl) .
            END.
        END CASE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartBusinessEntityForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityForm ():

    END DESTRUCTOR .

END CLASS.
