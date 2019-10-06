/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartObjectMasterForm
    Purpose     : Maintenance of SmartObject Master
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 25.05.2016 14:59:20
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                     FROM PROPATH .
USING Consultingwerk.Framework.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.*    FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Infragistics.Win.UltraWinTabControl.*                FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterForm INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE cbPageGuid AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE instancesAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE instancesBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE linksAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE linksBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE objectInstanceAttributesUserControl1 AS Consultingwerk.Windows.Framework.Repository.Object.ObjectInstanceAttributesUserControl NO-UNDO.
    DEFINE PRIVATE VARIABLE objectMasterAttributesUserControl1 AS Consultingwerk.Windows.Framework.Repository.Object.ObjectMasterAttributesUserControl NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE panel2 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE pagesBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE pagesAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser2 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser3 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartLinkViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Object.SmartLinkViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartObjectInstanceViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Object.SmartObjectInstanceViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartObjectMasterViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectMasterForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectMasterForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectMasterForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPageViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Object.SmartPageViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPropertyGridControl1 AS Consultingwerk.Windows.Framework.Repository.Object.SmartPropertyGridControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartObjectMasterForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter2 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter3 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter4 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter5 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i &ACCESS=PROTECTED}

    &SCOPED-DEFINE DATASET-NAME dsAttributeValue
    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i &ACCESS=PROTECTED &SUFFIX=_inst}
    &UNDEFINE DATASET-NAME

    /**
     * Purpose: Returns if the currently opened SmartObjectMaster is a
     *          container or not
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EditingContainer AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the designer class name for the current container (if any)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DesignerType AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the SmartObjectMasterForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartObjectMasterForm ():

        SUPER().
        InitializeComponent().

        instancesAdapter:CollectFilterValues:Subscribe(THIS-OBJECT:instancesAdapter_CollectFilterValues).

        objectMasterAttributesUserControl1:BindDataset(DATASET dsDesignAttributeValue BIND) .
        objectInstanceAttributesUserControl1:BindDataset(DATASET dsDesignAttributeValue_inst BIND) .

        smartObjectInstanceViewerControl1:FilterComboEditor = cbPageGuid .

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Enables/Disables the tabs
     * Notes:
     * @param plEnable Logical value indicating if the tab pages shoudl be enabled
     */
    METHOD PROTECTED VOID EnableTabs (plEnable AS LOGICAL):

        {Consultingwerk/foreach.i UltraTab oTab in ultraTabControl1:Tabs}

            IF oTab:Index > 0 THEN DO:

                IF oTab:Key <> "design":U THEN
                    oTab:Enabled = plEnable AND THIS-OBJECT:EditingContainer .
                ELSE
                    oTab:Enabled = THIS-OBJECT:EditingContainer AND
                                   CharacterType:IsNotNullOrEmpty(THIS-OBJECT:DesignerType) .
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ObjectMaster":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool93 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool93 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenObjectMaster":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool59 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool59 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool60 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool60 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool61 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool61 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool62 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool62 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool65 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool65 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool66 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool66 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool67 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool67 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool68 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool68 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool69 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool69 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool70 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool70 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool71 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool71 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool72 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool72 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool73 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool73 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool74 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool74 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool75 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool75 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool76 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool76 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool77 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool77 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool78 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool78 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool79 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool79 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool80 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool80 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
        appearance42 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool81 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool81 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
        appearance43 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool82 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool82 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
        appearance44 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool83 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool83 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
        appearance45 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool84 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool84 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
        appearance46 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool85 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool85 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
        appearance47 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool86 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool86 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
        appearance48 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool87 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool87 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
        appearance49 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool88 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool88 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
        appearance50 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool89 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool89 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
        appearance51 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool90 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool90 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
        appearance52 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool91 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool91 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
        appearance53 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
        appearance54 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool92 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool92 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
        appearance55 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
        appearance56 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool94 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool94 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenObjectMaster":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
        appearance57 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
        appearance58 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartPage":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PageGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ContainerObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PageSequence":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PageLabel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SecurityToken":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EnableOnCreate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EnableOnModify":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EnableOnView":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartPage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand3 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand3 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartObjectInstance":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectInstanceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ContainerObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectMasterName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ParentInstanceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LayoutPosition":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("InstanceName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("InstanceDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PageGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectSequence":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("eSmartObjectInstance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartLink":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn35 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn35 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ContainerObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn36 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn36 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkTypeGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn37 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn37 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn38 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn38 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SourceObjectInstanceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn39 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn39 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SourceObjectName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn40 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn40 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SourcePageNumber":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn41 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn41 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TargetObjectInstanceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn42 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn42 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TargetObjectName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn43 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn43 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TargetPageNumber":U).
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
        tableDesc2 = NEW Progress.Data.TableDesc("eSmartLink":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc4 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc4 = NEW Progress.Data.TableDesc("eSmartObjectMaster":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:objectMasterAttributesUserControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.ObjectMasterAttributesUserControl().
        THIS-OBJECT:smartObjectMasterViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.SmartObjectMasterViewerControl().
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartPageViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.SmartPageViewerControl().
        THIS-OBJECT:pagesAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraSplitter2 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:pagesBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:objectInstanceAttributesUserControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.ObjectInstanceAttributesUserControl().
        THIS-OBJECT:ultraSplitter3 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:smartDataBrowser2 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:instancesBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:instancesAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:panel2 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbPageGuid = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraSplitter4 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartObjectInstanceViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.SmartObjectInstanceViewerControl().
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartPropertyGridControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.SmartPropertyGridControl().
        THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartLinkViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Object.SmartLinkViewerControl().
        THIS-OBJECT:linksAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraSplitter5 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartDataBrowser3 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:linksBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartObjectMasterViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartPageViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pagesAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pagesBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl5:SuspendLayout().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:instancesBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:instancesAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel2:SuspendLayout().
        CAST(THIS-OBJECT:cbPageGuid, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartObjectInstanceViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        THIS-OBJECT:ultraTabPageControl4:SuspendLayout().
        CAST(THIS-OBJECT:smartLinkViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:linksAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:linksBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:objectMasterAttributesUserControl1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartObjectMasterViewerControl1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl1, "ultraTabPageControl1":U).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        /*  */
        /* objectMasterAttributesUserControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:objectMasterAttributesUserControl1, "objectMasterAttributesUserControl1":U).
        THIS-OBJECT:objectMasterAttributesUserControl1:Name = "objectMasterAttributesUserControl1":U.
        THIS-OBJECT:objectMasterAttributesUserControl1:HasChangesChanged:Subscribe(THIS-OBJECT:objectMasterAttributesUserControl1_HasChangesChanged).
        /*  */
        /* smartObjectMasterViewerControl1 */
        /*  */
        THIS-OBJECT:smartObjectMasterViewerControl1:ActivateCurrentGridRowDuringEdit = TRUE.
        THIS-OBJECT:smartObjectMasterViewerControl1:AllowCloseOfUnmodifiedNewRecord = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartObjectMasterViewerControl1, "smartObjectMasterViewerControl1":U).
        THIS-OBJECT:smartObjectMasterViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartObjectMasterViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartObjectMasterViewerControl1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:smartObjectMasterViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartObjectMasterViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartObjectMasterViewerControl1:Name = "smartObjectMasterViewerControl1":U.
        THIS-OBJECT:smartObjectMasterViewerControl1:ReadOnlyDisabled = FALSE.
        THIS-OBJECT:smartObjectMasterViewerControl1:SaveMode = FALSE.
        THIS-OBJECT:smartObjectMasterViewerControl1:AfterAddRecord:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_AfterAddRecord).
        THIS-OBJECT:smartObjectMasterViewerControl1:AfterCancelUpdate:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_AfterCancelUpdate).
        THIS-OBJECT:smartObjectMasterViewerControl1:AfterDeleteRecord:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_AfterDeleteRecord).
        THIS-OBJECT:smartObjectMasterViewerControl1:AfterSaveChanges:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_AfterSaveChanges).
        THIS-OBJECT:smartObjectMasterViewerControl1:FieldsDisabled:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_FieldsDisabled).
        THIS-OBJECT:smartObjectMasterViewerControl1:FieldsEnabled:Subscribe(THIS-OBJECT:smartObjectMasterViewerControl1_FieldsEnabled).
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:AppServerPartition = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:BatchSize = 1.
        THIS-OBJECT:smartBusinessEntityAdapter1:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:DatasetControllerType = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityJoin = "NO,NO":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityView = "eSmartObjectInstance,eSmartPage":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:FetchLastOnServer = TRUE.
        THIS-OBJECT:smartBusinessEntityAdapter1:ForeignFields = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartBusinessEntityAdapter1:NonUpdatableColumns = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:PrefetchOnFindRowWhere = 0.
        THIS-OBJECT:smartBusinessEntityAdapter1:QuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:SmartDataSourceActive = TRUE.
        THIS-OBJECT:smartBusinessEntityAdapter1:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:smartBusinessEntityAdapter1:CurrentChanged:Subscribe(THIS-OBJECT:smartBusinessEntityAdapter1_CurrentChanged).
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool18.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool50:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool19.
        arrayvar1[2] = buttonTool48.
        arrayvar1[3] = buttonTool49.
        arrayvar1[4] = buttonTool50.
        arrayvar1[5] = buttonTool51.
        arrayvar1[6] = buttonTool52.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool93:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool93.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        buttonTool57:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool57:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool58:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool58:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool59:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool59:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool60:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool61:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool62:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool57.
        arrayvar3[2] = buttonTool58.
        arrayvar3[3] = buttonTool59.
        arrayvar3[4] = buttonTool60.
        arrayvar3[5] = buttonTool61.
        arrayvar3[6] = buttonTool62.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar6[1] = buttonTool65.
        arrayvar6[2] = buttonTool66.
        arrayvar6[3] = buttonTool67.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar6).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool68:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar7[1] = buttonTool68.
        arrayvar7[2] = buttonTool69.
        arrayvar7[3] = buttonTool70.
        arrayvar7[4] = buttonTool71.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool72:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool72.
        arrayvar8[2] = buttonTool73.
        arrayvar8[3] = buttonTool74.
        arrayvar8[4] = buttonTool75.
        arrayvar8[5] = buttonTool76.
        arrayvar8[6] = buttonTool77.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool78.
        arrayvar9[2] = buttonTool79.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance42:Image = CAST(resources:GetObject("appearance42.Image":U), System.Object).
        buttonTool80:SharedPropsInternal:AppearancesSmall:Appearance = appearance42.
        resources:ApplyResources(buttonTool80:SharedPropsInternal, "buttonTool80.SharedPropsInternal":U).
        buttonTool80:SharedPropsInternal:Enabled = FALSE.
        buttonTool80:ForceApplyResources = "SharedPropsInternal":U.
        appearance43:Image = CAST(resources:GetObject("appearance43.Image":U), System.Object).
        buttonTool81:SharedPropsInternal:AppearancesSmall:Appearance = appearance43.
        resources:ApplyResources(buttonTool81:SharedPropsInternal, "buttonTool81.SharedPropsInternal":U).
        buttonTool81:SharedPropsInternal:Enabled = FALSE.
        buttonTool81:ForceApplyResources = "SharedPropsInternal":U.
        appearance44:Image = CAST(resources:GetObject("appearance44.Image":U), System.Object).
        buttonTool82:SharedPropsInternal:AppearancesSmall:Appearance = appearance44.
        resources:ApplyResources(buttonTool82:SharedPropsInternal, "buttonTool82.SharedPropsInternal":U).
        buttonTool82:SharedPropsInternal:Enabled = FALSE.
        buttonTool82:ForceApplyResources = "SharedPropsInternal":U.
        appearance45:Image = CAST(resources:GetObject("appearance45.Image":U), System.Object).
        buttonTool83:SharedPropsInternal:AppearancesSmall:Appearance = appearance45.
        resources:ApplyResources(buttonTool83:SharedPropsInternal, "buttonTool83.SharedPropsInternal":U).
        buttonTool83:SharedPropsInternal:Enabled = FALSE.
        buttonTool83:ForceApplyResources = "SharedPropsInternal":U.
        appearance46:Image = CAST(resources:GetObject("appearance46.Image":U), System.Object).
        buttonTool84:SharedPropsInternal:AppearancesSmall:Appearance = appearance46.
        resources:ApplyResources(buttonTool84:SharedPropsInternal, "buttonTool84.SharedPropsInternal":U).
        buttonTool84:SharedPropsInternal:Enabled = FALSE.
        buttonTool84:ForceApplyResources = "SharedPropsInternal":U.
        appearance47:Image = CAST(resources:GetObject("appearance47.Image":U), System.Object).
        buttonTool85:SharedPropsInternal:AppearancesSmall:Appearance = appearance47.
        resources:ApplyResources(buttonTool85:SharedPropsInternal, "buttonTool85.SharedPropsInternal":U).
        buttonTool85:SharedPropsInternal:Enabled = FALSE.
        buttonTool85:ForceApplyResources = "SharedPropsInternal":U.
        appearance48:Image = CAST(resources:GetObject("appearance48.Image":U), System.Object).
        buttonTool86:SharedPropsInternal:AppearancesSmall:Appearance = appearance48.
        resources:ApplyResources(buttonTool86:SharedPropsInternal, "buttonTool86.SharedPropsInternal":U).
        buttonTool86:SharedPropsInternal:Enabled = FALSE.
        buttonTool86:ForceApplyResources = "SharedPropsInternal":U.
        appearance49:Image = CAST(resources:GetObject("appearance49.Image":U), System.Object).
        buttonTool87:SharedPropsInternal:AppearancesSmall:Appearance = appearance49.
        resources:ApplyResources(buttonTool87:SharedPropsInternal, "buttonTool87.SharedPropsInternal":U).
        buttonTool87:SharedPropsInternal:Enabled = FALSE.
        buttonTool87:ForceApplyResources = "SharedPropsInternal":U.
        appearance50:Image = CAST(resources:GetObject("appearance50.Image":U), System.Object).
        buttonTool88:SharedPropsInternal:AppearancesSmall:Appearance = appearance50.
        resources:ApplyResources(buttonTool88:SharedPropsInternal, "buttonTool88.SharedPropsInternal":U).
        buttonTool88:SharedPropsInternal:Enabled = FALSE.
        buttonTool88:ForceApplyResources = "SharedPropsInternal":U.
        appearance51:Image = CAST(resources:GetObject("appearance51.Image":U), System.Object).
        buttonTool89:SharedPropsInternal:AppearancesSmall:Appearance = appearance51.
        resources:ApplyResources(buttonTool89:SharedPropsInternal, "buttonTool89.SharedPropsInternal":U).
        buttonTool89:SharedPropsInternal:Enabled = FALSE.
        buttonTool89:ForceApplyResources = "SharedPropsInternal":U.
        appearance52:Image = CAST(resources:GetObject("appearance52.Image":U), System.Object).
        buttonTool90:SharedPropsInternal:AppearancesSmall:Appearance = appearance52.
        resources:ApplyResources(buttonTool90:SharedPropsInternal, "buttonTool90.SharedPropsInternal":U).
        buttonTool90:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool90:ForceApplyResources = "SharedPropsInternal":U.
        appearance53:Image = CAST(resources:GetObject("appearance53.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesLarge:Appearance = appearance53.
        appearance54:Image = CAST(resources:GetObject("appearance54.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesSmall:Appearance = appearance54.
        resources:ApplyResources(buttonTool91:SharedPropsInternal, "buttonTool91.SharedPropsInternal":U).
        buttonTool91:SharedPropsInternal:Enabled = FALSE.
        buttonTool91:ForceApplyResources = "SharedPropsInternal":U.
        appearance55:Image = CAST(resources:GetObject("appearance55.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesLarge:Appearance = appearance55.
        appearance56:Image = CAST(resources:GetObject("appearance56.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesSmall:Appearance = appearance56.
        resources:ApplyResources(buttonTool92:SharedPropsInternal, "buttonTool92.SharedPropsInternal":U).
        buttonTool92:SharedPropsInternal:Enabled = FALSE.
        buttonTool92:ForceApplyResources = "SharedPropsInternal":U.
        appearance57:Image = CAST(resources:GetObject("appearance57.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesLarge:Appearance = appearance57.
        appearance58:Image = CAST(resources:GetObject("appearance58.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesSmall:Appearance = appearance58.
        resources:ApplyResources(buttonTool94:SharedPropsInternal, "buttonTool94.SharedPropsInternal":U).
        buttonTool94:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 14 NO-UNDO.
        arrayvar11[1] = buttonTool80.
        arrayvar11[2] = buttonTool81.
        arrayvar11[3] = buttonTool82.
        arrayvar11[4] = buttonTool83.
        arrayvar11[5] = buttonTool84.
        arrayvar11[6] = buttonTool85.
        arrayvar11[7] = buttonTool86.
        arrayvar11[8] = buttonTool87.
        arrayvar11[9] = buttonTool88.
        arrayvar11[10] = buttonTool89.
        arrayvar11[11] = buttonTool90.
        arrayvar11[12] = buttonTool91.
        arrayvar11[13] = buttonTool92.
        arrayvar11[14] = buttonTool94.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartPageViewerControl1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraSplitter2).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl2, "ultraTabPageControl2":U).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        /*  */
        /* smartPageViewerControl1 */
        /*  */
        THIS-OBJECT:smartPageViewerControl1:ActivateCurrentGridRowDuringEdit = TRUE.
        THIS-OBJECT:smartPageViewerControl1:AllowCloseOfUnmodifiedNewRecord = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartPageViewerControl1, "smartPageViewerControl1":U).
        THIS-OBJECT:smartPageViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartPageViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartPageViewerControl1:LinkDataSource = THIS-OBJECT:pagesAdapter.
        THIS-OBJECT:smartPageViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartPageViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartPageViewerControl1:Name = "smartPageViewerControl1":U.
        THIS-OBJECT:smartPageViewerControl1:ReadOnlyDisabled = FALSE.
        THIS-OBJECT:smartPageViewerControl1:SaveMode = FALSE.
        /*  */
        /* pagesAdapter */
        /*  */
        THIS-OBJECT:pagesAdapter:AppServerPartition = "":U.
        THIS-OBJECT:pagesAdapter:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:pagesAdapter:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:pagesAdapter:EntityTable = "eSmartPage":U.
        THIS-OBJECT:pagesAdapter:ForeignFields = "ObjectMasterGuid,ContainerObjectMasterGuid":U.
        THIS-OBJECT:pagesAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:pagesAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:pagesAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:pagesAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:pagesAdapter:QuerySort = "":U.
        THIS-OBJECT:pagesAdapter:SmartDataSourceActive = TRUE.
        THIS-OBJECT:pagesAdapter:ThrowErrorsFromRetrieveData = FALSE.
        /*  */
        /* ultraSplitter2 */
        /*  */
        THIS-OBJECT:ultraSplitter2:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter2, "ultraSplitter2":U).
        THIS-OBJECT:ultraSplitter2:Name = "ultraSplitter2":U.
        THIS-OBJECT:ultraSplitter2:RestoreExtent = 0.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:pagesBindingSource.
        THIS-OBJECT:smartDataBrowser1:DeselectRowsOnActivate = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn1:Header, "ultraGridColumn1.Header":U).
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn1:ForceApplyResources = "Header":U.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn2:Header, "ultraGridColumn2.Header":U).
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn2:ForceApplyResources = "Header":U.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn3:Header, "ultraGridColumn3.Header":U).
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:ForceApplyResources = "Header":U.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn4:Header, "ultraGridColumn4.Header":U).
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:ForceApplyResources = "Header":U.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn5:Header, "ultraGridColumn5.Header":U).
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:ForceApplyResources = "Header":U.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn6:Header, "ultraGridColumn6.Header":U).
        ultraGridColumn6:Header:VisiblePosition = 5.
        ultraGridColumn6:ForceApplyResources = "Header":U.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn7:Header, "ultraGridColumn7.Header":U).
        ultraGridColumn7:Header:VisiblePosition = 6.
        ultraGridColumn7:ForceApplyResources = "Header":U.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn8:Header, "ultraGridColumn8.Header":U).
        ultraGridColumn8:Header:VisiblePosition = 7.
        ultraGridColumn8:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 8 NO-UNDO.
        arrayvar12[1] = ultraGridColumn1.
        arrayvar12[2] = ultraGridColumn2.
        arrayvar12[3] = ultraGridColumn3.
        arrayvar12[4] = ultraGridColumn4.
        arrayvar12[5] = ultraGridColumn5.
        arrayvar12[6] = ultraGridColumn6.
        arrayvar12[7] = ultraGridColumn7.
        arrayvar12[8] = ultraGridColumn8.
        ultraGridBand1:Columns:AddRange(arrayvar12).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        resources:ApplyResources(appearance10, "appearance10":U).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser1, "smartDataBrowser1":U).
        THIS-OBJECT:smartDataBrowser1:EditInViewerOnDefaultAction = FALSE.
        THIS-OBJECT:smartDataBrowser1:FetchAllRecordsOnGroupBy = FALSE.
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:pagesAdapter.
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:RepositionedRow = 0.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "93a4d3e9-af5d-6da5-1b14-67cce46bbbc3":U.
        THIS-OBJECT:smartDataBrowser1:SmartFilterActive = FALSE.
        THIS-OBJECT:smartDataBrowser1:SmartFilterUseMatches = FALSE.
        THIS-OBJECT:smartDataBrowser1:SmartFilterUseOperators = FALSE.
        /*  */
        /* pagesBindingSource */
        /*  */
        THIS-OBJECT:pagesBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:pagesBindingSource:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:pagesBindingSource:EntityTable = "eSmartPage":U.
        THIS-OBJECT:pagesBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:pagesBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
        arrayvar13[1] = NEW Progress.Data.ColumnPropDesc("PageGuid":U, "Page":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[3] = NEW Progress.Data.ColumnPropDesc("PageSequence":U, "Page Number":U, Progress.Data.DataType:INTEGER).
        arrayvar13[4] = NEW Progress.Data.ColumnPropDesc("PageLabel":U, "Page Label":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[5] = NEW Progress.Data.ColumnPropDesc("SecurityToken":U, "Security Token":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[6] = NEW Progress.Data.ColumnPropDesc("EnableOnCreate":U, "Enable on create":U, Progress.Data.DataType:LOGICAL).
        arrayvar13[7] = NEW Progress.Data.ColumnPropDesc("EnableOnModify":U, "Enable on modify":U, Progress.Data.DataType:LOGICAL).
        arrayvar13[8] = NEW Progress.Data.ColumnPropDesc("EnableOnView":U, "Enable on view":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar13.
        THIS-OBJECT:pagesBindingSource:TableSchema = tableDesc1.
        /*  */
        /* ultraTabPageControl5 */
        /*  */
        THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:objectInstanceAttributesUserControl1).
        THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:ultraSplitter3).
        THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:panel1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl5, "ultraTabPageControl5":U).
        THIS-OBJECT:ultraTabPageControl5:Name = "ultraTabPageControl5":U.
        /*  */
        /* objectInstanceAttributesUserControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:objectInstanceAttributesUserControl1, "objectInstanceAttributesUserControl1":U).
        THIS-OBJECT:objectInstanceAttributesUserControl1:Name = "objectInstanceAttributesUserControl1":U.
        THIS-OBJECT:objectInstanceAttributesUserControl1:HasChangesChanged:Subscribe(THIS-OBJECT:objectInstanceAttributesUserControl1_HasChangesChanged).
        /*  */
        /* ultraSplitter3 */
        /*  */
        THIS-OBJECT:ultraSplitter3:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(240)), System.Convert:ToInt32(System.Convert:ToByte(246)), System.Convert:ToInt32(System.Convert:ToByte(254))).
        resources:ApplyResources(THIS-OBJECT:ultraSplitter3, "ultraSplitter3":U).
        THIS-OBJECT:ultraSplitter3:Name = "ultraSplitter3":U.
        THIS-OBJECT:ultraSplitter3:RestoreExtent = 202.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:smartDataBrowser2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:panel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraSplitter4).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:smartObjectInstanceViewerControl1).
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:Name = "panel1":U.
        /*  */
        /* smartDataBrowser2 */
        /*  */
        THIS-OBJECT:smartDataBrowser2:DataSource = THIS-OBJECT:instancesBindingSource.
        THIS-OBJECT:smartDataBrowser2:DeselectRowsOnActivate = TRUE.
        appearance25:BackColor = System.Drawing.SystemColors:Window.
        appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Appearance = appearance25.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn9:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn9:Header, "ultraGridColumn9.Header":U).
        ultraGridColumn9:Header:VisiblePosition = 0.
        ultraGridColumn9:Hidden = TRUE.
        ultraGridColumn9:ForceApplyResources = "Header":U.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn10:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn10:Header, "ultraGridColumn10.Header":U).
        ultraGridColumn10:Header:VisiblePosition = 1.
        ultraGridColumn10:Hidden = TRUE.
        ultraGridColumn10:ForceApplyResources = "Header":U.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn11:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn11:Header, "ultraGridColumn11.Header":U).
        ultraGridColumn11:Header:VisiblePosition = 2.
        ultraGridColumn11:Hidden = TRUE.
        ultraGridColumn11:ForceApplyResources = "Header":U.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn18:Header, "ultraGridColumn18.Header":U).
        ultraGridColumn18:Header:VisiblePosition = 3.
        ultraGridColumn18:ForceApplyResources = "Header":U.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn12:Header, "ultraGridColumn12.Header":U).
        ultraGridColumn12:Header:VisiblePosition = 4.
        ultraGridColumn12:Hidden = TRUE.
        ultraGridColumn12:ForceApplyResources = "Header":U.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn13:Header, "ultraGridColumn13.Header":U).
        ultraGridColumn13:Header:VisiblePosition = 5.
        ultraGridColumn13:ForceApplyResources = "Header":U.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn14:Header, "ultraGridColumn14.Header":U).
        ultraGridColumn14:Header:VisiblePosition = 6.
        ultraGridColumn14:ForceApplyResources = "Header":U.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn15:Header, "ultraGridColumn15.Header":U).
        ultraGridColumn15:Header:VisiblePosition = 7.
        ultraGridColumn15:ForceApplyResources = "Header":U.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn16:Header, "ultraGridColumn16.Header":U).
        ultraGridColumn16:Header:VisiblePosition = 8.
        ultraGridColumn16:ForceApplyResources = "Header":U.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn17:Header, "ultraGridColumn17.Header":U).
        ultraGridColumn17:Header:VisiblePosition = 9.
        ultraGridColumn17:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS System.Object EXTENT 10 NO-UNDO.
        arrayvar14[1] = ultraGridColumn9.
        arrayvar14[2] = ultraGridColumn10.
        arrayvar14[3] = ultraGridColumn11.
        arrayvar14[4] = ultraGridColumn18.
        arrayvar14[5] = ultraGridColumn12.
        arrayvar14[6] = ultraGridColumn13.
        arrayvar14[7] = ultraGridColumn14.
        arrayvar14[8] = ultraGridColumn15.
        arrayvar14[9] = ultraGridColumn16.
        arrayvar14[10] = ultraGridColumn17.
        ultraGridBand3:Columns:AddRange(arrayvar14).
        ultraGridBand3:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand3:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BandsSerializer:Add(ultraGridBand3).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance26:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:Appearance = appearance26.
        appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance28:BackColor2 = System.Drawing.SystemColors:Control.
        appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxRowScrollRegions = 1.
        appearance29:BackColor = System.Drawing.SystemColors:Window.
        appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveCellAppearance = appearance29.
        appearance30:BackColor = System.Drawing.SystemColors:Highlight.
        appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveRowAppearance = appearance30.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance31:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CardAreaAppearance = appearance31.
        appearance32:BorderColor = System.Drawing.Color:Silver.
        appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellAppearance = appearance32.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellPadding = 0.
        appearance33:BackColor = System.Drawing.SystemColors:Control.
        appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance33:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:GroupByRowAppearance = appearance33.
        resources:ApplyResources(appearance34, "appearance34":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderAppearance = appearance34.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance35:BackColor = System.Drawing.SystemColors:Window.
        appearance35:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowAppearance = appearance35.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser2, "smartDataBrowser2":U).
        THIS-OBJECT:smartDataBrowser2:EditInViewerOnDefaultAction = FALSE.
        THIS-OBJECT:smartDataBrowser2:FetchAllRecordsOnGroupBy = FALSE.
        THIS-OBJECT:smartDataBrowser2:LinkDataSource = THIS-OBJECT:instancesAdapter.
        THIS-OBJECT:smartDataBrowser2:Name = "smartDataBrowser2":U.
        THIS-OBJECT:smartDataBrowser2:RepositionedRow = 0.
        THIS-OBJECT:smartDataBrowser2:SettingsKey = "dcf93514-6061-548f-1b14-c6cddc817c76":U.
        THIS-OBJECT:smartDataBrowser2:SmartFilterActive = FALSE.
        THIS-OBJECT:smartDataBrowser2:SmartFilterUseMatches = FALSE.
        THIS-OBJECT:smartDataBrowser2:SmartFilterUseOperators = FALSE.
        THIS-OBJECT:smartDataBrowser2:EnabledChanged:Subscribe(THIS-OBJECT:smartDataBrowser2_EnabledChanged).
        /*  */
        /* instancesBindingSource */
        /*  */
        THIS-OBJECT:instancesBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:instancesBindingSource:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:instancesBindingSource:EntityTable = "eSmartObjectInstance":U.
        THIS-OBJECT:instancesBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:instancesBindingSource:NoLOBs = FALSE.
        tableDesc3:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
        arrayvar15[1] = NEW Progress.Data.ColumnPropDesc("ObjectInstanceGuid":U, "ObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[3] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[4] = NEW Progress.Data.ColumnPropDesc("ObjectMasterName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[5] = NEW Progress.Data.ColumnPropDesc("ParentInstanceGuid":U, "Parent Instance":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[6] = NEW Progress.Data.ColumnPropDesc("LayoutPosition":U, "Layout Position":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[7] = NEW Progress.Data.ColumnPropDesc("InstanceName":U, "Instance Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[8] = NEW Progress.Data.ColumnPropDesc("InstanceDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[9] = NEW Progress.Data.ColumnPropDesc("PageGuid":U, "Page":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[10] = NEW Progress.Data.ColumnPropDesc("ObjectSequence":U, "Object Sequence":U, Progress.Data.DataType:INTEGER).
        tableDesc3:Columns = arrayvar15.
        THIS-OBJECT:instancesBindingSource:TableSchema = tableDesc3.
        /*  */
        /* instancesAdapter */
        /*  */
        THIS-OBJECT:instancesAdapter:AppServerPartition = "":U.
        THIS-OBJECT:instancesAdapter:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:instancesAdapter:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:instancesAdapter:EntityTable = "eSmartObjectInstance":U.
        THIS-OBJECT:instancesAdapter:ForeignFields = "ObjectMasterGuid,ContainerObjectMasterGuid":U.
        THIS-OBJECT:instancesAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:instancesAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:instancesAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:instancesAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:instancesAdapter:QuerySort = "BY eSmartObjectInstance.ObjectSequence":U.
        THIS-OBJECT:instancesAdapter:SmartDataSourceActive = TRUE.
        THIS-OBJECT:instancesAdapter:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:instancesAdapter:CurrentChanged:Subscribe(THIS-OBJECT:instancesAdapter_CurrentChanged).
        /*  */
        /* panel2 */
        /*  */
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:cbPageGuid).
        resources:ApplyResources(THIS-OBJECT:panel2, "panel2":U).
        THIS-OBJECT:panel2:Name = "panel2":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* cbPageGuid */
        /*  */
        resources:ApplyResources(THIS-OBJECT:cbPageGuid, "cbPageGuid":U).
        THIS-OBJECT:cbPageGuid:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbPageGuid:Name = "cbPageGuid":U.
        THIS-OBJECT:cbPageGuid:ValueChanged:Subscribe(THIS-OBJECT:cbPageGuid_ValueChanged).
        /*  */
        /* ultraSplitter4 */
        /*  */
        THIS-OBJECT:ultraSplitter4:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(240)), System.Convert:ToInt32(System.Convert:ToByte(246)), System.Convert:ToInt32(System.Convert:ToByte(254))).
        resources:ApplyResources(THIS-OBJECT:ultraSplitter4, "ultraSplitter4":U).
        THIS-OBJECT:ultraSplitter4:Name = "ultraSplitter4":U.
        THIS-OBJECT:ultraSplitter4:RestoreExtent = 300.
        /*  */
        /* smartObjectInstanceViewerControl1 */
        /*  */
        THIS-OBJECT:smartObjectInstanceViewerControl1:ActivateCurrentGridRowDuringEdit = TRUE.
        THIS-OBJECT:smartObjectInstanceViewerControl1:AllowCloseOfUnmodifiedNewRecord = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartObjectInstanceViewerControl1, "smartObjectInstanceViewerControl1":U).
        THIS-OBJECT:smartObjectInstanceViewerControl1:FilterComboEditor = ?.
        THIS-OBJECT:smartObjectInstanceViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartObjectInstanceViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartObjectInstanceViewerControl1:LinkDataSource = THIS-OBJECT:instancesAdapter.
        THIS-OBJECT:smartObjectInstanceViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartObjectInstanceViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartObjectInstanceViewerControl1:Name = "smartObjectInstanceViewerControl1":U.
        THIS-OBJECT:smartObjectInstanceViewerControl1:ReadOnlyDisabled = FALSE.
        THIS-OBJECT:smartObjectInstanceViewerControl1:SaveMode = FALSE.
        THIS-OBJECT:smartObjectInstanceViewerControl1:AfterSaveChanges:Subscribe(THIS-OBJECT:smartObjectInstanceViewerControl1_AfterSaveChanges).
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartPropertyGridControl1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl3, "ultraTabPageControl3":U).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 230.
        /*  */
        /* smartPropertyGridControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartPropertyGridControl1, "smartPropertyGridControl1":U).
        THIS-OBJECT:smartPropertyGridControl1:Name = "smartPropertyGridControl1":U.
        /*  */
        /* ultraTabPageControl4 */
        /*  */
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:smartLinkViewerControl1).
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:ultraSplitter5).
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:smartDataBrowser3).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl4, "ultraTabPageControl4":U).
        THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4":U.
        /*  */
        /* smartLinkViewerControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartLinkViewerControl1, "smartLinkViewerControl1":U).
        THIS-OBJECT:smartLinkViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartLinkViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartLinkViewerControl1:LinkDataSource = THIS-OBJECT:linksAdapter.
        THIS-OBJECT:smartLinkViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartLinkViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartLinkViewerControl1:Name = "smartLinkViewerControl1":U.
        /*  */
        /* linksAdapter */
        /*  */
        THIS-OBJECT:linksAdapter:AppServerPartition = "":U.
        THIS-OBJECT:linksAdapter:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:linksAdapter:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:linksAdapter:EntityTable = "eSmartLink":U.
        THIS-OBJECT:linksAdapter:ForeignFields = "ObjectMasterGuid,ContainerObjectMasterGuid":U.
        THIS-OBJECT:linksAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:linksAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:linksAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:linksAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:linksAdapter:QuerySort = "":U.
        THIS-OBJECT:linksAdapter:SmartDataSourceActive = TRUE.
        THIS-OBJECT:linksAdapter:ThrowErrorsFromRetrieveData = FALSE.
        /*  */
        /* ultraSplitter5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraSplitter5, "ultraSplitter5":U).
        THIS-OBJECT:ultraSplitter5:Name = "ultraSplitter5":U.
        THIS-OBJECT:ultraSplitter5:RestoreExtent = 0.
        /*  */
        /* smartDataBrowser3 */
        /*  */
        THIS-OBJECT:smartDataBrowser3:DataSource = THIS-OBJECT:linksBindingSource.
        THIS-OBJECT:smartDataBrowser3:DeselectRowsOnActivate = TRUE.
        appearance13:BackColor = System.Drawing.SystemColors:Window.
        appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Appearance = appearance13.
        ultraGridColumn35:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn35:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn35:Header, "ultraGridColumn35.Header":U).
        ultraGridColumn35:Header:VisiblePosition = 0.
        ultraGridColumn35:Hidden = TRUE.
        ultraGridColumn35:ForceApplyResources = "Header":U.
        ultraGridColumn20:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn20:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn20:Header, "ultraGridColumn20.Header":U).
        ultraGridColumn20:Header:VisiblePosition = 1.
        ultraGridColumn20:Hidden = TRUE.
        ultraGridColumn20:ForceApplyResources = "Header":U.
        ultraGridColumn36:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn36:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn36:Header, "ultraGridColumn36.Header":U).
        ultraGridColumn36:Header:VisiblePosition = 2.
        ultraGridColumn36:Hidden = TRUE.
        ultraGridColumn36:ForceApplyResources = "Header":U.
        ultraGridColumn37:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn37:Header, "ultraGridColumn37.Header":U).
        ultraGridColumn37:Header:VisiblePosition = 3.
        ultraGridColumn37:ForceApplyResources = "Header":U.
        ultraGridColumn38:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn38:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn38:Header, "ultraGridColumn38.Header":U).
        ultraGridColumn38:Header:VisiblePosition = 4.
        ultraGridColumn38:Hidden = TRUE.
        ultraGridColumn38:ForceApplyResources = "Header":U.
        ultraGridColumn39:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn39:Header, "ultraGridColumn39.Header":U).
        ultraGridColumn39:Header:VisiblePosition = 5.
        ultraGridColumn39:ForceApplyResources = "Header":U.
        ultraGridColumn40:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn40:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn40:Header, "ultraGridColumn40.Header":U).
        ultraGridColumn40:Header:VisiblePosition = 6.
        ultraGridColumn40:ForceApplyResources = "Header":U.
        ultraGridColumn41:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn41:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn41:Header, "ultraGridColumn41.Header":U).
        ultraGridColumn41:Header:VisiblePosition = 7.
        ultraGridColumn41:Hidden = TRUE.
        ultraGridColumn41:ForceApplyResources = "Header":U.
        ultraGridColumn42:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn42:Header, "ultraGridColumn42.Header":U).
        ultraGridColumn42:Header:VisiblePosition = 8.
        ultraGridColumn42:ForceApplyResources = "Header":U.
        ultraGridColumn43:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn43:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn43:Header, "ultraGridColumn43.Header":U).
        ultraGridColumn43:Header:VisiblePosition = 9.
        ultraGridColumn43:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS System.Object EXTENT 10 NO-UNDO.
        arrayvar16[1] = ultraGridColumn35.
        arrayvar16[2] = ultraGridColumn20.
        arrayvar16[3] = ultraGridColumn36.
        arrayvar16[4] = ultraGridColumn37.
        arrayvar16[5] = ultraGridColumn38.
        arrayvar16[6] = ultraGridColumn39.
        arrayvar16[7] = ultraGridColumn40.
        arrayvar16[8] = ultraGridColumn41.
        arrayvar16[9] = ultraGridColumn42.
        arrayvar16[10] = ultraGridColumn43.
        ultraGridBand2:Columns:AddRange(arrayvar16).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance14:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:Appearance = appearance14.
        appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance16:BackColor2 = System.Drawing.SystemColors:Control.
        appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxRowScrollRegions = 1.
        appearance17:BackColor = System.Drawing.SystemColors:Window.
        appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveCellAppearance = appearance17.
        appearance18:BackColor = System.Drawing.SystemColors:Highlight.
        appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveRowAppearance = appearance18.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance19:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CardAreaAppearance = appearance19.
        appearance20:BorderColor = System.Drawing.Color:Silver.
        appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellAppearance = appearance20.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellPadding = 0.
        appearance21:BackColor = System.Drawing.SystemColors:Control.
        appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance21:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:GroupByRowAppearance = appearance21.
        resources:ApplyResources(appearance22, "appearance22":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderAppearance = appearance22.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance23:BackColor = System.Drawing.SystemColors:Window.
        appearance23:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowAppearance = appearance23.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser3, "smartDataBrowser3":U).
        THIS-OBJECT:smartDataBrowser3:EditInViewerOnDefaultAction = FALSE.
        THIS-OBJECT:smartDataBrowser3:FetchAllRecordsOnGroupBy = FALSE.
        THIS-OBJECT:smartDataBrowser3:LinkDataSource = THIS-OBJECT:linksAdapter.
        THIS-OBJECT:smartDataBrowser3:Name = "smartDataBrowser3":U.
        THIS-OBJECT:smartDataBrowser3:RepositionedRow = 0.
        THIS-OBJECT:smartDataBrowser3:SettingsKey = "df287328-d118-6eb9-1d14-7a76f0c5235f":U.
        THIS-OBJECT:smartDataBrowser3:SmartFilterActive = FALSE.
        THIS-OBJECT:smartDataBrowser3:SmartFilterUseMatches = FALSE.
        THIS-OBJECT:smartDataBrowser3:SmartFilterUseOperators = FALSE.
        /*  */
        /* linksBindingSource */
        /*  */
        THIS-OBJECT:linksBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:linksBindingSource:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:linksBindingSource:EntityTable = "eSmartLink":U.
        THIS-OBJECT:linksBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:linksBindingSource:NoLOBs = FALSE.
        tableDesc2:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
        arrayvar17[1] = NEW Progress.Data.ColumnPropDesc("LinkGuid":U, "LinkGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[3] = NEW Progress.Data.ColumnPropDesc("LinkTypeGuid":U, "LinkTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[4] = NEW Progress.Data.ColumnPropDesc("LinkName":U, "Link Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[5] = NEW Progress.Data.ColumnPropDesc("SourceObjectInstanceGuid":U, "SourceObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[6] = NEW Progress.Data.ColumnPropDesc("SourceObjectName":U, "Source Object":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[7] = NEW Progress.Data.ColumnPropDesc("SourcePageNumber":U, "Source Page":U, Progress.Data.DataType:INTEGER).
        arrayvar17[8] = NEW Progress.Data.ColumnPropDesc("TargetObjectInstanceGuid":U, "TargetObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[9] = NEW Progress.Data.ColumnPropDesc("TargetObjectName":U, "Source Object":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[10] = NEW Progress.Data.ColumnPropDesc("TargetPageNumber":U, "Source Page":U, Progress.Data.DataType:INTEGER).
        tableDesc2:Columns = arrayvar17.
        THIS-OBJECT:linksBindingSource:TableSchema = tableDesc2.
        /*  */
        /* SmartObjectMasterForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartObjectMasterForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartObjectMasterForm_Fill_Panel, "SmartObjectMasterForm_Fill_Panel":U).
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:Name = "SmartObjectMasterForm_Fill_Panel":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:CloseButtonLocation = Infragistics.Win.UltraWinTabs.TabCloseButtonLocation:None.
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl4).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl5).
        resources:ApplyResources(THIS-OBJECT:ultraTabControl1, "ultraTabControl1":U).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        appearance37:Image = CAST(resources:GetObject("appearance37.Image":U), System.Object).
        ultraTab1:Appearance = appearance37.
        ultraTab1:Key = "master":U.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        resources:ApplyResources(ultraTab1, "ultraTab1":U).
        ultraTab1:ForceApplyResources = "":U.
        appearance38:Image = CAST(resources:GetObject("appearance38.Image":U), System.Object).
        ultraTab2:Appearance = appearance38.
        ultraTab2:Key = "pages":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        resources:ApplyResources(ultraTab2, "ultraTab2":U).
        ultraTab2:ForceApplyResources = "":U.
        appearance39:Image = CAST(resources:GetObject("appearance39.Image":U), System.Object).
        ultraTab5:Appearance = appearance39.
        ultraTab5:Key = "instances":U.
        ultraTab5:TabPage = THIS-OBJECT:ultraTabPageControl5.
        resources:ApplyResources(ultraTab5, "ultraTab5":U).
        ultraTab5:ForceApplyResources = "":U.
        appearance40:Image = CAST(resources:GetObject("appearance40.Image":U), System.Object).
        ultraTab3:Appearance = appearance40.
        ultraTab3:Key = "design":U.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        resources:ApplyResources(ultraTab3, "ultraTab3":U).
        ultraTab3:ForceApplyResources = "":U.
        appearance41:Image = CAST(resources:GetObject("appearance41.Image":U), System.Object).
        ultraTab4:Appearance = appearance41.
        ultraTab4:Key = "links":U.
        ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl4.
        resources:ApplyResources(ultraTab4, "ultraTab4":U).
        ultraTab4:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 5 NO-UNDO.
        arrayvar18[1] = ultraTab1.
        arrayvar18[2] = ultraTab2.
        arrayvar18[3] = ultraTab5.
        arrayvar18[4] = ultraTab3.
        arrayvar18[5] = ultraTab4.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar18).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage1, "ultraTabSharedControlsPage1":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc4:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS Progress.Data.ColumnPropDesc EXTENT 14 NO-UNDO.
        arrayvar19[1] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[2] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[3] = NEW Progress.Data.ColumnPropDesc("CustomizationResultGuid":U, "Customization Result":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[4] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[5] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[6] = NEW Progress.Data.ColumnPropDesc("ObjectDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[7] = NEW Progress.Data.ColumnPropDesc("ObjectPackage":U, "Object Package":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[8] = NEW Progress.Data.ColumnPropDesc("ObjectExtension":U, "Extension":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[9] = NEW Progress.Data.ColumnPropDesc("StaticObject":U, "Static Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar19[10] = NEW Progress.Data.ColumnPropDesc("TemplateObject":U, "Template Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar19[11] = NEW Progress.Data.ColumnPropDesc("DeploymentType":U, "Deployment Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar19[12] = NEW Progress.Data.ColumnPropDesc("RunnableFromMenu":U, "Runnable from Menu":U, Progress.Data.DataType:LOGICAL).
        arrayvar19[13] = NEW Progress.Data.ColumnPropDesc("Disabled":U, "Disabled":U, Progress.Data.DataType:LOGICAL).
        arrayvar19[14] = NEW Progress.Data.ColumnPropDesc("DesignTimeDataSourceGuid":U, "Design Time Data Source":U, Progress.Data.DataType:CHARACTER).
        tableDesc4:Columns = arrayvar19.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc4.
        /*  */
        /* m_SmartObjectMasterForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left, "m_SmartObjectMasterForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:Name = "m_SmartObjectMasterForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectMasterForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right, "m_SmartObjectMasterForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:Name = "m_SmartObjectMasterForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectMasterForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top, "m_SmartObjectMasterForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:Name = "m_SmartObjectMasterForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom, "m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartObjectMasterForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartObjectMasterForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectMasterForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartObjectMasterForm":U.
        THIS-OBJECT:PrimaryDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:PrimaryDataTarget = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:PrimaryNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartObjectMasterViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartPageViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pagesAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pagesBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl5:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:instancesBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:instancesAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel2:ResumeLayout(FALSE).
        THIS-OBJECT:panel2:PerformLayout().
        CAST(THIS-OBJECT:cbPageGuid, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartObjectInstanceViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl4:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartLinkViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:linksAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:linksBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartObjectMasterForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Initializes the Designer for the current type
     * Notes:
     */
    METHOD PROTECTED VOID InitializeDesinger():

        MESSAGE "tbd":U
            VIEW-AS ALERT-BOX.

    END METHOD.

    /**
     * Purpose: Event handler for the ValueChanged event of the cbPageGuid
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID cbPageGuid_ValueChanged (sender AS System.Object,
                                                 e AS System.EventArgs):

        instancesAdapter:RetrieveData() .

    END METHOD.

    /**
     * Purpose: Event handler for the CollectFilterValues event of the instancesAdapter
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The CollectFilterValuesEventArgs with the data for this event
     */
    METHOD PRIVATE VOID instancesAdapter_CollectFilterValues (sender AS Progress.Lang.Object,
                                                              e AS CollectFilterValuesEventArgs):

        IF cbPageGuid:SelectedIndex > 0 THEN
            e:QueryExpressions:GetItem ("eSmartObjectInstance":U):Add ("eSmartObjectInstance.PageGuid":U, OperatorEnum:EQ, UNBOX (cbPageGuid:Value)) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the CurrentChanged event of the instancesAdapter
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID instancesAdapter_CurrentChanged (sender AS System.Object,
                                                         e AS System.EventArgs):

        IF NOT VALID-OBJECT (ultraTabControl1:SelectedTab) OR
           ultraTabControl1:SelectedTab:Key <> "instances":U THEN
            RETURN .

        THIS-OBJECT:ShowObjectInstanceDetails() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Raises the Shown Event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() .

        THIS-OBJECT:OpenObjectMaster() .

        IF THIS-OBJECT:smartBusinessEntityAdapter1:Available = ? OR
           THIS-OBJECT:smartBusinessEntityAdapter1:Available = FALSE THEN DO:

            THIS-OBJECT:smartBusinessEntityAdapter1:RetrieveData () .

            THIS-OBJECT:ShowObjectDetails () .
        END.

    END METHOD .

    /**
     * Purpose: Event handler for the HasChangesChanged event of the objectInstanceAttributesUserControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID objectInstanceAttributesUserControl1_HasChangesChanged (sender AS System.Object,
                                                                                  e AS System.EventArgs):

        IF objectInstanceAttributesUserControl1:HasChanges THEN DO:

            IF smartObjectInstanceViewerControl1:SmartTableIOState = TableIOStateEnum:FieldsDisabled THEN
                smartObjectInstanceViewerControl1:EnableFields(EnableFieldsEnum:Update) .

            smartObjectInstanceViewerControl1:SetModifyingState() .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the HasChangesChanged event of the objectMasterAttributesUserControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID objectMasterAttributesUserControl1_HasChangesChanged (sender AS System.Object,
                                                                              e AS System.EventArgs):

        IF objectMasterAttributesUserControl1:HasChanges THEN DO:

            IF smartObjectMasterViewerControl1:SmartTableIOState = TableIOStateEnum:FieldsDisabled THEN
                smartObjectMasterViewerControl1:EnableFields(EnableFieldsEnum:Update) .

            smartObjectMasterViewerControl1:SetModifyingState() .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Opens an Object Master
     * Notes:
     */
    METHOD PROTECTED VOID OpenObjectMaster():

        DEFINE VARIABLE oForm AS OpenObjectMasterDialogForm NO-UNDO .

        oForm = NEW OpenObjectMasterDialogForm () .

        WAIT-FOR oForm:ShowDialog () .

        IF oForm:SelectedObjectMasterGuid > "":U THEN DO:
            smartBusinessEntityAdapter1:FindRowWhere (SUBSTITUTE ("where eSmartObjectMaster.ObjectMasterGuid = &1":U,
                                                                  QUOTER (oForm:SelectedObjectMasterGuid)),
                                                      FindRowWhereModeEnum:FindOnServerOnly) .

            THIS-OBJECT:ShowObjectDetails() .
        END.

    END METHOD.

    /**
     * Purpose: Shows the details of the current object
     * Notes:
     */
    METHOD PROTECTED VOID ShowObjectDetails():

        IF smartBusinessEntityAdapter1:Available THEN DO:

            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                           "GetObjectMasterDesignAttributes":U,
                                                           INPUT-OUTPUT DATASET dsDesignAttributeValue BY-REFERENCE,
                                                           NEW GetObjectMasterDesignAttributesParameter (smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U))) .

            DatasetHelper:SetTrackingChanges (DATASET dsDesignAttributeValue:HANDLE, TRUE) .

            objectMasterAttributesUserControl1:ReloadAttributes () .
            smartObjectInstanceViewerControl1:UpdatePagesComboEditor() .
        END.

    END METHOD.

    /**
     * Purpose: Shows the details of the current object instance
     * Notes:
     */
    METHOD PROTECTED VOID ShowObjectInstanceDetails():

        IF instancesAdapter:Available AND NOT objectInstanceAttributesUserControl1:HasChanges THEN DO:

            IF instancesAdapter:GetFieldValues("ObjectInstanceGuid":U) > "":U THEN DO:
                FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                               "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                               "GetObjectInstanceDesignAttributes":U,
                                                               INPUT-OUTPUT DATASET dsDesignAttributeValue_inst BY-REFERENCE,
                                                               NEW GetObjectInstanceDesignAttributesParameter (instancesAdapter:GetFieldValues("ObjectInstanceGuid":U))) .

                DatasetHelper:SetTrackingChanges (DATASET dsDesignAttributeValue_inst:HANDLE, TRUE) .
            END.
            ELSE DO: /* creating */
                DatasetHelper:SetTrackingChanges (DATASET dsDesignAttributeValue_inst:HANDLE, FALSE) .
                DATASET dsDesignAttributeValue_inst:EMPTY-DATASET () .
            END.

            objectInstanceAttributesUserControl1:ObjectInstanceId = instancesAdapter:GetFieldValues("ObjectInstanceGuid":U) .
            objectInstanceAttributesUserControl1:ObjectMasterId   = instancesAdapter:GetFieldValues("ObjectMasterGuid":U) .
            objectInstanceAttributesUserControl1:ReloadAttributes () .
        END.

    END METHOD.

    /**
     * Purpose: Event handler for the CurrentChanged of the smartBusinessEntityAdapter1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartBusinessEntityAdapter1_CurrentChanged (sender AS System.Object,
                                                                    e AS System.EventArgs):

        DEFINE VARIABLE oMasterAttribute AS GetObjectMasterAttributeValueParameter NO-UNDO .
        DEFINE VARIABLE hDataset         AS HANDLE                                 NO-UNDO .

        IF smartBusinessEntityAdapter1:Available AND smartBusinessEntityAdapter1:GetFieldValues("ObjectTypeGuid":U) > "":U THEN
            THIS-OBJECT:EditingContainer = (NEW ObjectTypeDatasetModel
                (smartBusinessEntityAdapter1:GetFieldValues("ObjectTypeGuid":U)):SmartObjectType:ContainerType) .

        ELSE
            THIS-OBJECT:EditingContainer = FALSE .

        IF THIS-OBJECT:EditingContainer AND
           smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U) > "":U THEN DO ON ERROR UNDO, THROW:

            ASSIGN oMasterAttribute = NEW GetObjectMasterAttributeValueParameter (smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U),
                                                                                  "DesignerType":U) .
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                           "GetObjectMasterAttributeValue":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oMasterAttribute) .

            THIS-OBJECT:DesignerType = oMasterAttribute:CharacterValue .

            IF THIS-OBJECT:DesignerType > "":U AND ultraTabControl1:SelectedTab:Key = "design":U THEN
                THIS-OBJECT:InitializeDesinger () .

            @SuppressUnusedWarnings.
            CATCH rex AS RepositoryException:
                THIS-OBJECT:DesignerType = "":U .
            END CATCH.
        END.
        ELSE
            THIS-OBJECT:DesignerType = "":U .

        THIS-OBJECT:EnableTabs (smartObjectMasterViewerControl1:SmartTableIOState = TableIOStateEnum:FieldsDisabled) .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW err .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the EnabledChanged event of the smartDataBrowser2
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartDataBrowser2_EnabledChanged (sender AS System.Object,
                                                          e AS System.EventArgs):

        cbPageGuid:Enabled =  smartDataBrowser2:Enabled .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handler for the AfterSaveChanges event of the smartObjectInstanceViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartObjectInstanceViewerControl1_AfterSaveChanges (sender AS System.Object,
                                                                            e AS System.EventArgs):

        IF smartObjectMasterViewerControl1:AddingRecord OR
           smartObjectMasterViewerControl1:CopyingRecord THEN DO:
/*
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                           "GetObjectMasterDesignAttributes":U,
                                                           INPUT-OUTPUT DATASET dsDesignAttributeValue BY-REFERENCE,
                                                           NEW GetObjectMasterDesignAttributesParameter (smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U))) .

            objectMasterAttributesUserControl1:ResetChanges() .
            */
            DatasetHelper:SetTrackingChanges(DATASET dsDesignAttributeValue_inst:HANDLE, TRUE) .
        END.
        ELSE
            objectInstanceAttributesUserControl1:SaveChanges (instancesAdapter:GetFieldValues("ObjectInstanceGuid":U)) .

        objectInstanceAttributesUserControl1:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            smartObjectInstanceViewerControl1:EnableFields (EnableFieldsEnum:Update) .
            smartObjectInstanceViewerControl1:SetModifyingState() .

            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the AfterAddRecord event of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID smartObjectMasterViewerControl1_AfterAddRecord (sender AS System.Object,
                                                                          e AS System.EventArgs):

        objectMasterAttributesUserControl1:Enabled = FALSE .

        DatasetHelper:SetTrackingChanges(DATASET dsDesignAttributeValue:HANDLE, FALSE) .

        DATASET dsDesignAttributeValue:EMPTY-DATASET() .

        objectMasterAttributesUserControl1:ShowObjectMasterAttributes(?) .

        THIS-OBJECT:EnableTabs (FALSE) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the AfterCancelUpdate event of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID smartObjectMasterViewerControl1_AfterCancelUpdate (sender AS System.Object,
                                                                             e AS System.EventArgs):

        DatasetHelper:SetTrackingChanges(DATASET dsDesignAttributeValue:HANDLE, FALSE) .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .

        THIS-OBJECT:ShowObjectDetails() .

        objectMasterAttributesUserControl1:ResetChanges() .

        objectMasterAttributesUserControl1:Enabled = TRUE .

        THIS-OBJECT:EnableTabs (TRUE) .

        DatasetHelper:SetTrackingChanges(DATASET dsDesignAttributeValue:HANDLE, TRUE) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the AfterDeleteRecord event of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID smartObjectMasterViewerControl1_AfterDeleteRecord (sender AS System.Object,
                                                                             e AS System.EventArgs):

        THIS-OBJECT:OpenObjectMaster() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handler for the AfterSaveChanges event of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartObjectMasterViewerControl1_AfterSaveChanges (sender AS System.Object,
                                                                          e AS System.EventArgs):

        IF smartObjectMasterViewerControl1:AddingRecord OR
           smartObjectMasterViewerControl1:CopyingRecord THEN DO:

            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                           "GetObjectMasterDesignAttributes":U,
                                                           INPUT-OUTPUT DATASET dsDesignAttributeValue BY-REFERENCE,
                                                           NEW GetObjectMasterDesignAttributesParameter (smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U))) .

            objectMasterAttributesUserControl1:ResetChanges() .

            DatasetHelper:SetTrackingChanges(DATASET dsDesignAttributeValue:HANDLE, TRUE) .
        END.
        ELSE
            objectMasterAttributesUserControl1:SaveChanges (smartBusinessEntityAdapter1:GetFieldValues("ObjectMasterGuid":U)) .

        objectMasterAttributesUserControl1:Enabled = TRUE .

        THIS-OBJECT:EnableTabs (TRUE) .

        CATCH err AS Progress.Lang.Error:
            smartObjectMasterViewerControl1:EnableFields (EnableFieldsEnum:Update) .
            smartObjectMasterViewerControl1:SetModifyingState() .

            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the FieldsDisabled of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartObjectMasterViewerControl1_FieldsDisabled (sender AS System.Object,
                                                                        e AS System.EventArgs):

        IF smartToolbarController1:Tools:Exists ("OpenObjectMaster":U) THEN
            smartToolbarController1:Tools["OpenObjectMaster":U]:SharedProps:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the FieldsEnabled of the smartObjectMasterViewerControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartObjectMasterViewerControl1_FieldsEnabled (sender AS System.Object,
                                                                       e AS System.EventArgs):

        IF smartToolbarController1:Tools:Exists ("OpenObjectMaster":U) THEN
            smartToolbarController1:Tools["OpenObjectMaster":U]:SharedProps:Enabled = FALSE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ToolClick event of the smartToolbarController1
     * Notes:
     * @param sender The Reference to the System.Object that raises the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "OpenObjectMaster":U THEN
                THIS-OBJECT:OpenObjectMaster () .

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the SelectedTabChanged event of the ultraTabControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The SelectedTabChangedEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs):

        CASE e:Tab:Index:

            WHEN 0 THEN DO:
                smartToolbarController1:ActivateSmartNavigationTarget(smartBusinessEntityAdapter1) .
                smartToolbarController1:ActivateSmartTableIOTarget(smartObjectMasterViewerControl1) .
            END.
            WHEN 1 THEN DO:
                smartToolbarController1:ActivateSmartNavigationTarget(pagesAdapter) .
                smartToolbarController1:ActivateSmartTableIOTarget(smartPageViewerControl1) .
            END.
            WHEN 2 THEN DO:
                smartToolbarController1:ActivateSmartNavigationTarget(instancesAdapter) .
                smartToolbarController1:ActivateSmartTableIOTarget(smartObjectInstanceViewerControl1) .

                smartObjectInstanceViewerControl1:UpdatePagesComboEditor() .

                IF smartObjectInstanceViewerControl1:SmartTableIOState <> TableIOStateEnum:ModifyingData THEN
                    THIS-OBJECT:ShowObjectInstanceDetails() .
            END.
            WHEN 4 THEN DO:
                smartToolbarController1:ActivateSmartNavigationTarget (linksAdapter) .
                smartToolbarController1:ActivateSmartTableIOTarget (smartLinkViewerControl1) .


            END.

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Destructor for the SmartObjectMasterForm class
     * Notes:
     */
    DESTRUCTOR PUBLIC SmartObjectMasterForm ():

    END DESTRUCTOR .

END CLASS.
