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
    File        : ResourceTranslationMaintenanceForm
    Purpose     : Translation of Localizable resx files
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 09 22:54:49 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Session.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Localization.*    FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.Localization.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Localization.ResourceTranslationMaintenanceForm INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE ResourceTranslationMaintenanceForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.

    DEFINE VARIABLE cBaseTitle          AS CHARACTER                        NO-UNDO .
    DEFINE VARIABLE oResourceTranslator AS ResourceTranslator               NO-UNDO .
    DEFINE VARIABLE oTranslationService AS IUserInterfaceTranslationService NO-UNDO .
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPictureBox1 AS Infragistics.Win.UltraWinEditors.UltraPictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.


    { Consultingwerk/Util/TempTables/ttClassNames.i }
    { Consultingwerk/SmartFramework/Localization/eStringResources.i }
    { Consultingwerk/SmartFramework/ttLanguage.i &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY }

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the UI class that is being translated file name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the current resource file name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceFileName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ResourceTranslationMaintenanceForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ResourceTranslationMaintenanceForm ():

        SUPER().

        InitializeComponent().

        ASSIGN cBaseTitle = THIS-OBJECT:Text .

        oTranslationService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Localization.IUserInterfaceTranslationService
                                                            "NEW Consultingwerk.SmartFramework.Localization.UserInterfaceTranslationService()"} .

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE eStringResources:HANDLE.
        smartTempTableAdapter1:RetrieveData () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Filters out non localizable Controls
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FilterLocalizableControls ():

        DEFINE VARIABLE cResourceFileName AS CHARACTER NO-UNDO.

        DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.

        FOR EACH ttClassNames:

            ASSIGN cResourceFileName = LocalizationHelper:ClassNameToResourceFileName (ttClassNames.ClassName) .

            FILE-INFORMATION:FILE-NAME = cResourceFileName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
                DELETE ttClassNames .
                NEXT .
            END.

            oResourceTranslator = NEW ResourceTranslator (FILE-INFORMATION:FULL-PATHNAME) .

            IF NOT oResourceTranslator:IsLocalizable THEN DO:
                DELETE ttClassNames .
                NEXT .
            END .

            CATCH err AS Progress.Lang.Error:
                DELETE ttClassNames .
                NEXT .
            END CATCH.
        END.

    END METHOD .

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
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("resourcefile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenUserControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Localizable Only":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveTranslations":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
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
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
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
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenUserControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveTranslations":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Localizable Only":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eStringResources":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ResourceKey":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OriginalString":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TranslatedString":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TranslationType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ResourceKeyPartial":U).
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
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eStringResources":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Localization.ResourceTranslationMaintenanceForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraPictureBox1 = NEW Infragistics.Win.UltraWinEditors.UltraPictureBox().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
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
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:KeyTip = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.KeyTip":U).
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
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipText = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipText":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTextFormatted = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTextFormatted":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTitle = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTitle":U).
        ribbonTab1:Caption = resources:GetString("ribbonTab1.Caption":U).
        ribbonGroup1:Caption = resources:GetString("ribbonGroup1.Caption":U).
        ribbonGroup1:KeyTip = resources:GetString("ribbonGroup1.KeyTip":U).
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        stateButtonTool1:Checked = TRUE.
        stateButtonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool12:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool18.
        arrayvar2[2] = buttonTool19.
        arrayvar2[3] = stateButtonTool1.
        arrayvar2[4] = buttonTool12.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = resources:GetString("ribbonGroup2.Caption":U).
        ribbonGroup2:KeyTip = resources:GetString("ribbonGroup2.KeyTip":U).
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar3[1] = buttonTool8.
        arrayvar3[2] = buttonTool9.
        arrayvar3[3] = buttonTool10.
        arrayvar3[4] = buttonTool11.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        ribbonGroup3:Caption = resources:GetString("ribbonGroup3.Caption":U).
        ribbonGroup3:KeyTip = resources:GetString("ribbonGroup3.KeyTip":U).
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool15:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar4[1] = buttonTool13.
        arrayvar4[2] = buttonTool14.
        arrayvar4[3] = buttonTool15.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        arrayvar5[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar5).
        ribbonTab1:KeyTip = resources:GetString("ribbonTab1.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar6[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar7[1] = buttonTool20.
        arrayvar7[2] = buttonTool21.
        arrayvar7[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar7).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:KeyTip = resources:GetString("ultraToolbar1.KeyTip":U).
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool23.
        arrayvar8[2] = buttonTool24.
        arrayvar8[3] = buttonTool25.
        arrayvar8[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar1:Text = resources:GetString("ultraToolbar1.Text":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        ultraToolbar2:KeyTip = resources:GetString("ultraToolbar2.KeyTip":U).
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar9[1] = buttonTool27.
        arrayvar9[2] = buttonTool28.
        arrayvar9[3] = buttonTool29.
        arrayvar9[4] = buttonTool30.
        arrayvar9[5] = buttonTool31.
        arrayvar9[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar2:Text = resources:GetString("ultraToolbar2.Text":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        ultraToolbar3:KeyTip = resources:GetString("ultraToolbar3.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar10[1] = buttonTool33.
        arrayvar10[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar3:Text = resources:GetString("ultraToolbar3.Text":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar11[1] = ultraToolbar1.
        arrayvar11[2] = ultraToolbar2.
        arrayvar11[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar11).
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        appearance14:TextHAlignAsString = resources:GetString("appearance14.TextHAlignAsString":U).
        appearance14:TextVAlignAsString = resources:GetString("appearance14.TextVAlignAsString":U).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool35:SharedPropsInternal:Caption = resources:GetString("resource.Caption":U).
        buttonTool35:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText":U).
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        appearance15:TextHAlignAsString = resources:GetString("appearance15.TextHAlignAsString":U).
        appearance15:TextVAlignAsString = resources:GetString("appearance15.TextVAlignAsString":U).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool36:SharedPropsInternal:Caption = resources:GetString("resource.Caption1":U).
        buttonTool36:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption1":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText1":U).
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        appearance16:TextHAlignAsString = resources:GetString("appearance16.TextHAlignAsString":U).
        appearance16:TextVAlignAsString = resources:GetString("appearance16.TextVAlignAsString":U).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool37:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        buttonTool37:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption2":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText2":U).
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        appearance17:TextHAlignAsString = resources:GetString("appearance17.TextHAlignAsString":U).
        appearance17:TextVAlignAsString = resources:GetString("appearance17.TextVAlignAsString":U).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool38:SharedPropsInternal:Caption = resources:GetString("resource.Caption3":U).
        buttonTool38:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption3":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText3":U).
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        appearance18:TextHAlignAsString = resources:GetString("appearance18.TextHAlignAsString":U).
        appearance18:TextVAlignAsString = resources:GetString("appearance18.TextVAlignAsString":U).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool39:SharedPropsInternal:Caption = resources:GetString("resource.Caption4":U).
        buttonTool39:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption4":U).
        buttonTool39:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText4":U).
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        appearance19:TextHAlignAsString = resources:GetString("appearance19.TextHAlignAsString":U).
        appearance19:TextVAlignAsString = resources:GetString("appearance19.TextVAlignAsString":U).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool40:SharedPropsInternal:Caption = resources:GetString("resource.Caption5":U).
        buttonTool40:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption5":U).
        buttonTool40:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText5":U).
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        appearance20:TextHAlignAsString = resources:GetString("appearance20.TextHAlignAsString":U).
        appearance20:TextVAlignAsString = resources:GetString("appearance20.TextVAlignAsString":U).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool41:SharedPropsInternal:Caption = resources:GetString("resource.Caption6":U).
        buttonTool41:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption6":U).
        buttonTool41:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText6":U).
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        appearance21:TextHAlignAsString = resources:GetString("appearance21.TextHAlignAsString":U).
        appearance21:TextVAlignAsString = resources:GetString("appearance21.TextVAlignAsString":U).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool42:SharedPropsInternal:Caption = resources:GetString("resource.Caption7":U).
        buttonTool42:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption7":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText7":U).
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        appearance22:TextHAlignAsString = resources:GetString("appearance22.TextHAlignAsString":U).
        appearance22:TextVAlignAsString = resources:GetString("appearance22.TextVAlignAsString":U).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool43:SharedPropsInternal:Caption = resources:GetString("resource.Caption8":U).
        buttonTool43:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption8":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText8":U).
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        appearance23:TextHAlignAsString = resources:GetString("appearance23.TextHAlignAsString":U).
        appearance23:TextVAlignAsString = resources:GetString("appearance23.TextVAlignAsString":U).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool44:SharedPropsInternal:Caption = resources:GetString("resource.Caption9":U).
        buttonTool44:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption9":U).
        buttonTool44:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText9":U).
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        appearance24:TextHAlignAsString = resources:GetString("appearance24.TextHAlignAsString":U).
        appearance24:TextVAlignAsString = resources:GetString("appearance24.TextVAlignAsString":U).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool45:SharedPropsInternal:Caption = resources:GetString("resource.Caption10":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        appearance25:TextHAlignAsString = resources:GetString("appearance25.TextHAlignAsString":U).
        appearance25:TextVAlignAsString = resources:GetString("appearance25.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        appearance26:TextHAlignAsString = resources:GetString("appearance26.TextHAlignAsString":U).
        appearance26:TextVAlignAsString = resources:GetString("appearance26.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool46:SharedPropsInternal:Caption = resources:GetString("resource.Caption11":U).
        buttonTool46:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption10":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText10":U).
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        appearance27:TextHAlignAsString = resources:GetString("appearance27.TextHAlignAsString":U).
        appearance27:TextVAlignAsString = resources:GetString("appearance27.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance27.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        appearance28:TextHAlignAsString = resources:GetString("appearance28.TextHAlignAsString":U).
        appearance28:TextVAlignAsString = resources:GetString("appearance28.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance28.
        buttonTool47:SharedPropsInternal:Caption = resources:GetString("resource.Caption12":U).
        buttonTool47:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption11":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText11":U).
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        appearance29:TextHAlignAsString = resources:GetString("appearance29.TextHAlignAsString":U).
        appearance29:TextVAlignAsString = resources:GetString("appearance29.TextVAlignAsString":U).
        buttonTool48:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        appearance30:TextHAlignAsString = resources:GetString("appearance30.TextHAlignAsString":U).
        appearance30:TextVAlignAsString = resources:GetString("appearance30.TextVAlignAsString":U).
        buttonTool48:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool48:SharedPropsInternal:Caption = resources:GetString("resource.Caption13":U).
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        appearance31:TextHAlignAsString = resources:GetString("appearance31.TextHAlignAsString":U).
        appearance31:TextVAlignAsString = resources:GetString("appearance31.TextVAlignAsString":U).
        buttonTool49:SharedPropsInternal:AppearancesLarge:Appearance = appearance31.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        appearance32:TextHAlignAsString = resources:GetString("appearance32.TextHAlignAsString":U).
        appearance32:TextVAlignAsString = resources:GetString("appearance32.TextVAlignAsString":U).
        buttonTool49:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        buttonTool49:SharedPropsInternal:Caption = resources:GetString("resource.Caption14":U).
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        appearance33:TextHAlignAsString = resources:GetString("appearance33.TextHAlignAsString":U).
        appearance33:TextVAlignAsString = resources:GetString("appearance33.TextVAlignAsString":U).
        buttonTool16:SharedPropsInternal:AppearancesLarge:Appearance = appearance33.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        appearance34:TextHAlignAsString = resources:GetString("appearance34.TextHAlignAsString":U).
        appearance34:TextVAlignAsString = resources:GetString("appearance34.TextVAlignAsString":U).
        buttonTool16:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        buttonTool16:SharedPropsInternal:Caption = resources:GetString("resource.Caption15":U).
        stateButtonTool2:Checked = TRUE.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        appearance35:TextHAlignAsString = resources:GetString("appearance35.TextHAlignAsString":U).
        appearance35:TextVAlignAsString = resources:GetString("appearance35.TextVAlignAsString":U).
        stateButtonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance35.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        appearance36:TextHAlignAsString = resources:GetString("appearance36.TextHAlignAsString":U).
        appearance36:TextVAlignAsString = resources:GetString("appearance36.TextVAlignAsString":U).
        stateButtonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance36.
        stateButtonTool2:SharedPropsInternal:Caption = resources:GetString("resource.Caption16":U).
        stateButtonTool2:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText12":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 17 NO-UNDO.
        arrayvar12[1] = buttonTool35.
        arrayvar12[2] = buttonTool36.
        arrayvar12[3] = buttonTool37.
        arrayvar12[4] = buttonTool38.
        arrayvar12[5] = buttonTool39.
        arrayvar12[6] = buttonTool40.
        arrayvar12[7] = buttonTool41.
        arrayvar12[8] = buttonTool42.
        arrayvar12[9] = buttonTool43.
        arrayvar12[10] = buttonTool44.
        arrayvar12[11] = buttonTool45.
        arrayvar12[12] = buttonTool46.
        arrayvar12[13] = buttonTool47.
        arrayvar12[14] = buttonTool48.
        arrayvar12[15] = buttonTool49.
        arrayvar12[16] = buttonTool16.
        arrayvar12[17] = stateButtonTool2.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar12).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* ResourceTranslationMaintenanceForm_Fill_Panel */
        /*  */
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:AccessibleDescription = resources:GetString("ResourceTranslationMaintenanceForm_Fill_Panel.AccessibleDescription":U).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:AccessibleName = resources:GetString("ResourceTranslationMaintenanceForm_Fill_Panel.AccessibleName":U).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Anchor = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:AutoSize = System.Convert:ToBoolean(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.AutoSize":U)).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:BackgroundImageLayout = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* ResourceTranslationMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:AccessibleDescription = resources:GetString("ResourceTranslationMaintenanceForm_Fill_Panel.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:AccessibleName = resources:GetString("ResourceTranslationMaintenanceForm_Fill_Panel.ClientArea.AccessibleName":U).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:Font = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Dock = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Font = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.Font":U), System.Drawing.Font).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ImeMode = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Location = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.Location":U), System.Drawing.Point).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:MaximumSize = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Name = "ResourceTranslationMaintenanceForm_Fill_Panel":U.
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:RightToLeft = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:Size = CAST(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.Size":U), System.Drawing.Size).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:TabIndex = System.Convert:ToInt32(resources:GetObject("ResourceTranslationMaintenanceForm_Fill_Panel.TabIndex":U)).
        /*  */
        /* ultraPanel1 */
        /*  */
        THIS-OBJECT:ultraPanel1:AccessibleDescription = resources:GetString("ultraPanel1.AccessibleDescription":U).
        THIS-OBJECT:ultraPanel1:AccessibleName = resources:GetString("ultraPanel1.AccessibleName":U).
        THIS-OBJECT:ultraPanel1:Anchor = CAST(resources:GetObject("ultraPanel1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraPanel1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraPanel1.AutoSize":U)).
        THIS-OBJECT:ultraPanel1:BackgroundImageLayout = CAST(resources:GetObject("ultraPanel1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:AccessibleDescription = resources:GetString("ultraPanel1.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:ultraPanel1:ClientArea:AccessibleName = resources:GetString("ultraPanel1.ClientArea.AccessibleName":U).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraPictureBox1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraPanel1:ClientArea:Font = CAST(resources:GetObject("ultraPanel1.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraPanel1:Dock = CAST(resources:GetObject("ultraPanel1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraPanel1:Font = CAST(resources:GetObject("ultraPanel1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraPanel1:ImeMode = CAST(resources:GetObject("ultraPanel1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraPanel1:Location = CAST(resources:GetObject("ultraPanel1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraPanel1:MaximumSize = CAST(resources:GetObject("ultraPanel1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:RightToLeft = CAST(resources:GetObject("ultraPanel1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraPanel1:Size = CAST(resources:GetObject("ultraPanel1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraPanel1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraPanel1.TabIndex":U)).
        /*  */
        /* ultraPictureBox1 */
        /*  */
        THIS-OBJECT:ultraPictureBox1:AccessibleDescription = resources:GetString("ultraPictureBox1.AccessibleDescription":U).
        THIS-OBJECT:ultraPictureBox1:AccessibleName = resources:GetString("ultraPictureBox1.AccessibleName":U).
        THIS-OBJECT:ultraPictureBox1:Anchor = CAST(resources:GetObject("ultraPictureBox1.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Right.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Top.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:ultraPictureBox1:Appearance = appearance1.
        THIS-OBJECT:ultraPictureBox1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraPictureBox1.AutoSize":U)).
        THIS-OBJECT:ultraPictureBox1:BackgroundImage = CAST(resources:GetObject("ultraPictureBox1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraPictureBox1:BackgroundImageLayout = CAST(resources:GetObject("ultraPictureBox1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraPictureBox1:BorderShadowColor = System.Drawing.Color:Empty.
        THIS-OBJECT:ultraPictureBox1:Dock = CAST(resources:GetObject("ultraPictureBox1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraPictureBox1:Font = CAST(resources:GetObject("ultraPictureBox1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraPictureBox1:ImeMode = CAST(resources:GetObject("ultraPictureBox1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraPictureBox1:Location = CAST(resources:GetObject("ultraPictureBox1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraPictureBox1:MaximumSize = CAST(resources:GetObject("ultraPictureBox1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraPictureBox1:Name = "ultraPictureBox1":U.
        THIS-OBJECT:ultraPictureBox1:RightToLeft = CAST(resources:GetObject("ultraPictureBox1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraPictureBox1:Size = CAST(resources:GetObject("ultraPictureBox1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraPictureBox1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraPictureBox1.TabIndex":U)).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AccessibleDescription = resources:GetString("ultraTextEditor1.AccessibleDescription":U).
        THIS-OBJECT:ultraTextEditor1:AccessibleName = resources:GetString("ultraTextEditor1.AccessibleName":U).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(resources:GetObject("ultraTextEditor1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraTextEditor1.AutoSize":U)).
        THIS-OBJECT:ultraTextEditor1:BackgroundImage = CAST(resources:GetObject("ultraTextEditor1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraTextEditor1:BackgroundImageLayout = CAST(resources:GetObject("ultraTextEditor1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraTextEditor1:Dock = CAST(resources:GetObject("ultraTextEditor1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraTextEditor1:Enabled = System.Convert:ToBoolean(resources:GetObject("ultraTextEditor1.Enabled":U)).
        THIS-OBJECT:ultraTextEditor1:Font = CAST(resources:GetObject("ultraTextEditor1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraTextEditor1:ImeMode = CAST(resources:GetObject("ultraTextEditor1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraTextEditor1:Location = CAST(resources:GetObject("ultraTextEditor1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraTextEditor1:MaximumSize = CAST(resources:GetObject("ultraTextEditor1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:RightToLeft = CAST(resources:GetObject("ultraTextEditor1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraTextEditor1:Size = CAST(resources:GetObject("ultraTextEditor1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraTextEditor1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraTextEditor1.TabIndex":U)).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AccessibleDescription = resources:GetString("ultraLabel1.AccessibleDescription":U).
        THIS-OBJECT:ultraLabel1:AccessibleName = resources:GetString("ultraLabel1.AccessibleName":U).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(resources:GetObject("ultraLabel1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraLabel1.AutoSize":U)).
        THIS-OBJECT:ultraLabel1:BackgroundImage = CAST(resources:GetObject("ultraLabel1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraLabel1:BackgroundImageLayout = CAST(resources:GetObject("ultraLabel1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraLabel1:Dock = CAST(resources:GetObject("ultraLabel1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraLabel1:Font = CAST(resources:GetObject("ultraLabel1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraLabel1:ImeMode = CAST(resources:GetObject("ultraLabel1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraLabel1:Location = CAST(resources:GetObject("ultraLabel1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraLabel1:MaximumSize = CAST(resources:GetObject("ultraLabel1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:RightToLeft = CAST(resources:GetObject("ultraLabel1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraLabel1:Size = CAST(resources:GetObject("ultraLabel1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraLabel1.TabIndex":U)).
        THIS-OBJECT:ultraLabel1:Text = resources:GetString("ultraLabel1.Text":U).
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:AccessibleDescription = resources:GetString("smartUpdatableBrowser1.AccessibleDescription":U).
        THIS-OBJECT:smartUpdatableBrowser1:AccessibleName = resources:GetString("smartUpdatableBrowser1.AccessibleName":U).
        THIS-OBJECT:smartUpdatableBrowser1:Anchor = CAST(resources:GetObject("smartUpdatableBrowser1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartUpdatableBrowser1:BackgroundImage = CAST(resources:GetObject("smartUpdatableBrowser1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartUpdatableBrowser1:BackgroundImageLayout = CAST(resources:GetObject("smartUpdatableBrowser1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForAdd = resources:GetString("smartUpdatableBrowser1.DisabledColumnsForAdd":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForUpdate = resources:GetString("smartUpdatableBrowser1.DisabledColumnsForUpdate":U).
        appearance2:BackColor = System.Drawing.SystemColors:Window.
        appearance2:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance2.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Width = 216.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:Width = 264.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDownList.
        ultraGridColumn4:Width = 185.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:Header:VisiblePosition = 4.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar13[1] = ultraGridColumn1.
        arrayvar13[2] = ultraGridColumn2.
        arrayvar13[3] = ultraGridColumn3.
        arrayvar13[4] = ultraGridColumn4.
        arrayvar13[5] = ultraGridColumn5.
        ultraGridBand1:Columns:AddRange(arrayvar13).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance3:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance3:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance3:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance3:BorderColor = System.Drawing.SystemColors:Window.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance3.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance5:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance5:BackColor2 = System.Drawing.SystemColors:Control.
        appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance5.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        appearance6:BackColor = System.Drawing.SystemColors:Window.
        appearance6:ForeColor = System.Drawing.SystemColors:ControlText.
        appearance6:TextHAlignAsString = resources:GetString("appearance6.TextHAlignAsString":U).
        appearance6:TextVAlignAsString = resources:GetString("appearance6.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance6.
        appearance7:BackColor = System.Drawing.SystemColors:Highlight.
        appearance7:ForeColor = System.Drawing.SystemColors:HighlightText.
        appearance7:TextHAlignAsString = resources:GetString("appearance7.TextHAlignAsString":U).
        appearance7:TextVAlignAsString = resources:GetString("appearance7.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance7.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance8:BackColor = System.Drawing.SystemColors:Window.
        appearance8:TextHAlignAsString = resources:GetString("appearance8.TextHAlignAsString":U).
        appearance8:TextVAlignAsString = resources:GetString("appearance8.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance8.
        appearance9:BorderColor = System.Drawing.Color:Silver.
        appearance9:TextHAlignAsString = resources:GetString("appearance9.TextHAlignAsString":U).
        appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        appearance9:TextVAlignAsString = resources:GetString("appearance9.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance9.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance10:BackColor = System.Drawing.SystemColors:Control.
        appearance10:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance10:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance10:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance10:BorderColor = System.Drawing.SystemColors:Window.
        appearance10:TextHAlignAsString = resources:GetString("appearance10.TextHAlignAsString":U).
        appearance10:TextVAlignAsString = resources:GetString("appearance10.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance10.
        appearance11:TextHAlignAsString = resources:GetString("appearance11.TextHAlignAsString":U).
        appearance11:TextVAlignAsString = resources:GetString("appearance11.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance12:BackColor = System.Drawing.SystemColors:Window.
        appearance12:BorderColor = System.Drawing.Color:Silver.
        appearance12:TextHAlignAsString = resources:GetString("appearance12.TextHAlignAsString":U).
        appearance12:TextVAlignAsString = resources:GetString("appearance12.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance13:BackColor = System.Drawing.SystemColors:ControlLight.
        appearance13:TextHAlignAsString = resources:GetString("appearance13.TextHAlignAsString":U).
        appearance13:TextVAlignAsString = resources:GetString("appearance13.TextVAlignAsString":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance13.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartUpdatableBrowser1:Dock = CAST(resources:GetObject("smartUpdatableBrowser1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartUpdatableBrowser1:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:Font = CAST(resources:GetObject("smartUpdatableBrowser1.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartUpdatableBrowser1:ImeMode = CAST(resources:GetObject("smartUpdatableBrowser1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = CAST(resources:GetObject("smartUpdatableBrowser1.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartUpdatableBrowser1:MaximumSize = CAST(resources:GetObject("smartUpdatableBrowser1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:PromptForSaveValuesOnRowDeactivate = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:RightToLeft = CAST(resources:GetObject("smartUpdatableBrowser1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartUpdatableBrowser1:SecurityKey = resources:GetString("smartUpdatableBrowser1.SecurityKey":U).
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = resources:GetString("smartUpdatableBrowser1.SettingsKey":U).
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdate = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdateForced = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdateForLeftRight = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:Size = CAST(resources:GetObject("smartUpdatableBrowser1.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartUpdatableBrowser1:SmartTableIOState = resources:GetString("smartUpdatableBrowser1.SmartTableIOState":U).
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = System.Convert:ToInt32(resources:GetObject("smartUpdatableBrowser1.TabIndex":U)).
        THIS-OBJECT:smartUpdatableBrowser1:Text = resources:GetString("smartUpdatableBrowser1.Text":U).
        THIS-OBJECT:smartUpdatableBrowser1:VisibleGridColumns = resources:GetString("smartUpdatableBrowser1.VisibleGridColumns":U).
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar14 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar14.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar15[1] = NEW Progress.Data.ColumnPropDesc("ResourceKey":U, "Key":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[2] = NEW Progress.Data.ColumnPropDesc("OriginalString":U, "Original Text":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[3] = NEW Progress.Data.ColumnPropDesc("TranslatedString":U, "Translation":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[4] = NEW Progress.Data.ColumnPropDesc("TranslationType":U, "Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[5] = NEW Progress.Data.ColumnPropDesc("ResourceKeyPartial":U, "Partial Key":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar15.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:AppServerPartition = resources:GetString("smartTempTableAdapter1.AppServerPartition":U).
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceExcludeFields = resources:GetString("smartTempTableAdapter1.BindingSourceExcludeFields":U).
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceIncludeFields = resources:GetString("smartTempTableAdapter1.BindingSourceIncludeFields":U).
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:QuerySort = resources:GetString("smartTempTableAdapter1.QuerySort":U).
        THIS-OBJECT:smartTempTableAdapter1:QueryString = resources:GetString("smartTempTableAdapter1.QueryString":U).
        /*  */
        /* m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleDescription = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleDescription":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleName = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleName":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:Anchor = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImage = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImageLayout = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:ImeMode = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:Location = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:MaximumSize = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:RightToLeft = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:Size = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleDescription = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleDescription":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleName = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleName":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:Anchor = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImage = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImageLayout = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:ImeMode = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:Location = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:MaximumSize = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:RightToLeft = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:Size = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleDescription = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleDescription":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleName = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleName":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:Anchor = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImage = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImageLayout = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:ImeMode = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:Location = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:MaximumSize = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:RightToLeft = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:Size = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleDescription = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleDescription":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleName = resources:GetString("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleName":U).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:Anchor = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImage = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImageLayout = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:ImeMode = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:Location = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:MaximumSize = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:RightToLeft = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:Size = CAST(resources:GetObject("m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ResourceTranslationMaintenanceForm */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ResourceTranslationMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "ResourceTranslationMaintenanceForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ResourceTranslationMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oList             AS Infragistics.Win.ValueList NO-UNDO .
        DEFINE VARIABLE oImageProvider    AS IImageProvider             NO-UNDO .
        DEFINE VARIABLE oLanguageProvider AS ILanguageProvider          NO-UNDO .

        SUPER:OnLoad (e) .

        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["TranslationType":U]:ValueList =
            ValueLists:GetTranslationTypeValueList () .

        ASSIGN oLanguageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILanguageProvider}
               oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider}.

        oLanguageProvider:GetAvailableLanguages (OUTPUT TABLE ttLanguage) .

        FIND ttLanguage WHERE ttLanguage.LanguageKey = SessionManager:LanguageKey NO-ERROR .

        IF AVAILABLE ttLanguage THEN DO:
            ultraTextEditor1:Text = ttLanguage.LanguageName .

            IF VALID-OBJECT (oImageProvider) THEN
                ultraPictureBox1:Image = oImageProvider:ImageForKey (ttLanguage.LanguageImage) .
        END.
        ELSE DO:
            IF SessionManager:LanguageDisplayName > "":U THEN
                ultraTextEditor1:Text = SessionManager:LanguageDisplayName .
            ELSE
                ultraTextEditor1:Text = SessionManager:LanguageIsoCode .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a dialog to select a class of the given type
        Notes:
        @param pcTypeName The base class to search for
        @param pcDesktopAlertTitle The title of the UltraDesktopAlert
        @param pcDesktopAlertText The text of the UltraDesktopAlert
        @param pcDialogText The text for the dialog form to select the class name
        @return The selected class name
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER OpenClass (pcTypeName AS CHARACTER,
                                          pcDesktopAlertTitle AS CHARACTER,
                                          pcDesktopAlertText AS CHARACTER,
                                          pcDialogText AS CHARACTER):

        DEFINE VARIABLE oForm            AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult    AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo            AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO .

        DEFINE VARIABLE lLocalizableOnly AS LOGICAL                                                         NO-UNDO .

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show (pcDesktopAlertTitle,
                                                                                pcDesktopAlertText) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath (pcTypeName,
                                                                  FALSE,
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        ASSIGN lLocalizableOnly = UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1,
                                                                                 "Localizable Only":U) .

        IF lLocalizableOnly THEN
            THIS-OBJECT:FilterLocalizableControls () .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = pcDialogText .

        IF lLocalizableOnly THEN
            oForm:Remarks = "Note: Only Localizable Controls are shown."{&TRAN} .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .

        IF Progress.Util.EnumHelper:AreEqual(oDialogResult,
                                             System.Windows.Forms.DialogResult:Ok) THEN

            RETURN oForm:GetSelectedBusinessEntityName() .

        RETURN ? .

        FINALLY:
            IF VALID-OBJECT (oForm) THEN
                oForm:Dispose() .

            GarbageCollectorHelper:DeleteObject (oForm) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a Form for translation
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenForm ():

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.

        ASSIGN cClassName = THIS-OBJECT:OpenClass ("System.Windows.Forms.Form":U,
                                                   "SmartFramework"{&TRAN},
                                                   "Scanning Project Folder(s) for Forms."{&TRAN},
                                                   "Select Form Class"{&TRAN}) .

        IF Consultingwerk.CharacterType:IsNullOrEmpty (cClassName) THEN
            RETURN .

        THIS-OBJECT:OpenResourceFile (cClassName) .

        THIS-OBJECT:UpdateTitle (cClassName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the resource file for the given UI class
        Notes:
        @param pcClassName The UI class name to process the resx file for
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenResourceFile (pcClassName AS CHARACTER):

        DEFINE VARIABLE cResourceFileName AS CHARACTER NO-UNDO.

        DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.

        ASSIGN cResourceFileName = LocalizationHelper:ClassNameToResourceFileName (pcClassName) .

        FILE-INFORMATION:FILE-NAME = cResourceFileName .

        oResourceTranslator = NEW ResourceTranslator (FILE-INFORMATION:FULL-PATHNAME) .

        THIS-OBJECT:ResourceFileName = cResourceFileName .

        IF NOT oResourceTranslator:IsLocalizable THEN
            UNDO, THROW NEW AppError ("The selected class is not Localizable. Please turn on the Localizable flag in the Visual Designer before proceeding."{&TRAN}, 0) .

        ASSIGN THIS-OBJECT:ClassName = pcClassName .

        oTranslationService:GetTranslationsForObject (pcClassName,
                                                      oResourceTranslator:GetResourceStrings(),
                                                      OUTPUT TABLE eStringResources) .

        smartTempTableAdapter1:RetrieveData () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a User Control for translation
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenUserControl ():

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.

        ASSIGN cClassName = THIS-OBJECT:OpenClass ("System.Windows.Forms.UserControl":U,
                                                   "SmartFramework"{&TRAN},
                                                   "Scanning Project Folder(s) for User Controls."{&TRAN},
                                                   "Select User Control Class"{&TRAN}) .

        IF Consultingwerk.CharacterType:IsNullOrEmpty (cClassName) THEN
            RETURN .

        THIS-OBJECT:OpenResourceFile (cClassName) .

        THIS-OBJECT:UpdateTitle (cClassName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the current translations
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveTranslations ():

        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
            smartUpdatableBrowser1:SaveChanges () .

        oTranslationService:StoreTranslationsForObject (THIS-OBJECT:ClassName,
                                                        INPUT-OUTPUT TABLE eStringResources) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "OpenForm":U THEN
                THIS-OBJECT:OpenForm () .

            WHEN "OpenUserControl":U THEN
                THIS-OBJECT:OpenUserControl () .

            WHEN "SaveTranslations":U THEN
                THIS-OBJECT:SaveTranslations () .

        END CASE.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the title of the Form
        Notes:
        @param pcClassName The class name to show in the title
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateTitle (pcClassName AS CHARACTER):

        IF THIS-OBJECT:ResourceFileName > "":U THEN
            ASSIGN THIS-OBJECT:Text = SUBSTITUTE ("&1 <&2>":U,
                                                  cBaseTitle,
                                                  pcClassName) .

        ELSE
            ASSIGN THIS-OBJECT:Text = cBaseTitle .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ResourceTranslationMaintenanceForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ResourceTranslationMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
