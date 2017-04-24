/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CustomPropertiesEditorForm
    Purpose     : Supports editing custom properties for the Business Entity,
                  Temp-Tables, Data-Relations, Fields and Indexes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 19 17:03:24 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*            FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.CustomPropertiesEditorForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE CustomPropertiesEditorForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    {Consultingwerk/BusinessEntityDesigner/Services/ttCustomProperties.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the Scope of the Editor
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Scope AS CustomPropertiesScopeEnum NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomPropertiesEditorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomPropertiesEditorForm ():

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PropertyName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PropertyValue":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("Table":U).
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
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.CustomPropertiesEditorForm":U).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* CustomPropertiesEditorForm_Fill_Panel */
        /*  */
        /*  */
        /* CustomPropertiesEditorForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 24).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:Name = "CustomPropertiesEditorForm_Fill_Panel":U.
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:Size = NEW System.Drawing.Size(625, 359).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(538, 324).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 2.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(457, 324).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 1.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartUpdatableBrowser1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Width = 314.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Width = 309.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 2 NO-UNDO.
        arrayvar0[1] = ultraGridColumn1.
        arrayvar0[2] = ultraGridColumn2.
        ultraGridBand1:Columns:AddRange(arrayvar0).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "8aae5563-e584-cfb0-e213-a5f0a4a81913":U.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(625, 318).
        THIS-OBJECT:smartUpdatableBrowser1:SmartTableIOState = "FieldsDisabled":U.
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 0.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:BeforeCellActivate:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_BeforeCellActivate).
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("PropertyName":U, "Property Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("PropertyValue":U, "Value":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar2.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar3).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar4[1] = buttonTool2.
        arrayvar4[2] = buttonTool3.
        arrayvar4[3] = buttonTool4.
        arrayvar4[4] = buttonTool5.
        arrayvar4[5] = buttonTool6.
        arrayvar4[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar4).
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
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar5[1] = buttonTool8.
        arrayvar5[2] = buttonTool9.
        arrayvar5[3] = buttonTool10.
        arrayvar5[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar5).
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
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar6[1] = buttonTool12.
        arrayvar6[2] = buttonTool13.
        arrayvar6[3] = buttonTool14.
        arrayvar6[4] = buttonTool15.
        arrayvar6[5] = buttonTool16.
        arrayvar6[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar6).
        ribbonGroup3:Caption = "Transaction":U.
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar7[1] = buttonTool18.
        arrayvar7[2] = buttonTool19.
        ribbonGroup3:Tools:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar8[1] = ribbonGroup1.
        arrayvar8[2] = ribbonGroup2.
        arrayvar8[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar8).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar9[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar9).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar10[1] = buttonTool20.
        arrayvar10[2] = buttonTool21.
        arrayvar10[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar10).
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar11[1] = buttonTool23.
        arrayvar11[2] = buttonTool24.
        arrayvar11[3] = buttonTool25.
        arrayvar11[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar11).
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 1.
        ultraToolbar2:DockedRow = 0.
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar12[1] = buttonTool27.
        arrayvar12[2] = buttonTool28.
        arrayvar12[3] = buttonTool29.
        arrayvar12[4] = buttonTool30.
        arrayvar12[5] = buttonTool31.
        arrayvar12[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar12).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 2 NO-UNDO.
        arrayvar13[1] = ultraToolbar1.
        arrayvar13[2] = ultraToolbar2.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar13).
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool35:SharedPropsInternal:Caption = "First record":U.
        buttonTool35:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = "First record":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool36:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool36:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool37:SharedPropsInternal:Caption = "Next record":U.
        buttonTool37:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool38:SharedPropsInternal:Caption = "Last record":U.
        buttonTool38:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool39:SharedPropsInternal:Caption = "Add record":U.
        buttonTool39:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool39:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool40:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool40:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool40:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool41:SharedPropsInternal:Caption = "Update record":U.
        buttonTool41:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool41:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool42:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool42:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool43:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool43:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool44:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool44:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool44:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool45:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance24.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool46:SharedPropsInternal:Caption = "Commit":U.
        buttonTool46:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool47:SharedPropsInternal:Caption = "Revert":U.
        buttonTool47:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar14[1] = buttonTool35.
        arrayvar14[2] = buttonTool36.
        arrayvar14[3] = buttonTool37.
        arrayvar14[4] = buttonTool38.
        arrayvar14[5] = buttonTool39.
        arrayvar14[6] = buttonTool40.
        arrayvar14[7] = buttonTool41.
        arrayvar14[8] = buttonTool42.
        arrayvar14[9] = buttonTool43.
        arrayvar14[10] = buttonTool44.
        arrayvar14[11] = buttonTool45.
        arrayvar14[12] = buttonTool46.
        arrayvar14[13] = buttonTool47.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar14).
        /*  */
        /* m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 24).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:Name = "m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 359).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(625, 24).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:Name = "m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 359).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:Name = "m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(625, 24).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 383).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:Name = "m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(625, 0).
        THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* CustomPropertiesEditorForm */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(625, 383).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomPropertiesEditorForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "CustomPropertiesEditorForm":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "Custom Properties Editor":U.
        THIS-OBJECT:WindowPositionRegistryKey = "CustomPropertiesEditorForm":U.
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:CustomPropertiesEditorForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
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

        DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
        DEFINE VARIABLE oList       AS CharacterList                   NO-UNDO .
        DEFINE VARIABLE oValueList  AS Infragistics.Win.ValueList      NO-UNDO .

        SUPER:OnLoad (e) .

        oController = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController} .

        oList = oController:GetCustomPropertyNames (THIS-OBJECT:Scope) .

        IF oList:Count > 0 THEN DO:
            oValueList = ValueListHelper:FromList (oList) .

            smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["PropertyName":U]:ValueList = oValueList .
        END .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttCustomProperties:HANDLE .
        smartTempTableAdapter1:RetrieveData () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeCellActivate event of the smartUpdatableBrowser1
        Notes:   Populates the value list of possible values based on the property
        @param sender The reference to the object that raised the event
        @param e The CancelableCellEventArgs with the data for this object
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartUpdatableBrowser1_BeforeCellActivate (sender AS System.Object,
                                                                   e AS Infragistics.Win.UltraWinGrid.CancelableCellEventArgs):

        DEFINE VARIABLE cPropertyName AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE oController   AS BusinessEntityDatasetController NO-UNDO .
        DEFINE VARIABLE oList         AS CharacterList                   NO-UNDO .
        DEFINE VARIABLE oValueList    AS Infragistics.Win.ValueList      NO-UNDO .

        IF e:Cell:Column:Key <> "PropertyValue":U THEN
            RETURN .

        ASSIGN e:Cell:ValueList = ?
               cPropertyName    = UNBOX (e:Cell:Row:Cells ["PropertyName":U]:Value) .

        IF cPropertyName > "":U THEN DO:
            oController = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController} .

            oList = oController:GetCustomPropertyValues (THIS-OBJECT:Scope, cPropertyName) .

            IF oList:Count > 0 THEN DO:
                oValueList = ValueListHelper:FromList (oList) .

                e:Cell:ValueList = oValueList .
            END .
        END .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1 (OK)
        Notes:   When user clicks OK, save pending changes without prompting
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:

            smartUpdatableBrowser1:SaveChanges () .

            IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
                RETURN .
        END.

        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK .
        THIS-OBJECT:Close () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Visualizes the Form for Update and blocks the UI
        Notes:
        @param poScope The Custom properties Scope
        @param ttCustomProperties The temp-table with the custom properties to edit
        @param plOk OUTPUT Logical value indicating if the user has pressed OK or Cancel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateCustomProperties (poScope AS CustomPropertiesScopeEnum,
                                               INPUT-OUTPUT TABLE FOR ttCustomProperties,
                                               OUTPUT plOk AS LOGICAL):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        ASSIGN plOk              = FALSE
               THIS-OBJECT:Scope = poScope.

        WAIT-FOR THIS-OBJECT:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        ASSIGN plOk = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CustomPropertiesEditorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CustomPropertiesEditorForm ():

    END DESTRUCTOR .

END CLASS.
