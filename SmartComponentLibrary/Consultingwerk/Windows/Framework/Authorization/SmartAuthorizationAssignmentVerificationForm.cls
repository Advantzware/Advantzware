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
    File        : SmartAuthorizationAssignmentVerificationForm
    Purpose     :
    Syntax      :
    Description : Form that allows to verify security assignment
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 12 23:42:34 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                       FROM PROPATH .
USING Consultingwerk.Framework.Session.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*    FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.* FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*              FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

&SCOPED-DEFINE ToolKey "Verify Security Assignment":U

CLASS Consultingwerk.Windows.Framework.Authorization.SmartAuthorizationAssignmentVerificationForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE cbRestricted AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE loSecurityItem AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityAssignment_SecurityRealmGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE coSecurityRealm AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE panel2 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE pbRestricted AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE pbNotRestricted AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE pbUnknown AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartAuthorizationAssignmentVerificationForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    {Consultingwerk/SmartFramework/Authorization/ttUserGoups.i}

    DEFINE DATASET dsUserGroups FOR ttUserGroups .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartAuthorizationAssignmentVerificationForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartAuthorizationAssignmentVerificationForm ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttUserGroups:HANDLE .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the properties of the Lookup Control based on the selected
                 security realm
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AssignLookupProperties ():

        DEFINE VARIABLE oParameter AS SecurityRealmLookupParameter NO-UNDO .

        ASSIGN oParameter                   = NEW SecurityRealmLookupParameter ()
               oParameter:SecurityRealmGuid = UNBOX (coSecurityRealm:Value) .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService":U,
                                                     "GetSecurityRealmLookupParameter":U,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        oParameter:LookupDescriptor:AssignToSmartBusinessEntityLookup (loSecurityItem) .

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
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Authorization Assignment":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Verify Security Assignment":U).
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
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Verify Security Assignment":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartGroup":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Order":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ParentGroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ParentGroupName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyName":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartAuthorizationAssignmentVerificationForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:panel2 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:pbUnknown = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:cbRestricted = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:pbRestricted = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:pbNotRestricted = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:loSecurityItem = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:coSecurityRealm = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:panel2:SuspendLayout().
        CAST(THIS-OBJECT:pbUnknown, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbRestricted, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pbRestricted, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pbNotRestricted, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:loSecurityItem, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coSecurityRealm, System.ComponentModel.ISupportInitialize):BeginInit().
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
        resources:ApplyResources(ribbonGroup3, "ribbonGroup3":U).
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar4[1] = buttonTool18.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        arrayvar5[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar5).
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
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool23.
        arrayvar8[2] = buttonTool24.
        arrayvar8[3] = buttonTool25.
        arrayvar8[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar8).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
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
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        resources:ApplyResources(buttonTool35:SharedPropsInternal, "buttonTool35.SharedPropsInternal":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:ForceApplyResources = "SharedPropsInternal":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        resources:ApplyResources(buttonTool36:SharedPropsInternal, "buttonTool36.SharedPropsInternal":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:ForceApplyResources = "SharedPropsInternal":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        resources:ApplyResources(buttonTool37:SharedPropsInternal, "buttonTool37.SharedPropsInternal":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:ForceApplyResources = "SharedPropsInternal":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        resources:ApplyResources(buttonTool38:SharedPropsInternal, "buttonTool38.SharedPropsInternal":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:ForceApplyResources = "SharedPropsInternal":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        resources:ApplyResources(buttonTool39:SharedPropsInternal, "buttonTool39.SharedPropsInternal":U).
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:ForceApplyResources = "SharedPropsInternal":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        resources:ApplyResources(buttonTool40:SharedPropsInternal, "buttonTool40.SharedPropsInternal":U).
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:ForceApplyResources = "SharedPropsInternal":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        resources:ApplyResources(buttonTool41:SharedPropsInternal, "buttonTool41.SharedPropsInternal":U).
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:ForceApplyResources = "SharedPropsInternal":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        resources:ApplyResources(buttonTool42:SharedPropsInternal, "buttonTool42.SharedPropsInternal":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:ForceApplyResources = "SharedPropsInternal":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        resources:ApplyResources(buttonTool43:SharedPropsInternal, "buttonTool43.SharedPropsInternal":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:ForceApplyResources = "SharedPropsInternal":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        resources:ApplyResources(buttonTool44:SharedPropsInternal, "buttonTool44.SharedPropsInternal":U).
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:ForceApplyResources = "SharedPropsInternal":U.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        resources:ApplyResources(buttonTool45:SharedPropsInternal, "buttonTool45.SharedPropsInternal":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool45:ForceApplyResources = "SharedPropsInternal":U.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        resources:ApplyResources(buttonTool19:SharedPropsInternal, "buttonTool19.SharedPropsInternal":U).
        buttonTool19:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 12 NO-UNDO.
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
        arrayvar11[12] = buttonTool19.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* SmartAuthorizationAssignmentVerificationForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartAuthorizationAssignmentVerificationForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:panel2).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel, "SmartAuthorizationAssignmentVerificationForm_Fill_Panel":U).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:Name = "SmartAuthorizationAssignmentVerificationForm_Fill_Panel":U.
        /*  */
        /* panel2 */
        /*  */
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:pbUnknown).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:cbRestricted).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:pbRestricted).
        THIS-OBJECT:panel2:Controls:Add(THIS-OBJECT:pbNotRestricted).
        resources:ApplyResources(THIS-OBJECT:panel2, "panel2":U).
        THIS-OBJECT:panel2:Name = "panel2":U.
        /*  */
        /* pbUnknown */
        /*  */
        resources:ApplyResources(THIS-OBJECT:pbUnknown, "pbUnknown":U).
        THIS-OBJECT:pbUnknown:Name = "pbUnknown":U.
        THIS-OBJECT:pbUnknown:TabStop = FALSE.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser1, "smartDataBrowser1":U).
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:Hidden = TRUE.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:Hidden = TRUE.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn6:Header:VisiblePosition = 5.
        ultraGridColumn6:Hidden = TRUE.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:Header:VisiblePosition = 6.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 7 NO-UNDO.
        arrayvar12[1] = ultraGridColumn1.
        arrayvar12[2] = ultraGridColumn2.
        arrayvar12[3] = ultraGridColumn3.
        arrayvar12[4] = ultraGridColumn4.
        arrayvar12[5] = ultraGridColumn5.
        arrayvar12[6] = ultraGridColumn6.
        arrayvar12[7] = ultraGridColumn7.
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
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar13 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar13.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar14[1] = NEW Progress.Data.ColumnPropDesc("Order":U, "Order":U, Progress.Data.DataType:INTEGER).
        arrayvar14[2] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar14[3] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "Group Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[4] = NEW Progress.Data.ColumnPropDesc("ParentGroupGuid":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar14[5] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, ?, Progress.Data.DataType:CHARACTER).
        arrayvar14[6] = NEW Progress.Data.ColumnPropDesc("ParentGroupName":U, "Parent Group Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[7] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "Login Company":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar14.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = ?.
        /*  */
        /* ultraLabel4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel4, "ultraLabel4":U).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraTextEditor2 */
        /*  */
        THIS-OBJECT:ultraTextEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor2, "ultraTextEditor2":U).
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2":U.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        /*  */
        /* cbRestricted */
        /*  */
        resources:ApplyResources(THIS-OBJECT:cbRestricted, "cbRestricted":U).
        THIS-OBJECT:cbRestricted:Name = "cbRestricted":U.
        THIS-OBJECT:cbRestricted:ThreeState = TRUE.
        /*  */
        /* pbRestricted */
        /*  */
        THIS-OBJECT:pbRestricted:BackColor = System.Drawing.Color:Transparent.
        resources:ApplyResources(THIS-OBJECT:pbRestricted, "pbRestricted":U).
        THIS-OBJECT:pbRestricted:Name = "pbRestricted":U.
        THIS-OBJECT:pbRestricted:TabStop = FALSE.
        /*  */
        /* pbNotRestricted */
        /*  */
        THIS-OBJECT:pbNotRestricted:BackColor = System.Drawing.Color:Transparent.
        resources:ApplyResources(THIS-OBJECT:pbNotRestricted, "pbNotRestricted":U).
        THIS-OBJECT:pbNotRestricted:Name = "pbNotRestricted":U.
        THIS-OBJECT:pbNotRestricted:TabStop = FALSE.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 265.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:loSecurityItem).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:coSecurityRealm).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label).
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:Name = "panel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueChanged:Subscribe(THIS-OBJECT:smartBusinessEntityLookup1_LookupKeyValueChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* loSecurityItem */
        /*  */
        resources:ApplyResources(THIS-OBJECT:loSecurityItem, "loSecurityItem":U).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:loSecurityItem:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:loSecurityItem:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:loSecurityItem:Name = "loSecurityItem":U.
        THIS-OBJECT:loSecurityItem:OwningGrid = ?.
        THIS-OBJECT:loSecurityItem:LookupKeyValueChanged:Subscribe(THIS-OBJECT:loSecurityItem_LookupKeyValueChanged).
        THIS-OBJECT:loSecurityItem:BeforePerformLookup:Subscribe(THIS-OBJECT:loSecurityItem_BeforePerformLookup).
        THIS-OBJECT:loSecurityItem:ShowLookupDialog:Subscribe(THIS-OBJECT:loSecurityItem_ShowLookupDialog).
        /*  */
        /* coSecurityRealm */
        /*  */
        resources:ApplyResources(THIS-OBJECT:coSecurityRealm, "coSecurityRealm":U).
        THIS-OBJECT:coSecurityRealm:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coSecurityRealm:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coSecurityRealm:LimitToList = TRUE.
        THIS-OBJECT:coSecurityRealm:Name = "coSecurityRealm":U.
        /*  */
        /* eSmartSecurityAssignment_SecurityRealmGuid_Label */
        /*  */
        appearance13:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Appearance = appearance13.
        resources:ApplyResources(THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label, "eSmartSecurityAssignment_SecurityRealmGuid_Label":U).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Name = "eSmartSecurityAssignment_SecurityRealmGuid_Label":U.
        /*  */
        /* m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left, "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:Name = "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right, "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:Name = "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top, "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:Name = "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom, "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartAuthorizationAssignmentVerificationForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartAuthorizationAssignmentVerificationForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartAuthorizationAssignmentVerificationForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartAuthorizationAssignmentVerificationForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:panel2:ResumeLayout(FALSE).
        THIS-OBJECT:panel2:PerformLayout().
        CAST(THIS-OBJECT:pbUnknown, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbRestricted, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pbRestricted, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pbNotRestricted, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:loSecurityItem, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coSecurityRealm, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforePerformLookup event of the loSecurityItem
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventHandler with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID loSecurityItem_BeforePerformLookup (sender AS System.Object,
                                                            e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (coSecurityRealm:SelectedItem) THEN DO:
            MessageFormHelper:ShowMessage ("Please select a security realm first."{&TRAN},
                                           THIS-OBJECT:FindForm ():Text,
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            e:Cancel = TRUE .
            RETURN .
        END .

        THIS-OBJECT:AssignLookupProperties () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LookupKeyValueChanged event of the loSecurityItem
        Notes:   Conditionally enables the verification button
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID loSecurityItem_LookupKeyValueChanged (sender AS System.Object, e AS System.EventArgs):

        smartToolbarController1:Tools[{&ToolKey}]:SharedProps:Enabled =

                (smartBusinessEntityLookup1:LookupKeyValue > "":U AND
                 loSecurityItem:LookupKeyValue             > "":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ShowLookupDialog event of the loSecurityItem
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventHandler with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID loSecurityItem_ShowLookupDialog (sender AS System.Object,
                                                         e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (coSecurityRealm:SelectedItem) THEN DO:
            MessageFormHelper:ShowMessage ("Please select a security realm first."{&TRAN},
                                           THIS-OBJECT:FindForm ():Text,
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            e:Cancel = TRUE .
            RETURN .
        END .

        THIS-OBJECT:AssignLookupProperties () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        smartToolbarController1:Tools[{&ToolKey}]:SharedProps:Enabled = FALSE .

        IF SessionManager:LoginCompanyKey > "":U THEN DO:

            ASSIGN smartBusinessEntityLookup1:LookupQueryString = SUBSTITUTE
                ("for each eSmartUser where eSmartUser.UserName begins ~"&&1~" and eSmartUser.LoginCompanyGuid = &1 ~
                                         or eSmartUser.UserName begins ~"&&1~" and eSmartUser.LoginCompanyGuid = &2":U,
                  QUOTER (SessionManager:LoginCompanyKey),
                  QUOTER ("":U)) .

            ASSIGN smartBusinessEntityLookup1:LookupDialogQueryString = SUBSTITUTE
                ("for each eSmartUser where eSmartUser.LoginCompanyGuid = &1 ~
                                         or eSmartUser.LoginCompanyGuid = &2":U,
                  QUOTER (SessionManager:LoginCompanyKey),
                  QUOTER ("":U)) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs an Authorization check at the backend
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformIsRestrictedCheck ():

        DEFINE VARIABLE oParameter AS IsRestrictedCheckParameter NO-UNDO .

        oParameter = NEW IsRestrictedCheckParameter () .

        ASSIGN oParameter:RealmCode        = (NEW SecurityRealmDatasetModel (UNBOX (THIS-OBJECT:coSecurityRealm:SelectedItem:DataValue))):SmartSecurityRealm:SecurityRealmCode
               oParameter:SecurityItemGuid = THIS-OBJECT:loSecurityItem:LookupKeyValue
               oParameter:UserGuid         = THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue
               oParameter:LoginCompanyGuid = SessionManager:LoginCompanyKey
               oParameter:ReturnGroups     = TRUE .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "IsRestrictedCheck":U,
                                                     INPUT-OUTPUT DATASET dsUserGroups,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        THIS-OBJECT:cbRestricted:Checked  = oParameter:IsRestricted .

        IF oParameter:Reason BEGINS "Group:":U THEN DO:
            FIND ttUserGroups WHERE
                ttUserGroups.GroupGuid = ENTRY (2, oParameter:Reason, ":":U) NO-ERROR .

            IF AVAILABLE ttUserGroups THEN
                THIS-OBJECT:ultraTextEditor1:Text = SUBSTITUTE ("User group: &1"{&TRAN}, ttUserGroups.GroupName) .
            ELSE
                THIS-OBJECT:ultraTextEditor1:Text = oParameter:Reason .

            ErrorHelper:ResetErrorStatus() .
        END.
        ELSE
            THIS-OBJECT:ultraTextEditor1:Text = oParameter:Reason .

        THIS-OBJECT:ultraTextEditor2:Text = SUBSTITUTE ("&1 msec"{&TRAN}, oParameter:Runtime) .

        smartTempTableAdapter1:RetrieveData() .

        CASE oParameter:IsRestricted:
            WHEN TRUE THEN
                ASSIGN THIS-OBJECT:pbRestricted:Visible    = TRUE
                       THIS-OBJECT:pbNotRestricted:Visible = FALSE
                       THIS-OBJECT:pbUnknown:Visible       = FALSE .
            WHEN FALSE THEN
                ASSIGN THIS-OBJECT:pbRestricted:Visible    = FALSE
                       THIS-OBJECT:pbNotRestricted:Visible = TRUE
                       THIS-OBJECT:pbUnknown:Visible       = FALSE .
            OTHERWISE DO:
                ASSIGN THIS-OBJECT:pbRestricted:Visible    = FALSE
                       THIS-OBJECT:pbNotRestricted:Visible = FALSE
                       THIS-OBJECT:pbUnknown:Visible       = TRUE .

                MessageFormHelper:ShowMessage ("Is Restricted returned the unknown value!"{&TRAN},
                                               THIS-OBJECT:Text,
                                               MessageFormImages:ImageWarning) .
            END.

        END CASE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LookupKeyValueChanged event of the smartBusinessEntityLookup1
        Notes:   Conditionally enables the verification button
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityLookup1_LookupKeyValueChanged (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        smartToolbarController1:Tools[{&ToolKey}]:SharedProps:Enabled =

                (smartBusinessEntityLookup1:LookupKeyValue > "":U AND
                 loSecurityItem:LookupKeyValue             > "":U) .

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
            WHEN {&ToolKey} THEN
                THIS-OBJECT:PerformIsRestrictedCheck () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartAuthorizationAssignmentVerificationForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartAuthorizationAssignmentVerificationForm ():

    END DESTRUCTOR .

END CLASS.
