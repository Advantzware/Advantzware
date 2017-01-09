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
    File        : FieldEditorForm
    Purpose     : Form that enables the user to maintain the fields of a
                  Business Entity Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 27 21:47:52 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.UI.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.FieldEditorForm INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE m_FieldEditorForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_FieldEditorForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_FieldEditorForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_FieldEditorForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE FieldEditorForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.

    {Consultingwerk/BusinessEntityDesigner/Services/eField.i &PREFIX=p_ &REFERENCE-ONLY=REFERENCE-ONLY &NO-BEFORE}

    DEFINE TEMP-TABLE eField NO-UNDO REFERENCE-ONLY
        LIKE p_eField
        FIELD OriginalFieldName AS CHARACTER .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldEditorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldEditorForm ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.":U &THEN
        smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 .
        smartToolbarController1:Ribbon:ApplicationMenuButtonImage = ? .
        &ENDIF

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeAssignRecord event of the smartTempTableAdapter1
                 instance
        Notes:   Validates uniqueness of the field name
        @param sender The reference to the object that raised the event
        @param e The BeforeAssignRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BeforeAssignRecordHandler (sender AS System.Object,
                                                   e AS BeforeAssignRecordEventArgs) .

        DEFINE BUFFER b_eField FOR eField .

        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.

        ASSIGN cFieldName =  UNBOX (smartTempTableAdapter1:BindingSource:InputValue ["FieldName":U]) .

        FOR EACH b_eField WHERE b_eField.FieldName = cFieldName
            ON ERROR UNDO, THROW:

            IF ROWID (b_eField) <> smartTempTableAdapter1:BufferHandle:ROWID THEN
                UNDO, THROW NEW AppError ("Field Name must be unique.":U, 0) .

        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a unqiue field name in a temp-table
        Notes:
        @param pcFieldName The base name of the Field
        @return The Unique field name
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER EnsureUniqueFieldName (pcFieldName AS CHARACTER):

        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO INIT 2.

        DEFINE BUFFER eField FOR eField .

        IF NOT CAN-FIND (eField WHERE eField.FieldName = pcFieldName) THEN
            RETURN pcFieldName .

        REPEAT:
            ASSIGN cFieldName = SUBSTITUTE ("&1&2":U,
                                            pcFieldName,
                                            i) .

            IF NOT CAN-FIND (eField WHERE eField.FieldName = cFieldName) THEN
                RETURN cFieldName .

            i = i + 1 .
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
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OK":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
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
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
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
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
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
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
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
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OK":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Cancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eField":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BusinessEntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TempTableName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldOrder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldDataType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldInitial":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldLabel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldFormat":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSerializeHidden":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSerializeName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldXmlDataType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldXmlNodeType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldXmlNodeName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSource":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldExtent":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldGetAccess":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSetAccess":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldCalcExpression":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldCaseSensitive":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldCustomControlName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldInitialUnknownValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldValidationInstance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSemanticType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OriginalFieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn26 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn26 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldSkipAssignment":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("eField":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.FieldEditorForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:FieldEditorForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:FieldEditorForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:FieldEditorForm_Fill_Panel:SuspendLayout().
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
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar1[1] = buttonTool3.
        arrayvar1[2] = buttonTool4.
        arrayvar1[3] = buttonTool5.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Fields":U.
        buttonTool48:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool49:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = buttonTool48.
        arrayvar2[2] = buttonTool49.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Navigation":U.
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
        ribbonGroup3:Caption = "Maintenance":U.
        buttonTool16:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool17:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool15:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool12:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar4[1] = buttonTool16.
        arrayvar4[2] = buttonTool17.
        arrayvar4[3] = buttonTool13.
        arrayvar4[4] = buttonTool14.
        arrayvar4[5] = buttonTool15.
        arrayvar4[6] = buttonTool12.
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
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
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
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar9[1] = buttonTool29.
        arrayvar9[2] = buttonTool30.
        arrayvar9[3] = buttonTool31.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar10[1] = buttonTool33.
        arrayvar10[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar3:Text = "Transaction Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar11[1] = ultraToolbar1.
        arrayvar11[2] = ultraToolbar2.
        arrayvar11[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar11).
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
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool41:SharedPropsInternal:Caption = "Update record":U.
        buttonTool41:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool41:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool42:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool42:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool43:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool43:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool45:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance21.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool46:SharedPropsInternal:Caption = "Commit":U.
        buttonTool46:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance23.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool47:SharedPropsInternal:Caption = "Revert":U.
        buttonTool47:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool50:SharedPropsInternal:Caption = "OK":U.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesLarge:Appearance = appearance27.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesSmall:Appearance = appearance28.
        buttonTool51:SharedPropsInternal:Caption = "Cancel":U.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool2:SharedPropsInternal:Caption = "Add record":U.
        buttonTool2:SharedPropsInternal:CustomizerCaption = "Add":U.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance31.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        buttonTool6:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool6:SharedPropsInternal:CustomizerCaption = "Copy":U.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesLarge:Appearance = appearance33.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        buttonTool7:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool7:SharedPropsInternal:CustomizerCaption = "Delete":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 15 NO-UNDO.
        arrayvar12[1] = buttonTool35.
        arrayvar12[2] = buttonTool36.
        arrayvar12[3] = buttonTool37.
        arrayvar12[4] = buttonTool38.
        arrayvar12[5] = buttonTool41.
        arrayvar12[6] = buttonTool42.
        arrayvar12[7] = buttonTool43.
        arrayvar12[8] = buttonTool45.
        arrayvar12[9] = buttonTool46.
        arrayvar12[10] = buttonTool47.
        arrayvar12[11] = buttonTool50.
        arrayvar12[12] = buttonTool51.
        arrayvar12[13] = buttonTool2.
        arrayvar12[14] = buttonTool6.
        arrayvar12[15] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar12).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* FieldEditorForm_Fill_Panel */
        /*  */
        /*  */
        /* FieldEditorForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:FieldEditorForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:FieldEditorForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:FieldEditorForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:FieldEditorForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 164).
        THIS-OBJECT:FieldEditorForm_Fill_Panel:Name = "FieldEditorForm_Fill_Panel":U.
        THIS-OBJECT:FieldEditorForm_Fill_Panel:Size = NEW System.Drawing.Size(710, 298).
        THIS-OBJECT:FieldEditorForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForAdd = "FieldOrder":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForUpdate = "FieldOrder":U.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:Caption = " ":U.
        ultraGridColumn1:Header:VisiblePosition = 1.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:Caption = " ":U.
        ultraGridColumn2:Header:VisiblePosition = 2.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:Caption = "Name":U.
        ultraGridColumn3:Header:VisiblePosition = 3.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:False.
        ultraGridColumn4:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn4:Header:Caption = "Order":U.
        ultraGridColumn4:Header:VisiblePosition = 0.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:Header:Caption = "Data Type":U.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:Header:Caption = "Description":U.
        ultraGridColumn6:Header:VisiblePosition = 5.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:Header:Caption = "Initial":U.
        ultraGridColumn7:Header:VisiblePosition = 6.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:Header:Caption = "Label":U.
        ultraGridColumn8:Header:VisiblePosition = 7.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn9:Header:Caption = "Format":U.
        ultraGridColumn9:Header:VisiblePosition = 8.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn10:Header:Caption = "Serialize Hidden":U.
        ultraGridColumn10:Header:VisiblePosition = 9.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn11:Header:Caption = "Serialize Name":U.
        ultraGridColumn11:Header:VisiblePosition = 10.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:Header:Caption = "XML Data Type":U.
        ultraGridColumn12:Header:VisiblePosition = 11.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn13:Header:Caption = "XML Node Type":U.
        ultraGridColumn13:Header:VisiblePosition = 12.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn14:Header:Caption = "XML Node Name":U.
        ultraGridColumn14:Header:VisiblePosition = 13.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn15:Header:Caption = "Source":U.
        ultraGridColumn15:Header:VisiblePosition = 14.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn16:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn16:Header:Caption = "Extent":U.
        ultraGridColumn16:Header:VisiblePosition = 15.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:Header:Caption = "GET Access":U.
        ultraGridColumn17:Header:VisiblePosition = 16.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn18:Header:Caption = "SET Access":U.
        ultraGridColumn18:Header:VisiblePosition = 17.
        ultraGridColumn19:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn19:Header:Caption = "Calc Expression ":U.
        ultraGridColumn19:Header:VisiblePosition = 18.
        ultraGridColumn20:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn20:Header:Caption = "Case Sensitive ":U.
        ultraGridColumn20:Header:VisiblePosition = 19.
        ultraGridColumn21:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn21:Header:Caption = "Custom Control Name ":U.
        ultraGridColumn21:Header:VisiblePosition = 20.
        ultraGridColumn22:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn22:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn22:Header:Caption = "Initial ? ":U.
        ultraGridColumn22:Header:VisiblePosition = 21.
        ultraGridColumn22:Hidden = TRUE.
        ultraGridColumn23:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn23:Header:Caption = "Validation Instance ":U.
        ultraGridColumn23:Header:VisiblePosition = 22.
        ultraGridColumn23:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn24:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn24:Header:Caption = "Semantic Type ":U.
        ultraGridColumn24:Header:VisiblePosition = 23.
        ultraGridColumn25:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn25:Header:Caption = " ":U.
        ultraGridColumn25:Header:VisiblePosition = 24.
        ultraGridColumn26:Header:VisiblePosition = 25.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS System.Object EXTENT 26 NO-UNDO.
        arrayvar13[1] = ultraGridColumn1.
        arrayvar13[2] = ultraGridColumn2.
        arrayvar13[3] = ultraGridColumn3.
        arrayvar13[4] = ultraGridColumn4.
        arrayvar13[5] = ultraGridColumn5.
        arrayvar13[6] = ultraGridColumn6.
        arrayvar13[7] = ultraGridColumn7.
        arrayvar13[8] = ultraGridColumn8.
        arrayvar13[9] = ultraGridColumn9.
        arrayvar13[10] = ultraGridColumn10.
        arrayvar13[11] = ultraGridColumn11.
        arrayvar13[12] = ultraGridColumn12.
        arrayvar13[13] = ultraGridColumn13.
        arrayvar13[14] = ultraGridColumn14.
        arrayvar13[15] = ultraGridColumn15.
        arrayvar13[16] = ultraGridColumn16.
        arrayvar13[17] = ultraGridColumn17.
        arrayvar13[18] = ultraGridColumn18.
        arrayvar13[19] = ultraGridColumn19.
        arrayvar13[20] = ultraGridColumn20.
        arrayvar13[21] = ultraGridColumn21.
        arrayvar13[22] = ultraGridColumn22.
        arrayvar13[23] = ultraGridColumn23.
        arrayvar13[24] = ultraGridColumn24.
        arrayvar13[25] = ultraGridColumn25.
        arrayvar13[26] = ultraGridColumn26.
        ultraGridBand1:Columns:AddRange(arrayvar13).
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
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartUpdatableBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:PromptForSaveValuesOnRowDeactivate = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "d9651234-1cf0-e3b1-da13-fe2c88e1c814":U.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdate = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(710, 298).
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 5.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:AfterCopyRecord:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_AfterCopyRecord).
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
        DEFINE VARIABLE arrayvar15 AS Progress.Data.ColumnPropDesc EXTENT 26 NO-UNDO.
        arrayvar15[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[2] = NEW Progress.Data.ColumnPropDesc("TempTableName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[3] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[4] = NEW Progress.Data.ColumnPropDesc("FieldOrder":U, "":U, Progress.Data.DataType:INTEGER).
        arrayvar15[5] = NEW Progress.Data.ColumnPropDesc("FieldDataType":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[6] = NEW Progress.Data.ColumnPropDesc("FieldDescription":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[7] = NEW Progress.Data.ColumnPropDesc("FieldInitial":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[8] = NEW Progress.Data.ColumnPropDesc("FieldLabel":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[9] = NEW Progress.Data.ColumnPropDesc("FieldFormat":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[10] = NEW Progress.Data.ColumnPropDesc("FieldSerializeHidden":U, "":U, Progress.Data.DataType:LOGICAL).
        arrayvar15[11] = NEW Progress.Data.ColumnPropDesc("FieldSerializeName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[12] = NEW Progress.Data.ColumnPropDesc("FieldXmlDataType":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[13] = NEW Progress.Data.ColumnPropDesc("FieldXmlNodeType":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[14] = NEW Progress.Data.ColumnPropDesc("FieldXmlNodeName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[15] = NEW Progress.Data.ColumnPropDesc("FieldSource":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[16] = NEW Progress.Data.ColumnPropDesc("FieldExtent":U, "":U, Progress.Data.DataType:INTEGER).
        arrayvar15[17] = NEW Progress.Data.ColumnPropDesc("FieldGetAccess":U, "&Get Access":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[18] = NEW Progress.Data.ColumnPropDesc("FieldSetAccess":U, "&Set Access":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[19] = NEW Progress.Data.ColumnPropDesc("FieldCalcExpression":U, "Calc Expression":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[20] = NEW Progress.Data.ColumnPropDesc("FieldCaseSensitive":U, "Case Sensitive":U, Progress.Data.DataType:LOGICAL).
        arrayvar15[21] = NEW Progress.Data.ColumnPropDesc("FieldCustomControlName":U, "Custom Control Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[22] = NEW Progress.Data.ColumnPropDesc("FieldInitialUnknownValue":U, "Initial ?":U, Progress.Data.DataType:LOGICAL).
        arrayvar15[23] = NEW Progress.Data.ColumnPropDesc("FieldValidationInstance":U, "Validation Instance":U, Progress.Data.DataType:CLOB).
        arrayvar15[24] = NEW Progress.Data.ColumnPropDesc("FieldSemanticType":U, "Semantic Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[25] = NEW Progress.Data.ColumnPropDesc("OriginalFieldName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar15[26] = NEW Progress.Data.ColumnPropDesc("FieldSkipAssignment":U, "Add field to AssignSkipDictionary":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar15.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartTempTableAdapter1:AfterUpdateRecord:Subscribe(THIS-OBJECT:smartTempTableAdapter1_AfterUpdateRecord).
        THIS-OBJECT:smartTempTableAdapter1:BeforeAssignRecord:Subscribe(THIS-OBJECT:BeforeAssignRecordHandler).
        THIS-OBJECT:smartTempTableAdapter1:BeforeCreateRecord:Subscribe(THIS-OBJECT:smartTempTableAdapter1_BeforeCreateRecord).
        THIS-OBJECT:smartTempTableAdapter1:BeforeDeleteRecord:Subscribe(THIS-OBJECT:smartTempTableAdapter1_BeforeDeleteRecord).
        /*  */
        /* m_FieldEditorForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 164).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:Name = "m_FieldEditorForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 298).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_FieldEditorForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(718, 164).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:Name = "m_FieldEditorForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 298).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_FieldEditorForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:Name = "m_FieldEditorForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(726, 164).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_FieldEditorForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 462).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:Name = "m_FieldEditorForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(726, 8).
        THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* FieldEditorForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(726, 470).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:FieldEditorForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_FieldEditorForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "FieldEditorForm":U.
        THIS-OBJECT:Text = "Field Editor":U.
        THIS-OBJECT:WindowPositionRegistryKey = "FieldEditorForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:FieldEditorForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:FieldEditorForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        SUPER:OnShown (e) .

        PROCESS EVENTS .

        smartUpdatableBrowser1:EnableFields (EnableFieldsEnum:Update) .

        oValueList = NEW Infragistics.Win.ValueList () .
        oValueList:ValueListItems:Add ("PUBLIC":U) .
        oValueList:ValueListItems:Add ("PROTECTED":U) .
        oValueList:ValueListItems:Add ("PRIVATE":U) .

        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["FieldGetAccess":U]:ValueList = oValueList .
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["FieldSetAccess":U]:ValueList = CAST (oValueList:Clone(),
                                                                                                    Infragistics.Win.ValueList) .

        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["OriginalFieldName":U]:Hidden = TRUE .
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["OriginalFieldName":U]:ExcludeFromColumnChooser = ExcludeFromColumnChooser:True .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the WAIT-FOR Operation for this Form
        Notes:   Seperate method, to work around compile error with WAIT-FOR in non
                 void method ShowEditor
        @param poResult OUTPUT The DialogResult of the wait for operation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformWaitFor (OUTPUT poResult AS System.Windows.Forms.DialogResult):

        WAIT-FOR THIS-OBJECT:ShowDialog () SET poResult .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Form as a modal dialog
        Notes:
        @param pcTableName The name of the table (for the heading of the Form)
        @param eField INPUT-OUTPUT TABLE The temp-table with the fields to update
        @param poValueList The ValueList with the data types
        @return Logical value indicating if the user has chosen OK
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ShowEditor (pcTableName AS CHARACTER,
                                      INPUT-OUTPUT TABLE FOR eField BIND,
                                      poValueList AS Infragistics.Win.ValueList):

        DEFINE VARIABLE oResult AS System.Windows.Forms.DialogResult NO-UNDO .

        THIS-OBJECT:Text = SUBSTITUTE ("&1 - &2":U,
                                       THIS-OBJECT:Text,
                                       pcTableName) .

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE eField:HANDLE .
        smartTempTableAdapter1:QuerySort = "BY FieldOrder":U .
        smartTempTableAdapter1:RetrieveData() .

        smartUpdatableBrowser1:DeselectRowsOnActivate = FALSE .
        smartUpdatableBrowser1:EnableMultiSelection () .

        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns ["FieldDataType":U]:ValueList = poValueList .

        THIS-OBJECT:PerformWaitFor (OUTPUT oResult) .

        RETURN Progress.Util.EnumHelper:AreEqual (oResult,
                                                  System.Windows.Forms.DialogResult:OK) .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the smartTempTableAdapter1
        Notes:   Ensures property ordering (by Field Order) of the records
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartTempTableAdapter1_AfterUpdateRecord (sender AS System.Object,
                                                                  e AS AfterUpdateRecordEventArgs):

        DEFINE BUFFER b_eField FOR eField .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEFINE QUERY bQuery FOR b_eField .

        OPEN QUERY bQuery PRESELECT EACH b_eField BY b_eField.FieldOrder .

        GET FIRST bQuery .

        DO WHILE NOT QUERY bQuery:QUERY-OFF-END:
            i = i + 1 .

            ASSIGN b_eField.FieldOrder = i .

            GET NEXT bQuery .
        END .

        ASSIGN i = smartTempTableAdapter1:BindingSource:Position .

        smartTempTableAdapter1:RetrieveData () .

        ASSIGN smartTempTableAdapter1:BindingSource:Position = i .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler of the BeforeCreateRecord event of the smartTempTableAdapter1
        Notes:   Assigns FieldOrder and FieldName
        @param sender The reference to the object that raised the event
        @param e The BeforeCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartTempTableAdapter1_BeforeCreateRecord (sender AS System.Object,
                                                                   e AS BeforeCreateRecordEventArgs):

        DEFINE BUFFER b_eField FOR eField .

        IF VALID-OBJECT (smartUpdatableBrowser1:ActiveRow) THEN
            e:BufferHandle::FieldOrder = UNBOX (smartUpdatableBrowser1:ActiveRow:Cells["FieldOrder":U]:Value) + 1.
        ELSE DO:
            FIND LAST b_eField USE-INDEX Order.

            IF AVAILABLE b_eField THEN
                e:BufferHandle::FieldOrder = b_eField.FieldOrder + 1 .
            ELSE
                e:BufferHandle::FieldOrder = 1 .
        END.

        e:BufferHandle::FieldName = THIS-OBJECT:EnsureUniqueFieldName ("Field":U) .
        e:BufferHandle::FieldDataType = "CHARACTER":U .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeDeleteRecord event of the
        Notes:   Implements multi-selection deletion
        @param sender The reference to the object that raised the event
        @param e The BeforeDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartTempTableAdapter1_BeforeDeleteRecord (sender AS System.Object,
                                                                   e AS BeforeDeleteRecordEventArgs):

        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.

        smartUpdatableBrowser1:ActiveRow:Selected = TRUE .

        e:Cancel = TRUE .

        {Consultingwerk/foreach.i UltraGridRow oRow in smartUpdatableBrowser1:Selected:Rows}
            cFieldName = UNBOX (oRow:Cells["FieldName":U]:Value) .

            FIND eField WHERE eField.FieldName = cFieldName .

            DELETE eField .
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            smartUpdatableBrowser1:Selected:Rows:Clear() .
            smartTempTableAdapter1:RetrieveData() .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The System.Object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "OK":U THEN DO:
                THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK .
                THIS-OBJECT:Close () .
            END.
            WHEN "Cancel":U THEN DO:
                THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel .
                THIS-OBJECT:Close () .
            END.

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterCopyRecord event of the smartUpdatableBrowser1
        Notes:   Assigns unique FieldName
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartUpdatableBrowser1_AfterCopyRecord (sender AS System.Object,
                                                                e AS System.EventArgs):

        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (smartUpdatableBrowser1:ActiveRow) THEN
            RETURN .

        ASSIGN cFieldName = smartUpdatableBrowser1:ActiveRow:Cells["FieldName":U]:Value .

        smartUpdatableBrowser1:ActiveRow:Cells["FieldName":U]:Value = THIS-OBJECT:EnsureUniqueFieldName (cFieldName) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FieldEditorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC FieldEditorForm ():

    END DESTRUCTOR .

END CLASS.
