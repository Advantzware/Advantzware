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
    File        : BatchGeneratorWizard
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 20 08:58:38 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                               FROM PROPATH .
USING Consultingwerk.Forms.*                                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Batch.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.BatchGenerator.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                          FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Consultingwerk.Windows.Forms.*                                 FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                            FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .
USING System.Windows.Forms.*                                         FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                             FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.BatchGenerator.BatchGeneratorWizard INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE coDatabaseName AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbForceUpperCase AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTables AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDatasetControllerNamePattern AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDataAccessNamePattern AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbBusinessEntityNamePattern AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE toCreateSubPackage AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE toGenerateDatasetModelClasses AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE VARIABLE oStatusForm AS GenericProcessingStatusWithCancelForm NO-UNDO .
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BatchGeneratorWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BatchGeneratorWizard ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnOk
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnOk_Click (sender AS System.Object,
                                     e AS System.EventArgs):

        DEFINE VARIABLE oGenerator AS BatchGenerator NO-UNDO .

        DO ON ERROR UNDO, THROW:

            SESSION:SET-WAIT-STATE ("general":U) .

            oStatusForm = NEW GenericProcessingStatusWithCancelForm () .
            oStatusForm:TopMost = TRUE .
            oStatusForm:Show () .

            oGenerator = NEW BatchGenerator () .
            oGenerator:StatusChanged:Subscribe (StatusChangedHandler) .

            oGenerator:GenerateBusinessEntities (tbPackage:Text,                          /* Package */
                                                 toCreateSubPackage:Checked,              /* Create sub packages */
                                                 tbBusinessEntityNamePattern:Text,        /* BE Name Pattern */
                                                 tbDataAccessNamePattern:Text,            /* DA Name Pattern */
                                                 tbDatasetControllerNamePattern:Text,     /* DSC Name Pattern */
                                                 cbForceUpperCase:Checked,                /* Force Uppercase first Name of Table */
                                                 coDatabaseName:Text,                     /* DB Name */
                                                 tbTables:Text,                           /* Table Filter (CAN-DO), i.e. "Cu*,Or*" or "*" or "Customer" */
                                                 toGenerateDatasetModelClasses:Checked) . /* Generate DatasetModel Classes */

            oGenerator:StatusChanged:Unsubscribe (StatusChangedHandler) .

            THIS-OBJECT:Close () .

            FINALLY:
                SESSION:SET-WAIT-STATE ("":U) .

                oStatusForm:Close () .
            END FINALLY.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.BatchGenerator.BatchGeneratorWizard":U).
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbBusinessEntityNamePattern = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbDataAccessNamePattern = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbDatasetControllerNamePattern = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbTables = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:coDatabaseName = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbForceUpperCase = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:toCreateSubPackage = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:toGenerateDatasetModelClasses = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:tbPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbBusinessEntityNamePattern, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDataAccessNamePattern, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDatasetControllerNamePattern, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTables, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coDatabaseName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbForceUpperCase, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:toCreateSubPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:toGenerateDatasetModelClasses, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(85, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Package Name:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 65).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(158, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "Business Entity Name Pattern:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(12, 92).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(144, 14).
        THIS-OBJECT:ultraLabel3:TabIndex = 5.
        THIS-OBJECT:ultraLabel3:Text = "Data Access Name Pattern:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(12, 119).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(171, 14).
        THIS-OBJECT:ultraLabel4:TabIndex = 7.
        THIS-OBJECT:ultraLabel4:Text = "Dataset Controller Name Pattern:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(12, 145).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(61, 14).
        THIS-OBJECT:ultraLabel5:TabIndex = 9.
        THIS-OBJECT:ultraLabel5:Text = "First Letter:":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(12, 172).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(56, 14).
        THIS-OBJECT:ultraLabel6:TabIndex = 11.
        THIS-OBJECT:ultraLabel6:Text = "Database:":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        THIS-OBJECT:ultraLabel7:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(12, 199).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(93, 14).
        THIS-OBJECT:ultraLabel7:TabIndex = 13.
        THIS-OBJECT:ultraLabel7:Text = "Tables (CAN-DO)":U.
        /*  */
        /* tbPackage */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbPackage:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbPackage:Location = NEW System.Drawing.Point(180, 8).
        THIS-OBJECT:tbPackage:Name = "tbPackage":U.
        THIS-OBJECT:tbPackage:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:tbPackage:TabIndex = 1.
        THIS-OBJECT:tbPackage:EditorButtonClick:Subscribe(THIS-OBJECT:tbPackage_EditorButtonClick).
        THIS-OBJECT:tbPackage:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        THIS-OBJECT:tbPackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* tbBusinessEntityNamePattern */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbBusinessEntityNamePattern:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbBusinessEntityNamePattern:Location = NEW System.Drawing.Point(180, 61).
        THIS-OBJECT:tbBusinessEntityNamePattern:Name = "tbBusinessEntityNamePattern":U.
        THIS-OBJECT:tbBusinessEntityNamePattern:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:tbBusinessEntityNamePattern:TabIndex = 4.
        THIS-OBJECT:tbBusinessEntityNamePattern:Text = "&1BusinessEntity":U.
        THIS-OBJECT:tbBusinessEntityNamePattern:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        /*  */
        /* tbDataAccessNamePattern */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataAccessNamePattern:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataAccessNamePattern:Location = NEW System.Drawing.Point(180, 88).
        THIS-OBJECT:tbDataAccessNamePattern:Name = "tbDataAccessNamePattern":U.
        THIS-OBJECT:tbDataAccessNamePattern:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:tbDataAccessNamePattern:TabIndex = 6.
        THIS-OBJECT:tbDataAccessNamePattern:Text = "&1DataAccess":U.
        THIS-OBJECT:tbDataAccessNamePattern:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        /*  */
        /* tbDatasetControllerNamePattern */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatasetControllerNamePattern:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatasetControllerNamePattern:Location = NEW System.Drawing.Point(180, 115).
        THIS-OBJECT:tbDatasetControllerNamePattern:Name = "tbDatasetControllerNamePattern":U.
        THIS-OBJECT:tbDatasetControllerNamePattern:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:tbDatasetControllerNamePattern:TabIndex = 8.
        THIS-OBJECT:tbDatasetControllerNamePattern:Text = "&1DatasetController":U.
        THIS-OBJECT:tbDatasetControllerNamePattern:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        /*  */
        /* tbTables */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTables:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTables:Location = NEW System.Drawing.Point(180, 195).
        THIS-OBJECT:tbTables:Name = "tbTables":U.
        THIS-OBJECT:tbTables:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:tbTables:TabIndex = 14.
        THIS-OBJECT:tbTables:Text = "*":U.
        THIS-OBJECT:tbTables:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        /*  */
        /* coDatabaseName */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coDatabaseName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coDatabaseName:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coDatabaseName:Location = NEW System.Drawing.Point(180, 168).
        THIS-OBJECT:coDatabaseName:Name = "coDatabaseName":U.
        THIS-OBJECT:coDatabaseName:Size = NEW System.Drawing.Size(295, 21).
        THIS-OBJECT:coDatabaseName:SortStyle = Infragistics.Win.ValueListSortStyle:Ascending.
        THIS-OBJECT:coDatabaseName:TabIndex = 12.
        THIS-OBJECT:coDatabaseName:TextChanged:Subscribe(THIS-OBJECT:tbPackage_TextChanged).
        /*  */
        /* cbForceUpperCase */
        /*  */
        THIS-OBJECT:cbForceUpperCase:AutoSize = TRUE.
        THIS-OBJECT:cbForceUpperCase:Location = NEW System.Drawing.Point(180, 144).
        THIS-OBJECT:cbForceUpperCase:Name = "cbForceUpperCase":U.
        THIS-OBJECT:cbForceUpperCase:Size = NEW System.Drawing.Size(113, 17).
        THIS-OBJECT:cbForceUpperCase:TabIndex = 10.
        THIS-OBJECT:cbForceUpperCase:Text = "Force Upper Case":U.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:Enabled = FALSE.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(319, 257).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 17.
        THIS-OBJECT:btnOk:Text = "&OK":U.
        THIS-OBJECT:btnOk:Click:Subscribe(THIS-OBJECT:btnOk_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(400, 257).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 18.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* toCreateSubPackage */
        /*  */
        THIS-OBJECT:toCreateSubPackage:Location = NEW System.Drawing.Point(180, 35).
        THIS-OBJECT:toCreateSubPackage:Name = "toCreateSubPackage":U.
        THIS-OBJECT:toCreateSubPackage:Size = NEW System.Drawing.Size(295, 20).
        THIS-OBJECT:toCreateSubPackage:TabIndex = 2.
        THIS-OBJECT:toCreateSubPackage:Text = "Create distinct package per Entity":U.
        /*  */
        /* toGenerateDatasetModelClasses */
        /*  */
        THIS-OBJECT:toGenerateDatasetModelClasses:Location = NEW System.Drawing.Point(180, 222).
        THIS-OBJECT:toGenerateDatasetModelClasses:Name = "toGenerateDatasetModelClasses":U.
        THIS-OBJECT:toGenerateDatasetModelClasses:Size = NEW System.Drawing.Size(295, 20).
        THIS-OBJECT:toGenerateDatasetModelClasses:TabIndex = 16.
        THIS-OBJECT:toGenerateDatasetModelClasses:Text = "Generate":U.
        /*  */
        /* ultraLabel8 */
        /*  */
        THIS-OBJECT:ultraLabel8:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel8:Location = NEW System.Drawing.Point(12, 225).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        THIS-OBJECT:ultraLabel8:Size = NEW System.Drawing.Size(121, 14).
        THIS-OBJECT:ultraLabel8:TabIndex = 15.
        THIS-OBJECT:ultraLabel8:Text = "DatasetModel Classes:":U.
        /*  */
        /* BatchGeneratorWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(487, 293).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:toGenerateDatasetModelClasses).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:toCreateSubPackage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbForceUpperCase).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:coDatabaseName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTables).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDatasetControllerNamePattern).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDataAccessNamePattern).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbBusinessEntityNamePattern).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbPackage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(503, 293).
        THIS-OBJECT:Name = "BatchGeneratorWizard":U.
        THIS-OBJECT:Text = "Batch Generator Wizard":U.
        THIS-OBJECT:UseStartupSizeAsMinSize = TRUE.
        THIS-OBJECT:WindowPositionRegistryKey = "BatchGeneratorWizard":U.
        CAST(THIS-OBJECT:tbPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbBusinessEntityNamePattern, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDataAccessNamePattern, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDatasetControllerNamePattern, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTables, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coDatabaseName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbForceUpperCase, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:toCreateSubPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:toGenerateDatasetModelClasses, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

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

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-DBS:
            coDatabaseName:ValueList:ValueListItems:Add (BOX (LDBNAME (i))) .
        END.

        IF coDatabaseName:ValueList:ValueListItems:Count > 0 THEN
            coDatabaseName:SelectedIndex = 0 .

        SUPER:OnLoad (e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the StatusChanged event of the BatchGenerator
                 class
        Notes:
        @param sender The reference to the object that raised this event
        @param e The ProcessingStatusChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StatusChangedHandler (sender AS Progress.Lang.Object,
                                                e AS ProcessingStatusChangedEventArgs):

        IF VALID-OBJECT (oStatusForm) THEN DO:
            oStatusForm:StatusText = e:Message .

            IF oStatusForm:Cancelled = TRUE THEN
                e:Cancel = TRUE .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the smartBusinessEntityBindingSource1_eBusinessEntity_BusinessEntityPackage
                 Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbPackage_EditorButtonClick (sender AS System.Object,
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
                ASSIGN THIS-OBJECT:tbPackage:Text =
                        REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (FILE-INFORMATION:FULL-PATHNAME, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, ".":U) .
            ELSE
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                              ("Unable to translate path into package."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               MessageFormImages:ImageWarning) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of all input controls
        Notes:   Verifies that all fields a populated and enables the btnOk
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbPackage_TextChanged (sender AS System.Object,
                                               e AS System.EventArgs):

        btnOk:Enabled = (THIS-OBJECT:tbPackage:Text > "":U AND
                         THIS-OBJECT:tbBusinessEntityNamePattern:Text > "":U AND
                         THIS-OBJECT:tbDataAccessNamePattern:Text > "":U AND
                         THIS-OBJECT:tbDatasetControllerNamePattern:Text > "":U AND
                         THIS-OBJECT:coDatabaseName:Text > "":U AND
                         THIS-OBJECT:tbTables:Text > "":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BatchGeneratorWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BatchGeneratorWizard ():

    END DESTRUCTOR .

END CLASS.
