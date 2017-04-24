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
    File        : Web2FormWizard
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 17 18:09:47 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH.
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH.
USING Consultingwerk.*                                     FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Consultingwerk.OERA.*                                FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.*       FROM PROPATH .
USING Consultingwerk.Forms.*                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.Web2.* FROM PROPATH .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.Web2.Web2FormWizard
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE cbViewerLayout AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbGridLayout AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE btnGenerate AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnClose AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewerTemplate AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewerName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbToolbarName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbHeading AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbGridName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDataSourceName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbControllerName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel9 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE VARIABLE oDescriptor AS IBusinessEntityDescriptor NO-UNDO .

    /**
     * Purpose: Gets and sets the Business Entity Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Business Entity Package
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the Web2FormWizard class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Web2FormWizard ():

        SUPER().

        InitializeComponent().

        THIS-OBJECT:StoreFormPosition = FALSE .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.


    /**
     * Purpose:
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID cbViewerLayout_ValueChanged (INPUT sender AS System.Object,
                               INPUT e AS System.EventArgs):

        IF UNBOX (cbViewerLayout:Value) > "":U THEN
            ASSIGN tbViewerTemplate:Text    = "":U
                   tbViewerTemplate:Enabled = FALSE .
        ELSE
            ASSIGN tbViewerTemplate:Enabled = TRUE .

        THIS-OBJECT:ValidateButtonGenerator() .

    END METHOD .

    /**
     * Purpose: Validates if the BtnGenerate should be enabled
     * Notes:
     */
    METHOD PROTECTED VOID ValidateButtonGenerator():

        ASSIGN btnGenerate:Enabled = UNBOX  (cbGridLayout:Value) > "":U AND
                                     (UNBOX (cbViewerLayout:Value) > "":U OR tbViewerTemplate:Text > "":U) AND
                                     tbControllerName:Text > "":U AND
                                     tbHeading:Text > "":U AND
                                     tbDataSourceName:Text > "":U AND
                                     tbGridName:Text > "":U AND
                                     tbToolbarName:Text > "":U AND
                                     tbViewerName:Text > "":U .
    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnGenerate
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID btnGenerate_Click (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE cFileName     AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oGenerator    AS Web2FormGenerator NO-UNDO .
        DEFINE VARIABLE cEntityName   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cEntityTable  AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cEntityView   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult      NO-UNDO .

        WAIT-FOR saveFileDialog1:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ASSIGN cFileName    = saveFileDialog1:FileName
               cEntityName  = oDescriptor:BusinessEntityName
               cEntityTable = oDescriptor:Views:GetItem (UNBOX (cbGridLayout:Value)):EntityTable
               cEntityView  = oDescriptor:Views:GetItem (UNBOX (cbGridLayout:Value)):EntityView .

        ASSIGN oGenerator = NEW Web2FormGenerator() .

        oGenerator:Generate (cFileName,
                             ?,
                             tbControllerName:Text,
                             tbHeading:Text,
                             UNBOX (cbGridLayout:Value),
                             cEntityName,
                             cEntityTable,
                             cEntityView,
                             tbDataSourceName:Text,
                             tbGridName:Text,
                             tbToolbarName:Text,
                             tbViewerName:Text,
                             UNBOX (cbViewerLayout:Value),
                             tbViewerTemplate:Text) .

        WindowsExplorerHelper:ShowFileInExplorer (cFileName).

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handler for the ValueChanged event of the two combo editors
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID cbGridLayout_ValueChanged (sender AS System.Object,
                                                   e AS System.EventArgs):

        THIS-OBJECT:InitializeComponentNames() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.Web2.Web2FormWizard":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel9 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbToolbarName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbViewerName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbGridName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbDataSourceName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbViewerTemplate = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:cbViewerLayout = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbGridLayout = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbHeading = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbControllerName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnGenerate = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnClose = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:tbToolbarName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbViewerName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbGridName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbViewerTemplate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbViewerLayout, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbGridLayout, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbHeading, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbControllerName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel9).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbToolbarName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbViewerName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbGridName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbDataSourceName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbViewerTemplate).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbViewerLayout).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbGridLayout).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbHeading).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbControllerName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(546, 332).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* ultraLabel9 */
        /*  */
        THIS-OBJECT:ultraLabel9:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel9:Location = NEW System.Drawing.Point(12, 258).
        THIS-OBJECT:ultraLabel9:Name = "ultraLabel9":U.
        THIS-OBJECT:ultraLabel9:Size = NEW System.Drawing.Size(79, 14).
        THIS-OBJECT:ultraLabel9:TabIndex = 19.
        THIS-OBJECT:ultraLabel9:Text = "Toolbar Name:":U.
        /*  */
        /* ultraLabel8 */
        /*  */
        THIS-OBJECT:ultraLabel8:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel8:Location = NEW System.Drawing.Point(12, 231).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        THIS-OBJECT:ultraLabel8:Size = NEW System.Drawing.Size(75, 14).
        THIS-OBJECT:ultraLabel8:TabIndex = 18.
        THIS-OBJECT:ultraLabel8:Text = "Viewer Name:":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        THIS-OBJECT:ultraLabel7:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(12, 204).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(62, 14).
        THIS-OBJECT:ultraLabel7:TabIndex = 17.
        THIS-OBJECT:ultraLabel7:Text = "Grid Name:":U.
        /*  */
        /* tbToolbarName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbToolbarName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbToolbarName:Location = NEW System.Drawing.Point(150, 254).
        THIS-OBJECT:tbToolbarName:Name = "tbToolbarName":U.
        THIS-OBJECT:tbToolbarName:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbToolbarName:TabIndex = 16.
        THIS-OBJECT:tbToolbarName:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbViewerName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerName:Location = NEW System.Drawing.Point(150, 227).
        THIS-OBJECT:tbViewerName:Name = "tbViewerName":U.
        THIS-OBJECT:tbViewerName:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbViewerName:TabIndex = 15.
        THIS-OBJECT:tbViewerName:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbGridName */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbGridName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbGridName:Location = NEW System.Drawing.Point(150, 200).
        THIS-OBJECT:tbGridName:Name = "tbGridName":U.
        THIS-OBJECT:tbGridName:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbGridName:TabIndex = 14.
        THIS-OBJECT:tbGridName:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbDataSourceName */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Location = NEW System.Drawing.Point(150, 173).
        THIS-OBJECT:tbDataSourceName:Name = "tbDataSourceName":U.
        THIS-OBJECT:tbDataSourceName:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbDataSourceName:TabIndex = 13.
        THIS-OBJECT:tbDataSourceName:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbViewerTemplate */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerTemplate:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbViewerTemplate:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbViewerTemplate:Location = NEW System.Drawing.Point(150, 133).
        THIS-OBJECT:tbViewerTemplate:Name = "tbViewerTemplate":U.
        THIS-OBJECT:tbViewerTemplate:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbViewerTemplate:TabIndex = 12.
        THIS-OBJECT:tbViewerTemplate:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        THIS-OBJECT:tbViewerTemplate:EditorButtonClick:Subscribe(THIS-OBJECT:tbViewerTemplate_EditorButtonClick).
        /*  */
        /* cbViewerLayout */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbViewerLayout:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbViewerLayout:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbViewerLayout:Location = NEW System.Drawing.Point(150, 106).
        THIS-OBJECT:cbViewerLayout:Name = "cbViewerLayout":U.
        THIS-OBJECT:cbViewerLayout:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:cbViewerLayout:TabIndex = 11.
        THIS-OBJECT:cbViewerLayout:ValueChanged:Subscribe(THIS-OBJECT:cbViewerLayout_ValueChanged).
        /*  */
        /* cbGridLayout */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbGridLayout:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbGridLayout:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbGridLayout:Location = NEW System.Drawing.Point(150, 79).
        THIS-OBJECT:cbGridLayout:Name = "cbGridLayout":U.
        THIS-OBJECT:cbGridLayout:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:cbGridLayout:TabIndex = 10.
        THIS-OBJECT:cbGridLayout:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbHeading */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbHeading:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbHeading:Location = NEW System.Drawing.Point(150, 39).
        THIS-OBJECT:tbHeading:Name = "tbHeading":U.
        THIS-OBJECT:tbHeading:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbHeading:TabIndex = 9.
        THIS-OBJECT:tbHeading:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* tbControllerName */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbControllerName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbControllerName:Location = NEW System.Drawing.Point(150, 12).
        THIS-OBJECT:tbControllerName:Name = "tbControllerName":U.
        THIS-OBJECT:tbControllerName:Size = NEW System.Drawing.Size(385, 21).
        THIS-OBJECT:tbControllerName:TabIndex = 8.
        THIS-OBJECT:tbControllerName:Text = "simpleCtrl":U.
        THIS-OBJECT:tbControllerName:ValueChanged:Subscribe(THIS-OBJECT:tbControllerName_ValueChanged).
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(12, 177).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(103, 14).
        THIS-OBJECT:ultraLabel6:TabIndex = 7.
        THIS-OBJECT:ultraLabel6:Text = "Data Source Name:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(12, 137).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(92, 14).
        THIS-OBJECT:ultraLabel5:TabIndex = 6.
        THIS-OBJECT:ultraLabel5:Text = "Viewer Template:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(12, 110).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(79, 14).
        THIS-OBJECT:ultraLabel4:TabIndex = 5.
        THIS-OBJECT:ultraLabel4:Text = "Viewer Layout:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(12, 83).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(66, 14).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "Grid Layout:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 43).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(50, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "Heading:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 16).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(87, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Controller Name":U.
        /*  */
        /* btnGenerate */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnGenerate:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnGenerate:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnGenerate:Location = NEW System.Drawing.Point(379, 338).
        THIS-OBJECT:btnGenerate:Name = "btnGenerate":U.
        THIS-OBJECT:btnGenerate:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnGenerate:TabIndex = 0.
        THIS-OBJECT:btnGenerate:Text = "&Generate":U.
        THIS-OBJECT:btnGenerate:Click:Subscribe(THIS-OBJECT:btnGenerate_Click).
        /*  */
        /* btnClose */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnClose:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnClose:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnClose:Location = NEW System.Drawing.Point(460, 338).
        THIS-OBJECT:btnClose:Name = "btnClose":U.
        THIS-OBJECT:btnClose:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnClose:TabIndex = 1.
        THIS-OBJECT:btnClose:Text = "&Close":U.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "html":U.
        THIS-OBJECT:openFileDialog1:FileName = "openFileDialog1":U.
        THIS-OBJECT:openFileDialog1:Filter = "html-Files|*.html|All Files|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Open Viewer Template":U.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "html":U.
        THIS-OBJECT:saveFileDialog1:Filter = "html-Files|*.html|All Files|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save Form as":U.
        /*  */
        /* Web2FormWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnGenerate.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnClose.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(547, 373).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnGenerate).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnClose).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "Web2FormWizard":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Angular JS Web2 Form Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "Web2FormWizard":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:tbToolbarName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbViewerName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbGridName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbViewerTemplate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbViewerLayout, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbGridLayout, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbHeading, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbControllerName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Initializes the component names
     * Notes:
     */
    METHOD PROTECTED VOID InitializeComponentNames():

        DEFINE VARIABLE cGridName   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cViewerName AS CHARACTER NO-UNDO.

        ASSIGN cGridName   = UNBOX (cbGridLayout:Value)
               cViewerName = UNBOX (cbViewerLayout:Value) .

        IF CharacterType:IsNullOrEmpty (cViewerName) THEN
            ASSIGN cViewerName              = cGridName
                   tbViewerTemplate:Enabled = TRUE .
        ELSE
            ASSIGN tbViewerTemplate:Enabled = FALSE
                   tbViewerTemplate:Text    = "":U .

        ASSIGN tbDataSourceName:Text = SUBSTITUTE ("&1DataSource":U, cGridName)
               tbGridName:Text       = SUBSTITUTE ("&1Grid":U, cGridName)
               tbViewerName:Text     = SUBSTITUTE ("&1Viewer":U, cViewerName)
               tbToolbarName:Text    = SUBSTITUTE ("&1Toolbar":U, cViewerName) .

    END METHOD.

    /**
     * Purpose: Initializes the Wizard fields
     * Notes:
     */
    METHOD PROTECTED VOID InitializeFields ():

        DEFINE VARIABLE cEntityName AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oService    AS IBusinessEntityDescriptorService     NO-UNDO .
        DEFINE VARIABLE oParameter  AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE hDataset    AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE cTitle      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cViewerName AS CHARACTER                            NO-UNDO .

        oService = {Consultingwerk/get-service.i Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptorService
                                                 "NEW BusinessEntityDescriptorService ()"}.

        oService:ClearCache() .

        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (THIS-OBJECT:BusinessEntityPackage) THEN
            ASSIGN cEntityName = SUBSTITUTE ("&1.&2":U,
                                             THIS-OBJECT:BusinessEntityPackage,
                                             THIS-OBJECT:BusinessEntityName) .
        ELSE
            ASSIGN cEntityName = THIS-OBJECT:BusinessEntityName .

        oParameter = NEW GetBusinessEntityDescriptorParameter (TRUE) .

        ServiceInterface:InvokeMethod (cEntityName,
                                       "GetBusinessEntityDescriptor":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        ASSIGN oDescriptor = oParameter:Descriptor .

        cbViewerLayout:ValueList:ValueListItems:Add ("":U):DisplayText = "< Use Template >"{&TRAN} .

        {Consultingwerk/foreachABL.i BusinessEntityViewDescriptor oView IN oDescriptor:Views}

            cbGridLayout:ValueList:ValueListItems:Add (oView:Name) .
            cbViewerLayout:ValueList:ValueListItems:Add (oView:Name) .

        END.

        IF oDescriptor:DefaultViewName > "":U THEN
            ASSIGN cbGridLayout:Value   = BOX (oDescriptor:DefaultViewName)
                   cbViewerLayout:Value = BOX (oDescriptor:DefaultViewName) .
        ELSE IF oDescriptor:Views:Count > 0 THEN
            ASSIGN cbGridLayout:Value   = BOX (oDescriptor:Views:GetItem (1):Name)
                   cbViewerLayout:Value = BOX (oDescriptor:Views:GetItem (1):Name) .

        THIS-OBJECT:InitializeComponentNames () .

        ASSIGN cTitle = SUBSTITUTE ("&1 &2":U ,
                                    StringHelper:WellFormFromCamelCase (hDataset:GET-BUFFER-HANDLE (1):NAME),
                                    "Maintenance"{&TRAN}) .

        IF cTitle BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix THEN
            cTitle = TRIM (SUBSTRING (cTitle, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) + 1, -1, "CHARACTER":U)) .

        tbHeading:Text = cTitle .

        ASSIGN cViewerName = SUBSTITUTE ("&1&2":U ,
                                         hDataset:GET-BUFFER-HANDLE (1):NAME,
                                         "Viewer.html":U) .

        IF cViewerName BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix THEN
            cViewerName = TRIM (SUBSTRING (cViewerName, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) + 1, -1, "CHARACTER":U)) .

        openFileDialog1:FileName = cViewerName .

        ASSIGN cViewerName = SUBSTITUTE ("&1&2":U ,
                                         hDataset:GET-BUFFER-HANDLE (1):NAME,
                                         "Form.html":U) .

        IF cViewerName BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix THEN
            cViewerName = TRIM (SUBSTRING (cViewerName, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) + 1, -1, "CHARACTER":U)) .

        saveFileDialog1:FileName = cViewerName .

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hDataset) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Raise the Shown event
     * Notes:   Calls into InitalizeFields
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        THIS-OBJECT:InitializeFields () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose:
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID tbControllerName_ValueChanged (sender AS System.Object,
                                                       e AS System.EventArgs):

        THIS-OBJECT:ValidateButtonGenerator () .

    END METHOD .

    /**
     * Purpose: Event handler for the EditorButtonClick event of the
     *          tbViewerTemplate
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID tbViewerTemplate_EditorButtonClick (sender AS System.Object,
                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        WAIT-FOR openFileDialog1:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        tbViewerTemplate:Text = REPLACE (FileHelper:RelativeFileName (openFileDialog1:FileName),
                                         "~\":U,
                                         "/":U) .

    END METHOD .

    /**
     * Purpose: Destructor for the Web2FormWizard class
     * Notes:
     */
    DESTRUCTOR PUBLIC Web2FormWizard ():

    END DESTRUCTOR .

END CLASS.
