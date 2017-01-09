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
    File        : RestInterfaceWizard
    Purpose     : Wizard for creating REST Interfaces (Business Entity) classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 08 15:10:06 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Plugins.RestInterfaceGenerator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.RestInterface.*        FROM PROPATH .
USING Consultingwerk.Framework.*                                             FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                                 FROM PROPATH .
USING Consultingwerk.Forms.*                                                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                                  FROM PROPATH .
USING Consultingwerk.Util.*                                                  FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                                     FROM ASSEMBLY .
USING Progress.Lang.*                                                        FROM PROPATH .
USING System.Windows.Forms.*                                                 FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.RestInterfaceGenerator.RestInterfaceWizard INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnGenerate AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbFindTable AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAppServerModule AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDatasetPath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbIncludeFilePath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbSourceFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbURI AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbInterfacePackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbInterfaceClassName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel10 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel9 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Package
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Dataset
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the List of Tables
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tables AS CharacterList NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RestInterfaceWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RestInterfaceWizard ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:Tables = NEW CharacterList () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Client event of the btnGenerate
        Notes:
        @param sender The refernece to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnGenerate_Click (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE oSettings  AS Consultingwerk.Framework.ISettingsService NO-UNDO .
        DEFINE VARIABLE oGenerator AS RestInterfaceGenerator                    NO-UNDO .
        DEFINE VARIABLE cFileName  AS CHARACTER                                 NO-UNDO .

        oSettings = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF tbSourceFolder:Text > "":U THEN
            IF VALID-OBJECT (oSettings) THEN
                oSettings:StoreSetting ("RestInterface":U,
                                        "RestInterfaceSourceFolder":U,
                                        tbSourceFolder:Text) .

        oGenerator = NEW RestInterfaceGenerator () .

        ASSIGN oGenerator:AppServerModule         = tbAppServerModule:Text
               oGenerator:BusinessEntityName      = THIS-OBJECT:BusinessEntityName
               oGenerator:BusinessEntityPackage   = THIS-OBJECT:BusinessEntityPackage
               oGenerator:DatasetName             = THIS-OBJECT:DatasetName
               oGenerator:DatasetPath             = tbDatasetPath:Text
               oGenerator:Description             = tbDescription:Text
               oGenerator:FindTable               = cbFindTable:Text
               oGenerator:IncludeFilePath         = tbIncludeFilePath:Text
               oGenerator:ServiceInterfaceName    = tbInterfaceClassName:Text
               oGenerator:ServiceInterfacePackage = tbInterfacePackage:Text
               oGenerator:SourceFolder            = tbSourceFolder:Text
               oGenerator:URI                     = tbURI:Text
               .

        cFileName = oGenerator:Generate() .

        MessageFormHelper:ShowMessage (SUBSTITUTE ("Successfully generated Rest Interface:~n&1"{&TRAN},
                                                   cFileName)) .

        CATCH err AS Progress.Lang.Error :
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
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.RestInterfaceGenerator.RestInterfaceWizard":U).
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:btnGenerate = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbDatasetPath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbInterfaceClassName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbInterfacePackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAppServerModule = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbURI = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel9 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbSourceFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:cbFindTable = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbIncludeFilePath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel10 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:tbDatasetPath, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbInterfaceClassName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbInterfacePackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAppServerModule, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbURI, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbSourceFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbFindTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbIncludeFilePath, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        THIS-OBJECT:folderBrowserDialog1:RootFolder = System.Environment+SpecialFolder:MyComputer.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "i":U.
        THIS-OBJECT:openFileDialog1:Filter = "Include Files|*.i":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Select Dataset Include":U.
        /*  */
        /* btnGenerate */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnGenerate:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnGenerate:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnGenerate:Location = NEW System.Drawing.Point(343, 294).
        THIS-OBJECT:btnGenerate:Name = "btnGenerate":U.
        THIS-OBJECT:btnGenerate:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnGenerate:TabIndex = 18.
        THIS-OBJECT:btnGenerate:Text = "&Generate":U.
        THIS-OBJECT:btnGenerate:Click:Subscribe(THIS-OBJECT:btnGenerate_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(424, 294).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 19.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* tbDatasetPath */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatasetPath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbDatasetPath:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbDatasetPath:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbDatasetPath:Location = NEW System.Drawing.Point(149, 174).
        THIS-OBJECT:tbDatasetPath:Name = "tbDatasetPath":U.
        THIS-OBJECT:tbDatasetPath:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbDatasetPath:TabIndex = 13.
        THIS-OBJECT:tbDatasetPath:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        THIS-OBJECT:tbDatasetPath:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:tbDatasetPath:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 178).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 12.
        THIS-OBJECT:ultraLabel1:Text = "Dataset Path:":U.
        /*  */
        /* tbInterfaceClassName */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbInterfaceClassName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbInterfaceClassName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbInterfaceClassName:Location = NEW System.Drawing.Point(149, 12).
        THIS-OBJECT:tbInterfaceClassName:Name = "tbInterfaceClassName":U.
        THIS-OBJECT:tbInterfaceClassName:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbInterfaceClassName:TabIndex = 1.
        THIS-OBJECT:tbInterfaceClassName:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* tbDescription */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbDescription:Location = NEW System.Drawing.Point(149, 120).
        THIS-OBJECT:tbDescription:Name = "tbDescription":U.
        THIS-OBJECT:tbDescription:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbDescription:TabIndex = 9.
        /*  */
        /* tbInterfacePackage */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbInterfacePackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Text = "...":U.
        THIS-OBJECT:tbInterfacePackage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tbInterfacePackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbInterfacePackage:Location = NEW System.Drawing.Point(149, 39).
        THIS-OBJECT:tbInterfacePackage:Name = "tbInterfacePackage":U.
        THIS-OBJECT:tbInterfacePackage:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbInterfacePackage:TabIndex = 3.
        THIS-OBJECT:tbInterfacePackage:EditorButtonClick:Subscribe(THIS-OBJECT:tbInterfacePackage_EditorButtonClick).
        THIS-OBJECT:tbInterfacePackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:tbInterfacePackage:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* tbAppServerModule */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAppServerModule:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAppServerModule:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbAppServerModule:Location = NEW System.Drawing.Point(149, 66).
        THIS-OBJECT:tbAppServerModule:Name = "tbAppServerModule":U.
        THIS-OBJECT:tbAppServerModule:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbAppServerModule:TabIndex = 5.
        /*  */
        /* tbURI */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbURI:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbURI:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbURI:Location = NEW System.Drawing.Point(161, 147).
        THIS-OBJECT:tbURI:Name = "tbURI":U.
        THIS-OBJECT:tbURI:Size = NEW System.Drawing.Size(338, 21).
        THIS-OBJECT:tbURI:TabIndex = 11.
        THIS-OBJECT:tbURI:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 43).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Interface Package:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(12, 70).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "AppServer Module:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(12, 124).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 8.
        THIS-OBJECT:ultraLabel4:Text = "Description:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(12, 151).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 10.
        THIS-OBJECT:ultraLabel5:Text = "URI:":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(12, 16).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel7:TabIndex = 0.
        THIS-OBJECT:ultraLabel7:Text = "Interface Class Name:":U.
        /*  */
        /* ultraLabel9 */
        /*  */
        THIS-OBJECT:ultraLabel9:Location = NEW System.Drawing.Point(12, 97).
        THIS-OBJECT:ultraLabel9:Name = "ultraLabel9":U.
        THIS-OBJECT:ultraLabel9:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel9:TabIndex = 6.
        THIS-OBJECT:ultraLabel9:Text = "Source Folder:":U.
        /*  */
        /* tbSourceFolder */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbSourceFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton3:Text = "...":U.
        THIS-OBJECT:tbSourceFolder:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:tbSourceFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbSourceFolder:Location = NEW System.Drawing.Point(149, 93).
        THIS-OBJECT:tbSourceFolder:Name = "tbSourceFolder":U.
        THIS-OBJECT:tbSourceFolder:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbSourceFolder:TabIndex = 7.
        THIS-OBJECT:tbSourceFolder:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick_1).
        THIS-OBJECT:tbSourceFolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:tbSourceFolder:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* cbFindTable */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbFindTable:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbFindTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbFindTable:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbFindTable:Location = NEW System.Drawing.Point(149, 232).
        THIS-OBJECT:cbFindTable:Name = "cbFindTable":U.
        THIS-OBJECT:cbFindTable:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:cbFindTable:TabIndex = 17.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(12, 236).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 16.
        THIS-OBJECT:ultraLabel6:Text = "Search using table:":U.
        /*  */
        /* ultraLabel8 */
        /*  */
        THIS-OBJECT:ultraLabel8:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel8:Location = NEW System.Drawing.Point(149, 151).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        THIS-OBJECT:ultraLabel8:Size = NEW System.Drawing.Size(7, 14).
        THIS-OBJECT:ultraLabel8:TabIndex = 18.
        THIS-OBJECT:ultraLabel8:Text = "/":U.
        /*  */
        /* tbIncludeFilePath */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbIncludeFilePath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton4:Text = "...":U.
        THIS-OBJECT:tbIncludeFilePath:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:tbIncludeFilePath:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbIncludeFilePath:Location = NEW System.Drawing.Point(149, 201).
        THIS-OBJECT:tbIncludeFilePath:Name = "tbIncludeFilePath":U.
        THIS-OBJECT:tbIncludeFilePath:Size = NEW System.Drawing.Size(350, 21).
        THIS-OBJECT:tbIncludeFilePath:TabIndex = 15.
        THIS-OBJECT:tbIncludeFilePath:TextChanged:Subscribe(THIS-OBJECT:RestInterfaceWizard_TextChanged).
        /*  */
        /* ultraLabel10 */
        /*  */
        THIS-OBJECT:ultraLabel10:Location = NEW System.Drawing.Point(12, 205).
        THIS-OBJECT:ultraLabel10:Name = "ultraLabel10":U.
        THIS-OBJECT:ultraLabel10:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel10:TabIndex = 14.
        THIS-OBJECT:ultraLabel10:Text = "Include Path:":U.
        /*  */
        /* RestInterfaceWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnGenerate.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(511, 329).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel10).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbIncludeFilePath).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbFindTable).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel9).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbSourceFolder).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbURI).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAppServerModule).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbInterfacePackage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbInterfaceClassName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbDatasetPath).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnGenerate).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "RestInterfaceWizard":U.
        THIS-OBJECT:Text = "REST Interface (JSDO) Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "RestInterfaceWizard":U.
        CAST(THIS-OBJECT:tbDatasetPath, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbInterfaceClassName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbInterfacePackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAppServerModule, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbURI, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbSourceFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbFindTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbIncludeFilePath, System.ComponentModel.ISupportInitialize):EndInit().
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
        Purpose: Raises the load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE cInterfaceName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSourceFolder  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cProjectName   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRelativePath  AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oSettings AS Consultingwerk.Framework.ISettingsService NO-UNDO .

        SUPER:OnLoad (e) .

        oSettings = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettings) THEN DO:
            ASSIGN cSourceFolder = oSettings:GetSetting ("RestInterface":U,
                                                         "RestInterfaceSourceFolder":U) .

            IF cSourceFolder > "":U THEN
                tbSourceFolder:Text = cSourceFolder.

            FILE-INFORMATION:FILE-NAME = SUBSTITUTE ("&1~\&2.i":U,
                                              cSourceFolder,
                                              THIS-OBJECT:DatasetName) .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN DO:
                ASSIGN cProjectName = OpenEdgeArchitectHelper:FindProjectNameForPath (FILE-INFORMATION:FULL-PATHNAME) .

                IF cProjectName > "":U THEN DO:

                    ASSIGN cRelativePath = SUBSTRING (FILE-INFORMATION:FULL-PATHNAME,
                                                      LENGTH (OpenEdgeArchitectHelper:FindProjectPath (FILE-INFORMATION:FULL-PATHNAME), "CHARACTER":U) + 2,
                                                      -1,
                                                      "CHARACTER":U)
                           cRelativePath = REPLACE (cRelativePath, "~\":U, "/":U)
                           tbDatasetPath:Text = SUBSTITUTE ("&1/&2":U,
                                                             cProjectName,
                                                             cRelativePath) .
                END.
            END.
        END.

        IF THIS-OBJECT:BusinessEntityName MATCHES "*BusinessEntity":U THEN
            ASSIGN cInterfaceName = SUBSTRING (THIS-OBJECT:BusinessEntityName,
                                               1,
                                               LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 14,
                                               "CHARACTER":U) .
        ELSE IF THIS-OBJECT:BusinessEntityName MATCHES "*Entity":U THEN
            ASSIGN cInterfaceName = SUBSTRING (THIS-OBJECT:BusinessEntityName,
                                               1,
                                               LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 6,
                                               "CHARACTER":U) .
        ELSE
            ASSIGN cInterfaceName = THIS-OBJECT:BusinessEntityName .

        ASSIGN THIS-OBJECT:tbInterfaceClassName:Text = cInterfaceName
               THIS-OBJECT:tbURI:Text                = SUBSTITUTE ("&1":U,
                                                                   cInterfaceName)
               THIS-OBJECT:tbDescription:Text        = SUBSTITUTE ("Service Interface for &1"{&TRAN},
                                                                   THIS-OBJECT:BusinessEntityName)
               THIS-OBJECT:tbAppServerModule:Text    = "AppServer":U .

        {Consultingwerk/foreachPrimitiveList.i Character cTable in THIS-OBJECT:Tables}
            cbFindTable:ValueList:ValueListItems:Add (cTable, cTable) .
        END.

        IF THIS-OBJECT:Tables:Count > 0 THEN
            cbFindTable:Text = THIS-OBJECT:Tables:GetValue (1) .

        THIS-OBJECT:ValidateSettings () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbDatasetPath,
                 tbInterfaceClassName, tbInterfacePackage, tbURI, tbIncludeFilePath
                 and tbSourceFolder
        Notes:   Validates settings and enabled Generate button
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID RestInterfaceWizard_TextChanged (sender AS System.Object,
                                                         e AS System.EventArgs):

        THIS-OBJECT:ValidateSettings () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbInterfacePackage
                 text editor
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbInterfacePackage_EditorButtonClick (sender AS System.Object,
                                                              e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult        AS System.Windows.Forms.DialogResult                   NO-UNDO .
        DEFINE VARIABLE cRelative            AS CHARACTER                                           NO-UNDO .
        DEFINE VARIABLE oFolderBrowserDialog AS Consultingwerk.Windows.Util.Forms.FolderBrowserForm NO-UNDO .
        DEFINE VARIABLE cPath                AS CHARACTER                                           NO-UNDO .

        ASSIGN cPath = FileHelper:CombinePath (THIS-OBJECT:tbSourceFolder:Text,
                                               THIS-OBJECT:tbAppServerModule:Text) .

        FILE-INFORMATION:FILE-NAME = cPath .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN cPath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE DO:
            FILE-INFORMATION:FULL-PATHNAME = ".":U .
            ASSIGN cPath = FILE-INFORMATION:FULL-PATHNAME .
        END.

        oFolderBrowserDialog = NEW Consultingwerk.Windows.Util.Forms.FolderBrowserForm () .
        oFolderBrowserDialog:RootFolder = cPath .

        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO:

            ASSIGN cRelative = SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (cPath, "CHARACTER":U) + 2, -1, "CHARACTER":U)

            tbInterfacePackage:Text = REPLACE (cRelative, "~\":U, ".":U) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbDatasetPath
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object,
                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE cProjectName  AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cRelativePath AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cPath         AS CHARACTER                         NO-UNDO .

        WAIT-FOR openFileDialog1:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        ASSIGN cProjectName = OpenEdgeArchitectHelper:FindProjectNameForPath (openFileDialog1:FileName) .

        IF cProjectName > "":U THEN DO:

            ASSIGN cRelativePath = SUBSTRING (openFileDialog1:FileName,
                                              LENGTH (OpenEdgeArchitectHelper:FindProjectPath (openFileDialog1:FileName), "CHARACTER":U) + 2,
                                              -1,
                                              "CHARACTER":U)
                   cRelativePath = REPLACE (cRelativePath, "~\":U, "/":U)
                   tbDatasetPath:Text = SUBSTITUTE ("&1/&2":U,
                                                     cProjectName,
                                                     cRelativePath) .
        END.
        ELSE DO:
            tbDatasetPath:Text = FileHelper:RelativeFileName (openFileDialog1:FileName) .

            MessageFormHelper:ShowMessage ("Could not determine the Progress Developer Studio project name for the selected file.~n"{&TRAN} +
                                           "Please edit the file name, so that it matches the Eclipse resource name."{&TRAN}) .
        END.

        /* Enter Dataset Include file path */
        ASSIGN cPath = FileHelper:CombinePath (THIS-OBJECT:tbSourceFolder:Text,
                                               THIS-OBJECT:tbAppServerModule:Text) .

        FILE-INFORMATION:FILE-NAME = cPath .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN cPath = FILE-INFORMATION:FULL-PATHNAME .
        ELSE DO:
            FILE-INFORMATION:FULL-PATHNAME = ".":U .
            ASSIGN cPath = FILE-INFORMATION:FULL-PATHNAME .
        END.

        ASSIGN cRelativePath = SUBSTRING (openFileDialog1:FileName, LENGTH (cPath, "CHARACTER":U) + 2, -1, "CHARACTER":U) .

        IF cRelativePath > "":U THEN
            tbIncludeFilePath:Text = REPLACE (cRelativePath, "~\":U, "/":U) .
        ELSE
            tbIncludeFilePath:Text = REPLACE (FileHelper:RelativeFileName (openFileDialog1:FileName), "~\":U, "/":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbSourceFolder
        Notes:
        @param sender The reference to the object that raised the event
        @aram e The
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick_1 (sender AS System.Object,
                                                              e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        WAIT-FOR folderBrowserDialog1:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        tbSourceFolder:Text = folderBrowserDialog1:SelectedPath .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates settings and enabled btnGenerated
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ValidateSettings ():

        ASSIGN btnGenerate:Enabled = THIS-OBJECT:tbDatasetPath:Text > "":U AND
                                     THIS-OBJECT:tbInterfaceClassName:Text > "":U AND
                                     THIS-OBJECT:tbInterfacePackage:Text > "":U AND
                                     THIS-OBJECT:tbURI:Text > "":U AND
                                     THIS-OBJECT:tbIncludeFilePath:Text > "":U AND
                                     THIS-OBJECT:tbSourceFolder:Text > "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the RestInterfaceWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC RestInterfaceWizard ():

    END DESTRUCTOR .

END CLASS.
