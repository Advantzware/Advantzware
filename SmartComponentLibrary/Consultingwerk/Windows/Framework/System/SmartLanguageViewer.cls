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
    File        : SmartLanguageViewer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Oct 10 18:04:26 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartViewerControl FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*        FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*              FROM PROPATH .
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartLanguageViewer
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components                                             AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLanguage_RcodeLanguage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLanguage_RcodeLanguage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageBindingSource_eSmartLanguage_LanguageImage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageBindingSource_eSmartLanguage_LanguageName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1                                            AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartLanguageBindingSource_eSmartLanguages_Name_Label  AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartLanguageBindingSource_eSmartLanguages_Image_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.

    CONSTRUCTOR PUBLIC SmartLanguageViewer ():
        SUPER ().
        InitializeComponent ().

        THIS-OBJECT:smartBusinessEntityLookup1:AfterInitializeBusinessEntityAdapter:Subscribe (AfterInitializeBusinessEntityAdapterHandler) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterInitializeBusinessEntityAdapterHandler
                 event of the culture lookup
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterInitializeBusinessEntityAdapterHandler (sender AS System.Object,
                                                                       e AS AfterInitializeBusinessEntityAdapterEventArgs):

        e:BusinessEntityAdapter:AppServerPartition = "Local":U .

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartLanguageViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartLanguage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem4 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem4 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem5 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem5 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem6 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem6 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("SelectLanguageImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LanguageBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLanguage_RcodeLanguage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        CAST(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LanguageBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLanguage_RcodeLanguage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* SmartLanguageBindingSource_eSmartLanguages_Name_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label, "SmartLanguageBindingSource_eSmartLanguages_Name_Label":U).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Name = "SmartLanguageBindingSource_eSmartLanguages_Name_Label":U.
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName, "LanguageBindingSource_eSmartLanguage_LanguageName":U).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:LanguageBindingSource, "LanguageName":U, TRUE)).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:Name = "LanguageBindingSource_eSmartLanguage_LanguageName":U.
        /*  */
        /* LanguageBindingSource */
        /*  */
        THIS-OBJECT:LanguageBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:LanguageBindingSource:EntityName = "Consultingwerk.SmartFramework.System.LanguageBusinessEntity":U.
        THIS-OBJECT:LanguageBindingSource:EntityTable = "eSmartLanguage":U.
        THIS-OBJECT:LanguageBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:LanguageBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("LanguageName":U, "LanguageName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("LanguageImage":U, "LanguageImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LanguageIsoCode":U, "ISO-Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("RcodeLanguage":U, "RCode Language":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:LanguageBindingSource:TableSchema = tableDesc1.
        /*  */
        /* SmartLanguageBindingSource_eSmartLanguages_Image_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label, "SmartLanguageBindingSource_eSmartLanguages_Image_Label":U).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Name = "SmartLanguageBindingSource_eSmartLanguages_Image_Label":U.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartLanguage_RcodeLanguage).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownListWidth = 200.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "AllCultures":U.
        resources:ApplyResources(valueListItem1, "valueListItem1":U).
        valueListItem1:ForceApplyResources = "":U.
        valueListItem2:DataValue = "InstalledWin32Cultures":U.
        resources:ApplyResources(valueListItem2, "valueListItem2":U).
        valueListItem2:ForceApplyResources = "":U.
        valueListItem3:DataValue = "NeutralCultures":U.
        resources:ApplyResources(valueListItem3, "valueListItem3":U).
        valueListItem3:ForceApplyResources = "":U.
        valueListItem4:DataValue = "ReplacementCultures":U.
        resources:ApplyResources(valueListItem4, "valueListItem4":U).
        valueListItem4:ForceApplyResources = "":U.
        valueListItem5:DataValue = "SpecificCultures":U.
        resources:ApplyResources(valueListItem5, "valueListItem5":U).
        valueListItem5:ForceApplyResources = "":U.
        valueListItem6:DataValue = "UserCustomCulture":U.
        resources:ApplyResources(valueListItem6, "valueListItem6":U).
        valueListItem6:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        arrayvar1[3] = valueListItem3.
        arrayvar1[4] = valueListItem4.
        arrayvar1[5] = valueListItem5.
        arrayvar1[6] = valueListItem6.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "LanguageIsoCode":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "Name,DisplayName,EnglishName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "LanguageBindingSource_eSmartLanguage_LanguageName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "Name,DisplayName,EnglishName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS,BEGINS,BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.Windows.Framework.Localization.UserInterfaceCultureBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eUserInterfaceCulture":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "DisplayName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "Name":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "for each eUserInterfaceCulture where eUserInterfaceCulture.Name begins ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:BeforePerformLookup:Subscribe(THIS-OBJECT:BeforePerformLookupHandler).
        THIS-OBJECT:smartBusinessEntityLookup1:ShowLookupDialog:Subscribe(THIS-OBJECT:ShowLookupDialogHandler).
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label, "LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label":U).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Name = "LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label":U.
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageImage */
        /*  */
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage, "LanguageBindingSource_eSmartLanguage_LanguageImage":U).
        editorButton2:Key = "SelectLanguageImage":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "LanguageImage":U, TRUE)).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:Name = "LanguageBindingSource_eSmartLanguage_LanguageImage":U.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:EditorButtonClick:Subscribe(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage_EditorButtonClick).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* eSmartLanguage_RcodeLanguage_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label, "eSmartLanguage_RcodeLanguage_Label":U).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Name = "eSmartLanguage_RcodeLanguage_Label":U.
        /*  */
        /* eSmartLanguage_RcodeLanguage */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartLanguage_RcodeLanguage, "eSmartLanguage_RcodeLanguage":U).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "RcodeLanguage":U, TRUE)).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:Name = "eSmartLanguage_RcodeLanguage":U.
        /*  */
        /* SmartLanguageViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "SmartLanguageViewer":U.
        CAST(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LanguageBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLanguage_RcodeLanguage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
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
        Purpose: Invokes a FileOpenDialog to select an Image
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LanguageBindingSource_eSmartLanguage_LanguageImage_EditorButtonClick (sender AS System.Object,
                                                                                              e      AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        DEFINE VARIABLE oDialogResult      AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE cShortenedFileName AS CHARACTER                         NO-UNDO.

        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult.

        IF NOT oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN
            RETURN.

        ASSIGN cShortenedFileName = Consultingwerk.Util.FileHelper:RelativeFileName (openFileDialog1:FileName).

        CAST (sender, System.Windows.Forms.Control):Text = cShortenedFileName.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assignes the filename of the droped file to the ImageEditors
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragDrop (sender AS System.Object,
                                                 e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.

        IF NOT Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN
            RETURN.

        cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).

        IF EXTENT (cFileNames) > 0 THEN
            CAST (sender, System.Windows.Forms.Control):Text = Consultingwerk.Util.FileHelper:RelativeFileName (cFileNames[1]).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allowes the FileDrop to the editor control if the events data
                 contains exact one file.
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragEnter (sender AS System.Object,
                                                  e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.

        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).

            IF EXTENT (cFileNames) = 1 THEN
                e:Effect = System.Windows.Forms.DragDropEffects:Copy.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ShowLookupDialog event of the language lookup
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ShowLookupDialogHandler (sender AS System.Object,
                                                 e AS System.ComponentModel.CancelEventArgs):

        smartBusinessEntityLookup1:LookupDialogQueryString = SUBSTITUTE ("for each eUserInterfaceCulture where eUserInterfaceCulture.CultureType = &1":U,
                                                                         QUOTER (UNBOX (ultraComboEditor1:Value))) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforePerformLookup event of the language lookup
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BeforePerformLookupHandler (INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs):

        smartBusinessEntityLookup1:LookupQueryString = SUBSTITUTE ("for each eUserInterfaceCulture where eUserInterfaceCulture.Name begins ~"&&1~" and eUserInterfaceCulture.CultureType = &1":U,
                                                                         QUOTER (UNBOX (ultraComboEditor1:Value))) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartLanguageViewer class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartLanguageViewer ():

    END DESTRUCTOR .

END CLASS.
