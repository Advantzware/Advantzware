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

    DEFINE PRIVATE VARIABLE cCurrentLanguageGuid AS CHARACTER NO-UNDO.
    
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

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
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
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:AutoSize = TRUE.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Location = NEW System.Drawing.Point(11, 45).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Name = "SmartLanguageBindingSource_eSmartLanguages_Name_Label":U.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:TabIndex = 2.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Name_Label:Text = "&Name:":U.
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:LanguageBindingSource, "LanguageName":U, TRUE)).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:Location = NEW System.Drawing.Point(121, 41).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:Name = "LanguageBindingSource_eSmartLanguage_LanguageName":U.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:Size = NEW System.Drawing.Size(216, 21).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageName:TabIndex = 3.
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
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:AutoSize = TRUE.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Location = NEW System.Drawing.Point(11, 72).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Name = "SmartLanguageBindingSource_eSmartLanguages_Image_Label":U.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Size = NEW System.Drawing.Size(39, 14).
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:TabIndex = 4.
        THIS-OBJECT:SmartLanguageBindingSource_eSmartLanguages_Image_Label:Text = "&Image:":U.
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
        THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(351, 212).
        THIS-OBJECT:ultraPanel1:TabIndex = 5.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownListWidth = 200.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "AllCultures":U.
        valueListItem1:DisplayText = "All Cultures":U.
        valueListItem2:DataValue = "InstalledWin32Cultures":U.
        valueListItem2:DisplayText = "Installed Win32 Cultures":U.
        valueListItem3:DataValue = "NeutralCultures":U.
        valueListItem3:DisplayText = "Neutral Cultures":U.
        valueListItem4:DataValue = "ReplacementCultures":U.
        valueListItem4:DisplayText = "Replacement Cultures":U.
        valueListItem5:DataValue = "SpecificCultures":U.
        valueListItem5:DisplayText = "Specific Cultures":U.
        valueListItem6:DataValue = "UserCustomCulture":U.
        valueListItem6:DisplayText = "User Custom Culture":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        arrayvar1[3] = valueListItem3.
        arrayvar1[4] = valueListItem4.
        arrayvar1[5] = valueListItem5.
        arrayvar1[6] = valueListItem6.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(243, 14).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(94, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 6.
        THIS-OBJECT:ultraComboEditor1:Text = "NeutralCultures":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "ShowLookupDialog":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "LanguageIsoCode":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:Location = NEW System.Drawing.Point(121, 15).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "Name,DisplayName,EnglishName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "LanguageBindingSource_eSmartLanguage_LanguageName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "Name,DisplayName,EnglishName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS,BEGINS,BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogTitle = "Lookup UI Culture / Language":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.Windows.Framework.Localization.UserInterfaceCultureBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eUserInterfaceCulture":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "DisplayName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "Name":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "for each eUserInterfaceCulture where eUserInterfaceCulture.Name begins ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Margin = NEW System.Windows.Forms.Padding(2).
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:Size = NEW System.Drawing.Size(117, 21).
        THIS-OBJECT:smartBusinessEntityLookup1:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityLookup1:BeforePerformLookup:Subscribe(THIS-OBJECT:BeforePerformLookupHandler).
        THIS-OBJECT:smartBusinessEntityLookup1:ShowLookupDialog:Subscribe(THIS-OBJECT:ShowLookupDialogHandler).
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Appearance = appearance3.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:AutoSize = TRUE.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Location = NEW System.Drawing.Point(11, 18).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Name = "LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label":U.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Size = NEW System.Drawing.Size(58, 14).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:TabIndex = 0.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageIsoCode_Label:Text = "&ISO-Code:":U.
        /*  */
        /* LanguageBindingSource_eSmartLanguage_LanguageImage */
        /*  */
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:AllowDrop = TRUE.
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Key = "SelectLanguageImage":U.
        editorButton2:Text = "...":U.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "LanguageImage":U, TRUE)).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:Location = NEW System.Drawing.Point(121, 68).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:Name = "LanguageBindingSource_eSmartLanguage_LanguageImage":U.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:Size = NEW System.Drawing.Size(216, 21).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:TabIndex = 5.
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:EditorButtonClick:Subscribe(THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage_EditorButtonClick).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        THIS-OBJECT:LanguageBindingSource_eSmartLanguage_LanguageImage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* eSmartLanguage_RcodeLanguage_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Location = NEW System.Drawing.Point(11, 99).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Name = "eSmartLanguage_RcodeLanguage_Label":U.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Size = NEW System.Drawing.Size(100, 14).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:TabIndex = 2.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage_Label:Text = "&R-Code Language:":U.
        /*  */
        /* eSmartLanguage_RcodeLanguage */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LanguageBindingSource, "RcodeLanguage":U, TRUE)).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:Location = NEW System.Drawing.Point(121, 95).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:Name = "eSmartLanguage_RcodeLanguage":U.
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:Size = NEW System.Drawing.Size(216, 21).
        THIS-OBJECT:eSmartLanguage_RcodeLanguage:TabIndex = 3.
        /*  */
        /* SmartLanguageViewer */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Margin = NEW System.Windows.Forms.Padding(4).
        THIS-OBJECT:Name = "SmartLanguageViewer":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(351, 212).
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
        
        IF NOT oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN RETURN.
        
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
