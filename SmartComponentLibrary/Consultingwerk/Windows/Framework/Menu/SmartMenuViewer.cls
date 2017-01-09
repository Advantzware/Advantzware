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
    File        : SmartMenuViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 02:00:01 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*                 FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartViewerControl FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                  FROM PROPATH .
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*                FROM PROPATH .  
USING Infragistics.Win.UltraWinEditors.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                        FROM PROPATH .
USING System.Windows.Forms.*                                 FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Menu.SmartMenuViewer 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                           AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource                                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuStructureType_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuSmallImage_Label    AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuSmallImage          AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuName_Label          AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuName                AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuLargeImage_Label    AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuLargeImage          AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource_eSmartMenu_MenuBeginsAGroup        AS Infragistics.Win.UltraWinEditors.UltraCheckEditor                              NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1                                      AS System.Windows.Forms.OpenFileDialog                                            NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartComboEditor1 AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE StructureTypeComboEditor AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    
    DEFINE PRIVATE VARIABLE oStructureTypeValueList AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    
    CONSTRUCTOR PUBLIC SmartMenuViewer ():

        SUPER ().
        InitializeComponent ().
        
        PopulateComboEditor ().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Override of the AddRecord Method to initialize a new Record with
                 the StructureType - MENU.
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID AddRecord ():
        
        SUPER:AddRecord().
        
        StructureTypeComboEditor:Value = BOX ("Menu":U).
    
    END METHOD .

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("SelectSmallImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("SelectLargeImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:MenuBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:StructureTypeComboEditor = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:MenuBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:StructureTypeComboEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* MenuBindingSource */
        /*  */
        THIS-OBJECT:MenuBindingSource:AddFields = FALSE.
        THIS-OBJECT:MenuBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:MenuBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:MenuBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:MenuBindingSource:EntityTable = "eSmartMenu":U.
        THIS-OBJECT:MenuBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:MenuBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:MenuBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 12 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("MenuGuid":U, "MenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ParentMenuGuid":U, "ParentMenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("MenuName":U, "MenuName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("MenuStructureType":U, "MenuStructureType":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("MenuBeginsAGroup":U, "MenuBeginsAGroup":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("MenuSmallImage":U, "MenuSmallImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("MenuLargeImage":U, "MenuLargeImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("MenuSequence":U, "MenuSequence":U, Progress.Data.DataType:INTEGER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("FunctionGuid":U, "FunctionGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("HasChild":U, "HasChild":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("FunctionName":U, "FunctionName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("MenuStyleGuid":U, "MenuStyleGuid":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:MenuBindingSource:TableSchema = tableDesc1.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:Appearance = appearance1.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:AutoSize = TRUE.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:Location = NEW System.Drawing.Point(13, 18).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:Name = "MenuBindingSource_eSmartMenu_MenuName_Label":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:TabIndex = 0.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label:Text = "&Name:":U.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "MenuName":U, TRUE)).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:Location = NEW System.Drawing.Point(123, 14).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:Name = "MenuBindingSource_eSmartMenu_MenuName":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName:TabIndex = 1.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuStructureType_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:Appearance = appearance2.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:AutoSize = TRUE.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:Location = NEW System.Drawing.Point(13, 124).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:Name = "MenuBindingSource_eSmartMenu_MenuStructureType_Label":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:Size = NEW System.Drawing.Size(81, 14).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:TabIndex = 7.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label:Text = "&Structure Type:":U.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuBeginsAGroup */
        /*  */
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:MenuBindingSource, "MenuBeginsAGroup":U, TRUE)).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:Location = NEW System.Drawing.Point(123, 41).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:Name = "MenuBindingSource_eSmartMenu_MenuBeginsAGroup":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:TabIndex = 2.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup:Text = "Begins a Group":U.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuSmallImage_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:Appearance = appearance3.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:AutoSize = TRUE.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:Location = NEW System.Drawing.Point(13, 71).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:Name = "MenuBindingSource_eSmartMenu_MenuSmallImage_Label":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:Size = NEW System.Drawing.Size(70, 14).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:TabIndex = 3.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label:Text = "&Small Image:":U.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuSmallImage */
        /*  */
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:AllowDrop = TRUE.
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "SelectSmallImage":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "MenuSmallImage":U, TRUE)).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:Location = NEW System.Drawing.Point(123, 67).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:Name = "MenuBindingSource_eSmartMenu_MenuSmallImage":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:TabIndex = 4.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:EditorButtonClick:Subscribe(THIS-OBJECT:ImageSelection_EditorButtonClick).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:KeyDown:Subscribe(THIS-OBJECT:ImageSelection_KeyDownHandler).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuLargeImage_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:Appearance = appearance4.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:AutoSize = TRUE.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:Location = NEW System.Drawing.Point(13, 97).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:Name = "MenuBindingSource_eSmartMenu_MenuLargeImage_Label":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:Size = NEW System.Drawing.Size(71, 14).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:TabIndex = 5.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label:Text = "&Large Image:":U.
        /*  */
        /* MenuBindingSource_eSmartMenu_MenuLargeImage */
        /*  */
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:AllowDrop = TRUE.
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Key = "SelectLargeImage":U.
        editorButton2:Text = "...":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "MenuLargeImage":U, TRUE)).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:Location = NEW System.Drawing.Point(123, 93).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:Name = "MenuBindingSource_eSmartMenu_MenuLargeImage":U.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:TabIndex = 6.
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:EditorButtonClick:Subscribe(THIS-OBJECT:ImageSelection_EditorButtonClick).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:KeyDown:Subscribe(THIS-OBJECT:ImageSelection_KeyDownHandler).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "PNG|*.png|JPG|*.jpg|BMP|*.bmp|All files|*.*":U.
        THIS-OBJECT:openFileDialog1:FileName = "":U.
        /*  */
        /* StructureTypeComboEditor */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:StructureTypeComboEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:StructureTypeComboEditor:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "MenuStructureType":U, TRUE)).
        THIS-OBJECT:StructureTypeComboEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:StructureTypeComboEditor:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:StructureTypeComboEditor:Location = NEW System.Drawing.Point(123, 120).
        THIS-OBJECT:StructureTypeComboEditor:Name = "StructureTypeComboEditor":U.
        THIS-OBJECT:StructureTypeComboEditor:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:StructureTypeComboEditor:TabIndex = 8.
        THIS-OBJECT:StructureTypeComboEditor:ValueChanged:Subscribe(THIS-OBJECT:StructureTypeComboEditor_ValueChanged).
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton3:Key = "ShowLookupDialog":U.
        editorButton3:Text = "...":U.
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:MenuBindingSource, "FunctionGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "FunctionName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:Enabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Location = NEW System.Drawing.Point(123, 174).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "FunctionName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "MenuBindingSource_eSmartMenu_MenuLargeImage,MenuBindingSource_eSmartMenu_MenuName,MenuBindingSource_eSmartMenu_MenuSmallImage":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "FunctionName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "FOR EACH eSmartFunction":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogTitle = "Function Lookup":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartFunction":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "FunctionLargeImage,FunctionName,FunctionSmallImage":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "FunctionName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "FunctionGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartFunctionl WHERE eSmartFunction.FunctionGuid = '&1'":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:smartBusinessEntityLookup1:TabIndex = 10.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance5.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(13, 178).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(84, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 9.
        THIS-OBJECT:ultraLabel1:Text = "&Function Name:":U.
        /*  */
        /* smartComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:MenuBindingSource, "MenuStyleGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DisplayFields = "MenuStyleCode,MenuStyleDescription":U.
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DisplaySubstitute = "&1 - &2":U.
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:EntityName = "Consultingwerk.SmartFramework.Menu.MenuStyleBusinessEntity":U.
        THIS-OBJECT:smartComboEditor1:EntityTable = "eSmartMenuStyle":U.
        THIS-OBJECT:smartComboEditor1:FlagText = "Default":U.
        THIS-OBJECT:smartComboEditor1:KeyField = "MenuStyleGuid":U.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Location = NEW System.Drawing.Point(123, 147).
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        THIS-OBJECT:smartComboEditor1:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:smartComboEditor1:TabIndex = 11.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(13, 151).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 12.
        THIS-OBJECT:ultraLabel2:Text = "Menu Style:":U.
        /*  */
        /* SmartMenuViewer */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:StructureTypeComboEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuStructureType_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage).
        THIS-OBJECT:Name = "SmartMenuViewer":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(330, 230).
        CAST(THIS-OBJECT:MenuBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuBeginsAGroup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuSmallImage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuBindingSource_eSmartMenu_MenuLargeImage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:StructureTypeComboEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the OpenFileDialog for the ImageEditors to select a path
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_KeyDownHandler (sender AS System.Object, 
                                                       e AS KeyEventArgs):

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) THEN  

            THIS-OBJECT:ImageSelection_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Displays the OpenFileDialog for the ImageEditors to select a path
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_EditorButtonClick (sender AS System.Object, 
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

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData . 

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).

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
        Purpose: Populates the ValueList of the ComboEditor
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateComboEditor ():
        DEFINE VARIABLE oCharacterList AS CharacterList NO-UNDO.
        
        oCharacterList = NEW CharacterList () .
        oCharacterList:Add ("Menu":U) .
        oCharacterList:Add ("Item":U) .

        oStructureTypeValueList = Consultingwerk.Util.ValueListHelper:FromList (oCharacterList).
        
        StructureTypeComboEditor:ValueList = oStructureTypeValueList.
        
        StructureTypeComboEditor:Value = BOX ("Menu":U).
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (oCharacterList).
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Setting the FunctionLookup enabled when adding a new Item manually
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID StructureTypeComboEditor_ValueChanged (sender AS System.Object, 
	                                                           e      AS System.EventArgs):
		
		IF UNBOX (StructureTypeComboEditor:Value) = "Item":U THEN
		    THIS-OBJECT:SetControlEnabled (smartComboEditor1,
		                                   Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Never) .
		ELSE DO:
            THIS-OBJECT:SetControlEnabled (smartComboEditor1,
                                           Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Update) .
            smartComboEditor1:Enabled = CAST (sender, System.Windows.Forms.Control):Enabled  .                              
        END.                                           
		
		
		IF NOT THIS-OBJECT:AddingRecord THEN RETURN.
		
		smartBusinessEntityLookup1:Enabled = (UNBOX (StructureTypeComboEditor:Value) = "Item":U). 

	END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartMenuViewer class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenuViewer ():

    END DESTRUCTOR .

END CLASS.
