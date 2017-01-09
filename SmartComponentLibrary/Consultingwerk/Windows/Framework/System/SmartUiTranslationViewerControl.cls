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
    File        : SmartUiTranslationViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:42:47
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartUiTranslationViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLanguageGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbObjectName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbObjectName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbResourceKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbResourceKey AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOriginalString_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOriginalString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslatedString_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslatedString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartUiTranslationViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartUiTranslationViewerControl ():
                
        SUPER().
        InitializeComponent().
       
        IF NOT THIS-OBJECT:DesignTime THEN 
            ASSIGN 
                THIS-OBJECT:ultraComboEditor1:ValueList = ValueLists:GetLanguageValueList () 
                    .
        
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartUiTranslation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartUiTranslationViewerControl":U).
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
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbLanguageGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbObjectName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbObjectName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbResourceKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbResourceKey = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbOriginalString_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbOriginalString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbTranslatedString_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTranslatedString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbResourceKey, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbOriginalString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTranslatedString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartUiTranslation":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("UiTranslationGuid":U, "UiTranslationGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Container Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ResourceKey":U, "Resource Key":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("OriginalString":U, "OriginalString":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("TranslatedString":U, "TranslatedString":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbLanguageGuid_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbLanguageGuid_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:tbLanguageGuid_Label, "tbLanguageGuid_Label":U).
        THIS-OBJECT:tbLanguageGuid_Label:Name = "tbLanguageGuid_Label":U.
        /*  */
        /* tbObjectName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbObjectName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbObjectName_Label, "tbObjectName_Label":U).
        THIS-OBJECT:tbObjectName_Label:Name = "tbObjectName_Label":U.
        /*  */
        /* tbObjectName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbObjectName, "tbObjectName":U).
        THIS-OBJECT:tbObjectName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectName":U, TRUE)).
        THIS-OBJECT:tbObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbObjectName:Name = "tbObjectName":U.
        /*  */
        /* tbResourceKey_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbResourceKey_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:tbResourceKey_Label, "tbResourceKey_Label":U).
        THIS-OBJECT:tbResourceKey_Label:Name = "tbResourceKey_Label":U.
        /*  */
        /* tbResourceKey */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbResourceKey, "tbResourceKey":U).
        THIS-OBJECT:tbResourceKey:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ResourceKey":U, TRUE)).
        THIS-OBJECT:tbResourceKey:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbResourceKey:Name = "tbResourceKey":U.
        /*  */
        /* tbOriginalString_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbOriginalString_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:tbOriginalString_Label, "tbOriginalString_Label":U).
        THIS-OBJECT:tbOriginalString_Label:Name = "tbOriginalString_Label":U.
        /*  */
        /* tbOriginalString */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbOriginalString, "tbOriginalString":U).
        THIS-OBJECT:tbOriginalString:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "OriginalString":U, TRUE)).
        THIS-OBJECT:tbOriginalString:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbOriginalString:Name = "tbOriginalString":U.
        /*  */
        /* tbTranslatedString_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbTranslatedString_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:tbTranslatedString_Label, "tbTranslatedString_Label":U).
        THIS-OBJECT:tbTranslatedString_Label:Name = "tbTranslatedString_Label":U.
        /*  */
        /* tbTranslatedString */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbTranslatedString, "tbTranslatedString":U).
        THIS-OBJECT:tbTranslatedString:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TranslatedString":U, TRUE)).
        THIS-OBJECT:tbTranslatedString:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTranslatedString:Name = "tbTranslatedString":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LanguageGuid":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        /*  */
        /* SmartUiTranslationViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbLanguageGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbObjectName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbObjectName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbResourceKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbResourceKey).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbOriginalString_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbOriginalString).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslatedString_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslatedString).
        THIS-OBJECT:Name = "SmartUiTranslationViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbResourceKey, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbOriginalString, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTranslatedString, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartUiTranslationViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartUiTranslationViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
