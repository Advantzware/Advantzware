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
    File        : SmartTranslationViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 10.01.2013 15:02:58
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartTranslationViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLanguageGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslationScope_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslationScope AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslationKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslationKey AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOriginalString_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOriginalString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslatedString_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTranslatedString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTranslationViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTranslationViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            ASSIGN 
                THIS-OBJECT:ultraComboEditor1:ValueList = ValueLists:GetLanguageValueList ("":U, "<default>"{&TRAN}) 
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartTranslationViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartTranslation":U).
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
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbLanguageGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTranslationScope_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTranslationScope = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbTranslationKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTranslationKey = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbOriginalString_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbOriginalString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbTranslatedString_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbTranslatedString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTranslationScope, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTranslationKey, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbOriginalString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTranslatedString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = resources:GetString("smartBusinessEntityBindingSource1.BindTo":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = resources:GetString("smartBusinessEntityBindingSource1.EntityJoin":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = resources:GetString("smartBusinessEntityBindingSource1.EntityName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = resources:GetString("smartBusinessEntityBindingSource1.EntityTable":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityView = resources:GetString("smartBusinessEntityBindingSource1.EntityView":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:MapNullToEmptyString = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("TranslationGuid":U, "TranslationGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TranslationScope":U, "Scope":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("TranslationKey":U, "Key":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("OriginalString":U, "OriginalString":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("TranslatedString":U, "TranslatedString":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbLanguageGuid_Label */
        /*  */
        THIS-OBJECT:tbLanguageGuid_Label:AccessibleDescription = resources:GetString("tbLanguageGuid_Label.AccessibleDescription":U).
        THIS-OBJECT:tbLanguageGuid_Label:AccessibleName = resources:GetString("tbLanguageGuid_Label.AccessibleName":U).
        THIS-OBJECT:tbLanguageGuid_Label:Anchor = CAST(resources:GetObject("tbLanguageGuid_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:tbLanguageGuid_Label:Appearance = appearance1.
        THIS-OBJECT:tbLanguageGuid_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbLanguageGuid_Label.AutoSize":U)).
        THIS-OBJECT:tbLanguageGuid_Label:BackgroundImage = CAST(resources:GetObject("tbLanguageGuid_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbLanguageGuid_Label:BackgroundImageLayout = CAST(resources:GetObject("tbLanguageGuid_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbLanguageGuid_Label:Dock = CAST(resources:GetObject("tbLanguageGuid_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbLanguageGuid_Label:Font = CAST(resources:GetObject("tbLanguageGuid_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbLanguageGuid_Label:ImeMode = CAST(resources:GetObject("tbLanguageGuid_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbLanguageGuid_Label:Location = CAST(resources:GetObject("tbLanguageGuid_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbLanguageGuid_Label:MaximumSize = CAST(resources:GetObject("tbLanguageGuid_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbLanguageGuid_Label:Name = "tbLanguageGuid_Label":U.
        THIS-OBJECT:tbLanguageGuid_Label:RightToLeft = CAST(resources:GetObject("tbLanguageGuid_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbLanguageGuid_Label:Size = CAST(resources:GetObject("tbLanguageGuid_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbLanguageGuid_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("tbLanguageGuid_Label.TabIndex":U)).
        THIS-OBJECT:tbLanguageGuid_Label:Text = resources:GetString("tbLanguageGuid_Label.Text":U).
        /*  */
        /* tbTranslationScope_Label */
        /*  */
        THIS-OBJECT:tbTranslationScope_Label:AccessibleDescription = resources:GetString("tbTranslationScope_Label.AccessibleDescription":U).
        THIS-OBJECT:tbTranslationScope_Label:AccessibleName = resources:GetString("tbTranslationScope_Label.AccessibleName":U).
        THIS-OBJECT:tbTranslationScope_Label:Anchor = CAST(resources:GetObject("tbTranslationScope_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:tbTranslationScope_Label:Appearance = appearance2.
        THIS-OBJECT:tbTranslationScope_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslationScope_Label.AutoSize":U)).
        THIS-OBJECT:tbTranslationScope_Label:BackgroundImage = CAST(resources:GetObject("tbTranslationScope_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslationScope_Label:BackgroundImageLayout = CAST(resources:GetObject("tbTranslationScope_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslationScope_Label:Dock = CAST(resources:GetObject("tbTranslationScope_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslationScope_Label:Font = CAST(resources:GetObject("tbTranslationScope_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslationScope_Label:ImeMode = CAST(resources:GetObject("tbTranslationScope_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslationScope_Label:Location = CAST(resources:GetObject("tbTranslationScope_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslationScope_Label:MaximumSize = CAST(resources:GetObject("tbTranslationScope_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationScope_Label:Name = "tbTranslationScope_Label":U.
        THIS-OBJECT:tbTranslationScope_Label:RightToLeft = CAST(resources:GetObject("tbTranslationScope_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslationScope_Label:Size = CAST(resources:GetObject("tbTranslationScope_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationScope_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslationScope_Label.TabIndex":U)).
        THIS-OBJECT:tbTranslationScope_Label:Text = resources:GetString("tbTranslationScope_Label.Text":U).
        /*  */
        /* tbTranslationScope */
        /*  */
        THIS-OBJECT:tbTranslationScope:AccessibleDescription = resources:GetString("tbTranslationScope.AccessibleDescription":U).
        THIS-OBJECT:tbTranslationScope:AccessibleName = resources:GetString("tbTranslationScope.AccessibleName":U).
        THIS-OBJECT:tbTranslationScope:Anchor = CAST(resources:GetObject("tbTranslationScope.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTranslationScope:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslationScope.AutoSize":U)).
        THIS-OBJECT:tbTranslationScope:BackgroundImage = CAST(resources:GetObject("tbTranslationScope.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslationScope:BackgroundImageLayout = CAST(resources:GetObject("tbTranslationScope.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslationScope:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TranslationScope":U, TRUE)).
        THIS-OBJECT:tbTranslationScope:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTranslationScope:Dock = CAST(resources:GetObject("tbTranslationScope.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslationScope:Font = CAST(resources:GetObject("tbTranslationScope.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslationScope:ImeMode = CAST(resources:GetObject("tbTranslationScope.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslationScope:Location = CAST(resources:GetObject("tbTranslationScope.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslationScope:MaximumSize = CAST(resources:GetObject("tbTranslationScope.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationScope:Name = "tbTranslationScope":U.
        THIS-OBJECT:tbTranslationScope:RightToLeft = CAST(resources:GetObject("tbTranslationScope.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslationScope:Size = CAST(resources:GetObject("tbTranslationScope.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationScope:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslationScope.TabIndex":U)).
        /*  */
        /* tbTranslationKey_Label */
        /*  */
        THIS-OBJECT:tbTranslationKey_Label:AccessibleDescription = resources:GetString("tbTranslationKey_Label.AccessibleDescription":U).
        THIS-OBJECT:tbTranslationKey_Label:AccessibleName = resources:GetString("tbTranslationKey_Label.AccessibleName":U).
        THIS-OBJECT:tbTranslationKey_Label:Anchor = CAST(resources:GetObject("tbTranslationKey_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:tbTranslationKey_Label:Appearance = appearance3.
        THIS-OBJECT:tbTranslationKey_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslationKey_Label.AutoSize":U)).
        THIS-OBJECT:tbTranslationKey_Label:BackgroundImage = CAST(resources:GetObject("tbTranslationKey_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslationKey_Label:BackgroundImageLayout = CAST(resources:GetObject("tbTranslationKey_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslationKey_Label:Dock = CAST(resources:GetObject("tbTranslationKey_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslationKey_Label:Font = CAST(resources:GetObject("tbTranslationKey_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslationKey_Label:ImeMode = CAST(resources:GetObject("tbTranslationKey_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslationKey_Label:Location = CAST(resources:GetObject("tbTranslationKey_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslationKey_Label:MaximumSize = CAST(resources:GetObject("tbTranslationKey_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationKey_Label:Name = "tbTranslationKey_Label":U.
        THIS-OBJECT:tbTranslationKey_Label:RightToLeft = CAST(resources:GetObject("tbTranslationKey_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslationKey_Label:Size = CAST(resources:GetObject("tbTranslationKey_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationKey_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslationKey_Label.TabIndex":U)).
        THIS-OBJECT:tbTranslationKey_Label:Text = resources:GetString("tbTranslationKey_Label.Text":U).
        /*  */
        /* tbTranslationKey */
        /*  */
        THIS-OBJECT:tbTranslationKey:AccessibleDescription = resources:GetString("tbTranslationKey.AccessibleDescription":U).
        THIS-OBJECT:tbTranslationKey:AccessibleName = resources:GetString("tbTranslationKey.AccessibleName":U).
        THIS-OBJECT:tbTranslationKey:Anchor = CAST(resources:GetObject("tbTranslationKey.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTranslationKey:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslationKey.AutoSize":U)).
        THIS-OBJECT:tbTranslationKey:BackgroundImage = CAST(resources:GetObject("tbTranslationKey.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslationKey:BackgroundImageLayout = CAST(resources:GetObject("tbTranslationKey.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslationKey:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TranslationKey":U, TRUE)).
        THIS-OBJECT:tbTranslationKey:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTranslationKey:Dock = CAST(resources:GetObject("tbTranslationKey.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslationKey:Font = CAST(resources:GetObject("tbTranslationKey.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslationKey:ImeMode = CAST(resources:GetObject("tbTranslationKey.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslationKey:Location = CAST(resources:GetObject("tbTranslationKey.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslationKey:MaximumSize = CAST(resources:GetObject("tbTranslationKey.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationKey:Name = "tbTranslationKey":U.
        THIS-OBJECT:tbTranslationKey:NullText = resources:GetString("tbTranslationKey.NullText":U).
        THIS-OBJECT:tbTranslationKey:RightToLeft = CAST(resources:GetObject("tbTranslationKey.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslationKey:Size = CAST(resources:GetObject("tbTranslationKey.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslationKey:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslationKey.TabIndex":U)).
        /*  */
        /* tbOriginalString_Label */
        /*  */
        THIS-OBJECT:tbOriginalString_Label:AccessibleDescription = resources:GetString("tbOriginalString_Label.AccessibleDescription":U).
        THIS-OBJECT:tbOriginalString_Label:AccessibleName = resources:GetString("tbOriginalString_Label.AccessibleName":U).
        THIS-OBJECT:tbOriginalString_Label:Anchor = CAST(resources:GetObject("tbOriginalString_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:tbOriginalString_Label:Appearance = appearance4.
        THIS-OBJECT:tbOriginalString_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbOriginalString_Label.AutoSize":U)).
        THIS-OBJECT:tbOriginalString_Label:BackgroundImage = CAST(resources:GetObject("tbOriginalString_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbOriginalString_Label:BackgroundImageLayout = CAST(resources:GetObject("tbOriginalString_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbOriginalString_Label:Dock = CAST(resources:GetObject("tbOriginalString_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbOriginalString_Label:Font = CAST(resources:GetObject("tbOriginalString_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbOriginalString_Label:ImeMode = CAST(resources:GetObject("tbOriginalString_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbOriginalString_Label:Location = CAST(resources:GetObject("tbOriginalString_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbOriginalString_Label:MaximumSize = CAST(resources:GetObject("tbOriginalString_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbOriginalString_Label:Name = "tbOriginalString_Label":U.
        THIS-OBJECT:tbOriginalString_Label:RightToLeft = CAST(resources:GetObject("tbOriginalString_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbOriginalString_Label:Size = CAST(resources:GetObject("tbOriginalString_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbOriginalString_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("tbOriginalString_Label.TabIndex":U)).
        THIS-OBJECT:tbOriginalString_Label:Text = resources:GetString("tbOriginalString_Label.Text":U).
        /*  */
        /* tbOriginalString */
        /*  */
        THIS-OBJECT:tbOriginalString:AccessibleDescription = resources:GetString("tbOriginalString.AccessibleDescription":U).
        THIS-OBJECT:tbOriginalString:AccessibleName = resources:GetString("tbOriginalString.AccessibleName":U).
        THIS-OBJECT:tbOriginalString:Anchor = CAST(resources:GetObject("tbOriginalString.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbOriginalString:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbOriginalString.AutoSize":U)).
        THIS-OBJECT:tbOriginalString:BackgroundImage = CAST(resources:GetObject("tbOriginalString.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbOriginalString:BackgroundImageLayout = CAST(resources:GetObject("tbOriginalString.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbOriginalString:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "OriginalString":U, TRUE)).
        THIS-OBJECT:tbOriginalString:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbOriginalString:Dock = CAST(resources:GetObject("tbOriginalString.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbOriginalString:Font = CAST(resources:GetObject("tbOriginalString.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbOriginalString:ImeMode = CAST(resources:GetObject("tbOriginalString.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbOriginalString:Location = CAST(resources:GetObject("tbOriginalString.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbOriginalString:MaximumSize = CAST(resources:GetObject("tbOriginalString.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbOriginalString:Name = "tbOriginalString":U.
        THIS-OBJECT:tbOriginalString:NullText = resources:GetString("tbOriginalString.NullText":U).
        THIS-OBJECT:tbOriginalString:RightToLeft = CAST(resources:GetObject("tbOriginalString.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbOriginalString:Size = CAST(resources:GetObject("tbOriginalString.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbOriginalString:TabIndex = System.Convert:ToInt32(resources:GetObject("tbOriginalString.TabIndex":U)).
        /*  */
        /* tbTranslatedString_Label */
        /*  */
        THIS-OBJECT:tbTranslatedString_Label:AccessibleDescription = resources:GetString("tbTranslatedString_Label.AccessibleDescription":U).
        THIS-OBJECT:tbTranslatedString_Label:AccessibleName = resources:GetString("tbTranslatedString_Label.AccessibleName":U).
        THIS-OBJECT:tbTranslatedString_Label:Anchor = CAST(resources:GetObject("tbTranslatedString_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:tbTranslatedString_Label:Appearance = appearance5.
        THIS-OBJECT:tbTranslatedString_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslatedString_Label.AutoSize":U)).
        THIS-OBJECT:tbTranslatedString_Label:BackgroundImage = CAST(resources:GetObject("tbTranslatedString_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslatedString_Label:BackgroundImageLayout = CAST(resources:GetObject("tbTranslatedString_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslatedString_Label:Dock = CAST(resources:GetObject("tbTranslatedString_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslatedString_Label:Font = CAST(resources:GetObject("tbTranslatedString_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslatedString_Label:ImeMode = CAST(resources:GetObject("tbTranslatedString_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslatedString_Label:Location = CAST(resources:GetObject("tbTranslatedString_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslatedString_Label:MaximumSize = CAST(resources:GetObject("tbTranslatedString_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslatedString_Label:Name = "tbTranslatedString_Label":U.
        THIS-OBJECT:tbTranslatedString_Label:RightToLeft = CAST(resources:GetObject("tbTranslatedString_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslatedString_Label:Size = CAST(resources:GetObject("tbTranslatedString_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslatedString_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslatedString_Label.TabIndex":U)).
        THIS-OBJECT:tbTranslatedString_Label:Text = resources:GetString("tbTranslatedString_Label.Text":U).
        /*  */
        /* tbTranslatedString */
        /*  */
        THIS-OBJECT:tbTranslatedString:AccessibleDescription = resources:GetString("tbTranslatedString.AccessibleDescription":U).
        THIS-OBJECT:tbTranslatedString:AccessibleName = resources:GetString("tbTranslatedString.AccessibleName":U).
        THIS-OBJECT:tbTranslatedString:Anchor = CAST(resources:GetObject("tbTranslatedString.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTranslatedString:AutoSize = System.Convert:ToBoolean(resources:GetObject("tbTranslatedString.AutoSize":U)).
        THIS-OBJECT:tbTranslatedString:BackgroundImage = CAST(resources:GetObject("tbTranslatedString.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tbTranslatedString:BackgroundImageLayout = CAST(resources:GetObject("tbTranslatedString.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tbTranslatedString:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TranslatedString":U, TRUE)).
        THIS-OBJECT:tbTranslatedString:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbTranslatedString:Dock = CAST(resources:GetObject("tbTranslatedString.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tbTranslatedString:Font = CAST(resources:GetObject("tbTranslatedString.Font":U), System.Drawing.Font).
        THIS-OBJECT:tbTranslatedString:ImeMode = CAST(resources:GetObject("tbTranslatedString.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tbTranslatedString:Location = CAST(resources:GetObject("tbTranslatedString.Location":U), System.Drawing.Point).
        THIS-OBJECT:tbTranslatedString:MaximumSize = CAST(resources:GetObject("tbTranslatedString.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslatedString:Name = "tbTranslatedString":U.
        THIS-OBJECT:tbTranslatedString:RightToLeft = CAST(resources:GetObject("tbTranslatedString.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tbTranslatedString:Size = CAST(resources:GetObject("tbTranslatedString.Size":U), System.Drawing.Size).
        THIS-OBJECT:tbTranslatedString:TabIndex = System.Convert:ToInt32(resources:GetObject("tbTranslatedString.TabIndex":U)).
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:AccessibleDescription = resources:GetString("ultraComboEditor1.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor1:AccessibleName = resources:GetString("ultraComboEditor1.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(resources:GetObject("ultraComboEditor1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor1.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor1:BackgroundImage = CAST(resources:GetObject("ultraComboEditor1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor1:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LanguageGuid":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:Dock = CAST(resources:GetObject("ultraComboEditor1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Font = CAST(resources:GetObject("ultraComboEditor1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor1:ImeMode = CAST(resources:GetObject("ultraComboEditor1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor1:Location = CAST(resources:GetObject("ultraComboEditor1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor1:MaximumSize = CAST(resources:GetObject("ultraComboEditor1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:RightToLeft = CAST(resources:GetObject("ultraComboEditor1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor1:Size = CAST(resources:GetObject("ultraComboEditor1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor1.TabIndex":U)).
        /*  */
        /* SmartTranslationViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbLanguageGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslationScope_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslationScope).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslationKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslationKey).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbOriginalString_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbOriginalString).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslatedString_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTranslatedString).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartTranslationViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTranslationScope, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTranslationKey, System.ComponentModel.ISupportInitialize):EndInit().
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

    DESTRUCTOR PUBLIC SmartTranslationViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
