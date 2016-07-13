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
    File        : SmartMessageViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 09.01.2013 08:04:07
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

CLASS Consultingwerk.Windows.Framework.System.SmartMessageViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageGroup_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageGroup AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageNumber_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageNumber AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLanguageGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageText_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageText AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageDetail_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageDetail AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMessageType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor2 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMessageViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMessageViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            ASSIGN 
                THIS-OBJECT:ultraComboEditor1:ValueList = ValueLists:GetLanguageValueList ("":U, "<default>"{&TRAN}) 
                THIS-OBJECT:ultraComboEditor2:ValueList = ValueLists:GetMessageTypeValueList ("":U, "<unspecified>"{&TRAN}) .
        
    END CONSTRUCTOR.

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartMessage":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbMessageGroup_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMessageGroup = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbMessageNumber_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMessageNumber = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:tbLanguageGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMessageText_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMessageText = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbMessageDetail_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMessageDetail = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbMessageType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraComboEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMessageGroup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMessageNumber, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMessageText, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMessageDetail, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.MessageBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartMessage":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("MessageGuid":U, "MessageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("MessageGroup":U, "Message Group":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("MessageNumber":U, "MessageNumber":U, Progress.Data.DataType:INTEGER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("MessageText":U, "MessageText":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("MessageDetail":U, "MessageDetail":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("MessageType":U, "MessageType":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbMessageGroup_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageGroup_Label:Appearance = appearance1.
        THIS-OBJECT:tbMessageGroup_Label:AutoSize = TRUE.
        THIS-OBJECT:tbMessageGroup_Label:Location = NEW System.Drawing.Point(20, 24).
        THIS-OBJECT:tbMessageGroup_Label:Name = "tbMessageGroup_Label":U.
        THIS-OBJECT:tbMessageGroup_Label:Size = NEW System.Drawing.Size(88, 14).
        THIS-OBJECT:tbMessageGroup_Label:TabIndex = 0.
        THIS-OBJECT:tbMessageGroup_Label:Text = "Message Group:":U.
        /*  */
        /* tbMessageGroup */
        /*  */
        THIS-OBJECT:tbMessageGroup:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageGroup":U, TRUE)).
        THIS-OBJECT:tbMessageGroup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageGroup:Location = NEW System.Drawing.Point(130, 20).
        THIS-OBJECT:tbMessageGroup:Name = "tbMessageGroup":U.
        THIS-OBJECT:tbMessageGroup:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:tbMessageGroup:TabIndex = 1.
        /*  */
        /* tbMessageNumber_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageNumber_Label:Appearance = appearance2.
        THIS-OBJECT:tbMessageNumber_Label:AutoSize = TRUE.
        THIS-OBJECT:tbMessageNumber_Label:Location = NEW System.Drawing.Point(300, 24).
        THIS-OBJECT:tbMessageNumber_Label:Name = "tbMessageNumber_Label":U.
        THIS-OBJECT:tbMessageNumber_Label:Size = NEW System.Drawing.Size(48, 14).
        THIS-OBJECT:tbMessageNumber_Label:TabIndex = 2.
        THIS-OBJECT:tbMessageNumber_Label:Text = "Number:":U.
        /*  */
        /* tbMessageNumber */
        /*  */
        THIS-OBJECT:tbMessageNumber:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageNumber":U, TRUE)).
        THIS-OBJECT:tbMessageNumber:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageNumber:Location = NEW System.Drawing.Point(356, 20).
        THIS-OBJECT:tbMessageNumber:Name = "tbMessageNumber":U.
        THIS-OBJECT:tbMessageNumber:Size = NEW System.Drawing.Size(134, 21).
        THIS-OBJECT:tbMessageNumber:TabIndex = 3.
        /*  */
        /* tbLanguageGuid_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbLanguageGuid_Label:Appearance = appearance3.
        THIS-OBJECT:tbLanguageGuid_Label:AutoSize = TRUE.
        THIS-OBJECT:tbLanguageGuid_Label:Location = NEW System.Drawing.Point(20, 51).
        THIS-OBJECT:tbLanguageGuid_Label:Name = "tbLanguageGuid_Label":U.
        THIS-OBJECT:tbLanguageGuid_Label:Size = NEW System.Drawing.Size(58, 14).
        THIS-OBJECT:tbLanguageGuid_Label:TabIndex = 4.
        THIS-OBJECT:tbLanguageGuid_Label:Text = "Language:":U.
        /*  */
        /* tbMessageText_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageText_Label:Appearance = appearance4.
        THIS-OBJECT:tbMessageText_Label:AutoSize = TRUE.
        THIS-OBJECT:tbMessageText_Label:Location = NEW System.Drawing.Point(20, 78).
        THIS-OBJECT:tbMessageText_Label:Name = "tbMessageText_Label":U.
        THIS-OBJECT:tbMessageText_Label:Size = NEW System.Drawing.Size(29, 14).
        THIS-OBJECT:tbMessageText_Label:TabIndex = 8.
        THIS-OBJECT:tbMessageText_Label:Text = "Text:":U.
        /*  */
        /* tbMessageText */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMessageText:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMessageText:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageText":U, TRUE)).
        THIS-OBJECT:tbMessageText:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageText:Location = NEW System.Drawing.Point(130, 74).
        THIS-OBJECT:tbMessageText:Multiline = TRUE.
        THIS-OBJECT:tbMessageText:Name = "tbMessageText":U.
        THIS-OBJECT:tbMessageText:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:tbMessageText:Size = NEW System.Drawing.Size(360, 57).
        THIS-OBJECT:tbMessageText:TabIndex = 9.
        /*  */
        /* tbMessageDetail_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageDetail_Label:Appearance = appearance5.
        THIS-OBJECT:tbMessageDetail_Label:AutoSize = TRUE.
        THIS-OBJECT:tbMessageDetail_Label:Location = NEW System.Drawing.Point(20, 139).
        THIS-OBJECT:tbMessageDetail_Label:Name = "tbMessageDetail_Label":U.
        THIS-OBJECT:tbMessageDetail_Label:Size = NEW System.Drawing.Size(36, 14).
        THIS-OBJECT:tbMessageDetail_Label:TabIndex = 10.
        THIS-OBJECT:tbMessageDetail_Label:Text = "Detail:":U.
        /*  */
        /* tbMessageDetail */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMessageDetail:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMessageDetail:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageDetail":U, TRUE)).
        THIS-OBJECT:tbMessageDetail:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageDetail:Location = NEW System.Drawing.Point(130, 137).
        THIS-OBJECT:tbMessageDetail:Multiline = TRUE.
        THIS-OBJECT:tbMessageDetail:Name = "tbMessageDetail":U.
        THIS-OBJECT:tbMessageDetail:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:tbMessageDetail:Size = NEW System.Drawing.Size(360, 165).
        THIS-OBJECT:tbMessageDetail:TabIndex = 11.
        /*  */
        /* tbMessageType_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageType_Label:Appearance = appearance6.
        THIS-OBJECT:tbMessageType_Label:AutoSize = TRUE.
        THIS-OBJECT:tbMessageType_Label:Location = NEW System.Drawing.Point(300, 51).
        THIS-OBJECT:tbMessageType_Label:Name = "tbMessageType_Label":U.
        THIS-OBJECT:tbMessageType_Label:Size = NEW System.Drawing.Size(33, 14).
        THIS-OBJECT:tbMessageType_Label:TabIndex = 6.
        THIS-OBJECT:tbMessageType_Label:Text = "Type:":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LanguageGuid":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(130, 47).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 5.
        /*  */
        /* ultraComboEditor2 */
        /*  */
        THIS-OBJECT:ultraComboEditor2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor2:Location = NEW System.Drawing.Point(356, 47).
        THIS-OBJECT:ultraComboEditor2:Name = "ultraComboEditor2":U.
        THIS-OBJECT:ultraComboEditor2:Size = NEW System.Drawing.Size(134, 21).
        THIS-OBJECT:ultraComboEditor2:TabIndex = 7.
        /*  */
        /* SmartMessageViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageGroup_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageGroup).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageNumber_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageNumber).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbLanguageGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageText_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageText).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageDetail_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageDetail).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMessageType_Label).
        THIS-OBJECT:Name = "SmartMessageViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(493, 305).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMessageGroup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMessageNumber, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMessageText, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMessageDetail, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartMessageViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
