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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartMessage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartMessageViewerControl":U).
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
        resources:ApplyResources(THIS-OBJECT:tbMessageGroup_Label, "tbMessageGroup_Label":U).
        THIS-OBJECT:tbMessageGroup_Label:Name = "tbMessageGroup_Label":U.
        /*  */
        /* tbMessageGroup */
        /*  */
        THIS-OBJECT:tbMessageGroup:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageGroup":U, TRUE)).
        THIS-OBJECT:tbMessageGroup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:tbMessageGroup, "tbMessageGroup":U).
        THIS-OBJECT:tbMessageGroup:Name = "tbMessageGroup":U.
        /*  */
        /* tbMessageNumber_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageNumber_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbMessageNumber_Label, "tbMessageNumber_Label":U).
        THIS-OBJECT:tbMessageNumber_Label:Name = "tbMessageNumber_Label":U.
        /*  */
        /* tbMessageNumber */
        /*  */
        THIS-OBJECT:tbMessageNumber:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageNumber":U, TRUE)).
        THIS-OBJECT:tbMessageNumber:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:tbMessageNumber, "tbMessageNumber":U).
        THIS-OBJECT:tbMessageNumber:Name = "tbMessageNumber":U.
        /*  */
        /* tbLanguageGuid_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbLanguageGuid_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:tbLanguageGuid_Label, "tbLanguageGuid_Label":U).
        THIS-OBJECT:tbLanguageGuid_Label:Name = "tbLanguageGuid_Label":U.
        /*  */
        /* tbMessageText_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageText_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:tbMessageText_Label, "tbMessageText_Label":U).
        THIS-OBJECT:tbMessageText_Label:Name = "tbMessageText_Label":U.
        /*  */
        /* tbMessageText */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbMessageText, "tbMessageText":U).
        THIS-OBJECT:tbMessageText:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageText":U, TRUE)).
        THIS-OBJECT:tbMessageText:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageText:Name = "tbMessageText":U.
        THIS-OBJECT:tbMessageText:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        /*  */
        /* tbMessageDetail_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageDetail_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:tbMessageDetail_Label, "tbMessageDetail_Label":U).
        THIS-OBJECT:tbMessageDetail_Label:Name = "tbMessageDetail_Label":U.
        /*  */
        /* tbMessageDetail */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbMessageDetail, "tbMessageDetail":U).
        THIS-OBJECT:tbMessageDetail:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageDetail":U, TRUE)).
        THIS-OBJECT:tbMessageDetail:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbMessageDetail:Name = "tbMessageDetail":U.
        THIS-OBJECT:tbMessageDetail:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        /*  */
        /* tbMessageType_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbMessageType_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:tbMessageType_Label, "tbMessageType_Label":U).
        THIS-OBJECT:tbMessageType_Label:Name = "tbMessageType_Label":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LanguageGuid":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        /*  */
        /* ultraComboEditor2 */
        /*  */
        THIS-OBJECT:ultraComboEditor2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MessageType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor2, "ultraComboEditor2":U).
        THIS-OBJECT:ultraComboEditor2:Name = "ultraComboEditor2":U.
        /*  */
        /* SmartMessageViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
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
