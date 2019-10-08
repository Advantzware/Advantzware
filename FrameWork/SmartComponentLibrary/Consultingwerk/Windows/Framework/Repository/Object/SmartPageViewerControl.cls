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
    File        : SmartPageViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 30.11.2016 19:08:12
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartPageViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_PageSequence_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_PageSequence AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_PageLabel_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_PageLabel AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_SecurityToken_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_SecurityToken AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnCreate_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnCreate AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnModify_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnModify AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnView_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartPage_EnableOnView AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartPageViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartPageViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:SetControlEnabled(eSmartPage_PageSequence, ControlEnabledEnum:Add) .

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartPage":U).
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
        THIS-OBJECT:eSmartPage_PageSequence_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_PageSequence = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartPage_PageLabel_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_PageLabel = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartPage_SecurityToken_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_SecurityToken = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_EnableOnCreate = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartPage_EnableOnModify_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_EnableOnModify = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartPage_EnableOnView_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartPage_EnableOnView = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_PageSequence, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_PageLabel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_SecurityToken, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnCreate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnModify, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnView, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartPage":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityView = "":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("PageGuid":U, "Page":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("PageSequence":U, "Page Number":U, Progress.Data.DataType:INTEGER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("PageLabel":U, "Page Label":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("SecurityToken":U, "Security Token":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("EnableOnCreate":U, "Enable on create":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("EnableOnModify":U, "Enable on modify":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("EnableOnView":U, "Enable on view":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartPage_PageSequence_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_PageSequence_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartPage_PageSequence_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_PageSequence_Label:Location = NEW System.Drawing.Point(20, 24).
        THIS-OBJECT:eSmartPage_PageSequence_Label:Name = "eSmartPage_PageSequence_Label":U.
        THIS-OBJECT:eSmartPage_PageSequence_Label:TabIndex = 1.
        THIS-OBJECT:eSmartPage_PageSequence_Label:Text = "Page Number:":U.
        /*  */
        /* eSmartPage_PageSequence */
        /*  */
        THIS-OBJECT:eSmartPage_PageSequence:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PageSequence":U, TRUE)).
        THIS-OBJECT:eSmartPage_PageSequence:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartPage_PageSequence:Location = NEW System.Drawing.Point(130, 20).
        THIS-OBJECT:eSmartPage_PageSequence:Name = "eSmartPage_PageSequence":U.
        THIS-OBJECT:eSmartPage_PageSequence:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Integer.
        THIS-OBJECT:eSmartPage_PageSequence:Size = NEW System.Drawing.Size(80, 21).
        THIS-OBJECT:eSmartPage_PageSequence:TabIndex = 2.
        /*  */
        /* eSmartPage_PageLabel_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_PageLabel_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartPage_PageLabel_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_PageLabel_Label:Location = NEW System.Drawing.Point(20, 50).
        THIS-OBJECT:eSmartPage_PageLabel_Label:Name = "eSmartPage_PageLabel_Label":U.
        THIS-OBJECT:eSmartPage_PageLabel_Label:TabIndex = 3.
        THIS-OBJECT:eSmartPage_PageLabel_Label:Text = "Page Label:":U.
        /*  */
        /* eSmartPage_PageLabel */
        /*  */
        THIS-OBJECT:eSmartPage_PageLabel:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PageLabel":U, TRUE)).
        THIS-OBJECT:eSmartPage_PageLabel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartPage_PageLabel:Location = NEW System.Drawing.Point(130, 46).
        THIS-OBJECT:eSmartPage_PageLabel:Name = "eSmartPage_PageLabel":U.
        THIS-OBJECT:eSmartPage_PageLabel:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartPage_PageLabel:TabIndex = 4.
        /*  */
        /* eSmartPage_SecurityToken_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_SecurityToken_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartPage_SecurityToken_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_SecurityToken_Label:Location = NEW System.Drawing.Point(20, 76).
        THIS-OBJECT:eSmartPage_SecurityToken_Label:Name = "eSmartPage_SecurityToken_Label":U.
        THIS-OBJECT:eSmartPage_SecurityToken_Label:TabIndex = 5.
        THIS-OBJECT:eSmartPage_SecurityToken_Label:Text = "Security Token:":U.
        /*  */
        /* eSmartPage_SecurityToken */
        /*  */
        THIS-OBJECT:eSmartPage_SecurityToken:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityToken":U, TRUE)).
        THIS-OBJECT:eSmartPage_SecurityToken:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartPage_SecurityToken:Location = NEW System.Drawing.Point(130, 72).
        THIS-OBJECT:eSmartPage_SecurityToken:Name = "eSmartPage_SecurityToken":U.
        THIS-OBJECT:eSmartPage_SecurityToken:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartPage_SecurityToken:TabIndex = 6.
        /*  */
        /* eSmartPage_EnableOnCreate_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:Location = NEW System.Drawing.Point(20, 102).
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:Name = "eSmartPage_EnableOnCreate_Label":U.
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:TabIndex = 7.
        THIS-OBJECT:eSmartPage_EnableOnCreate_Label:Text = "Enable on create:":U.
        /*  */
        /* eSmartPage_EnableOnCreate */
        /*  */
        THIS-OBJECT:eSmartPage_EnableOnCreate:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EnableOnCreate":U, TRUE)).
        THIS-OBJECT:eSmartPage_EnableOnCreate:Location = NEW System.Drawing.Point(130, 98).
        THIS-OBJECT:eSmartPage_EnableOnCreate:Name = "eSmartPage_EnableOnCreate":U.
        THIS-OBJECT:eSmartPage_EnableOnCreate:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartPage_EnableOnCreate:TabIndex = 8.
        THIS-OBJECT:eSmartPage_EnableOnCreate:Text = "Enable on create":U.
        THIS-OBJECT:eSmartPage_EnableOnCreate:UseMnemonics = TRUE.
        /*  */
        /* eSmartPage_EnableOnModify_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:Location = NEW System.Drawing.Point(20, 128).
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:Name = "eSmartPage_EnableOnModify_Label":U.
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:TabIndex = 9.
        THIS-OBJECT:eSmartPage_EnableOnModify_Label:Text = "Enable on modify:":U.
        /*  */
        /* eSmartPage_EnableOnModify */
        /*  */
        THIS-OBJECT:eSmartPage_EnableOnModify:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EnableOnModify":U, TRUE)).
        THIS-OBJECT:eSmartPage_EnableOnModify:Location = NEW System.Drawing.Point(130, 124).
        THIS-OBJECT:eSmartPage_EnableOnModify:Name = "eSmartPage_EnableOnModify":U.
        THIS-OBJECT:eSmartPage_EnableOnModify:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartPage_EnableOnModify:TabIndex = 10.
        THIS-OBJECT:eSmartPage_EnableOnModify:Text = "Enable on modify":U.
        THIS-OBJECT:eSmartPage_EnableOnModify:UseMnemonics = TRUE.
        /*  */
        /* eSmartPage_EnableOnView_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartPage_EnableOnView_Label:Appearance = appearance6.
        THIS-OBJECT:eSmartPage_EnableOnView_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartPage_EnableOnView_Label:Location = NEW System.Drawing.Point(20, 154).
        THIS-OBJECT:eSmartPage_EnableOnView_Label:Name = "eSmartPage_EnableOnView_Label":U.
        THIS-OBJECT:eSmartPage_EnableOnView_Label:TabIndex = 11.
        THIS-OBJECT:eSmartPage_EnableOnView_Label:Text = "Enable on view:":U.
        /*  */
        /* eSmartPage_EnableOnView */
        /*  */
        THIS-OBJECT:eSmartPage_EnableOnView:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EnableOnView":U, TRUE)).
        THIS-OBJECT:eSmartPage_EnableOnView:Location = NEW System.Drawing.Point(130, 150).
        THIS-OBJECT:eSmartPage_EnableOnView:Name = "eSmartPage_EnableOnView":U.
        THIS-OBJECT:eSmartPage_EnableOnView:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:eSmartPage_EnableOnView:TabIndex = 12.
        THIS-OBJECT:eSmartPage_EnableOnView:Text = "Enable on view":U.
        THIS-OBJECT:eSmartPage_EnableOnView:UseMnemonics = TRUE.
        /*  */
        /* SmartPageViewerControl */
        /*  */
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_PageSequence_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_PageSequence).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_PageLabel_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_PageLabel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_SecurityToken_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_SecurityToken).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnCreate_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnCreate).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnModify_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnModify).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnView_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartPage_EnableOnView).
        THIS-OBJECT:Name = "SmartPageViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(300, 300).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_PageSequence, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_PageLabel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_SecurityToken, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnCreate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnModify, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartPage_EnableOnView, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.



    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartPageViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartPageViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
