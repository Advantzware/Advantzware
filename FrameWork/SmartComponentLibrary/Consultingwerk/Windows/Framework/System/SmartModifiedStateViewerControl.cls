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
    File        : SmartModifiedStateViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 01:05:36
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartModifiedStateViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateTable_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateTable AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateKeyFieldValues_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateKeyFieldValues AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_Description_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_Description AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateTimeStamp_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateTimeStamp AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateUser_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModifiedState_ModifiedStateUser AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartModifiedStateViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartModifiedStateViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design
        Notes:
    ------------------------------------------------------------------------------*/
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartModifiedState":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartModifiedStateViewerControl":U).
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
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartModifiedState_Description_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModifiedState_Description = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModifiedState_Description, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartModifiedState":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ModifiedStateGuid":U, "ModifiedStateGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ModifiedStateTable":U, "Table":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ModifiedStateKeyFieldValues":U, "Key Field Values":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ModifiedStateTimeStamp":U, "Time Stamp":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ModifiedStateUser":U, "User Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("Description":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartModifiedState_ModifiedStateTable_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable_Label, "eSmartModifiedState_ModifiedStateTable_Label":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable_Label:Name = "eSmartModifiedState_ModifiedStateTable_Label":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateTable */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable, "eSmartModifiedState_ModifiedStateTable":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModifiedStateTable":U, TRUE)).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTable:Name = "eSmartModifiedState_ModifiedStateTable":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateKeyFieldValues_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues_Label, "eSmartModifiedState_ModifiedStateKeyFieldValues_Label":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues_Label:Name = "eSmartModifiedState_ModifiedStateKeyFieldValues_Label":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateKeyFieldValues */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues, "eSmartModifiedState_ModifiedStateKeyFieldValues":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModifiedStateKeyFieldValues":U, TRUE)).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues:Name = "eSmartModifiedState_ModifiedStateKeyFieldValues":U.
        /*  */
        /* eSmartModifiedState_Description_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModifiedState_Description_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_Description_Label, "eSmartModifiedState_Description_Label":U).
        THIS-OBJECT:eSmartModifiedState_Description_Label:Name = "eSmartModifiedState_Description_Label":U.
        /*  */
        /* eSmartModifiedState_Description */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_Description, "eSmartModifiedState_Description":U).
        THIS-OBJECT:eSmartModifiedState_Description:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Description":U, TRUE)).
        THIS-OBJECT:eSmartModifiedState_Description:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModifiedState_Description:Name = "eSmartModifiedState_Description":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateTimeStamp_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp_Label, "eSmartModifiedState_ModifiedStateTimeStamp_Label":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp_Label:Name = "eSmartModifiedState_ModifiedStateTimeStamp_Label":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateTimeStamp */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp, "eSmartModifiedState_ModifiedStateTimeStamp":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModifiedStateTimeStamp":U, TRUE)).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp:Name = "eSmartModifiedState_ModifiedStateTimeStamp":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateUser_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser_Label, "eSmartModifiedState_ModifiedStateUser_Label":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser_Label:Name = "eSmartModifiedState_ModifiedStateUser_Label":U.
        /*  */
        /* eSmartModifiedState_ModifiedStateUser */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser, "eSmartModifiedState_ModifiedStateUser":U).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModifiedStateUser":U, TRUE)).
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModifiedState_ModifiedStateUser:Name = "eSmartModifiedState_ModifiedStateUser":U.
        /*  */
        /* SmartModifiedStateViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_Description_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_Description).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser).
        THIS-OBJECT:Name = "SmartModifiedStateViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateKeyFieldValues, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModifiedState_Description, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateTimeStamp, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModifiedState_ModifiedStateUser, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.



    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartModifiedStateViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartModifiedStateViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
