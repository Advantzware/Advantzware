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
    File        : SmartAttachmentRoleViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 24.11.2012 14:52:23
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartAttachmentRoleViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAttachmentRoleName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAttachmentRoleName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAttachmentRoleDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAttachmentRoleDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartAttachmentRoleViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartAttachmentRoleViewerControl ():

        SUPER().
        InitializeComponent().

    END CONSTRUCTOR.

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

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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

        CATCH err AS Progress.Lang.Error:
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartAttachmentRole":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartAttachmentRoleViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbAttachmentRoleName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbAttachmentRoleName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAttachmentRoleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbAttachmentRoleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAttachmentRoleName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAttachmentRoleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.AttachmentRoleBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartAttachmentRole":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("AttachmentRoleGUID":U, "AttachmentRoleGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("AttachmentRoleName":U, "Attachment Role":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("AttachmentRoleDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("AttachmentRoleImageSmall":U, "AttachmentRoleImageSmall":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("AttachmentRoleImageLarge":U, "AttachmentRoleImageLarge":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbAttachmentRoleName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbAttachmentRoleName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:tbAttachmentRoleName_Label, "tbAttachmentRoleName_Label":U).
        THIS-OBJECT:tbAttachmentRoleName_Label:Name = "tbAttachmentRoleName_Label":U.
        /*  */
        /* tbAttachmentRoleName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbAttachmentRoleName, "tbAttachmentRoleName":U).
        THIS-OBJECT:tbAttachmentRoleName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttachmentRoleName":U, TRUE)).
        THIS-OBJECT:tbAttachmentRoleName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbAttachmentRoleName:Name = "tbAttachmentRoleName":U.
        /*  */
        /* tbAttachmentRoleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbAttachmentRoleDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbAttachmentRoleDescription_Label, "tbAttachmentRoleDescription_Label":U).
        THIS-OBJECT:tbAttachmentRoleDescription_Label:Name = "tbAttachmentRoleDescription_Label":U.
        /*  */
        /* tbAttachmentRoleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbAttachmentRoleDescription, "tbAttachmentRoleDescription":U).
        THIS-OBJECT:tbAttachmentRoleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttachmentRoleDescription":U, TRUE)).
        THIS-OBJECT:tbAttachmentRoleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbAttachmentRoleDescription:Name = "tbAttachmentRoleDescription":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label, "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label:Name = "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall, "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttachmentRoleImageSmall":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:Name = "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label, "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label:Name = "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge, "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttachmentRoleImageLarge":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:Name = "smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* SmartAttachmentRoleViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAttachmentRoleName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAttachmentRoleName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAttachmentRoleDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbAttachmentRoleDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge).
        THIS-OBJECT:Name = "SmartAttachmentRoleViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAttachmentRoleName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAttachmentRoleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageSmall, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartAttachmentRole_AttachmentRoleImageLarge, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartAttachmentRoleViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
