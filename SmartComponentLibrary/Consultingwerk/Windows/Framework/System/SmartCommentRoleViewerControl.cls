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
    File        : SmartCommentRoleViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 24.11.2012 14:59:48
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartCommentRoleViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCommentRoleName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCommentRoleName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCommentRoleDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCommentRoleDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartCommentRoleViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartCommentRoleViewerControl ():
                
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartCommentRole":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartCommentRoleViewerControl":U).
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
        THIS-OBJECT:tbCommentRoleName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbCommentRoleName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbCommentRoleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbCommentRoleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbCommentRoleName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbCommentRoleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.CommentRoleBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartCommentRole":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("CommentRoleGUID":U, "CommentRoleGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("CommentRoleName":U, "Comment Role":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CommentRoleDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("CommentRoleImageSmall":U, "CommentRoleImageSmall":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("CommentRoleImageLarge":U, "CommentRoleImageLarge":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbCommentRoleName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbCommentRoleName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:tbCommentRoleName_Label, "tbCommentRoleName_Label":U).
        THIS-OBJECT:tbCommentRoleName_Label:Name = "tbCommentRoleName_Label":U.
        /*  */
        /* tbCommentRoleName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbCommentRoleName, "tbCommentRoleName":U).
        THIS-OBJECT:tbCommentRoleName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CommentRoleName":U, TRUE)).
        THIS-OBJECT:tbCommentRoleName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbCommentRoleName:Name = "tbCommentRoleName":U.
        /*  */
        /* tbCommentRoleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbCommentRoleDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:tbCommentRoleDescription_Label, "tbCommentRoleDescription_Label":U).
        THIS-OBJECT:tbCommentRoleDescription_Label:Name = "tbCommentRoleDescription_Label":U.
        /*  */
        /* tbCommentRoleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbCommentRoleDescription, "tbCommentRoleDescription":U).
        THIS-OBJECT:tbCommentRoleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CommentRoleDescription":U, TRUE)).
        THIS-OBJECT:tbCommentRoleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbCommentRoleDescription:Name = "tbCommentRoleDescription":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label, "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label:Name = "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall, "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CommentRoleImageSmall":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:Name = "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label, "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label:Name = "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge, "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CommentRoleImageLarge":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:Name = "smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* SmartCommentRoleViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbCommentRoleName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbCommentRoleName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbCommentRoleDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbCommentRoleDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge).
        THIS-OBJECT:Name = "SmartCommentRoleViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbCommentRoleName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbCommentRoleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageSmall, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartCommentRole_CommentRoleImageLarge, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartCommentRoleViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
