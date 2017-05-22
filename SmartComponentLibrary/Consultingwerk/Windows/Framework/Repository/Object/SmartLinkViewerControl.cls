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
    File        : SmartLinkViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : mikef
    Created     : Tue Dec 27 23:10:48 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*      FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Object.* FROM PROPATH .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartLinkViewerControl INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLink_TargetObjectInstanceGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLink_SourceObjectInstanceGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLink_LinkTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLink_LinkName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLink_LinkName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE linksBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartComboEditor1 AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.

    /**
     * Purpose: Constructor for the SmartLinkViewerControl class
     * Notes:
    */
    CONSTRUCTOR PUBLIC SmartLinkViewerControl ():

        SUPER().

        InitializeComponent().

        THIS-OBJECT:smartBusinessEntityLookup1:BeforePerformLookup:Subscribe (BeforePerformLookupHandler) .
        THIS-OBJECT:smartBusinessEntityLookup2:BeforePerformLookup:Subscribe (BeforePerformLookupHandler) .

        THIS-OBJECT:smartBusinessEntityLookup1:ShowLookupDialog:Subscribe (ShowLookupDialogHandler) .
        THIS-OBJECT:smartBusinessEntityLookup2:ShowLookupDialog:Subscribe (ShowLookupDialogHandler) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the BeforePerformLookup event of the
     *          lookup controls
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PRIVATE VOID BeforePerformLookupHandler (sender AS System.Object,
                                                    e AS System.ComponentModel.CancelEventArgs):

        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = SUBSTITUTE ("FOR EACH eSmartObjectInstance WHERE eSmartObjectInstance.InstanceName BEGINS ~"&&1~" AND eSmartObjectInstance.ContainerObjectMasterGuid = &1":U,
                                                                               QUOTER (THIS-OBJECT:SmartDataSource:GetFieldValues("ContainerObjectMasterGuid":U))) .
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ShowLookupDialog event of the
     *          lookup controls
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ShowLookupDialogHandler (sender AS System.Object,
                                                 e AS System.ComponentModel.CancelEventArgs):

        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = SUBSTITUTE ("FOR EACH eSmartObjectInstance WHERE eSmartObjectInstance.ContainerObjectMasterGuid = &1":U,
                                                                               QUOTER (THIS-OBJECT:SmartDataSource:GetFieldValues("ContainerObjectMasterGuid":U))) .
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQueryString = THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD.

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartLink":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.SmartLinkViewerControl":U).
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:linksBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartLink_LinkTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLink_LinkName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLink_LinkName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLink_SourceObjectInstanceGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLink_TargetObjectInstanceGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityLookup2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:linksBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLink_LinkName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* linksBindingSource */
        /*  */
        THIS-OBJECT:linksBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:linksBindingSource:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:linksBindingSource:EntityTable = "eSmartLink":U.
        THIS-OBJECT:linksBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:linksBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("LinkGuid":U, "LinkGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("LinkTypeGuid":U, "LinkTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LinkName":U, "Link Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("SourceObjectInstanceGuid":U, "SourceObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("SourceObjectName":U, "Source Object":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("SourcePageNumber":U, "Source Page":U, Progress.Data.DataType:INTEGER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("TargetObjectInstanceGuid":U, "TargetObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("TargetObjectName":U, "Source Object":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("TargetPageNumber":U, "Source Page":U, Progress.Data.DataType:INTEGER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:linksBindingSource:TableSchema = tableDesc1.
        /*  */
        /* eSmartLink_LinkTypeGuid_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLink_LinkTypeGuid_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartLink_LinkTypeGuid_Label, "eSmartLink_LinkTypeGuid_Label":U).
        THIS-OBJECT:eSmartLink_LinkTypeGuid_Label:Name = "eSmartLink_LinkTypeGuid_Label":U.
        /*  */
        /* eSmartLink_LinkName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLink_LinkName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartLink_LinkName_Label, "eSmartLink_LinkName_Label":U).
        THIS-OBJECT:eSmartLink_LinkName_Label:Name = "eSmartLink_LinkName_Label":U.
        /*  */
        /* eSmartLink_LinkName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartLink_LinkName, "eSmartLink_LinkName":U).
        THIS-OBJECT:eSmartLink_LinkName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:linksBindingSource, "LinkName":U, TRUE)).
        THIS-OBJECT:eSmartLink_LinkName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLink_LinkName:Name = "eSmartLink_LinkName":U.
        /*  */
        /* eSmartLink_SourceObjectInstanceGuid_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLink_SourceObjectInstanceGuid_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartLink_SourceObjectInstanceGuid_Label, "eSmartLink_SourceObjectInstanceGuid_Label":U).
        THIS-OBJECT:eSmartLink_SourceObjectInstanceGuid_Label:Name = "eSmartLink_SourceObjectInstanceGuid_Label":U.
        /*  */
        /* eSmartLink_TargetObjectInstanceGuid_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartLink_TargetObjectInstanceGuid_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartLink_TargetObjectInstanceGuid_Label, "eSmartLink_TargetObjectInstanceGuid_Label":U).
        THIS-OBJECT:eSmartLink_TargetObjectInstanceGuid_Label:Name = "eSmartLink_TargetObjectInstanceGuid_Label":U.
        /*  */
        /* smartComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartComboEditor1, "smartComboEditor1":U).
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:linksBindingSource, "LinkTypeGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        THIS-OBJECT:smartComboEditor1:Leave:Subscribe(THIS-OBJECT:smartComboEditor1_Leave).
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:linksBindingSource, "SourceObjectInstanceGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:linksBindingSource, "SourceObjectName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "InstanceName,ObjectMasterName,InstanceDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "InstanceName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINSMATCHES":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartObjectInstance":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "InstanceName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "ObjectInstanceGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartObjectInstance WHERE eSmartObjectInstance.InstanceName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        /*  */
        /* smartBusinessEntityLookup2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup2, "smartBusinessEntityLookup2":U).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:linksBindingSource, "TargetObjectInstanceGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:linksBindingSource, "TargetObjectName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserFields = "InstanceName,ObjectMasterName,InstanceDescription":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterFields = "InstanceName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterOperators = "BEGINSMATCHES":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityTable = "eSmartObjectInstance":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyField = "InstanceName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValueColumn = "ObjectInstanceGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = "FOR EACH eSmartObjectInstance WHERE eSmartObjectInstance.InstanceName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:Name = "smartBusinessEntityLookup2":U.
        THIS-OBJECT:smartBusinessEntityLookup2:OwningGrid = ?.
        /*  */
        /* SmartLinkViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLink_LinkTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLink_LinkName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLink_LinkName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLink_SourceObjectInstanceGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLink_TargetObjectInstanceGuid_Label).
        THIS-OBJECT:Name = "SmartLinkViewerControl":U.
        CAST(THIS-OBJECT:linksBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLink_LinkName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Event handler for the Leave event of the smartComboEditor1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartComboEditor1_Leave (sender AS System.Object,
                                                 e AS System.EventArgs):

        IF THIS-OBJECT:smartComboEditor1:Enabled AND
           THIS-OBJECT:eSmartLink_LinkName:Enabled AND
           THIS-OBJECT:eSmartLink_LinkName:Text = "":U AND
           THIS-OBJECT:smartComboEditor1:Text <> THIS-OBJECT:smartComboEditor1:ValueList:ValueListItems[0]:DisplayText THEN

            THIS-OBJECT:eSmartLink_LinkName:Text = THIS-OBJECT:smartComboEditor1:Text .

    END METHOD .

    /**
     * Purpose: Destructor for the SmartLinkViewerControl class
     * Notes:
     */
    DESTRUCTOR PUBLIC SmartLinkViewerControl ():

    END DESTRUCTOR .

END CLASS.
