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
    File        : SmartSchedulerJobStatusViewerControl
    Purpose     : Viewer Control for Scheduler JobStatus details
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 19:15:47
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobStatusViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_ScheduleDateTime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_ScheduleDateTime AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Recurring AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobStatus_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobStatus AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_StatusText_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_StatusText AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Percentage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Percentage AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_MaxExecutionDelay_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_MaxExecutionDelay AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Cancel AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_StartDateTime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_StartDateTime AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_FinishedDateTime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_FinishedDateTime AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobParamter_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_JobParamter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_SchedulerJobName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_SchedulerJobName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Description_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobStatus_Description AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobStatusViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobStatusViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSchedulerJobStatus":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobStatusViewerControl":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_Recurring = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_Cancel = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobStatus_Description_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_Description = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Recurring, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Cancel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Description, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartSchedulerJobStatus":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 18 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SchedulerJobStatusGuid":U, "Job Status ID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SchedulerJobGuid":U, "Job Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ScheduleDateTime":U, "Scheduled Job Execution":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("JobName":U, "JobName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("Recurring":U, "Recurring":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("JobStatus":U, "Job Status":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("StatusText":U, "Status Text":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("Percentage":U, "Progress":U, Progress.Data.DataType:INTEGER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("MaxExecutionDelay":U, "Max. Delay":U, Progress.Data.DataType:INTEGER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("Cancel":U, "Cancel Req.":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("StartDateTime":U, "Start DateTime":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("FinishedDateTime":U, "Finished DateTime":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("JobParamter":U, "JobParamter":U, Progress.Data.DataType:CLOB).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("ContextDataSet":U, "ContextDataSet":U, Progress.Data.DataType:CLOB).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("DataBaseConnections":U, "DataBaseConnections":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("SchedulerJobName":U, "JobName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("Description":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSchedulerJobStatus_ScheduleDateTime_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime_Label, "eSmartSchedulerJobStatus_ScheduleDateTime_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime_Label:Name = "eSmartSchedulerJobStatus_ScheduleDateTime_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_ScheduleDateTime */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ScheduleDateTime":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime, "eSmartSchedulerJobStatus_ScheduleDateTime":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime:Name = "eSmartSchedulerJobStatus_ScheduleDateTime":U.
        /*  */
        /* eSmartSchedulerJobStatus_Recurring */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_Recurring:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Recurring":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Recurring, "eSmartSchedulerJobStatus_Recurring":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Recurring:Name = "eSmartSchedulerJobStatus_Recurring":U.
        THIS-OBJECT:eSmartSchedulerJobStatus_Recurring:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobStatus_JobStatus_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus_Label, "eSmartSchedulerJobStatus_JobStatus_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus_Label:Name = "eSmartSchedulerJobStatus_JobStatus_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_JobStatus */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "JobStatus":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus, "eSmartSchedulerJobStatus_JobStatus":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus:Name = "eSmartSchedulerJobStatus_JobStatus":U.
        /*  */
        /* eSmartSchedulerJobStatus_StatusText_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText_Label, "eSmartSchedulerJobStatus_StatusText_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText_Label:Name = "eSmartSchedulerJobStatus_StatusText_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_StatusText */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText, "eSmartSchedulerJobStatus_StatusText":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "StatusText":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobStatus_StatusText:Name = "eSmartSchedulerJobStatus_StatusText":U.
        /*  */
        /* eSmartSchedulerJobStatus_Percentage_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage_Label, "eSmartSchedulerJobStatus_Percentage_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage_Label:Name = "eSmartSchedulerJobStatus_Percentage_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_Percentage */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Percentage":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage, "eSmartSchedulerJobStatus_Percentage":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Percentage:Name = "eSmartSchedulerJobStatus_Percentage":U.
        /*  */
        /* eSmartSchedulerJobStatus_MaxExecutionDelay_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay_Label, "eSmartSchedulerJobStatus_MaxExecutionDelay_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay_Label:Name = "eSmartSchedulerJobStatus_MaxExecutionDelay_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_MaxExecutionDelay */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MaxExecutionDelay":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay, "eSmartSchedulerJobStatus_MaxExecutionDelay":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay:Name = "eSmartSchedulerJobStatus_MaxExecutionDelay":U.
        /*  */
        /* eSmartSchedulerJobStatus_Cancel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Cancel, "eSmartSchedulerJobStatus_Cancel":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Cancel:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Cancel":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_Cancel:Name = "eSmartSchedulerJobStatus_Cancel":U.
        THIS-OBJECT:eSmartSchedulerJobStatus_Cancel:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobStatus_StartDateTime_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime_Label, "eSmartSchedulerJobStatus_StartDateTime_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime_Label:Name = "eSmartSchedulerJobStatus_StartDateTime_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_StartDateTime */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "StartDateTime":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime, "eSmartSchedulerJobStatus_StartDateTime":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime:Name = "eSmartSchedulerJobStatus_StartDateTime":U.
        /*  */
        /* eSmartSchedulerJobStatus_FinishedDateTime_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime_Label, "eSmartSchedulerJobStatus_FinishedDateTime_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime_Label:Name = "eSmartSchedulerJobStatus_FinishedDateTime_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_FinishedDateTime */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FinishedDateTime":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime, "eSmartSchedulerJobStatus_FinishedDateTime":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime:Name = "eSmartSchedulerJobStatus_FinishedDateTime":U.
        /*  */
        /* eSmartSchedulerJobStatus_JobParamter_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter_Label, "eSmartSchedulerJobStatus_JobParamter_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter_Label:Name = "eSmartSchedulerJobStatus_JobParamter_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_JobParamter */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter, "eSmartSchedulerJobStatus_JobParamter":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "JobParamter":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter:Name = "eSmartSchedulerJobStatus_JobParamter":U.
        /*  */
        /* eSmartSchedulerJobStatus_SchedulerJobName_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName_Label, "eSmartSchedulerJobStatus_SchedulerJobName_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName_Label:Name = "eSmartSchedulerJobStatus_SchedulerJobName_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_SchedulerJobName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName, "eSmartSchedulerJobStatus_SchedulerJobName":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SchedulerJobName":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName:Name = "eSmartSchedulerJobStatus_SchedulerJobName":U.
        /*  */
        /* eSmartSchedulerJobStatus_Description_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_Description_Label:Appearance = appearance10.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Description_Label, "eSmartSchedulerJobStatus_Description_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Description_Label:Name = "eSmartSchedulerJobStatus_Description_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_Description */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_Description, "eSmartSchedulerJobStatus_Description":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_Description:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Description":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_Description:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobStatus_Description:Name = "eSmartSchedulerJobStatus_Description":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance11.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance12:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance12.
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* eSmartSchedulerJobStatus_JobName_Label */
        /*  */
        appearance13:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName_Label:Appearance = appearance13.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobName_Label, "eSmartSchedulerJobStatus_JobName_Label":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName_Label:Name = "eSmartSchedulerJobStatus_JobName_Label":U.
        /*  */
        /* eSmartSchedulerJobStatus_JobName */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "JobName":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobStatus_JobName, "eSmartSchedulerJobStatus_JobName":U).
        THIS-OBJECT:eSmartSchedulerJobStatus_JobName:Name = "eSmartSchedulerJobStatus_JobName":U.
        /*  */
        /* SmartSchedulerJobStatusViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Recurring).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Cancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Description_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_Description).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSchedulerJobStatus_JobName).
        THIS-OBJECT:Name = "SmartSchedulerJobStatusViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_ScheduleDateTime, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Recurring, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobStatus, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_StatusText, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Percentage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_MaxExecutionDelay, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Cancel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_StartDateTime, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_FinishedDateTime, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobParamter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_SchedulerJobName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_Description, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobStatus_JobName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSchedulerJobStatusViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSchedulerJobStatusViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
