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
    File        : SmartSchedulerJobPlanViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 10:39:09
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.*       FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Util.Enum.*                      FROM PROPATH .
USING Consultingwerk.Windows.Framework.Scheduler.*    FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobPlanViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE btnNone AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnAll AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnNoneDays AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnAllDays AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_MaxRuntime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_MaxRuntime AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_MaxExecutionDelay_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_MaxExecutionDelay AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_JobParamter_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_JobParamter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_PlanningType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_ScheduleDateTime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringMonday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringTuesday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringWednesday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringThursday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringFriday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringSaturday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringSunday AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_WeekInterval_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_WeekInterval AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringJanuary AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringFebruary AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringMarch AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringApril AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringMay AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringJune AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringJuly AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringAugust AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringSeptember AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringOctober AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringNovember AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringDecember AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringDaylist_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringDaylist AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringExecutionTimes_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_RecurringExecutionTimes AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_ValidFrom_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_ValidTo_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_Active AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_DataBaseConnections_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_DataBaseConnections AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJobPlan_SchedulerJobName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDateTimeEditor1 AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDateTimeEditor2 AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDateTimeEditor3 AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox3 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox2 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel3 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel2 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    /**
     * Purpose: Constructor for the SmartSchedulerJobPlanViewerControl class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartSchedulerJobPlanViewerControl ():

        SUPER ().
        InitializeComponent ().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        ultraComboEditor1:ValueList = ValueListHelper:FromEnum (Progress.Lang.Class:GetClass("Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum":U), ValueFromEnum:EnumName).

        THIS-OBJECT:SetControlEnabled(ultraSplitter1, ControlEnabledEnum:Always) .

   END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the SizeChanged event of the
     *          eSmartSchedulerJobPlan_RecurringDaylist control.
     *          Adjust the MaximumSize property of the ultraLabel7 to "create" a line break
     * Notes:
     * @param sender The source of the event
     * @param e An EventArgs that contains no event data
     */
    METHOD PRIVATE VOID eSmartSchedulerJobPlan_RecurringDaylist_SizeChanged (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        ASSIGN ultraLabel7:MaximumSize = NEW System.Drawing.Size (eSmartSchedulerJobPlan_RecurringDaylist:Size:Width, 0).

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSchedulerJobPlan":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobPlanViewerControl":U).
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
        DEFINE VARIABLE dateTimeIncrement1 AS Infragistics.Shared.DateTimeIncrement NO-UNDO.
        dateTimeIncrement1 = NEW Infragistics.Shared.DateTimeIncrement().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dateTimeIncrement2 AS Infragistics.Shared.DateTimeIncrement NO-UNDO.
        dateTimeIncrement2 = NEW Infragistics.Shared.DateTimeIncrement().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dateTimeIncrement3 AS Infragistics.Shared.DateTimeIncrement NO-UNDO.
        dateTimeIncrement3 = NEW Infragistics.Shared.DateTimeIncrement().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_PlanningType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_ScheduleDateTime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidFrom_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidTo_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_Active = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJobPlan_SchedulerJobName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraDateTimeEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:ultraGroupBox3 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnNone = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnAll = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraGroupBox2 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:btnNoneDays = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnAllDays = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraDateTimeEditor3 = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:ultraDateTimeEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraPanel2 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraPanel3 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_Active, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:ultraDateTimeEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox3:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox2:SuspendLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraDateTimeEditor3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDateTimeEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel2:SuspendLayout().
        THIS-OBJECT:ultraPanel3:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel3:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartSchedulerJobPlan":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 36 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SchedulerJobPlanGuid":U, "Job Status ID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SchedulerJobGuid":U, "Job Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("MaxRuntime":U, "Max. Runtime":U, Progress.Data.DataType:INTEGER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("MaxExecutionDelay":U, "Max. Delay":U, Progress.Data.DataType:INTEGER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("JobParamter":U, "Job Paramter":U, Progress.Data.DataType:CLOB).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("PlanningType":U, "Plan Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ScheduleDateTime":U, "Once Now/Scheduled":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("RecurringMonday":U, "Recurring Monday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("RecurringTuesday":U, "Recurring Tuesday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("RecurringWednesday":U, "Recurring Wendesday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("RecurringThursday":U, "Recurring Thursday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("RecurringFriday":U, "Recurring Friday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("RecurringSaturday":U, "Recurring Saturday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("RecurringSunday":U, "Recurring Sunday":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("WeekInterval":U, "Week Interval":U, Progress.Data.DataType:INTEGER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("RecurringJanuary":U, "Recurring January":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("RecurringFebruary":U, "Recurring February":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("RecurringMarch":U, "Recurring March":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[19] = NEW Progress.Data.ColumnPropDesc("RecurringApril":U, "Recurring April":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[20] = NEW Progress.Data.ColumnPropDesc("RecurringMay":U, "Recurring May":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[21] = NEW Progress.Data.ColumnPropDesc("RecurringJune":U, "Recurring June":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[22] = NEW Progress.Data.ColumnPropDesc("RecurringJuly":U, "Recurring July":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[23] = NEW Progress.Data.ColumnPropDesc("RecurringAugust":U, "Recurring August":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[24] = NEW Progress.Data.ColumnPropDesc("RecurringSeptember":U, "Recurring September":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[25] = NEW Progress.Data.ColumnPropDesc("RecurringOctober":U, "Recurring October":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[26] = NEW Progress.Data.ColumnPropDesc("RecurringNovember":U, "Recurring November":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[27] = NEW Progress.Data.ColumnPropDesc("RecurringDecember":U, "Recurring December":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[28] = NEW Progress.Data.ColumnPropDesc("RecurringDaylist":U, "Recurring Daylist":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[29] = NEW Progress.Data.ColumnPropDesc("RecurringExecutionTimes":U, "Recurring Execution Times":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[30] = NEW Progress.Data.ColumnPropDesc("ValidFrom":U, "JobPlan valid from":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[31] = NEW Progress.Data.ColumnPropDesc("ValidTo":U, "JobPlan valid to":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[32] = NEW Progress.Data.ColumnPropDesc("Active":U, "JobPlan is active":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[33] = NEW Progress.Data.ColumnPropDesc("ContextDataSet":U, "Context DataSet":U, Progress.Data.DataType:CLOB).
        arrayvar0[34] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "User Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[35] = NEW Progress.Data.ColumnPropDesc("DataBaseConnections":U, "DataBase Connections":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[36] = NEW Progress.Data.ColumnPropDesc("SchedulerJobName":U, "Job Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSchedulerJobPlan_MaxRuntime_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime_Label, "eSmartSchedulerJobPlan_MaxRuntime_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime_Label:Name = "eSmartSchedulerJobPlan_MaxRuntime_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_MaxRuntime */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MaxRuntime":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime, "eSmartSchedulerJobPlan_MaxRuntime":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime:Name = "eSmartSchedulerJobPlan_MaxRuntime":U.
        /*  */
        /* eSmartSchedulerJobPlan_MaxExecutionDelay_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay_Label, "eSmartSchedulerJobPlan_MaxExecutionDelay_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay_Label:Name = "eSmartSchedulerJobPlan_MaxExecutionDelay_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_MaxExecutionDelay */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MaxExecutionDelay":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay, "eSmartSchedulerJobPlan_MaxExecutionDelay":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay:Name = "eSmartSchedulerJobPlan_MaxExecutionDelay":U.
        /*  */
        /* eSmartSchedulerJobPlan_JobParamter_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter_Label, "eSmartSchedulerJobPlan_JobParamter_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter_Label:Name = "eSmartSchedulerJobPlan_JobParamter_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_JobParamter */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter, "eSmartSchedulerJobPlan_JobParamter":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "JobParamter":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter:Name = "eSmartSchedulerJobPlan_JobParamter":U.
        /*  */
        /* eSmartSchedulerJobPlan_PlanningType_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_PlanningType_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_PlanningType_Label, "eSmartSchedulerJobPlan_PlanningType_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_PlanningType_Label:Name = "eSmartSchedulerJobPlan_PlanningType_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_ScheduleDateTime_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_ScheduleDateTime_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_ScheduleDateTime_Label, "eSmartSchedulerJobPlan_ScheduleDateTime_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_ScheduleDateTime_Label:Name = "eSmartSchedulerJobPlan_ScheduleDateTime_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringMonday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringMonday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday, "eSmartSchedulerJobPlan_RecurringMonday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday:Name = "eSmartSchedulerJobPlan_RecurringMonday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringTuesday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringTuesday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday, "eSmartSchedulerJobPlan_RecurringTuesday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday:Name = "eSmartSchedulerJobPlan_RecurringTuesday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringWednesday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringWednesday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday, "eSmartSchedulerJobPlan_RecurringWednesday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday:Name = "eSmartSchedulerJobPlan_RecurringWednesday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringThursday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringThursday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday, "eSmartSchedulerJobPlan_RecurringThursday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday:Name = "eSmartSchedulerJobPlan_RecurringThursday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringFriday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringFriday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday, "eSmartSchedulerJobPlan_RecurringFriday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday:Name = "eSmartSchedulerJobPlan_RecurringFriday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringSaturday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringSaturday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday, "eSmartSchedulerJobPlan_RecurringSaturday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday:Name = "eSmartSchedulerJobPlan_RecurringSaturday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringSunday */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringSunday":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday, "eSmartSchedulerJobPlan_RecurringSunday":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday:Name = "eSmartSchedulerJobPlan_RecurringSunday":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_WeekInterval_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval_Label, "eSmartSchedulerJobPlan_WeekInterval_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval_Label:Name = "eSmartSchedulerJobPlan_WeekInterval_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_WeekInterval */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "WeekInterval":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval, "eSmartSchedulerJobPlan_WeekInterval":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval:Name = "eSmartSchedulerJobPlan_WeekInterval":U.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringJanuary */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringJanuary":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary, "eSmartSchedulerJobPlan_RecurringJanuary":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary:Name = "eSmartSchedulerJobPlan_RecurringJanuary":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringFebruary */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringFebruary":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary, "eSmartSchedulerJobPlan_RecurringFebruary":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary:Name = "eSmartSchedulerJobPlan_RecurringFebruary":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringMarch */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringMarch":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch, "eSmartSchedulerJobPlan_RecurringMarch":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch:Name = "eSmartSchedulerJobPlan_RecurringMarch":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringApril */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringApril":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril, "eSmartSchedulerJobPlan_RecurringApril":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril:Name = "eSmartSchedulerJobPlan_RecurringApril":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringMay */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringMay":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay, "eSmartSchedulerJobPlan_RecurringMay":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay:Name = "eSmartSchedulerJobPlan_RecurringMay":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringJune */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringJune":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune, "eSmartSchedulerJobPlan_RecurringJune":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune:Name = "eSmartSchedulerJobPlan_RecurringJune":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringJuly */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringJuly":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly, "eSmartSchedulerJobPlan_RecurringJuly":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly:Name = "eSmartSchedulerJobPlan_RecurringJuly":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringAugust */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringAugust":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust, "eSmartSchedulerJobPlan_RecurringAugust":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust:Name = "eSmartSchedulerJobPlan_RecurringAugust":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringSeptember */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringSeptember":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember, "eSmartSchedulerJobPlan_RecurringSeptember":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember:Name = "eSmartSchedulerJobPlan_RecurringSeptember":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringOctober */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringOctober":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober, "eSmartSchedulerJobPlan_RecurringOctober":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober:Name = "eSmartSchedulerJobPlan_RecurringOctober":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringNovember */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringNovember":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember, "eSmartSchedulerJobPlan_RecurringNovember":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember:Name = "eSmartSchedulerJobPlan_RecurringNovember":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringDecember */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringDecember":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember, "eSmartSchedulerJobPlan_RecurringDecember":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember:Name = "eSmartSchedulerJobPlan_RecurringDecember":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringDaylist_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_Label, "eSmartSchedulerJobPlan_RecurringDaylist_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_Label:Name = "eSmartSchedulerJobPlan_RecurringDaylist_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringDaylist */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist, "eSmartSchedulerJobPlan_RecurringDaylist":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringDaylist":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist:Name = "eSmartSchedulerJobPlan_RecurringDaylist":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist:SizeChanged:Subscribe(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_SizeChanged).
        /*  */
        /* eSmartSchedulerJobPlan_RecurringExecutionTimes_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes_Label, "eSmartSchedulerJobPlan_RecurringExecutionTimes_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes_Label:Name = "eSmartSchedulerJobPlan_RecurringExecutionTimes_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_RecurringExecutionTimes */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes, "eSmartSchedulerJobPlan_RecurringExecutionTimes":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "RecurringExecutionTimes":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes:Name = "eSmartSchedulerJobPlan_RecurringExecutionTimes":U.
        /*  */
        /* eSmartSchedulerJobPlan_ValidFrom_Label */
        /*  */
        appearance9:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidFrom_Label:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_ValidFrom_Label, "eSmartSchedulerJobPlan_ValidFrom_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidFrom_Label:Name = "eSmartSchedulerJobPlan_ValidFrom_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_ValidTo_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidTo_Label:Appearance = appearance10.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_ValidTo_Label, "eSmartSchedulerJobPlan_ValidTo_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_ValidTo_Label:Name = "eSmartSchedulerJobPlan_ValidTo_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_Active */
        /*  */
        THIS-OBJECT:eSmartSchedulerJobPlan_Active:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Active":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_Active, "eSmartSchedulerJobPlan_Active":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_Active:Name = "eSmartSchedulerJobPlan_Active":U.
        THIS-OBJECT:eSmartSchedulerJobPlan_Active:UseMnemonics = TRUE.
        /*  */
        /* eSmartSchedulerJobPlan_DataBaseConnections_Label */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections_Label:Appearance = appearance11.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections_Label, "eSmartSchedulerJobPlan_DataBaseConnections_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections_Label:Name = "eSmartSchedulerJobPlan_DataBaseConnections_Label":U.
        /*  */
        /* eSmartSchedulerJobPlan_DataBaseConnections */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections, "eSmartSchedulerJobPlan_DataBaseConnections":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DataBaseConnections":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections:Name = "eSmartSchedulerJobPlan_DataBaseConnections":U.
        /*  */
        /* eSmartSchedulerJobPlan_SchedulerJobName_Label */
        /*  */
        appearance12:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJobPlan_SchedulerJobName_Label:Appearance = appearance12.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJobPlan_SchedulerJobName_Label, "eSmartSchedulerJobPlan_SchedulerJobName_Label":U).
        THIS-OBJECT:eSmartSchedulerJobPlan_SchedulerJobName_Label:Name = "eSmartSchedulerJobPlan_SchedulerJobName_Label":U.
        /*  */
        /* ultraGroupBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraGroupBox1, "ultraGroupBox1":U).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraDateTimeEditor1).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_ScheduleDateTime_Label).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        /*  */
        /* ultraDateTimeEditor1 */
        /*  */
        THIS-OBJECT:ultraDateTimeEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ScheduleDateTime":U, TRUE)).
        THIS-OBJECT:ultraDateTimeEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ultraDateTimeEditor1, "ultraDateTimeEditor1":U).
        THIS-OBJECT:ultraDateTimeEditor1:MaskInput = "~{date~} ~{time~}":U.
        THIS-OBJECT:ultraDateTimeEditor1:Name = "ultraDateTimeEditor1":U.
        THIS-OBJECT:ultraDateTimeEditor1:SpinButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:Always.
        dateTimeIncrement1:Hours = 1.
        THIS-OBJECT:ultraDateTimeEditor1:SpinIncrement = dateTimeIncrement1.
        /*  */
        /* ultraGroupBox3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraGroupBox3, "ultraGroupBox3":U).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:btnNone).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:btnAll).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist_Label).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary).
        THIS-OBJECT:ultraGroupBox3:Name = "ultraGroupBox3":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        appearance13:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel7:Appearance = appearance13.
        resources:ApplyResources(THIS-OBJECT:ultraLabel7, "ultraLabel7":U).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        /*  */
        /* btnNone */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnNone, "btnNone":U).
        THIS-OBJECT:btnNone:Name = "btnNone":U.
        THIS-OBJECT:btnNone:Click:Subscribe(THIS-OBJECT:OnClickHandler).
        /*  */
        /* btnAll */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnAll, "btnAll":U).
        THIS-OBJECT:btnAll:Name = "btnAll":U.
        THIS-OBJECT:btnAll:Click:Subscribe(THIS-OBJECT:OnClickHandler).
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance14:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance14.
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraGroupBox2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraGroupBox2, "ultraGroupBox2":U).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:btnNoneDays).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:btnAllDays).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval_Label).
        THIS-OBJECT:ultraGroupBox2:Name = "ultraGroupBox2":U.
        /*  */
        /* btnNoneDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnNoneDays, "btnNoneDays":U).
        THIS-OBJECT:btnNoneDays:Name = "btnNoneDays":U.
        THIS-OBJECT:btnNoneDays:Click:Subscribe(THIS-OBJECT:OnClickHandler).
        /*  */
        /* btnAllDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnAllDays, "btnAllDays":U).
        THIS-OBJECT:btnAllDays:Name = "btnAllDays":U.
        THIS-OBJECT:btnAllDays:Click:Subscribe(THIS-OBJECT:OnClickHandler).
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance15:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance15.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PlanningType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "ValueListItem0":U.
        resources:ApplyResources(valueListItem1, "valueListItem1":U).
        valueListItem1:ForceApplyResources = "":U.
        valueListItem2:DataValue = "ValueListItem1":U.
        resources:ApplyResources(valueListItem2, "valueListItem2":U).
        valueListItem2:ForceApplyResources = "":U.
        valueListItem3:DataValue = "ValueListItem2":U.
        resources:ApplyResources(valueListItem3, "valueListItem3":U).
        valueListItem3:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        arrayvar1[3] = valueListItem3.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar1).
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:SelectionChanged:Subscribe(THIS-OBJECT:OnSelectionChangedHandler).
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SchedulerJobName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SchedulerJobGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "SchedulerJobName,Description":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "eSmartSchedulerJobPlan_SchedulerJobGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartSchedulerJob":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "SchedulerJobGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "SchedulerJobName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "SchedulerJobGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartSchedulerJob WHERE eSmartSchedulerJob.SchedulerJobName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        /*  */
        /* ultraPanel1 */
        /*  */
        THIS-OBJECT:ultraPanel1:AutoScroll = TRUE.
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraDateTimeEditor3).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraDateTimeEditor2).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_ValidTo_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_ValidFrom_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes_Label).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        appearance16:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel6:Appearance = appearance16.
        resources:ApplyResources(THIS-OBJECT:ultraLabel6, "ultraLabel6":U).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        /*  */
        /* ultraDateTimeEditor3 */
        /*  */
        THIS-OBJECT:ultraDateTimeEditor3:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ValidTo":U, TRUE)).
        THIS-OBJECT:ultraDateTimeEditor3:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ultraDateTimeEditor3, "ultraDateTimeEditor3":U).
        THIS-OBJECT:ultraDateTimeEditor3:MaskInput = "~{date~} ~{time~}":U.
        THIS-OBJECT:ultraDateTimeEditor3:Name = "ultraDateTimeEditor3":U.
        THIS-OBJECT:ultraDateTimeEditor3:SpinButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:Always.
        dateTimeIncrement2:Hours = 1.
        THIS-OBJECT:ultraDateTimeEditor3:SpinIncrement = dateTimeIncrement2.
        /*  */
        /* ultraDateTimeEditor2 */
        /*  */
        THIS-OBJECT:ultraDateTimeEditor2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ValidFrom":U, TRUE)).
        THIS-OBJECT:ultraDateTimeEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ultraDateTimeEditor2, "ultraDateTimeEditor2":U).
        THIS-OBJECT:ultraDateTimeEditor2:MaskInput = "~{LOC~}mm/dd/yyyy hh:mm:ss tt":U.
        THIS-OBJECT:ultraDateTimeEditor2:Name = "ultraDateTimeEditor2":U.
        THIS-OBJECT:ultraDateTimeEditor2:SpinButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:Always.
        dateTimeIncrement3:Hours = 1.
        THIS-OBJECT:ultraDateTimeEditor2:SpinIncrement = dateTimeIncrement3.
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance17:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel3:Appearance = appearance17.
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance18:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel4:Appearance = appearance18.
        resources:ApplyResources(THIS-OBJECT:ultraLabel4, "ultraLabel4":U).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance19:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel5:Appearance = appearance19.
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* ultraPanel2 */
        /*  */
        THIS-OBJECT:ultraPanel2:AutoScroll = TRUE.
        /*  */
        /* ultraPanel2.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_SchedulerJobName_Label).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_Active).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter_Label).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay_Label).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime_Label).
        resources:ApplyResources(THIS-OBJECT:ultraPanel2, "ultraPanel2":U).
        THIS-OBJECT:ultraPanel2:Name = "ultraPanel2":U.
        /*  */
        /* ultraPanel3 */
        /*  */
        THIS-OBJECT:ultraPanel3:AutoScroll = TRUE.
        /*  */
        /* ultraPanel3.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:ultraGroupBox2).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:ultraGroupBox3).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections_Label).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_PlanningType_Label).
        THIS-OBJECT:ultraPanel3:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections).
        resources:ApplyResources(THIS-OBJECT:ultraPanel3, "ultraPanel3":U).
        THIS-OBJECT:ultraPanel3:Name = "ultraPanel3":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 0.
        /*  */
        /* SmartSchedulerJobPlanViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel3).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:smartBusinessEntityLookup1.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:smartBusinessEntityLookup1.
        THIS-OBJECT:Name = "SmartSchedulerJobPlanViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_MaxRuntime, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_MaxExecutionDelay, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_JobParamter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMonday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringTuesday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringWednesday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringThursday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFriday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSaturday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSunday, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_WeekInterval, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJanuary, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringFebruary, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMarch, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringApril, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringMay, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJune, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringJuly, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringAugust, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringSeptember, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringOctober, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringNovember, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDecember, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringDaylist, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_RecurringExecutionTimes, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_Active, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJobPlan_DataBaseConnections, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:ultraDateTimeEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox3:PerformLayout().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox2:PerformLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraDateTimeEditor3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDateTimeEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel2:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel2:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel3:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel3:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel3:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.


    /**
     * Purpose: Event handler for the Click event of the button controls.
     *          Checks or unchecks the Weekday or Month check boxes
     * Notes:
     * @param sender The source of the event
     * @param e An EventArgs that contains no event data
     */
    METHOD PUBLIC VOID OnClickHandler(INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        DEFINE VARIABLE oButton AS Infragistics.Win.Misc.UltraButton NO-UNDO.

        IF NOT TYPE-OF (sender, Infragistics.Win.Misc.UltraButton) THEN
            RETURN.

        oButton = CAST (sender, Infragistics.Win.Misc.UltraButton).

        CASE oButton:Name:

            WHEN "btnAll":U THEN
                THIS-OBJECT:SetCheckState (TRUE, SchedulerJobPlanningTypeEnum:Monthly).

            WHEN "btnNone":U THEN
                THIS-OBJECT:SetCheckState (FALSE, SchedulerJobPlanningTypeEnum:Monthly).

            WHEN "btnAllDays":U THEN
                THIS-OBJECT:SetCheckState (TRUE, SchedulerJobPlanningTypeEnum:Weekly).

            WHEN "btnNoneDays":U THEN
                THIS-OBJECT:SetCheckState (FALSE, SchedulerJobPlanningTypeEnum:Weekly).

        END CASE.

    END METHOD.


    /**
     * Purpose: Event handler for the SelectionChanged event ot the PlanningType Control.
     *          Dependend on the PanningType diffent GroupBoxes are shown
     * Notes:
     * @param sender The source of the event
     * @param e An EventArgs that contains no event data
     */
    METHOD PRIVATE VOID OnSelectionChangedHandler (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        ASSIGN
            ultraGroupBox1:Visible = FALSE
            ultraGroupBox2:Visible = FALSE
            ultraGroupBox3:Visible = FALSE
            ultraPanel1:Visible    = FALSE
            .

        IF NOT VALID-OBJECT(ultraComboEditor1:SelectedItem) THEN
            RETURN.

        CASE STRING (UNBOX (ultraComboEditor1:SelectedItem:DataValue)):
            WHEN "Once":U THEN
                ASSIGN
                    ultraGroupBox1:Visible = TRUE.
            WHEN "Weekly":U THEN
                ASSIGN
                    ultraGroupBox2:Visible = TRUE
                    ultraPanel1:Visible    = TRUE
                    .
            WHEN "Monthly":U THEN
                ASSIGN
                    ultraGroupBox3:Visible = TRUE
                    ultraPanel1:Visible    = TRUE
                    .
        END CASE .

    END METHOD .

    /**
     * Purpose: Sets the Checked state for a group of check boxes
     * Notes:
     * @param plCheckState Logical value indicating if the Controls should be checked or not
     * @param oTypeEnum The SchedulerJobPlanningTypeEnum describing which controls to check or uncheck
     */
    METHOD PUBLIC VOID SetCheckState (plCheckState AS LOGICAL,
                                      oTypeEnum    AS SchedulerJobPlanningTypeEnum):

        CASE oTypeEnum:
            WHEN SchedulerJobPlanningTypeEnum:Weekly THEN
                ASSIGN
                    eSmartSchedulerJobPlan_RecurringMonday:Checked    = plCheckState
                    eSmartSchedulerJobPlan_RecurringTuesday:Checked   = plCheckState
                    eSmartSchedulerJobPlan_RecurringWednesday:Checked = plCheckState
                    eSmartSchedulerJobPlan_RecurringThursday:Checked  = plCheckState
                    eSmartSchedulerJobPlan_RecurringFriday:Checked    = plCheckState
                    eSmartSchedulerJobPlan_RecurringSaturday:Checked  = plCheckState
                    eSmartSchedulerJobPlan_RecurringSunday:Checked    = plCheckState
                    .
            WHEN SchedulerJobPlanningTypeEnum:Monthly THEN
                ASSIGN
                    eSmartSchedulerJobPlan_RecurringJanuary:Checked   = plCheckState
                    eSmartSchedulerJobPlan_RecurringFebruary:Checked  = plCheckState
                    eSmartSchedulerJobPlan_RecurringMarch:Checked     = plCheckState
                    eSmartSchedulerJobPlan_RecurringApril:Checked     = plCheckState
                    eSmartSchedulerJobPlan_RecurringMay:Checked       = plCheckState
                    eSmartSchedulerJobPlan_RecurringJune:Checked      = plCheckState
                    eSmartSchedulerJobPlan_RecurringJuly:Checked      = plCheckState
                    eSmartSchedulerJobPlan_RecurringAugust:Checked    = plCheckState
                    eSmartSchedulerJobPlan_RecurringSeptember:Checked = plCheckState
                    eSmartSchedulerJobPlan_RecurringOctober:Checked   = plCheckState
                    eSmartSchedulerJobPlan_RecurringNovember:Checked  = plCheckState
                    eSmartSchedulerJobPlan_RecurringDecember:Checked  = plCheckState
                    .
        END CASE.

        THIS-OBJECT:SetModifyingState ().

    END METHOD.

    /**
     * Purpose: Destructor for the SmartSchedulerJobPlanViewerControl class
     * Notes:
     */
    DESTRUCTOR PUBLIC SmartSchedulerJobPlanViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
