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
    File        : SchedulerService
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 2 13:45:53 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.Framework.Session.*        FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerService
    IMPLEMENTS ISchedulerService:

    /**
     * Purpose: Determines the next planned execution time for the
     *          given job plan
     * Notes:   Only supported for recurring job plans
     * @param pcSchedulerJobPlanGuid The job plan to return the next execution time for
     * @param pdtPreviousExecution The date-time-tz value identifying when the job was previously executed - or ? when the Job has neven been executed so far
     * @return The date and time when the job plan is due for the next execution
     */
    METHOD PUBLIC DATETIME-TZ GetNextExecutionTime (pcSchedulerJobPlanGuid AS CHARACTER,
                                                    pdtPreviousExecution AS DATETIME-TZ):

        DEFINE VARIABLE oJobPlan AS SchedulerJobPlanDatasetModel NO-UNDO .

        oJobPlan = NEW SchedulerJobPlanDatasetModel (pcSchedulerJobPlanGuid) .

        IF NOT oJobPlan:SmartSchedulerJobPlan:Available THEN
            UNDO, THROW NEW SchedulerException ("Invalid Scheduler Job Plan."{&TRAN}, 0) .

        RETURN THIS-OBJECT:GetNextExecutionTime (pdtPreviousExecution,
                                                 oJobPlan:SmartSchedulerJobPlan:PlanningType,
                                                 oJobPlan:SmartSchedulerJobPlan:WeekInterval,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringMonday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringTuesday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringWednesday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringThursday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringFriday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringSaturday,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringSunday,

                                                 oJobPlan:SmartSchedulerJobPlan:RecurringJanuary,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringFebruary,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringMarch,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringApril,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringMay,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringJune,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringJuly,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringAugust,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringSeptember,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringOctober,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringNovember,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringDecember,
                                                 oJobPlan:SmartSchedulerJobPlan:RecurringDaylist,

                                                 oJobPlan:SmartSchedulerJobPlan:RecurringExecutionTimes,

                                                 oJobPlan:SmartSchedulerJobPlan:ValidFrom,
                                                 oJobPlan:SmartSchedulerJobPlan:ValidTo) .

    END METHOD .

    /**
     * Purpose: Determines the next planned execution time for the
     *          given job plan
     * Notes:   Only supported for recurring job plans. This method performs
     *          the actual work. It is not accessing the Business Entity to
     *          retrieve the Job Plan parameters. This allows to call into this
     *          method from within the Job Plan Business Entity
     * @param pdtPreviousExecution The date-time-tz value identifying when the job was previously executed - or ? when the Job has neven been executed so far
     * @param poPlanningType The SchedulerJobPlanningTypeEnum value
     * @param piWeekInterval The week interval
     * @param plRecurringMonday Plan includes Monday
     * @param plRecurringTuesday Plan includes Tuesday
     * @param plRecurringWednesday Plan includes Wednesday
     * @param plRecurringThursday Plan includes Thursday
     * @param plRecurringFriday Plan includes Friday
     * @param plRecurringSaturday Plan includes Saturday
     * @param plRecurringSunday Plan includes Sunday
     * @param plRecurringJanuary The plan includes January
     * @param plRecurringFebruary The plan includes February
     * @param plRecurringMarch The plan includes March
     * @param plRecurringApril The plan includes April
     * @param plRecurringMay The plan includes May
     * @param plRecurringJune The plan includes June
     * @param plRecurringJuly The plan includes July
     * @param plRecurringAugust The plan includes August
     * @param plRecurringSeptember The plan includes September
     * @param plRecurringOctober The plan includes October
     * @param plRecurringNovember The plan includes November
     * @param plRecurringDecember The plan includes December
     * @param pcRecurringDaylist The list of days of month (relative to 1st or last day)
     * @param pcRecurringExecutionTimes The list of planning times
     * @param pdtValidFrom The valid from date of the job plan
     * @param pdtValidTo The valid to date of the job plan
     * @return The date and time when the job plan is due for the next execution
     */
    METHOD PUBLIC DATETIME-TZ GetNextExecutionTime (pdtPreviousExecution AS DATETIME-TZ,
                                                    poPlanningType AS SchedulerJobPlanningTypeEnum,
                                                    piWeekInterval AS INTEGER,
                                                    plRecurringMonday AS LOGICAL,
                                                    plRecurringTuesday AS LOGICAL,
                                                    plRecurringWednesday AS LOGICAL,
                                                    plRecurringThursday AS LOGICAL,
                                                    plRecurringFriday AS LOGICAL,
                                                    plRecurringSaturday AS LOGICAL,
                                                    plRecurringSunday AS LOGICAL,

                                                    plRecurringJanuary AS LOGICAL,
                                                    plRecurringFebruary AS LOGICAL,
                                                    plRecurringMarch AS LOGICAL,
                                                    plRecurringApril AS LOGICAL,
                                                    plRecurringMay AS LOGICAL,
                                                    plRecurringJune AS LOGICAL,
                                                    plRecurringJuly AS LOGICAL,
                                                    plRecurringAugust AS LOGICAL,
                                                    plRecurringSeptember AS LOGICAL,
                                                    plRecurringOctober AS LOGICAL,
                                                    plRecurringNovember AS LOGICAL,
                                                    plRecurringDecember AS LOGICAL,
                                                    pcRecurringDaylist AS CHARACTER,

                                                    pcRecurringExecutionTimes AS CHARACTER,

                                                    pdtValidFrom AS DATETIME-TZ,
                                                    pdtValidTo AS DATETIME-TZ):

        DEFINE VARIABLE dtTemp AS DATETIME-TZ NO-UNDO.

        IF pdtPreviousExecution = ? OR pdtPreviousExecution < NOW THEN
            ASSIGN pdtPreviousExecution = NOW .

        IF pdtValidFrom <> ? AND pdtValidFrom > pdtPreviousExecution THEN
            ASSIGN pdtPreviousExecution = pdtValidFrom .

        CASE poPlanningType:
            WHEN SchedulerJobPlanningTypeEnum:Monthly THEN DO:
                ASSIGN dtTemp =  GetNextExecutionTimeMonthly (pdtPreviousExecution,
                                                              plRecurringJanuary,
                                                              plRecurringFebruary,
                                                              plRecurringMarch,
                                                              plRecurringApril,
                                                              plRecurringMay,
                                                              plRecurringJune,
                                                              plRecurringJuly,
                                                              plRecurringAugust,
                                                              plRecurringSeptember,
                                                              plRecurringOctober,
                                                              plRecurringNovember,
                                                              plRecurringDecember,
                                                              pcRecurringDaylist,
                                                              pcRecurringExecutionTimes) .

                IF pdtValidTo <> ? AND pdtValidTo < dtTemp THEN
                    RETURN ? .
                ELSE
                    RETURN dtTemp .
            END.
            WHEN SchedulerJobPlanningTypeEnum:Weekly THEN DO:
                ASSIGN dtTemp = GetNextExecutionTimeWeekly (pdtPreviousExecution,
                                                            piWeekInterval,
                                                            plRecurringMonday,
                                                            plRecurringTuesday,
                                                            plRecurringWednesday,
                                                            plRecurringThursday,
                                                            plRecurringFriday,
                                                            plRecurringSaturday,
                                                            plRecurringSunday,
                                                            pcRecurringExecutionTimes) .

                IF pdtValidTo <> ? AND pdtValidTo < dtTemp THEN
                    RETURN ? .
                ELSE
                    RETURN dtTemp .
            END.

            OTHERWISE
                UNDO, THROW NEW SchedulerException ("Unsupported planning type."{&TRAN}, 0) .

        END CASE .

    END METHOD.

    /**
     * Purpose: Determines the next planned execution time for the
     *          given job plan based on a weekly planning pattern
     * Notes:   Only supported for recurring job plans
     * @param pdtPreviousExecution The date-time-tz value identifying when the job was previously executed - or ? when the Job has neven been executed so far
     * @param piWeekInterval The week interval
     * @param plRecurringMonday Plan includes Monday
     * @param plRecurringTuesday Plan includes Tuesday
     * @param plRecurringWednesday Plan includes Wednesday
     * @param plRecurringThursday Plan includes Thursday
     * @param plRecurringFriday Plan includes Friday
     * @param plRecurringSaturday Plan includes Saturday
     * @param plRecurringSunday Plan includes Sunday
     * @param pcRecurringExecutionTimes The list of planning times
     * @return The date and time when the job plan is due for the next execution
     */
    METHOD PROTECTED DATETIME-TZ GetNextExecutionTimeWeekly (pdtPreviousExecution AS DATETIME-TZ,
                                                             piWeekInterval AS INTEGER,
                                                             plRecurringMonday AS LOGICAL,
                                                             plRecurringTuesday AS LOGICAL,
                                                             plRecurringWednesday AS LOGICAL,
                                                             plRecurringThursday AS LOGICAL,
                                                             plRecurringFriday AS LOGICAL,
                                                             plRecurringSaturday AS LOGICAL,
                                                             plRecurringSunday AS LOGICAL,
                                                             pcRecurringExecutionTimes AS CHARACTER):

        DEFINE VARIABLE dtTemp AS DATETIME-TZ NO-UNDO.

        /* Next execution still on the same day */
        ASSIGN dtTemp = SchedulerWeeklyPlanningHelper:NextPlanningOnSameDay (pdtPreviousExecution,
                                                                             pcRecurringExecutionTimes) .

        IF dtTemp <> ? THEN
            RETURN dtTemp .

        /* Next day in same week? */
        ASSIGN dtTemp = SchedulerWeeklyPlanningHelper:NextPlanningInSameWeek (pdtPreviousExecution,
                                                                              plRecurringMonday,
                                                                              plRecurringTuesday,
                                                                              plRecurringWednesday,
                                                                              plRecurringThursday,
                                                                              plRecurringFriday,
                                                                              plRecurringSaturday,
                                                                              plRecurringSunday,
                                                                              pcRecurringExecutionTimes) .

        IF dtTemp <> ? THEN
            RETURN dtTemp .

        /* In X weeks */
        RETURN SchedulerWeeklyPlanningHelper:NextPlanningInNextWeek (pdtPreviousExecution,
                                                                     piWeekInterval,
                                                                     plRecurringMonday,
                                                                     plRecurringTuesday,
                                                                     plRecurringWednesday,
                                                                     plRecurringThursday,
                                                                     plRecurringFriday,
                                                                     plRecurringSaturday,
                                                                     plRecurringSunday,
                                                                     pcRecurringExecutionTimes) .

    END METHOD .

    /**
     * Purpose: Determines the next planned execution time for the
     *          given job plan based on a weekly planning pattern
     * Notes:   Only supported for recurring job plans
     * @param pdtPreviousExecution The date-time-tz value identifying when the job was previously executed - or ? when the Job has neven been executed so far
     * @param plRecurringJanuary The plan includes January
     * @param plRecurringFebruary The plan includes February
     * @param plRecurringMarch The plan includes March
     * @param plRecurringApril The plan includes April
     * @param plRecurringMay The plan includes May
     * @param plRecurringJune The plan includes June
     * @param plRecurringJuly The plan includes July
     * @param plRecurringAugust The plan includes August
     * @param plRecurringSeptember The plan includes September
     * @param plRecurringOctober The plan includes October
     * @param plRecurringNovember The plan includes November
     * @param plRecurringDecember The plan includes December
     * @param pcRecurringDaylist The list of days of month (relative to 1st or last day)
     * @param pcRecurringExecutionTimes The list of planning times
     * @return The date and time when the job plan is due for the next execution
     */
    METHOD PROTECTED DATETIME-TZ GetNextExecutionTimeMonthly (pdtPreviousExecution AS DATETIME-TZ,
                                                              plRecurringJanuary AS LOGICAL,
                                                              plRecurringFebruary AS LOGICAL,
                                                              plRecurringMarch AS LOGICAL,
                                                              plRecurringApril AS LOGICAL,
                                                              plRecurringMay AS LOGICAL,
                                                              plRecurringJune AS LOGICAL,
                                                              plRecurringJuly AS LOGICAL,
                                                              plRecurringAugust AS LOGICAL,
                                                              plRecurringSeptember AS LOGICAL,
                                                              plRecurringOctober AS LOGICAL,
                                                              plRecurringNovember AS LOGICAL,
                                                              plRecurringDecember AS LOGICAL,
                                                              pcRecurringDaylist AS CHARACTER,
                                                              pcRecurringExecutionTimes AS CHARACTER):

        DEFINE VARIABLE dtTemp AS DATETIME-TZ NO-UNDO.

        /* Next execution still on the same day */
        ASSIGN dtTemp = SchedulerWeeklyPlanningHelper:NextPlanningOnSameDay (pdtPreviousExecution,
                                                                             pcRecurringExecutionTimes) .

        IF dtTemp <> ? THEN
            RETURN dtTemp .

        /* Next execution still in the same month */
        ASSIGN dtTemp = SchedulerMonthlyPlanningHelper:NextPlanningInSameMonth (pdtPreviousExecution,
                                                                                pcRecurringDaylist,
                                                                                pcRecurringExecutionTimes) .

        IF dtTemp <> ? THEN
            RETURN dtTemp .

        /* Next execution in next month */
        RETURN SchedulerMonthlyPlanningHelper:NextPlanningInNextMonth (pdtPreviousExecution,
                                                                       plRecurringJanuary,
                                                                       plRecurringFebruary,
                                                                       plRecurringMarch,
                                                                       plRecurringApril,
                                                                       plRecurringMay,
                                                                       plRecurringJune,
                                                                       plRecurringJuly,
                                                                       plRecurringAugust,
                                                                       plRecurringSeptember,
                                                                       plRecurringOctober,
                                                                       plRecurringNovember,
                                                                       plRecurringDecember,
                                                                       pcRecurringDaylist,
                                                                       pcRecurringExecutionTimes) .
    END METHOD .

    /**
     * Purpose: Schedules a Job for scheduled one time execution
     * Notes:   Only creates an entry in the SmartSchedulerJobStatus table. An entry
     *          in the SmartSchedulerJobPlan DB table is not required for one time
     *          execution jobs
     * @param pcSchedulerJobName The name of the scheduler job to execute
     * @param pcJobName The name for the SchedulerJobStatus entry
     * @param pdtScheduleDateTime The date/time when the job schould be executed
     * @param poJobParameter The serializable Job Parameter instance
     * @return The IScheduleJobResponse describing the identifiers of the planned job
     */
    METHOD PUBLIC IScheduleJobResponse ScheduleJob (pcSchedulerJobName AS CHARACTER,
                                                    pcJobName AS CHARACTER,
                                                    pdtScheduleDateTime AS DATETIME-TZ,
                                                    poJobParameter AS ISerializable):

        DEFINE VARIABLE oSchedulerJob       AS SchedulerJobDatasetModel       NO-UNDO .
        DEFINE VARIABLE oSchedulerJobStatus AS SchedulerJobStatusDatasetModel NO-UNDO .
        DEFINE VARIABLE lcContext           AS LONGCHAR                       NO-UNDO .
        DEFINE VARIABLE cJobName            AS CHARACTER                      NO-UNDO .

        Assert:NotNullOrEmpty (pcSchedulerJobName) .
        Assert:NotNull(pdtScheduleDateTime) .

        oSchedulerJob = NEW SchedulerJobDatasetModel() .
        oSchedulerJob:SmartSchedulerJob:Filter:SchedulerJobName:EQ(pcSchedulerJobName):Run().

        IF NOT oSchedulerJob:SmartSchedulerJob:Available THEN
            UNDO, THROW NEW InvalidSchedulerJobNameException("Invalid scheduler job name."{&TRAN}, 0) .

        oSchedulerJobStatus = NEW SchedulerJobStatusDatasetModel() .
        oSchedulerJobStatus:TrackingChanges = TRUE .
        oSchedulerJobStatus:SmartSchedulerJobStatus:Create() .

        IF pcJobName > "":U THEN
            ASSIGN cJobName = pcJobName .
        ELSE
            ASSIGN cJobName = oSchedulerJob:SmartSchedulerJob:SchedulerJobName .

        ASSIGN oSchedulerJobStatus:SmartSchedulerJobStatus:SchedulerJobGuid    = oSchedulerJob:SmartSchedulerJob:SchedulerJobGuid
               oSchedulerJobStatus:SmartSchedulerJobStatus:ScheduleDateTime    = pdtScheduleDateTime
               oSchedulerJobStatus:SmartSchedulerJobStatus:JobName             = cJobName
               oSchedulerJobStatus:SmartSchedulerJobStatus:Recurring           = FALSE
               oSchedulerJobStatus:SmartSchedulerJobStatus:JobStatus           = SchedulerJobStatusEnum:Scheduled
               oSchedulerJobStatus:SmartSchedulerJobStatus:StatusText          = ""
               oSchedulerJobStatus:SmartSchedulerJobStatus:Percentage          = 0
               oSchedulerJobStatus:SmartSchedulerJobStatus:MaxExecutionDelay   = 0
               oSchedulerJobStatus:SmartSchedulerJobStatus:Cancel              = FALSE
               oSchedulerJobStatus:SmartSchedulerJobStatus:StartDateTime       = ?
               oSchedulerJobStatus:SmartSchedulerJobStatus:FinishedDateTime    = ?
               oSchedulerJobStatus:SmartSchedulerJobStatus:JobParamter         = poJobParameter:Serialize()
               oSchedulerJobStatus:SmartSchedulerJobStatus:UserGuid            = SessionManager:UserKey
               oSchedulerJobStatus:SmartSchedulerJobStatus:DataBaseConnections = ?
               .

        IF VALID-HANDLE (SessionManager:ContextDataset) THEN DO:

            ASSIGN
                ContextWrapper:ClientPrincipal              = SessionManager:ClientPrincipal
                ContextWrapper:LanguageDisplayName          = SessionManager:LanguageDisplayName
                ContextWrapper:LanguageIsoCode              = SessionManager:LanguageIsoCode
                ContextWrapper:LanguageKey                  = SessionManager:LanguageKey
                ContextWrapper:LoginCompanyKey              = SessionManager:LoginCompanyKey
                ContextWrapper:LoginCompanyName             = SessionManager:LoginCompanyName
                ContextWrapper:LoginCompanyReferenceChar    = SessionManager:LoginCompanyReferenceChar
                ContextWrapper:LoginCompanyReferenceDecimal = SessionManager:LoginCompanyReferenceDecimal
                ContextWrapper:LoginCompanyReferenceInteger = SessionManager:LoginCompanyReferenceInteger
                ContextWrapper:LoginCompanyShortName        = SessionManager:LoginCompanyShortName
                ContextWrapper:ServerLogEntryTypes          = SessionManager:ServerLogEntryTypes
                ContextWrapper:ServerDebugMode              = SessionManager:ServerDebugMode
                ContextWrapper:ServerCustomLogEntries       = SessionManager:ServerCustomLogEntries
                ContextWrapper:UserName                     = SessionManager:UserName
                ContextWrapper:UserKey                      = SessionManager:UserKey
                ContextWrapper:UserGroupKeys                = SessionManager:UserGroupKeys
                ContextWrapper:UserGroupNames               = SessionManager:UserGroupNames
                .

            FIX-CODEPAGE (lcContext) = Codepages:UTF-8 .

            /* Did we change the context dataset? */
            SessionManager:ContextDataset:WRITE-XML ("longchar":U,
                                                     lcContext,
                                                     FALSE,
                                                     Codepages:UTF-8) .

            ASSIGN oSchedulerJobStatus:SmartSchedulerJobStatus:ContextDataSet = lcContext .
        END.

        oSchedulerJobStatus:DatasetHandle::eSmartSchedulerJobStatus::ClientPrincipal = SessionManager:ClientPrincipal .

        oSchedulerJobStatus:SaveChanges() .

        RETURN NEW ScheduleJobResponse (oSchedulerJob:SmartSchedulerJob:SchedulerJobGuid,
                                        oSchedulerJobStatus:SmartSchedulerJobStatus:SchedulerJobStatusGuid) .

    END METHOD .

    /**
     * Purpose: Schedules a Job for scheduled one time execution
     * Notes:   Only creates an entry in the SmartSchedulerJobStatus table. An entry
     *          in the SmartSchedulerJobPlan DB table is not required for one time
     *          execution jobs
     * @param pcSchedulerJobName The name of the scheduler job to execute
     * @param pdtScheduleDateTime The date/time when the job schould be executed
     * @param poJobParameter The serializable Job Parameter instance
     * @return The IScheduleJobResponse describing the identifiers of the planned job
     */
    METHOD PUBLIC IScheduleJobResponse ScheduleJob  (pcSchedulerJobName AS CHARACTER,
                                                     pdtScheduleDateTime AS DATETIME-TZ,
                                                     poJobParameter AS ISerializable):

        RETURN THIS-OBJECT:ScheduleJob(pcSchedulerJobName, ?, pdtScheduleDateTime, poJobParameter) .

    END METHOD .

    /**
     * Purpose: Schedules a Job for direct one time execution
     * Notes:   Only creates an entry in the SmartSchedulerJobStatus table. An entry
     *          in the SmartSchedulerJobPlan DB table is not required for one time
     *          execution jobs
     * @param pcSchedulerJobName The name of the scheduler job to execute
     * @param poJobParameter The serializable Job Parameter instance
     * @return The IScheduleJobResponse describing the identifiers of the planned job
     */
    METHOD PUBLIC IScheduleJobResponse ScheduleJob (pcSchedulerJobName AS CHARACTER,
                                                    poJobParameter AS ISerializable):

        RETURN THIS-OBJECT:ScheduleJob(pcSchedulerJobName, ?, NOW, poJobParameter) .

    END METHOD .

    /**
     * Purpose: Schedules a Job for direct one time execution
     * Notes:   Only creates an entry in the SmartSchedulerJobStatus table. An entry
     *          in the SmartSchedulerJobPlan DB table is not required for one time
     *          execution jobs
     * @param pcSchedulerJobName The name of the scheduler job to execute
     * @param pcJobName The name for the SchedulerJobStatus entry
     * @param poJobParameter The serializable Job Parameter instance
     * @return The IScheduleJobResponse describing the identifiers of the planned job
     */
    METHOD PUBLIC IScheduleJobResponse ScheduleJob (pcSchedulerJobName AS CHARACTER,
                                                    pcJobName AS CHARACTER,
                                                    poJobParameter AS ISerializable):

        RETURN THIS-OBJECT:ScheduleJob(pcSchedulerJobName, pcJobName, NOW, poJobParameter) .

    END METHOD .

    /**
     * Purpose: Sets the signal to cancel a JobStatus
     * Notes:   Requires the JobStatus of Runnig
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusCancelSignal (pcSchedulerJobStatusGuid AS CHARACTER):

        DEFINE VARIABLE oJobStatusModel AS SchedulerJobStatusDatasetModel NO-UNDO .

        oJobStatusModel = NEW SchedulerJobStatusDatasetModel() .
        oJobStatusModel:SetJobStatus (NEW SetJobStatusParameter (pcSchedulerJobStatusGuid,
                                                                 SchedulerJobStatusEnum:Cancelled)) .

    END METHOD .

    /**
     * Purpose: Sets a JobStatus OnHold. A job on hold can be later restarted
     * Notes:   Requires the JobStatus of Scheduled
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusOnHold (pcSchedulerJobStatusGuid AS CHARACTER):

        DEFINE VARIABLE oJobStatusModel AS SchedulerJobStatusDatasetModel NO-UNDO .

        oJobStatusModel = NEW SchedulerJobStatusDatasetModel() .
        oJobStatusModel:SetJobStatus (NEW SetJobStatusParameter (pcSchedulerJobStatusGuid,
                                                                 SchedulerJobStatusEnum:OnHold)) .

    END METHOD .

    /**
     * Purpose: Resumes a JobStatus that is on hold or to be cancelled
     * Notes:   Requires the JobStatus of OnHold or to be cancelled (Running and Cancel = TRUE)
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusResumed (pcSchedulerJobStatusGuid AS CHARACTER):

        DEFINE VARIABLE oJobStatusModel AS SchedulerJobStatusDatasetModel NO-UNDO .

        oJobStatusModel = NEW SchedulerJobStatusDatasetModel() .
        oJobStatusModel:SetJobStatus (NEW SetJobStatusParameter (pcSchedulerJobStatusGuid,
                                                                 SchedulerJobStatusEnum:Scheduled)) .

    END METHOD .

END CLASS.
