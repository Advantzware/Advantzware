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
    File        : ISchedulerService
    Purpose     : Interface for der Scheduler Service
    Syntax      :
    Description : This Interfaces exposes the scheduler control to an
                  application developer
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 12 15:21:03 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.Scheduler.ISchedulerService:

    /**
     * Purpose: Determines the next planned execution time for the
     *          given job plan
     * Notes:   Only supported for recurring job plans
     * @param pcSchedulerJobPlanGuid The job plan to return the next execution time for
     * @param pdtPreviousExecution The date-time-tz value identifying when the job was previously executed - or ? when the Job has neven been executed so far
     * @return The date and time when the job plan is due for the next execution
     */
    METHOD PUBLIC DATETIME-TZ GetNextExecutionTime (pcSchedulerJobPlanGuid AS CHARACTER,
                                                    pdtPreviousExecution AS DATETIME-TZ) .

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
     * @return The date and time when the job plan is due for the next execution     * @return The date and time when the job plan is due for the next execution
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
                                                    pdtValidTo AS DATETIME-TZ) .

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
                                                    poJobParameter AS ISerializable).

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
                                                     poJobParameter AS ISerializable).

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
                                                    poJobParameter AS ISerializable).

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
                                                    poJobParameter AS ISerializable).

    /**
     * Purpose: Sets the signal to cancel a JobStatus
     * Notes:   Requires the JobStatus of Runnig
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusCancelSignal (pcSchedulerJobStatusGuid AS CHARACTER).

    /**
     * Purpose: Sets a JobStatus OnHold. A job on hold can be later restarted
     * Notes:   Requires the JobStatus of Scheduled
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusOnHold (pcSchedulerJobStatusGuid AS CHARACTER).

    /**
     * Purpose: Resumes a JobStatus that is on hold or to be cancelled
     * Notes:   Requires the JobStatus of OnHold or to be cancelled (Running and Cancel = TRUE)
     * @param pcSchedulerJobStatusGuid The GUID of the SchedulerJobStatus to put on hold
     */
    METHOD PUBLIC VOID SetJobStatusResumed (pcSchedulerJobStatusGuid AS CHARACTER).

END INTERFACE.
