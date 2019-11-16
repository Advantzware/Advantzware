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
    File        : SchedulerRuntime
    Purpose     : Core scheduler runtime
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 17 09:55:38 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.*                                  FROM PROPATH.
USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.Runtime.* FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Scheduler.Runtime.SchedulerRuntime:

    /**
     * Purpose: Returns if the scheduler is executing a job
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY Active AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets the number of seconds (e.g. 300 = 5 minutes) the
     *          scheduler runtime job should keep running
     * Notes:   The currently executed job will not be stopped when the runtime
     *          limit is reached
     */
    DEFINE PUBLIC STATIC PROPERTY KeepRunning AS INTEGER NO-UNDO INIT 300
    GET.
    SET.

    /**
     * Purpose: Gets and sets the number of seconds to wait between two polling
     *          attempts for new jobs when no job had been found previously
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY PauseBetweenPolling AS INTEGER NO-UNDO INIT 5
    GET.
    SET.

    /**
     * Purpose: Returns the currently executed SchedulerJobStatusGuid
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY SchedulerJobStatusGuid AS CHARACTER NO-UNDO INIT ?
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets if verbose output of logging messages is requested
     * Notes:   "verbose" parameter of the PCTRun task
     */
    DEFINE PUBLIC STATIC PROPERTY Verbose AS LOGICAL NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Protected default constructor of the SchedulerRuntime class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED SchedulerRuntime():

    END CONSTRUCTOR.

    /**
     * Purpose: Performs execution of scheduler jobs
     * Notes:
     */
    METHOD PUBLIC STATIC VOID Execute():

        DEFINE VARIABLE oJobStatus AS SchedulerJobStatusDatasetModel     NO-UNDO .
        DEFINE VARIABLE oJob       AS SchedulerJobDatasetModel           NO-UNDO .
        DEFINE VARIABLE oParameter AS GetNextAvailableJobStatusParameter NO-UNDO .
        DEFINE VARIABLE dtStart    AS DATETIME-TZ                        NO-UNDO INIT NOW .
        DEFINE VARIABLE iPause     AS INTEGER                            NO-UNDO .

        ASSIGN iPause = SchedulerRuntime:PauseBetweenPolling .

        MESSAGE "*** Keep Running:       "{&TRAN} SchedulerRuntime:KeepRunning "seconds"{&TRAN} SKIP
                "*** Pause between polls:"{&TRAN} iPause                       "seconds"{&TRAN} SKIP(2) .

        REPEAT ON ERROR UNDO, THROW:

            IF SchedulerRuntime:Verbose THEN
                MESSAGE "*** Fetching next availalbe scheduler job."{&TRAN} .

            oParameter = NEW GetNextAvailableJobStatusParameter(5) .

            oJobStatus = NEW SchedulerJobStatusDatasetModel() .

            oJobStatus:GetNextAvailableJobStatus (oParameter) .

            IF oParameter:SchedulerJobStatusGuid > "":U THEN DO:
                oJobStatus:SmartSchedulerJobStatus:Filter:SchedulerJobStatusGuid:EQ(oParameter:SchedulerJobStatusGuid):Run () .

                IF oJobStatus:SmartSchedulerJobStatus:Available = FALSE THEN DO:

                    UNDO, THROW NEW SchedulerException(SUBSTITUTE ("*** Sanity check: Scheduler job status with GUID &1 has disappeared."{&TRAN},
                                                                   oParameter:SchedulerJobStatusGuid),
                                                       0) .
                END.

                oJob = NEW SchedulerJobDatasetModel (oJobStatus:SmartSchedulerJobStatus:SchedulerJobGuid) .

                IF oJob:SmartSchedulerJob:Available = FALSE THEN DO:

                    UNDO, THROW NEW SchedulerException(SUBSTITUTE ("*** Sanity check: Scheduler job with GUID &1 is not available."{&TRAN},
                                                                   oParameter:SchedulerJobStatusGuid),
                                                       0) .
                END.

                SchedulerRuntime:ExecuteSchedulerJob (oJob, oJobStatus) .

            END.
            ELSE DO:
                IF SchedulerRuntime:Verbose THEN
                    MESSAGE "*** No job available to execute."{&TRAN}.

                IF SchedulerRuntime:KeepRunning = 0 OR INTERVAL (NOW, dtStart, "seconds":U) >= SchedulerRuntime:KeepRunning THEN
                    RETURN .

                IF iPause > 0 THEN
                    PAUSE iPause NO-MESSAGE .
            END.
        END.

    END METHOD.

    /**
     * Purpose: Executes a single SchedulerJob instance
     * Notes:
     * @param poJob The SchedulerJobDatasetModel with the job to execute
     * @param poJobStatus The SchedulerJobStatusDatasetModel with the job instance to execute
     */
    METHOD PUBLIC STATIC VOID ExecuteSchedulerJob (poJob AS SchedulerJobDatasetModel,
                                                   poJobStatus AS SchedulerJobStatusDatasetModel):

        DEFINE VARIABLE oJobCommand    AS ISchedulerJobCommand           NO-UNDO .
        DEFINE VARIABLE oParameter     AS Progress.Lang.Object           NO-UNDO .
        DEFINE VARIABLE lcJobCommand   AS LONGCHAR                       NO-UNDO .
        DEFINE VARIABLE lcParameter    AS LONGCHAR                       NO-UNDO .
        DEFINE VARIABLE cJobStatusGuid AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE oJobStatus     AS SchedulerJobStatusDatasetModel NO-UNDO .

        ASSIGN cJobStatusGuid                          = poJobStatus:SmartSchedulerJobStatus:SchedulerJobStatusGuid
               SchedulerRuntime:SchedulerJobStatusGuid = cJobStatusGuid .

        oJobStatus = NEW SchedulerJobStatusDatasetModel(cJobStatusGuid) .
        oJobStatus:TrackingChanges = TRUE .
        oJobStatus:SmartSchedulerJobStatus:JobStatus = SchedulerJobStatusEnum:Running .
        oJobStatus:SaveChanges().

        IF SchedulerRuntime:Verbose THEN
            MESSAGE SUBSTITUTE ("*** Executing scheduler job status:~n"{&TRAN} +
                                "*** GUID:          &1~n"{&TRAN} +
                                "*** Name:          &2~n"{&TRAN} +
                                "*** Job Name:      &3~n"{&TRAN} +
                                "*** Schedule Time: &4~n~n"{&TRAN},
                                poJobStatus:SmartSchedulerJobStatus:SchedulerJobStatusGuid,
                                poJobStatus:SmartSchedulerJobStatus:JobName,
                                poJobStatus:SmartSchedulerJobStatus:SchedulerJobName,
                                poJobStatus:SmartSchedulerJobStatus:ScheduleDateTime) .

        IF oJobStatus:SmartSchedulerJobStatus:Recurring THEN DO:
            oJobStatus:ScheduleNextJobStatus (NEW ScheduleNextJobStatusParameter
                (poJobStatus:SmartSchedulerJobStatus:SchedulerJobStatusGuid)) .
        END.

        FIX-CODEPAGE (lcJobCommand) = Codepages:UTF-8 .
        FIX-CODEPAGE (lcParameter)  = Codepages:UTF-8 .

        ASSIGN lcJobCommand = poJob:SmartSchedulerJob:JobCommand
               lcParameter  = poJobStatus:SmartSchedulerJobStatus:JobParamter .

        oJobCommand = CAST (Serializable:DeserializeInstance(lcJobCommand),
                            ISchedulerJobCommand) .

        IF lcParameter > "":U THEN
            oParameter = Serializable:DeserializeInstance(lcParameter) .


        DO ON ERROR UNDO, THROW:
            SchedulerRuntime:InvokeSchedulerJobCommand (oJobCommand,
                                                        oParameter) .

            oJobStatus = NEW SchedulerJobStatusDatasetModel(cJobStatusGuid) .
            oJobStatus:TrackingChanges = TRUE .
            oJobStatus:SmartSchedulerJobStatus:FinishedDateTime = NOW .

            IF oJobStatus:SmartSchedulerJobStatus:JobStatus <> SchedulerJobStatusEnum:Cancelled THEN
                oJobStatus:SmartSchedulerJobStatus:JobStatus = SchedulerJobStatusEnum:Finished .
            oJobStatus:SaveChanges().

            IF SchedulerRuntime:Verbose THEN
                MESSAGE "*** Completed."{&TRAN}.

            CATCH err AS Progress.Lang.Error:

                MESSAGE SKIP (1) ErrorHelper:FormattedErrorMessages(err) SKIP (1) .

                oJobStatus = NEW SchedulerJobStatusDatasetModel(cJobStatusGuid) .
                oJobStatus:TrackingChanges = TRUE .
                oJobStatus:SmartSchedulerJobStatus:FinishedDateTime = NOW .
                oJobStatus:SmartSchedulerJobStatus:StatusText = ErrorHelper:FormattedErrorMessages(err) .
                oJobStatus:SmartSchedulerJobStatus:JobStatus = SchedulerJobStatusEnum:Failed .
                oJobStatus:SaveChanges().

            END CATCH.

            FINALLY:
                IF SchedulerRuntime:Verbose THEN
                    MESSAGE "*** Done executing job."{&TRAN}.
            END FINALLY.
        END.

        FINALLY:
            SchedulerRuntime:SchedulerJobStatusGuid = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Calls the Start method of the
     * Notes:   Moved outside of ExecuteSchedulerJob for ability to handle
     *          STOP conditions
     * @param poJobCommand The ISchedulerJobCommand instance to execute
     * @param poParameter The parameter object for the job instance
     */
    METHOD PROTECTED STATIC VOID InvokeSchedulerJobCommand (poJobCommand AS ISchedulerJobCommand,
                                                            poParameter AS Progress.Lang.Object):

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException():

            poJobCommand:Start (poParameter) .

        END. /* on stop */

    END METHOD.

    /**
     * Purpose: Tests if the current Job is cancelled
     * Notes:   When the Job was cancelled, the SmartSchedulerJobStatus record
     *          will be set to the Cancelled status immediately. The calling
     *          routine is expected to leave promptly
     * @return Logical value indicating if the Job was cancelled
     */
    METHOD PUBLIC STATIC LOGICAL IsCancelled():

        DEFINE VARIABLE oJobStatus     AS SchedulerJobStatusDatasetModel NO-UNDO .

        oJobStatus = NEW SchedulerJobStatusDatasetModel () .

        IF NOT oJobStatus:SmartSchedulerJobStatus:CanFind (SUBSTITUTE ("where eSmartSchedulerJobStatus.SchedulerJobStatusGuid = &1 and eSmartSchedulerJobStatus.Cancel = true":U,
                                                                       QUOTER (SchedulerRuntime:SchedulerJobStatusGuid))) THEN
            RETURN FALSE .

        oJobStatus:SmartSchedulerJobStatus:Filter:SchedulerJobStatusGuid:EQ (SchedulerRuntime:SchedulerJobStatusGuid):Run() .
        oJobStatus:TrackingChanges = TRUE .
        oJobStatus:SmartSchedulerJobStatus:JobStatus = SchedulerJobStatusEnum:Cancelled .
        oJobStatus:SmartSchedulerJobStatus:Cancel    = FALSE .
        oJobStatus:SaveChanges().

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Updates the Job's status
     * Notes:
     * @param pcStatusText The status text
     * @param piPercentFinished The percentage to indicate the job completion
     */
    METHOD PUBLIC STATIC VOID UpdateJobStatus (pcStatusText AS CHARACTER,
                                               piPercentFinished AS INTEGER):

        DEFINE VARIABLE oJobStatus     AS SchedulerJobStatusDatasetModel NO-UNDO .

        oJobStatus = NEW SchedulerJobStatusDatasetModel () .
        oJobStatus:UpdateJobStatus (NEW UpdateJobStatusParameter (SchedulerRuntime:SchedulerJobStatusGuid,
                                                                  pcStatusText,
                                                                  piPercentFinished)) .

        IF SchedulerRuntime:Verbose THEN DO:
            IF pcStatusText > "":U AND piPercentFinished <> ? THEN
                MESSAGE SUBSTITUTE ("&1%: &2":U, piPercentFinished, pcStatusText) .
            ELSE IF pcStatusText > "":U THEN
                MESSAGE pcStatusText .
            ELSE
                MESSAGE SUBSTITUTE ("&1%":U, piPercentFinished) .
        END.

    END METHOD.

    /**
     * Purpose: Updates the Job's status
     * Notes:
     * @param pcStatusText The status text
     */
    METHOD PUBLIC STATIC VOID UpdateJobStatus (pcStatusText AS CHARACTER):

        SchedulerRuntime:UpdateJobStatus (pcStatusText, ?) .

    END METHOD.

    /**
     * Purpose: Updates the Job's status
     * Notes:
     * @param piPercentFinished The percentage to indicate the job completion
     */
    METHOD PUBLIC STATIC VOID UpdateJobStatus (piPercentFinished AS INTEGER):

        SchedulerRuntime:UpdateJobStatus (?, piPercentFinished) .

    END METHOD.

END CLASS.
