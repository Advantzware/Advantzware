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
    File        : SchedulerJobStatusDataAccess
    Purpose     : Data Access Component
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 19:11:37
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH.
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobStatus.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE QUERY qry_SmartSchedulerJobStatusSmart FOR SmartSchedulerJobStatus, SmartSchedulerJob .
    DEFINE PRIVATE DATA-SOURCE src_SmartSchedulerJobStatusSmart FOR QUERY qry_SmartSchedulerJobStatusSmart .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    /**
     * Purpose: Constructor for the SchedulerJobStatusDataAccess class
     * Notes:   Used by the BusinessEntity, will receive the handle of the dataset
     *          instance form the BusinessEntity as a parameter. Permanently BIND's
     *          to that dataset using the PRIVATE BindDataset Method.
     * @param phDataset The handle of the Business Entity dataset
     */
    CONSTRUCTOR PUBLIC SchedulerJobStatusDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        THIS-OBJECT:AssignSkipList = "SchedulerJobStatusGuid":U.

        /* Mike Fechner, Consultingwerk Ltd. 01.07.2014
           SCL-334: By default, prepare queries of joined data source
                    queries. This behavior is the new default, and should
                    only be set to FALSE, when customers are already manipulating
                    the data-source query in code (e.g. QUERY-PREPARE in a BEFORE-FILL
                    call back) */
        THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE .

        /* Allows to force applying the Source Default Query to child tables
           even when no filter expression was provided by the consumer */
        /* THIS-OBJECT:ForceApplyQueryOnChildTables = FALSE . */

    END CONSTRUCTOR.

    /**
     * Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
     *          Invoked in FetchData and SaveChanges
     */
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSchedulerJobStatus:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartSchedulerJobStatus:HANDLE,
                                      DATA-SOURCE src_SmartSchedulerJobStatusSmart:HANDLE, "
                                      SchedulerJobStatusGuid,SmartSchedulerJobStatus.SchedulerJobStatusGuid,
                                      SchedulerJobGuid,SmartSchedulerJobStatus.SchedulerJobGuid,
                                      SchedulerJobPlanGuid,SmartSchedulerJobStatus.SchedulerJobPlanGuid,
                                      ScheduleDateTime,SmartSchedulerJobStatus.ScheduleDateTime,
                                      JobName,SmartSchedulerJobStatus.JobName,
                                      Recurring,SmartSchedulerJobStatus.Recurring,
                                      JobStatus,SmartSchedulerJobStatus.JobStatus,
                                      StatusText,SmartSchedulerJobStatus.StatusText,
                                      Percentage,SmartSchedulerJobStatus.Percentage,
                                      MaxExecutionDelay,SmartSchedulerJobStatus.MaxExecutionDelay,
                                      Cancel,SmartSchedulerJobStatus.Cancel,
                                      StartDateTime,SmartSchedulerJobStatus.StartDateTime,
                                      FinishedDateTime,SmartSchedulerJobStatus.FinishedDateTime,
                                      JobParamter,SmartSchedulerJobStatus.JobParamter,
                                      ContextDataSet,SmartSchedulerJobStatus.ContextDataSet,
                                      UserGuid,SmartSchedulerJobStatus.UserGuid,
                                      DataBaseConnections,SmartSchedulerJobStatus.DataBaseConnections,
                                      ClientPrincipal,SmartSchedulerJobStatus.ClientPrincipal,
                                      SchedulerJobName,SmartSchedulerJob.SchedulerJobName,
                                      Description,SmartSchedulerJob.Description":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /**
     * Purpose: Do nothing, just receive the parameter and bind to the received
     *          Dataset reference
     * Notes:   Invoked from the class constructor
     * @param dsSchedulerJobStatus DATASET to bind to
     */
    METHOD PRIVATE VOID BindDataset (DATASET dsSchedulerJobStatus BIND):
        /* NOOP */
    END METHOD.

    /**
     * Purpose: Cleanup of the SmartSchedulerJobStatus records.
     * Notes:   Deletes the finished and erroneous SmartScheduler JobStatus records older than a given date
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID CleanupJobStatus (poParameter AS CleanupJobStatusParameter):

        /* delete "Failed" SmartSchedulerJobStatus records older then 'x' days */
        IF poParameter:DeleteFailedFrom > 0 THEN
            THIS-OBJECT:DeleteSmartSchedulerJobStatus (TODAY - poParameter:DeleteFailedFrom,
                                                       SchedulerJobStatusEnum:Failed).

        /* delete "Finished" SmartSchedulerJobStatus records older then 'x' days */
        IF poParameter:DeleteFinishedFrom > 0 THEN
            THIS-OBJECT:DeleteSmartSchedulerJobStatus (TODAY - poParameter:DeleteFinishedFrom,
                                                       SchedulerJobStatusEnum:Finished).

        /* delete "Finished" SmartSchedulerJobStatus records older then 'x' days */
        IF poParameter:DeleteCancelledFrom > 0 THEN
            THIS-OBJECT:DeleteSmartSchedulerJobStatus (TODAY - poParameter:DeleteCancelledFrom,
                                                       SchedulerJobStatusEnum:Cancelled).

        /* delete "Finished" SmartSchedulerJobStatus records older then 'x' days */
        IF poParameter:DeleteRunningFrom > 0 THEN
            THIS-OBJECT:DeleteSmartSchedulerJobStatus (TODAY - poParameter:DeleteRunningFrom,
                                                       SchedulerJobStatusEnum:Running).

    END METHOD.


    /**
     * Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
     *          as needed
     * Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
     *          Invoked in FetchData, note that the callback methods need to be
     *          PUBLIC and need to have the Dataset defined as an INPUT Parameter.
     *          Errors thrown from or raised in the FILL call-back methods of the
     *          DataAccess class (SET-CALLBACK) will stop the Dataset from getting
     *          filled / filled further. However errors thrown from the call-back
     *          methods are not thrown from the FILL() method further up into a
     *          catch block or to the client.
     *          See http://knowledgebase.progress.com/articles/Article/P188375
     * Sample:  BUFFER eCustomer:SET-CALLBACK (Consultingwerk.DatasetBufferCallbackEnum:AfterFill,
     *                                         "eCustomerAfterFill":U) .
     */
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /**
     * Purpose: deletes the SmartSchedulerJobStatus of a givin list of JobStatus and a givin date
     * Notes:
     * @param pdDateToDelete Date from which the JobStatus records gets deleted
     * @param poJobStatus SchedulerJobStatusEnum Object to Filter on JobStatus
     */
    METHOD PRIVATE VOID DeleteSmartSchedulerJobStatus (pdDateToDelete AS DATETIME-TZ,
                                                       poJobStatus    AS SchedulerJobStatusEnum):

        DEFINE QUERY qQuery FOR SmartSchedulerJobStatus .

        OPEN QUERY qQuery FOR EACH SmartSchedulerJobStatus WHERE SmartSchedulerJobStatus.JobStatus     EQ poJobStatus:ToString()
                                                             AND SmartSchedulerJobStatus.StartDateTime LT pdDateToDelete NO-LOCK .
        GET FIRST qQuery NO-LOCK .

        DO WHILE NOT QUERY qQuery:QUERY-OFF-END
            ON ERROR UNDO, THROW
            TRANSACTION:

            GET CURRENT qQuery EXCLUSIVE-LOCK NO-WAIT.

            IF AVAILABLE SmartSchedulerJobStatus THEN DO:

                DELETE SmartSchedulerJobStatus.

            END.

            FINALLY:
                GET NEXT qQuery NO-LOCK .
            END FINALLY.
        END.

    END METHOD.

    /**
     * Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
     *          Invoked in FetchData and SaveChanges
     */
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSchedulerJobStatus:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartSchedulerJobStatus:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /**
     * Purpose: Determines the next available job for execution
     * Notes:
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID GetNextAvailableJobStatus (poParameter AS GetNextAvailableJobStatusParameter):

        DEFINE VARIABLE i            AS INTEGER NO-UNDO .
        DEFINE VARIABLE iMaxWaitTime AS INTEGER NO-UNDO .

        DEFINE QUERY qQuery FOR SmartSchedulerJobStatus .

        ASSIGN iMaxWaitTime                       = poParameter:MaxWaitTime
               poParameter:SchedulerJobStatusGuid = ? .

        IF iMaxWaitTime = ? OR iMaxWaitTime < 1 THEN
            ASSIGN iMaxWaitTime = 1 .

        DO i = 1 TO iMaxWaitTime:
            OPEN QUERY qQuery FOR EACH SmartSchedulerJobStatus WHERE SmartSchedulerJobStatus.JobStatus        =  SchedulerJobStatusEnum:Scheduled:ToString()
                                                                 AND SmartSchedulerJobStatus.Cancel           =  FALSE
                                                                 AND SmartSchedulerJobStatus.ScheduleDateTime <= NOW NO-LOCK .

            GET FIRST qQuery NO-LOCK .

            DO WHILE NOT QUERY qQuery:QUERY-OFF-END
                ON ERROR UNDO, THROW
                TRANSACTION:

                GET CURRENT qQuery EXCLUSIVE-LOCK NO-WAIT .

                IF AVAILABLE SmartSchedulerJobStatus AND
                   SmartSchedulerJobStatus.JobStatus =  SchedulerJobStatusEnum:Scheduled:ToString() THEN DO:

                    ASSIGN SmartSchedulerJobStatus.JobStatus     = SchedulerJobStatusEnum:Running:ToString()
                           SmartSchedulerJobStatus.StartDateTime = NOW
                           SmartSchedulerJobStatus.StatusText    = SUBSTITUTE ("Started at PID &1"{&TRAN}, Consultingwerk.Environment:Pid)

                           poParameter:SchedulerJobStatusGuid    = SmartSchedulerJobStatus.SchedulerJobStatusGuid .

                    GET CURRENT qQuery NO-LOCK .

                    RETURN .
                END.

                GET NEXT qQuery NO-LOCK .
            END.

            PAUSE 1 NO-MESSAGE .

        END. /* iMaxWaitTime */

    END METHOD .

    /**
     * Purpose: Schedules the next execution of the given Job Status record
     * Notes:   Only for repeating patterns
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID ScheduleNextJobStatus (poParameter AS ScheduleNextJobStatusParameter):

        DEFINE BUFFER SmartSchedulerJobStatus    FOR SmartSchedulerJobStatus .
        DEFINE BUFFER NewSmartSchedulerJobStatus FOR SmartSchedulerJobStatus .
        DEFINE BUFFER SmartSchedulerJobPlan      FOR SmartSchedulerJobPlan .

        DEFINE VARIABLE oSchedulerService   AS ISchedulerService              NO-UNDO .
        DEFINE VARIABLE oPlanningType       AS SchedulerJobPlanningTypeEnum   NO-UNDO .
        DEFINE VARIABLE oEnumClass          AS Progress.Lang.Class            NO-UNDO .
        DEFINE VARIABLE dtScheduleDateTime  AS DATETIME-TZ                    NO-UNDO INIT ? .

        FIND SmartSchedulerJobStatus WHERE SmartSchedulerJobStatus.SchedulerJobStatusGuid = poParameter:SchedulerJobStatusGuid
            NO-LOCK NO-ERROR .

        IF NOT AVAILABLE SmartSchedulerJobStatus THEN DO:
            ErrorHelper:ResetErrorStatus() .
            RETURN .
        END.

        IF NOT SmartSchedulerJobStatus.Recurring THEN
            RETURN .

        FIND SmartSchedulerJobPlan OF SmartSchedulerJobStatus
            NO-LOCK NO-ERROR .

        IF NOT AVAILABLE SmartSchedulerJobPlan THEN DO:
            ErrorHelper:ResetErrorStatus() .
            RETURN .
        END.

        oEnumClass = Progress.Lang.Class:GetClass ("Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum":U) .

        IF oEnumClass:IsA ("Consultingwerk.Enum":U) THEN
            ASSIGN oPlanningType = DYNAMIC-INVOKE (oEnumClass:TypeName, "FromString":U, SmartSchedulerJobPlan.PlanningType) .

&IF DEFINED (NativeEnums) NE 0 &THEN
        ELSE IF oEnumClass:IsA ("Progress.Lang.Enum":U) THEN
            ASSIGN oPlanningType = CAST (Progress.Lang.Enum:ToObject (oEnumClass:TypeName, SmartSchedulerJobPlan.PlanningType),
                                         SchedulerJobPlanningTypeEnum) .
&ENDIF

        CASE SmartSchedulerJobPlan.PlanningType:
            WHEN "Weekly":U OR WHEN "Monthly":U THEN DO:
                ASSIGN oSchedulerService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Scheduler.ISchedulerService
                                                                         "NEW Consultingwerk.SmartFramework.Scheduler.SchedulerService ()"}

                       dtScheduleDateTime = oSchedulerService:GetNextExecutionTime (SmartSchedulerJobStatus.ScheduleDateTime,
                                                                                    oPlanningType,
                                                                                    SmartSchedulerJobPlan.WeekInterval,
                                                                                    SmartSchedulerJobPlan.RecurringMonday,
                                                                                    SmartSchedulerJobPlan.RecurringTuesday,
                                                                                    SmartSchedulerJobPlan.RecurringWednesday,
                                                                                    SmartSchedulerJobPlan.RecurringThursday,
                                                                                    SmartSchedulerJobPlan.RecurringFriday,
                                                                                    SmartSchedulerJobPlan.RecurringSaturday,
                                                                                    SmartSchedulerJobPlan.RecurringSunday,
                                                                                    SmartSchedulerJobPlan.RecurringJanuary,
                                                                                    SmartSchedulerJobPlan.RecurringFebruary,
                                                                                    SmartSchedulerJobPlan.RecurringMarch,
                                                                                    SmartSchedulerJobPlan.RecurringApril,
                                                                                    SmartSchedulerJobPlan.RecurringMay,
                                                                                    SmartSchedulerJobPlan.RecurringJune,
                                                                                    SmartSchedulerJobPlan.RecurringJuly,
                                                                                    SmartSchedulerJobPlan.RecurringAugust,
                                                                                    SmartSchedulerJobPlan.RecurringSeptember,
                                                                                    SmartSchedulerJobPlan.RecurringOctober,
                                                                                    SmartSchedulerJobPlan.RecurringNovember,
                                                                                    SmartSchedulerJobPlan.RecurringDecember,
                                                                                    SmartSchedulerJobPlan.RecurringDaylist,
                                                                                    SmartSchedulerJobPlan.RecurringExecutionTimes,
                                                                                    SmartSchedulerJobPlan.ValidFrom,
                                                                                    SmartSchedulerJobPlan.ValidTo)
                       .
            END.
        END CASE .

        /* Cancel here - when there is not schedule date/time */
        IF dtScheduleDateTime = ? THEN
            RETURN .

        CREATE NewSmartSchedulerJobStatus.
        BUFFER-COPY SmartSchedulerJobStatus
            EXCEPT SchedulerJobStatusGuid
            TO NewSmartSchedulerJobStatus

            ASSIGN NewSmartSchedulerJobStatus.JobStatus            = SchedulerJobStatusEnum:Scheduled:ToString()
                   NewSmartSchedulerJobStatus.Recurring            = TRUE
                   NewSmartSchedulerJobStatus.StatusText           = "":U
                   NewSmartSchedulerJobStatus.Percentage           = 0
                   NewSmartSchedulerJobStatus.Cancel               = FALSE
                   NewSmartSchedulerJobStatus.StartDateTime        = ?
                   NewSmartSchedulerJobStatus.FinishedDateTime     = ?
                   NewSmartSchedulerJobStatus.ScheduleDateTime     = dtScheduleDateTime
                .

        RELEASE NewSmartSchedulerJobStatus .

    END METHOD .

    /**
     * Purpose: Assigns a Scheduler Job Status
     * Notes:   Supported JobStatus values are Cancelled, OnHold, Scheduled
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID SetJobStatus (poParameter AS SetJobStatusParameter):

        DEFINE BUFFER SmartSchedulerJobStatus FOR SmartSchedulerJobStatus .

        DO FOR SmartSchedulerJobStatus TRANSACTION STOP-AFTER 2 ON ERROR UNDO, THROW:

            FIND SmartSchedulerJobStatus WHERE SmartSchedulerJobStatus.SchedulerJobStatusGuid = poParameter:SchedulerJobStatusGuid
                EXCLUSIVE-LOCK .

            CASE poParameter:JobStatus:

                WHEN SchedulerJobStatusEnum:Cancelled THEN DO:
                    /* Can only cancel a Job that is running */
                    IF SmartSchedulerJobStatus.JobStatus <> SchedulerJobStatusEnum:Running:ToString() THEN
                        UNDO, THROW NEW SchedulerException("A Job can only be cancelled when it's in Running state."{&TRAN}, 0) .

                    ASSIGN SmartSchedulerJobStatus.Cancel = TRUE .
                END.
                WHEN SchedulerJobStatusEnum:OnHold THEN DO:
                    /* Can only hold a Job that is scheduled */
                    IF SmartSchedulerJobStatus.JobStatus <> SchedulerJobStatusEnum:Scheduled:ToString() THEN
                        UNDO, THROW NEW SchedulerException("A Job can be set on hold when it's in Scheduled state."{&TRAN}, 0) .

                    ASSIGN SmartSchedulerJobStatus.JobStatus = SchedulerJobStatusEnum:OnHold:ToString()  .
                END.
                WHEN SchedulerJobStatusEnum:Scheduled THEN DO:
                    /* Can only re-schedule a Job that is on hold */
                    IF SmartSchedulerJobStatus.JobStatus <> SchedulerJobStatusEnum:OnHold:ToString() THEN
                        UNDO, THROW NEW SchedulerException("A Job can be set re-scheduled when it's in OnHold state."{&TRAN}, 0) .

                    ASSIGN SmartSchedulerJobStatus.JobStatus = SchedulerJobStatusEnum:Scheduled:ToString()  .
                END.

            END CASE .

            RELEASE SmartSchedulerJobStatus .
        END.

    END METHOD .

    /**
     * Purpose: Returns the database field name matching a temp-table field name
     *          contained in a consumers query string (query string vs. temp-table
     *          definition)
     * Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
     *          DataAccess:FetchData () query preparation
     *          TO-DO: Provide code for alternative mapping
     * @param pcTable The temp table name to return the source (database table) field name for
     * @param pcColumn The temp table column name to return the source (database table) field name for
     * @return The database field name mapped to the passed in temp-table field
     */
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartSchedulerJobStatus":U THEN
                cMap = DATA-SOURCE src_SmartSchedulerJobStatusSmart:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        ASSIGN iPos = LOOKUP(pcColumn, cMap) .

        IF iPos MODULO 2 = 1 THEN
            RETURN ENTRY(LOOKUP(pcColumn,cMap) + 1,cMap).
        ELSE
            RETURN "":U .

    END METHOD.

    /**
     * Purpose: Returns the base query string used to retrieve data for the temp
     *          tables. This query string will be appended by the query provided by
     *          the consumer (FetchDataRequest object).
     * Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
     *          DataAccess:FetchData () query preparation
     *          TO-DO: Provide code to return the DATA-SOURCE root query string
     * @param pcTable The temp table name to return the source default (database) query string for
     * @return The base query string used to retrieve data for the given temp-table
     */
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartSchedulerJobStatus":U THEN
                RETURN "FOR EACH SmartSchedulerJobStatus, FIRST SmartSchedulerJob OF SmartSchedulerJobStatus":U.

        END CASE .

    END METHOD.

    /**
     * Purpose: Updates the Scheduler Job Status details
     * Notes:
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID UpdateJobStatus (poParameter AS UpdateJobStatusParameter):

        DEFINE BUFFER SmartSchedulerJobStatus FOR SmartSchedulerJobStatus .

        DO FOR SmartSchedulerJobStatus TRANSACTION STOP-AFTER 2 ON ERROR UNDO, THROW:

            FIND SmartSchedulerJobStatus WHERE SmartSchedulerJobStatus.SchedulerJobStatusGuid = poParameter:SchedulerJobStatusGuid
                EXCLUSIVE-LOCK .

            IF SmartSchedulerJobStatus.JobStatus <> SchedulerJobStatusEnum:Running:ToString() THEN
                UNDO, THROW NEW SchedulerException("The Job Status can only be updated for job's in the Running state."{&TRAN}, 0) .

            IF poParameter:StatusText <> ? THEN
                ASSIGN SmartSchedulerJobStatus.StatusText = poParameter:StatusText .

            IF poParameter:PercentFinished <> ? THEN
                ASSIGN SmartSchedulerJobStatus.Percentage = poParameter:PercentFinished .

            RELEASE SmartSchedulerJobStatus .
        END.

    END METHOD .



END CLASS.
