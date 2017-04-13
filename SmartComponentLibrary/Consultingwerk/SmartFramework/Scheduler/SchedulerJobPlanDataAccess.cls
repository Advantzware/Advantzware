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
    File        : SchedulerJobPlanDataAccess
    Purpose     : Data Access Component
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 10:32:39
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.Session.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobPlan.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE QUERY qry_SmartSchedulerJobPlanSmartSc FOR SmartSchedulerJobPlan, SmartSchedulerJob .
    DEFINE PRIVATE DATA-SOURCE src_SmartSchedulerJobPlanSmartSc FOR QUERY qry_SmartSchedulerJobPlanSmartSc .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    DEFINE VARIABLE cOnHold     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScheduled  AS CHARACTER NO-UNDO.

    /**
     * Purpose: Constructor for the SchedulerJobPlanDataAccess class
     * Notes:   Used by the BusinessEntity, will receive the handle of the dataset
     *          instance form the BusinessEntity as a parameter. Permanently BIND's
     *          to that dataset using the PRIVATE BindDataset Method.
     * @param phDataset The handle of the Business Entity dataset
     */
    CONSTRUCTOR PUBLIC SchedulerJobPlanDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        THIS-OBJECT:AssignSkipList = "SchedulerJobPlanGuid":U.

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

        ASSIGN cOnHold    = SchedulerJobStatusEnum:OnHold:ToString()
               cScheduled = SchedulerJobStatusEnum:Scheduled:ToString() .

    END CONSTRUCTOR.

    /**
     * Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
     *          Invoked in FetchData and SaveChanges
     */
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSchedulerJobPlan:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                      DATA-SOURCE src_SmartSchedulerJobPlanSmartSc:HANDLE, "
                                      SchedulerJobPlanGuid,SmartSchedulerJobPlan.SchedulerJobPlanGuid,
                                      SchedulerJobGuid,SmartSchedulerJobPlan.SchedulerJobGuid,
                                      MaxRuntime,SmartSchedulerJobPlan.MaxRuntime,
                                      MaxExecutionDelay,SmartSchedulerJobPlan.MaxExecutionDelay,
                                      JobParamter,SmartSchedulerJobPlan.JobParamter,
                                      PlanningType,SmartSchedulerJobPlan.PlanningType,
                                      ScheduleDateTime,SmartSchedulerJobPlan.ScheduleDateTime,
                                      RecurringMonday,SmartSchedulerJobPlan.RecurringMonday,
                                      RecurringTuesday,SmartSchedulerJobPlan.RecurringTuesday,
                                      RecurringWednesday,SmartSchedulerJobPlan.RecurringWednesday,
                                      RecurringThursday,SmartSchedulerJobPlan.RecurringThursday,
                                      RecurringFriday,SmartSchedulerJobPlan.RecurringFriday,
                                      RecurringSaturday,SmartSchedulerJobPlan.RecurringSaturday,
                                      RecurringSunday,SmartSchedulerJobPlan.RecurringSunday,
                                      WeekInterval,SmartSchedulerJobPlan.WeekInterval,
                                      RecurringJanuary,SmartSchedulerJobPlan.RecurringJanuary,
                                      RecurringFebruary,SmartSchedulerJobPlan.RecurringFebruary,
                                      RecurringMarch,SmartSchedulerJobPlan.RecurringMarch,
                                      RecurringApril,SmartSchedulerJobPlan.RecurringApril,
                                      RecurringMay,SmartSchedulerJobPlan.RecurringMay,
                                      RecurringJune,SmartSchedulerJobPlan.RecurringJune,
                                      RecurringJuly,SmartSchedulerJobPlan.RecurringJuly,
                                      RecurringAugust,SmartSchedulerJobPlan.RecurringAugust,
                                      RecurringSeptember,SmartSchedulerJobPlan.RecurringSeptember,
                                      RecurringOctober,SmartSchedulerJobPlan.RecurringOctober,
                                      RecurringNovember,SmartSchedulerJobPlan.RecurringNovember,
                                      RecurringDecember,SmartSchedulerJobPlan.RecurringDecember,
                                      RecurringDaylist,SmartSchedulerJobPlan.RecurringDaylist,
                                      RecurringExecutionTimes,SmartSchedulerJobPlan.RecurringExecutionTimes,
                                      ValidFrom,SmartSchedulerJobPlan.ValidFrom,
                                      ValidTo,SmartSchedulerJobPlan.ValidTo,
                                      Active,SmartSchedulerJobPlan.Active,
                                      ContextDataSet,SmartSchedulerJobPlan.ContextDataSet,
                                      UserGuid,SmartSchedulerJobPlan.UserGuid,
                                      DataBaseConnections,SmartSchedulerJobPlan.DataBaseConnections,
                                      ClientPrincipal,SmartSchedulerJobPlan.ClientPrincipal,
                                      SchedulerJobName,SmartSchedulerJob.SchedulerJobName":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /**
     * Purpose: Do nothing, just receive the parameter and bind to the received
     *          Dataset reference
     * Notes:   Invoked from the class constructor
     * @param dsSchedulerJobPlan DATASET to bind to
     */
    METHOD PRIVATE VOID BindDataset (DATASET dsSchedulerJobPlan BIND):
        /* NOOP */
    END METHOD.

    /**
     * Purpose: Creates the next scheduled Job Status record for
     *          the currently modifed eSmartSchedulerJobPlan record
     * Notes:   The current eSmartSchedulerJobPlan is still in scope
     */
    METHOD PROTECTED VOID CreateNextJobForPlan():

        DEFINE VARIABLE oSchedulerJob       AS SchedulerJobDatasetModel       NO-UNDO .
        DEFINE VARIABLE lcContext           AS LONGCHAR                       NO-UNDO .
        DEFINE VARIABLE oSchedulerService   AS ISchedulerService              NO-UNDO .
        DEFINE VARIABLE oPlanningType       AS SchedulerJobPlanningTypeEnum   NO-UNDO .
        DEFINE VARIABLE oEnumClass          AS Progress.Lang.Class            NO-UNDO .
        DEFINE VARIABLE dtScheduleDateTime  AS DATETIME-TZ                    NO-UNDO .

        DEFINE BUFFER SmartSchedulerJobStatus FOR SmartSchedulerJobStatus .

        oEnumClass = Progress.Lang.Class:GetClass ("Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum":U) .

        IF oEnumClass:IsA ("Consultingwerk.Enum":U) THEN
            ASSIGN oPlanningType = DYNAMIC-INVOKE (oEnumClass:TypeName, "FromString":U, eSmartSchedulerJobPlan.PlanningType) .

&IF DEFINED (NativeEnums) NE 0 &THEN
        ELSE IF oEnumClass:IsA ("Progress.Lang.Enum":U) THEN
            ASSIGN oPlanningType = CAST (Progress.Lang.Enum:ToObject (oEnumClass:TypeName, eSmartSchedulerJobPlan.PlanningType),
                                         SchedulerJobPlanningTypeEnum) .
&ENDIF

        CASE eSmartSchedulerJobPlan.PlanningType:
            WHEN "Once":U THEN
                ASSIGN dtScheduleDateTime = eSmartSchedulerJobPlan.ScheduleDateTime .

            WHEN "Weekly":U OR WHEN "Monthly":U THEN DO:
                ASSIGN oSchedulerService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Scheduler.ISchedulerService
                                                                         "NEW Consultingwerk.SmartFramework.Scheduler.SchedulerService ()"}

                       dtScheduleDateTime = oSchedulerService:GetNextExecutionTime (?,
                                                                                    oPlanningType,
                                                                                    eSmartSchedulerJobPlan.WeekInterval,
                                                                                    eSmartSchedulerJobPlan.RecurringMonday,
                                                                                    eSmartSchedulerJobPlan.RecurringTuesday,
                                                                                    eSmartSchedulerJobPlan.RecurringWednesday,
                                                                                    eSmartSchedulerJobPlan.RecurringThursday,
                                                                                    eSmartSchedulerJobPlan.RecurringFriday,
                                                                                    eSmartSchedulerJobPlan.RecurringSaturday,
                                                                                    eSmartSchedulerJobPlan.RecurringSunday,
                                                                                    eSmartSchedulerJobPlan.RecurringJanuary,
                                                                                    eSmartSchedulerJobPlan.RecurringFebruary,
                                                                                    eSmartSchedulerJobPlan.RecurringMarch,
                                                                                    eSmartSchedulerJobPlan.RecurringApril,
                                                                                    eSmartSchedulerJobPlan.RecurringMay,
                                                                                    eSmartSchedulerJobPlan.RecurringJune,
                                                                                    eSmartSchedulerJobPlan.RecurringJuly,
                                                                                    eSmartSchedulerJobPlan.RecurringAugust,
                                                                                    eSmartSchedulerJobPlan.RecurringSeptember,
                                                                                    eSmartSchedulerJobPlan.RecurringOctober,
                                                                                    eSmartSchedulerJobPlan.RecurringNovember,
                                                                                    eSmartSchedulerJobPlan.RecurringDecember,
                                                                                    eSmartSchedulerJobPlan.RecurringDaylist,
                                                                                    eSmartSchedulerJobPlan.RecurringExecutionTimes,
                                                                                    eSmartSchedulerJobPlan.ValidFrom,
                                                                                    eSmartSchedulerJobPlan.ValidTo)
                       .
            END.

        END CASE .

        /* Cancel here - when there is not schedule date/time */
        IF dtScheduleDateTime = ? THEN
            RETURN .

        oSchedulerJob = NEW SchedulerJobDatasetModel (eSmartSchedulerJobPlan.SchedulerJobGuid) .

        IF NOT oSchedulerJob:SmartSchedulerJob:Available THEN
            UNDO, THROW NEW InvalidSchedulerJobNameException("Invalid scheduler job name."{&TRAN}, 0) .

        CREATE SmartSchedulerJobStatus .
        ASSIGN SmartSchedulerJobStatus.SchedulerJobGuid     = eSmartSchedulerJobPlan.SchedulerJobGuid
               SmartSchedulerJobStatus.SchedulerJobPlanGuid = eSmartSchedulerJobPlan.SchedulerJobPlanGuid
               SmartSchedulerJobStatus.JobName              = oSchedulerJob:SmartSchedulerJob:SchedulerJobName
               SmartSchedulerJobStatus.JobStatus            = SchedulerJobStatusEnum:Scheduled:ToString()
               SmartSchedulerJobStatus.Recurring            = eSmartSchedulerJobPlan.PlanningType <> "Once":U
               SmartSchedulerJobStatus.StatusText           = "":U
               SmartSchedulerJobStatus.Percentage           = 0
               SmartSchedulerJobStatus.MaxExecutionDelay    = eSmartSchedulerJobPlan.MaxExecutionDelay
               SmartSchedulerJobStatus.Cancel               = FALSE
               SmartSchedulerJobStatus.StartDateTime        = ?
               SmartSchedulerJobStatus.FinishedDateTime     = ?
               SmartSchedulerJobStatus.ScheduleDateTime     = dtScheduleDateTime
               SmartSchedulerJobStatus.UserGuid             = SessionManager:UserKey
               SmartSchedulerJobStatus.DataBaseConnections  = eSmartSchedulerJobPlan.DataBaseConnections .
               .

        COPY-LOB FROM eSmartSchedulerJobPlan.JobParamter TO SmartSchedulerJobStatus.JobParamter .



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

            ASSIGN SmartSchedulerJobStatus.ContextDataSet = lcContext .
        END.

        SmartSchedulerJobStatus.ClientPrincipal = SessionManager:ClientPrincipal .

        RELEASE SmartSchedulerJobStatus .

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
     * Purpose: Deletes the jobs in the database for the given plan guid
     * Notes:   Deletes Scheduled and OnHold jobs
     * @param pcSchedulerJobPlanGuid The guid of the job plan to delete scheduled status records for
     */
    METHOD PROTECTED VOID DeleteJobsForPlan (pcSchedulerJobPlanGuid AS CHARACTER):

        DEFINE BUFFER SmartSchedulerJobStatus FOR SmartSchedulerJobStatus .

        FOR EACH SmartSchedulerJobStatus WHERE (SmartSchedulerJobStatus.SchedulerJobPlanGuid = eSmartSchedulerJobPlan.SchedulerJobPlanGuid
                                           AND  SmartSchedulerJobStatus.JobStatus            = cOnHold)
                                            OR (SmartSchedulerJobStatus.SchedulerJobPlanGuid = eSmartSchedulerJobPlan.SchedulerJobPlanGuid
                                           AND  SmartSchedulerJobStatus.JobStatus            = cScheduled) EXCLUSIVE-LOCK:

            IF SmartSchedulerJobStatus.JobStatus = cOnHold OR
               SmartSchedulerJobStatus.JobStatus = cScheduled THEN

                DELETE SmartSchedulerJobStatus .
        END.

    END METHOD.

    /**
     * Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
     *          Invoked in FetchData and SaveChanges
     */
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSchedulerJobPlan:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartSchedulerJobPlan:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /**
     * Purpose: Validation method for eSmartSchedulerJobPlan
     * Notes:
     */
    METHOD PROTECTED VOID eSmartSchedulerJobPlanCreateEndTrans ():

        Consultingwerk.Util.BufferHelper:FindAfterBuffer (BUFFER eSmartSchedulerJobPlanBefore:HANDLE) .

        IF eSmartSchedulerJobPlan.SchedulerJobPlanGuid > "":U THEN
            THIS-OBJECT:DeleteJobsForPlan (eSmartSchedulerJobPlan.SchedulerJobPlanGuid) .

        THIS-OBJECT:CreateNextJobForPlan () .

    END METHOD .

    /**
     * Purpose: Validation method for eSmartSchedulerJobPlan
     * Notes:
     */
    METHOD PROTECTED VOID eSmartSchedulerJobPlanDeleteEndTrans ():

        IF eSmartSchedulerJobPlanBefore.SchedulerJobPlanGuid > "":U THEN
            THIS-OBJECT:DeleteJobsForPlan (eSmartSchedulerJobPlanBefore.SchedulerJobPlanGuid) .

    END METHOD .

    /**
     * Purpose: Validation method for eSmartSchedulerJobPlan
     * Notes:
     */
    METHOD PROTECTED VOID eSmartSchedulerJobPlanModifyEndTrans ():

        Consultingwerk.Util.BufferHelper:FindAfterBuffer (BUFFER eSmartSchedulerJobPlanBefore:HANDLE) .

        IF eSmartSchedulerJobPlan.SchedulerJobPlanGuid > "":U THEN
            THIS-OBJECT:DeleteJobsForPlan (eSmartSchedulerJobPlan.SchedulerJobPlanGuid) .

        THIS-OBJECT:CreateNextJobForPlan () .

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
            WHEN "eSmartSchedulerJobPlan":U THEN
                cMap = DATA-SOURCE src_SmartSchedulerJobPlanSmartSc:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

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
            WHEN "eSmartSchedulerJobPlan":U THEN
                RETURN "FOR EACH SmartSchedulerJobPlan, FIRST SmartSchedulerJob OF SmartSchedulerJobPlan":U.

        END CASE .

    END METHOD.

END CLASS.
