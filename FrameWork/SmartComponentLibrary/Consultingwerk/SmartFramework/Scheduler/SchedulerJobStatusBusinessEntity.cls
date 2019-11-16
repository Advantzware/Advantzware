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
    File        : SchedulerJobStatusBusinessEntity
    Purpose     : Business Entity for SmartSchedulerJobStatus
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 19:11:37
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.Exceptions.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

@BusinessEntityTable (name="eSmartSchedulerJobStatus", MandatoryColumns="", DefaultSort="BY eSmartSchedulerJobStatus.ScheduleDateTime DESC") .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobStatus.i }

    /**
     * Purpose: Constructor of the SchedulerJobStatusBusinessEntity class
     * Notes:   Passes the handle of the dataset instance to the base class and
     *          sets the default DataAccessName
     */
    CONSTRUCTOR PUBLIC SchedulerJobStatusBusinessEntity ():
        SUPER (DATASET dsSchedulerJobStatus:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusDataAccess":U .

    END CONSTRUCTOR.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Scheduler.CleanupJobStatusParameter", datasetInput="false", datasetOutput="true") .
    /**
     * Purpose: Cleanup of the SmartSchedulerJobStatus records
     * Notes:
     * @param dsSchedulerJobStatus INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID CleanupJobStatus (INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                         poParameter AS CleanupJobStatusParameter):

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        CAST (THIS-OBJECT:DataAccessObject, SchedulerJobStatusDataAccess):CleanupJobStatus (poParameter) .

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Scheduler.GetNextAvailableJobStatusParameter", datasetInput="false", datasetOutput="true") .
    /**
     * Purpose: Determines the next available job for execution
     * Notes:
     * @param dsSchedulerJobStatus INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID GetNextAvailableJobStatus (INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                                  poParameter AS GetNextAvailableJobStatusParameter):

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        CAST (THIS-OBJECT:DataAccessObject, SchedulerJobStatusDataAccess):GetNextAvailableJobStatus (poParameter) .

    END METHOD .

    /**
     * Purpose: Provides a hook to modify data in the ProDataset after Read and
     *          Update operations (i.e. population of aggregated values)
     * Notes:   Invoked during FetchData () and SaveChanges ()
     */
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Scheduler.ScheduleNextJobStatusParameter", datasetInput="false", datasetOutput="true") .
    /**
     * Purpose: Schedules the next execution of the given Job Status record
     * Notes:   Only for repeating patterns
     * @param dsSchedulerJobStatus INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID ScheduleNextJobStatus (INPUT-OUTPUT DATASET dsSchedulerJobStatus, poParameter AS Consultingwerk.SmartFramework.Scheduler.ScheduleNextJobStatusParameter):

        ObjectAssert:IsValid(poParameter, "poParameter":U) .
        Assert:NotNullOrEmpty(poParameter:SchedulerJobStatusGuid, "poParameter:SchedulerJobStatusGuid":U) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException ()
           ON ERROR UNDO, THROW:
            CAST (THIS-OBJECT:DataAccessObject, SchedulerJobStatusDataAccess):ScheduleNextJobStatus (poParameter) .
        END.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Scheduler.SetJobStatusParameter", datasetInput="false", datasetOutput="true") .
    /**
     * Purpose: Assigns a Scheduler Job Status
     * Notes:   Supported JobStatus values are Cancelled, OnHold, Scheduled
     * @param dsSchedulerJobStatus INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID SetJobStatus (INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                    poParameter AS Consultingwerk.SmartFramework.Scheduler.SetJobStatusParameter):

        ObjectAssert:IsValid(poParameter, "poParameter":U) .
        Assert:NotNullOrEmpty(poParameter:SchedulerJobStatusGuid, "poParameter:SchedulerJobStatusGuid":U) .
        ObjectAssert:IsValid(poParameter:JobStatus, "poParameter:JobStatus":U) .

        IF poParameter:JobStatus <> SchedulerJobStatusEnum:Cancelled AND
           poParameter:JobStatus <> SchedulerJobStatusEnum:OnHold AND
           poParameter:JobStatus <> SchedulerJobStatusEnum:Scheduled THEN
            UNDO, THROW NEW SchedulerException("Supported values for JobStatus are Cancel, OnHold and Scheduled"{&TRAN}, 0) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException ()
           ON ERROR UNDO, THROW:
            CAST (THIS-OBJECT:DataAccessObject, SchedulerJobStatusDataAccess):SetJobStatus (poParameter) .
        END.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Scheduler.SetJobStatusParameter", datasetInput="false", datasetOutput="true") .
    /**
     * Purpose: Updates the Scheduler Job Status details
     * Notes:
     * @param dsSchedulerJobStatus INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID UpdateJobStatus (INPUT-OUTPUT DATASET dsSchedulerJobStatus,
                                        poParameter AS Consultingwerk.SmartFramework.Scheduler.UpdateJobStatusParameter):

        ObjectAssert:IsValid(poParameter, "poParameter":U) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException ()
           ON ERROR UNDO, THROW:
            CAST (THIS-OBJECT:DataAccessObject, SchedulerJobStatusDataAccess):UpdateJobStatus (poParameter) .
        END.

    END METHOD .

    /**
     * Purpose: Provides a hook for high level data validation before Update
     *          operations
     * Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
     *          is set, the Update operation will be cancelled before writing back
     *          the data to the database using the DataAccess object
     */
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        @GeneratedValidationStart.
        /* The following code is auto-generated. Don't insert custom code
           between the @GeneratedValidationStart. and @GeneratedValidationEnd.
           annotations as it would get overwritten when you update the Business
           entity. */

        FOR EACH eSmartSchedulerJobStatus ON ERROR UNDO, THROW:
            Consultingwerk.OERA.ValidationHelper:IsNotUnknown (BUFFER eSmartSchedulerJobStatus:HANDLE, "ScheduleDateTime":U, "Scheduled Date and Time cannot be empty.":U).
            Consultingwerk.OERA.ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobStatus:HANDLE, "SchedulerJobGuid":U, "SchedulerJobID cannot be empty.":U).
        END .
        @GeneratedValidationEnd.

        FOR EACH eSmartSchedulerJobStatus ON ERROR UNDO, THROW:
            IF NOT (NEW SchedulerJobDatasetModel()):SmartSchedulerJob:Filter:SchedulerJobGuid:EQ(eSmartSchedulerJobStatus.SchedulerJobGuid):CanFind() THEN
                DatasetHelper:AddErrorString(BUFFER eSmartSchedulerJobStatus:HANDLE,
                                             MessageFormatter:GetMessage ("VALMSG":U, 2, "SchedulerJobGuid":U, eSmartSchedulerJobStatus.SchedulerJobGuid),
                                             "SchedulerJobGuid":U).

            ValidationHelper:IsInList (BUFFER eSmartSchedulerJobStatus:HANDLE,
                                       "JobStatus":U,
                                       "OnHold,Scheduled,Running,Finished,Failed,TimedOut,Cancelled":U,
                                       "Job Status is invalid."{&TRAN}) .
        END.

    END METHOD.

END CLASS.
