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
    File        : SchedulerJobPlanBusinessEntity
    Purpose     : Business Entity for SmartSchedulerJobPlan
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30.05.2016 10:32:39
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Session.*        FROM PROPATH.
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityTable (name="eSmartSchedulerJobPlan", MandatoryColumns="", DefaultSort="BY eSmartSchedulerJobPlan.SchedulerJobName") .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Scheduler/dsSchedulerJobPlan.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SchedulerJobPlanBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SchedulerJobPlanBusinessEntity ():
        SUPER (DATASET dsSchedulerJobPlan:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTimePart      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTimeValue     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cTimeToken     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iNumTimeParts  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cTimePart      AS CHARACTER NO-UNDO.

        FOR EACH eSmartSchedulerJobPlan ON ERROR UNDO, THROW:

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                       "SchedulerJobGuid":U,
                                       "Job cannot be empty":U).

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                       "SchedulerJobName":U,
                                       "Job Name cannot be empty":U).

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                       "PlanningType":U,
                                       "Planning Type cannot be empty.":U).

            ValidationHelper:IsInList (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                       "PlanningType":U,
                                       "Once,Weekly,Monthly":U,
                                       "Invalid Planning Type."{&TRAN}) .

            /* mandatory settings for recurring jobs */
            IF eSmartSchedulerJobPlan.PlanningType NE "Once":U THEN DO:
                ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                           "RecurringExecutionTimes":U,
                                           "Recurring Execution Times cannot be empty":U).

                /* mandatory setting for recurring days */
                IF eSmartSchedulerJobPlan.PlanningType = "Weekly":U THEN DO:
                    ValidationHelper:IsNotUnknownOrZero (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                               "WeekInterval":U,
                                               "Week Interval cannot be empty":U).
                END.

                /* mandatory setting for recurring months */
                IF eSmartSchedulerJobPlan.PlanningType = "Monthly":U THEN DO:
                    ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                               "RecurringDaylist":U,
                                               "Recurring Days list cannot be empty":U).
                END.

                /* Test for period (.) is required as INTEGER (ENTRY (...)) check below would not complain on periods */
                IF INDEX (eSmartSchedulerJobPlan.RecurringExecutionTimes, ".":U) > 0 THEN
                    DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                                  "Invalid time format."{&TRAN},
                                                  "RecurringExecutionTimes":U,
                                                  "eSmartSchedulerJobPlan":U) .

                /* Check all the TimeFormats of a List */
                DO i = 1 TO NUM-ENTRIES (eSmartSchedulerJobPlan.RecurringExecutionTimes, ",":U):
                    cTimeToken = ENTRY (i, eSmartSchedulerJobPlan.RecurringExecutionTimes, ",":U).

                    ASSIGN
                        iNumTimeParts = NUM-ENTRIES (cTimeToken,":":U).

                    IF iNumTimeParts > 2 OR
                       INDEX (cTimeToken,".":U) > 0 THEN
                        DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE,
                                                      "Invalid time format."{&TRAN},
                                                      "RecurringExecutionTimes":U,
                                                      "eSmartSchedulerJobPlan":U) .

                    DO iTimePart = 1 TO NUM-ENTRIES(cTimeToken, ":":U) ON ERROR UNDO, THROW:

                        ASSIGN
                            cTimePart = ENTRY (iTimePart, cTimeToken, ":":U).

                        IF cTimePart = "":U THEN
                            DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE, "Time portion can not be empty"{&TRAN}, "RecurringExecutionTimes":U, "eSmartSchedulerJobPlan":U) .

                        ASSIGN
                            iTimeValue = INTEGER (cTimePart).

                        /* Hours */
                        IF iTimePart = 1 AND
                           (iTimeValue < 0 OR
                            iTimeValue > 23) THEN
                            DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE, "Invalid time. Hours must be between 0 and 23"{&TRAN}, "RecurringExecutionTimes":U, "eSmartSchedulerJobPlan":U) .

                        /* Minutes */
                        IF iTimePart = 2 AND
                           (iTimeValue < 0 OR
                            iTimeValue > 59) THEN
                            DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE, "Invalid time. Minutes must be between 0 and 59"{&TRAN}, "RecurringExecutionTimes":U, "eSmartSchedulerJobPlan":U) .

                        @SuppressUnusedWarnings.
                        CATCH ple AS Progress.Lang.Error :
                            DatasetHelper:AddErrorString (BUFFER eSmartSchedulerJobPlan:HANDLE, "Invalid time format or value"{&TRAN}, "RecurringExecutionTimes":U, "eSmartSchedulerJobPlan":U) .
                        END CATCH.
                    END.
                END.
            END.

            /* might be needed for future purposes
            IF ROW-STATE (eSmartSchedulerJobPlan) = ROW-CREATED THEN DO:

                IF NOT BUFFER eSmartSchedulerJobPlan:ERROR AND
                   NOT DATASET dsSchedulerJobPlan:ERROR THEN DO ON ERROR UNDO, THROW:

                    ASSIGN
                        eSmartSchedulerJobPlan.ClientPrincipal = SessionManager:ClientPrincipal
                        eSmartSchedulerJobPlan.UserGuid        = SessionManager:UserName
                        .

                END.
            END.
            */

        END.

    END METHOD.

END CLASS.
