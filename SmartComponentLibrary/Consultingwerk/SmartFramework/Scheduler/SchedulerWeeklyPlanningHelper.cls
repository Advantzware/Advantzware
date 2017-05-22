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
    File        : SchedulerWeeklyPlanningHelper
    Purpose     : Helper for the Scheduler Service for date/time calculation
                  based on the weekly planning pattern
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 28 15:28:45 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH.
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH.
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerWeeklyPlanningHelper:

    /**
     * Purpose: Returns the next planning on a day in a future week. Week is
     *          based on current week and Interval
     * Notes:   Weekstart is Monday, Interval is 1 based, so 2 means every second
     *          week
     * @param pdtPreviousExecution The previous execution date/time
     * @param piWeekinterval The week interval
     * @param plPlanMonday Plan includes Monday
     * @param plPlanTuesday Plan includes Tuesday
     * @param plPlanWednesday Plan includes Wednesday
     * @param plPlanThursday Plan includes Thursday
     * @param plPlanFriday Plan includes Friday
     * @param plPlanSaturday Plan includes Saturday
     * @param plPlanSunday Plan includes Sunday
     * @param pcTimes The list of planning times
     * @return The next planning date/time or ?
     */
    METHOD PUBLIC STATIC DATETIME-TZ NextPlanningInNextWeek (pdtPreviousExecution AS DATETIME-TZ,
                                                             piWeekinterval AS INTEGER,
                                                             plPlanMonday AS LOGICAL,
                                                             plPlanTuesday AS LOGICAL,
                                                             plPlanWednesday AS LOGICAL,
                                                             plPlanThursday AS LOGICAL,
                                                             plPlanFriday AS LOGICAL,
                                                             plPlanSaturday AS LOGICAL,
                                                             plPlanSunday AS LOGICAL,
                                                             pcTimes AS CHARACTER):

        DEFINE VARIABLE daDay    AS DATE        NO-UNDO .
        DEFINE VARIABLE daMonday AS DATE        NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER     NO-UNDO .
        DEFINE VARIABLE dtNext   AS DATETIME-TZ NO-UNDO .

        ASSIGN daDay    = DateType:FirstDayOfWeek (DATE (pdtPreviousExecution))
               daMonday = daDay + 7 * piWeekinterval .

        weekdayloop:
        DO i = 1 TO 7:

            IF (i = 1 AND plPlanMonday    = FALSE) OR
               (i = 2 AND plPlanTuesday   = FALSE) OR
               (i = 3 AND plPlanWednesday = FALSE) OR
               (i = 4 AND plPlanThursday  = FALSE) OR
               (i = 5 AND plPlanFriday    = FALSE) OR
               (i = 6 AND plPlanSaturday  = FALSE) OR
               (i = 7 AND plPlanSunday    = FALSE) THEN
               NEXT weekdayloop .

            IF LOOKUP ("00:00":U, pcTimes) > 0 THEN
                RETURN DATETIME (daMonday + i - 1) .

            ASSIGN dtNext = NextPlanningOnSameDay (daMonday + i - 1, pcTimes) .

            IF dtNext <> ? THEN
                RETURN dtNext .
        END.

        RETURN ?.

    END METHOD .

    /**
     * Purpose: Returns the next planning on a following day but still in the
     *          same week
     * Notes:   Weekstart is Monday
     * @param pdtPreviousExecution The previous execution date/time
     * @param plPlanMonday Plan includes Monday
     * @param plPlanTuesday Plan includes Tuesday
     * @param plPlanWednesday Plan includes Wednesday
     * @param plPlanThursday Plan includes Thursday
     * @param plPlanFriday Plan includes Friday
     * @param plPlanSaturday Plan includes Saturday
     * @param plPlanSunday Plan includes Sunday
     * @param pcTimes The list of planning times
     * @return The next planning date/time or ?
     */
    METHOD PUBLIC STATIC DATETIME-TZ NextPlanningInSameWeek (pdtPreviousExecution AS DATETIME-TZ,
                                                             plPlanMonday AS LOGICAL,
                                                             plPlanTuesday AS LOGICAL,
                                                             plPlanWednesday AS LOGICAL,
                                                             plPlanThursday AS LOGICAL,
                                                             plPlanFriday AS LOGICAL,
                                                             plPlanSaturday AS LOGICAL,
                                                             plPlanSunday AS LOGICAL,
                                                             pcTimes AS CHARACTER):

        DEFINE VARIABLE daDay    AS DATE        NO-UNDO .
        DEFINE VARIABLE daMonday AS DATE        NO-UNDO .
        DEFINE VARIABLE iWeekday AS INTEGER     NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER     NO-UNDO .
        DEFINE VARIABLE dtNext   AS DATETIME-TZ NO-UNDO .

        ASSIGN daDay    = DATE (pdtPreviousExecution)
               iWeekday = DateType:IsoWeekDay (daDay)
               daMonday = DateType:FirstDayOfWeek (daDay).

        weekdayloop:
        DO i = iWeekday TO 7:

            IF (i = 1 AND plPlanMonday    = FALSE) OR
               (i = 2 AND plPlanTuesday   = FALSE) OR
               (i = 3 AND plPlanWednesday = FALSE) OR
               (i = 4 AND plPlanThursday  = FALSE) OR
               (i = 5 AND plPlanFriday    = FALSE) OR
               (i = 6 AND plPlanSaturday  = FALSE) OR
               (i = 7 AND plPlanSunday    = FALSE) THEN
               NEXT weekdayloop .

            IF daMonday + i - 1 = daDay THEN
                ASSIGN dtNext = NextPlanningOnSameDay (pdtPreviousExecution, pcTimes) .
            ELSE
                ASSIGN dtNext = NextPlanningOnSameDay (daMonday + i - 1, pcTimes) .

            IF dtNext <> ? THEN
                RETURN dtNext .
        END.

        RETURN ?.

    END METHOD.

    /**
     * Purpose: Returns the next planning on the same day
     * Notes:
     * @param pdtPreviousExecution The previous execution date/time
     * @param pcTimes The list of planning times
     * @return The next planning date/time or ?
     */
    METHOD PUBLIC STATIC DATETIME-TZ NextPlanningOnSameDay (pdtPreviousExecution AS DATETIME-TZ,
                                                            pcTimes AS CHARACTER):

        DEFINE VARIABLE daDay     AS DATE        NO-UNDO .
        DEFINE VARIABLE dtNext    AS DATETIME-TZ NO-UNDO .
        DEFINE VARIABLE dtCurrent AS DATETIME-TZ NO-UNDO .
        DEFINE VARIABLE cEntry    AS CHARACTER   NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER     NO-UNDO .

        ASSIGN daDay  = DATE (pdtPreviousExecution)
               dtNext = daDay + 2 .

        DO i = 1 TO NUM-ENTRIES (pcTimes):
            ASSIGN cEntry    = ENTRY (i, pcTimes)
                   dtCurrent = DataTypeHelper:DateTimeFromDateAndTimeString (daDay, cEntry).

            IF dtCurrent > pdtPreviousExecution AND dtCurrent < dtNext THEN
                ASSIGN dtNext = dtCurrent .
        END.

        IF DATE (dtNext) = daDay THEN
            RETURN dtNext .
        ELSE
            RETURN ? .

    END METHOD.

    /**
     * Purpose: Protected default constructor
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED SchedulerWeeklyPlanningHelper():

    END CONSTRUCTOR.

END CLASS.
