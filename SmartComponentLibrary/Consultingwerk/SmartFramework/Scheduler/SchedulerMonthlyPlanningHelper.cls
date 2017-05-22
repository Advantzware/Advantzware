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
    File        : SchedulerMonthlyPlanningHelper
    Purpose     : Helper for the Scheduler Service for date/time calculation
                  based on the monthly planning pattern
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 29 00:11:58 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerMonthlyPlanningHelper:

    /**
     * Purpose: Returns the next planning on the same day
     * Notes:
     * @param pdtPreviousExecution The previous execution date/time
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
     * @param pcDays The comma delimited list of days
     * @param pcTimes The list of planning times
     * @return The next planning date/time or ?
     */
    METHOD PUBLIC STATIC DATETIME-TZ NextPlanningInNextMonth (pdtPreviousExecution AS DATETIME-TZ,
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
                                                              pcDays AS CHARACTER,
                                                              pcTimes AS CHARACTER):

        DEFINE VARIABLE i      AS INTEGER     NO-UNDO .
        DEFINE VARIABLE dtTemp AS DATETIME-TZ NO-UNDO .
        DEFINE VARIABLE dtDay  AS DATE        NO-UNDO .
        DEFINE VARIABLE iMonth AS INTEGER     NO-UNDO .

        ASSIGN dtDay = DATE (pdtPreviousExecution) .

        DO i = 1 TO 12:
            ASSIGN dtDay  = DateType:FirstDayOfNextMonth(dtDay)
                   iMonth = MONTH (dtDay) .

            IF (iMonth =  1 AND plRecurringJanuary) OR
               (iMonth =  2 AND plRecurringFebruary) OR
               (iMonth =  3 AND plRecurringMarch) OR
               (iMonth =  4 AND plRecurringApril) OR
               (iMonth =  5 AND plRecurringMay) OR
               (iMonth =  6 AND plRecurringJune) OR
               (iMonth =  7 AND plRecurringJuly) OR
               (iMonth =  8 AND plRecurringAugust) OR
               (iMonth =  9 AND plRecurringSeptember) OR
               (iMonth = 10 AND plRecurringOctober) OR
               (iMonth = 11 AND plRecurringNovember) OR
               (iMonth = 12 AND plRecurringDecember) THEN DO:

                dtTemp = SchedulerMonthlyPlanningHelper:NextPlanningInSameMonth (dtDay,
                                                                                 pcDays,
                                                                                 pcTimes) .

                IF dtTemp <> ? THEN
                    RETURN dtTemp .
            END.
        END.

        RETURN ? .

    END METHOD .

    /**
     * Purpose: Returns the next planning on the same day
     * Notes:
     * @param pdtPreviousExecution The previous execution date/time
     * @param pcDays The comma delimited list of days
     * @param pcTimes The list of planning times
     * @return The next planning date/time or ?
     */
    METHOD PUBLIC STATIC DATETIME-TZ NextPlanningInSameMonth (pdtPreviousExecution AS DATETIME-TZ,
                                                              pcDays AS CHARACTER,
                                                              pcTimes AS CHARACTER):

        DEFINE VARIABLE daDay    AS DATE        NO-UNDO .
        DEFINE VARIABLE iDay     AS INTEGER     NO-UNDO .
        DEFINE VARIABLE iMonth   AS INTEGER     NO-UNDO .
        DEFINE VARIABLE iYear    AS INTEGER     NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER     NO-UNDO .
        DEFINE VARIABLE dtNext   AS DATETIME-TZ NO-UNDO .
        DEFINE VARIABLE dtTemp   AS DATETIME-TZ NO-UNDO .
        DEFINE VARIABLE cEntry   AS CHARACTER   NO-UNDO .
        DEFINE VARIABLE iEntry   AS INTEGER     NO-UNDO .

        ASSIGN daDay    = DATE (pdtPreviousExecution)
               iDay     = DAY (daDay)
               iMonth   = MONTH (daDay)
               iYear    = YEAR (daDay) .

        dayloop:
        DO i = 1 TO NUM-ENTRIES (pcDays):

            ASSIGN cEntry = ENTRY (i, pcDays)
                   iEntry = INTEGER (cEntry) .

            IF iEntry > 0 THEN DO:
                ASSIGN daDay = DATE (iMonth, iEntry, iYear) .

                IF MONTH (daDay) <> iMonth THEN
                    ASSIGN daDay = DateType:LastDayOfMonth (DATE (iMonth, 1, iYear)) .
            END.
            ELSE DO:
                ASSIGN daDay = DateType:LastDayOfMonth (DATE (iMonth, 1, iYear)) + 1 + iEntry .

                IF MONTH (daDay) <> iMonth THEN
                    ASSIGN daDay = DATE (iMonth, 1, iYear) .
            END.

            IF daDay < DATE (pdtPreviousExecution) THEN
                NEXT dayloop .

            IF DATE (pdtPreviousExecution) = daDay THEN
                ASSIGN dtTemp = SchedulerWeeklyPlanningHelper:NextPlanningOnSameDay (pdtPreviousExecution, pcTimes) .
            ELSE
                ASSIGN dtTemp = SchedulerWeeklyPlanningHelper:NextPlanningOnSameDay (daDay, pcTimes) .

            IF dtTemp <> ? AND (dtNext = ? OR dtTemp < dtNext) THEN
                ASSIGN dtNext = dtTemp .
        END.

        RETURN dtNext .

    END METHOD.

    /**
     * Purpose: Protected default constructor
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED SchedulerMonthlyPlanningHelper():

    END CONSTRUCTOR.

END CLASS.
