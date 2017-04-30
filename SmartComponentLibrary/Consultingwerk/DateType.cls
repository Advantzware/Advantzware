/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DateType
    Purpose     : Contains supporting routines for ABL DATE Variables
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 11 22:44:25 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.DateType:

    /**
     * Purpose: Constructor for the DateType class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PRIVATE DateType ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the date of the first day of the month relatively to the given date
     * Notes:   When no date is passed in, the current month will be used
     * @param pdtDate The base date
     * @return The date of the first day of the year
     */
    METHOD PUBLIC STATIC DATE FirstDayOfMonth (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        RETURN DATE (MONTH (pdtDate), 1, YEAR (pdtDate)) .

    END METHOD .

    /**
     * Purpose: Returns the date of the first day of the next month relatively to the given
     *          date
     * Notes:   When no date is passed in, the current month will be used
     * @param pdtDate The base date
     * @return The date of the first day of the year
     */
    METHOD PUBLIC STATIC DATE FirstDayOfNextMonth (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        ASSIGN pdtDate = DateType:FirstDayOfMonth (pdtDate) .

        RETURN DateType:FirstDayOfMonth (pdtDate + 31) .

    END METHOD .

    /**
     * Purpose: Returns the date of the first day in the given week
     * Notes:   Follows the the ISO 8601 norm used in Germany and most European
     *          countries where the week starts with Monday
     * @param pdtDate The base dayte
     * @return The date of the first day of the week
     */
    METHOD PUBLIC STATIC DATE FirstDayOfWeek (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        RETURN pdtDate - IsoWeekDay (pdtDate) + 1 .

    END METHOD .

    /**
     * Purpose: Returns the date of January 1st relatively to the given date
     * Notes:   When no date is passed in, the current year will be used
     * @param pdtDate The base date
     * @return The date of the first day of the year
     */
    METHOD PUBLIC STATIC DATE FirstDayOfYear (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        RETURN DATE (1, 1, YEAR (pdtDate)) .

    END METHOD .

    /**
     * Purpose: Returns true when the date of the given date is in a leap year
     * Notes:   When no date is passed in, the current month will be used
     * @param pdtDate The base date
     * @return Logical value indicating if the given date is in a leap year
     */
    METHOD PUBLIC STATIC LOGICAL IsLeapYear (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        IF LastDayOfYear (pdtDate) - FirstDayOfYear (pdtDate) = 365 THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns the week day (1..7) using the ISO 8601 norm used in Germany
     *          and most European countries where the week starts with Monday
     * Notes:   http://en.wikipedia.org/wiki/ISO_week_date
     * @param pdtDate The date to use for the IsoWeekDay calculation
     * @return The weekday as a number from 1..7
     */
    METHOD PUBLIC STATIC INTEGER IsoWeekDay (pdtDate AS DATE):

        DEFINE VARIABLE iWeekday AS INTEGER NO-UNDO.

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        ASSIGN iWeekday = WEEKDAY (pdtDate) - 1 .

        /* Sunday */
        IF iWeekday = 0 THEN
            RETURN 7 .

        RETURN iWeekday .

    END METHOD .

    /**
     * Purpose: Returns the date of the last day of the month relatively to the given date
     * Notes:   When no date is passed in, the current month will be used
     * @param pdtDate The base date
     * @return The date of the first day of the year
     */
    METHOD PUBLIC STATIC DATE LastDayOfMonth (pdtDate AS DATE):

        DEFINE VARIABLE iMonth AS INTEGER NO-UNDO.
        DEFINE VARIABLE iYear  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iDays  AS INTEGER NO-UNDO EXTENT 12
            INITIAL [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ] .

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        ASSIGN iMonth = MONTH (pdtDate)
               iYear = YEAR (pdtDate) .

        IF iMonth = 2 AND IsLeapYear (pdtDate) THEN
                RETURN DATE (iMonth, 29, iYear) .

        RETURN DATE (iMonth, iDays[iMonth], iYear) .

    END METHOD .

    /**
     * Purpose: Returns the date of the last day in the given week
     * Notes:   Follows the the ISO 8601 norm used in Germany and most European
     *          countries where the week starts with Monday
     * @param pdtDate The base dayte
     * @return The date of the last day of the week
     */
    METHOD PUBLIC STATIC DATE LastDayOfWeek (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        RETURN pdtDate - IsoWeekDay (pdtDate) + 7 .

    END METHOD .

    /**
     * Purpose: Returns the date of December 31st relatively to the given date
     * Notes:   When no date is passed in, the current year will be used
     * @param pdtDate The base date
     * @return The date of the first day of the year
     */
    METHOD PUBLIC STATIC DATE LastDayOfYear (pdtDate AS DATE):

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        RETURN DATE (12, 31, YEAR (pdtDate)) .

    END METHOD .

    /**
     * Purpose: Returns the week number of the given date
     * Notes:
     * @param pdtDate The base date
     * @return The week number of the given date
     */
    METHOD PUBLIC STATIC INTEGER WeekNumber (pdtDate AS DATE):

        DEFINE VARIABLE dtStartWeek1 AS DATE    NO-UNDO.
        DEFINE VARIABLE iWeek        AS INTEGER NO-UNDO.

        IF pdtDate = ? THEN
            pdtDate = TODAY .

        ASSIGN dtStartWeek1 = FirstDayOfWeek (DATE (1, 4, YEAR (pdtDate)))
               iWeek        = INTEGER ((FirstDayOfWeek (pdtDate) - dtStartWeek1) / 7) + 1 .

        RETURN iWeek .

    END METHOD .

END CLASS.
