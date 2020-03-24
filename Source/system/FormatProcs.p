
/*------------------------------------------------------------------------
    File        : FormatProcs.p
    Purpose     : Procedure to format data types to string

    Syntax      :

    Description : Procedure to format data types to string.
                  ****Note - All the date formats are strictly case-sensitive and use the specified
                  formats only for accurate output. This is required to eliminate any conflicts in
                  different format type while conversion.
                  To simply remember all date related formats should be in upper case and time
                  related formats should be in lower case. 

    Author(s)   : Mithun Porandla
    Created     : Mon Mar 23 15:30:12 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Definitions  ************************** */
/* The values of the below variables need to be in upper case and modifying them will cause inaccurate output */ 
DEFINE VARIABLE gcMonthShort AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"].
DEFINE VARIABLE gcMonthLong  AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"]. 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Format_Date:
/*------------------------------------------------------------------------------
 Purpose: Formats the given date to a string in the given format
 Notes: Valid Format Types:
        MONTH - Displays the name of the month in long format ( Eg. 03/14/2020 - March)
        MON   - Displays the name of the month in short format ( Eg. 03/14/2020 - Mar)
        MM    - Displays month in two digits ( Eg. 03/14/2020 - 03)
        M     - Displays month in single digit for months between 1 and 9 ( Eg. 03/14/2020 - 3)
        YYYYY - Displays year in four ( Eg. 03/14/2020 - 2020)
        YYY   - Displays last three digits of the year ( Eg. 03/14/2020 - 020)
        YY    - Displays last two digits of the year ( Eg. 03/14/2020 - 20)
        DD    - Displays day of the date in two digits ( Eg. 03/04/2020 - 04)
        D     - Displays day of the date in single digit for days between 1 and 9 ( Eg. 03/04/2020 - 4)
        TH    - Displays the ordinals of the day (Eg. 03/04/2020 - 4th, 03/01/2020 - 1st)
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDate      AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateString AS CHARACTER NO-UNDO CASE-SENSITIVE.

    DEFINE VARIABLE iDay   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMonth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iYear  AS INTEGER   NO-UNDO.        

    IF ipdtDate EQ ? THEN
        RETURN.
        
    ASSIGN
        iDay           = DAY(ipdtDate)
        iMonth         = MONTH(ipdtDate)
        iYear          = YEAR(ipdtDate)        
        opcDateString  = ipcFormat
        .

    IF INDEX(opcDateString, "MONTH") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "MONTH", gcMonthLong[iMonth]).
    ELSE IF INDEX(opcDateString, "MON") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "MON", gcMonthShort[iMonth]).
    ELSE IF INDEX(opcDateString, "MM") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "MM", STRING(iMonth,"99")).
    ELSE IF INDEX(opcDateString, "M") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "M", TRIM(STRING(iMonth,">9"))).

    IF INDEX(opcDateString, "YYYY") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "YYYY", STRING(iYear,"9999")).
    ELSE IF INDEX(opcDateString, "YYY") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "YYY", SUBSTRING(STRING(iYear),2,3)).
    ELSE IF INDEX(opcDateString, "YY") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "YY", SUBSTRING(STRING(iYear),3,2)).

    IF INDEX(opcDateString, "DD") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "DD", STRING(iDay,"99")).
    ELSE IF INDEX(opcDateString, "D") GT 0 THEN
        opcDateString = REPLACE(opcDateString, "D", TRIM(STRING(iDay,">9"))).

    IF INDEX(opcDateString, "TH") GT 0 THEN DO:
        IF iDay EQ 1 OR iDay EQ 21 THEN
            opcDateString = REPLACE(opcDateString, "TH", "ST").
        ELSE IF iDay EQ 2 OR iDay EQ 22 THEN
            opcDateString = REPLACE(opcDateString, "TH", "ND").
        ELSE IF iDay EQ 3 OR iDay EQ 23 THEN
            opcDateString = REPLACE(opcDateString, "TH", "RD").
        ELSE
            opcDateString = REPLACE(opcDateString, "TH", "TH").
    END.
END PROCEDURE.

PROCEDURE Format_DateTime:
/*------------------------------------------------------------------------------
 Purpose: Formats the date time datatype to string in a given format 
 Notes: Valid Format Types:
        All valid format types of Format_Date
        All valid format types of Format_MTime
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDateTime      AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateTimeString AS CHARACTER NO-UNDO.

    IF ipdtDateTime EQ ? THEN
        RETURN.
    
    opcDateTimeString = ipcFormat.

    RUN Format_MTime (
        INPUT  MTIME(ipdtDateTime),
        INPUT  opcDateTimeString,
        OUTPUT opcDateTimeString   
        ).

    RUN Format_Date (
        INPUT  DATE(ipdtDateTime),
        INPUT  opcDateTimeString,
        OUTPUT opcDateTimeString   
        ).
END PROCEDURE.

PROCEDURE Format_DateTimeTZ:
/*------------------------------------------------------------------------------
 Purpose: Formats the date time zone datatype to string in a given format
 Notes: Valid Format Types:
        All formats valid for Format_DateTime
        tz - Displays the time zone ( Eg. +05:30)
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDateTimeTZ      AS DATETIME-TZ NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat           AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateTimeTZString AS CHARACTER   NO-UNDO CASE-SENSITIVE.
    
    DEFINE VARIABLE ipdtDateTime AS DATETIME  NO-UNDO.
    DEFINE VARIABLE ipcTimeZone  AS CHARACTER NO-UNDO.
    
    ASSIGN
        ipdtDateTime        = ipdtDateTimeTZ
        opcDateTimeTZString = ipcFormat
        ipcTimeZone         = STRING(ipdtDateTimeTZ)
        ipcTimeZone         = SUBSTRING(ipcTimeZone, LENGTH(ipcTimeZone) - 5, 6)
        .

    IF INDEX(opcDateTimeTZString, "tz") GT 0 THEN
        opcDateTimeTZString = REPLACE(opcDateTimeTZString, "tz", ipcTimeZone).
    
    RUN Format_DateTime (
        INPUT  ipdtDateTime,
        INPUT  opcDateTimeTZString,
        OUTPUT opcDateTimeTZString   
        ).
END PROCEDURE.

PROCEDURE Format_MTime:
/*------------------------------------------------------------------------------
 Purpose: Formats the given integer to a string in given format
 Notes: Valid Format type:
        All valid formats for Format_Time
        nnn - Displays the milliseconds in three digits format ( Eg. 12 - 012)
        n   - Displays the milliseconds in a trimmed leading zero format ( Eg. 12 - 12)
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiMTime       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMTimeString AS CHARACTER NO-UNDO CASE-SENSITIVE.
    
    DEFINE VARIABLE iTime         AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMilliseconds AS INTEGER NO-UNDO.    
    
    ASSIGN
        opcMTimeString = ipcFormat
        iTime          = INTEGER(TRUNCATE(ipiMTime / 1000, 0))
        iMilliseconds  = ipiMTime MODULO 1000
        .

    RUN Format_Time (
        INPUT  iTime,
        INPUT  opcMTimeString,
        OUTPUT opcMTimeString
        ).

    IF INDEX(opcMTimeString, "nnn") GT 0 THEN
        opcMTimeString = REPLACE(opcMTimeString, "nnn", STRING(iMilliSeconds,"999")).
    ELSE IF INDEX(opcMTimeString, "n") GT 0 THEN
        opcMTimeString = REPLACE(opcMTimeString, "n", TRIM(STRING(iMilliSeconds,">>9"))).
END PROCEDURE.

PROCEDURE Format_Time:
/*------------------------------------------------------------------------------
 Purpose: Formats the given integer value to a string in given format
 Notes: Valid Format types:
        hh - Displays hours of the time in two digits ( Eg. 3600 - 01)
        h  - Displays hours of the time in trimmed leading zero format ( Eg. 3600 - 1)
        mm - Displays minutes of the time in two digits ( Eg. 3660 - 01)
        m  - Displays minutes of the time in trimmed leading zero format ( Eg. 3660 - 1)
        hh - Displays seconds of the time in two digits ( Eg. 3661 - 01)
        h  - Displays seconds of the time in trimmed leading zero format ( Eg. 3661 - 1)
        am - Displays time in 12 hour format         
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiTime       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTimeString AS CHARACTER NO-UNDO CASE-SENSITIVE.
    
    DEFINE VARIABLE iHours   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMinutes AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSeconds AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAMPM    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTime    AS CHARACTER NO-UNDO.
    
    ASSIGN
        opcTimeString = ipcFormat
        cTime         = IF INDEX(opcTimeString, "am") GT 0 THEN
                            STRING(ipiTime, "hh:mm:ss:am")
                        ELSE
                            STRING(ipiTime, "hh:mm:ss")
        iHours        = INTEGER(ENTRY(1,cTime,":"))
        iMinutes      = INTEGER(ENTRY(2,cTime,":"))
        iSeconds      = INTEGER(ENTRY(3,cTime,":"))
        cAMPM         = IF INDEX(opcTimeString, "am") GT 0 THEN
                            ENTRY(4,cTime,":")
                        ELSE
                            ""
        .
    
    /* As this format may have a conflict with minutes format, first replace the am with **
       and later replace the same with AM/PM format */ 
    IF INDEX(opcTimeString, "am") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "am", "**").
    
    IF INDEX(opcTimeString, "hh") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "hh", STRING(iHours,"99")).
    ELSE IF INDEX(opcTimeString, "h") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "h", TRIM(STRING(iHours,">9"))).

    IF INDEX(opcTimeString, "mm") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "mm", STRING(iMinutes,"99")).
    ELSE IF INDEX(opcTimeString, "m") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "m", TRIM(STRING(iMinutes,">9"))).

    IF INDEX(opcTimeString, "ss") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "ss", STRING(iSeconds,"99")).
    ELSE IF INDEX(opcTimeString, "s") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "s", TRIM(STRING(iSeconds,">9"))).    

    IF INDEX(opcTimeString, "**") GT 0 THEN
        opcTimeString = REPLACE(opcTimeString, "**", cAMPM).
END PROCEDURE.

