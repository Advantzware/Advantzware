
/*------------------------------------------------------------------------
    File        : FormatProcs.p
    Purpose     : Procedure to format data types to string

    Syntax      :

    Description : Procedure to format data types to string.
                  ****Note - All the date formats for months and minutes are case-sensitive and use 
                  the specified formats only for accurate output. This is required to eliminate any 
                  conflicts in different month and minute format types while conversion.

    Author(s)   : Mithun Porandla
    Created     : Mon Mar 23 15:30:12 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Definitions  ************************** */
/* The values of the below variables need to be in upper case and modifying them will cause inaccurate output */ 
DEFINE VARIABLE gcMonthShort AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"].
DEFINE VARIABLE gcMonthLong  AS CHARACTER EXTENT 12 NO-UNDO INITIAL ["JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"]. 

/* The below variables store the respective formatted values and replaced in the 
   input format at later point */
/* The input formats are replaced with a unique special character string and 
   replaced by the below variables at the end. This is to eliminate any conflicts
   while replacing the format types */       
DEFINE VARIABLE gcReplaceMonth        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceYear         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceDay          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceOrdinal      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceHours        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceMinutes      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceSeconds      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceMilliSeconds AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplace12hrs        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcReplaceTimeZone     AS CHARACTER NO-UNDO.
DEFINE VARIABLE glFormatAll           AS LOGICAL   NO-UNDO INITIAL TRUE.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetReplaceString RETURNS CHARACTER PRIVATE
	( INPUT ipiCount AS INTEGER ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Format_Date:
/*------------------------------------------------------------------------------
 Purpose: Formats the given date to a string in the given format
 Notes: Valid Format Types:
        MONTH   - Displays the name of the month in upper case long format ( Eg. 03/14/2020 - MARCH)
        month   - Displays the name of the month in lower case long format ( Eg. 03/14/2020 - march)
        Month   - Displays the name of the month in upper camel case long format ( Eg. 03/14/2020 - March)
        MON/MMM - Displays the name of the month in upper case short format ( Eg. 03/14/2020 - MAR)
        mon/mmm - Displays the name of the month in lower case short format ( Eg. 03/14/2020 - mar)
        Mon/Mmm - Displays the name of the month in upper camel case short format ( Eg. 03/14/2020 - Mar)
        MM      - Displays month in two digits ( Eg. 03/14/2020 - 03)
        M       - Displays month in single digit for months between 1 and 9 ( Eg. 03/14/2020 - 3)
        YYYYY   - Displays year in four ( Eg. 03/14/2020 - 2020)
        YYY     - Displays last three digits of the year ( Eg. 03/14/2020 - 020)
        YY      - Displays last two digits of the year ( Eg. 03/14/2020 - 20)
        DD      - Displays day of the date in two digits ( Eg. 03/04/2020 - 04)
        D       - Displays day of the date in single digit for days between 1 and 9 ( Eg. 03/04/2020 - 4)
        TH      - Displays the ordinals of the day (Eg. 03/04/2020 - 4th, 03/01/2020 - 1st)
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDate      AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateString AS CHARACTER NO-UNDO.

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

    RUN pFormatMonth (
        INPUT  iMonth,
        INPUT  opcDateString,
        OUTPUT opcDateString
        ).

    IF INDEX(opcDateString, "yyyy") GT 0 THEN
        ASSIGN
            opcDateString = REPLACE(opcDateString, "yyyy", fGetReplaceString(2))
            gcReplaceYear = STRING(iYear,"9999")
            .
    ELSE IF INDEX(opcDateString, "yyy") GT 0 THEN
        ASSIGN
            opcDateString = REPLACE(opcDateString, "yyy", fGetReplaceString(2))
            gcReplaceYear = SUBSTRING(STRING(iYear),2,3)
            .
    ELSE IF INDEX(opcDateString, "yy") GT 0 THEN
        ASSIGN
            opcDateString = REPLACE(opcDateString, "yy", fGetReplaceString(2))
            gcReplaceYear = SUBSTRING(STRING(iYear),3,2)
            .

    IF INDEX(opcDateString, "dd") GT 0 THEN
        ASSIGN
            opcDateString = REPLACE(opcDateString, "dd", fGetReplaceString(3))
            gcReplaceDay  = STRING(iDay,"99")
            .
    ELSE IF INDEX(opcDateString, "d") GT 0 THEN
        ASSIGN
            opcDateString = REPLACE(opcDateString, "d", fGetReplaceString(3))
            gcReplaceDay  = TRIM(STRING(iDay,">9"))
            .

    IF INDEX(opcDateString, "th") GT 0 THEN DO:
        IF iDay EQ 1 OR iDay EQ 21 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "st"
                .
        ELSE IF iDay EQ 2 OR iDay EQ 22 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "nd"
                .
        ELSE IF iDay EQ 3 OR iDay EQ 23 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "rd"
                .
        ELSE
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "th"
                .
    END.
    
    RUN pFormatAll (
        INPUT-OUTPUT opcDateString
        ).
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

    DEFINE VARIABLE lFormatAll AS LOGICAL NO-UNDO.
    
    IF ipdtDateTime EQ ? THEN
        RETURN.
    
    ASSIGN
        opcDateTimeString = ipcFormat
        lFormatAll        = glFormatAll           
        glFormatAll       = FALSE
        .

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
        
    glFormatAll = lFormatAll.
    
    RUN pFormatAll (
        INPUT-OUTPUT opcDateTimeString
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
    DEFINE OUTPUT PARAMETER opcDateTimeTZString AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE ipdtDateTime AS DATETIME  NO-UNDO.
    DEFINE VARIABLE ipcTimeZone  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatAll   AS LOGICAL   NO-UNDO.
    
    IF ipdtDateTimeTZ EQ ? THEN
        RETURN.
    
    ASSIGN
        ipdtDateTime        = ipdtDateTimeTZ
        opcDateTimeTZString = ipcFormat
        ipcTimeZone         = STRING(ipdtDateTimeTZ)
        ipcTimeZone         = SUBSTRING(ipcTimeZone, LENGTH(ipcTimeZone) - 5, 6)
        lFormatAll          = glFormatAll
        glFormatAll         = FALSE
        .

    IF INDEX(opcDateTimeTZString, "tz") GT 0 THEN
        ASSIGN
            opcDateTimeTZString = REPLACE(opcDateTimeTZString, "tz", fGetReplaceString(10))
            gcReplaceTimeZone   = ipcTimeZone
            .
    
    RUN Format_DateTime (
        INPUT  ipdtDateTime,
        INPUT  opcDateTimeTZString,
        OUTPUT opcDateTimeTZString   
        ).

    glFormatAll = lFormatAll.
    
    RUN pFormatAll (
        INPUT-OUTPUT opcDateTimeTZString
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
    DEFINE OUTPUT PARAMETER opcMTimeString AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iTime         AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMilliseconds AS INTEGER NO-UNDO.    
    DEFINE VARIABLE lFormatAll    AS LOGICAL NO-UNDO.
    
    ASSIGN
        opcMTimeString = ipcFormat
        iTime          = INTEGER(TRUNCATE(ipiMTime / 1000, 0))
        iMilliseconds  = ipiMTime MODULO 1000
        lFormatAll     = glFormatAll           
        glFormatAll    = FALSE
        .

    RUN Format_Time (
        INPUT  iTime,
        INPUT  opcMTimeString,
        OUTPUT opcMTimeString
        ).
    
    glFormatAll = lFormatAll.
    
    IF INDEX(opcMTimeString, "nnn") GT 0 THEN
        ASSIGN
            opcMTimeString        = REPLACE(opcMTimeString, "nnn", fGetReplaceString(9))
            gcReplaceMilliSeconds = STRING(iMilliSeconds,"999")
            .
    ELSE IF INDEX(opcMTimeString, "n") GT 0 THEN
        ASSIGN
            opcMTimeString        = REPLACE(opcMTimeString, "n", fGetReplaceString(9))
            gcReplaceMilliSeconds = TRIM(STRING(iMilliSeconds,">>9"))
            .

    RUN pFormatAll (
        INPUT-OUTPUT opcMTimeString
        ).        
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
    DEFINE OUTPUT PARAMETER opcTimeString AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iHours   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMinutes AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSeconds AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAMPM    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTime    AS CHARACTER NO-UNDO.

    opcTimeString = ipcFormat.
    
    IF INDEX(opcTimeString, "am") GT 0 OR INDEX(opcTimeString, "pm") GT 0 THEN
        ASSIGN
            opcTimeString  = REPLACE(opcTimeString, "am", fGetReplaceString(5))
            opcTimeString  = REPLACE(opcTimeString, "pm", fGetReplaceString(5))
            cTime          = STRING(ipiTime, "hh:mm:ss:am")
            cAMPM          = ENTRY(4,cTime,":")
            gcReplace12hrs = cAMPM
            .
    ELSE
        ASSIGN
            cTime = STRING(ipiTime, "hh:mm:ss")
            cAMPM = ""
            .
                
    ASSIGN
        iHours        = INTEGER(ENTRY(1,cTime,":"))
        iMinutes      = INTEGER(ENTRY(2,cTime,":"))
        iSeconds      = INTEGER(ENTRY(3,cTime,":"))
        .
        
    IF INDEX(opcTimeString, "hh") GT 0 THEN
        ASSIGN
            opcTimeString  = REPLACE(opcTimeString, "hh", fGetReplaceString(6))
            gcReplaceHours = STRING(iHours,"99")
            .
    ELSE IF INDEX(opcTimeString, "h") GT 0 THEN
        ASSIGN
            opcTimeString  = REPLACE(opcTimeString, "h", fGetReplaceString(6))
            gcReplaceHours = TRIM(STRING(iHours,">9"))
            .

    RUN pFormatMinutes (
        INPUT  iMinutes,
        INPUT  opcTimeString,
        OUTPUT opcTimeString
        ).

    IF INDEX(opcTimeString, "ss") GT 0 THEN
        ASSIGN
            opcTimeString    = REPLACE(opcTimeString, "ss", fGetReplaceString(8))
            gcReplaceSeconds = STRING(iSeconds,"99")
            .
    ELSE IF INDEX(opcTimeString, "s") GT 0 THEN
        ASSIGN
            opcTimeString    = REPLACE(opcTimeString, "s", fGetReplaceString(8))
            gcReplaceSeconds = TRIM(STRING(iSeconds,">9"))
            .    

    RUN pFormatAll (
        INPUT-OUTPUT opcTimeString
        ).        
END PROCEDURE.

PROCEDURE pFormatAll PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER iopcFormatString AS CHARACTER NO-UNDO.
    
    IF glFormatAll THEN DO:
        ASSIGN
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(1), gcReplaceMonth)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(2), gcReplaceYear)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(3), gcReplaceDay)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(4), gcReplaceOrdinal)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(5), gcReplace12hrs)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(6), gcReplaceHours)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(7), gcReplaceMinutes)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(8), gcReplaceSeconds)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(9), gcReplaceMilliSeconds)
            iopcFormatString = REPLACE(iopcFormatString, fGetReplaceString(10), gcReplaceTimeZone)
            .

        ASSIGN
            gcReplaceMonth        = ""
            gcReplaceYear         = ""
            gcReplaceDay          = ""
            gcReplaceOrdinal      = ""
            gcReplaceHours        = ""
            gcReplaceMinutes      = ""
            gcReplaceSeconds      = ""
            gcReplaceMilliSeconds = ""
            gcReplace12hrs        = ""
            gcReplaceTimeZone     = ""
            glFormatAll           = TRUE
            .
    END.
END PROCEDURE.

PROCEDURE pFormatMinutes PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiMinutes    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTimeString AS CHARACTER NO-UNDO CASE-SENSITIVE.
    
    opcTimeString = ipcFormat.

    IF INDEX(opcTimeString, "mm") GT 0 THEN
        ASSIGN
            opcTimeString    = REPLACE(opcTimeString, "mm", fGetReplaceString(7))
            gcReplaceMinutes = STRING(ipiMinutes,"99")
            .
    ELSE IF INDEX(opcTimeString, "m") GT 0 THEN
        ASSIGN
            opcTimeString    = REPLACE(opcTimeString, "m", fGetReplaceString(7))
            gcReplaceMinutes = TRIM(STRING(ipiMinutes,">9"))
            .
END PROCEDURE.

PROCEDURE pFormatMonth PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiMonth      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateString AS CHARACTER NO-UNDO CASE-SENSITIVE.

    opcDateString  = ipcFormat.

    IF INDEX(opcDateString, "MONTH") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "MONTH", fGetReplaceString(1))
            gcReplaceMonth = gcMonthLong[ipiMonth]
            .
    ELSE IF INDEX(opcDateString, "month") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "month", fGetReplaceString(1))
            gcReplaceMonth = LC(gcMonthLong[ipiMonth])
            .
    ELSE IF INDEX(opcDateString, "Month") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "Month", fGetReplaceString(1))
            gcReplaceMonth = SUBSTRING(gcMonthLong[ipiMonth],1,1) + LC(SUBSTRING(gcMonthLong[ipiMonth],2,LENGTH(gcMonthLong[ipiMonth])))
            .
    ELSE IF INDEX(opcDateString, "MON") GT 0 OR INDEX(opcDateString, "MMM") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "MON", fGetReplaceString(1))
            opcDateString  = REPLACE(opcDateString, "MMM", fGetReplaceString(1))
            gcReplaceMonth = gcMonthShort[ipiMonth]
            .
    ELSE IF INDEX(opcDateString, "mon") GT 0 OR INDEX(opcDateString, "mmm") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "mon", fGetReplaceString(1))
            opcDateString  = REPLACE(opcDateString, "mmm", fGetReplaceString(1))
            gcReplaceMonth = LC(gcMonthShort[ipiMonth])
            .
    ELSE IF INDEX(opcDateString, "Mon") GT 0 OR INDEX(opcDateString, "Mmm") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "Mon", fGetReplaceString(1))
            opcDateString  = REPLACE(opcDateString, "Mmm", fGetReplaceString(1))
            gcReplaceMonth = SUBSTRING(gcMonthShort[ipiMonth],1,1) + LC(SUBSTRING(gcMonthShort[ipiMonth],2,LENGTH(gcMonthShort[ipiMonth])))
            .
    ELSE IF INDEX(opcDateString, "MM") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "MM", fGetReplaceString(1))
            gcReplaceMonth = STRING(ipiMonth,"99")
            .
    ELSE IF INDEX(opcDateString, "M") GT 0 THEN
        ASSIGN
            opcDateString  = REPLACE(opcDateString, "M", fGetReplaceString(1))
            gcReplaceMonth = TRIM(STRING(ipiMonth,">9"))
            .
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetReplaceString RETURNS CHARACTER PRIVATE
	( INPUT ipiCount AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReplaceChar AS CHARACTER NO-UNDO.
        
    cReplaceChar = FILL("#",ipiCount) + FILL("$",ipiCount) + FILL("%",ipiCount).    
      
    RETURN cReplaceChar.
END FUNCTION.

