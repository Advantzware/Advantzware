
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

USING Progress.Json.ObjectModel.*.

/* ***************************  Definitions  ************************** */
{sys/inc/var.i}

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

DEFINE VARIABLE oJSONObject AS JsonObject NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fEscapeExceptionCharactersJSON RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER ) FORWARD.

FUNCTION fEscapeExceptionCharactersXML RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER ) FORWARD.

FUNCTION fGetReplaceString RETURNS CHARACTER PRIVATE
	( INPUT ipiCount AS INTEGER ) FORWARD.  

FUNCTION sfFormat_JobFormat RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER ) FORWARD.
  
FUNCTION sfFormat_SingleJob RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER ) FORWARD.
  
FUNCTION sfFormat_JobFormatWithHyphen RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER ) FORWARD.  
  


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
        TH/th   - Displays the ordinals of the day (Eg. 03/04/2020 - 4th, 03/01/2020 - 1st). Case Sensitive
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

    RUN pFormatOrdinal (
        INPUT  iDay,
        INPUT  opcDateString,
        OUTPUT opcDateString
        ).
    
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

    RUN Format_Date (
        INPUT  DATE(ipdtDateTime),
        INPUT  opcDateTimeString,
        OUTPUT opcDateTimeString   
        ).

    RUN Format_MTime (
        INPUT  MTIME(ipdtDateTime),
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
        ss - Displays seconds of the time in two digits ( Eg. 3661 - 01)
        s  - Displays seconds of the time in trimmed leading zero format ( Eg. 3661 - 1)
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

PROCEDURE pApplyFunction PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcFunction      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcFunctionText  AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcTargetString AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReplaceSource AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReplaceTarget AS CHARACTER NO-UNDO.
    
    IF ipcFunction EQ "REPLACE" THEN DO:
        ASSIGN
            cReplaceSource  = ENTRY(1, ipcFunctionText)
            cReplaceTarget  = ENTRY(2, ipcFunctionText)
            cReplaceSource  = REPLACE(cReplaceSource, '"', '')
            cReplaceSource  = REPLACE(cReplaceSource, "'", "")
            cReplaceTarget  = REPLACE(cReplaceTarget, '"', '')
            cReplaceTarget  = REPLACE(cReplaceTarget, "'", "")                    
            iopcTargetString = REPLACE(iopcTargetString, cReplaceSource, cReplaceTarget)
            NO-ERROR. 
    END.
    ELSE IF ipcFunction EQ "TRIM" THEN 
        iopcTargetString = TRIM(iopcTargetString).
    ELSE IF ipcFunction EQ "CAPS" THEN
        iopcTargetString = CAPS(iopcTargetString).

END PROCEDURE.

PROCEDURE pUpdateRequestData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Replaces the given key field with the value in the request data
 Notes: Below is the format for the key field to enter a format or data type in configuration.
        $keyfield|format|datatype|alignmentstype|function|$
        Eg. $poID|>>>>>>>9|INT|$, $poNotes|X(30)|$, $poData|YYYYMMDD|DATE|$, $poID|>>>>>>>9|INT|L|$,
            $poID|>>>>>>>9|INT||TRIM|$
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcField           AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcValue           AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldValuePrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValueSuffix AS CHARACTER NO-UNDO.        
    DEFINE VARIABLE cFormat           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormatType       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNextChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSourceString     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTargetString     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionValue    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionValue2   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionValue3   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionValue4   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionValue5   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionText     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionText2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionText3    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionText4    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunctionText5    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAlignmentStyle   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction2        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction3        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction4        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction5        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrim             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatAvail      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFormatTypeAvail  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAlignmentAvail   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFunctionAvail    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFunctionAvail2   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFunctionAvail3   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFunctionAvail4   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFunctionAvail5   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cIfValue          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIfTrueValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIfFalseValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsNullValue      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lHasQuote         AS LOGICAL   NO-UNDO.
    
    ASSIGN
        cFieldValuePrefix = "$"
        cFieldValueSuffix = "$"
        .

    IF ipcValue EQ ? THEN
        ASSIGN
            ipcValue     = ""
            lIsNullValue = TRUE
            .    

    cTargetString = ipcValue.

    IF ipcRequestDataType EQ "XML" THEN
        cTargetString = fEscapeExceptionCharactersXML (cTargetString).      
    ELSE IF ipcRequestDataType EQ "JSON" THEN
        cTargetString = fEscapeExceptionCharactersJSON (cTargetString).

    /* Replaces the key field with value in request data */
    ioplcRequestData = REPLACE(ioplcRequestData, cFieldValuePrefix + ipcField + cFieldValueSuffix, cTargetString).
    
    cTargetString = ipcValue.
    
    /* If the key field is available with a specific format or data type for conversion */
    DO WHILE ioplcRequestData MATCHES "*" + cFieldValuePrefix + ipcField + "|*|" + cFieldValueSuffix + "*":
        /* cNextChar fetches the character after the at the end of key field */
        ASSIGN
            cFormat         = ""
            cFormatType     = ""
            cAlignmentStyle = ""
            cSourceString   = ""
            cFunction       = ""
            cFunctionValue  = ""
            cFunction2      = ""
            cFunctionValue2 = ""
            cFunction3      = ""
            cFunctionValue3 = ""
            cFunction4      = ""
            cFunctionValue4 = ""
            cFunction5      = ""
            cFunctionValue5 = ""            
            cNextChar       = SUBSTRING(ioplcRequestData,INDEX(ioplcRequestData,cFieldValuePrefix + ipcField + "|") + LENGTH(cFieldValuePrefix + ipcField), 1)
            .
        
        ASSIGN
            lFormatAvail     = FALSE
            lFormatTypeAvail = FALSE
            lAlignmentAvail  = FALSE
            lFunctionAvail   = FALSE
            .
            
        /* If $ do nothing, as it would have been already replaced */
        IF cNextChar EQ "$" THEN
            cFormat = "".
        /* If |, then a format exists */
        ELSE IF cNextChar EQ "|" THEN DO:
            iIndex = INDEX(ioplcRequestData,cFieldValuePrefix + ipcField + "|") + LENGTH(cFieldValuePrefix + ipcField) + 1.
            
            /* Code to retrieve the format */
            DO WHILE TRUE:
                IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                    LEAVE.
                
                ASSIGN
                    cFormat = cFormat + SUBSTRING(ioplcRequestData,iIndex,1)
                    iIndex  = iIndex + 1
                    .
            END.

            lFormatAvail = TRUE.
            
            /* Block to check if a data type exist */            
            iIndex = iIndex + 1.
            
            /* If the next character after the format is not $, then data type exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:     
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                        LEAVE.
                    
                    ASSIGN
                        cFormatType = cFormatType + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex      = iIndex + 1
                        .
                END.
                
                lFormatTypeAvail = TRUE.
            END.
            ELSE 
                iIndex = iIndex - 1.
            
            /* Block to check if a alignment style exist */
            iIndex = iIndex + 1.
  
            /* If the next character after the format is not $, then alignment style exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" THEN
                        LEAVE.
  
                    ASSIGN
                        cAlignmentStyle = cAlignmentStyle + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lAlignmentAvail = TRUE.
            END. 
            ELSE
                iIndex = iIndex - 1.
                
            iIndex = iIndex + 1.    
            
            lHasQuote = FALSE.
            
            /* If the next character after the alignment is not $, then a function exist */
            IF SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" AND NOT lHasQuote THEN
                        LEAVE.
                    
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ '"' THEN
                        lHasQuote = NOT lHasQuote.
  
                    ASSIGN
                        cFunctionValue = cFunctionValue + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex         = iIndex + 1
                        .
                END.
                
                lFunctionAvail = TRUE.
            END.
            
            iIndex = iIndex + 1.
            
            lHasQuote = FALSE.

            /* If the next character after the first function is not $, then a second function exist */
            IF lFunctionAvail AND SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" AND NOT lHasQuote THEN
                        LEAVE.
                    
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ '"' THEN
                        lHasQuote = NOT lHasQuote.
  
                    ASSIGN
                        cFunctionValue2 = cFunctionValue2 + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lFunctionAvail2 = TRUE.
            END.   
                                         
            iIndex = iIndex + 1.
            
            lHasQuote = FALSE.

            /* If the next character after the second function is not $, then a third function exist */
            IF lFunctionAvail2 AND SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" AND NOT lHasQuote THEN
                        LEAVE.
                    
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ '"' THEN
                        lHasQuote = NOT lHasQuote.
  
                    ASSIGN
                        cFunctionValue3 = cFunctionValue3 + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lFunctionAvail3 = TRUE.
            END.      
                                               
            iIndex = iIndex + 1.
            
            lHasQuote = FALSE.

            /* If the next character after the third function is not $, then a fourth function exist */
            IF lFunctionAvail3 AND SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" AND NOT lHasQuote THEN
                        LEAVE.
                    
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ '"' THEN
                        lHasQuote = NOT lHasQuote.
  
                    ASSIGN
                        cFunctionValue4 = cFunctionValue4 + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lFunctionAvail4 = TRUE.
            END.
                                                           
            iIndex = iIndex + 1.
            
            lHasQuote = FALSE.

            /* If the next character after the fourth function is not $, then a fifth function exist */
            IF lFunctionAvail4 AND SUBSTRING(ioplcRequestData,iIndex,1) NE "$" THEN DO:
                DO WHILE TRUE:
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ "|" AND NOT lHasQuote THEN
                        LEAVE.
                    
                    IF SUBSTRING(ioplcRequestData,iIndex,1) EQ '"' THEN
                        lHasQuote = NOT lHasQuote.
  
                    ASSIGN
                        cFunctionValue5 = cFunctionValue5 + SUBSTRING(ioplcRequestData,iIndex,1)
                        iIndex          = iIndex + 1
                        .
                END.
                
                lFunctionAvail5 = TRUE.
            END.                                                           
        END.    
        ELSE
            cFormat = ?.    
        
        ASSIGN
            cFunction      = ""
            cFunctionText  = ""
            cFunction2     = ""
            cFunctionText2 = ""
            cFunction3     = ""
            cFunctionText3 = ""
            cFunction4     = ""
            cFunctionText4 = ""
            cFunction5     = ""
            cFunctionText5 = ""            
            .
        
        IF cFunctionValue NE "" THEN DO:
            ASSIGN
                cFunction     = SUBSTRING(cFunctionValue, 1, INDEX(cFunctionValue, "[") - 1)
                cFunctionText = REPLACE(SUBSTRING(cFunctionValue, INDEX(cFunctionValue, '[') + 1), ']', '') 
                NO-ERROR.
        END.

        IF cFunctionValue2 NE "" THEN DO:
            ASSIGN
                cFunction2     = SUBSTRING(cFunctionValue2, 1, INDEX(cFunctionValue2, "[") - 1)
                cFunctionText2 = REPLACE(SUBSTRING(cFunctionValue2, INDEX(cFunctionValue2, '[') + 1), ']', '') 
                NO-ERROR.
        END.

        IF cFunctionValue3 NE "" THEN DO:
            ASSIGN
                cFunction3     = SUBSTRING(cFunctionValue3, 1, INDEX(cFunctionValue3, "[") - 1)
                cFunctionText3 = REPLACE(SUBSTRING(cFunctionValue3, INDEX(cFunctionValue3, '[') + 1), ']', '') 
                NO-ERROR.
        END.
                        
        IF cFunctionValue4 NE "" THEN DO:
            ASSIGN
                cFunction4     = SUBSTRING(cFunctionValue4, 1, INDEX(cFunctionValue4, "[") - 1)
                cFunctionText4 = REPLACE(SUBSTRING(cFunctionValue4, INDEX(cFunctionValue4, '[') + 1), ']', '') 
                NO-ERROR.
        END.

        IF cFunctionValue5 NE "" THEN DO:
            ASSIGN
                cFunction5     = SUBSTRING(cFunctionValue5, 1, INDEX(cFunctionValue5, "[") - 1)
                cFunctionText5 = REPLACE(SUBSTRING(cFunctionValue5, INDEX(cFunctionValue5, '[') + 1), ']', '') 
                NO-ERROR.
        END.
                                        
        IF cFunction EQ "IF" THEN DO:
            ASSIGN
                cIfValue      = ENTRY(1, cFunctionText)
                cIfTrueValue  = ENTRY(2, cFunctionText)
                cIfFalseValue = ENTRY(3, cFunctionText)
                cIfValue      = REPLACE(cIfValue, '"', '')
                cIfValue      = REPLACE(cIfValue, "'", "")
                cIfTrueValue  = REPLACE(cIfTrueValue, '"', '')
                cIfTrueValue  = REPLACE(cIfTrueValue, "'", "")
                cIfFalseValue = REPLACE(cIfFalseValue, '"', '')
                cIfFalseValue = REPLACE(cIfFalseValue, "'", "")
                NO-ERROR.

            IF (cIfValue EQ ipcValue) OR (lIsNullValue AND cIfValue EQ "?") THEN
                cFunctionText = cIfTrueValue.
            ELSE
                cFunctionText = cIfFalseValue.
                
            ASSIGN
                cFunctionText  = REPLACE(cFunctionText, "!VALUE!", cTargetString)
                cTargetString  = cFunctionText
                .
        END.
        
        IF cFormatType NE "" THEN DO:
            /* To convert a decimal value into integer without losing the original value */
            /* Decimal places will be rounded to Progress standard */
            IF cFormatType EQ "DECIMAL-INTEGER" OR cFormatType EQ "DEC-INT" THEN
                ASSIGN
                    cTargetString = STRING(DECIMAL(cTargetString),cFormat)
                    cTargetString = REPLACE(cTargetString, ".", "")
                    cTargetString = TRIM(cTargetString)
                    NO-ERROR.
            ELSE IF cFormatType BEGINS "INT" THEN
                ASSIGN
                    cTargetString = STRING(INTEGER(cTargetString),cFormat)
                    cTargetString = TRIM(cTargetString)
                    NO-ERROR.
            ELSE IF cFormatType BEGINS "DEC" THEN
                ASSIGN
                    cTargetString = STRING(DECIMAL(cTargetString),cFormat)
                    cTargetString = TRIM(cTargetString)
                    NO-ERROR.
            ELSE IF cFormatType EQ "TIME" THEN
                RUN Format_Time (
                    INPUT  INTEGER(cTargetString),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType EQ "MTIME" THEN
                RUN Format_MTime (
                    INPUT  INTEGER(cTargetString),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType EQ "DATE" THEN
                RUN Format_DateTimeTZ (
                    INPUT  DATETIME-TZ(cTargetString),
                    INPUT  cFormat,
                    OUTPUT cTargetString
                    ) NO-ERROR.
            ELSE IF cFormatType BEGINS "LOG" THEN
                ASSIGN
                    cTargetString = STRING(LOGICAL(cTargetString), cFormat)
                    cTargetString = TRIM(cTargetString)
                    .
        END.
        ELSE
            cTargetString = IF cFormat EQ "" THEN
                                cTargetString
                            ELSE
                                STRING(cTargetString,cFormat).

        IF cFormatType BEGINS "INT" OR cFormatType BEGINS "DEC" THEN DO:
            IF cAlignmentStyle EQ "L" THEN
                cTargetString = cTargetString + FILL(" ", LENGTH(cFormat) - LENGTH(cTargetString)).
            ELSE IF cAlignmentStyle EQ "R" THEN
                cTargetString = FILL(" ", LENGTH(cFormat) - LENGTH(cTargetString)) + cTargetString.
        END.
        
        /* Some functions may need to be applied after format changes */
        RUN pApplyFunction (cFunction,  cFunctionText,  INPUT-OUTPUT cTargetString).
        RUN pApplyFunction (cFunction2, cFunctionText2, INPUT-OUTPUT cTargetString).
        RUN pApplyFunction (cFunction3, cFunctionText3, INPUT-OUTPUT cTargetString).
        RUN pApplyFunction (cFunction4, cFunctionText4, INPUT-OUTPUT cTargetString).
        RUN pApplyFunction (cFunction5, cFunctionText5, INPUT-OUTPUT cTargetString).
        
        IF lFormatAvail THEN        
            cSourceString = cSourceString + cFieldValuePrefix + ipcField + "|" + cFormat + "|".
        
        /* Constructing the string to replace with the formatted string */
        IF lFormatTypeAvail THEN
            cSourceString = cSourceString + cFormatType + "|".
        
        IF lAlignmentAvail THEN
            cSourceString = cSourceString + cAlignmentStyle + "|".

        IF lFunctionAvail THEN
            cSourceString = cSourceString + cFunctionValue + "|".

        IF lFunctionAvail2 THEN
            cSourceString = cSourceString + cFunctionValue2 + "|".

        IF lFunctionAvail3 THEN
            cSourceString = cSourceString + cFunctionValue3 + "|".

        IF lFunctionAvail4 THEN
            cSourceString = cSourceString + cFunctionValue4 + "|".

        IF lFunctionAvail5 THEN
            cSourceString = cSourceString + cFunctionValue5 + "|".

        cSourceString = cSourceString + cFieldValueSuffix.

        IF ipcRequestDataType EQ "XML" THEN
            cTargetString = fEscapeExceptionCharactersXML (cTargetString).      
        ELSE IF ipcRequestDataType EQ "JSON" THEN
            cTargetString = fEscapeExceptionCharactersJSON (cTargetString).

        /* Replace the key field with format and data type with the value */
        IF (cFormat NE ? AND cFormat NE "") OR lFunctionAvail THEN
            ioplcRequestData = REPLACE(ioplcRequestData,cSourceString, cTargetString).
    END.    
END PROCEDURE.

PROCEDURE Format_UpdateRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData   AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcField           AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcValue           AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
    
    RUN pUpdateRequestData (
        INPUT-OUTPUT ioplcRequestData,
        INPUT        ipcField,
        INPUT        ipcValue,
        INPUT        ipcRequestDataType   
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

PROCEDURE pFormatOrdinal PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiDay        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateString AS CHARACTER NO-UNDO CASE-SENSITIVE.

    opcDateString  = ipcFormat.
    
    IF INDEX(opcDateString, "th") GT 0 THEN DO:
        IF ipiDay EQ 1 OR ipiDay EQ 21 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "st"
                .
        ELSE IF ipiDay EQ 2 OR ipiDay EQ 22 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "th", fGetReplaceString(4))
                gcReplaceOrdinal = "nd"
                .
        ELSE IF ipiDay EQ 3 OR ipiDay EQ 23 THEN
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
    
    IF INDEX(opcDateString, "TH") GT 0 THEN DO:
        IF ipiDay EQ 1 OR ipiDay EQ 21 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "TH", fGetReplaceString(4))
                gcReplaceOrdinal = "st"
                .
        ELSE IF ipiDay EQ 2 OR ipiDay EQ 22 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "TH", fGetReplaceString(4))
                gcReplaceOrdinal = "nd"
                .
        ELSE IF ipiDay EQ 3 OR ipiDay EQ 23 THEN
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "TH", fGetReplaceString(4))
                gcReplaceOrdinal = "rd"
                .
        ELSE
            ASSIGN
                opcDateString    = REPLACE(opcDateString, "TH", fGetReplaceString(4))
                gcReplaceOrdinal = "th"
                .
    END.    
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

PROCEDURE pJobFormat PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcJobValue   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob2Value  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobValue  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJob2Value AS CHARACTER NO-UNDO.

    opcJobValue = FILL(" ", iJobLen - length(trim(ipcJobValue))) + trim(ipcJobValue).

    opcJob2Value = STRING(ipiJob2Value,"999").
    
END PROCEDURE.

PROCEDURE Format_UpdateLineCount:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER ioplcRequestData AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcRequestSubstring AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cLineString        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLineCount         AS INTEGER   NO-UNDO.
    
    cLineString =  "$LineNumber$".
    
    DO WHILE ioplcRequestData MATCHES "*" + cLineString + "*":
        iIndex = INDEX(ioplcRequestData, cLineString).
        
        lcRequestSubstring = SUBSTRING(ioplcRequestData, 1, iIndex + LENGTH(cLineString) - 1).
        
        iLineCount = NUM-ENTRIES(lcRequestSubstring, CHR(10)).
        
        lcRequestSubstring = REPLACE(lcRequestSubstring, cLineString, STRING (iLineCount)).
        
        ioplcRequestData = lcRequestSubstring + SUBSTRING(ioplcRequestData, iIndex + LENGTH(cLineString)).
    END.
END PROCEDURE.

PROCEDURE Format_UpdatePageCount:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER ioplcRequestData AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcRequestSubstring AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cPageString        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageCount         AS INTEGER   NO-UNDO.
    
    cPageString =  "$PageNumber$".
    
    DO WHILE ioplcRequestData MATCHES "*" + cPageString + "*":
        iIndex = INDEX(ioplcRequestData, cPageString).
        
        lcRequestSubstring = SUBSTRING(ioplcRequestData, 1, iIndex + LENGTH(cPageString) - 1).
        
        iPageCount = NUM-ENTRIES(lcRequestSubstring, CHR(12)).
        
        lcRequestSubstring = REPLACE(lcRequestSubstring, cPageString, STRING (iPageCount)).
        
        ioplcRequestData = lcRequestSubstring + SUBSTRING(ioplcRequestData, iIndex + LENGTH(cPageString)).
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fEscapeExceptionCharactersJSON RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    IF NOT VALID-OBJECT (oJSONObject) THEN
        oJSONObject = NEW JsonObject().
    
    /* Add a property to JSON Object. JsonObject will automatically escape any exception character */
    oJSONObject:Add("EscapeExceptionalCharacters", ipcValue).
    
    /* The output from GetJsonText will have all the exceptonal character escaped */
    ipcValue = oJSONObject:GetJsonText("EscapeExceptionalCharacters").
    
    /* Remove the property so next Add won't have any error */
    oJSONObject:Remove("EscapeExceptionalCharacters").
    
    RETURN ipcValue.
END FUNCTION.

FUNCTION fEscapeExceptionCharactersXML RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    
    cValue = ipcValue.
    
    ASSIGN
        cValue = REPLACE(cValue, '&', "&amp;")
        cValue = REPLACE(cValue, '"', "&quot;")
        cValue = REPLACE(cValue, "'", "&apos;")
        cValue = REPLACE(cValue, '<', "&lt;")
        cValue = REPLACE(cValue, '>', "&gt;")
        .
    
    RETURN cValue.
END FUNCTION.

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

   
FUNCTION sfFormat_JobFormat RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBeginJob  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBeginJob2 AS CHARACTER NO-UNDO.
    
    RUN pJobFormat(INPUT ipcJobNo, INPUT ipiJobNo2, OUTPUT cBeginJob, OUTPUT cBeginJob2).
     
     RETURN STRING(cBeginJob + cBeginJob2) .

END FUNCTION.

     
FUNCTION sfFormat_SingleJob RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBeginJob  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBeginJob2 AS CHARACTER NO-UNDO.
    
    RUN pJobFormat(INPUT ipcJobNo, INPUT 0, OUTPUT cBeginJob, OUTPUT cBeginJob2).
     
    RETURN STRING(cBeginJob) .

END FUNCTION.

FUNCTION sfFormat_JobFormatWithHyphen RETURNS CHARACTER
  ( ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBeginJob  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBeginJob2 AS CHARACTER NO-UNDO.
    
    RUN pJobFormat(INPUT ipcJobNo, INPUT ipiJobNo2, OUTPUT cBeginJob, OUTPUT cBeginJob2).
     
     RETURN STRING(cBeginJob + "-" + cBeginJob2) .

END FUNCTION.
