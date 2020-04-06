
/*------------------------------------------------------------------------
    File        : FormatProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester program for FormatProcs.p

    Author(s)   : Mithun Porandla
Mithun Porandla
    Created     : Mon Mar 30 05:29:19 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdFormatProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFormat       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate        AS DATE      NO-UNDO.
DEFINE VARIABLE dtDateTime    AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtDateTimeTZ  AS DATETIME  NO-UNDO.
DEFINE VARIABLE iTime         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMTime        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOutputFormat AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */    

/* ***************************  Main Block  *************************** */
RUN system/FormatProcs.p PERSISTENT SET hdFormatProcs.

ASSIGN
    cFormat      = "hh:mm:ss"
    dtDate       = TODAY
    dtDateTime   = DATETIME(TODAY, MTIME)
    dtDateTimeTZ = dtDateTime
    iTime        = TIME
    iMTime       = MTIME
    .

/* Note that the minutes is case-sensitive and should be strictly lower case */
/* Valid formats - hh, HH, h, H, mm, m, ss, SS, s, S, am, AM, pm, PM */
RUN Format_Time IN hdFormatProcs (
    INPUT  iTime,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_Time" SKIP
    "Input:" iTime SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.    
     
cFormat = "h_m_s".     
RUN Format_Time IN hdFormatProcs (
    INPUT  iTime,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_Time" SKIP
    "Input:" iTime SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.    
     
cFormat = "h_m_s_am".     
RUN Format_Time IN hdFormatProcs (
    INPUT  iTime,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_Time" SKIP
    "Input:" iTime SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.   
     
/* Valid formats - hh, HH, h, H, mm, m, ss, SS, s, S, nnn, n */
cFormat = "h:m:s:nnn".     
RUN Format_MTime IN hdFormatProcs (
    INPUT  iMTime,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_MTime" SKIP
    "Input:" iMTime SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.

/* Valid formats - dd, d, DD, D, MONTH, MON, month, mon, Month, Mon, mmm, Mmm, MMM, yyyy, YYYY, yy, YY, th */
cFormat = "Month ddth, yyyy".     
RUN Format_Date IN hdFormatProcs (
    INPUT  dtDate,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_Date" SKIP
    "Input:" dtDate SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.    

/* Valid formats - All valid formats for Format_Time and Format_Date */
cFormat = "Month ddth, yyyy hh:mm:ss am".     
RUN Format_DateTime IN hdFormatProcs (
    INPUT  dtDateTime,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_DateTime" SKIP
    "Input:" dtDateTime SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.
          
/* Valid formats - All valid formats for Format_Time and Format_Date and tz */
cFormat = "Month ddth, yyyy hh:mm:ss am tz".     
RUN Format_DateTimeTZ IN hdFormatProcs (
    INPUT  dtDateTimeTZ,
    INPUT  cFormat,
    OUTPUT cOutputFormat
    ).
    
MESSAGE "Procedure: Format_DateTimeTZ" SKIP
    "Input:" dtDateTimeTZ SKIP
    "Format:" cFormat SKIP
    "Output:" cOutputFormat 
VIEW-AS ALERT-BOX.          