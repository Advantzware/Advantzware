&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoa/appServer/aoaBin.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fDateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptionDate Procedure 
FUNCTION fDateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetParamValue Procedure 
FUNCTION fGetParamValue RETURNS CHARACTER
  ( ipcField AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fParameters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fParameters Procedure 
FUNCTION fParameters RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pGetColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetColumns Procedure 
PROCEDURE pGetColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE multiplier 150
    
    DEFINE INPUT PARAMETER iphTable            AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSelectedColumns  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cRowType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iWidth    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRptWidth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLeft     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    IF NOT VALID-HANDLE(iphTable) THEN RETURN.
        
    iphTable = iphTable:DEFAULT-BUFFER-HANDLE.
    DO idx = 1 TO NUM-ENTRIES(ipcSelectedColumns):
        ASSIGN
            cField    = ENTRY(idx,ipcSelectedColumns)
            iWidth    = MAX(iphTable:BUFFER-FIELD(cField):WIDTH,
                            LENGTH(iphTable:BUFFER-FIELD(cField):LABEL)
                            ) * {&multiplier}
            cRowType  = cRowType
                      + "|" + iphTable:BUFFER-FIELD(cField):NAME
                      + "," + STRING(iWidth)
                      + "," + STRING(iLeft)
            iLeft     = iLeft + iWidth + 50
            iRptWidth = iRptWidth + iWidth
            .
    END. /* each idx */
    iWidth = -1.
    DO idx = 1 TO NUM-ENTRIES(ipcAvailableColumns):
        ASSIGN
            cField   = ENTRY(idx,ipcAvailableColumns)
            cRowType = cRowType
                     + "|" + iphTable:BUFFER-FIELD(cField):NAME
                     + "," + STRING(iWidth)
            .
    END. /* each idx */
    
    cRowType = "ColumnMetaData," + STRING(iRptWidth) + cRowType.
    iphTable:BUFFER-CREATE.
    ASSIGN
        iphTable:BUFFER-FIELD("rowType"):BUFFER-VALUE()    = cRowType
        iphTable:BUFFER-FIELD("parameters"):BUFFER-VALUE() = fParameters()
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetParamValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValues Procedure 
PROCEDURE pGetParamValues :
/*------------------------------------------------------------------------------
  Purpose:     get user-print record requested
  Parameters:  Company, Program ID, User ID, Batch Seq
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cBatch AS CHARACTER NO-UNDO.

    IF ipiBatch EQ 0 THEN
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ ipcCompany
           AND user-print.program-id EQ ipcName
           AND user-print.user-id    EQ ipcUserID
           AND user-print.batch      EQ ""
         NO-ERROR.
    ELSE
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ ipcCompany
           AND user-print.batch-seq  EQ ipiBatch
           AND user-print.program-id EQ ipcName
           AND user-print.batch      EQ "Batch"
         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fDateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateOptionDate Procedure 
FUNCTION fDateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  convert date option into date based on input date
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtDate AS DATE NO-UNDO.

    CASE ipcDateOption:
        WHEN "Fixed Date" THEN
            dtDate = ipdtDate.
        WHEN "Current Date" THEN
            dtDate = TODAY.
        WHEN "Current Date -1" THEN
            dtDate = TODAY - 1.
        WHEN "Current Date +1" THEN
            dtDate = TODAY + 1.
        WHEN "Current Date -2" THEN
            dtDate = TODAY - 2.
        WHEN "Current Date +2" THEN
            dtDate = TODAY + 2.
        WHEN "Current Date -3" THEN
            dtDate = TODAY - 3.
        WHEN "Current Date +3" THEN
            dtDate = TODAY + 3.
        WHEN "Current Date -4" THEN
            dtDate = TODAY - 4.
        WHEN "Current Date +4" THEN
            dtDate = TODAY + 4.
        WHEN "Current Date -5" THEN
            dtDate = TODAY - 5.
        WHEN "Current Date +5" THEN
            dtDate = TODAY + 5.
        WHEN "Current Date -6" THEN
            dtDate = TODAY - 6.
        WHEN "Current Date +6" THEN
            dtDate = TODAY + 6.
        WHEN "Current Date -7" THEN
            dtDate = TODAY - 7.
        WHEN "Current Date +7" THEN
            dtDate = TODAY + 7.
        WHEN "Current Date -8" THEN
            dtDate = TODAY - 8.
        WHEN "Current Date +8" THEN
            dtDate = TODAY + 8.
        WHEN "Current Date -9" THEN
            dtDate = TODAY - 9.
        WHEN "Current Date +9" THEN
            dtDate = TODAY + 9.
        WHEN "Current Date -10" THEN
            dtDate = TODAY - 10.
        WHEN "Current Date +10" THEN
            dtDate = TODAY + 10.
        WHEN "Start of this Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).
        WHEN "End of this Month" THEN
            IF MONTH(TODAY) EQ 12 THEN
            dtDate = DATE(12,31,YEAR(TODAY)).
            ELSE
            dtDate = DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.
        WHEN "First Day of last Month" THEN
            IF MONTH(TODAY) EQ 1 THEN
            dtDate = DATE(12,1,YEAR(TODAY) - 1).
            ELSE
            dtDate = DATE(MONTH(TODAY) - 1,1,YEAR(TODAY)).
        WHEN "Last Day of last Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
        WHEN "Start of this Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY)).
        WHEN "End of this Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY)).
        WHEN "First Day of Last Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY) - 1).
        WHEN "Last Day of Last Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY) - 1).
        WHEN "Last Sunday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 1 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Last Monday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 2 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Last Tuesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 3 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Last Wednesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 4 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Last Thursday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 5 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Last Friday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 6 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Last Saturday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 7 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
        WHEN "Next Sunday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 1 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Next Monday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 2 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Next Tuesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 3 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Next Wednesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 4 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Next Thursday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 5 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Next Friday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 6 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Next Saturday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 7 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
    END CASE.
        
    RETURN dtDate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetParamValue Procedure 
FUNCTION fGetParamValue RETURNS CHARACTER
  ( ipcField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  get individual parameter field values
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF TRIM(user-print.field-name[idx]) EQ ipcField THEN DO:
            cReturnValue = user-print.field-value[idx].
            LEAVE.
        END. /* found screen object */
    END. /* do idx */

    RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fParameters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fParameters Procedure 
FUNCTION fParameters RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cShow        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParameter   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    cParameter = "Parameters".
    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        IF CAN-DO("svTitle,svAvailableColumns,svSelectedColumns",user-print.field-name[idx]) THEN NEXT.
        CASE user-print.field-name[idx]:
            WHEN "svShowParameters"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cParameter = "NoParameters".
            WHEN "svShowReportHeader" THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "0^".
            WHEN "svShowPageHeader"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "2^".
            WHEN "svShowGroupHeader"  THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "4^".
            WHEN "svShowGroupFooter"  THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "5^".
            WHEN "svShowPageFooter"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "3^".
            WHEN "svShowReportFooter" THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "1^".
            OTHERWISE
            cReturnValue = cReturnValue
                         + "|" + TRIM(user-print.field-name[idx])
                         + "^" + user-print.field-value[idx]
                         .
        END CASE.
    END. /* do idx */
    ASSIGN
        cShow        = TRIM(cShow,"^") + "|"
        cReturnValue = cShow + cParameter + cReturnValue
        .
    RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

