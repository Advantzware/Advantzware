&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : salrep\dashrep.p
    
    Purpose     : Sales Rep Highlights
     
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-as-of-date AS DATE NO-UNDO.

/* Includes */
{sys/inc/var.i shared}
 
/* Temp-Tables */
{salrep\dashrep.i}

/* Variables for excel Automation  */
DEF VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chFile               AS CHAR         NO-UNDO.
DEF VARIABLE CurrDir              AS CHARACTER    NO-UNDO.

DEF VAR v-this-month AS INT NO-UNDO.
DEF VAR v-days-this-month AS INT NO-UNDO.

&GLOBAL-DEFINE SHT-MONTHLY 3
&GLOBAL-DEFINE SHT-YTD 2
&GLOBAL-DEFINE SHT-MTD 1
&GLOBAL-DEFINE SHT-RAW 4

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 22.81
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
  v-this-month = MONTH(ip-as-of-date)
  v-days-this-month = DAY(ip-as-of-date).
run InitializeExcel.
run MainLoop.
run Cleanup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Clean up routine.
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose   :   Initializes Excel Environment
  Parameters:   None
  Notes     :   
------------------------------------------------------------------------------*/

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication.

  FILE-INFO:FILE-NAME = "template\DashSales.xlt".

  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
  if search (chFile) = ? then do:
    MESSAGE 'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose     : Main Loop for the Report Logic
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.
  
  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /*Display Data*/
  RUN raw-salesmen-proc.
  chWorkbook:WorkSheets({&SHT-MTD}):Activate no-error.
/*   commenting out the 2 below because I get an unexplained error and have no 
     more time to diagnose */
/*   chWorkbook:WorkSheets({&salesrep}):Columns("C:AN"):AutoFit.    */
/*   chWorkbook:WorkSheets({&raw-salesmen}):Columns("A:R"):AutoFit. */

  ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&SHT-MTD})
     chExcelApplication:VISIBLE = TRUE
     /* enable screen updating */
     chExcelApplication:ScreenUpdating = TRUE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-salesmen-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-salesmen-proc Procedure 
PROCEDURE raw-salesmen-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT NO-UNDO.
   DEF VAR date-amt-total AS DEC NO-UNDO.
   DEF VAR date-msf-total AS DEC NO-UNDO.
   DEF VAR mtd-amt-total AS DEC NO-UNDO.
   DEF VAR mtd-msf-total AS DEC NO-UNDO.
   DEF VAR ytd-amt-total AS DEC NO-UNDO.
   DEF VAR ytd-msf-total AS DEC NO-UNDO.
   
   chWorkbook:WorkSheets({&SHT-RAW}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&SHT-RAW}).
     row-count = 6.

   FOR EACH tt-raw-salesmen:

       ASSIGN
         chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-raw-salesmen.sman
         chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-salesmen.sname
         chWorkSheet:Range("C" + STRING(row-count)):VALUE = tt-raw-salesmen.DATE
         chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.date-sf,1)
         chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.date-msf,1)
         chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.date-amt,2)
         chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.date-cost,2)
         chWorkSheet:Range("H" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-salesmen.date-profit,2)) + "%"
         chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-sf,1)
         chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-msf,1)
         chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-amt,2)
         chWorkSheet:Range("L" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-cost,2)
         chWorkSheet:Range("M" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-salesmen.mtd-profit,2)) + "%"
         chWorkSheet:Range("N" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-sf,1)
         chWorkSheet:Range("O" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-msf,1)
         chWorkSheet:Range("P" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-amt,2)
         chWorkSheet:Range("Q" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-cost,2)
         chWorkSheet:Range("R" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-salesmen.ytd-profit,2)) + "%"
         row-count = row-count + 1.
   END.

   chWorkbook:WorkSheets({&SHT-MTD}):Activate no-error.
   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&SHT-MTD}).
      row-count = 6.

   FOR EACH tt-raw-salesmen:

/*        IF row-count LT 41 THEN */

       ASSIGN
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-salesmen.sname
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-amt,2)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.mtd-msf,2)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.mtd-msf,2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.mtd-amt,2) /
                                                                ROUND(tt-raw-salesmen.mtd-msf,2),2)
                                                             ELSE 0
          mtd-amt-total = mtd-amt-total + ROUND(tt-raw-salesmen.mtd-amt,2)
          mtd-msf-total = mtd-msf-total + ROUND(tt-raw-salesmen.mtd-msf,2)
          ytd-amt-total = ytd-amt-total + ROUND(tt-raw-salesmen.ytd-amt,2)
          ytd-msf-total = ytd-msf-total + ROUND(tt-raw-salesmen.ytd-msf,2)
          row-count = row-count + 1.
   END.
/*    ASSIGN                                                                         */
/*       chWorkSheet:Range("C42"):VALUE = ROUND(mtd-amt-total,2)                     */
/*       chWorkSheet:Range("D42"):VALUE = ROUND(mtd-msf-total,2)                     */
/*       chWorkSheet:Range("E42"):VALUE = IF mtd-msf-total NE 0 THEN                 */
/*                                           ROUND(ROUND(mtd-amt-total,2) /          */
/*                                                 ROUND(mtd-msf-total,2),2) ELSE 0  */
/*       chWorkSheet:Range("C83"):VALUE = ROUND(ytd-amt-total,2)                     */
/*       chWorkSheet:Range("D83"):VALUE = ROUND(ytd-msf-total,2)                     */
/*       chWorkSheet:Range("E83"):VALUE = IF ytd-msf-total NE 0 THEN                 */
/*                                           ROUND(ROUND(ytd-amt-total,2) /          */
/*                                                 ROUND(ytd-msf-total,2),2) ELSE 0  */
/*                                                                                   */
/*       chWorkSheet:Range("AM125"):VALUE = ROUND(ytd-amt-total,2)                   */
/*       chWorkSheet:Range("AN125"):VALUE = ROUND(ytd-msf-total,2)                   */
/*       chWorkSheet:Range("AO125"):VALUE = IF ytd-msf-total NE 0 THEN               */
/*                                            ROUND(ROUND(ytd-amt-total,2) /         */
/*                                                  ROUND(ytd-msf-total,2),2) ELSE 0 */
  chWorkbook:WorkSheets({&SHT-YTD}):Activate no-error.

   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&SHT-YTD}).
      row-count = 6.

   FOR EACH tt-raw-salesmen:

       IF row-count LT 62 THEN

       ASSIGN
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-salesmen.sname
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-amt,2)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.ytd-msf,2)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.ytd-msf,2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.ytd-amt,2) /
                                                                      ROUND(tt-raw-salesmen.ytd-msf,2),2)
                                                             ELSE 0
          row-count = row-count + 1.
   END.

/*    row-count = 89.                                                                                         */
   chWorkbook:WorkSheets({&SHT-MONTHLY}):Activate no-error.
   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&SHT-MONTHLY}).
      row-count = 5.
   FOR EACH tt-raw-salesmen:

/*        IF row-count LT 124 THEN */
          ASSIGN
             chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-salesmen.sname
             chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[1],2)
             chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[1],2)
             chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[1],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[1],2) / 
                                                                ROUND(tt-raw-salesmen.msf[1],2),2)
                                                                ELSE 0
             chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[2],2)
             chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[2],2)
             chWorkSheet:Range("H" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[2],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[2],2) / 
                                                                ROUND(tt-raw-salesmen.msf[2],2),2)
                                                                ELSE 0
             chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[3],2)
             chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[3],2)
             chWorkSheet:Range("K" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[3],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[3],2) / 
                                                                ROUND(tt-raw-salesmen.msf[3],2),2)
                                                                ELSE 0
             chWorkSheet:Range("L" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[4],2)
             chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[4],2)
             chWorkSheet:Range("N" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[4],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[4],2) / 
                                                                ROUND(tt-raw-salesmen.msf[4],2),2)
                                                                ELSE 0
             chWorkSheet:Range("O" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[5],2)
             chWorkSheet:Range("P" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[5],2)
             chWorkSheet:Range("Q" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[5],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[5],2) / 
                                                                ROUND(tt-raw-salesmen.msf[5],2),2)
                                                                ELSE 0
             chWorkSheet:Range("R" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[6],2)
             chWorkSheet:Range("S" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[6],2)
             chWorkSheet:Range("T" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[6],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[6],2) / 
                                                                ROUND(tt-raw-salesmen.msf[6],2),2)
                                                                ELSE 0
             chWorkSheet:Range("U" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[7],2)
             chWorkSheet:Range("V" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[7],2)
             chWorkSheet:Range("W" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[7],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[7],2) / 
                                                                ROUND(tt-raw-salesmen.msf[7],2),2)
                                                                ELSE 0
             chWorkSheet:Range("X" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[8],2)
             chWorkSheet:Range("Y" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[8],2)
             chWorkSheet:Range("Z" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[8],2) NE 0 THEN
                                                                ROUND(ROUND(tt-raw-salesmen.amt[8],2) / 
                                                                ROUND(tt-raw-salesmen.msf[8],2),2)
                                                                ELSE 0
             chWorkSheet:Range("AA" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[9],2)
             chWorkSheet:Range("AB" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[9],2)
             chWorkSheet:Range("AC" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[9],2) NE 0 THEN
                                                                 ROUND(ROUND(tt-raw-salesmen.amt[9],2) / 
                                                                 ROUND(tt-raw-salesmen.msf[9],2),2)
                                                                 ELSE 0
             chWorkSheet:Range("AD" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[10],2)
             chWorkSheet:Range("AE" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[10],2)
             chWorkSheet:Range("AF" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[10],2) NE 0 THEN
                                                                 ROUND(ROUND(tt-raw-salesmen.amt[10],2) / 
                                                                 ROUND(tt-raw-salesmen.msf[10],2),2)
                                                                 ELSE 0
             chWorkSheet:Range("AG" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[11],2)
             chWorkSheet:Range("AH" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[11],2)
             chWorkSheet:Range("AI" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[11],2) NE 0 THEN
                                                                 ROUND(ROUND(tt-raw-salesmen.amt[11],2) / 
                                                                 ROUND(tt-raw-salesmen.msf[11],2),2)
                                                                 ELSE 0
             chWorkSheet:Range("AJ" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.amt[12],2)
             chWorkSheet:Range("AK" + STRING(row-count)):VALUE = ROUND(tt-raw-salesmen.msf[12],2)
             chWorkSheet:Range("AL" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.msf[12],2) NE 0 THEN
                                                                 ROUND(ROUND(tt-raw-salesmen.amt[12],2) / 
                                                                 ROUND(tt-raw-salesmen.msf[12],2),2)
                                                                 ELSE 0
             chWorkSheet:Range("AM" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-salesmen.ytd-amt,2))
             chWorkSheet:Range("AN" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-salesmen.ytd-msf,2))
             chWorkSheet:Range("AO" + STRING(row-count)):VALUE = IF ROUND(tt-raw-salesmen.ytd-msf,2) NE 0 THEN
                                                                    ROUND(ROUND(tt-raw-salesmen.ytd-amt,2) /
                                                                    ROUND(tt-raw-salesmen.ytd-msf,2),2)
                                                                 ELSE 0
             row-count = row-count + 1.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

