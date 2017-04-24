&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : salrep\dashprod.p
    
    Purpose     : Product Highlights
     
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
{salrep\dashprod.i}

/* Variables for excel Automation  */
DEF VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chFile               AS CHAR         NO-UNDO.
DEF VARIABLE CurrDir              AS CHARACTER    NO-UNDO.

DEF VAR v-this-month AS INT NO-UNDO.
DEF VAR v-days-this-month AS INT NO-UNDO.

&GLOBAL-DEFINE summary-sheet 1
&GLOBAL-DEFINE prod-sheet 2
&GLOBAL-DEFINE raw-prod-sheet 3

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

  FILE-INFO:FILE-NAME = "template\DashProd.xlt".

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
  RUN raw-prod-proc.

  chWorkbook:WorkSheets({&summary-sheet}):Columns("C:F"):AutoFit.
  chWorkbook:WorkSheets({&prod-sheet}):Columns("A:X"):AutoFit.
  chWorkbook:WorkSheets({&raw-prod-sheet}):Columns("A:AF"):AutoFit.
  chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

  ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     chExcelApplication:VISIBLE = TRUE
     /* enable screen updating */
     chExcelApplication:ScreenUpdating = TRUE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-prod-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-proc Procedure 
PROCEDURE raw-prod-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT INIT 5 NO-UNDO.
   DEF VAR date-qty-total AS DEC NO-UNDO.
   DEF VAR date-qty-msf-total AS DEC NO-UNDO.
   DEF VAR date-run-hrs-total AS DEC NO-UNDO.
   DEF VAR date-qty-run-hr-total AS DEC NO-UNDO.
   DEF VAR date-mr-hrs-total AS DEC NO-UNDO.
   DEF VAR date-dt-chg-total AS DEC NO-UNDO.
   DEF VAR date-total-dt-total AS DEC NO-UNDO.
   DEF VAR date-total-chg-hrs-total AS DEC NO-UNDO.
   DEF VAR date-dt-nc-total AS DEC NO-UNDO.
   DEF VAR date-total-hrs-total AS DEC NO-UNDO.
   DEF VAR date-std-hrs-total AS DEC NO-UNDO.
   DEF VAR ytd-qty-total AS DEC NO-UNDO.
   DEF VAR ytd-qty-msf-total AS DEC NO-UNDO.
   DEF VAR ytd-run-hrs-total AS DEC NO-UNDO.
   DEF VAR ytd-mr-hrs-total AS DEC NO-UNDO.
   DEF VAR ytd-qty-run-hr-total AS DEC NO-UNDO.
   DEF VAR ytd-dt-chg-total AS DEC NO-UNDO.
   DEF VAR ytd-total-dt-total AS DEC NO-UNDO.
   DEF VAR ytd-total-chg-hrs-total AS DEC NO-UNDO.
   DEF VAR ytd-dt-nc-total AS DEC NO-UNDO.
   DEF VAR ytd-total-hrs-total AS DEC NO-UNDO.
   DEF VAR ytd-std-hrs-total AS DEC NO-UNDO.
   DEF VAR mtd-qty-total AS DEC NO-UNDO.
   DEF VAR mtd-qty-msf-total AS DEC NO-UNDO.
   DEF VAR mtd-run-hrs-total AS DEC NO-UNDO.
   DEF VAR mtd-qty-run-hr-total AS DEC NO-UNDO.
   DEF VAR mtd-mr-hrs-total AS DEC NO-UNDO.
   DEF VAR mtd-dt-chg-total AS DEC NO-UNDO.
   DEF VAR mtd-total-dt-total AS DEC NO-UNDO.
   DEF VAR mtd-total-chg-hrs-total AS DEC NO-UNDO.
   DEF VAR mtd-dt-nc-total AS DEC NO-UNDO.
   DEF VAR mtd-total-hrs-total AS DEC NO-UNDO.
   DEF VAR mtd-std-hrs-total AS DEC NO-UNDO.

   chWorkbook:WorkSheets({&raw-prod-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&raw-prod-sheet}).

   FOR EACH tt-raw-prod:
       ASSIGN
          chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-raw-prod.m-code
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-prod.DATE
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-qty,0)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-qty,0)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty,0)
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-qty-msf,1)
          chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-qty-msf,1)
          chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty-msf,1)
          chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-run-hr,2)
          chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-run-hr,2)
          chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-run-hr,2)
          chWorkSheet:Range("L" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-mr-hrs,2)
          chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-mr-hrs,2)
          chWorkSheet:Range("N" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-mr-hrs,2)
          chWorkSheet:Range("O" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-dt-charge,2)
          chWorkSheet:Range("P" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-dt-charge,2)
          chWorkSheet:Range("Q" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-dt-charge,2)
          chWorkSheet:Range("R" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-dt-nc,2)
          chWorkSheet:Range("S" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-dt-nc,2)
          chWorkSheet:Range("T" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-dt-nc,2)
          chWorkSheet:Range("U" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-std-hrs,2)
          chWorkSheet:Range("V" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-std-hrs,2)
          chWorkSheet:Range("W" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-std-hrs,2)
          chWorkSheet:Range("X" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-eff,1)) + "%"
          chWorkSheet:Range("Y" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-eff,1)) + "%"
          chWorkSheet:Range("Z" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-eff,1)) + "%"
          chWorkSheet:Range("AA" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-util,1)) + "%"
          chWorkSheet:Range("AB" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-util,1)) + "%"
          chWorkSheet:Range("AC" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-util,1)) + "%"

          chWorkSheet:Range("AD" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-dt-perc,1)) + "%"
          chWorkSheet:Range("AE" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-dt-perc,1)) + "%"
          chWorkSheet:Range("AF" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-dt-perc,1)) + "%"
          row-count = row-count + 1.
   END.

   chWorkbook:WorkSheets({&prod-sheet}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&prod-sheet})
     row-count = 6.

   FOR EACH tt-raw-prod:
     ASSIGN
       chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-prod.m-code
       chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty,0)
       chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty-msf,1)
       chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-run-hrs,2)
       chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-mr-hrs,2)
       chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-eff,1)) + "%"
       row-count = row-count + 1.
   END.

   row-count = 27.
   FOR EACH tt-raw-prod:
     ASSIGN
       chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-prod.m-code
       chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-qty,0)
       chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-qty-msf,1)
       chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-run-hrs,2)
       chWorkSheet:Range("F" + STRING(row-count)):VALUE = IF tt-raw-prod.date-run-hrs NE 0 THEN
                                                             ROUND(ROUND(tt-raw-prod.date-qty,0) /
                                                                   ROUND(tt-raw-prod.date-run-hrs,2),0)
                                                          ELSE 0
       chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-mr-hrs,2)
       chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-dt-charge,2)
       chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-dt-charge +
                                                                tt-raw-prod.date-dt-nc,2)
       chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-run-hrs,2) +
                                                          ROUND(tt-raw-prod.date-mr-hrs,2) +
                                                          ROUND(tt-raw-prod.date-dt-charge,2)
       chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-dt-charge +
                                                                tt-raw-prod.date-dt-nc,2) - 
                                                          ROUND(tt-raw-prod.date-dt-charge,2)
       chWorkSheet:Range("L" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-run-hrs,2) +
                                                          ROUND(tt-raw-prod.date-mr-hrs,2) +
                                                          ROUND(tt-raw-prod.date-dt-charge,2) +
                                                          ROUND(tt-raw-prod.date-dt-charge +
                                                                tt-raw-prod.date-dt-nc,2) - 
                                                          ROUND(tt-raw-prod.date-dt-charge,2)
       chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.date-std-hrs,2)
       chWorkSheet:Range("N" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-eff,1)) + "%"
       chWorkSheet:Range("O" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-util,1)) + "%"
       chWorkSheet:Range("P" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.date-dt-perc,1)) + "%"

       date-qty-total = date-qty-total + ROUND(tt-raw-prod.date-qty,0)
       date-qty-msf-total = date-qty-msf-total + ROUND(tt-raw-prod.date-qty-msf,1)
       date-run-hrs-total = date-run-hrs-total + ROUND(tt-raw-prod.date-run-hrs,2)
       date-qty-run-hr-total = date-qty-run-hr-total + (IF tt-raw-prod.date-run-hrs NE 0 THEN
                                                        ROUND(ROUND(tt-raw-prod.date-qty,0) /
                                                              ROUND(tt-raw-prod.date-run-hrs,2),0)
                                                        ELSE 0)
       date-mr-hrs-total = date-mr-hrs-total + ROUND(tt-raw-prod.date-mr-hrs,2)
       date-dt-chg-total = date-dt-chg-total + ROUND(tt-raw-prod.date-dt-charge,2)
       date-total-dt-total = date-total-dt-total + ROUND(tt-raw-prod.date-dt-charge + tt-raw-prod.date-dt-nc,2)
       date-total-chg-hrs-total = date-total-chg-hrs-total + ROUND(tt-raw-prod.date-run-hrs,2) + ROUND(tt-raw-prod.date-mr-hrs,2) +
                                                             ROUND(tt-raw-prod.date-dt-charge,2)
       date-dt-nc-total = date-dt-nc-total + ROUND(tt-raw-prod.date-dt-charge + tt-raw-prod.date-dt-nc,2) - 
                                             ROUND(tt-raw-prod.date-dt-charge,2)
       date-total-hrs-total = date-total-hrs-total + ROUND(tt-raw-prod.date-run-hrs,2) +
                              ROUND(tt-raw-prod.date-mr-hrs,2) + ROUND(tt-raw-prod.date-dt-charge,2) +
                              ROUND(tt-raw-prod.date-dt-charge + tt-raw-prod.date-dt-nc,2) - 
                              ROUND(tt-raw-prod.date-dt-charge,2)
       date-std-hrs-total = date-std-hrs-total + ROUND(tt-raw-prod.date-std-hrs,2)
       mtd-qty-total = mtd-qty-total + ROUND(tt-raw-prod.mtd-qty,0)
       mtd-qty-msf-total = mtd-qty-msf-total + ROUND(tt-raw-prod.mtd-qty-msf,1)
       mtd-run-hrs-total = mtd-run-hrs-total + ROUND(tt-raw-prod.mtd-run-hrs,2)
       mtd-qty-run-hr-total = mtd-qty-run-hr-total + (IF tt-raw-prod.mtd-run-hrs NE 0 THEN
                                                      ROUND(ROUND(tt-raw-prod.mtd-qty,0) /
                                                            ROUND(tt-raw-prod.mtd-run-hrs,2),0)
                                                      ELSE 0)
       mtd-mr-hrs-total = mtd-mr-hrs-total + ROUND(tt-raw-prod.mtd-mr-hrs,2)
       mtd-dt-chg-total = mtd-dt-chg-total + ROUND(tt-raw-prod.mtd-dt-charge,2)
       mtd-total-dt-total = mtd-total-dt-total + ROUND(tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc,2)
       mtd-total-chg-hrs-total = mtd-total-chg-hrs-total + ROUND(tt-raw-prod.mtd-run-hrs,2) + ROUND(tt-raw-prod.mtd-mr-hrs,2) +
                                 ROUND(tt-raw-prod.mtd-dt-charge,2)
       mtd-dt-nc-total = mtd-dt-nc-total + ROUND(tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc,2) - 
                         ROUND(tt-raw-prod.mtd-dt-charge,2)
       mtd-total-hrs-total = mtd-total-hrs-total + ROUND(tt-raw-prod.mtd-run-hrs,2) +
                             ROUND(tt-raw-prod.mtd-mr-hrs,2) + ROUND(tt-raw-prod.mtd-dt-charge,2) + ROUND(tt-raw-prod.mtd-dt-charge +
                             tt-raw-prod.mtd-dt-nc,2) - ROUND(tt-raw-prod.mtd-dt-charge,2)
       mtd-std-hrs-total = mtd-std-hrs-total + ROUND(tt-raw-prod.mtd-std-hrs,2)
       ytd-qty-total = ytd-qty-total + ROUND(tt-raw-prod.ytd-qty,0)
       ytd-qty-msf-total = ytd-qty-msf-total + ROUND(tt-raw-prod.ytd-qty-msf,1)
       ytd-run-hrs-total = ytd-run-hrs-total + ROUND(tt-raw-prod.ytd-run-hrs,2)
       ytd-qty-run-hr-total = ytd-qty-run-hr-total + (IF tt-raw-prod.ytd-run-hrs NE 0 THEN
                                                      ROUND(ROUND(tt-raw-prod.ytd-qty,0) /
                                                            ROUND(tt-raw-prod.ytd-run-hrs,2),0)
                                                      ELSE 0)
       ytd-mr-hrs-total = ytd-mr-hrs-total + ROUND(tt-raw-prod.ytd-mr-hrs,2)
       ytd-dt-chg-total = ytd-dt-chg-total + ROUND(tt-raw-prod.ytd-dt-charge,2)
       ytd-total-dt-total = ytd-total-dt-total + ROUND(tt-raw-prod.ytd-dt-charge + tt-raw-prod.ytd-dt-nc,2)
       ytd-total-chg-hrs-total = ytd-total-chg-hrs-total + ROUND(tt-raw-prod.ytd-run-hrs,2) + ROUND(tt-raw-prod.ytd-mr-hrs,2) +
                                 ROUND(tt-raw-prod.ytd-dt-charge,2)
       ytd-dt-nc-total = ytd-dt-nc-total + ROUND(tt-raw-prod.ytd-dt-charge + tt-raw-prod.ytd-dt-nc,2) - 
                         ROUND(tt-raw-prod.ytd-dt-charge,2)
       ytd-total-hrs-total = ytd-total-hrs-total + ROUND(tt-raw-prod.ytd-run-hrs,2) +
                             ROUND(tt-raw-prod.ytd-mr-hrs,2) + ROUND(tt-raw-prod.ytd-dt-charge,2) + ROUND(tt-raw-prod.ytd-dt-charge +
                             tt-raw-prod.ytd-dt-nc,2) - ROUND(tt-raw-prod.ytd-dt-charge,2)
       ytd-std-hrs-total = ytd-std-hrs-total + ROUND(tt-raw-prod.ytd-std-hrs,2)
       row-count = row-count + 1.
             
   END.

   ASSIGN
     chWorkSheet:Range("C22"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("D22"):VALUE = ROUND(ytd-qty-msf-total,1)
     chWorkSheet:Range("E22"):VALUE = ROUND(ytd-run-hrs-total,2)
     chWorkSheet:Range("F22"):VALUE = ROUND(ytd-mr-hrs-total,2)
     chWorkSheet:Range("C43"):VALUE = ROUND(date-qty-total,0)
     chWorkSheet:Range("D43"):VALUE = ROUND(date-qty-msf-total,1)
     chWorkSheet:Range("E43"):VALUE = ROUND(date-run-hrs-total,2)
     chWorkSheet:Range("F43"):VALUE = ROUND(date-qty-run-hr-total,0)
     chWorkSheet:Range("G43"):VALUE = ROUND(date-mr-hrs-total,2)
     chWorkSheet:Range("H43"):VALUE = ROUND(date-dt-chg-total,2)
     chWorkSheet:Range("I43"):VALUE = ROUND(date-total-dt-total,2)
     chWorkSheet:Range("J43"):VALUE = ROUND(date-total-chg-hrs-total,2)
     chWorkSheet:Range("K43"):VALUE = ROUND(date-dt-nc-total,2)
     chWorkSheet:Range("L43"):VALUE = ROUND(date-total-hrs-total,2)
     chWorkSheet:Range("M43"):VALUE = ROUND(date-std-hrs-total,2)
     chWorkSheet:Range("C64"):VALUE = ROUND(mtd-qty-total,0)
     chWorkSheet:Range("D64"):VALUE = ROUND(mtd-qty-msf-total,1)
     chWorkSheet:Range("E64"):VALUE = ROUND(mtd-run-hrs-total,2)
     chWorkSheet:Range("F64"):VALUE = ROUND(mtd-qty-run-hr-total,0)
     chWorkSheet:Range("G64"):VALUE = ROUND(mtd-mr-hrs-total,2)
     chWorkSheet:Range("H64"):VALUE = ROUND(mtd-dt-chg-total,2)
     chWorkSheet:Range("I64"):VALUE = ROUND(mtd-total-dt-total,2)
     chWorkSheet:Range("J64"):VALUE = ROUND(mtd-total-chg-hrs-total,2)
     chWorkSheet:Range("K64"):VALUE = ROUND(mtd-dt-nc-total,2)
     chWorkSheet:Range("L64"):VALUE = ROUND(mtd-total-hrs-total,2)
     chWorkSheet:Range("M64"):VALUE = ROUND(mtd-std-hrs-total,2)
     chWorkSheet:Range("C85"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("D85"):VALUE = ROUND(ytd-qty-msf-total,1)
     chWorkSheet:Range("E85"):VALUE = ROUND(ytd-run-hrs-total,2)
     chWorkSheet:Range("F85"):VALUE = ROUND(ytd-qty-run-hr-total,0)
     chWorkSheet:Range("G85"):VALUE = ROUND(ytd-mr-hrs-total,2)
     chWorkSheet:Range("H85"):VALUE = ROUND(ytd-dt-chg-total,2)
     chWorkSheet:Range("I85"):VALUE = ROUND(ytd-total-dt-total,2)
     chWorkSheet:Range("J85"):VALUE = ROUND(ytd-total-chg-hrs-total,2)
     chWorkSheet:Range("K85"):VALUE = ROUND(ytd-dt-nc-total,2)
     chWorkSheet:Range("L85"):VALUE = ROUND(ytd-total-hrs-total,2)
     chWorkSheet:Range("M85"):VALUE = ROUND(ytd-std-hrs-total,2).

   row-count = 48.
   FOR EACH tt-raw-prod:
     ASSIGN
       chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-prod.m-code
       chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-qty,0)
       chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-qty-msf,1)
       chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-run-hrs,2)
       chWorkSheet:Range("F" + STRING(row-count)):VALUE = IF tt-raw-prod.mtd-run-hrs NE 0 THEN
                                                             ROUND(ROUND(tt-raw-prod.mtd-qty,0) /
                                                                   ROUND(tt-raw-prod.mtd-run-hrs,2),0)
                                                          ELSE 0
       chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-mr-hrs,2)
       chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-dt-charge,2)
       chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-dt-charge +
                                                                       tt-raw-prod.mtd-dt-nc,2)
       chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-run-hrs,2) +
                                                                 ROUND(tt-raw-prod.mtd-mr-hrs,2) +
                                                                 ROUND(tt-raw-prod.mtd-dt-charge,2)
       chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-dt-charge +
                                                                       tt-raw-prod.mtd-dt-nc,2) - 
                                                                 ROUND(tt-raw-prod.mtd-dt-charge,2)
       chWorkSheet:Range("L" + STRING(row-count)):VALUE = DEC(chWorkSheet:Range("J" + STRING(row-count)):VALUE)
                                                        + DEC(chWorkSheet:Range("K" + STRING(row-count)):VALUE)
       chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.mtd-std-hrs,2)
       chWorkSheet:Range("N" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-eff,1)) + "%"
       chWorkSheet:Range("O" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-util,1)) + "%"
       chWorkSheet:Range("P" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.mtd-dt-perc,1)) + "%"
       row-count = row-count + 1.
   END.

   row-count = 69.
   FOR EACH tt-raw-prod:
     ASSIGN
       chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-raw-prod.m-code
       chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty,0)
       chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-qty-msf,1)
       chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-run-hrs,2)
       chWorkSheet:Range("F" + STRING(row-count)):VALUE = IF tt-raw-prod.ytd-run-hrs NE 0 THEN
                                                             ROUND(ROUND(tt-raw-prod.ytd-qty,0) /
                                                                   ROUND(tt-raw-prod.ytd-run-hrs,2),0)
                                                          ELSE 0
       chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-mr-hrs,2)
       chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-dt-charge,2)
       chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-dt-charge +
                                                                       tt-raw-prod.ytd-dt-nc,2)
       chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-run-hrs,2) +
                                                          ROUND(tt-raw-prod.ytd-mr-hrs,2) +
                                                          ROUND(tt-raw-prod.ytd-dt-charge,2)
       chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-dt-charge +
                                                                tt-raw-prod.ytd-dt-nc,2) - 
                                                          ROUND(tt-raw-prod.ytd-dt-charge,2)
       chWorkSheet:Range("L" + STRING(row-count)):VALUE = DEC(chWorkSheet:Range("J" + STRING(row-count)):VALUE)
                                                        + DEC(chWorkSheet:Range("K" + STRING(row-count)):VALUE)
       chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-raw-prod.ytd-std-hrs,2)
       chWorkSheet:Range("N" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-eff,1)) + "%"
       chWorkSheet:Range("O" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-util,1)) + "%"
       chWorkSheet:Range("P" + STRING(row-count)):VALUE = STRING(ROUND(tt-raw-prod.ytd-dt-perc,1)) + "%"
       row-count = row-count + 1.
   END.

   chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     chWorkSheet:Range("G1"):VALUE = ip-company
     chWorkSheet:Range("C5"):VALUE = ip-as-of-date
     chWorkSheet:Range("D5"):VALUE = IF date-run-hrs-total + date-mr-hrs-total + date-dt-chg-total NE 0 THEN
                                        ROUND((date-std-hrs-total / (date-run-hrs-total + date-mr-hrs-total + date-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("E5"):VALUE = ROUND(date-qty-total,0)
     chWorkSheet:Range("F5"):VALUE = ROUND(date-qty-msf-total,0)
     chWorkSheet:Range("C6"):VALUE = ip-as-of-date
     chWorkSheet:Range("D6"):VALUE = IF mtd-run-hrs-total + mtd-mr-hrs-total + mtd-dt-chg-total NE 0 THEN
                                        ROUND((mtd-std-hrs-total / (mtd-run-hrs-total + mtd-mr-hrs-total + mtd-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("E6"):VALUE = ROUND(mtd-qty-total,0)
     chWorkSheet:Range("F6"):VALUE = ROUND(mtd-qty-msf-total,0)
     chWorkSheet:Range("C7"):VALUE = ip-as-of-date
     chWorkSheet:Range("E7"):VALUE = ROUND(ROUND(mtd-qty-total,0) / v-days-this-month,0)
     chWorkSheet:Range("F7"):VALUE = ROUND(ROUND(mtd-qty-total,0) / v-days-this-month,0)
     chWorkSheet:Range("C8"):VALUE = ip-as-of-date
     chWorkSheet:Range("D8"):VALUE = IF ytd-run-hrs-total + ytd-mr-hrs-total + ytd-dt-chg-total NE 0 THEN
                                        ROUND((ytd-std-hrs-total / (ytd-run-hrs-total + ytd-mr-hrs-total + ytd-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("E8"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("F8"):VALUE = ROUND(ytd-qty-msf-total,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

