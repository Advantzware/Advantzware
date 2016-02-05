&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : salrep\dashboard.p
    
    Purpose     : Management Highlights
     
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-as-of-date AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-dso AS DEC NO-UNDO.
DEFINE INPUT PARAMETER ip-no-cents AS LOG NO-UNDO.

/* Includes */
{sys/inc/var.i shared}
 
/* Temp-Tables */
{salrep\dashtt.i}

/* Variables for excel Automation  */
DEF VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chFile               AS CHAR         NO-UNDO.
DEF VARIABLE CurrDir              AS CHARACTER    NO-UNDO.

DEF VAR v-this-month AS INT NO-UNDO.
DEF VAR v-days-this-month AS INT NO-UNDO.

&GLOBAL-DEFINE summary-sheet 1
&GLOBAL-DEFINE bookings-sheet 2
&GLOBAL-DEFINE prod-sheet 3
&GLOBAL-DEFINE sales-summary-sheet 4
&GLOBAL-DEFINE prod-category 5
&GLOBAL-DEFINE salesrep 6
&GLOBAL-DEFINE sales-forecast 7
&GLOBAL-DEFINE raw-sales-pc 8
&GLOBAL-DEFINE raw-op-sheet 9
&GLOBAL-DEFINE raw-sales-sheet 10
&GLOBAL-DEFINE raw-salesmen 11
&GLOBAL-DEFINE raw-prod-sheet 12

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

&IF DEFINED(EXCLUDE-DisplayData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayData Procedure 
PROCEDURE DisplayData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
  RUN raw-op-proc.
  RUN raw-prod-proc.
  RUN raw-sales-proc.
  RUN raw-sales-summary-proc.
  RUN raw-product-cat-proc.
  RUN raw-salesmen-proc.
  RUN raw-sales-forecast-proc.
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

  FILE-INFO:FILE-NAME = "template\dashboard.xlt".

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
     chFile = FILE-INFO:FULL-PATHNAME. /*
     chExcelApplication:VISIBLE = TRUE.  */

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
  run DisplayData.
  
  /* resize columns for best fit */
  chWorkbook:WorkSheets({&summary-sheet}):Columns("D:I"):AutoFit.
  chWorkbook:WorkSheets({&summary-sheet}):Columns("M:P"):AutoFit.
  chWorkbook:WorkSheets({&bookings-sheet}):Columns("C:H"):AutoFit.
  chWorkbook:WorkSheets({&prod-sheet}):Columns("C:P"):AutoFit.
  chWorkbook:WorkSheets({&sales-summary-sheet}):Columns("C:N"):AutoFit.
  chWorkbook:WorkSheets({&prod-category}):Columns("C:G"):AutoFit.
  chWorkbook:WorkSheets({&salesrep}):Columns("A:AO"):AutoFit.
  chWorkbook:WorkSheets({&sales-forecast}):Columns("G:K"):AutoFit.
  chWorkbook:WorkSheets({&raw-sales-pc}):Columns("B:Q"):AutoFit.
  chWorkbook:WorkSheets({&raw-op-sheet}):Columns("A:K"):AutoFit.
  chWorkbook:WorkSheets({&raw-sales-sheet}):Columns("A:F"):AutoFit.
  chWorkbook:WorkSheets({&raw-salesmen}):Columns("A:R"):AutoFit.
  chWorkbook:WorkSheets({&raw-prod-sheet}):Columns("A:AF"):AutoFit.
  
  IF ip-no-cents THEN
     RUN rounding-proc.

  chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.
  chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet}).

  /* enable screen updating */
  chExcelApplication:VISIBLE = TRUE.
  chExcelApplication:ScreenUpdating = TRUE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-op-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-op-proc Procedure 
PROCEDURE raw-op-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT INIT 5 NO-UNDO.
   DEF VAR v-month AS INT NO-UNDO.
   DEF VAR v-date-start AS DATE NO-UNDO.
   DEF VAR v-date-end AS DATE NO-UNDO.
   DEF VAR v-oe-amt AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-oe-qty AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-oe-msf AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-oe-tons AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-oe-gp AS DEC EXTENT 12 NO-UNDO.
   
   DEF VAR v-oe-amt-mtd AS DEC NO-UNDO.
   DEF VAR v-oe-qty-mtd AS DEC NO-UNDO.
   DEF VAR v-oe-msf-mtd AS DEC NO-UNDO.

   DEF VAR v-oe-amt-ytd AS DEC NO-UNDO.
   DEF VAR v-oe-qty-ytd AS DEC NO-UNDO.
   DEF VAR v-oe-msf-ytd AS DEC NO-UNDO.

   DEF VAR v-oe-amt-tot AS DEC NO-UNDO.
   DEF VAR v-oe-qty-tot AS DEC NO-UNDO.
   DEF VAR v-oe-msf-tot AS DEC NO-UNDO.
   DEF VAR v-oe-tons-tot AS DEC NO-UNDO.
   DEF VAR v-oe-gp-tot AS DEC NO-UNDO.
   
   DEF VAR v-rel-amt AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-rel-msf AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-rel-tons AS DEC EXTENT 12 NO-UNDO.
   DEF VAR v-rel-gp AS DEC EXTENT 12 NO-UNDO.
   
   DEF VAR v-rel-amt-tot AS DEC NO-UNDO.
   DEF VAR v-rel-msf-tot AS DEC NO-UNDO.
   DEF VAR v-rel-tons-tot AS DEC NO-UNDO.
   DEF VAR v-rel-gp-tot AS DEC NO-UNDO.

   chWorkbook:WorkSheets({&raw-op-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&raw-op-sheet}).

   FOR EACH tt-raw-op:

       ASSIGN
          chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-raw-op.DATE
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = ROUND(tt-raw-op.oe-dollars,0)
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-op.oe-qty,0)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-op.oe-qty-msf,0)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-op.oe-qty-tons,1)
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-op.oe-gp,0)

          chWorkSheet:Range("G" + STRING(row-count)):VALUE = ROUND(tt-raw-op.rel-dollars,0)
          chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-raw-op.rel-qty,0)
          chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-raw-op.rel-qty-msf,0)
          chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-raw-op.rel-qty-tons,1)
          chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-raw-op.rel-gp,0)
          row-count = row-count + 1.
   END.

   ASSIGN
     v-date-start = DATE(1,1,YEAR(ip-as-of-date))
     v-date-end   = DATE(12,31,YEAR(ip-as-of-date)).

   FOR EACH tt-raw-op WHERE
       tt-raw-op.DATE GE v-date-start AND
       tt-raw-op.DATE LE v-date-end:

       ASSIGN
         v-month = MONTH(tt-raw-op.DATE)

         v-oe-amt[v-month] = v-oe-amt[v-month] + ROUND(tt-raw-op.oe-dollars,0)
         v-oe-qty[v-month] = v-oe-qty[v-month] + ROUND(tt-raw-op.oe-qty,0)
         v-oe-msf[v-month] = v-oe-msf[v-month] + ROUND(tt-raw-op.oe-qty-msf,0)
         v-oe-tons[v-month] = v-oe-tons[v-month] + ROUND(tt-raw-op.oe-qty-tons,1)
         v-oe-gp[v-month]   = v-oe-gp[v-month] + ROUND(tt-raw-op.oe-gp,0)
         
         v-rel-amt[v-month] = v-rel-amt[v-month] + ROUND(tt-raw-op.rel-dollars,0)
         v-rel-msf[v-month] = v-rel-msf[v-month] + ROUND(tt-raw-op.rel-qty-msf,0)
         v-rel-tons[v-month] = v-rel-tons[v-month] + ROUND(tt-raw-op.rel-qty-tons,1)
         v-rel-gp[v-month]   = v-rel-gp[v-month] + ROUND(tt-raw-op.rel-gp,0).

       IF tt-raw-op.DATE LE ip-as-of-date THEN
       DO:
          IF v-month EQ v-this-month THEN
             ASSIGN
                v-oe-amt-mtd = v-oe-amt-mtd + ROUND(tt-raw-op.oe-dollars,0)
                v-oe-qty-mtd = v-oe-qty-mtd + ROUND(tt-raw-op.oe-qty,0)
                v-oe-msf-mtd = v-oe-msf-mtd + ROUND(tt-raw-op.oe-qty-msf,0).

          ASSIGN
                v-oe-amt-ytd = v-oe-amt-ytd + ROUND(tt-raw-op.oe-dollars,0)
                v-oe-qty-ytd = v-oe-qty-ytd + ROUND(tt-raw-op.oe-qty,0)
                v-oe-msf-ytd = v-oe-msf-ytd + ROUND(tt-raw-op.oe-qty-msf,0).
       END.
       
   END.

   DO v-month = 1 TO 12:
      ASSIGN
        v-oe-tons[v-month]    = ROUND(v-oe-tons[v-month],0)
        v-rel-tons[v-month]   = ROUND(v-rel-tons[v-month],0)
        v-oe-amt-tot       = v-oe-amt-tot + v-oe-amt[v-month]
        v-oe-qty-tot       = v-oe-qty-tot + v-oe-qty[v-month]
        v-oe-msf-tot       = v-oe-msf-tot + v-oe-msf[v-month]
        v-oe-tons-tot      = v-oe-tons-tot + v-oe-tons[v-month]
        v-oe-gp-tot        = v-oe-gp-tot   + v-oe-tons[v-month]
                                    
        v-rel-amt-tot       = v-rel-amt-tot + v-rel-amt[v-month]
        v-rel-msf-tot       = v-rel-msf-tot + v-rel-msf[v-month]
        v-rel-tons-tot      = v-rel-tons-tot + v-rel-tons[v-month]
        v-rel-gp-tot        = v-rel-gp-tot   + v-rel-tons[v-month].
   END.

   chWorkbook:WorkSheets({&bookings-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&bookings-sheet}).

   DO i = 6 TO 17:
      ASSIGN
        chWorkSheet:Range("C" + STRING(i)):VALUE = v-oe-amt[i - 5]
        chWorkSheet:Range("D" + STRING(i)):VALUE = v-oe-msf[i - 5]
        chWorkSheet:Range("E" + STRING(i)):VALUE = IF v-oe-msf[i - 5] NE 0 THEN
                                                      STRING(ROUND(v-oe-amt[i - 5] /
                                                            v-oe-msf[i - 5],2))
                                                   ELSE "0.00"
        chWorkSheet:Range("F" + STRING(i)):VALUE = v-oe-tons[i - 5]
        chWorkSheet:Range("G" + STRING(i)):VALUE = v-oe-gp[i - 5]
        chWorkSheet:Range("H" + STRING(i)):VALUE = IF v-oe-amt[i - 5] NE 0 THEN
                                                      STRING(ROUND(v-oe-gp[i - 5] /
                                                                   v-oe-amt[i - 5],4) * 100) + "%"
                                                   ELSE "0.00%".
   END.

   DO i = 25 TO 36:
      ASSIGN
        chWorkSheet:Range("C" + STRING(i)):VALUE = v-rel-amt[i - 24]
        chWorkSheet:Range("D" + STRING(i)):VALUE = v-rel-msf[i - 24]
        chWorkSheet:Range("E" + STRING(i)):VALUE = IF v-rel-msf[i - 24] NE 0 THEN
                                                      STRING(ROUND(v-rel-amt[i - 24] /
                                                            v-rel-msf[i - 24],4))
                                                   ELSE "0.00"
        chWorkSheet:Range("F" + STRING(i)):VALUE = v-rel-tons[i - 24]
        chWorkSheet:Range("G" + STRING(i)):VALUE = v-rel-gp[i - 24]
        chWorkSheet:Range("H" + STRING(i)):VALUE = IF v-rel-amt[i - 24] NE 0 THEN
                                                      STRING(ROUND(v-rel-gp[i - 24] /
                                                                   v-rel-amt[i - 24],4) * 100) + "%"
                                                   ELSE "0.00%".
   END.

   ASSIGN
     chWorkSheet:Range("C19"):VALUE = v-oe-amt-tot
     chWorkSheet:Range("D19"):VALUE = v-oe-msf-tot
     chWorkSheet:Range("E19"):VALUE = IF v-oe-msf-tot NE 0 THEN
                                         STRING(ROUND(v-oe-amt-tot / v-oe-msf-tot,4) * 100) + "%"
                                      ELSE "0.00%"
     chWorkSheet:Range("F19"):VALUE = v-oe-tons-tot
     chWorkSheet:Range("G19"):VALUE = v-oe-gp-tot
     chWorkSheet:Range("H19"):VALUE = IF v-oe-amt-tot NE 0 THEN
                                         STRING(ROUND(v-oe-gp-tot / v-oe-amt-tot,4) * 100) + "%"
                                      ELSE "0.00%"
                                          
     chWorkSheet:Range("C20"):VALUE = v-oe-amt-tot / 12
     chWorkSheet:Range("D20"):VALUE = v-oe-msf-tot / 12
     chWorkSheet:Range("E20"):VALUE = IF v-oe-msf-tot NE 0 THEN
                                         STRING(ROUND((v-oe-amt-tot / v-oe-msf-tot) / 12,4) * 100) + "%"
                                      ELSE "0.00%"
     chWorkSheet:Range("F20"):VALUE = v-oe-tons-tot / 12
     chWorkSheet:Range("G20"):VALUE = v-oe-gp-tot / 12
     chWorkSheet:Range("H20"):VALUE = IF v-oe-amt-tot NE 0 THEN
                                         STRING(ROUND((v-oe-gp-tot / v-oe-amt-tot) / 12,4) * 100) + "%"
                                      ELSE "0.00%"
                                          
     chWorkSheet:Range("C38"):VALUE = v-rel-amt-tot
     chWorkSheet:Range("D38"):VALUE = v-rel-msf-tot
     chWorkSheet:Range("E38"):VALUE = IF v-rel-msf-tot NE 0 THEN
                                         STRING(ROUND(v-rel-amt-tot / v-rel-msf-tot,4) * 100) + "%"
                                      ELSE "0.00%"
     chWorkSheet:Range("F38"):VALUE = v-rel-tons-tot
     chWorkSheet:Range("G38"):VALUE = v-rel-gp-tot
     chWorkSheet:Range("H38"):VALUE = IF v-rel-amt-tot NE 0 THEN
                                         STRING(ROUND(v-rel-gp-tot / v-rel-amt-tot,4) * 100) + "%"
                                      ELSE "0.00%"
                                          
     chWorkSheet:Range("C39"):VALUE = v-rel-amt-tot / 12
     chWorkSheet:Range("D39"):VALUE = v-rel-msf-tot / 12
     chWorkSheet:Range("E39"):VALUE = IF v-rel-msf-tot NE 0 THEN
                                         STRING(ROUND((v-rel-amt-tot / v-rel-msf-tot) / 12,4) * 100) + "%"
                                      ELSE "0.00%"
     chWorkSheet:Range("F39"):VALUE = v-rel-tons-tot / 12
     chWorkSheet:Range("G39"):VALUE = v-rel-gp-tot / 12
     chWorkSheet:Range("H39"):VALUE = IF v-rel-amt-tot NE 0 THEN
                                         STRING(ROUND((v-rel-gp-tot / v-rel-amt-tot) / 12,4) * 100) + "%"
                                      ELSE "0.00%".

   chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     chWorkSheet:Range("D5"):VALUE = ip-as-of-date
     chWorkSheet:Range("D6"):VALUE = ip-as-of-date
     chWorkSheet:Range("D7"):VALUE = ip-as-of-date
     chWorkSheet:Range("D8"):VALUE = ip-as-of-date.

   FIND FIRST tt-raw-op WHERE
        tt-raw-op.DATE EQ ip-as-of-date
        NO-ERROR.

   IF AVAIL tt-raw-op THEN
   DO:
      ASSIGN
        chWorkSheet:Range("E5"):VALUE = ROUND(tt-raw-op.oe-dollars,0)
        chWorkSheet:Range("F5"):VALUE = ROUND(tt-raw-op.oe-qty,0)
        chWorkSheet:Range("G5"):VALUE = ROUND(tt-raw-op.oe-qty-msf,0)
        chWorkSheet:Range("H5"):VALUE = IF ROUND(tt-raw-op.oe-qty-msf,0) NE 0 THEN
                                           ROUND(tt-raw-op.oe-dollars,0) / ROUND(tt-raw-op.oe-qty-msf,0)
                                        ELSE 0.
      RELEASE tt-raw-op.
   END.

   ASSIGN
      chWorkSheet:Range("E6"):VALUE = v-oe-amt-mtd
      chWorkSheet:Range("F6"):VALUE = v-oe-qty-mtd
      chWorkSheet:Range("G6"):VALUE = v-oe-msf-mtd
      chWorkSheet:Range("H6"):VALUE = IF v-oe-msf-mtd NE 0 THEN
                                         ROUND(v-oe-amt-mtd / v-oe-msf-mtd,0)
                                      ELSE 0
      chWorkSheet:Range("E7"):VALUE = ROUND(v-oe-amt-mtd / v-days-this-month,0)
      chWorkSheet:Range("F7"):VALUE = ROUND(v-oe-qty-mtd / v-days-this-month,0)
      chWorkSheet:Range("G7"):VALUE = ROUND(v-oe-msf-mtd / v-days-this-month,0)
      chWorkSheet:Range("H7"):VALUE = IF v-oe-msf-mtd NE 0 THEN
                                         ROUND(ROUND(v-oe-amt-mtd / v-oe-msf-mtd,0) / v-days-this-month,0)
                                      ELSE 0
      chWorkSheet:Range("E8"):VALUE = v-oe-amt-ytd
      chWorkSheet:Range("F8"):VALUE = v-oe-qty-ytd
      chWorkSheet:Range("G8"):VALUE = v-oe-msf-ytd
      chWorkSheet:Range("H8"):VALUE = IF v-oe-msf-ytd NE 0 THEN
                                         ROUND(v-oe-amt-ytd / v-oe-msf-ytd,0)
                                      ELSE 0
      chWorkSheet:Range("M15"):VALUE = ip-dso.

   FIND FIRST tt-ar-ap NO-ERROR.

   IF AVAIL tt-ar-ap THEN
      ASSIGN
        chWorkSheet:Range("M12"):VALUE = ip-as-of-date
        chWorkSheet:Range("M13"):VALUE = ip-as-of-date        
        chWorkSheet:Range("N12"):VALUE = tt-ar-ap.date-ar-rec-amt
        chWorkSheet:Range("N13"):VALUE = tt-ar-ap.mtd-ar-rec-amt
        chWorkSheet:Range("N14"):VALUE = ROUND(tt-ar-ap.mtd-ar-rec-amt / v-days-this-month,2)
        chWorkSheet:Range("O12"):VALUE = tt-ar-ap.date-ap-paid-amt
        chWorkSheet:Range("O13"):VALUE = tt-ar-ap.mtd-ap-paid-amt
        chWorkSheet:Range("O14"):VALUE = ROUND(tt-ar-ap.mtd-ap-paid-amt /  v-days-this-month,2).

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

   row-count = 111.
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
     chWorkSheet:Range("C106"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("D106"):VALUE = ROUND(ytd-qty-msf-total,1)
     chWorkSheet:Range("E106"):VALUE = ROUND(ytd-run-hrs-total,2)
     chWorkSheet:Range("F106"):VALUE = ROUND(ytd-mr-hrs-total,2)
     chWorkSheet:Range("C211"):VALUE = ROUND(date-qty-total,0)
     chWorkSheet:Range("D211"):VALUE = ROUND(date-qty-msf-total,1)
     chWorkSheet:Range("E211"):VALUE = ROUND(date-run-hrs-total,2)
     chWorkSheet:Range("F211"):VALUE = ROUND(date-qty-run-hr-total,0)
     chWorkSheet:Range("G211"):VALUE = ROUND(date-mr-hrs-total,2)
     chWorkSheet:Range("H211"):VALUE = ROUND(date-dt-chg-total,2)
     chWorkSheet:Range("I211"):VALUE = ROUND(date-total-dt-total,2)
     chWorkSheet:Range("J211"):VALUE = ROUND(date-total-chg-hrs-total,2)
     chWorkSheet:Range("K211"):VALUE = ROUND(date-dt-nc-total,2)
     chWorkSheet:Range("L211"):VALUE = ROUND(date-total-hrs-total,2)
     chWorkSheet:Range("M211"):VALUE = ROUND(date-std-hrs-total,2)
     chWorkSheet:Range("C316"):VALUE = ROUND(mtd-qty-total,0)
     chWorkSheet:Range("D316"):VALUE = ROUND(mtd-qty-msf-total,1)
     chWorkSheet:Range("E316"):VALUE = ROUND(mtd-run-hrs-total,2)
     chWorkSheet:Range("F316"):VALUE = ROUND(mtd-qty-run-hr-total,0)
     chWorkSheet:Range("G316"):VALUE = ROUND(mtd-mr-hrs-total,2)
     chWorkSheet:Range("H316"):VALUE = ROUND(mtd-dt-chg-total,2)
     chWorkSheet:Range("I316"):VALUE = ROUND(mtd-total-dt-total,2)
     chWorkSheet:Range("J316"):VALUE = ROUND(mtd-total-chg-hrs-total,2)
     chWorkSheet:Range("K316"):VALUE = ROUND(mtd-dt-nc-total,2)
     chWorkSheet:Range("L316"):VALUE = ROUND(mtd-total-hrs-total,2)
     chWorkSheet:Range("M316"):VALUE = ROUND(mtd-std-hrs-total,2)
     chWorkSheet:Range("C421"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("D421"):VALUE = ROUND(ytd-qty-msf-total,1)
     chWorkSheet:Range("E421"):VALUE = ROUND(ytd-run-hrs-total,2)
     chWorkSheet:Range("F421"):VALUE = ROUND(ytd-qty-run-hr-total,0)
     chWorkSheet:Range("G421"):VALUE = ROUND(ytd-mr-hrs-total,2)
     chWorkSheet:Range("H421"):VALUE = ROUND(ytd-dt-chg-total,2)
     chWorkSheet:Range("I421"):VALUE = ROUND(ytd-total-dt-total,2)
     chWorkSheet:Range("J421"):VALUE = ROUND(ytd-total-chg-hrs-total,2)
     chWorkSheet:Range("K421"):VALUE = ROUND(ytd-dt-nc-total,2)
     chWorkSheet:Range("L421"):VALUE = ROUND(ytd-total-hrs-total,2)
     chWorkSheet:Range("M421"):VALUE = ROUND(ytd-std-hrs-total,2).

   row-count = 216.
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

   row-count = 321.
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
     chWorkSheet:Range("M5"):VALUE = ip-as-of-date
     chWorkSheet:Range("N5"):VALUE = IF date-run-hrs-total + date-mr-hrs-total + date-dt-chg-total NE 0 THEN
                                        ROUND((date-std-hrs-total / (date-run-hrs-total + date-mr-hrs-total + date-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("O5"):VALUE = ROUND(date-qty-total,0)
     chWorkSheet:Range("P5"):VALUE = ROUND(date-qty-msf-total,0)
     chWorkSheet:Range("M6"):VALUE = ip-as-of-date
     chWorkSheet:Range("N6"):VALUE = IF mtd-run-hrs-total + mtd-mr-hrs-total + mtd-dt-chg-total NE 0 THEN
                                        ROUND((mtd-std-hrs-total / (mtd-run-hrs-total + mtd-mr-hrs-total + mtd-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("O6"):VALUE = ROUND(mtd-qty-total,0)
     chWorkSheet:Range("P6"):VALUE = ROUND(mtd-qty-msf-total,0)
     chWorkSheet:Range("M7"):VALUE = ip-as-of-date
     chWorkSheet:Range("O7"):VALUE = ROUND(ROUND(mtd-qty-total,0) / v-days-this-month,0)
     chWorkSheet:Range("P7"):VALUE = ROUND(ROUND(mtd-qty-total,0) / v-days-this-month,0)
     chWorkSheet:Range("M8"):VALUE = ip-as-of-date
     chWorkSheet:Range("N8"):VALUE = IF ytd-run-hrs-total + ytd-mr-hrs-total + ytd-dt-chg-total NE 0 THEN
                                        ROUND((ytd-std-hrs-total / (ytd-run-hrs-total + ytd-mr-hrs-total + ytd-dt-chg-total)) * 100,2)
                                     ELSE 0
     chWorkSheet:Range("O8"):VALUE = ROUND(ytd-qty-total,0)
     chWorkSheet:Range("P8"):VALUE = ROUND(ytd-qty-msf-total,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-product-cat-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-product-cat-proc Procedure 
PROCEDURE raw-product-cat-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT NO-UNDO.
   DEF VAR date-amt-total AS DEC NO-UNDO.
   DEF VAR date-msf-total AS DEC NO-UNDO.
   DEF VAR date-cost-total AS DEC NO-UNDO.
   DEF VAR mtd-amt-total AS DEC NO-UNDO.
   DEF VAR mtd-msf-total AS DEC NO-UNDO.
   DEF VAR mtd-cost-total AS DEC NO-UNDO.
   DEF VAR ytd-amt-total AS DEC NO-UNDO.
   DEF VAR ytd-msf-total AS DEC NO-UNDO.
   DEF VAR ytd-cost-total AS DEC NO-UNDO.

   chWorkbook:WorkSheets({&raw-sales-pc}):Activate no-error.

   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&raw-sales-pc})
      row-count = 6.

   FOR EACH tt-sales-prod-cat:

       ASSIGN
         chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-sales-prod-cat.prod-cat
         chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-sales-prod-cat.DATE
         chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-sf,1)
         chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-msf,1)
         chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-amt,2)
         chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-cost,0)
         chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.date-profit,2)) + "%"
         chWorkSheet:Range("H" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-sf,1)
         chWorkSheet:Range("I" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-msf,1)
         chWorkSheet:Range("J" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-amt,2)
         chWorkSheet:Range("K" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-cost,0)
         chWorkSheet:Range("L" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.mtd-profit,2)) + "%"
         chWorkSheet:Range("M" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-sf,1)
         chWorkSheet:Range("N" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-msf,1)
         chWorkSheet:Range("O" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-amt,2)
         chWorkSheet:Range("P" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-cost,0)
         chWorkSheet:Range("Q" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.ytd-profit,2)) + "%"
         row-count = row-count + 1.
   END.

   chWorkbook:WorkSheets({&prod-category}):Activate no-error.

   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&prod-category}).
      row-count = 6.

   FOR EACH tt-sales-prod-cat:

       IF row-count LT 21 THEN

       ASSIGN
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-sales-prod-cat.prod-cat
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-amt,2)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-msf,2)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-sales-prod-cat.ytd-msf,2) NE 0 THEN
                                                                ROUND(ROUND(tt-sales-prod-cat.ytd-amt,2) /
                                                                ROUND(tt-sales-prod-cat.ytd-msf,2),2)
                                                             ELSE 0
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.ytd-cost,0)
          chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.ytd-profit,2)) + "%"
          date-amt-total = date-amt-total + ROUND(tt-sales-prod-cat.date-amt,2)
          date-msf-total = date-msf-total + ROUND(tt-sales-prod-cat.date-msf,2)
          date-cost-total = date-cost-total + ROUND(tt-sales-prod-cat.date-cost,0)
          mtd-amt-total = mtd-amt-total + ROUND(tt-sales-prod-cat.mtd-amt,2)
          mtd-msf-total = mtd-msf-total + ROUND(tt-sales-prod-cat.mtd-msf,2)
          mtd-cost-total = mtd-cost-total + ROUND(tt-sales-prod-cat.mtd-cost,0)
          ytd-amt-total = ytd-amt-total + ROUND(tt-sales-prod-cat.ytd-amt,2)
          ytd-msf-total = ytd-msf-total + ROUND(tt-sales-prod-cat.ytd-msf,2)
          ytd-cost-total = ytd-cost-total + ROUND(tt-sales-prod-cat.ytd-cost,0)
          row-count = row-count + 1.
   END.

   ASSIGN
      chWorkSheet:Range("C22"):VALUE = ROUND(ytd-amt-total,2)
      chWorkSheet:Range("D22"):VALUE = ROUND(ytd-msf-total,2)
      chWorkSheet:Range("E22"):VALUE = IF ytd-msf-total NE 0 THEN
                                          ROUND(ROUND(ytd-amt-total,2) /
                                          ROUND(ytd-msf-total,2),2) ELSE 0
      chWorkSheet:Range("F22"):VALUE = ytd-cost-total
      chWorkSheet:Range("G22"):VALUE = IF ROUND(ytd-amt-total,2) NE 0 THEN
                                          (ROUND(ytd-amt-total,2) - ytd-cost-total) / 
                                          ROUND(ytd-amt-total,2) * 100 
                                       ELSE 0
      chWorkSheet:Range("C43"):VALUE = ROUND(date-amt-total,2)
      chWorkSheet:Range("D43"):VALUE = ROUND(date-msf-total,2)
      chWorkSheet:Range("E43"):VALUE = IF date-msf-total NE 0 THEN
                                          ROUND(ROUND(date-amt-total,2) /
                                                ROUND(date-msf-total,2),2) ELSE 0
      chWorkSheet:Range("F43"):VALUE = date-cost-total

      chWorkSheet:Range("C64" + STRING(row-count)):VALUE = ROUND(mtd-amt-total,2)
      chWorkSheet:Range("D64" + STRING(row-count)):VALUE = ROUND(mtd-msf-total,2)
      chWorkSheet:Range("E64" + STRING(row-count)):VALUE = IF mtd-msf-total NE 0 THEN
                                                              ROUND(ROUND(mtd-amt-total,2) /
                                                                    ROUND(mtd-msf-total,2),2) ELSE 0
      chWorkSheet:Range("F64"):VALUE = mtd-cost-total
      row-count = 27.

   FOR EACH tt-sales-prod-cat:

       IF row-count LT 42 THEN

       ASSIGN
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-sales-prod-cat.prod-cat
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-amt,2)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-msf,2)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-sales-prod-cat.date-msf,2) NE 0 THEN
                                                                ROUND(ROUND(tt-sales-prod-cat.date-amt,2) /
                                                                      ROUND(tt-sales-prod-cat.date-msf,2),2)
                                                             ELSE 0
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.date-cost,0)
          chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.date-profit,2)) + "%"
          row-count = row-count + 1.
   END.

   row-count = 48.

   FOR EACH tt-sales-prod-cat:

       IF row-count LT 63 THEN

       ASSIGN
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = tt-sales-prod-cat.prod-cat
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-amt,2)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-msf,2)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF ROUND(tt-sales-prod-cat.mtd-msf,2) NE 0 THEN
                                                                ROUND(ROUND(tt-sales-prod-cat.mtd-amt,2) /
                                                                      ROUND(tt-sales-prod-cat.mtd-msf,2),2)
                                                             ELSE 0
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-sales-prod-cat.mtd-cost,0)
          chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(ROUND(tt-sales-prod-cat.mtd-profit,2)) + "%"
          row-count = row-count + 1.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-sales-forecast-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-sales-forecast-proc Procedure 
PROCEDURE raw-sales-forecast-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR letter-array AS CHAR EXTENT 4 NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.
  DEF VAR total-invoiced-amt AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR total-invoiced-qty AS DEC NO-UNDO.
  DEF VAR total-invoiced-msf AS DEC NO-UNDO.
  DEF VAR total-invoiced-profit AS DEC DECIMALS 2 NO-UNDO.

  DEF VAR total-backlog-amt AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR total-backlog-qty AS DEC NO-UNDO.
  DEF VAR total-backlog-msf AS DEC NO-UNDO.
  DEF VAR total-backlog-profit AS DEC DECIMALS 2 NO-UNDO.

  DEF VAR total-released-amt AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR total-released-qty AS DEC NO-UNDO.
  DEF VAR total-released-msf AS DEC NO-UNDO.
  DEF VAR total-released-profit AS DEC DECIMALS 2 NO-UNDO.

  chWorkbook:WorkSheets({&sales-forecast}):Activate no-error.

  ASSIGN
    chWorkSheet = chExcelApplication:Sheets:item({&sales-forecast})
    letter-array[1] = 'G'
    letter-array[2] = 'H'
    letter-array[3] = 'I'
    letter-array[4] = 'J'.

  FOR EACH tt-sales-forecast:
      ASSIGN
         v-count = v-count + 1
         chWorkSheet:Range(letter-array[v-count] + "5"):VALUE = tt-sales-forecast.company
         chWorkSheet:Range(letter-array[v-count] + "6"):VALUE = ROUND(tt-sales-forecast.invoiced-amt,2)
         chWorkSheet:Range(letter-array[v-count] + "7"):VALUE = ROUND(tt-sales-forecast.backlog-amt,2)
         chWorkSheet:Range(letter-array[v-count] + "8"):VALUE = ROUND(tt-sales-forecast.released-amt,2)
         chWorkSheet:Range(letter-array[v-count] + "9"):VALUE = ROUND(tt-sales-forecast.invoiced-amt,2)
                                                              + ROUND(tt-sales-forecast.backlog-amt,2)
                                                              + ROUND(tt-sales-forecast.released-amt,2)

         total-invoiced-amt = total-invoiced-amt + ROUND(tt-sales-forecast.invoiced-amt,2)
         total-backlog-amt = total-backlog-amt + ROUND(tt-sales-forecast.backlog-amt,2)
         total-released-amt = total-released-amt + ROUND(tt-sales-forecast.released-amt,2)

         chWorkSheet:Range(letter-array[v-count] + "15"):VALUE = tt-sales-forecast.company
         chWorkSheet:Range(letter-array[v-count] + "16"):VALUE = ROUND(tt-sales-forecast.invoiced-qty,0)
         chWorkSheet:Range(letter-array[v-count] + "17"):VALUE = ROUND(tt-sales-forecast.backlog-qty,0)
         chWorkSheet:Range(letter-array[v-count] + "18"):VALUE = ROUND(tt-sales-forecast.released-qty,0)
         chWorkSheet:Range(letter-array[v-count] + "19"):VALUE = ROUND(tt-sales-forecast.invoiced-qty,0)
                                                               + ROUND(tt-sales-forecast.backlog-qty,0)
                                                               + ROUND(tt-sales-forecast.released-qty,0)

         total-invoiced-qty = total-invoiced-qty + ROUND(tt-sales-forecast.invoiced-qty,0)
         total-backlog-qty = total-backlog-qty + ROUND(tt-sales-forecast.backlog-qty,0)
         total-released-qty = total-released-qty + ROUND(tt-sales-forecast.released-qty,0)

         chWorkSheet:Range(letter-array[v-count] + "25"):VALUE = tt-sales-forecast.company
         chWorkSheet:Range(letter-array[v-count] + "26"):VALUE = ROUND(tt-sales-forecast.invoiced-msf,0)
         chWorkSheet:Range(letter-array[v-count] + "27"):VALUE = ROUND(tt-sales-forecast.backlog-msf,0)
         chWorkSheet:Range(letter-array[v-count] + "28"):VALUE = ROUND(tt-sales-forecast.released-msf,0)
         chWorkSheet:Range(letter-array[v-count] + "29"):VALUE = ROUND(tt-sales-forecast.invoiced-msf,0)
                                                               + ROUND(tt-sales-forecast.backlog-msf,0)
                                                               + ROUND(tt-sales-forecast.released-msf,0)

         total-invoiced-msf = total-invoiced-msf + ROUND(tt-sales-forecast.invoiced-msf,0)
         total-backlog-msf = total-backlog-msf + ROUND(tt-sales-forecast.backlog-msf,0)
         total-released-msf = total-released-msf + ROUND(tt-sales-forecast.released-msf,0)

         chWorkSheet:Range(letter-array[v-count] + "35"):VALUE = tt-sales-forecast.company
         chWorkSheet:Range(letter-array[v-count] + "36"):VALUE = IF ROUND(tt-sales-forecast.invoiced-amt,2) NE 0 THEN
                                                                    ROUND(ROUND(tt-sales-forecast.invoiced-amt,2) /
                                                                    ROUND(tt-sales-forecast.invoiced-msf,2),2)
                                                                 ELSE 0
         chWorkSheet:Range(letter-array[v-count] + "37"):VALUE = IF ROUND(tt-sales-forecast.backlog-msf,2) NE 0 THEN
                                                                    ROUND(ROUND(tt-sales-forecast.backlog-amt,2) /
                                                                    ROUND(tt-sales-forecast.backlog-msf,2),2)
                                                                 ELSE 0
         chWorkSheet:Range(letter-array[v-count] + "38"):VALUE = IF ROUND(tt-sales-forecast.released-msf,2) NE 0 THEN
                                                                    ROUND(ROUND(tt-sales-forecast.released-qty,0) /
                                                                    ROUND(tt-sales-forecast.released-msf,2),2)
                                                                 ELSE 0
         chWorkSheet:Range(letter-array[v-count] + "39"):VALUE = DECIMAL(chWorkSheet:Range(letter-array[v-count] + "36"):VALUE)
                                                               + DECIMAL(chWorkSheet:Range(letter-array[v-count] + "37"):VALUE)
                                                               + DECIMAL(chWorkSheet:Range(letter-array[v-count] + "38"):VALUE)
                                                                     
         chWorkSheet:Range(letter-array[v-count] + "45"):VALUE = tt-sales-forecast.company
         chWorkSheet:Range(letter-array[v-count] + "46"):VALUE = ROUND(tt-sales-forecast.invoiced-profit,2)
         chWorkSheet:Range(letter-array[v-count] + "47"):VALUE = ROUND(tt-sales-forecast.backlog-profit,2)
         chWorkSheet:Range(letter-array[v-count] + "48"):VALUE = ROUND(tt-sales-forecast.released-profit,2)
         chWorkSheet:Range(letter-array[v-count] + "49"):VALUE = ROUND(tt-sales-forecast.invoiced-profit,2)
                                                               + ROUND(tt-sales-forecast.backlog-profit,2)
                                                               + ROUND(tt-sales-forecast.released-profit,2)
                                                                     
         total-invoiced-profit = total-invoiced-profit + ROUND(tt-sales-forecast.invoiced-profit,2)
         total-backlog-profit = total-backlog-profit + ROUND(tt-sales-forecast.backlog-profit,2)
         total-released-profit = total-released-profit + ROUND(tt-sales-forecast.released-profit,2).

  END.

  ASSIGN
    chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
    chWorkSheet:Range("D20"):VALUE = ip-as-of-date
    chWorkSheet:Range("E20"):VALUE = total-invoiced-amt
    chWorkSheet:Range("F20"):VALUE = total-invoiced-qty
    chWorkSheet:Range("G20"):VALUE = total-invoiced-msf
    chWorkSheet:Range("H20"):VALUE = IF total-invoiced-msf NE 0 THEN
                                        total-invoiced-amt / total-invoiced-msf
                                     ELSE 0
    chWorkSheet:Range("I20"):VALUE = total-invoiced-profit

    chWorkSheet:Range("D21"):VALUE = ip-as-of-date
    chWorkSheet:Range("E21"):VALUE = total-released-amt
    chWorkSheet:Range("F21"):VALUE = total-released-qty
    chWorkSheet:Range("G21"):VALUE = total-released-msf
    chWorkSheet:Range("H21"):VALUE = IF total-released-msf NE 0 THEN
                                        total-released-amt / total-released-msf
                                     ELSE 0
    chWorkSheet:Range("I21"):VALUE = total-released-profit

    chWorkSheet:Range("D22"):VALUE = ip-as-of-date
    chWorkSheet:Range("E22"):VALUE = total-backlog-amt
    chWorkSheet:Range("F22"):VALUE = total-backlog-qty
    chWorkSheet:Range("G22"):VALUE = total-backlog-msf
    chWorkSheet:Range("H22"):VALUE = IF total-backlog-msf NE 0 THEN
                                        total-backlog-amt / total-backlog-msf
                                     ELSE 0
    chWorkSheet:Range("I22"):VALUE = total-backlog-profit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-sales-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-sales-proc Procedure 
PROCEDURE raw-sales-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT INIT 5 NO-UNDO.
   DEF VAR this-year AS INT NO-UNDO.
   DEF VAR last-year AS INT NO-UNDO.
   DEF VAR month-var AS INT NO-UNDO.
   DEF VAR tot-amt AS DEC EXTENT 12 NO-UNDO.
   DEF VAR tot-amt-ly AS DEC EXTENT 12 NO-UNDO.
   DEF VAR tot-msf AS DEC EXTENT 12 NO-UNDO.
   DEF VAR tot-msf-ly AS DEC EXTENT 12 NO-UNDO.
   DEF VAR tot-tons AS DEC EXTENT 12 NO-UNDO.
   DEF VAR tot-tons-ly AS DEC EXTENT 12 NO-UNDO.
   DEF VAR gtot-amt AS DEC NO-UNDO.
   DEF VAR gtot-amt-ly AS DEC NO-UNDO.
   DEF VAR gtot-msf AS DEC NO-UNDO.
   DEF VAR gtot-msf-ly AS DEC NO-UNDO.
   DEF VAR gtot-tons AS DEC NO-UNDO.
   DEF VAR gtot-tons-ly AS DEC NO-UNDO.
   DEF VAR date-amt-total AS DEC NO-UNDO.
   DEF VAR date-qty-total AS DEC NO-UNDO.
   DEF VAR date-msf-total AS DEC NO-UNDO.
   DEF VAR date-np-total AS DEC NO-UNDO.
   DEF VAR mtd-amt-total AS DEC NO-UNDO.
   DEF VAR mtd-qty-total AS DEC NO-UNDO.
   DEF VAR mtd-msf-total AS DEC NO-UNDO.
   DEF VAR mtd-np-total AS DEC NO-UNDO.
   DEF VAR ytd-amt-total AS DEC NO-UNDO.
   DEF VAR ytd-qty-total AS DEC NO-UNDO.
   DEF VAR ytd-msf-total AS DEC NO-UNDO.
   DEF VAR ytd-np-total AS DEC NO-UNDO.
   DEF VAR indexi AS INT NO-UNDO.

   chWorkbook:WorkSheets({&raw-sales-sheet}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&raw-sales-sheet})
     this-year = YEAR(ip-as-of-date)
     last-year = this-year - 1.

   FOR EACH tt-raw-sales WHERE
       YEAR(tt-raw-sales.DATE) EQ last-year
       NO-LOCK:

       ASSIGN
          chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-raw-sales.DATE
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-amt,2)
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-qty,1)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-msf,1)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-tons,0)
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-net-profit,2)
          row-count = row-count + 1
          month-var = MONTH(tt-raw-sales.DATE)
          tot-amt-ly[month-var] = tot-amt-ly[month-var] + ROUND(tt-raw-sales.date-amt,2)
          tot-msf-ly[month-var] = tot-msf-ly[month-var] + ROUND(tt-raw-sales.date-msf,1)
          tot-tons-ly[month-var] = tot-tons-ly[month-var] + ROUND(tt-raw-sales.date-tons,0).
   END.

   FOR EACH tt-raw-sales WHERE
       YEAR(tt-raw-sales.DATE) EQ this-year
       NO-LOCK:

       ASSIGN
          chWorkSheet:Range("A" + STRING(row-count)):VALUE = tt-raw-sales.DATE
          chWorkSheet:Range("B" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-amt,2)
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-qty,1)
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-msf,1)
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = ROUND(tt-raw-sales.date-tons,0)
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = tt-raw-sales.date-net-profit
          row-count = row-count + 1
          month-var = MONTH(tt-raw-sales.DATE)
          tot-amt[month-var] = tot-amt[month-var] + ROUND(tt-raw-sales.date-amt,2)
          tot-msf[month-var] = tot-msf[month-var] + ROUND(tt-raw-sales.date-msf,1)
          tot-tons[month-var] = tot-tons[month-var] + ROUND(tt-raw-sales.date-tons,0).

       IF tt-raw-sales.DATE EQ ip-as-of-date THEN
          ASSIGN
            date-amt-total = date-amt-total + ROUND(tt-raw-sales.date-amt,2)
            date-qty-total = date-qty-total + ROUND(tt-raw-sales.date-qty,1)
            date-msf-total = date-msf-total + ROUND(tt-raw-sales.date-msf,1)
            date-np-total  = date-np-total  + ROUND(tt-raw-sales.date-amt - tt-raw-sales.date-cost,2).

       IF tt-raw-sales.DATE LE ip-as-of-date THEN
       DO:
          IF MONTH(tt-raw-sales.DATE) EQ v-this-month THEN
             ASSIGN
               mtd-amt-total = mtd-amt-total + ROUND(tt-raw-sales.date-amt,2)
               mtd-qty-total = mtd-qty-total + ROUND(tt-raw-sales.date-qty,1)
               mtd-msf-total = mtd-msf-total + ROUND(tt-raw-sales.date-msf,1)
               mtd-np-total  = mtd-np-total  + ROUND(tt-raw-sales.date-amt - tt-raw-sales.date-cost,2).

          ASSIGN
               ytd-amt-total = ytd-amt-total + ROUND(tt-raw-sales.date-amt,2)
               ytd-qty-total = ytd-qty-total + ROUND(tt-raw-sales.date-qty,1)
               ytd-msf-total = ytd-msf-total + ROUND(tt-raw-sales.date-msf,1)
               ytd-np-total  = ytd-np-total  + ROUND(tt-raw-sales.date-amt - tt-raw-sales.date-cost,2).
       END.

   END.

   chWorkbook:WorkSheets({&sales-summary-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&sales-summary-sheet}).

   DO row-count = 5 TO 16:
      ASSIGN
         indexi = row-count - 4
         chWorkSheet:Range("C" + STRING(row-count)):VALUE = tot-amt[indexi]
         chWorkSheet:Range("D" + STRING(row-count)):VALUE = tot-msf[indexi]
         chWorkSheet:Range("E" + STRING(row-count)):VALUE = IF tot-msf[indexi] NE 0 THEN
                                                               tot-amt[indexi] /
                                                               tot-msf[indexi]
                                                            ELSE 0
         chWorkSheet:Range("F" + STRING(row-count)):VALUE = tot-tons[indexi]
                                                                
         chWorkSheet:Range("K" + STRING(row-count)):VALUE = tot-amt-ly[indexi]
         chWorkSheet:Range("L" + STRING(row-count)):VALUE = tot-msf-ly[indexi]
         chWorkSheet:Range("M" + STRING(row-count)):VALUE = IF tot-msf-ly[indexi] NE 0 THEN
                                                               tot-amt-ly[indexi] /
                                                               tot-msf-ly[indexi]
                                                            ELSE 0
         chWorkSheet:Range("N" + STRING(row-count)):VALUE = tot-tons-ly[indexi]
                                                                
         gtot-amt = gtot-amt + tot-amt[indexi]
         gtot-amt-ly = gtot-amt-ly + tot-amt-ly[indexi]
         gtot-msf = gtot-msf + tot-msf[indexi]
         gtot-msf-ly = gtot-msf-ly + tot-msf-ly[indexi]
         gtot-tons = gtot-tons + tot-tons[indexi]
         gtot-tons-ly = gtot-tons-ly + tot-tons-ly[indexi].
   END.

   ASSIGN
     chWorkSheet:Range("C18"):VALUE = gtot-amt
     chWorkSheet:Range("D18"):VALUE = gtot-msf
     chWorkSheet:Range("E18"):VALUE = IF gtot-msf NE 0 THEN gtot-amt / gtot-msf ELSE 0
     chWorkSheet:Range("F18"):VALUE = gtot-tons
     chWorkSheet:Range("K18"):VALUE = gtot-amt-ly
     chWorkSheet:Range("L18"):VALUE = gtot-msf-ly
     chWorkSheet:Range("M18"):VALUE = IF gtot-msf-ly NE 0 THEN gtot-amt-ly / gtot-msf-ly ELSE 0
     chWorkSheet:Range("N18"):VALUE = gtot-tons-ly.

   chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     chWorkSheet:Range("D12"):VALUE = ip-as-of-date
     chWorkSheet:Range("D13"):VALUE = ip-as-of-date
     chWorkSheet:Range("D14"):VALUE = ip-as-of-date
     chWorkSheet:Range("D15"):VALUE = ip-as-of-date
     chWorkSheet:Range("E12"):VALUE = date-amt-total
     chWorkSheet:Range("F12"):VALUE = date-qty-total
     chWorkSheet:Range("G12"):VALUE = date-msf-total
     chWorkSheet:Range("H12"):VALUE = IF date-msf-total NE 0 THEN
                                         date-amt-total / date-msf-total
                                      ELSE 0
     chWorkSheet:Range("I12"):VALUE = date-np-total
     chWorkSheet:Range("E13"):VALUE = mtd-amt-total
     chWorkSheet:Range("F13"):VALUE = mtd-qty-total
     chWorkSheet:Range("G13"):VALUE = mtd-msf-total
     chWorkSheet:Range("H13"):VALUE = IF mtd-msf-total NE 0 THEN
                                         mtd-amt-total / mtd-msf-total
                                      ELSE 0
     chWorkSheet:Range("I13"):VALUE = mtd-np-total
                                          
     chWorkSheet:Range("E14"):VALUE = mtd-amt-total / v-days-this-month
     chWorkSheet:Range("F14"):VALUE = mtd-qty-total / v-days-this-month
     chWorkSheet:Range("G14"):VALUE = mtd-msf-total / v-days-this-month
     chWorkSheet:Range("H14"):VALUE = IF mtd-msf-total NE 0 THEN
                                         (mtd-amt-total / mtd-msf-total) / v-days-this-month
                                      ELSE 0
     chWorkSheet:Range("I14"):VALUE = mtd-np-total / v-days-this-month

                                          
     chWorkSheet:Range("E15"):VALUE = ytd-amt-total
     chWorkSheet:Range("F15"):VALUE = ytd-qty-total
     chWorkSheet:Range("G15"):VALUE = ytd-msf-total
     chWorkSheet:Range("H15"):VALUE = IF ytd-msf-total NE 0 THEN
                                         ytd-amt-total / ytd-msf-total
                                      ELSE 0
     chWorkSheet:Range("I15"):VALUE = ytd-np-total.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-raw-sales-summary-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-sales-summary-proc Procedure 
PROCEDURE raw-sales-summary-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-month-var AS INT NO-UNDO.
   DEF VAR mtd-budget-amt AS DEC EXTENT 12 NO-UNDO.
   DEF VAR mtd-msf AS DEC EXTENT 12 NO-UNDO.
   DEF VAR mtd-tons AS DEC EXTENT 12 NO-UNDO.

   DEF VAR mtd-budget-amt-tot AS DEC NO-UNDO.
   DEF VAR mtd-msf-tot AS DEC NO-UNDO.
   DEF VAR mtd-tons-tot AS DEC NO-UNDO.

   DEF VAR i AS INT NO-UNDO.
   
   chWorkbook:WorkSheets({&sales-summary-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&sales-summary-sheet}).

   FOR EACH smanbcst WHERE
       smanbcst.company EQ ip-company AND
       smanbcst.sman GT "" AND
       smanbcst.budget-yr EQ YEAR(ip-as-of-date)
       NO-LOCK,
       FIRST sman WHERE
             sman.company EQ ip-company AND
             sman.sman EQ smanbcst.sman
             NO-LOCK:
       
       ASSIGN
          lv-month-var = IF smanbcst.budget-period EQ 13 THEN 12 ELSE
                         smanbcst.budget-period
          mtd-budget-amt[lv-month-var] = mtd-budget-amt[lv-month-var]
                                       + smanbcst.budget-amt
          mtd-msf[lv-month-var]        = mtd-msf[lv-month-var]
                                       + smanbcst.msf
          mtd-tons[lv-month-var]       = mtd-tons[lv-month-var]
                                       + smanbcst.tons.

   END.

   DO i = 1 TO 12:
      ASSIGN
         chWorkSheet:Range("G" + STRING(4 + i)):VALUE = mtd-budget-amt[i]
         chWorkSheet:Range("H" + STRING(4 + i)):VALUE = mtd-msf[i]
         chWorkSheet:Range("I" + STRING(4 + i)):VALUE = IF mtd-msf[i] NE 0 THEN
                                                           ROUND(mtd-budget-amt[i] / mtd-msf[i],2)
                                                        ELSE 0
         chWorkSheet:Range("J" + STRING(4 + i)):VALUE = mtd-tons[i]
         mtd-budget-amt-tot = mtd-budget-amt-tot + mtd-budget-amt[i]
         mtd-msf-tot = mtd-msf-tot + mtd-msf[i]
         mtd-tons-tot = mtd-tons-tot + mtd-tons[i].
   END.

   ASSIGN
      chWorkSheet:Range("G18"):VALUE = mtd-budget-amt-tot
      chWorkSheet:Range("H18"):VALUE = mtd-msf-tot
      chWorkSheet:Range("I18"):VALUE = IF mtd-msf-tot NE 0 THEN
                                          ROUND(mtd-budget-amt-tot / mtd-msf-tot,2)
                                       ELSE 0
      chWorkSheet:Range("J18"):VALUE = mtd-tons-tot.

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
   
   chWorkbook:WorkSheets({&raw-salesmen}):Activate no-error.

   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&raw-salesmen}).
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

   chWorkbook:WorkSheets({&salesrep}):Activate no-error.

   ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item({&salesrep}).
      row-count = 6.

   FOR EACH tt-raw-salesmen:

       IF row-count LT 31 THEN

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

   ASSIGN
      chWorkSheet:Range("C32"):VALUE = ROUND(mtd-amt-total,2)
      chWorkSheet:Range("D32"):VALUE = ROUND(mtd-msf-total,2)
      chWorkSheet:Range("E32"):VALUE = IF mtd-msf-total NE 0 THEN
                                          ROUND(ROUND(mtd-amt-total,2) /
                                                ROUND(mtd-msf-total,2),2) ELSE 0
      
      chWorkSheet:Range("C63"):VALUE = ROUND(ytd-amt-total,2)
      chWorkSheet:Range("D63"):VALUE = ROUND(ytd-msf-total,2)
      chWorkSheet:Range("E63"):VALUE = IF ytd-msf-total NE 0 THEN
                                          ROUND(ROUND(ytd-amt-total,2) /
                                                ROUND(ytd-msf-total,2),2) ELSE 0

      chWorkSheet:Range("AM95"):VALUE = ROUND(ytd-amt-total,2)
      chWorkSheet:Range("AN95"):VALUE = ROUND(ytd-msf-total,2)
      chWorkSheet:Range("AO95"):VALUE = IF ytd-msf-total NE 0 THEN
                                           ROUND(ROUND(ytd-amt-total,2) /
                                                 ROUND(ytd-msf-total,2),2) ELSE 0
      row-count = 37.

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

   row-count = 69.

   FOR EACH tt-raw-salesmen:

       IF row-count LT 94 THEN
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

&IF DEFINED(EXCLUDE-round-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE round-proc Procedure 
PROCEDURE round-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-column AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-start-cell AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-end-cell AS INT NO-UNDO.

   DEF VAR v-count AS INT NO-UNDO.

   DO v-count = ip-start-cell TO ip-end-cell:
      chWorkSheet:Range(ip-column + STRING(v-count)):NumberFormat = "###,###,###,###".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rounding-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rounding-proc Procedure 
PROCEDURE rounding-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /*Page 1*/
   chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet}).
   
   RUN round-proc(INPUT "E",INPUT 5, INPUT 8).
   RUN round-proc(INPUT "E",INPUT 12, INPUT 15).
   RUN round-proc(INPUT "E",INPUT 20, INPUT 24).
   RUN round-proc(INPUT "H",INPUT 5, INPUT 8).
   RUN round-proc(INPUT "H",INPUT 20, INPUT 24).
   RUN round-proc(INPUT "I",INPUT 20, INPUT 24).
   RUN round-proc(INPUT "N",INPUT 12, INPUT 14).    
   RUN round-proc(INPUT "O",INPUT 12, INPUT 14).
   
   
   
   /*Page 2*/
   chWorkbook:WorkSheets({&bookings-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&bookings-sheet}).
   
   RUN round-proc(INPUT "C",INPUT 6, INPUT 17).
   RUN round-proc(INPUT "C",INPUT 19, INPUT 20).
   RUN round-proc(INPUT "C",INPUT 25, INPUT 36).
   RUN round-proc(INPUT "C",INPUT 38, INPUT 39).
   RUN round-proc(INPUT "E",INPUT 6, INPUT 17).
   RUN round-proc(INPUT "E",INPUT 19, INPUT 20).
   RUN round-proc(INPUT "E",INPUT 25, INPUT 36).
   RUN round-proc(INPUT "E",INPUT 38, INPUT 39).
   RUN round-proc(INPUT "G",INPUT 6, INPUT 17).
   RUN round-proc(INPUT "G",INPUT 19, INPUT 20).
   RUN round-proc(INPUT "G",INPUT 25, INPUT 36).
   RUN round-proc(INPUT "G",INPUT 38, INPUT 39).

   /*Page 4*/
   chWorkbook:WorkSheets({&sales-summary-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&sales-summary-sheet}).

   RUN round-proc(INPUT "C",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "E",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "G",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "I",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "K",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "M",INPUT 5, INPUT 16).
   RUN round-proc(INPUT "C",INPUT 18, INPUT 18).
   RUN round-proc(INPUT "E",INPUT 18, INPUT 18).
   RUN round-proc(INPUT "G",INPUT 18, INPUT 18).
   RUN round-proc(INPUT "I",INPUT 18, INPUT 18).
   RUN round-proc(INPUT "K",INPUT 18, INPUT 18).
   RUN round-proc(INPUT "M",INPUT 18, INPUT 18).

   

   /*Page 5*/
   chWorkbook:WorkSheets({&prod-category}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&prod-category}).

   RUN round-proc(INPUT "C",INPUT 6, INPUT 20).
   RUN round-proc(INPUT "C",INPUT 22, INPUT 22).
   RUN round-proc(INPUT "E",INPUT 6, INPUT 20).
   RUN round-proc(INPUT "E",INPUT 22, INPUT 22).
   RUN round-proc(INPUT "F",INPUT 6, INPUT 20).
   RUN round-proc(INPUT "F",INPUT 22, INPUT 22).
   RUN round-proc(INPUT "C",INPUT 27, INPUT 41).
   RUN round-proc(INPUT "C",INPUT 43, INPUT 43).
   RUN round-proc(INPUT "E",INPUT 27, INPUT 41).
   RUN round-proc(INPUT "E",INPUT 43, INPUT 43).
   RUN round-proc(INPUT "F",INPUT 27, INPUT 41).
   RUN round-proc(INPUT "F",INPUT 43, INPUT 43).
   RUN round-proc(INPUT "C",INPUT 48, INPUT 62).
   RUN round-proc(INPUT "C",INPUT 64, INPUT 64).
   RUN round-proc(INPUT "E",INPUT 48, INPUT 62).
   RUN round-proc(INPUT "E",INPUT 64, INPUT 64).
   RUN round-proc(INPUT "F",INPUT 48, INPUT 62).
   RUN round-proc(INPUT "F",INPUT 64, INPUT 64).
    
   /*Page 6*/
   chWorkbook:WorkSheets({&salesrep}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&salesrep}).

   RUN round-proc(INPUT "C",INPUT 6, INPUT 30).
   RUN round-proc(INPUT "C",INPUT 32, INPUT 32).
   RUN round-proc(INPUT "C",INPUT 37, INPUT 61).
   RUN round-proc(INPUT "C",INPUT 63, INPUT 63).
   RUN round-proc(INPUT "C",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "C",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "E",INPUT 6, INPUT 30).
   RUN round-proc(INPUT "E",INPUT 32, INPUT 32).
   RUN round-proc(INPUT "E",INPUT 37, INPUT 61).
   RUN round-proc(INPUT "E",INPUT 63, INPUT 63).
   RUN round-proc(INPUT "E",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "E",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "F",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "F",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "H",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "H",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "I",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "I",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "K",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "K",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "L",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "L",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "N",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "N",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "O",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "O",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "Q",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "Q",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "R",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "R",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "T",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "T",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "U",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "U",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "W",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "W",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "X",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "X",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "Z",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "Z",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AA",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AA",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AC",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AC",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AD",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AD",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AF",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AF",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AG",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AG",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AI",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AI",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AJ",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AJ",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AL",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AL",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AM",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AM",INPUT 95, INPUT 95).
   RUN round-proc(INPUT "AO",INPUT 69, INPUT 93).
   RUN round-proc(INPUT "AO",INPUT 95, INPUT 95).

   /*Page 7*/
   chWorkbook:WorkSheets({&sales-forecast}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&sales-forecast}).

   RUN round-proc(INPUT "G",INPUT 6, INPUT 11).
   RUN round-proc(INPUT "G",INPUT 36, INPUT 41).
   RUN round-proc(INPUT "G",INPUT 46, INPUT 51).
   RUN round-proc(INPUT "H",INPUT 6, INPUT 11).
   RUN round-proc(INPUT "H",INPUT 36, INPUT 41).
   RUN round-proc(INPUT "H",INPUT 46, INPUT 51).
   RUN round-proc(INPUT "I",INPUT 6, INPUT 11).
   RUN round-proc(INPUT "I",INPUT 36, INPUT 41).
   RUN round-proc(INPUT "I",INPUT 46, INPUT 51).
   RUN round-proc(INPUT "J",INPUT 6, INPUT 11).
   RUN round-proc(INPUT "J",INPUT 36, INPUT 41).
   RUN round-proc(INPUT "J",INPUT 46, INPUT 51).
   RUN round-proc(INPUT "K",INPUT 6, INPUT 11).
   RUN round-proc(INPUT "K",INPUT 36, INPUT 41).
   RUN round-proc(INPUT "K",INPUT 46, INPUT 51).

   /*Page 8*/
   chWorkbook:WorkSheets({&raw-sales-pc}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&raw-sales-pc}).

   RUN round-proc(INPUT "E",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "F",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "G",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "J",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "K",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "L",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "O",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "P",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "Q",INPUT 6, INPUT 500).

   /*Page 10*/
   chWorkbook:WorkSheets({&raw-sales-sheet}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&raw-sales-sheet}).

   RUN round-proc(INPUT "B",INPUT 5, INPUT 500).
   RUN round-proc(INPUT "F",INPUT 5, INPUT 500).

   /*Page 11*/
   chWorkbook:WorkSheets({&raw-salesmen}):Activate no-error.
   chWorkSheet = chExcelApplication:Sheets:item({&raw-salesmen}).

   RUN round-proc(INPUT "F",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "G",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "H",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "K",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "L",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "M",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "P",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "Q",INPUT 6, INPUT 500).
   RUN round-proc(INPUT "R",INPUT 6, INPUT 500).
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

