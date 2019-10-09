&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : salrep\dashboard-book.p
    
    Purpose     : Bookings Highlights
     
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
{salrep\dashbook.i}

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
&GLOBAL-DEFINE raw-op-sheet 3

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

  FILE-INFO:FILE-NAME = "template\DashBook.xlt".

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
  RUN raw-op-proc.

  chWorkbook:WorkSheets({&summary-sheet}):Columns("D:H"):AutoFit.
  chWorkbook:WorkSheets({&bookings-sheet}):Columns("C:H"):AutoFit.
  chWorkbook:WorkSheets({&raw-op-sheet}):Columns("A:K"):AutoFit.
  chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.
  chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet}).

  /* enable screen updating */
  ASSIGN chExcelApplication:ScreenUpdating = TRUE.
         chExcelApplication:VISIBLE = TRUE.
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
     chWorkSheet:Range("G1"):VALUE = ip-company
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
                                      ELSE 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

