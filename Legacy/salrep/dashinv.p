&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : salrep\dashinv.p
    
    Purpose     : Invoice Highlights
     
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
{salrep\dashinv.i}

/* Variables for excel Automation  */
DEF VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chFile               AS CHAR         NO-UNDO.
DEF VARIABLE CurrDir              AS CHARACTER    NO-UNDO.

DEF VAR v-this-month AS INT NO-UNDO.
DEF VAR v-days-this-month AS INT NO-UNDO.

&GLOBAL-DEFINE summary-sheet 1
&GLOBAL-DEFINE sales-summary-sheet 2
&GLOBAL-DEFINE raw-sales-sheet 3

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

  FILE-INFO:FILE-NAME = "template\DashInv.xlt".

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
  RUN raw-sales-proc.

  /* size the columns automatically */
  chWorkbook:WorkSheets({&summary-sheet}):Columns("D:I"):AutoFit.
  chWorkbook:WorkSheets({&sales-summary-sheet}):Columns("C:N"):AutoFit.
  chWorkbook:WorkSheets({&raw-sales-sheet}):Columns("A:F"):AutoFit.


  /* let the user in */
  ASSIGN chExcelApplication:VISIBLE = TRUE.
  chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

  ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     /* enable screen updating */
     chExcelApplication:ScreenUpdating = TRUE.
  
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
     chWorkSheet:Range("D5"):VALUE = ip-as-of-date
     chWorkSheet:Range("D6"):VALUE = ip-as-of-date
     chWorkSheet:Range("D7"):VALUE = ip-as-of-date
     chWorkSheet:Range("D8"):VALUE = ip-as-of-date
     chWorkSheet:Range("E5"):VALUE = date-amt-total
     chWorkSheet:Range("F5"):VALUE = date-qty-total
     chWorkSheet:Range("G5"):VALUE = date-msf-total
     chWorkSheet:Range("H5"):VALUE = IF date-msf-total NE 0 THEN
                                         date-amt-total / date-msf-total
                                      ELSE 0
     chWorkSheet:Range("I5"):VALUE = date-np-total
                                          
     chWorkSheet:Range("E6"):VALUE = mtd-amt-total
     chWorkSheet:Range("F6"):VALUE = mtd-qty-total
     chWorkSheet:Range("G6"):VALUE = mtd-msf-total
     chWorkSheet:Range("H6"):VALUE = IF mtd-msf-total NE 0 THEN
                                         mtd-amt-total / mtd-msf-total
                                      ELSE 0
     chWorkSheet:Range("I6"):VALUE = mtd-np-total
                                          
     chWorkSheet:Range("E7"):VALUE = mtd-amt-total / v-days-this-month
     chWorkSheet:Range("F7"):VALUE = mtd-qty-total / v-days-this-month
     chWorkSheet:Range("G7"):VALUE = mtd-msf-total / v-days-this-month
     chWorkSheet:Range("H7"):VALUE = IF mtd-msf-total NE 0 THEN
                                         (mtd-amt-total / mtd-msf-total) / v-days-this-month
                                      ELSE 0
     chWorkSheet:Range("I7"):VALUE = mtd-np-total / v-days-this-month

                                          
     chWorkSheet:Range("E8"):VALUE = ytd-amt-total
     chWorkSheet:Range("F8"):VALUE = ytd-qty-total
     chWorkSheet:Range("G8"):VALUE = ytd-msf-total
     chWorkSheet:Range("H8"):VALUE = IF ytd-msf-total NE 0 THEN
                                         ytd-amt-total / ytd-msf-total
                                      ELSE 0
     chWorkSheet:Range("I8"):VALUE = ytd-np-total.

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

