&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* {custom/globdefs.i} */

/*    {methods/defines/hndldefs.i}       */
/* /*    {methods/prgsecur.i}         */ */
/*                                       */
/*                                       */
/* {custom/gcompany.i}                   */
/* {custom/gloc.i}                       */
/* {custom/getcmpny.i}                   */
/* {custom/getloc.i}                     */
{custom/gcompany.i}
{custom/gloc.i}
{custom/persist.i}
{sys/inc/var.i new shared}
assign
 cocode = g_company
 locode = g_loc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RS_industry btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS RS_industry 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE RS_industry AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Folding Carton Materials", 1,
"Corragated / Foam Materials", 2
     SIZE 59 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RS_industry AT ROW 5.52 COL 10 NO-LABEL WIDGET-ID 6
     btn-process AT ROW 7.43 COL 18
     btn-cancel AT ROW 7.43 COL 43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.4 BY 27.91.

DEFINE FRAME FRAME-B
     "This utility will import Inks from an Excel file." VIEW-AS TEXT
          SIZE 51 BY .95 AT ROW 2.19 COL 13.8
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77 BY 3.81
         BGCOLOR 11 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import Ink File"
         HEIGHT             = 8.33
         WIDTH              = 77.6
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".



DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (RS_industry:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import Ink File */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import Ink File */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
   ASSIGN {&DISPLAYED-OBJECTS}.

   RUN import-excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 RUN Get-Company  (OUTPUT gcompany).
RUN Get-location (OUTPUT gloc).
assign cocode = gcompany
       locode = gloc.  


  RUN enable_UI.



  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY RS_industry 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RS_industry btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel C-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR chFile       AS CHAR        NO-UNDO.
   DEF VAR v-ok         AS LOG         NO-UNDO.
   DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
   DEF VAR chWorkBook   AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorksheet  AS COM-HANDLE  NO-UNDO.
   DEF VAR v-RowCount   AS INT INIT 2  NO-UNDO.
   DEF VAR v-rm-stocked AS CHAR        NO-UNDO.
   DEF VAR v-estimate-mat AS CHAR      NO-UNDO.
   DEF VAR v-taxable    AS CHAR        NO-UNDO.
   DEF VAR v-cnt        AS INT         NO-UNDO.
   DEF VAR v-i-no       AS CHAR        NO-UNDO.
   DEF VAR v-ink-type   AS CHAR        NO-UNDO.
   DEF VAR v-press-type AS CHAR        NO-UNDO.

   DEF VAR v-i-name     AS CHAR        NO-UNDO.
   DEF VAR v-i-dscr     AS CHAR        NO-UNDO.
   DEF VAR v-est-dscr   AS CHAR        NO-UNDO.
   DEF VAR v-mat-type   AS CHAR        NO-UNDO.
   DEF VAR v-cost-type  AS CHAR        NO-UNDO.
   DEF VAR v-procat     AS CHAR        NO-UNDO.
   DEF VAR v-yield      AS CHAR        NO-UNDO.
   DEF VAR v-min-lbs    AS CHAR        NO-UNDO.
   DEF VAR v-q-ptd      AS CHAR        NO-UNDO.
   DEF VAR v-q-ytd      AS CHAR        NO-UNDO.
   DEF VAR v-q-lyr      AS CHAR        NO-UNDO.
   DEF VAR v-weight-100 AS CHAR        NO-UNDO. 
   DEF VAR v-run-cost   AS CHAR        NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      SYSTEM-DIALOG GET-FILE chFile 
                    TITLE "Select File to Import"
                    FILTERS "Excel File (*.xls,*.xlsx) " "*.xls,*.xlsx"
                    INITIAL-DIR "c:\"
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE v-ok.

      IF v-ok THEN DO:
         IF LENGTH(chFile) LT 4 OR
            (SUBSTR(chFile,LENGTH(chFile) - 3) NE ".xls" AND 
            SUBSTR(chFile,LENGTH(chFile) - 4) NE ".xlsx") THEN DO:
            MESSAGE "Invalid File.  Must Choose Excel (.xls) File."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
         END.

         SESSION:SET-WAIT-STATE ("general").

         /* Initialize Excel. */
         CREATE "Excel.Application" chExcelApplication NO-ERROR.

         /* Check if Excel got initialized. */
         IF NOT (VALID-HANDLE (chExcelApplication)) THEN DO:
            MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR. 
         END.

         /* Open our Excel File. */  
         chExcelApplication:VISIBLE = FALSE.
         chWorkbook = chExcelApplication:Workbooks:OPEN(chfile) NO-ERROR.

         /* Do not display Excel error messages. */
         chExcelApplication:DisplayAlerts = FALSE NO-ERROR.

         /* Go to the Active Sheet. */
         chWorkbook:WorkSheets(1):Activate NO-ERROR.

         ASSIGN
            chWorkSheet = chExcelApplication:Sheets:ITEM(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE = ? THEN LEAVE.

            ASSIGN
               v-rm-stocked   = ""
               v-estimate-mat = ""
               v-ink-type     = ""
               v-press-type   = ""
               v-i-name       = ""         
               v-i-dscr       = ""        
               v-est-dscr     = ""  
               v-mat-type     = ""  
               v-cost-type    = ""  
               v-procat       = "" 
               v-ink-type     = "" 
               v-press-type   = "" 
               v-yield        = "0" 
               v-min-lbs      = "0" 
               v-q-ptd        = "0" 
               v-q-ytd        = "0" 
               v-q-lyr        = "0" 
               v-weight-100   = "0". 

            v-i-no = chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE NO-ERROR.

            FIND FIRST ITEM WHERE ITEM.company = cocode 
                              AND ITEM.i-no = TRIM(v-i-no) NO-ERROR.

            IF NOT AVAILABLE(ITEM) THEN DO:
               CREATE item.
               ASSIGN
                  item.company      = cocode
                  item.i-no         = v-i-no.
            END.

            ASSIGN
               item.loc          = "MAIN"
               item.loc-bin      = "FLOOR"
               item.cons-uom     = "LB"
               item.pur-uom      = "EA"
               ITEM.industry     = STRING(RS_industry).
/*                item.i-no         = chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE NO-ERROR. */
               v-i-name          = chWorkSheet:Range("B" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-i-dscr          = chWorkSheet:Range("C" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-est-dscr        = chWorkSheet:Range("D" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-rm-stocked      = chWorkSheet:Range("E" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-estimate-mat    = chWorkSheet:Range("F" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-taxable         = chWorkSheet:Range("G" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-mat-type        = chWorkSheet:Range("H" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-cost-type       = chWorkSheet:Range("I" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-procat          = chWorkSheet:Range("J" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-ink-type        = chWorkSheet:Range("K" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-press-type      = chWorkSheet:Range("L" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-yield           = chWorkSheet:Range("M" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-min-lbs         = chWorkSheet:Range("N" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-q-ptd           = chWorkSheet:Range("R" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-q-ytd           = chWorkSheet:Range("S" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-q-lyr           = chWorkSheet:Range("T" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-weight-100      = chWorkSheet:Range("U" + STRING(v-RowCount)):VALUE NO-ERROR.
               v-run-cost        = chWorkSheet:Range("V" + STRING(v-RowCount)):VALUE NO-ERROR.

               IF TRIM(v-i-name) = ? THEN DO:
                  MESSAGE "= ?"
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.

               END.

               IF TRIM(v-i-name)       = ? THEN ITEM.i-name = "".    ELSE ITEM.i-name = v-i-name.         
               IF TRIM(v-i-dscr)       = ? THEN ITEM.i-dscr = "".    ELSE ITEM.i-dscr = v-i-dscr. 
               IF TRIM(v-est-dscr)     = ? THEN ITEM.est-dscr = "".  ELSE ITEM.est-dscr = v-est-dscr. 
               IF TRIM(v-mat-type)     = ? THEN ITEM.mat-type = "".  ELSE ITEM.mat-type = v-mat-type. 
               IF TRIM(v-cost-type)    = ? THEN ITEM.cost-type = "". ELSE ITEM.cost-type = v-cost-type. 
               IF TRIM(v-procat)       = ? THEN ITEM.procat = "".    ELSE ITEM.procat = v-procat. 
               IF TRIM(v-ink-type)     = ? THEN ITEM.ink-type = "".  ELSE ITEM.ink-type = v-ink-type. 
               IF TRIM(v-press-type)   = ? THEN ITEM.press-type = "". ELSE ITEM.press-type = v-press-type. 
               IF TRIM(v-yield)        = ? THEN ITEM.yield = 0.      ELSE ITEM.yield = DECI(v-yield). 
               IF TRIM(v-min-lbs)      = ? THEN ITEM.min-lbs = 0.    ELSE ITEM.min-lbs = DECI(v-min-lbs). 
               IF TRIM(v-q-ptd)        = ? THEN ITEM.q-ptd = 0.      ELSE ITEM.q-ptd = DECI(v-q-ptd). 
               IF TRIM(v-q-ytd)        = ? THEN ITEM.q-ytd = 0.      ELSE ITEM.q-ytd = DECI(v-q-ytd). 
               IF TRIM(v-q-lyr)        = ? THEN ITEM.q-lyr = 0.      ELSE ITEM.q-lyr = DECI(v-q-lyr). 
               IF TRIM(v-weight-100)   = ? THEN ITEM.weight-100 = 0. ELSE ITEM.weight-100 = DECI(v-weight-100). 

            item.i-code = "R".
            IF TRIM(v-rm-stocked) BEGINS "Y" THEN 
               item.i-code = "R".
            IF TRIM(v-estimate-mat) BEGINS "Y" THEN
               item.i-code = "E".
            IF TRIM(v-taxable) BEGINS "Y" THEN
               item.tax-rcpt = YES.
            ELSE
               item.tax-rcpt = NO.

            item.ink-type = "I".
            IF v-ink-type <> "" THEN DO:
               IF v-ink-type BEGINS "I" THEN 
                  item.ink-type = "I".
               IF v-ink-type BEGINS "L" THEN 
                  item.ink-type = "L".
               IF v-ink-type BEGINS "U" THEN 
                  item.ink-type = "U".
               IF v-ink-type BEGINS "V" THEN 
                  item.ink-type = "V".
               IF v-ink-type BEGINS "A" THEN 
                  item.ink-type = "A".
            END.

            item.press-type = "F".
            IF v-press-type <> "" THEN DO:
               IF v-press-type BEGINS "F" THEN 
                  item.press-type = "F".
               IF v-press-type BEGINS "G" THEN 
                  item.press-type = "G".
               IF v-press-type BEGINS "L" THEN 
                  item.press-type = "L".
               IF v-press-type BEGINS "O" THEN 
                  item.press-type = "O".
               IF v-press-type BEGINS "S" THEN 
                  item.press-type = "S".
            END.

            FIND FIRST e-item WHERE e-item.company EQ item.company
                                AND e-item.i-no    EQ item.i-no NO-ERROR.
            IF NOT AVAIL e-item THEN DO:
               CREATE e-item.
               ASSIGN
                  e-item.company = item.company
                  e-item.i-no    = item.i-no.
            END.
            e-item.std-uom = item.pur-uom.

            FIND FIRST e-item-vend WHERE e-item-vend.company   EQ item.company
                                     AND e-item-vend.item-type EQ YES
                                     AND e-item-vend.i-no      EQ item.i-no
                                     AND e-item-vend.vend-no   EQ "" NO-ERROR.
            IF NOT AVAIL e-item-vend THEN DO:
               CREATE e-item-vend.
               ASSIGN
                  e-item-vend.company   = item.company
                  e-item-vend.item-type = YES
                  e-item-vend.i-no      = item.i-no
                  e-item-vend.vend-no   = "".
            END.
            ASSIGN
               e-item-vend.std-uom     = item.cons-uom
               e-item-vend.run-qty[1]  = 999999.99.

            IF TRIM(v-run-cost) = ? THEN e-item-vend.run-cost[1] = 0. ELSE  e-item-vend.run-cost[1]  =  DECI(v-run-cost).  

            ASSIGN
               ITEM.last-cost = e-item-vend.run-cost[1]
               ITEM.avg-cost  = e-item-vend.run-cost[1].

            ASSIGN
               v-RowCount = v-RowCount + 1
               v-cnt = v-cnt + 1.

           STATUS DEFAULT "Processing.... " + TRIM(STRING(v-cnt)).


         END.
      END.

      /*Free memory*/
      chWorkbook = chExcelApplication:Workbooks:CLOSE() NO-ERROR.
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chExcelApplication NO-ERROR.
   END.

   STATUS DEFAULT "".

   SESSION:SET-WAIT-STATE("").
   OUTPUT CLOSE.

   MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

   APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

