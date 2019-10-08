&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

------------------------------------------------------------------------*/
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
DEF INPUT PARAMETER ipv-loc LIKE carrier.loc NO-UNDO.
DEF INPUT PARAMETER ipv-carrier LIKE carrier.carrier NO-UNDO.
/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF STREAM outStream.
DEF STREAM sUpload.
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i} */

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF TEMP-TABLE tt-act FIELD act-num AS cha
                      FIELD bud AS DEC EXTENT 13.

DEFINE STREAM s-input.



DEF var qtr-file-name   AS char NO-UNDO.
DEF var save_name       AS char NO-UNDO.

DEF var cmdline         AS char NO-UNDO.
DEF var v-amount        AS dec NO-UNDO.
DEF var v-acct          AS char NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 IMAGE-1 in-file-name fiQty1 ~
fiQty2 fiQty3 fiQty4 fiQty5 fiQty6 fiQty7 fiQty8 btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS in-file-name fiQty1 fiQty2 fiQty3 fiQty4 ~
fiQty5 fiQty6 fiQty7 fiQty8 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiQty1 AS CHARACTER FORMAT "X(256)":U INITIAL "499" 
     LABEL "Quantity 1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty2 AS CHARACTER FORMAT "X(256)":U INITIAL "999" 
     LABEL "Quantity 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty3 AS CHARACTER FORMAT "X(256)":U INITIAL "1999" 
     LABEL "Quantity 3" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty4 AS CHARACTER FORMAT "X(256)":U INITIAL "4999" 
     LABEL "Quantity 4" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty5 AS CHARACTER FORMAT "X(256)":U INITIAL "9999" 
     LABEL "Quantity 5" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty6 AS CHARACTER FORMAT "X(256)":U INITIAL "9999999" 
     LABEL "Quantity 6" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Quantity 7" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Quantity 8" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE in-file-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import From File" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "P:/asi10test/rco1010/images/browse-u.bmp":U
     SIZE 8 BY 1.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 3.81.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 13.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     in-file-name AT ROW 2.91 COL 22 COLON-ALIGNED
     fiQty1 AT ROW 6.71 COL 22 COLON-ALIGNED WIDGET-ID 2
     fiQty2 AT ROW 8.14 COL 22 COLON-ALIGNED WIDGET-ID 4
     fiQty3 AT ROW 9.57 COL 22 COLON-ALIGNED WIDGET-ID 6
     fiQty4 AT ROW 11 COL 22 COLON-ALIGNED WIDGET-ID 8
     fiQty5 AT ROW 12.43 COL 22 COLON-ALIGNED WIDGET-ID 10
     fiQty6 AT ROW 13.86 COL 22 COLON-ALIGNED WIDGET-ID 12
     fiQty7 AT ROW 15.29 COL 22 COLON-ALIGNED WIDGET-ID 14
     fiQty8 AT ROW 16.71 COL 22 COLON-ALIGNED WIDGET-ID 16
     btn-ok AT ROW 18.86 COL 23
     btn-cancel AT ROW 18.86 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Freight amounts will be loaded to these lower quantity bounds:" VIEW-AS TEXT
          SIZE 63 BY .95 AT ROW 5.29 COL 9 WIDGET-ID 24
     RECT-7 AT ROW 1 COL 2
     RECT-8 AT ROW 5.05 COL 2 WIDGET-ID 22
     IMAGE-1 AT ROW 2.91 COL 76 WIDGET-ID 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.


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
         TITLE              = "Import Fedex Freight Values"
         HEIGHT             = 20.19
         WIDTH              = 95.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import Fedex Freight Values */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import Fedex Freight Values */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.

  ASSIGN in-file-name.

  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "ERROR: Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO in-file-name.
    RETURN NO-apply.
  END.

  MESSAGE "Are you sure you want to import Fedex shipping rates?" 
       VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
  IF ll-ans THEN do:
      run processFile (INPUT in-file-name).
      IF ERROR-STATUS:ERROR = NO THEN
        MESSAGE "Rates are imported." VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 C-Win
ON MOUSE-SELECT-CLICK OF IMAGE-1 IN FRAME FRAME-A
DO:
  APPLY 'help' TO in-file-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME in-file-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL in-file-name C-Win
ON HELP OF in-file-name IN FRAME FRAME-A /* Import From File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-file ls-filename 
                 title "Select File to insert"
                 filters "Excel File (*.xlsx) " "*.xlsx,*.xls",
                         "Text File  (*.txt) " "*.txt",
                         "All Files    (*.*) " "*.*"
                 initial-dir "c:\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL in-file-name C-Win
ON LEAVE OF in-file-name IN FRAME FRAME-A /* Import From File */
DO:
  IF LASTKEY = -1 THEN RETURN.
  ASSIGN in-file-name.

  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "ERROR: Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-apply.
  END.
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
DEF VAR access-close AS LOG.
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.


  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
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
  DISPLAY in-file-name fiQty1 fiQty2 fiQty3 fiQty4 fiQty5 fiQty6 fiQty7 fiQty8 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 IMAGE-1 in-file-name fiQty1 fiQty2 fiQty3 fiQty4 fiQty5 
         fiQty6 fiQty7 fiQty8 btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processFile C-Win 
PROCEDURE processFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcInFile AS CHAR NO-UNDO.

  DEFINE VAR ok-file AS logi INIT YES.
  DEFINE VAR cLogFile AS CHAR NO-UNDO.
  DEFINE VAR cInLine AS CHAR NO-UNDO.
  DEFINE VAR cFileName AS CHAR NO-UNDO.
  DEFINE VAR cPnP AS CHAR NO-UNDO.
  DEFINE VAR lError AS LOG NO-UNDO.

  DEF VAR hLib          AS HANDLE NO-UNDO.  
  DEF VAR cTemplateFile AS CHAR NO-UNDO .
  DEF VAR dz_d AS DECI EXTENT 10.
  DEF VAR dz_dec AS DECI.
  DEF VAR dz_int AS INTE.

  DEF VAR col_src_zip      AS CHARACTER .
  DEF VAR col_dst_st  AS CHARACTER .
  DEF VAR col_class   AS CHARACTER .
  DEF VAR col_dst_zip   AS CHARACTER .
  DEF VAR Col_style   AS CHARACTER .
  DEF VAR Col_dz      AS CHARACTER .
  DEF VAR n_dz        AS I .
  DEF VAR iFileCnt    AS INT NO-UNDO.
  DEF VAR line-ctr AS INTE.
  DEF VAR vsrcZip AS CHAR.
  DEF VAR vDestZip AS CHAR.
  DEF VAR vDestSt AS CHAR.
  DEF VAR vClass AS CHAR.
  DEF VAR vdoz AS INTE.
  DEF VAR fr-ctl AS INTE.
  DEF VAR vCompanyZip LIKE company.zip NO-UNDO.

  DEF VAR col-list AS CHAR.
  DEF VAR vcol-label1 AS CHAR.
  DEF VAR vcol-label2 AS CHAR.
  DEF VAR vcol-label3 AS CHAR.
  DEF VAR vcol-label4 AS CHAR.
  DEF VAR vcol-label5 AS CHAR.
  DEF VAR vcust AS CHAR EXTENT 10.
  DEF VAR vLoadError AS LOG NO-UNDO.
  DEF VAR phand AS HANDLE NO-UNDO.
  DEF VAR nhand AS HANDLE NO-UNDO.
  DEF VAR v-stat-win AS HANDLE NO-UNDO.
  DEF VAR vrun-count AS INT NO-UNDO.

  DEFINE VAR chExcelApplication  AS COM-HANDLE NO-UNDO.
  DEFINE VAR chWorkbook          AS COM-HANDLE NO-UNDO.
  DEFINE VAR chWorksheet         AS COM-HANDLE NO-UNDO.
  DEFINE VAR chRange            AS COM-HANDLE NO-UNDO. 

  DISABLE TRIGGERS FOR LOAD OF carr-mtx.

  RUN windows/w-message.w PERSISTENT.
  phand = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(phand):
    nhand = phand:NEXT-SIBLING.
    IF phand:PRIVATE-DATA = "Process Status" THEN
      v-stat-win = phand.
    phand = nhand.
  END.

  IF VALID-HANDLE(v-stat-win) THEN
    RUN setWindowTitle IN v-stat-win (INPUT "Loading Fedex Freight Values"). 

  IF VALID-HANDLE(v-stat-win) THEN
    RUN Process-Message IN v-stat-win (INPUT "Converting file..."). 

  RUN methods/excelrep.p PERSISTENT SET hLib.
  THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib).

  SESSION:SET-WAIT-STATE("GENERAL").

  cFileName = ENTRY(NUM-ENTRIES(pcInFile,"\"),pcInFile,"\").
  cLogFile = "c:\temp\" + ENTRY(1,cFileName,".") + "-" + STRING(TODAY,'999999') + ".log".

  CREATE "Excel.Application" chExcelApplication.
  ASSIGN chExcelApplication:Visible = FALSE.

  chExcelApplication:Workbooks:Open(pcInFile,2,TRUE,,,,TRUE).

  ASSIGN chWorkbook = chExcelApplication:WorkBooks:Item(1)
         chWorkSheet = chExcelApplication:Sheets:Item(1).
  pcInFile = "c:\tmp\savecsv.csv".
  os-command silent del value(pcInFile).
/*  IF SEARCH(pcInFile) <> ? THEN
    OS-DELETE value(pcInfile) NO-ERROR. */
  chExcelApplication:DisplayAlerts = FALSE.
  chWorkbook:Saveas(pcInFile,6,,,,,,,,,,).
  chWorkBook:Close().

  /* RELEASE OBJECT chRange NO-ERROR. */
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chWorkBook NO-ERROR.

  chExcelApplication:Quit().

  RELEASE OBJECT chExcelApplication NO-ERROR. 
  IF VALID-HANDLE(v-stat-win) THEN
    RUN Process-Message IN v-stat-win (INPUT "Uploading records..."). 


  FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
  IF AVAIL company THEN DO:
    ASSIGN vCompanyZip = company.zip.
  END.

  INPUT STREAM sUpload FROM VALUE(pcInFile).
  IMPORT STREAM sUpload DELIMITER "," COL_src_zip COL_dst_zip COL_dst_st COL_class.
  ASSIGN i = 1
         line-ctr = 0
         vLoadError = NO. 
  load-loop:
  REPEAT:

    i = i + 1.

    IMPORT STREAM sUpload DELIMITER "," vSrcZip vDestZip vDestSt vClass dz_d[1]
                         dz_d[2] dz_d[3] dz_d[4] dz_d[5] dz_d[6] dz_d[7].

    IF vSrcZip NE vCompanyZip THEN DO:
      /* end of file reached */
      IF vSrcZip = "" THEN
      LEAVE load-loop.

      MESSAGE "Incorrect company defined - aborting load."
              "(" vCompanyZip " vs " vSrcZip ")"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      vLoadError = YES.
      LEAVE load-loop.
    END.
    dz_d[1] = ROUND(dz_d[1], 2).
    dz_d[2] = ROUND(dz_d[2], 2).
    dz_d[3] = ROUND(dz_d[3], 2).
    dz_d[4] = ROUND(dz_d[4], 2).
    dz_d[5] = ROUND(dz_d[5], 2).
    dz_d[6] = ROUND(dz_d[6], 2).
    dz_d[7] = ROUND(dz_d[7], 2).

    line-ctr = line-ctr + 1.
    vrun-count = vrun-count + 1.

    IF vrun-count GE 500 THEN DO:
        vrun-count = 0.
        IF VALID-HANDLE(v-stat-win) THEN
         RUN Process-Message IN v-stat-win (INPUT "Processed " + STRING(line-ctr) + " lines"). 
    END.


    FIND carr-mtx WHERE carr-mtx.company = cocode
                    AND carr-mtx.loc     = ipv-loc
                    AND carr-mtx.carrier = ipv-carrier
                    AND carr-mtx.del-zone = vClass
                    AND carr-mtx.del-zip = vDestZip
                  NO-ERROR.
    IF NOT AVAIL carr-mtx THEN DO:
      CREATE carr-mtx.
      ASSIGN carr-mtx.company  = cocode
             carr-mtx.loc      = ipv-loc
             carr-mtx.carrier  = ipv-carrier
             carr-mtx.del-zone = vClass
             carr-mtx.del-zip  = vDestZip
             carr-mtx.del-dscr = "Fedex Class " + vClass + " Zip " + vDestZip.
    END.
    IF carr-mtx.weight[1] NE DEC(fiQty1) OR
           carr-mtx.weight[2] NE DEC(fiQty2) OR
           carr-mtx.weight[3] NE DEC(fiQty3) OR
           carr-mtx.weight[4] NE DEC(fiQty4) OR
           carr-mtx.weight[5] NE DEC(fiQty5) OR
           carr-mtx.weight[6] NE DEC(fiQty6) OR
           carr-mtx.weight[7] NE DEC(fiQty7) OR
           carr-mtx.weight[8] NE DEC(fiQty8) OR
           carr-mtx.min-rate  NE dz_d[1] OR
           carr-mtx.rate[1]   NE dz_d[2] OR
           carr-mtx.rate[2]   NE dz_d[3] OR
           carr-mtx.rate[3]   NE dz_d[4] OR
           carr-mtx.rate[4]   NE dz_d[5] OR
           carr-mtx.rate[5]   NE dz_d[6] OR
           carr-mtx.rate[6]   NE dz_d[7] THEN
    ASSIGN carr-mtx.weight[1] = DEC(fiQty1)
           carr-mtx.weight[2] = DEC(fiQty2)
           carr-mtx.weight[3] = DEC(fiQty3)
           carr-mtx.weight[4] = DEC(fiQty4)
           carr-mtx.weight[5] = DEC(fiQty5)
           carr-mtx.weight[6] = DEC(fiQty6)
           carr-mtx.weight[7] = DEC(fiQty7)
           carr-mtx.weight[8] = DEC(fiQty8)
           carr-mtx.min-rate  = dz_d[1]
           carr-mtx.rate[1]   = dz_d[2]
           carr-mtx.rate[2]   = dz_d[3]
           carr-mtx.rate[3]   = dz_d[4]
           carr-mtx.rate[4]   = dz_d[5]
           carr-mtx.rate[5]   = dz_d[6]
           carr-mtx.rate[6]   = dz_d[7].                  

  END. /* repeat on each column */

  IF VALID-HANDLE(v-stat-win) THEN
   RUN Process-Message IN v-stat-win (INPUT "Process Complete - records processed: " + STRING(line-ctr)). 

  IF VALID-HANDLE(v-stat-win) THEN
    RUN disable_ui IN v-stat-win.

  SESSION:SET-WAIT-STATE("").

  IF vLoadError THEN
    RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

