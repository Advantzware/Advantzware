&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-quolst.w

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

/* Local Variable Definitions ---                                       */
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.

/* gdm - 10130808 */
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust-no end_cust-no ~
begin_slsmn end_slsmn begin_date end_date rd-dest tbAutoClose tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_date end_date rd-dest tbAutoClose tb_runExcel fi_file 

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
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-quolst.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To CSV", 3,
"To Email", 5
     SIZE 20 BY 3.62 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 6.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 4.52.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 3.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 3.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_date AT ROW 4.52 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Date"
     rd-dest AT ROW 7.62 COL 5 NO-LABEL
     tbAutoClose AT ROW 9.1 COL 55 WIDGET-ID 16
     tb_runExcel AT ROW 10.1 COL 75 RIGHT-ALIGNED WIDGET-ID 12
     fi_file AT ROW 11.33 COL 29 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 8
     btn-ok AT ROW 13.52 COL 26.8
     btn-cancel AT ROW 13.52 COL 47.2
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.67 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 5
          BGCOLOR 15 
     RECT-6 AT ROW 6.43 COL 2
     RECT-7 AT ROW 1.48 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57
         BGCOLOR 15 .


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
         TITLE              = "Quoted Price List"
         HEIGHT             = 14.48
         WIDTH              = 95.8
         MAX-HEIGHT         = 46.48
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.48
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Quoted Price List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Quoted Price List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
    /* gdm - 10130807 */
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

//  assign rd-dest
  //       lines-per-page .

  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type=" "
                            &begin_cust="begin_cust-no"
                            &end_cust="begin_cust-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust="begin_cust-no"
                             &end_cust="begin_cust-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust="begin_cust-no"
                                  &end_cust="begin_cust-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE. 
    IF tbAutoClose:CHECKED THEN 
        APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run CSV? */
DO:
  ASSIGN {&self-name}.
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

 ASSIGN   begin_date  = DATE(1,1,YEAR(TODAY))
          END_date    = TODAY.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
  RUN enable_UI.

  {methods/nowait.i}

   /* gdm - 10130807 */
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
   END.

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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_date end_date 
          rd-dest tbAutoClose tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn 
         begin_date end_date rd-dest tbAutoClose tb_runExcel fi_file btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                          /* font #*/ /* use-dialog(1) and landscape(2) */
  */
//  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
//  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ ce/rep/quote1.p 05/02 JLF */
/*                                                                            */
/* QUOTED PRICE LIST                                                          */                                                                       
/* -------------------------------------------------------------------------- */


{sys/form/r-top3w.f}

str-tit = coname + " - " + loname.
{sys/inc/ctrtext.i str-tit 112}.

DEF VAR fcust LIKE quote.cust-no NO-UNDO.
DEF VAR tcust LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEF VAR fsman LIKE quote.sman NO-UNDO.
DEF VAR tsman LIKE fsman INIT "zzz" NO-UNDO.
DEF VAR fdate AS DATE FORMAT "99/99/9999" INIT 01/01/01 NO-UNDO.
DEF VAR tdate LIKE fdate INIT TODAY NO-UNDO.

DEF VAR v-cst AS LOG FORMAT "yes/no" INIT NO NO-UNDO.

DEF VAR v-cst-hdr AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-price AS DEC NO-UNDO.
DEF VAR v-rels AS INT FORMAT ">>>" NO-UNDO.
DEF VAR v-sname LIKE sman.sname NO-UNDO.

FIND FIRST quote NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.
FIND FIRST sman NO-LOCK NO-ERROR.

/* gdm - 10130808 */
DEF VAR v_exclhdr1 AS CHAR NO-UNDO.
DEF VAR v_exclhdr2 AS CHAR NO-UNDO.
DEF VAR v_exclhdr3 AS CHAR NO-UNDO.

/* gdm - 10130808 */
DEF VAR v_billto AS CHAR             NO-UNDO.
DEF VAR v_addr1  AS CHAR             NO-UNDO.
DEF VAR v_addr2  AS CHAR             NO-UNDO.  
DEF VAR v_city   LIKE cust.city      NO-UNDO.
DEF VAR v_st     LIKE cust.state     NO-UNDO.
DEF VAR v_zip    LIKE cust.zip       NO-UNDO.
DEF VAR v_style  LIKE eb.style       NO-UNDO.
DEF VAR v_quo-dt AS CHAR             NO-UNDO.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

ASSIGN
 str-tit2 = TRIM(c-win:TITLE) 
 {sys/inc/ctrtext.i str-tit2 112}

 fcust    = begin_cust-no
 tcust    = end_cust-no
 fsman    = begin_slsmn
 tsman    = end_slsmn
 fdate    = begin_date
 tdate    = end_date.
/*
 fest     = begin_est
 test     = end_est.
  */

/* gdm - 10130808 */
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
ASSIGN
    v_exclhdr1 = 
    "Company Name,Address1,City,State,Zip,Rep," +
    "Qty,Price/M,Rel,Cust,Part#,Item Name,Size,Style,Board,Colors,Quote#,Est#,Date".

FORM HEADER
     SKIP(1)
     FILL("-",132)                          FORMAT "x(132)"
     quotehd.billto[1]                      FORMAT "x(25)"
     cust.addr[1]   WHEN AVAIL cust         FORMAT "x(25)"
     cust.addr[2]   WHEN AVAIL cust         FORMAT "x(25)"
     cust.city      WHEN AVAIL cust         FORMAT "x(15)"
     cust.state     WHEN AVAIL cust         FORMAT "x(02)"
     cust.zip       WHEN AVAIL cust         FORMAT "x(10)"
     v-sname                                FORMAT "x(24)"
     FILL("-",132)                          FORMAT "x(132)"
     SKIP(1)     
   WITH FRAME r-top STREAM-IO.

ASSIGN
     str-tit2 = "Quoted Price List - by Customer by Quote#"
     {sys/inc/ctrtext.i str-tit2 112}
     str-tit3 = "FROM  " +
                string(fdate,"99/99/99") + "  TO  " +
                string(tdate,"99/99/99")
     str-tit3 = TRIM(str-tit3)
     {sys/inc/ctrtext.i str-tit3 130}.

{sys/inc/print1.i}

  //  {sys/inc/outprint.i  value(lines-per-page) }
    
    {sys/inc/outprint.i}

//IF td-show-parm THEN RUN show-param.
/*
display str-tit with frame r-top.
*/
{sa/sa-sls01.i}

/* gdm - 10130808 */
//IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(cFileName).
    PUT STREAM excel UNFORMATTED
        v_exclhdr1
    SKIP.

// END.
SESSION:SET-WAIT-STATE("general").


   FOR EACH quotehd
        WHERE quotehd.company  EQ cocode
          AND quotehd.loc      EQ locode
          AND quotehd.cust-no  GE fcust
          AND quotehd.cust-no  LE tcust
          AND quotehd.sman     GE fsman
          AND quotehd.sman     LE tsman
          AND quotehd.quo-date GE fdate
          AND quotehd.quo-date LE tdate
        NO-LOCK:

       {custom/statusMsg.i " 'Processing Estimate#:  '  + quotehd.est-no  "}

      CREATE report.
      ASSIGN
       report.term-id = v-term
       report.key-01  = quotehd.cust-no
       report.key-02  = STRING(quotehd.q-no,"9999999999")
       report.rec-id  = RECID(quotehd).
    END.

    FOR EACH report WHERE report.term-id EQ v-term,
        FIRST quotehd WHERE RECID(quotehd)    EQ report.rec-id NO-LOCK,
        FIRST est   WHERE est.company    EQ quotehd.company                     
                      AND est.est-no     EQ quotehd.est-no NO-LOCK
        BREAK BY report.key-01: 

        {custom/statusMsg.i " 'Processing Estimate#:  '  + quotehd.est-no  "}

      FIND FIRST cust
          WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
          NO-LOCK NO-ERROR.     

      FIND FIRST sman
          WHERE sman.company EQ quotehd.company
            AND sman.sman    EQ quotehd.sman
         NO-LOCK NO-ERROR.

      IF FIRST-OF(report.key-01) THEN DO:
        v-sname = IF AVAIL sman THEN sman.sname ELSE quotehd.sman.
        IF FIRST(report.key-01) THEN VIEW FRAME r-top.

        ASSIGN
            v_billto = quotehd.billto[1]
            v_addr1  = cust.addr[1]   
            v_addr2  = cust.addr[2]    
            v_city   = cust.city     
            v_st     = cust.state    
            v_zip    = cust.zip.

        PAGE.
      END.

      FOR EACH quoteitm OF quotehd NO-LOCK:

        FIND FIRST eb WHERE eb.company = quoteitm.company
                        AND eb.est-no = est.est-no                     
                        AND eb.part-no EQ quoteitm.part-no
                        AND eb.form-no NE 0
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN
           FIND FIRST eb WHERE eb.company = quoteitm.company
                        AND eb.est-no = est.est-no                     
                        AND eb.form-no NE 0
                        NO-LOCK NO-ERROR.

        IF AVAIL eb THEN
           FIND FIRST ef WHERE ef.company = quoteitm.company
                           AND ef.est-no   EQ est.est-no
                           AND ef.form-no EQ eb.form-no
                           NO-LOCK NO-ERROR.

        v-rels = 1.

        ASSIGN v_style  = IF AVAIL eb THEN eb.style ELSE "".              

   /*     
        if quoteitm.line ge 1 and quoteitm.line le 10 and
                   quote.prof-on[quoteit.line] gt ""        then
        do j = 1 to length(trim(quote.prof-on[quoteit.line])):
          if substr(quote.prof-on[quoteit.line],j,1) ne ""     and
            (substr(quote.prof-on[quoteit.line],j,1) lt "0" or
             substr(quote.prof-on[quoteit.line],j,1) gt "9")   then do:
            j = 0.
            leave.
          end.  
        end.

        if j ne 0 then v-rels = int(quote.prof-on[quoteit.line]).
  */        
      FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
                          AND quoteqty.loc = quoteitm.loc
                          AND quoteqty.q-no = quoteitm.q-no
                          AND quoteqty.LINE = quoteitm.LINE NO-LOCK:

        IF quoteqty.uom EQ "M" THEN
          v-price = quoteqty.price.
        ELSE
          RUN sys/ref/convcuom.p (quoteqty.uom, "M", 0, 0, 0, 0,
                                  quoteqty.price, OUTPUT v-price).

        ASSIGN v_quo-dt = STRING(quotehd.quo-date,"99/99/99").

        DISPLAY quoteqty.qty        LABEL "Qty"
                                    FORMAT ">>,>>>,>>9"
                v-price             LABEL "Price/M"
                                    FORMAT ">>,>>9.99"
                quoteqty.rels       LABEL "Rel"         FORMAT ">>>"
                quoteitm.part-no    LABEL "Cust Part#"
                                    FORMAT "x(15)"
                quoteitm.part-dscr1 LABEL "Item Name"
                                    FORMAT "x(20)"
                quoteitm.size       LABEL "Size"
                                    FORMAT "x(10)"
                quoteitm.style      LABEL "Style"
                                    FORMAT "x(5)"
                eb.style WHEN AVAIL eb @ quoteitm.style
                quoteitm.i-dscr     LABEL "Board"
                                    FORMAT "x(10)"
                quoteitm.i-coldscr  LABEL "Colors"
                                    FORMAT "x(20)"
                quotehd.q-no        LABEL "Quote#"
                quotehd.est-no      LABEL "    Est#"    FORMAT "x(8)"
                quotehd.quo-date    LABEL "Date"        FORMAT "99/99/99"

            WITH FRAME detail DOWN NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 150.

        DOWN WITH FRAME detail.

        /* gdm - 10130808 */
//        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED               
                '"' v_billto '",'            
                '"' v_addr1 + v_addr2  '",'  
                '"' v_city   '",'            
                '"' v_st     '",'            
                '"' v_zip    '",'            
                '"' v-sname  '",'           
                '"' quoteqty.qty        '",'
                '"' v-price             '",'
                '"' quoteqty.rels       '",'
                '"' quoteitm.part-no    '",'
                '"' quoteitm.part-dscr1 '",'
                '"' quoteitm.size       '",'
                '"' quoteitm.style      '",'
                '"' v_style             '",'
                '"' REPLACE(quoteitm.i-dscr,'"',' ')   '",'
                '"' quoteitm.i-coldscr  '",'
                '"' quotehd.q-no        '",'
                '"' quotehd.est-no      '",'
                '"' v_quo-dt            '"'
               SKIP.

      END.     

      END.
      PUT SKIP(1).

      DELETE report.
    END.

/* gdm - 10130808 */
//    IF tb_excel THEN DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
   // END.

/* gdm - 10130807 */
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

