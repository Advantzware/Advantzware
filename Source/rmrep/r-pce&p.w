&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-pce&p.w

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
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin.

 DEF NEW SHARED VAR v-post-date AS DATE INITIAL TODAY.
 DEF VAR v-cum-qty AS DEC EXTENT 2 FORMAT ">>>>>9.999". 
 DEF NEW SHARED VAR v-avgcost AS LOGICAL.
 DEF VAR save_id AS RECID.
 DEF VAR dollar-chg AS DEC.
 DEF VAR v-postable AS LOG NO-UNDO.
 DEF VAR v-invalid AS LOG NO-UNDO.
 DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
 DEF VAR is-xprint-form AS LOGICAL.
 DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEF VAR lv-uom LIKE rm-rcpth.pur-uom NO-UNDO.
DEF VAR ld-qty LIKE rm-rdtlh.qty NO-UNDO.
DEF VAR ld-cst LIKE  rm-rdtlh.cost NO-UNDO.
DEF VAR v-printed AS LOGI NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 post-date rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS post-date rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE post-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     post-date AT ROW 4.81 COL 39 COLON-ALIGNED
     rd-dest AT ROW 10.52 COL 6 NO-LABEL
     lv-ornt AT ROW 10.76 COL 31 NO-LABEL
     lines-per-page AT ROW 10.76 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 12.91 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 13.86 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.24 COL 31
     btn-ok AT ROW 19.57 COL 23
     btn-cancel AT ROW 19.57 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.57 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 9.1 COL 1.4
     RECT-7 AT ROW 1 COL 1.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 20.76.


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
         TITLE              = "Post Physical Counts"
         HEIGHT             = 21
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

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
ON END-ERROR OF C-Win /* Post Physical Counts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Post Physical Counts */
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
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
    DEF VAR lv-post AS LOG NO-UNDO.

    ASSIGN rd-dest
        post-date.
    SESSION:SET-WAIT-STATE ("general").

    ASSIGN rd-dest.
    IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
        THEN is-xprint-form = YES.     
    ELSE is-xprint-form = NO.

    RUN run-report. 

    SESSION:SET-WAIT-STATE ("").

    CASE rd-dest:
        WHEN 1 THEN RUN output-to-printer.
        WHEN 2 THEN RUN output-to-screen.
        WHEN 3 THEN RUN output-to-file.
        WHEN 4 THEN 
            DO:
           /*run output-to-fax.*/
                {custom/asifax.i &type= ''
                            &begin_cust= "post-date"
                            &END_cust= "post-date" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
            END. 
        WHEN 5 THEN 
            DO:
                IF is-xprint-form THEN 
                DO:
                    {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                END.
                ELSE 
                DO:
                    {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                END.
            END.
        WHEN 6 THEN RUN OUTPUT-to-port.
    END CASE. 

    SESSION:SET-WAIT-STATE("").

    IF rd-dest > 1 OR v-postable = NO THEN
        v-printed = YES.

    IF v-postable AND v-printed = YES THEN 
    DO:    
        lv-post = NO.

        MESSAGE "Post Physical Count?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

        IF lv-post THEN 
        DO:
            RUN cpost.
            MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.     
        END. 
    END.
    ELSE 
        IF v-printed = YES THEN
            MESSAGE "Nothing available for posting..." 
                VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL post-date C-Win
ON LEAVE OF post-date IN FRAME FRAME-A /* Transaction Date */
DO:
  ASSIGN {&self-name}.
END.
/*
  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

  */

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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
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

  post-date = TODAY.

  RUN enable_UI.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cpost C-Win 
PROCEDURE cpost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEF BUFFER b-rm-bin FOR rm-bin.

    DEF VAR v-temp-cost AS DEC FORMAT ">>>>>9.99".
    DEF VAR v-dunne     AS LOG INITIAL NO.
    DEF VAR next_r-no   LIKE rm-rcpth.r-no.
    DEF VAR v_r-no      LIKE rm-rcpth.r-no.
    DEF VAR v-r-qty     AS DEC NO-UNDO.
    DEF VAR v-i-qty     AS DEC NO-UNDO.
    DEF VAR v-t-qty     AS DEC NO-UNDO.

    postit:
    DO /* TRANSACTION */ ON ERROR UNDO postit, LEAVE postit:
        FOR EACH rm-rctd
            WHERE rm-rctd.company   EQ cocode
            AND rm-rctd.rita-code EQ "C",               
            FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no

            BREAK BY rm-rctd.i-no
            BY rm-rctd.rct-date
            BY rm-rctd.tag:

            ASSIGN
                item.last-count = 0
                item.q-onh      = 0
                item.last-date  = rm-rctd.rct-date.

            /** Find Bin & if not available then create it **/
            FIND FIRST rm-bin
                WHERE rm-bin.company EQ cocode
                AND rm-bin.loc     EQ rm-rctd.loc
                AND rm-bin.i-no    EQ rm-rctd.i-no
                AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                AND rm-bin.tag     EQ rm-rctd.tag
                NO-ERROR.

            IF NOT AVAIL rm-bin THEN 
            DO:
                IF rm-rctd.cost EQ 0 THEN ASSIGN  
                    rm-rctd.cost = IF v-avgcost THEN ITEM.avg-cost ELSE ITEM.last-cost
                    rm-rctd.cost-uom = ITEM.cons-uom.
                CREATE rm-bin.
                ASSIGN
                    rm-bin.company = rm-rctd.company
                    rm-bin.loc     = rm-rctd.loc
                    rm-bin.loc-bin = rm-rctd.loc-bin
                    rm-bin.tag     = rm-rctd.tag
                    rm-bin.i-no    = rm-rctd.i-no
                    rm-bin.cost    = rm-rctd.cost
                    rm-bin.po-no   = INTEGER(rm-rctd.po-no)
                    .
            END. /* not avail rm-bin */

            rm-bin.qty = rm-rctd.qty.

            /* Update bin with any transactions after this cycle count */
            FOR EACH rm-rcpth
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.i-no       EQ item.i-no
                AND rm-rcpth.trans-date GE rm-rctd.rct-date
                NO-LOCK USE-INDEX i-no,

                EACH rm-rdtlh
                WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                AND rm-rdtlh.loc       EQ rm-bin.loc
                AND rm-rdtlh.loc-bin   EQ rm-bin.loc-bin
                AND rm-rdtlh.tag       EQ rm-bin.tag
                NO-LOCK

                BY rm-rcpth.trans-date
                BY rm-rcpth.r-no
                BY RECID(rm-rdtlh):

                IF rm-rcpth.trans-date EQ rm-rctd.rct-date AND
                    rm-rcpth.r-no       LT rm-rctd.r-no       THEN NEXT. 

                {rm/rm-mkbin.i}
            END.           

            IF LAST-OF(rm-rctd.i-no) THEN 
            DO:
                v-temp-cost = 0.

                FOR EACH rm-bin
                    WHERE rm-bin.company EQ cocode
                    AND rm-bin.i-no    EQ item.i-no
                    ON ERROR UNDO postit, LEAVE:

                    ASSIGN
                        item.q-onh      = item.q-onh + rm-bin.qty
                        item.last-count = item.last-count + rm-bin.qty
                        v-temp-cost     = v-temp-cost + (rm-bin.qty * rm-bin.cost).
                END. /* each rm-bin */

                IF item.q-onh EQ 0 THEN item.avg-cost = 0.

                /** Calculate new average cost for item **/
                ELSE
                    IF v-temp-cost GT 0 THEN item.avg-cost = v-temp-cost / item.q-onh.

                item.q-avail = item.q-onh + item.q-ono - item.q-comm.
            END. /* last-of rm-rctd.i-no */

            {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */
            /* create rm-rcpth.
             {rm/rm-rcpt.i rm-rcpth rm-rctd}     /* Create Header History Records */
             CREATE rm-rdtlh.
             {rm/rm-rdtl.i rm-rdtlh rm-rctd}   /* Create Detail History Records */
             */
            DELETE rm-rctd.
        END. /* for each rm-rctd */

        v-dunne = TRUE.
    END. /* postit */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY post-date rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 post-date rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm btn-ok btn-cancel 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
/*      RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,                        */
/*                             INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result). */
/*                                     /* use-dialog(1) and landscape(2) */        */


/*      IF NOT RESULT THEN v-postable = NO. */

    RUN custom/prntproc2.p (list-name,INT(lv-font-no),lv-ornt, OUTPUT v-printed).

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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
    {sys/form/r-topw.f}

    FORM
        tt-rm-bin.i-no    LABEL "ITEM"
        item.i-name    LABEL "DESCRIPTION"
        item.cc-code   LABEL "CYCLE CODE"
        tt-rm-bin.loc     LABEL "WHSE"
        tt-rm-bin.loc-bin LABEL "BIN"
        tt-rm-bin.tag     FORMAT "X(20)" LABEL "TAG"
        tt-rm-bin.qty     FORM "->>,>>>,>>9.99" LABEL "BIN QTY"
        item.cons-uom  LABEL "UOM"
        rm-rctd.qty    LABEL "QTY COUNTED" FORMAT "->>>,>>9.999"
        dollar-chg     LABEL "VALUE CHANGE"
        SKIP
        WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 200.

    SESSION:SET-WAIT-STATE("general").
    v-postable = NO.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (112 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2. 

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH rm-rctd NO-LOCK WHERE 
        rm-rctd.company = cocode AND
        rm-rctd.rita-code = "C" BREAK BY rm-rctd.i-no BY rm-rctd.r-no WITH FRAME itemx:
        
        FIND FIRST item WHERE item.company = rm-rctd.company AND
            item.i-no    = rm-rctd.i-no NO-LOCK NO-ERROR.

        CREATE tt-rm-bin.

        FIND FIRST rm-bin
            WHERE rm-bin.company = rm-rctd.company
            AND rm-bin.loc     = rm-rctd.loc
            AND rm-bin.loc-bin = rm-rctd.loc-bin
            AND rm-bin.tag     = rm-rctd.tag
            AND rm-bin.i-no    = rm-rctd.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL rm-bin THEN
            BUFFER-COPY rm-bin EXCEPT rec_key TO tt-rm-bin.
        ELSE
            BUFFER-COPY rm-rctd EXCEPT po-no rec_key TO tt-rm-bin ASSIGN tt-rm-bin.po-no = INT(rm-rctd.po-no).

        IF LINE-COUNTER > 56 THEN PAGE.
        IF v-avgcost THEN
            dollar-chg = (rm-rctd.qty - tt-rm-bin.qty) * item.avg-cost.
        ELSE 
            dollar-chg = (rm-rctd.qty - tt-rm-bin.qty) * item.last-cost.
        DISPLAY
            tt-rm-bin.i-no    
            WHEN FIRST-OF(rm-rctd.r-no)
            item.i-name    
            WHEN FIRST-OF(rm-rctd.r-no)
            item.cc-code   
            WHEN FIRST-OF(rm-rctd.r-no)
            tt-rm-bin.loc
            tt-rm-bin.loc-bin
            tt-rm-bin.tag
            tt-rm-bin.qty
            item.cons-uom
             rm-rctd.qty    (sub-total by rm-rctd.i-no)
             dollar-chg     (sub-total by rm-rctd.i-no).
        DOWN.
        v-postable = YES.
        v-cum-qty[1] = v-cum-qty[1] + rm-rctd.qty.
        DELETE tt-rm-bin.     
    END. /* each rm-rctd */

    SESSION:SET-WAIT-STATE("").  
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

