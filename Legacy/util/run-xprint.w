&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Util\run-xprint.w

  Description: 
  
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
{methods/defines/hndldefs.i}
{system/sysconst.i}
{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File   AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE num-groups   AS INTEGER   NO-UNDO.
DEFINE VARIABLE group-ok     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL   NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
/*{salrep/dashprod.i NEW}*/


DEFINE TEMP-TABLE tt-oe-bolh 
    FIELD tt-recid    AS RECID
    FIELD bol-no      LIKE oe-bolh.ord-no
    FIELD i-count     AS INTEGER
    FIELD IS-SELECTED AS LOG     COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    .

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE            VARIABLE list-name         AS cha       NO-UNDO.
DEFINE            VARIABLE init-dir          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form    AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-prt-bypass     AS LOG       NO-UNDO.  /* bypass window's printer driver */
DEFINE            VARIABLE v-program         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE vcBOLNums         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lvFound           AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-pdf-file       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-font-no        AS CHARACTER FORMAT "X(256)":U INITIAL "15" NO-UNDO.
DEFINE            VARIABLE lv-ornt           AS CHARACTER INITIAL "P" NO-UNDO .
DEFINE NEW SHARED VARIABLE v-print-fmt       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.

{custom/xprint.i}

RUN sys/ref/nk1look.p (cocode, "BOLMaster", "C", NO, NO, "", "", 
    OUTPUT v-print-fmt, OUTPUT lvFound).

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS cha       FORM "x(200)" NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-10 fi_text rd-dest tb_log btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_text rd-dest tb_log begin_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE fi_text    AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 50 BY 5.24 NO-UNDO.

DEFINE VARIABLE begin_file AS CHARACTER FORMAT "X(100)" 
    VIEW-AS FILL-IN 
    SIZE 39.8 BY 1.

DEFINE VARIABLE rd-dest    AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 3
    SIZE 20 BY 5.05 NO-UNDO.

DEFINE RECTANGLE RECT-10
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 65 BY 5.95.

DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 71.6 BY 18.57.

DEFINE VARIABLE tb_log AS LOGICAL INITIAL NO 
    LABEL "Create Dbug Log File?" 
    VIEW-AS TOGGLE-BOX
    SIZE 33 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fi_text AT ROW 3.38 COL 16 NO-LABELS
    rd-dest AT ROW 11.48 COL 11.4 NO-LABELS
    tb_log AT ROW 14 COL 30 WIDGET-ID 8
    begin_file AT ROW 15.29 COL 27.6 COLON-ALIGNED HELP
    "File Path" NO-LABELS WIDGET-ID 10
    btn-ok AT ROW 17.52 COL 20.6
    btn-cancel AT ROW 17.52 COL 36
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 3
    BGCOLOR 2 
    "Output Options" VIEW-AS TEXT
    SIZE 19 BY .71 AT ROW 10.71 COL 10 WIDGET-ID 6
    RECT-9 AT ROW 1 COL 1
    RECT-10 AT ROW 10.95 COL 6 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 72 BY 19.14.


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
        TITLE              = "Xprint Test"
        HEIGHT             = 19.14
        WIDTH              = 72
        MAX-HEIGHT         = 32.52
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 32.52
        VIRTUAL-WIDTH      = 273.2
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
/* SETTINGS FOR FILL-IN begin_file IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    begin_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    fi_text:AUTO-RESIZE IN FRAME FRAME-A  = TRUE
    fi_text:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_log:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Xprint Test */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Xprint Test */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_file C-Win
ON LEAVE OF begin_file IN FRAME FRAME-A /* Beginning Customer# */
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

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

  
        CASE rd-dest:
            WHEN 1 THEN
                LvOutputSelection = "Printer".
            WHEN 2 THEN
                LvOutputSelection = "Screen". 
            WHEN 3 THEN
                LvOutputSelection = "Email".
     
        END CASE.

        ASSIGN 
            is-xprint-form = YES .
        RUN run-report("",NO).

        RUN GenerateReport("",NO).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_text C-Win
ON ENTRY OF fi_text IN FRAME FRAME-A
    DO:
        SELF:MODIFIED = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_text C-Win
ON HELP OF fi_text IN FRAME FRAME-A
    DO:
        DEFINE VARIABLE char-val AS cha   NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_text C-Win
ON LEAVE OF fi_text IN FRAME FRAME-A
    DO:
        DO WITH FRAME {&FRAME-NAME}:

            IF SELF:MODIFIED THEN
            DO:
        
            END.
        END.
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


&Scoped-define SELF-NAME tb_log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_log C-Win
ON VALUE-CHANGED OF tb_log IN FRAME FRAME-A /* Create Dbug Log File? */
    DO:
        ASSIGN {&self-name}.
        begin_file:SCREEN-VALUE = "Path: c:/temp/debug.csl" .

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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
   
 
    RUN enable_UI.
  
    {methods/nowait.i}
    {custom/usrprint.i}

 
 
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
    DISPLAY fi_text rd-dest tb_log begin_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-9 RECT-10 fi_text rd-dest tb_log btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
    DEFINE VARIABLE v-trans-lbl AS CHARACTER NO-UNDO .
   
    CASE rd-dest:
        WHEN 1 THEN RUN output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
        WHEN 2 THEN RUN output-to-screen(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
        WHEN 3 THEN 
            DO:
                IF is-xprint-form THEN 
                DO:
               
                    RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                    {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust= v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=  lv-pdf-file }
                END.
                ELSE 
                DO:
                    {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust= v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }
    
                END.
            END. 
    END CASE.
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

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    /*  ELSE IF lv-prt-bypass THEN
         RUN custom/d-print.w (list-name).*/
    ELSE
        RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).


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

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE
        RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProgram C-Win 
PROCEDURE pProgram :
    /* --------------------------------------------- oe/rep/oe-lad.p      RM ---- */
    /* print bill of ladings                                                      */
    /* -------------------------------------------------------------------------- */
  
    IF tb_log THEN 
    DO: 
        PUT "<DEBUG=ALL,FILE=c:/temp/debug.csl>" .
    END.

    PUT
        "<FMS Mincho>"
        "<P14><R4><C52><B>Asi Version #: {&awversion} " "</B><P10> "
        "<P14><R5><C52><B>Xprint Version #: " "</B><P10> "
        "<C3><R2><#1><C+3><R+8><C+45><IMAGE#1=" ls-full-img1  SKIP(1)    .

    PUT   "<#=100><AT=3.30,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" "Bar-test-45008" FORMAT "x(20)"  ">"
        "<AT=4.25,3>" "Bar-test-45008" FORMAT "x(20)"  .

    PUT SKIP(2)  "<FCourier New>"
        "Sold To:" SPACE(30) "Ship To:"  SKIP
        SPACE(5) "IBM CORP"  "1ST SOURCE SERVICE" AT 45 SKIP
        SPACE(5) "IBM Blvd" "DSC - S.E. - 05" AT 45 SKIP
        SPACE(5) "2nd Line of Address" "3850 PINSON VALLEY PKWY" AT 45 SKIP
        SPACE(5) "Rochester,NY 14606" "BIRMINGHAM, AL 35217" AT 45 SKIP .
    PUT SKIP(5)
        fi_text FORMAT "x(2000)" .
      


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* --------------------------------------------- oe/rep/oe-lad.p      RM ---- */
    /* print bill of ladings                                                      */
    /* -------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.

    {sys/form/r-top.i}

    {sys/inc/print1.i}
  
    {sys/inc/outprint.i value(99)}

    SESSION:SET-WAIT-STATE ("general").
   
  
    ASSIGN 
        lv-pdf-file = init-dir + "\TestXprint" + string(TIME) + ".pdf".


    IF IS-xprint-form THEN 
    DO:
  
        CASE rd-dest:
            WHEN 1 THEN 
                PUT "<PRINTER?>".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<PREVIEW><MODAL=NO>". 
                    ELSE
                        PUT "<PREVIEW>".        
                END.
            WHEN 3 THEN 
                DO:
                    PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ">" FORM "x(180)".
                END.
        END CASE.
    END.

    RUN pProgram . 
 
   
    OUTPUT CLOSE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

