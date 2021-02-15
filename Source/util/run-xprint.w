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
DEFINE VARIABLE hOutputProcs AS HANDLE NO-UNDO.

{custom/xprint.i}

RUN sys/ref/nk1look.p (cocode, "BOLMaster", "C", NO, NO, "", "", 
    OUTPUT v-print-fmt, OUTPUT lvFound).

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
ASSIGN 
    ls-full-img1 = cRtnChar  .
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath",
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.    

RUN system/OutputProcs.p PERSISTENT SET hOutputProcs.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-10 fi_text tb_rec tb_line ~
tb_image tb_bar-code begin_font rd-dest begin_font-size tb_log btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_text tb_rec tb_line tb_image ~
tb_bar-code begin_font rd-dest begin_font-size tb_log begin_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_text AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 50 BY 3.1 NO-UNDO.

DEFINE VARIABLE begin_file AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 39.8 BY 1.

DEFINE VARIABLE begin_font AS CHARACTER FORMAT "X(100)" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE begin_font-size AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Font Size" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
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

DEFINE VARIABLE tb_bar-code AS LOGICAL INITIAL no 
     LABEL "Print Bar Code" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_image AS LOGICAL INITIAL no 
     LABEL "Print Image" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_line AS LOGICAL INITIAL no 
     LABEL "Print Line" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_log AS LOGICAL INITIAL no 
     LABEL "Create Dbug Log File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rec AS LOGICAL INITIAL no 
     LABEL "Print Rectangle" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_text AT ROW 2.67 COL 16 NO-LABEL
     tb_rec AT ROW 6.19 COL 16.2 WIDGET-ID 12
     tb_line AT ROW 7.24 COL 16.2 WIDGET-ID 14
     tb_image AT ROW 8.29 COL 16.2 WIDGET-ID 16
     tb_bar-code AT ROW 9.38 COL 16.2 WIDGET-ID 18
     begin_font AT ROW 11.24 COL 43 COLON-ALIGNED HELP
          "File Path" WIDGET-ID 20
     rd-dest AT ROW 11.48 COL 11.4 NO-LABEL
     begin_font-size AT ROW 12.38 COL 52.4 COLON-ALIGNED HELP
          "File Path" WIDGET-ID 22
     tb_log AT ROW 14 COL 30 WIDGET-ID 8
     begin_file AT ROW 15.29 COL 27.6 COLON-ALIGNED HELP
          "File Path" NO-LABEL WIDGET-ID 10
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
/* SETTINGS FOR FILL-IN begin_file IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_font:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_font-size:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_text:AUTO-RESIZE IN FRAME FRAME-A      = TRUE
       fi_text:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_bar-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_image:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_line:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_log:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
ON LEAVE OF begin_file IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_font C-Win
ON LEAVE OF begin_font IN FRAME FRAME-A /* Font */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_font-size
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_font-size C-Win
ON LEAVE OF begin_font-size IN FRAME FRAME-A /* Font Size */
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
    DEF VAR lError AS LOG NO-UNDO.

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
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hOutputProcs).
         
    /* 95411 Add check for latest versions of 3rd party components to Xprint tester */
    RUN ipCheck3dPartyVersions (OUTPUT lError).
    IF lError THEN RETURN.
             
           RUN run-report("",NO).
        IF rd-dest EQ 3 THEN
        RUN GenerateReport("",YES).
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hOutputProcs). 

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


&Scoped-define SELF-NAME tb_bar-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_bar-code C-Win
ON VALUE-CHANGED OF tb_bar-code IN FRAME FRAME-A /* Create Bar Code */
DO:
        ASSIGN {&self-name}.
        begin_file:SCREEN-VALUE = "Path: c:/temp/debug.csl" .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_image C-Win
ON VALUE-CHANGED OF tb_image IN FRAME FRAME-A /* Create Image */
DO:
        ASSIGN {&self-name}.
        begin_file:SCREEN-VALUE = "Path: c:/temp/debug.csl" .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_line C-Win
ON VALUE-CHANGED OF tb_line IN FRAME FRAME-A /* Create Line */
DO:
        ASSIGN {&self-name}.
        begin_file:SCREEN-VALUE = "Path: c:/temp/debug.csl" .

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


&Scoped-define SELF-NAME tb_rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rec C-Win
ON VALUE-CHANGED OF tb_rec IN FRAME FRAME-A /* Create Rectangle */
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
  DISPLAY fi_text tb_rec tb_line tb_image tb_bar-code begin_font rd-dest 
          begin_font-size tb_log begin_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 RECT-10 fi_text tb_rec tb_line tb_image tb_bar-code begin_font 
         rd-dest begin_font-size tb_log btn-ok btn-cancel 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCheck3dPartyVersions C-Win
PROCEDURE ipCheck3dPartyVersions:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER oplError AS LOG.
    
    DEF VAR cCommand AS CHAR NO-UNDO.
    DEF VAR cVersionLine AS CHAR NO-UNDO.
    DEF VAR lInXprint AS LOG NO-UNDO.
    DEF VAR lInJasper AS LOG NO-UNDO.
    DEF VAR cXprintTgtVer AS CHAR NO-UNDO INITIAL "10.17".
    DEF VAR cJasperTgtVer AS CHAR NO-UNDO INITIAL "3.4.1".
    DEF VAR cXprintActVer AS CHAR NO-UNDO.
    DEF VAR cJasperActVer AS CHAR NO-UNDO.
    
    OS-DELETE VALUE("c:\tmp\3PVersion1.txt").
    OS-DELETE VALUE("c:\tmp\3PVersion2.txt").
    OS-DELETE VALUE("c:\tmp\3PVersion.txt").

    ASSIGN 
        cCommand = "reg export HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall c:\tmp\3PVersion1.txt".
    OS-COMMAND SILENT VALUE(cCommand).
    ASSIGN 
        cCommand = "reg export HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\JasperStarter c:\tmp\3PVersion2.txt".
    OS-COMMAND SILENT VALUE(cCommand).
    OS-COMMAND SILENT VALUE("COPY c:\tmp\3PVersion1.txt + c:\tmp\3PVersion2.txt c:\tmp\3PVersion.txt").
    
    INPUT FROM VALUE("c:\tmp\3PVersion.txt").
    REPEAT:
        IMPORT UNFORMATTED cVersionLine.
        IF INDEX(cVersionLine,'"DisplayName"="vpxPrint') NE 0 THEN 
        DO:
            ASSIGN 
                lInXprint = TRUE.
            NEXT.
        END.
        ELSE IF INDEX(cVersionLine,'"DisplayName"="JasperStarter') NE 0 THEN 
            DO: 
                ASSIGN 
                    cJasperActVer = ENTRY(2,cVersionLine," ")
                    cJasperActVer = TRIM(cJasperActVer,'"').
                NEXT.
            END.
            ELSE IF lInXprint AND INDEX(cVersionLine,"DisplayVersion") NE 0 THEN 
                DO: 
                    ASSIGN 
                        cXprintActVer = REPLACE(cVersionLine,'"','')
                        cXprintActVer = ENTRY(2,cXprintActVer,"=")
                        lInXprint = FALSE.
                    NEXT.
                END.
    END.

    OS-DELETE VALUE("c:\tmp\3PVersion1.txt").
    OS-DELETE VALUE("c:\tmp\3PVersion2.txt").
    OS-DELETE VALUE("c:\tmp\3PVersion.txt").

    IF cXprintActVer EQ "" THEN DO: 
        MESSAGE 
            "YOU DO NOT HAVE XPRINT SOFTWARE INSTALLED!" SKIP(1)
            "Advantzware will never send an email that asks you to provide, confirm or verify personal, login or account" SKIP 
            "information."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplError = TRUE.
    END.
    ELSE IF cXprintActVer LT cXprintTgtVer THEN DO: 
        MESSAGE 
            "Your xPrint version is out of date." SKIP 
            "You should update this from the Advantzware" SKIP 
            "'Install/LocalPrintInstall' folder to version " + cXprintTgtVer
            VIEW-AS ALERT-BOX.
        ASSIGN 
            oplError = TRUE.
    END.
    ELSE MESSAGE 
        "xPrint version check PASSED."
        VIEW-AS ALERT-BOX INFO.            

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
      DEFINE VARIABLE cText AS CHARACTER NO-UNDO .
      DEFINE VARIABLE iRowCount AS INTEGER.
  
    IF tb_log THEN 
    DO: 
        PUT "<DEBUG=ALL,FILE=c:/temp/debug.csl>" .
    END.

    RUN ChangeXprintFont(begin_font,begin_font-size).
    
    
    RUN Output_WriteToXprint(4,52,"Asi Version #: {&awversion} ",YES,NO,NO,NO) .

    RUN Output_WriteToXprint(5,52,"Xprint Version #: ",YES,NO,NO,NO) .

    IF tb_image THEN
    RUN Output_WriteToXprintImage(2,3,8,50,ls-full-img1) . /* row form, col from, row size,col size ,image path */
    
    iRowCount = iRowCount + 8 .
    
   IF tb_bar-code THEN
    RUN Output_WriteToXprintBarCode(13,20,2.5,30,"Test data test","39") . /* row form, col from, row size,col size ,value ,type*/

   iRowCount = iRowCount + 5 .

   IF tb_rec THEN
     RUN Output_WriteToXprintRect(18,26,1.5,43) . /* row from, row to, col from ,col to*/

   IF tb_rec THEN
     RUN Output_WriteToXprintRect(18,26,48.5,85) . /* row from, row to, col from ,col to*/
   
    RUN Output_WriteToXprint(19, 1," Sold To:" , NO, NO, NO, NO).
    RUN Output_WriteToXprint(19, 49," Ship To:" , NO, NO, NO, NO).

    RUN Output_WriteToXprint(20, 5," IBM CORP" , NO, NO, NO, NO).
    RUN Output_WriteToXprint(20, 52," 1ST SOURCE SERVICE" , NO, NO, NO, NO).

    RUN Output_WriteToXprint(21, 5," 2nd Line of Address" , NO, NO, NO, NO).
    RUN Output_WriteToXprint(21, 52," 3850 PINSON VALLEY PKWY" , NO, NO, NO, NO).

    RUN Output_WriteToXprint(22, 5," Rochester,NY 14606" , NO, NO, NO, NO).
    RUN Output_WriteToXprint(22, 52," BIRMINGHAM, AL 35217" , NO, NO, NO, NO).

    iRowCount = iRowCount + 9 .
   
    RUN AddRow(INPUT-OUTPUT iRowCount, INPUT-OUTPUT iRowCount).


    cText =  fi_text .
     RUN Output_WriteToXprint(35,5,cText,NO, NO, NO, NO) .

     iRowCount = iRowCount + 13 .

     IF tb_line THEN
         RUN Output_WriteToXprintLine(48,5,85) .
     iRowCount = iRowCount + 13 .


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
  
    /*{sys/inc/outprint.i value(99)}*/
   RUN Output_InitializeXprint(list-name, YES, lBussFormModle, begin_font, begin_font-size,"") .

    SESSION:SET-WAIT-STATE ("general").
   
  
    ASSIGN 
        lv-pdf-file = init-dir + "\TestXprint" + string(TIME) + ".pdf".


    IF IS-xprint-form THEN 
    DO:
  
        CASE rd-dest:
            WHEN 1 THEN 
                RUN Output_WriteToXprint(1,1,"<PRINTER?>",YES,NO,NO,NO) .
            WHEN 2 THEN 
                DO:
                END.
            WHEN 3 THEN 
                DO:
                 RUN Output_WriteToXprint(1,1,"<PDF-OUTPUT=" + lv-pdf-file + ">",YES,NO,NO,NO) .
                END.
        END CASE.
    END.

    RUN pProgram . 
    
    RUN Output_Close.
    
    IF rd-dest NE 3 THEN
    RUN Output_PrintXprintFile(list-name).

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

