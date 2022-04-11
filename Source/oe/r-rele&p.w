&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rele&p.w

  Description: Order Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/11/2002

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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEFINE VARIABLE ip-post AS LOG NO-UNDO.
&ELSE
DEFINE INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name        AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-last-assigned AS CHARACTER NO-UNDO.
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

{oe/oe-relp1.i NEW}

DEFINE TEMP-TABLE tt-user-print LIKE user-print.
DEFINE TEMP-TABLE tt-date 
    FIELD tt-name  AS cha
    FIELD tt-value AS cha.
DEFINE BUFFER xfg-bin FOR fg-bin.

DEFINE VARIABLE v-ship-lu             AS ch        INITIAL ["I,S,B"].
DEFINE VARIABLE v-ship-no             AS INTEGER.
DEFINE VARIABLE v-s-code              AS CHARACTER.
DEFINE VARIABLE v-no-post             AS INTEGER   FORMAT ">>>9".
DEFINE VARIABLE v-tot-post            AS INTEGER   FORMAT ">>>9".
DEFINE VARIABLE v-first-release       AS LOG.
DEFINE VARIABLE v-r-no                LIKE inv-head.r-no.
DEFINE VARIABLE v-ext-price           LIKE inv-line.t-price.
DEFINE VARIABLE v-nxt-r-no            AS INTEGER.
DEFINE VARIABLE v-po-no               LIKE oe-rel.po-no.
DEFINE VARIABLE v-royal               AS LOG.
DEFINE VARIABLE v-n-bol               LIKE oe-ctrl.n-bol.
DEFINE VARIABLE v-bol-qty             LIKE oe-boll.qty.
DEFINE VARIABLE temp-tax              AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE v-hold-list           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-invalid             AS LOG       NO-UNDO.
DEFINE VARIABLE lAllowUserMultRelease AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAccessClose          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAccessList           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName             AS CHARACTER NO-UNDO.

DEFINE STREAM s-temp.

DEFINE TEMP-TABLE w-nopost NO-UNDO
    FIELD ord-no   LIKE oe-relh.ord-no
    FIELD rel-date LIKE oe-relh.rel-date
    FIELD rel-no   LIKE oe-rell.rel-no
    FIELD b-ord-no LIKE oe-relh.b-ord-no
    FIELD cust-no  LIKE oe-relh.cust-no
    FIELD ship-id  LIKE oe-relh.ship-id
    FIELD i-no     LIKE oe-rell.i-no
    FIELD part-no  LIKE itemfg.part-no
    FIELD i-name   AS CHARACTER FORMAT "x(25)"
    FIELD loc      LIKE oe-rell.loc
    FIELD loc-bin  LIKE oe-rell.loc-bin
    FIELD tag      LIKE oe-rell.tag
    FIELD qty      LIKE oe-rell.qty
    FIELD code     AS CHARACTER FORMAT "x(5)"
    FIELD link-no  LIKE oe-rell.link-no.

DEFINE VARIABLE v-chkflg AS LOG NO-UNDO.

{sys/ref/relpost.i}

{sys/inc/relcrhold.i}

RUN methods/prgsecur.p
    (INPUT "MultReleaseAllow",
    INPUT "ACCESS", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT lAllowUserMultRelease, /* Allowed? Yes/NO */
    OUTPUT lAccessClose, /* used in template/windows.i  */
    OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_relnum ~
end_relnum begin_date end_date begin_cust end_cust begin_ord end_ord ~
tgMultipleReleases rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_text-1 tran-date tran-period ~
begin_relnum end_relnum begin_date end_date begin_cust end_cust begin_ord ~
end_ord tgMultipleReleases rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Release Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_relnum   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Release#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Release Date#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord        AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_relnum     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Release#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)":U INITIAL "c:~\tmp~\ReleaseEditList.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE fi_text-1      AS CHARACTER FORMAT "X(256)":U INITIAL "Selection Parameters" 
    VIEW-AS FILL-IN 
    SIZE 21.6 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Post Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period    AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To CSV", 3
    SIZE 15 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.05.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 11.43.

DEFINE VARIABLE tbAutoClose        AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel           AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV         AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm       AS LOGICAL INITIAL YES 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgMultipleReleases AS LOGICAL INITIAL YES 
    LABEL "Multiple Releases" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fi_text-1 AT ROW 1.19 COL 3 COLON-ALIGNED NO-LABELS
    tran-date AT ROW 2.43 COL 36 COLON-ALIGNED
    tran-period AT ROW 2.43 COL 64 COLON-ALIGNED
    begin_relnum AT ROW 3.62 COL 28 COLON-ALIGNED HELP
    "Enter the beginning release number"
    end_relnum AT ROW 3.62 COL 73 COLON-ALIGNED HELP
    "Enter the ending release number"
    begin_date AT ROW 4.57 COL 28 COLON-ALIGNED HELP
    "Enter the beginning release date"
    end_date AT ROW 4.57 COL 73 COLON-ALIGNED HELP
    "Enter the ending release date"
    begin_cust AT ROW 5.52 COL 28 COLON-ALIGNED HELP
    "Enter the beginning customer number"
    end_cust AT ROW 5.52 COL 73 COLON-ALIGNED HELP
    "Enter the ending customer number"
    begin_ord AT ROW 6.48 COL 28 COLON-ALIGNED HELP
    "Enter the beginning order number"
    end_ord AT ROW 6.48 COL 73 COLON-ALIGNED HELP
    "Enter the ending order number"
    tgMultipleReleases AT ROW 7.67 COL 30 WIDGET-ID 8
    lv-font-no AT ROW 13.86 COL 34 COLON-ALIGNED
    lv-ornt AT ROW 13.86 COL 44 NO-LABELS
    lines-per-page AT ROW 13.86 COL 87 COLON-ALIGNED
    rd-dest AT ROW 14.57 COL 4 NO-LABELS
    lv-font-name AT ROW 15.05 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 16.24 COL 72 WIDGET-ID 2
    td-show-parm AT ROW 16.29 COL 28
    fi_file AT ROW 17.24 COL 26 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 6
    tb_OpenCSV AT ROW 17.29 COL 77.4 WIDGET-ID 4
    tbAutoClose AT ROW 18.62 COL 28 WIDGET-ID 64
    btn-ok AT ROW 19.57 COL 27.8
    btn-cancel AT ROW 19.57 COL 50
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 13.14 COL 4
    RECT-6 AT ROW 13.57 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 25.24
    BGCOLOR 15 .

DEFINE FRAME FRAME-E
    "Release tickets MUST BE Printed prior to posting!" VIEW-AS TEXT
    SIZE 61 BY 1.19 AT ROW 4.1 COL 19
    BGCOLOR 8 FGCOLOR 0 FONT 5
    "The Edit List will show all available releases to be" VIEW-AS TEXT
    SIZE 61 BY 1.19 AT ROW 1.71 COL 19
    BGCOLOR 8 FGCOLOR 0 FONT 5
    "posted to all orders." VIEW-AS TEXT
    SIZE 26 BY 1.19 AT ROW 2.91 COL 19
    BGCOLOR 8 FGCOLOR 0 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 3 ROW 8.62
    SIZE 91 BY 4.52
    BGCOLOR 8 .


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
        TITLE              = "Release Edit List & Posting"
        HEIGHT             = 20.1
        WIDTH              = 95
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
/* REPARENT FRAME */
ASSIGN 
    FRAME FRAME-E:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_relnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_relnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_text-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tgMultipleReleases:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-E
                                                                        */
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
ON END-ERROR OF C-Win /* Release Edit List  Posting */
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
ON WINDOW-CLOSE OF C-Win /* Release Edit List  Posting */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Release Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord C-Win
ON LEAVE OF begin_ord IN FRAME FRAME-A /* Beginning Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_relnum C-Win
ON LEAVE OF begin_relnum IN FRAME FRAME-A /* Beginning Release# */
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
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.


        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        ASSIGN
            v-frel     = begin_relnum
            v-trel     = end_relnum
            v-fdat     = begin_date
            v-tdat     = end_date
            v-fcus     = begin_cust
            v-tcus     = end_cust
            v-ford     = begin_ord
            v-tord     = end_ord
            v-no-post  = 0
            v-tot-post = 0.

        IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN
            ASSIGN END_relnum = begin_relnum v-trel     = begin_relnum.


        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
        END CASE.
        SESSION:SET-WAIT-STATE ("").

        IF ip-post THEN 
        DO:
            IF v-tot-post GT 0 THEN 
            DO:
                lv-post = NO.

                MESSAGE "Post Releases?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE lv-post.

                IF lv-post THEN 
                DO:
                    RUN post-releases.

                    MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
                END.
            END.

            ELSE MESSAGE "No releases available for posting..." VIEW-AS ALERT-BOX ERROR.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Release Date# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord C-Win
ON LEAVE OF end_ord IN FRAME FRAME-A /* Ending Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_relnum C-Win
ON LEAVE OF end_relnum IN FRAME FRAME-A /* Ending Release# */
    DO:
        ASSIGN {&self-name}.
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
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest.
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


&Scoped-define SELF-NAME tgMultipleReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgMultipleReleases C-Win
ON VALUE-CHANGED OF tgMultipleReleases IN FRAME FRAME-A /* Multiple Releases */
    DO:
        ASSIGN {&SELF}.
        IF tgMultipleReleases:SCREEN-VALUE EQ "YES" THEN 
        DO:
            ASSIGN 
                END_relnum:VISIBLE = TRUE 
                begin_relnum:LABEL = "Beginning Release#".
            ENABLE END_relnum.
            END_relnum:SENSITIVE = YES.
        END.
        ELSE 
        DO:
            ASSIGN 
                END_relnum:VISIBLE      = FALSE
                begin_relnum:LABEL      = "Release#"
                END_relnum:SCREEN-VALUE = STRING(begin_relnum)  .
            DISABLE END_relnum.
            END_relnum:SENSITIVE = NO.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
    DO:
        ASSIGN {&self-name}.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date.
            IF v-invalid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    ASSIGN
        tran-date   = TODAY
        begin_date  = TODAY
        end_date    = TODAY
        c-win:TITLE = IF ip-post THEN "Release Posting/Create BOL"
                            ELSE "Release Edit List".


    v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BOLFMT"
        NO-LOCK NO-ERROR.
    v-royal = AVAILABLE sys-ctrl AND lookup(sys-ctrl.char-fld,v-hold-list) NE 0.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OT2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
        
    ASSIGN tgMultipleReleases.
    IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN 
    DO:
        ASSIGN 
            END_relnum:VISIBLE = FALSE 
            begin_relnum:LABEL = "Release#".
        DISABLE END_relnum.
        END_relnum:SENSITIVE = NO.
    END.
    RUN check-date.

    IF ip-post THEN 
    DO:
        fi_text-1:BGCOLOR = ?.
        DISPLAY fi_text-1.
    END.

    {methods/nowait.i}

    IF NOT ip-post THEN 
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        /*ASSIGN tgMultipleReleases.*/  
        APPLY "entry" TO begin_relnum.  
        DISABLE tran-date tran-period.
        /* In case this changes after usrprint */
        IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                END_relnum:VISIBLE = FALSE 
                begin_relnum:LABEL = "Release#".
            DISABLE END_relnum.
            END_relnum:SENSITIVE = NO.
        END.

    END.
    ELSE 
    DO:
        {custom/usrprintv.i "tgMultipleReleases"}
        /* ASSIGN tgMultipleReleases. */
        /* In case this changes after usrprint */
        IF lv-last-assigned NE "YES" THEN 
        DO:
            RUN disable-multi-release.
        END.
    END.
    IF NOT lAllowUserMultRelease THEN 
    DO: 
        ASSIGN 
            tgMultipleReleases:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No" 
            tgMultipleReleases:SENSITIVE IN FRAME {&FRAME-NAME}    = NO .
        RUN disable-multi-release.
    END.
    RUN pChangeDest.
    RUN pChangeFileName.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

        ELSE
            IF ip-post THEN 
            DO:
                MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-multi-release C-Win 
PROCEDURE disable-multi-release :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            END_relnum:VISIBLE = FALSE 
            begin_relnum:LABEL = "Release#".
        DISABLE END_relnum.
        END_relnum:SENSITIVE = NO.
    END.
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
    DISPLAY fi_text-1 tran-date tran-period begin_relnum end_relnum begin_date 
        end_date begin_cust end_cust begin_ord end_ord tgMultipleReleases 
        rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date begin_relnum end_relnum begin_date end_date 
        begin_cust end_cust begin_ord end_ord tgMultipleReleases rd-dest 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-E IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
    {oe/relexcpt.i}

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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
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

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */

    /* Use Progress Print. Always use Font#9 in Registry (set above) */
    RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
        INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
/* use-dialog(1) and landscape(2) */

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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-releases C-Win 
PROCEDURE post-releases :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-exception AS LOG NO-UNDO.

    DEFINE BUFFER upd-oe-relh FOR oe-relh.
    DEFINE BUFFER upd-oe-rell FOR oe-rell.

    {sa/sa-sls01.i}

    FOR EACH tt-except:
        DELETE tt-except.
    END.

    FOR EACH tt-fg-bin:
        DELETE tt-fg-bin.
    END.

    DISABLE TRIGGERS FOR LOAD OF itemfg.

    IF relpost-chr EQ "Nothing" THEN
        headblok:
        FOR EACH oe-relh FIELDS(company r-no deleted) NO-LOCK
            WHERE oe-relh.company  EQ cocode
            AND oe-relh.posted   EQ NO
            AND oe-relh.printed  EQ YES
            AND oe-relh.release# GE v-frel
            AND oe-relh.release# LE v-trel
            AND oe-relh.rel-date GE v-fdat
            AND oe-relh.rel-date LE v-tdat
            AND oe-relh.cust-no  GE v-fcus
            AND oe-relh.cust-no  LE v-tcus
            /* gdm - 03110907 */
            AND (NOT v-chkflg OR
            (v-chkflg AND oe-relh.w-ord = NO))
            /* gdm - 03110907 end */
            AND NOT CAN-FIND(FIRST oe-rell
            WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND (oe-rell.ord-no LT v-ford OR
            oe-rell.ord-no GT v-tord OR
            oe-rell.s-code EQ "I")
            USE-INDEX r-no)
            USE-INDEX post.

            RUN oe/relcheck.p (ROWID(oe-relh), OUTPUT ll-exception).
            IF ll-exception THEN NEXT headblok.


            {oe/oe-relp.i}
        END. /* each oe-relh */

    RUN oe/oe-relp2.p (v-term, v-royal).

    FOR EACH report WHERE report.term-id EQ v-term EXCLUSIVE-LOCK, 
        FIRST oe-boll
        WHERE RECID(oe-boll) EQ report.rec-id
        AND CAN-FIND(FIRST oe-bolh
        WHERE oe-bolh.b-no    EQ oe-boll.b-no
        AND oe-bolh.printed EQ NO)
        NO-LOCK:
        DELETE report.
    END.

    RUN oe/oe-bolp3.p(
        INPUT v-term,
        INPUT tran-date 
        ).

    delete-blok:
    FOR EACH oe-relh NO-LOCK
        WHERE oe-relh.company EQ cocode
        AND oe-relh.deleted EQ YES
        USE-INDEX deleted:

        FIND upd-oe-relh WHERE ROWID(upd-oe-relh) EQ ROWID(oe-relh) EXCLUSIVE NO-WAIT NO-ERROR.

        IF AVAILABLE upd-oe-relh THEN 
        DO:
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ oe-relh.company
                AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no:
                FIND upd-oe-rell WHERE ROWID(upd-oe-rell) EQ ROWID(oe-rell) EXCLUSIVE NO-WAIT NO-ERROR.
                IF AVAILABLE upd-oe-rell THEN DELETE upd-oe-rell.
                ELSE UNDO delete-blok, NEXT delete-blok.
            END. /* each oe-rell */
            DELETE upd-oe-relh.
        END.
    END.

    FIND FIRST tt-except NO-ERROR.
    IF AVAILABLE tt-except THEN 
    DO:
        ll-exception = YES.
        MESSAGE "  Releases Tickets have been found that are in use  "    SKIP
            "  or have insufficient inventory for posting to be  "    SKIP
            "  completed.  Do you wish to print the exceptions?  "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-exception.

        IF ll-exception THEN 
        DO:
            RUN exception-rpt.

            CASE rd-dest:
                WHEN 1 THEN RUN output-to-printer.
                WHEN 2 THEN RUN output-to-screen.
                WHEN 3 THEN RUN output-to-file.
            END CASE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------- oe/rep/neword.p 8/93 rd  */
    /* Order Entry - Edit Register                                                */
    /*  FOR: Order Status of - (N)ew, (A)pproved Credit, (U)pdated, (D)eleted,    */
    /*                          & (C)losed                                        */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-export    AS LOG       NO-UNDO.
    DEFINE VARIABLE v-excel-hdr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-exp-name  AS CHARACTER FORMAT "x(40)" NO-UNDO.    
    DEFINE VARIABLE v-part-no   LIKE itemfg.part-no NO-UNDO.
    DEFINE VARIABLE v-i-name    LIKE itemfg.i-name NO-UNDO.
    {sys/form/r-topw.f}

    FORM oe-rell.ord-no LABEL "Ord #"
        oe-relh.rel-date LABEL "  Date" FORMAT "99/99/99"
        oe-rell.rel-no COLUMN-LABEL "Rel" SPACE(0) "-" SPACE(0)
        oe-rell.b-ord-no COLUMN-LABEL "# " FORMAT "99"
        oe-relh.cust-no LABEL "Cust #"
        oe-relh.ship-id LABEL "Ship #"
        oe-rell.i-no LABEL "FG Item #"
        itemfg.part-no FORMAT "x(32)" LABEL "Customer Part #"
        itemfg.i-name FORMAT "x(20)" LABEL "Item Name"
        oe-rell.loc LABEL "Whse"
        oe-rell.loc-bin LABEL "Bin Loc"
        oe-rell.tag LABEL "Tag"
        oe-rell.qty LABEL "Tot Qty" FORMAT "->,>>>,>>>"
        v-s-code FORMAT "x" LABEL "C"
        oe-rell.link-no LABEL "Seq. #" FORMAT ">>>>>>>>>"
        WITH STREAM-IO WIDTH 162 DOWN FRAME rell.


    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = c-win:TITLE 
        {sys/inc/ctrtext.i str-tit2 112}

        str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAILABLE period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
        {sys/inc/ctrtext.i str-tit3 145}.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    ASSIGN
        v-export    = tb_excel
        v-excel-hdr = "Ord #,Date,Rel,#,Cust #,Ship #,FG Item #,Customer Part #,Item Name,Whse,Bin Loc,Tag,Tot Qty,C,Seq.#"
        v-exp-name  = cFileName.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(v-exp-name).
        PUT STREAM s-temp UNFORMATTED 
            v-excel-hdr                 
            SKIP.
    END. 

    DISPLAY WITH FRAME r-top.

    FOR EACH oe-relh
        WHERE oe-relh.company  EQ cocode
        AND oe-relh.posted   EQ NO
        AND (oe-relh.printed EQ YES OR NOT ip-post)
        AND oe-relh.release# GE v-frel
        AND oe-relh.release# LE v-trel
        AND oe-relh.rel-date GE v-fdat
        AND oe-relh.rel-date LE v-tdat
        AND oe-relh.cust-no  GE v-fcus
        AND oe-relh.cust-no  LE v-tcus
        /* gdm - 03110907 */
        AND (NOT v-chkflg OR
        (v-chkflg AND oe-relh.w-ord EQ NO))
        /* gdm - 03110907 end */
        AND NOT CAN-FIND(FIRST oe-rell
        WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
        AND (oe-rell.ord-no LT v-ford OR
        oe-rell.ord-no GT v-tord)
        USE-INDEX r-no)        

        USE-INDEX post NO-LOCK:  

        FOR EACH oe-rell
            WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no NO-LOCK:
            v-ship-no = LOOKUP(oe-rell.s-code,v-ship-lu).

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ oe-rell.i-no
                NO-LOCK NO-ERROR.

            ASSIGN 
                v-part-no = itemfg.part-no
                v-i-name  = itemfg.i-name.

            ASSIGN
                v-s-code   = IF oe-relh.posted AND oe-relh.deleted THEN "D"
                    ELSE oe-rell.s-code
                v-tot-post = v-tot-post + 1.

            DISPLAY oe-rell.ord-no oe-relh.rel-date oe-rell.rel-no
                oe-rell.b-ord-no oe-relh.cust-no oe-relh.ship-id
                oe-rell.i-no itemfg.part-no 
                WHEN AVAILABLE itemfg itemfg.i-name 
                WHEN AVAILABLE itemfg
                oe-rell.loc oe-rell.loc-bin oe-rell.tag oe-rell.qty
                v-s-code oe-rell.link-no
                WITH FRAME rell.
            DOWN WITH FRAME rell.

            IF v-export THEN
                PUT STREAM s-temp UNFORMATTED                 
                    '"' oe-rell.ord-no                    '",'
                    '"' oe-relh.rel-date                  '",'
                    '"' oe-rell.rel-no                    '",'
                    '"' oe-rell.b-ord-no                  '",'
                    '"' oe-relh.cust-no                   '",'
                    '"' oe-relh.ship-id                   '",'
                    '"' oe-rell.i-no                      '",'
                    '"' v-part-no                         '",'
                    '"' v-i-name                          '",'
                    '"' oe-rell.loc                       '",'
                    '"' oe-rell.loc-bin                   '",'
                    '"' oe-rell.tag                       '",'
                    '"' oe-rell.qty                       '",'
                    '"' v-s-code                          '",'
                    '"' oe-rell.link-no                   '"'
                    SKIP.                                     


        END. /* each oe-rell */
    END. /* each oe-relh */

    v-no-post = 0.
    FOR EACH w-nopost BREAK BY w-nopost.ord-no:
        IF FIRST(w-nopost.ord-no) THEN
            PUT "** Release Unable To Post To Orders. **" SKIP.

        IF v-export AND first(w-nopost.ord-no)
            THEN
            PUT STREAM s-temp UNFORMATTED
                "** Release Unable To Post To Orders. **" SKIP 
                "Ord #,Date,Rel,#,Cust #,Ship #,FG Item #,Customer Part #," + 
                "Item Name,Whse,Bin Loc,Tag,Tot Qty,C,Seq.#"
                SKIP.

        DISPLAY w-nopost.ord-no @ oe-rell.ord-no
            w-nopost.rel-date @ oe-relh.rel-date
            w-nopost.rel-no @ oe-rell.rel-no
            w-nopost.b-ord-no @ oe-rell.b-ord-no
            w-nopost.cust-no @ oe-relh.cust-no
            w-nopost.ship-id @ oe-relh.ship-id
            w-nopost.i-no @ oe-rell.i-no
            w-nopost.part-no @ itemfg.part-no
            w-nopost.i-name @ itemfg.i-name
            w-nopost.loc @ oe-rell.loc
            w-nopost.loc-bin @ oe-rell.loc-bin
            w-nopost.tag @ oe-rell.tag
            w-nopost.qty @ oe-rell.qty
            w-nopost.code @ v-s-code 
            w-nopost.link-no @ oe-rell.link-no
            WITH FRAME rell.
        DOWN WITH FRAME rell.

        IF v-export THEN
            PUT STREAM s-temp UNFORMATTED
                '"' w-nopost.ord-no   '",'      
                '"' w-nopost.rel-date '",'
                '"' w-nopost.rel-no   '",'
                '"' w-nopost.b-ord-no '",'
                '"' w-nopost.cust-no  '",'
                '"' w-nopost.ship-id  '",'
                '"' w-nopost.i-no     '",'
                '"' w-nopost.part-no  '",'
                '"' w-nopost.i-name   '",'
                '"' w-nopost.loc      '",'
                '"' w-nopost.loc-bin  '",'
                '"' w-nopost.tag      '",'
                '"' w-nopost.qty      '",'
                '"' w-nopost.code     '",'
                '"' w-nopost.link-no  '"' 
                SKIP.

        v-no-post = v-no-post + 1.
        DELETE w-nopost.
    END. /* each w-no-post */

    /* gdm 10/20/2008 */
    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp close.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(v-exp-name)).
    END.

    IF NOT ip-post THEN RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    ELSE
        RUN usr-print-one-field (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE usr-print-one-field C-Win 
PROCEDURE usr-print-one-field :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-program-id AS cha  NO-UNDO.
    DEFINE INPUT PARAMETER ip-frame-hdl AS HANDLE NO-UNDO.
    /*
    {custom/globdefs.i}
    
    {sys/inc/var.i NEW SHARED}
      */

    ASSIGN
        cocode = g_company
        locode = g_loc.  

    DEFINE VARIABLE lv-group-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE li           AS INTEGER NO-UNDO.

&SCOPED-DEFINE where-phrase                      ~
    WHERE user-print.company    EQ cocode        ~
      AND user-print.program-id EQ ip-program-id ~
      AND user-print.batch      EQ ""


    DEFINE BUFFER bf-user-print FOR user-print.
    DEFINE VARIABLE lv-update-all-batch AS LOG NO-UNDO.

    FOR EACH tt-date:
        DELETE tt-date.
    END.

    IF g_batch THEN
        FIND FIRST user-print NO-LOCK WHERE ROWID(user-print) EQ g_batch-rowid NO-ERROR.
    ELSE 
        FIND FIRST user-print NO-LOCK
    {&where-phrase} AND user-print.user-id EQ USERID("nosweat") NO-ERROR.

    IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK {&where-phrase} AND user-print.user-id EQ "" NO-ERROR.

    CREATE tt-user-print.

    IF AVAILABLE user-print THEN BUFFER-COPY user-print TO tt-user-print.

    ASSIGN
        lv-group-hdl = ip-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    li = 0.

    DO WHILE TRUE:
        li = li + 1.

        IF li GT EXTENT(tt-user-print.field-name) OR
            NOT VALID-HANDLE(lv-field-hdl)         THEN LEAVE.
        IF lv-field-hdl:NAME = "tgMultipleReleases" THEN 
        DO:

            ASSIGN
                tt-user-print.field-label[li] = lv-field-hdl:LABEL
                tt-user-print.field-name[li]  = lv-field-hdl:NAME
                tt-user-print.field-value[li] = IF lv-field-hdl:NAME BEGINS "sl" THEN lv-field-hdl:LIST-ITEMS
                                   ELSE lv-field-hdl:SCREEN-VALUE 
   NO-ERROR.     

        END.
        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.
    DO:
        FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE user-print THEN CREATE user-print.

        BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print.

        RELEASE user-print.

        FOR EACH user-print NO-LOCK
            {&where-phrase}
        AND user-print.user-id EQ "":
        FIND bf-user-print WHERE ROWID(bf-user-print) EQ ROWID(user-print)
        EXCLUSIVE NO-ERROR NO-WAIT.
        IF AVAILABLE bf-user-print THEN DELETE bf-user-print.
    END.

    CREATE user-print.

    BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print
        ASSIGN 
        user-print.user-id = "".  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
         Purpose:    
         Parameters:  <none>
         Notes:      
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
           
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeFileName C-Win 
PROCEDURE pChangeFileName :
    /*------------------------------------------------------------------------------
         Purpose:    
         Parameters:  <none>
         Notes:      
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF ip-post THEN
            ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\ReleasePostingBol.csv".
        ELSE
            ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\ReleaseEditList.csv".    
    END.

END PROCEDURE.


