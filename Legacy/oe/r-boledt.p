&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-boledt.p

  Description: BOL Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/12/2002

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

DEFINE VARIABLE ip-post   AS LOG INIT NO NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir  AS CHA NO-UNDO.

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

{oe/oe-bolp1.i NEW}

DEFINE BUFFER xfg-bin FOR fg-bin.

DEFINE VARIABLE v-back        LIKE itemfg.q-back.
DEFINE VARIABLE v-prt         AS LOG       INIT YES.
DEFINE VARIABLE v-bol-bal     LIKE oe-boll.qty.
DEFINE VARIABLE v-ref-no      AS INTEGER.
DEFINE VARIABLE v-rcpth-no    AS INTEGER.
DEFINE VARIABLE v-frst        AS LOG       INIT NO.
DEFINE VARIABLE v-ext-price   LIKE inv-line.t-price.
DEFINE VARIABLE olinecnt      AS INTEGER   INIT 0.
DEFINE VARIABLE frtcnt        AS LOG       INIT NO.
DEFINE VARIABLE v-create      AS LOG.
DEFINE VARIABLE v-bo-ch       AS LOG       NO-UNDO.
DEFINE VARIABLE v-close-qty   LIKE oe-ordl.qty.

DEFINE VARIABLE f             AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-assign-comm AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-amt     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE exist-flag    AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-comm    AS DECIMAL   EXTENT 3 INIT 0 NO-UNDO.
DEFINE VARIABLE temp-tax      AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE v-fg-qty      LIKE oe-boll.qty.
DEFINE VARIABLE v-po-no       LIKE oe-rel.po-no.
DEFINE VARIABLE v-rcpt-no     AS INTEGER.
DEFINE VARIABLE v-ship-inst   AS CHARACTER EXTENT 2.
DEFINE VARIABLE v-check-qty   AS LOG       NO-UNDO.
DEFINE VARIABLE v-fg-rctd-t   AS LOG       NO-UNDO.

DEFINE STREAM s-temp.

DEFINE TEMP-TABLE w-fg-bin NO-UNDO LIKE fg-bin.

DEFINE VARIABLE v-invalid AS LOG NO-UNDO.

FORMAT
    oe-bolh.bol-date
    SPACE(2)
    oe-bolh.bol-no   FORMAT ">>>>>>>>"
    SPACE(2)
    oe-bolh.carrier  FORMAT "x(7)"
    SPACE(2)
    oe-bolh.trailer  FORMAT "x(16)"
    SPACE(2) 
    oe-bolh.freight  FORMAT "->>>,>>9.99"
    SPACE(2) 
    oe-bolh.cwt
    SPACE(3) 
    oe-bolh.tot-wt   FORMAT "->>>,>>9"
    SPACE(2) 
    oe-bolh.cust-no
    SPACE(4)
    oe-bolh.ship-id
    SPACE(2)
    oe-bolh.deleted AT 106 FORMAT "*DELETED*/"
    SKIP(1)                                
  
    HEADER "Date           BOL.#  Carrier  Trailer               Freight    Rate     Tot WT  Cust#       Ship#   "
    "----------  --------  -------  ----------------  -----------  ------  ---------  --------    --------"

    WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE FRAME bolh.

FORMAT
    SPACE(5)
    oe-boll.i-no
    itemfg.i-name    FORMAT "x(20)"
    oe-boll.po-no
    oe-boll.ord-no
    oe-boll.rel-no   FORMAT ">>9" SPACE(0) "-" SPACE(0)
    oe-boll.b-ord-no FORMAT "99"
    oe-boll.loc
    oe-boll.loc-bin
    oe-boll.tag
    oe-boll.cases    FORMAT "->>>,>>9"
    oe-boll.qty-case FORMAT "->>>,>>9"
    oe-boll.partial  FORMAT "->>>,>>9"
    oe-boll.weight   FORMAT "->>>,>>9"

    HEADER
    SPACE(5) "Item#           Item Name            P.O. #            Ord#  Rel.# Whse. Bin Loc  Tag         Cases Qty/Case  Partial   Weight" SKIP
    SPACE(5) "--------------- -------------------- --------------- ------ ------ ----- -------- -------- -------- -------- -------- --------"
    WITH STREAM-IO WIDTH 132 DOWN NO-LABELS NO-BOX NO-UNDERLINE FRAME boll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_bolnum ~
end_bolnum begin_date end_date begin_cust end_cust rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_bolnum ~
end_bolnum begin_date end_date begin_cust end_cust rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE begin_bolnum   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning BOL#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_bolnum     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending BOL#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-boledt.csv" 
    LABEL "If Yes, File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

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
    LABEL "Transaction Date" 
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
    "To File", 3
    SIZE 21 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
    SIZE 94 BY 10.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
    SIZE 94 BY 10.24.

DEFINE VARIABLE tb_excel     AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel  AS LOGICAL INITIAL NO 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 2.43 COL 36 COLON-ALIGNED
    tran-period AT ROW 2.43 COL 64 COLON-ALIGNED
    begin_bolnum AT ROW 3.86 COL 28 COLON-ALIGNED HELP
    "Enter the beginning BOL number"
    end_bolnum AT ROW 3.86 COL 73 COLON-ALIGNED HELP
    "Enter the ending BOL number"
    begin_date AT ROW 4.81 COL 28 COLON-ALIGNED HELP
    "Enter the beginning BOL date"
    end_date AT ROW 4.81 COL 73 COLON-ALIGNED HELP
    "Enter the ending BOL date"
    begin_cust AT ROW 5.76 COL 28 COLON-ALIGNED HELP
    "Enter the beginning customer number"
    end_cust AT ROW 5.76 COL 73 COLON-ALIGNED HELP
    "Enter the ending customer number"
    rd-dest AT ROW 12.91 COL 6 NO-LABELS
    lv-ornt AT ROW 12.91 COL 30 NO-LABELS
    lines-per-page AT ROW 12.91 COL 83 COLON-ALIGNED
    lv-font-no AT ROW 15.14 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 16.24 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 17.43 COL 30
    tb_excel AT ROW 18.38 COL 30
    tb_runExcel AT ROW 18.38 COL 52
    fi_file AT ROW 19.33 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 22 COL 23
    btn-cancel AT ROW 22 COL 58
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 11.71 COL 4
    RECT-6 AT ROW 11.48 COL 1
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 95 BY 22.81.

DEFINE FRAME FRAME-E
    "The Edit List will show all available bills of lading to be" VIEW-AS TEXT
    SIZE 65 BY 1.19 AT ROW 1.24 COL 15
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "posted to all orders." VIEW-AS TEXT
    SIZE 26 BY 1.19 AT ROW 2.43 COL 15
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "Bills of Lading MUST BE printed prior to posting!" VIEW-AS TEXT
    SIZE 61 BY .95 AT ROW 3.86 COL 15
    BGCOLOR 11 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 7.19
    SIZE 94 BY 4.29
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
        TITLE              = "BOL Edit List & Posting"
        HEIGHT             = 23.05
        WIDTH              = 95.8
        MAX-HEIGHT         = 45.05
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 45.05
        VIRTUAL-WIDTH      = 256
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
                                                                        */
ASSIGN 
    begin_bolnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_bolnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* BOL Edit List  Posting */
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
ON WINDOW-CLOSE OF C-Win /* BOL Edit List  Posting */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bolnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bolnum C-Win
ON LEAVE OF begin_bolnum IN FRAME FRAME-A /* Beginning BOL# */
    DO:
        ASSIGN {&self-name}.
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning BOL Date */
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
      
        ASSIGN
            v-s-bol    = begin_bolnum
            v-e-bol    = end_bolnum
            v-s-date   = begin_date
            v-e-date   = end_date
            v-s-cust   = begin_cust
            v-e-cust   = end_cust
            v-no-post  = 0
            v-tot-post = 0
            v-tried    = NO.
       
        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.
        SESSION:SET-WAIT-STATE ("").


        IF ip-post THEN 
        DO:
            IF v-tot-post GT 0 THEN 
            DO:
                lv-post = NO.

                MESSAGE "Post BOLs?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE lv-post.

                IF lv-post THEN 
                DO:
                    RUN post-bols.

                    MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
                END.
            END.

            ELSE MESSAGE "No BOLs available for posting..." VIEW-AS ALERT-BOX ERROR.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bolnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bolnum C-Win
ON LEAVE OF end_bolnum IN FRAME FRAME-A /* Ending BOL# */
    DO:
        ASSIGN {&self-name}.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending BOL Date */
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
        DEFINE VARIABLE char-val AS cha NO-UNDO.

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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
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
DEFINE VARIABLE choice AS LOG NO-UNDO.

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
        c-win:TITLE = IF ip-post THEN "BOL Posting/Create Invoice"
                            ELSE "BOL Edit List".
  
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    v-u-inv = oe-ctrl.u-inv.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BOLPOST"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "BOLPOST"
            sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
            choice           = YES.
   
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE choice.
  
        IF NOT choice THEN sys-ctrl.char-fld EQ "Bin>Qty".
    END.
    ASSIGN
        v-check-qty = sys-ctrl.char-fld EQ "Bin>Qty"
        v-fg-rctd-t = sys-ctrl.int-fld EQ 0.
    
    RUN enable_UI.

    RUN check-date.
  
    {methods/nowait.i}

    IF NOT ip-post THEN 
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
tran-date:SCREEN-VALUE = STRING(TODAY).
APPLY "entry" TO begin_bolnum.    
DISABLE tran-date tran-period.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-nopost C-Win 
PROCEDURE create-nopost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-reason LIKE w-nopost.reason NO-UNDO.


    CREATE w-nopost.
    ASSIGN
        w-nopost.ord-no   = oe-boll.ord-no
        w-nopost.bol-date = oe-bolh.BOL-date
        w-nopost.bol-no   = oe-bolh.BOL-no
        w-nopost.rel-no   = oe-boll.REL-no
        w-nopost.b-ord-no = oe-boll.b-ord-no
        w-nopost.cust-no  = oe-bolh.cust-no
        w-nopost.po-no    = oe-boll.PO-NO
        w-nopost.reason   = ip-reason.

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
    DISPLAY tran-date tran-period begin_bolnum end_bolnum begin_date end_date 
        begin_cust end_cust rd-dest lv-ornt lines-per-page lv-font-no 
        lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date begin_bolnum end_bolnum begin_date end_date 
        begin_cust end_cust rd-dest lv-ornt lines-per-page lv-font-no 
        td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
/* BOL posting Exception Report                                               */
/* -------------------------------------------------------------------------- */

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.
 

    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = "BOL - Insufficient Inventory Report"
        {sys/inc/ctrtext.i str-tit2 112}
 
        str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAILABLE period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
        {sys/inc/ctrtext.i str-tit3 132}.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}
  
    DISPLAY WITH FRAME r-top.
  
    FOR EACH w-except,

        FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.bol-no  EQ w-except.bol-no
        NO-LOCK

        BREAK BY w-except.bol-no
        BY w-except.ord-no
        BY w-except.rel-no
        BY w-except.b-ord-no:

        IF FIRST-OF(w-except.bol-no) THEN 
        DO:
            DISPLAY oe-bolh.bol-date
                oe-bolh.bol-no
                oe-bolh.carrier
                oe-bolh.trailer
                oe-bolh.freight
                oe-bolh.cwt
                oe-bolh.tot-wt
                oe-bolh.cust-no
                oe-bolh.ship-id
                oe-bolh.deleted
                WITH FRAME bolh.
            DOWN WITH FRAME bolh.
        END.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-except.i-no
            NO-LOCK NO-ERROR.

        DISPLAY w-except.i-no       @ oe-boll.i-no
            itemfg.i-name       
            WHEN AVAILABLE itemfg
            w-except.po-no      @ oe-boll.po-no
            w-except.ord-no     @ oe-boll.ord-no
            w-except.rel-no     @ oe-boll.rel-no
            w-except.b-ord-no   @ oe-boll.b-ord-no
            w-except.loc        @ oe-boll.loc
            w-except.loc-bin    @ oe-boll.loc-bin
            w-except.tag        @ oe-boll.tag
            w-except.cases      @ oe-boll.cases
            w-except.qty-case   @ oe-boll.qty-case
            w-except.partial    @ oe-boll.partial
            w-except.weight     @ oe-boll.weight
            WITH FRAME boll.
        DOWN WITH FRAME boll.
    
        PUT SKIP(1).
    END.

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
    /*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
         DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
         DEFINE VARIABLE result AS LOGICAL NO-UNDO.
      
    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
      */
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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bols C-Win 
PROCEDURE post-bols :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-exception AS LOG NO-UNDO.


    {sa/sa-sls01.i}

    /**********************  POSTING BLOCK  ****************************/
    post-blok:
    DO TRANSACTION.
        bolh:
        FOR EACH oe-bolh
            WHERE oe-bolh.company  EQ cocode
            AND oe-bolh.posted   EQ NO
            AND oe-bolh.printed  EQ YES
            AND oe-bolh.deleted  EQ NO
            AND oe-bolh.bol-no   GE v-s-bol
            AND oe-bolh.bol-no   LE v-e-bol
            AND oe-bolh.bol-date GE v-s-date
            AND oe-bolh.bol-date LE v-e-date
            AND oe-bolh.cust-no  GE v-s-cust
            AND oe-bolh.cust-no  LE v-e-cust
            AND oe-bolh.trailer  NE "HOLD"
            AND oe-bolh.stat     EQ "R"
            AND NOT CAN-FIND(FIRST oe-boll
            WHERE oe-boll.company  EQ oe-bolh.company
            AND oe-boll.b-no     EQ oe-bolh.b-no
            AND (oe-boll.loc     EQ "" OR
            oe-boll.loc-bin EQ ""))
            USE-INDEX post,
      
            FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-bolh.cust-no
            NO-LOCK
      
            BREAK BY oe-bolh.bol-no
            BY oe-bolh.ord-no
            BY oe-bolh.rel-no.
            
            IF FIRST-OF(oe-bolh.bol-no) AND v-u-inv AND v-check-qty THEN
                FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK,

                    FIRST oe-ord
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  EQ oe-boll.ord-no
                    AND oe-ord.TYPE    NE "T"
                    NO-LOCK

                    BREAK BY oe-boll.i-no
                    BY oe-boll.job-no
                    BY oe-boll.job-no2
                    BY oe-boll.loc
                    BY oe-boll.loc-bin
                    BY oe-boll.tag:
             
                    FIND FIRST w-fg-bin
                        WHERE w-fg-bin.company EQ cocode
                        AND w-fg-bin.i-no    EQ oe-boll.i-no
                        AND w-fg-bin.job-no  EQ oe-boll.job-no
                        AND w-fg-bin.job-no2 EQ oe-boll.job-no2
                        AND w-fg-bin.loc     EQ oe-boll.loc
                        AND w-fg-bin.loc-bin EQ oe-boll.loc-bin
                        AND w-fg-bin.tag     EQ oe-boll.tag
                        NO-ERROR.
                    IF NOT AVAILABLE w-fg-bin THEN 
                    DO:
                        CREATE w-fg-bin.
                        ASSIGN
                            w-fg-bin.company = cocode
                            w-fg-bin.i-no    = oe-boll.i-no
                            w-fg-bin.job-no  = oe-boll.job-no
                            w-fg-bin.job-no2 = oe-boll.job-no2
                            w-fg-bin.loc     = oe-boll.loc
                            w-fg-bin.loc-bin = oe-boll.loc-bin
                            w-fg-bin.tag     = oe-boll.tag.
                    END.
                    w-fg-bin.qty = w-fg-bin.qty + oe-boll.qty.
              
                    IF LAST-OF(oe-boll.tag) THEN 
                    DO:
                        FIND FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                            AND fg-bin.i-no    EQ oe-boll.i-no
                            AND fg-bin.job-no  EQ oe-boll.job-no
                            AND fg-bin.job-no2 EQ oe-boll.job-no2
                            AND fg-bin.loc     EQ oe-boll.loc
                            AND fg-bin.loc-bin EQ oe-boll.loc-bin
                            AND fg-bin.tag     EQ oe-boll.tag
                            NO-LOCK NO-ERROR.
            
                        IF NOT AVAILABLE fg-bin OR fg-bin.qty LT w-fg-bin.qty THEN 
                        DO:
                            CREATE w-except.
                            BUFFER-COPY oe-boll TO w-except.
                        END.
                    END.
                END.

            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            FIND FIRST shipto
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-bolh.cust-no
                AND shipto.ship-id EQ oe-bolh.ship-id
                AND CAN-FIND(FIRST fg-bin WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ ""
                AND fg-bin.loc     EQ shipto.loc
                AND fg-bin.loc-bin EQ shipto.loc-bin)
                NO-LOCK NO-ERROR.

            olinecnt = olinecnt + 1.

            FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no,

                FIRST oe-ord
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ oe-boll.ord-no
                NO-LOCK,

                FIRST oe-ordl
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ oe-boll.ord-no
                AND oe-ordl.line    EQ oe-boll.line
                AND oe-ordl.i-no    EQ oe-boll.i-no
                USE-INDEX ord-no NO-LOCK,
        
                FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ oe-boll.i-no
                NO-LOCK:

                IF oe-ord.type EQ "T" THEN 
                DO:            /* Process in-house transfer */
                    IF AVAILABLE shipto THEN 
                    DO:

                        IF v-fg-rctd-t THEN
                        DO:
                            v-rcpt-no = 0.
            
                            FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESCENDING:
                                LEAVE.
                            END.
                            IF AVAILABLE fg-rctd THEN v-rcpt-no = fg-rctd.r-no.
            
                            FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                            IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT v-rcpt-no THEN v-rcpt-no = fg-rcpth.r-no.
                 
                            CREATE fg-rctd.
                            ASSIGN
                                fg-rctd.r-no       = v-rcpt-no + 1
                                fg-rctd.company    = cocode
                                fg-rctd.rct-date   = oe-bolh.bol-date
                                fg-rctd.trans-time = TIME
                                fg-rctd.i-no       = oe-boll.i-no
                                fg-rctd.rita-code  = "T"
                                fg-rctd.job-no     = oe-boll.job-no
                                fg-rctd.job-no2    = oe-boll.job-no2
                                fg-rctd.loc        = oe-boll.loc
                                fg-rctd.loc-bin    = oe-boll.loc-bin
                                fg-rctd.tag        = oe-boll.tag
                                fg-rctd.partial    = oe-boll.partial
                                fg-rctd.cases      = oe-boll.cases
                                fg-rctd.qty-case   = oe-boll.qty-case
                                fg-rctd.t-qty      = oe-boll.qty
                                fg-rctd.loc2       = shipto.loc
                                fg-rctd.loc-bin2   = shipto.loc-bin.
                        END.

                        FIND FIRST fg-bin         /* Make sure we have a bin to relieve */
                            WHERE fg-bin.company EQ cocode
                            AND fg-bin.i-no    EQ oe-boll.i-no
                            AND fg-bin.job-no  EQ oe-boll.job-no
                            AND fg-bin.job-no2 EQ oe-boll.job-no2
                            AND fg-bin.loc     EQ oe-boll.loc
                            AND fg-bin.loc-bin EQ oe-boll.loc-bin
                            AND fg-bin.tag     EQ oe-boll.tag
                            NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE fg-bin THEN 
                        DO:
                            CREATE fg-bin.
                            ASSIGN
                                fg-bin.company      = cocode
                                fg-bin.i-no         = oe-boll.i-no
                                fg-bin.job-no       = oe-boll.job-no
                                fg-bin.job-no2      = oe-boll.job-no2
                                fg-bin.loc          = oe-boll.loc
                                fg-bin.loc-bin      = oe-boll.loc-bin
                                fg-bin.tag          = oe-boll.tag
                                fg-bin.case-count   = oe-boll.qty-case
                                fg-bin.pur-uom      = itemfg.prod-uom
                                fg-bin.std-tot-cost = itemfg.std-tot-cost
                                fg-bin.std-mat-cost = itemfg.std-mat-cost
                                fg-bin.std-lab-cost = itemfg.std-lab-cost
                                fg-bin.std-var-cost = itemfg.std-var-cost
                                fg-bin.std-fix-cost = itemfg.std-fix-cost.
                        END.

                        FIND FIRST xfg-bin        /* Make sure we have a bin to receive */
                            WHERE xfg-bin.company EQ cocode
                            AND xfg-bin.i-no    EQ oe-boll.i-no
                            AND xfg-bin.job-no  EQ oe-boll.job-no
                            AND xfg-bin.job-no2 EQ oe-boll.job-no2
                            AND xfg-bin.loc     EQ shipto.loc
                            AND xfg-bin.loc-bin EQ shipto.loc-bin
                            AND xfg-bin.tag     EQ ""
                            NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE xfg-bin THEN 
                        DO:
                            CREATE xfg-bin.
                            ASSIGN
                                xfg-bin.company      = cocode
                                xfg-bin.i-no         = oe-boll.i-no
                                xfg-bin.job-no       = oe-boll.job-no
                                xfg-bin.job-no2      = oe-boll.job-no2
                                xfg-bin.loc          = shipto.loc
                                xfg-bin.loc-bin      = shipto.loc-bin
                                xfg-bin.case-count   = oe-boll.qty-case
                                xfg-bin.pur-uom      = fg-bin.pur-uom
                                xfg-bin.std-tot-cost = fg-bin.std-tot-cost
                                xfg-bin.std-mat-cost = fg-bin.std-mat-cost
                                xfg-bin.std-lab-cost = fg-bin.std-lab-cost
                                xfg-bin.std-var-cost = fg-bin.std-var-cost
                                xfg-bin.std-fix-cost = fg-bin.std-fix-cost.
                        END.
          
                        IF AVAILABLE fg-rctd THEN
                        DO:
                            ASSIGN 
                                fg-rctd.cost-uom = itemfg.prod-uom /* 29642 - IU2 Transfers with incorrect cost - MYT - 08/29/18 */
                                fg-rctd.pur-uom  = fg-bin.pur-uom
                                fg-rctd.std-cost = fg-bin.std-tot-cost.
            
                            IF fg-rctd.pur-uom EQ "EA" THEN
                                fg-rctd.ext-cost = fg-rctd.std-cost.
                            ELSE
                                RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                                    fg-rctd.std-cost, OUTPUT fg-rctd.ext-cost).
            
                            fg-rctd.ext-cost = fg-rctd.ext-cost * fg-rctd.t-qty.
                        END.

                        ASSIGN
                            oe-bolh.posted = YES
                            oe-boll.posted = YES.
                    END.  
                END.
      
                ELSE 
                DO:
                {oe/seq-bolh.i}
                END.
            END.      
        END. /* for each oe-bolh */
  
        FOR EACH w-fg-bin:
            DELETE w-fg-bin.
        END.
  
        RUN oe/oe-bolp3.p (v-term).
  
        HIDE FRAME post NO-PAUSE.
    END. /* post-blok*/

    delete-blok:
    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.deleted  EQ YES
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no  LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted:

        FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:
            DELETE oe-boll.
        END. /* each oe-boll */
  
        DELETE oe-bolh.
    END. /* each oe-bolh */

    FIND FIRST w-except NO-ERROR.
    IF AVAILABLE w-except THEN 
    DO:
        lv-exception = YES.
        MESSAGE "  Bill(s) of Lading have been found that do not have  "     SKIP
            "  sufficient inventory for posting to be completed.   "     SKIP
            "  Do you wish to print the exception report?          "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-exception.

        IF lv-exception THEN 
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
    /* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
    /* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-export    AS LOGICAL.
    DEFINE VARIABLE v-excel-hdr AS CHARACTER.
    DEFINE VARIABLE v-exp-name  AS CHARACTER FORMAT "x(40)" INIT "c:\tmp\r-boledt.csv".
{sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.
 
    FOR EACH w-fg-bin:
        DELETE w-fg-bin.
    END.

    FIND FIRST period WHERE 
        period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2    = c-win:TITLE
    {sys/inc/ctrtext.i str-tit2 112}
        str-tit3    = "Period " + STRING(tran-period,"99") + " - " +
        IF AVAILABLE period THEN
            (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
    {sys/inc/ctrtext.i str-tit3 132}
        v-export    = tb_excel
        v-excel-hdr = "Date,BOL.#,Carrier,Trailer,Freight,Rate,Tot WT,Cust#,Ship#,Deleted,Item#,Item Name,P.O. #,Ord#,Rel.#,Whse.,Bin Loc,Tag,Cases,Qty/Case,Partial,Weight"
        v-exp-name  = fi_file.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    SESSION:SET-WAIT-STATE ("general").
  
    DISPLAY WITH FRAME r-top.

    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(v-exp-name).
        PUT STREAM s-temp UNFORMATTED 
            v-excel-hdr                 
            SKIP.
    END.  
  
    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH oe-bolh WHERE 
        oe-bolh.company  EQ cocode
        AND oe-bolh.posted   EQ NO
        AND (oe-bolh.printed EQ YES OR NOT ip-post)
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX post NO-LOCK
        BREAK BY oe-bolh.bol-no:
    
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no.

        IF FIRST-OF(oe-bolh.bol-no) THEN
            FOR EACH oe-boll WHERE 
                oe-boll.company EQ oe-bolh.company 
                AND oe-boll.b-no    EQ oe-bolh.b-no NO-LOCK,

                FIRST oe-ord WHERE
                oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ oe-boll.ord-no
                AND oe-ord.TYPE    NE "T" NO-LOCK
                BREAK BY oe-boll.i-no
                BY oe-boll.job-no
                BY oe-boll.job-no2
                BY oe-boll.loc
                BY oe-boll.loc-bin
                BY oe-boll.tag:
             
                FIND FIRST w-fg-bin WHERE 
                    w-fg-bin.company EQ cocode
                    AND w-fg-bin.i-no    EQ oe-boll.i-no
                    AND w-fg-bin.job-no  EQ oe-boll.job-no
                    AND w-fg-bin.job-no2 EQ oe-boll.job-no2
                    AND w-fg-bin.loc     EQ oe-boll.loc
                    AND w-fg-bin.loc-bin EQ oe-boll.loc-bin
                    AND w-fg-bin.tag     EQ oe-boll.tag NO-ERROR.
                IF NOT AVAILABLE w-fg-bin THEN 
                DO:
                    CREATE w-fg-bin.
                    ASSIGN
                        w-fg-bin.company = cocode
                        w-fg-bin.i-no    = oe-boll.i-no
                        w-fg-bin.job-no  = oe-boll.job-no
                        w-fg-bin.job-no2 = oe-boll.job-no2
                        w-fg-bin.loc     = oe-boll.loc
                        w-fg-bin.loc-bin = oe-boll.loc-bin
                        w-fg-bin.tag     = oe-boll.tag.
                END.
                w-fg-bin.qty = w-fg-bin.qty + oe-boll.qty.
            END.
    END.

    FOR EACH oe-bolh WHERE 
        oe-bolh.company  EQ cocode
        AND oe-bolh.deleted  EQ YES
        AND oe-bolh.posted   EQ YES
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted NO-LOCK:

        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no.
    END.

    MAINBLOK:
    FOR EACH w-bolh BY w-bolh.bol-no BY w-bolh.ord-no
        BY w-bolh.rel-no BY w-bolh.b-ord-no:
        FIND oe-bolh WHERE RECID(oe-bolh) = w-bolh.w-recid NO-LOCK.

        v-tot-post = v-tot-post + 1.

        FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no
            NO-LOCK
            BREAK BY oe-boll.company
            BY oe-boll.b-no
            BY oe-boll.ord-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no:

            RELEASE oe-ord.
            RELEASE oe-ordl.

            IF NOT oe-bolh.deleted THEN 
            DO:
                FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company AND
                    oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN 
                DO:
                    RUN create-nopost ("Order Was Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST oe-ordl WHERE oe-ordl.company = cocode  AND
                    oe-ordl.ord-no = oe-boll.ord-no  AND
                    oe-ordl.line   = oe-boll.line NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    RUN create-nopost ("Order Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST oe-rell WHERE oe-rell.company = oe-boll.company AND
                    oe-rell.r-no = oe-boll.r-no AND
                    oe-rell.i-no = oe-boll.i-no AND
                    oe-rell.line = oe-boll.line
                    USE-INDEX r-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN 
                DO:
                    RUN create-nopost ("Release Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST itemfg WHERE itemfg.company = cocode AND
                    itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO:
                    RUN create-nopost ("Finish Good Item Was Not Found").
                    NEXT mainblok.
                END.
            
                IF oe-boll.loc EQ "" OR oe-boll.loc-bin EQ "" THEN 
                DO:
                    RUN create-nopost ("Warehouse or Bin is Blank").
                    NEXT mainblok.
                END.
            END.

            IF FIRST-OF(oe-boll.b-no) THEN 
            DO:
                DISPLAY oe-bolh.BOL-date
                    oe-bolh.BOL-no 
                    oe-bolh.CARRIER 
                    oe-bolh.TRAILER 
                    oe-bolh.FREIGHT
                    oe-bolh.CWT
                    oe-bolh.TOT-WT
                    oe-bolh.cust-no
                    oe-bolh.ship-id
                    oe-bolh.deleted
                    WITH FRAME bolh.
                DOWN WITH FRAME bolh.

                IF v-export THEN
                    PUT STREAM s-temp UNFORMATTED
                        '"' oe-bolh.BOL-date  '",'
                        '"' oe-bolh.BOL-no    '",'
                        '"' oe-bolh.CARRIER   '",'
                        '"' oe-bolh.TRAILER   '",'
                        '"' oe-bolh.FREIGHT   '",'
                        '"' oe-bolh.CWT       '",'
                        '"' oe-bolh.TOT-WT    '",'
                        '"' oe-bolh.cust-no   '",'
                        '"' oe-bolh.ship-id   '",'
                        '"' oe-bolh.deleted   '",'
                        SKIP .
            END.
      
      

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ oe-boll.i-no
                AND fg-bin.job-no  EQ oe-boll.job-no
                AND fg-bin.job-no2 EQ oe-boll.job-no2
                AND fg-bin.loc     EQ oe-boll.loc
                AND fg-bin.loc-bin EQ oe-boll.loc-bin
                AND fg-bin.tag     EQ oe-boll.tag
                NO-LOCK NO-ERROR.

            FIND FIRST w-fg-bin
                WHERE w-fg-bin.company EQ cocode
                AND w-fg-bin.i-no    EQ oe-boll.i-no
                AND w-fg-bin.job-no  EQ oe-boll.job-no
                AND w-fg-bin.job-no2 EQ oe-boll.job-no2
                AND w-fg-bin.loc     EQ oe-boll.loc
                AND w-fg-bin.loc-bin EQ oe-boll.loc-bin
                AND w-fg-bin.tag     EQ oe-boll.tag
                NO-ERROR.

            DISPLAY oe-boll.i-no
                itemfg.i-name 
                WHEN AVAILABLE itemfg
                oe-boll.po-no
                oe-boll.ord-no
                oe-boll.rel-no
                oe-boll.b-ord-no 
                oe-boll.loc
                oe-boll.loc-bin
                oe-boll.tag
                oe-boll.CASES
                oe-boll.qty-CASE
                oe-boll.PARTIAL
                oe-boll.WEIGHT
                WITH FRAME boll.
            DOWN WITH FRAME boll.
      
            IF v-export THEN
                PUT STREAM s-temp UNFORMATTED
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"",'
                    '"' oe-boll.i-no      '",'
                    '"' itemfg.i-name     '",'
                    '"' oe-boll.po-no     '",'
                    '"' oe-boll.ord-no    '",'
                    '"' STRING(oe-boll.rel-no,">>>9") + "-" +
                    string(oe-boll.b-ord-no,"99")  '",'
                    '"' oe-boll.loc       '",'
                    '"' oe-boll.loc-bin   '",'
                    '"' oe-boll.tag       '",'
                    '"' oe-boll.CASES     '",'
                    '"' oe-boll.qty-CASE  '",'
                    '"' oe-boll.PARTIAL   '",'
                    '"' oe-boll.WEIGHT    '",'
                    SKIP.

            IF AVAILABLE oe-ord                                 AND
                AVAILABLE oe-ordl                                AND
                oe-ordl.ship-qty + oe-boll.qty GT
                oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN 
            DO:
                PUT SPACE(10)
                    "*** Qty Shipped will exceed Qty Ordered + Allowable Overrun"
                    SKIP.
                IF v-export THEN
                    PUT STREAM s-temp UNFORMATTED
                        '"*** Qty Shipped will exceed Qty Ordered + Allowable Overrun"'
                        SKIP.
            END.
            IF oe-boll.qty GT 0 AND oe-bolh.posted EQ NO         AND
                (NOT AVAILABLE fg-bin OR
                (AVAILABLE w-fg-bin AND fg-bin.qty LT w-fg-bin.qty)) THEN 
            DO:
                PUT SPACE(10)
                    "** Insufficient Quantity, Bill of Lading quantity exceeds the quantity in the bin location **"
                    SKIP.
                IF v-export THEN
                    PUT STREAM s-temp UNFORMATTED
                        '"** Insufficient Quantity; Bill of Lading quantity exceeds the quantity in the bin location **"'
                        SKIP.
            END.
        END. /* each oe-boll */

        PUT SKIP(1).
    END. /* each oe-bolh */

    v-no-post = 0.

    FOR EACH w-nopost BREAK BY w-nopost.bol-no:
        IF FIRST(w-nopost.bol-no) THEN
            PUT SKIP(1)
                "** Bills Of Lading Unable To Be Posted. **" SKIP.
        IF v-export THEN
            PUT STREAM s-temp UNFORMATTED
                "                                          " SKIP(1)
                "** Bills Of Lading Unable To Be Posted. **" SKIP
                "BOL.#,Date,Order#,Rel#-BO#,Cust.#,PO#,Item,Name,Reason"
                SKIP.
        DISPLAY w-nopost.bol-no     COLUMN-LABEL "BOL.#"
            w-nopost.bol-date   COLUMN-LABEL "Date"
            w-nopost.ord-no     COLUMN-LABEL "Order#"
            STRING(w-nopost.rel-no,">>>9") + "-" +
            string(w-nopost.b-ord-no,"99")
            COLUMN-LABEL "Rel#-BO#"    FORMAT "x(7)"
            w-nopost.cust-no    COLUMN-LABEL "Cust.#"
            w-nopost.po-no      COLUMN-LABEL "PO#"
            w-nopost.i-no       COLUMN-LABEL "Item"
            w-nopost.i-name     COLUMN-LABEL "Name"         FORMAT "x(20)"
            w-nopost.reason     COLUMN-LABEL "Reason"       SKIP
            WITH DOWN STREAM-IO WIDTH 132 FRAME nopost2.
        DOWN WITH FRAME nopost2.
    
        PUT STREAM s-temp UNFORMATTED
            '"' w-nopost.bol-no               '",'    
            '"' w-nopost.bol-date             '",'
            '"' w-nopost.ord-no               '",'
            '"' STRING(w-nopost.rel-no,">>>9") + "-" +
            STRING(w-nopost.b-ord-no,"99")        '",'
            '"' w-nopost.cust-no              '",'   
            '"' w-nopost.po-no                '",'
            '"' w-nopost.i-no                 '",'
            '"' w-nopost.i-name               '",'
            '"' w-nopost.reason               '",'
            .

        v-no-post = v-no-post + 1.

        DELETE w-nopost.
    END.

    /* rtc 08/11/2008 */
    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp close.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-exp-name)).
    END.

    IF NOT ip-post THEN RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.
  
    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.
  
    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
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

