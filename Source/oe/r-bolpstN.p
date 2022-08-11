&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-bole&p.w

  Description: BOL Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/12/2002

  Mod: Ticket - 103137 (Format Change for Order No. and Job No).
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

DEFINE VARIABLE ip-post    AS LOG     INIT YES NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name  AS cha     NO-UNDO.
DEFINE VARIABLE init-dir   AS CHA     NO-UNDO.
DEFINE VARIABLE lSingleBOL AS LOGICAL NO-UNDO.

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

DEFINE VARIABLE v-back           LIKE itemfg.q-back.
DEFINE VARIABLE v-prt            AS LOG       INIT YES.
DEFINE VARIABLE v-bol-bal        LIKE oe-boll.qty.
DEFINE VARIABLE v-ref-no         AS INTEGER.
DEFINE VARIABLE v-rcpth-no       AS INTEGER.
DEFINE VARIABLE v-frst           AS LOG       INIT NO.
DEFINE VARIABLE v-ext-price      LIKE inv-line.t-price.
DEFINE VARIABLE olinecnt         AS INTEGER   INIT 0.
DEFINE VARIABLE frtcnt           AS LOG       INIT NO.
DEFINE VARIABLE v-create         AS LOG.
DEFINE VARIABLE v-bo-ch          AS LOG       NO-UNDO.
DEFINE VARIABLE v-close-qty      LIKE oe-ordl.qty.

DEFINE VARIABLE f                AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-assign-comm    AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-amt        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE exist-flag       AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-comm       AS DECIMAL   EXTENT 3 INIT 0 NO-UNDO.
DEFINE VARIABLE temp-tax         AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE v-fg-qty         LIKE oe-boll.qty.
DEFINE VARIABLE v-po-no          LIKE oe-rel.po-no.
DEFINE VARIABLE v-ship-inst      AS CHARACTER EXTENT 2.
DEFINE VARIABLE v-check-qty      AS LOG       NO-UNDO.
DEFINE VARIABLE dis-tag          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vtag             AS INTEGER   NO-UNDO.
DEFINE VARIABLE vtag2            AS INTEGER   NO-UNDO.
DEFINE VARIABLE td-full-tag      AS LOG       NO-UNDO.

DEFINE VARIABLE v-rtn-char       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found      AS LOG       NO-UNDO.
DEFINE VARIABLE invstatus-char   AS CHARACTER NO-UNDO.
DEFINE VARIABLE invstatus-log    AS LOG       NO-UNDO.
DEFINE VARIABLE v-invalid        AS LOG       NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.

RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

DEFINE VARIABLE cdAOABOLPost AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldAOABOLPost AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (
    g_company, "dAOABOLPost", "L", NO, NO, "", "",
    OUTPUT cdAOABOLPost, OUTPUT ldAOABOLPost
    ).

FORMAT
    oe-bolh.bol-date
    SPACE(2)
    oe-bolh.bol-no   FORMAT ">>>>>>>>"
    SPACE(2)
    oe-bolh.carrier
    SPACE(4)
    oe-bolh.trailer
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
    oe-bolh.deleted AT 106 FORMAT "*DELETED*/ "
    SKIP(1)
  
    HEADER "Date           BOL.#  Carrier  Trailer                   Freight    Rate     Tot WT  Cust#       Ship#   "
    "----------  --------  -------  --------------------  -----------  ------  ---------  --------    --------"

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
    SPACE(5) "Item#           Item Name            P.O. #              Ord#  Rel.# Whse. Bin Loc  Tag         Cases Qty/Case  Partial   Weight" SKIP
    SPACE(5) "--------------- -------------------- --------------- -------- ------ ----- -------- -------- -------- -------- -------- --------"
    WITH STREAM-IO WIDTH 134 DOWN NO-LABELS NO-BOX NO-UNDERLINE FRAME boll.

{oe/closchk.i NEW}

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no   LIKE oe-boll.bol-no
    FIELD ord-no   LIKE oe-boll.ord-no
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .


DEFINE STREAM st-email.

DEFINE STREAM ediBOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 tran-date begin_bolnum ~
begin_date end_date begin_cust end_cust rd-dest tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period tran-time ~
begin_bolnum begin_date end_date begin_cust end_cust rd-dest tbAutoClose 

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

DEFINE VARIABLE tran-time      AS CHARACTER FORMAT "x(20)":U 
    LABEL "Time" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1 NO-UNDO.

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
    SIZE 15 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 10.19.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.52.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 2.43 COL 29 COLON-ALIGNED
    tran-period AT ROW 2.43 COL 53 COLON-ALIGNED
    tran-time AT ROW 2.43 COL 74 COLON-ALIGNED WIDGET-ID 2
    begin_bolnum AT ROW 4.1 COL 29 COLON-ALIGNED HELP
    "Enter the beginning BOL number"
    end_bolnum AT ROW 4.1 COL 74 COLON-ALIGNED HELP
    "Enter the ending BOL number"
    begin_date AT ROW 5.05 COL 29 COLON-ALIGNED HELP
    "Enter the beginning BOL date"
    end_date AT ROW 5.05 COL 74 COLON-ALIGNED HELP
    "Enter the ending BOL date"
    begin_cust AT ROW 6 COL 29 COLON-ALIGNED HELP
    "Enter the beginning customer number"
    end_cust AT ROW 6 COL 74 COLON-ALIGNED HELP
    "Enter the ending customer number"
    lv-font-no AT ROW 12.43 COL 33 COLON-ALIGNED
    lv-ornt AT ROW 12.43 COL 42.6 NO-LABELS
    lines-per-page AT ROW 12.48 COL 86.6 COLON-ALIGNED
    rd-dest AT ROW 12.67 COL 4.8 NO-LABELS
    lv-font-name AT ROW 13.57 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 15.57 COL 30.6
    tbAutoClose AT ROW 16.76 COL 30.6 WIDGET-ID 64
    btn-ok AT ROW 17.71 COL 30.4
    btn-cancel AT ROW 17.71 COL 54.2
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 11.76 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    RECT-7 AT ROW 1.52 COL 3
    RECT-8 AT ROW 12.19 COL 3 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95 BY 18.29
    BGCOLOR 15 .

DEFINE FRAME FRAME-E
    "posted to all orders." VIEW-AS TEXT
    SIZE 26 BY 1.19 AT ROW 2.43 COL 15
    BGCOLOR 8 FONT 5
    "Bills of Lading MUST BE printed prior to posting!" VIEW-AS TEXT
    SIZE 61 BY .95 AT ROW 3.86 COL 15
    BGCOLOR 8 FONT 5
    "The Edit List will show all available bills of lading to be" VIEW-AS TEXT
    SIZE 65 BY 1.19 AT ROW 1.24 COL 15
    BGCOLOR 8 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 3 ROW 7.43
    SIZE 91 BY 4.29
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
        TITLE              = "BOL Edit List & Posting"
        HEIGHT             = 18.29
        WIDTH              = 95
        MAX-HEIGHT         = 21.86
        MAX-WIDTH          = 96.4
        VIRTUAL-HEIGHT     = 21.86
        VIRTUAL-WIDTH      = 96.4
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
    begin_bolnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_bolnum IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_bolnum:HIDDEN IN FRAME FRAME-A       = TRUE
    end_bolnum:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tran-time IN FRAME FRAME-A
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
        IF VALID-HANDLE(hdInventoryProcs) THEN 
            DELETE PROCEDURE hdInventoryProcs.
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
        DEFINE VARIABLE lv-post      AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lv-exception AS LOGICAL NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
    
        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.
  
        RUN pCheckPeriod  .
        IF v-invalid THEN RETURN NO-APPLY.    

        IF MONTH(tran-date) NE MONTH(TODAY) OR
            YEAR(tran-date)  NE YEAR(TODAY)THEN 
        DO:
            MESSAGE "The BOL posting date is not in the current month - " SKIP 
                " Are you sure you want to post using this date ?" 
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL  UPDATE lcheckflg AS LOGICAL .
            IF NOT lcheckflg THEN 
            DO:
                APPLY "entry" TO tran-date IN FRAME {&FRAME-NAME} .
                RETURN NO-APPLY .
            END.
        END.

        IF invstatus-char EQ "One Bol Only" THEN
            ASSIGN END_bolnum              = begin_bolnum
                END_bolnum:SCREEN-VALUE = begin_bolnum:SCREEN-VALUE.  
        ELSE
            ASSIGN END_bolnum.
    
        IF begin_bolnum EQ end_bolnum THEN
            lSingleBOL = TRUE.
  
        ASSIGN
            rd-dest
            tran-period
            v-s-bol    = begin_bolnum
            v-e-bol    = end_bolnum
            v-s-date   = begin_date
            v-e-date   = end_date
            v-s-cust   = begin_cust
            v-e-cust   = end_cust
            v-no-post  = 0
            v-tot-post = 0
            v-tried    = NO.

        FOR EACH w-ord:
            DELETE w-ord.
        END.
        FOR EACH tt-fg-bin:
            DELETE tt-fg-bin.
        END.

        EMPTY TEMP-TABLE tt-email.
        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.
  
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
                    WHEN 1 THEN 
                    RUN output-to-printer.
                    WHEN 2 THEN  
                    RUN output-to-screen.
                    WHEN 3 THEN 
                    RUN output-to-file.
                END CASE.
            END.
            FOR EACH w-except:
                DELETE w-except.
            END.    
        END.
 
        IF ip-post THEN 
        DO:
            IF CAN-FIND(FIRST w-bolh) THEN 
            DO:
                lv-post = NO.

                MESSAGE "Post BOLs?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE lv-post.

                IF lv-post THEN 
                DO:
                    IF ldAOABOLPost AND cdAOABOLPost EQ "YES" THEN
                        RUN pdAOABOLPost.
                    ELSE 
                    DO:
                        RUN post-bols.
       
                        /* close transfer order here */
                        RUN oe/closchk.p (0).

    
                        /* WFk- 5/4/12- This is here to make sure it is the last thing in */
                        /* the posting process.  Posting relies on a cleanup routine */
                        /* for releases instead of fixing the real problem.          */
                        FOR EACH w-bolh,
                            FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid NO-LOCK:
    
                            FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
                         
                                FIND FIRST oe-ordl NO-LOCK
                                    WHERE oe-ordl.company EQ oe-boll.company
                                    AND oe-ordl.ord-no EQ oe-boll.ord-no
                                    AND oe-ordl.line EQ oe-boll.line NO-ERROR.
                                RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).    
                            END.
                        END.
       
                        FOR EACH w-ord:
                            RUN oe/close.p (w-ord.rec-id, YES).  
                        END.

                        FIND FIRST tt-email NO-LOCK NO-ERROR.
                        IF AVAILABLE tt-email THEN RUN email-reorderitems.
                    END. /* else */ 
                    MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
                END.
            END.

            ELSE MESSAGE "No BOLs available for posting..." VIEW-AS ALERT-BOX ERROR.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME tran-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-time C-Win
ON LEAVE OF tran-time IN FRAME FRAME-A /* Time */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    
PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEFINE INPUT PARAMETER mailTo AS CHARACTER.
    DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT PARAMETER mailText AS CHARACTER.
    DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
    DEFINE INPUT PARAMETER mailDialog AS LONG.
    DEFINE OUTPUT PARAMETER retCode AS LONG.
END.

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
        AND sys-ctrl.name    EQ "BolPostTime"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "BolPostTime"
            sys-ctrl.descrip  = "Parameter to post Time of Invoice Creation"
            sys-ctrl.log-fld  = YES
            sys-ctrl.char-fld = "BolCreation" .
    END.
    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time"  THEN
        ASSIGN  tran-time = STRING(int(SUBSTRING(STRING(sys-ctrl.dec-fld),1,2)) * 60 * 60 + int(SUBSTRING(STRING(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
    ELSE ASSIGN tran-time = STRING(TIME,"hh:mm:ss") .
      
    RELEASE sys-ctrl.

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
  
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", NO, NO, "", "", 
        OUTPUT v-rtn-char, OUTPUT v-rec-found).
    invstatus-log = LOGICAL(v-rtn-char).
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", NO, NO, "", "", 
        OUTPUT invstatus-char, OUTPUT v-rec-found).

    v-check-qty = sys-ctrl.char-fld EQ "Bin>Qty".
    DO TRANSACTION:
        {sys/inc/fgreorder.i}
    END.

    IF InvStatus-char NE "One BOL Only" THEN
        ASSIGN END_bolnum:HIDDEN    = NO
            END_bolnum:SENSITIVE = YES.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    RUN check-date.
  
    IF NOT ip-post THEN 
        ASSIGN tran-date:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
            tran-period:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  
  
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        ASSIGN tran-time:SCREEN-VALUE = tran-time.
        APPLY "entry" TO begin_bolnum .
    END.    
  
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{AOA/includes/pInitDynParamValue.i}
{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pSetDynParamValue.i "dyn"}

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


    FIND FIRST itemfg
        WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK NO-ERROR.

    CREATE w-nopost.
    ASSIGN
        w-nopost.ord-no   = oe-boll.ord-no
        w-nopost.i-no     = oe-boll.i-no
        w-nopost.i-name   = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "Not on File"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reorder C-Win 
PROCEDURE create-reorder :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-oeboll-rowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-oeboll FOR oe-boll.

    DEFINE VARIABLE v-qty-onh   AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-avail AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-reord-qty AS INTEGER NO-UNDO.

    FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
    FIND itemfg WHERE itemfg.company = cocode AND
        itemfg.i-no = bf-oeboll.i-no NO-LOCK.

    v-qty-onh = 0.
    FOR EACH fg-bin
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        /*AND fg-bin.loc     GE begin_whse
        AND fg-bin.loc     LE end_whse*/ NO-LOCK:
        v-qty-onh = v-qty-onh + fg-bin.qty.
    END.

    ASSIGN
        v-qty-avail = v-qty-onh /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.
 
    IF itemfg.ord-level GT v-qty-avail THEN 
    DO:
        v-reord-qty = itemfg.ord-level - v-qty-avail.

        IF v-reord-qty LT itemfg.ord-min AND
            itemfg.ord-min NE 0 THEN 
            v-reord-qty = itemfg.ord-min.

        IF v-reord-qty GT itemfg.ord-max AND
            itemfg.ord-max NE 0 THEN 
            v-reord-qty = itemfg.ord-max.
    END.
    ELSE v-reord-qty = 0.


    IF v-reord-qty > 0 THEN 
    DO:
     
        CREATE tt-email.
        ASSIGN 
            tt-email.bol-no  = bf-oeboll.bol-no
            tt-email.ord-no  = bf-oeboll.ord-no
            tt-email.i-no    = bf-oeboll.i-no
            tt-email.qty     = v-reord-qty
            tt-email.cust-no = oe-bolh.cust-no
            .
                                    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-reorderitems C-Win 
PROCEDURE email-reorderitems :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*DEF INPUT PARAM ip-fgemail-file AS cha .*/

    DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-to-list     AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailto      AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailsubject AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailbody    AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailattach  AS cha       NO-UNDO.
    DEFINE VARIABLE v-fgemail-file AS cha       NO-UNDO.
    DEFINE VARIABLE v-dir          AS CHARACTER FORMAT "X(80)" NO-UNDO.
    DEFINE VARIABLE v-qty-onh      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty-avail    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty-alloc    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty-onOrder  AS INTEGER   NO-UNDO.

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.
    IF AVAILABLE users AND users.user_program[2] NE "" THEN v-dir = users.user_program[2] + "\".
    ELSE v-dir = "c:\tmp\".

    FOR EACH tt-email,
        FIRST cust NO-LOCK WHERE cust.company = cocode
        AND cust.cust-no = tt-email.cust-no
        AND cust.active = "E" BREAK BY tt-email.cust-no BY tt-email.i-no:
        IF FIRST-OF(tt-email.cust-no) THEN 
        DO:
            v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
            OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
            PUT STREAM st-email "***** Reorder Point Item from BOL Posting *****" SKIP
                "BOL#     Order#       FG Item#      ReOrder Qty     Avail Qty  On Hand Qty On Order Qty" SKIP
                "======== ========== =============== ============ ============ ============ ============" SKIP.
        END.
        IF FIRST-OF(tt-email.i-no) THEN 
        DO:
            /*v-qty-onh = 0.
            FOR EACH fg-bin WHERE fg-bin.company EQ cocode
                           AND fg-bin.i-no    EQ tt-email.i-no
                   /*AND fg-bin.loc     GE begin_whse
                   AND fg-bin.loc     LE end_whse*/  NO-LOCK:
                v-qty-onh = v-qty-onh + fg-bin.qty.
           END.
           */
            FIND itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = tt-email.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN ASSIGN v-qty-onh     = itemfg.q-onh
                    v-qty-onOrder = itemfg.q-ono
                    v-qty-alloc   = itemfg.q-alloc.
            ELSE ASSIGN v-qty-onh     = 0
                    v-qty-onOrder = 0
                    v-qty-alloc   = 0.
            {sys/inc/oereordr.i}

            v-qty-avail = v-qty-onh +
                (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE v-qty-onOrder) -
                v-qty-alloc.

        END.
       
        PUT STREAM st-email UNFORMATTED
            STRING(tt-email.bol-no) FORM "x(9)"
            STRING(tt-email.ord-no) FORM "x(10)"
            " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
            " " v-qty-avail  FORM "->>>,>>>,>>9"
            " " v-qty-onh FORM "->>>,>>>,>>9"
            " " v-qty-onOrder FORM "->>>,>>>,>>9"
            SKIP.
        IF LAST-OF(tt-email.cust-no) THEN 
        DO:
            OUTPUT STREAM st-email CLOSE.
            {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}

            IF ls-to-list NE '' THEN 
            DO:
                ASSIGN 
                    lv-mailto      = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailbody    = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailattach  = v-fgemail-file.
                RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
            END.
        END. /* last-of(tt-email.cust-no) */
    END.

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
    DISPLAY tran-date tran-period tran-time begin_bolnum begin_date end_date 
        begin_cust end_cust rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 RECT-8 tran-date begin_bolnum begin_date end_date begin_cust 
        end_cust rd-dest tbAutoClose btn-ok btn-cancel 
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

    ASSIGN 
        td-full-tag = YES.
    MESSAGE " Do you wish to print full tag value?  "  
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
        UPDATE td-full-tag .

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
            DISPLAY oe-bolh.bol-date FORMAT "99/99/9999" COLUMN-LABEL "Date"
                oe-bolh.bol-no FORMAT ">>>>>>>>" COLUMN-LABEL "   BOL #"
                oe-bolh.carrier FORMAT "X(5)" COLUMN-LABEL "Carrier"
                oe-bolh.trailer FORMAT "X(20)" COLUMN-LABEL "Trailer"
                oe-bolh.freight FORMAT "->>>,>>9.99" COLUMN-LABEL "    Freight"
                oe-bolh.cwt     COLUMN-LABEL "  Rate"
                oe-bolh.tot-wt  FORMAT "->>>,>>9" COLUMN-LABEL "   Tot WT"
                oe-bolh.cust-no COLUMN-LABEL "Cust#"
                oe-bolh.ship-id COLUMN-LABEL "Ship#"
                oe-bolh.deleted FORMAT "*DELETED*/ " COLUMN-LABEL "Deleted"
                SKIP(1)
                WITH FRAME bolh2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 150.
            DOWN WITH FRAME bolh2.
        END.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-except.i-no
            NO-LOCK NO-ERROR.

        vtag = 0.
        vtag2 = 0.
        vtag = LENGTH(w-except.tag).
        vtag2 = vtag - 5 .

        IF NOT td-full-tag AND vtag <> 0 THEN ASSIGN  dis-tag = SUBSTR(w-except.tag,vtag2,6) .
        ELSE ASSIGN dis-tag = w-except.tag .

        DISPLAY SPACE(5)
            w-except.i-no  COLUMN-LABEL "Item #"   
            dis-tag COLUMN-LABEL "Tag" FORMAT "X(22)"
            itemfg.i-name  FORMAT "X(20)" 
            WHEN AVAILABLE itemfg COLUMN-LABEL "Item Name"
            w-except.po-no COLUMN-LABEL "P.O. #"    
            w-except.ord-no COLUMN-LABEL "  Ord#"   
            STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") COLUMN-LABEL "Rel.#"    
            w-except.loc COLUMN-LABEL "Whse."
            w-except.loc-bin COLUMN-LABEL "Bin Loc"   
            
            w-except.cases FORMAT "->>>,>>9"   COLUMN-LABEL "   Cases"
            w-except.qty-case FORMAT "->>>,>>9" COLUMN-LABEL "Qty/Case" 
            w-except.partial FORMAT "->>>,>>9"  COLUMN-LABEL " Partial"
            w-except.weight FORMAT "->>>,>>9"   COLUMN-LABEL "  Weight"
            WITH FRAME boll2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 165.
        DOWN WITH FRAME boll2.
    
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
    /*   DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
         DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
         DEFINE VARIABLE result AS LOGICAL NO-UNDO.
      
         SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    
       /* Use Progress Print. Always use Font#9 in Registry (set above) */
       RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                 INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
       /* use-dialog(1) and landscape(2) */ */

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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckPeriod C-Win 
PROCEDURE pCheckPeriod :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/     
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.
  
        FIND FIRST period NO-LOCK
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-ERROR.
        IF AVAILABLE period THEN 
        DO:     
            IF begin_date LT period.pst OR end_date GT period.pend THEN
            DO:
                MESSAGE "The BOL posting date period is different from bol date " SKIP 
                    "Please enter same period for posting the bol " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.          
            END.      
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pdAOABOLPost C-Win 
PROCEDURE pdAOABOLPost :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cParamList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBOLPost    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable      AS HANDLE    NO-UNDO.

    /* subject parameter names listed alphabetically */
    ASSIGN
        cParamList  = "allBOL|"
                    + "allCustNo|"
                    + "allLocBin|"
                    + "allLocs|"
                    + "company|"
                    + "custList|"
                    + "DatePickList-1|"
                    + "DatePickList-2|"
                    + "DatePickList-3|"
                    + "endBOL|"
                    + "endBOLDate|"
                    + "endCustName|"
                    + "endCustNo|"
                    + "endLoc|"
                    + "endLocBin|"
                    + "endLocDescription|"
                    + "location|"
                    + "post|"
                    + "postDate|"
                    + "startBOL|"
                    + "startBOLDate|"
                    + "startCustName|"
                    + "startCustNo|"
                    + "startLoc|"
                    + "startLocBin|"
                    + "startLocDescription"
        /* subject parameter values listed alphabetically */
        cParamValue = "yes|" // allBOL
                    + "yes|" // allCustNo
                    + "yes|" // allLocBin
                    + "yes|" // allLocs
                    + g_company + "|" // company
                    + "no|" // custList
                    + "Fixed Date|" // DatePickList-1
                    + "Fixed Date|" // DatePickList-2
                    + "Fixed Date|" // DatePickList-3
                    + STRING(end_bolnum) + "|" // endBOL
                    + STRING(end_date,"99/99/9999") + "|" // endBOLDate
                    + "<End Range Value>|" // endCustName
                    + end_cust + "|" // endCustNo
                    + CHR(254) + "|" // endLoc
                    + CHR(254) + "|" // endLocBin
                    + "<End Range Value>|" // endLocDescription
                    + g_loc + "|" // location
                    + "yes|" // post
                    + STRING(tran-date,"99/99/9999") + "|" // postDate
                    + STRING(begin_bolnum) + "|" // startBOL
                    + STRING(begin_date,"99/99/9999") + "|" // startBOLDate
                    + "<Start Range Value>|" // startCustName
                    + begin_cust + "|" // startCustNo
                    + "|" // startLoc
                    + "|" // startLocBin
                    + "<Start Range Value>" // startLocDescription
        .
    RUN pInitDynParamValue (19, "", "", 0, cParamList, cParamValue).
    RUN AOA/dynBL/r-bolpst.p PERSISTENT SET hBOLPost.
    RUN pRunBusinessLogic IN hBOLPost (ROWID(dynParamValue), OUTPUT hTable).
    DELETE PROCEDURE hBOLPost.

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
    DEFINE VARIABLE d-out      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lr-rel-lib AS HANDLE  NO-UNDO.

    {sa/sa-sls01.i}

    DISABLE TRIGGERS FOR LOAD OF itemfg.

    FOR EACH w-bolh,
        FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
        BY oe-bolh.bol-no:
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ 'EDIBOLPost' NO-ERROR.

    /**********************  POSTING BLOCK  ****************************/
    post-blok:
    DO TRANSACTION.
        bolh:
        /*for each oe-bolh
            where oe-bolh.company  eq cocode
              and oe-bolh.posted   eq no
              and oe-bolh.printed  eq YES
              AND oe-bolh.deleted  EQ NO
              and oe-bolh.bol-no   ge v-s-bol
              and oe-bolh.bol-no   le v-e-bol
              and oe-bolh.bol-date ge v-s-date
              and oe-bolh.bol-date le v-e-date
              AND oe-bolh.cust-no  GE v-s-cust
              AND oe-bolh.cust-no  LE v-e-cust
              and oe-bolh.trailer  ne "HOLD"
              and oe-bolh.stat     eq "R"
              and not can-find(first oe-boll
                               where oe-boll.company  eq oe-bolh.company
                                 and oe-boll.b-no     eq oe-bolh.b-no
                                 and (oe-boll.loc     eq "" or
                                      oe-boll.loc-bin eq "" or
                                      oe-boll.qty     eq 0))
            use-index post,*/
        FOR EACH w-bolh,
            FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid,

            FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-bolh.cust-no
            NO-LOCK
      
            BREAK BY oe-bolh.bol-no
            BY oe-bolh.ord-no
            BY oe-bolh.rel-no.
            
            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            olinecnt = olinecnt + 1.

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company EQ sys-ctrl.company
                    AND sys-ctrl-shipto.name EQ sys-ctrl.name
                    AND sys-ctrl-shipto.cust-vend EQ YES
                    AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
                    AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto THEN
                    OUTPUT STREAM ediBOL TO VALUE(sys-ctrl.char-fld + '/' +
                        sys-ctrl-shipto.cust-vend-no + '/' +
                        'bol' + STRING(TODAY,'99999999')) APPEND.
            END. /* avail sys-ctrl */

            RUN-PROC = "sbo/oerel-recalc-act.p".
            RUN VALUE(run-proc) PERSISTENT SET phandle NO-ERROR.
            lr-rel-lib = phandle.

            FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
                EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ oe-boll.company
                AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:

                FOR EACH oe-rel 
                    WHERE oe-rel.company EQ oe-ordl.company
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.line    EQ oe-ordl.line
                    AND oe-rel.stat = "P"
                    AND oe-rel.link-no GT 0 
                    AND oe-rel.rel-no GT 0:

                    /* Set actual quantity */
                    IF AVAILABLE oe-rel AND VALID-HANDLE(lr-rel-lib) THEN 
                        RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).

                END.
            END.

            IF VALID-HANDLE(lr-rel-lib) THEN
                DELETE OBJECT lr-rel-lib.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                AND oe-boll.b-no    EQ oe-bolh.b-no:
                RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term, YES /* show msg */).

                IF fgreorder-log AND cust.ACTIVE EQ "E" THEN
                    RUN create-reorder (ROWID(oe-boll)).


                IF AVAILABLE sys-ctrl-shipto THEN 
                DO:
                    FIND FIRST oe-ordl NO-LOCK
                        WHERE oe-ordl.company EQ oe-boll.company
                        AND oe-ordl.ord-no EQ oe-boll.ord-no
                        AND oe-ordl.line EQ oe-boll.line NO-ERROR.
                    EXPORT STREAM ediBOL DELIMITER '~t'
                        oe-bolh.bol-date
                        oe-bolh.bol-no
                        oe-bolh.carrier
                        oe-bolh.trailer
                        oe-bolh.freight
                        oe-bolh.cwt
                        oe-bolh.tot-wt
                        oe-bolh.cust-no
                        oe-bolh.ship-id
                        oe-boll.i-no
                        oe-boll.po-no
                        oe-boll.ord-no
                        oe-boll.rel-no
                        oe-boll.loc
                        oe-boll.loc-bin
                        oe-boll.tag
                        oe-boll.cases
                        oe-boll.qty-case
                        oe-boll.partial
                        oe-boll.weight
                        oe-ordl.part-no 
                        WHEN AVAIL(oe-ordl).
                END. /* avail sys-ctrl-shipto */
 
            END. /* each oe-boll */

            IF AVAILABLE sys-ctrl-shipto THEN
                OUTPUT STREAM ediBOL CLOSE.

        END. /* for each oe-bolh */

        /*/* check all items shipped to complete for order type "T" */
        DEF BUFFER bf-word FOR w-ord.
        DEF VAR v-close AS LOG NO-UNDO.
        FOR EACH w-ord BREAK BY w-ord.ord-no:
            IF FIRST-OF(w-ord.ord-no) THEN DO:
               FIND FIRST oe-ord WHERE oe-ord.company = cocode AND
                                       oe-ord.ord-no = w-ord.ord-no NO-LOCK NO-ERROR.
               v-close = YES.
               FOR EACH oe-ordl OF oe-ord NO-LOCK:
                   IF NOT CAN-FIND(FIRST bf-word WHERE bf-word.rec-id = RECID(oe-ordl))
                      THEN v-close = NO.
               END.
               IF NOT v-close THEN
                  FOR EACH bf-word WHERE bf-word.ord-no = w-ord.ord-no:
                      DELETE bf-word.
                  END.
            END.
        END.*/

        FOR EACH tt-fg-bin:
            DELETE tt-fg-bin.
        END.

        RUN oe/oe-bolp3.p (
            INPUT v-term,
            INPUT tran-date
            ).


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

        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no:
            DELETE oe-boll.
        END. /* each oe-boll */
  
        DELETE oe-bolh.
    END. /* each oe-bolh */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
    /* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
    /* -------------------------------------------------------------------------- */

    DEFINE BUFFER b-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    DEFINE VARIABLE lValidBin AS LOGICAL NO-UNDO.

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.
 

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
        {sys/inc/ctrtext.i str-tit3 132}.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.
  
    DISPLAY WITH FRAME r-top.
  
    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH w-nopost:
        DELETE w-nopost.
    END.

    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.posted   EQ NO
        AND (oe-bolh.printed EQ YES OR NOT ip-post)
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        USE-INDEX post NO-LOCK:

        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no.
    END.

    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.deleted  EQ YES
        AND oe-bolh.posted   EQ YES
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
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

    FOR EACH w-bolh,
        FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
        BY oe-bolh.bol-no:
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    MAINBLOK:
    FOR EACH w-bolh BY w-bolh.bol-no BY w-bolh.ord-no
        BY w-bolh.rel-no BY w-bolh.b-ord-no:
        FIND oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid NO-LOCK NO-ERROR.
    
        IF NOT AVAILABLE oe-bolh THEN
            NEXT MAINBLOK.
        
        IF v-u-inv AND v-check-qty THEN
            RUN oe/bolcheck.p(
                INPUT ROWID(oe-bolh)
                ).
              
        v-tot-post = v-tot-post + 1.

        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            NO-LOCK
            BREAK BY oe-boll.company
            BY oe-boll.b-no
            BY oe-boll.ord-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no:

            RELEASE oe-ord.
            RELEASE oe-ordl.
            IF oe-bolh.trailer EQ "HOLD"  OR  oe-bolh.stat EQ "H" THEN 
            DO:
                IF lSingleBOL THEN
                    MESSAGE "BOL " + STRING(w-bolh.bol-no) + " is on HOLD Status"
                        VIEW-AS ALERT-BOX ERROR.    
                ELSE 
                    RUN create-nopost(
                        INPUT "BOL is on Hold Status"
                        ).
           
                DELETE w-bolh.
                NEXT mainblok.           
            END.  
      
            FIND FIRST w-except NO-LOCK 
                WHERE w-except.bol-no EQ oe-bolh.bol-no 
                NO-ERROR. 
            IF AVAILABLE w-except THEN 
            DO:
                IF lSingleBOL THEN 
                    MESSAGE "BOL # " +  STRING(w-bolh.bol-no) + " cannot be processed because there is not enough inventory to be shipped." SKIP
                        "Correct actual inventory available, select different tags or reduce the shipped quantity as your settings" SKIP
                        "Do not allow this condition to be processed."
                        VIEW-AS ALERT-BOX.  
                ELSE 
                    RUN create-nopost(
                        INPUT "Not Enough Quantity Available to be shipped"
                        ).   
                DELETE w-bolh. 
                NEXT MAINBLOK.                                                  
            END. 
     
            IF NOT oe-bolh.deleted THEN 
            DO:
                FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company AND
                    oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN 
                DO:
                  
                    IF lSingleBOL THEN
                        MESSAGE "Order Not Found for Bol " + STRING(w-bolh.bol-no)
                            VIEW-AS ALERT-BOX ERROR.                     
                    ELSE 
                        RUN create-nopost(
                            INPUT "Order Was Not Found"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.
                END.
        
                /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
                FIND cust 
                    WHERE cust.company EQ oe-bolh.company
                    AND cust.cust-no EQ oe-bolh.cust-no 
                    NO-LOCK NO-ERROR.
               
                IF AVAILABLE cust AND oe-boll.s-code EQ "T" THEN 
                DO:
                    RUN oe/custxship.p(
                        INPUT oe-bolh.company,
                        INPUT oe-bolh.cust-no,
                        INPUT oe-bolh.ship-id,
                        BUFFER shipto
                        ).
                    IF AVAILABLE shipto THEN 
                    DO:  
                        IF oe-boll.loc EQ shipto.loc THEN 
                        DO:   
                            IF lSingleBOL THEN     
                                MESSAGE "BOL" STRING(oe-bolh.bol-no) "Cannot Transfer to the Same Location" oe-boll.loc 
                                    VIEW-AS ALERT-BOX ERROR.
                            ELSE 
                                RUN create-nopost(
                                    INPUT "Cannot transfer to the same location"
                                    ).
                            DELETE w-bolh.
                            NEXT mainblok.
                        END.
                        RUN ValidateBin IN hdInventoryProcs(
                            INPUT cocode, 
                            INPUT shipto.loc,
                            INPUT shipto.loc-bin, 
                            OUTPUT lValidBin
                            ).
                        IF NOT lValidBin THEN 
                        DO:  
                            IF lSingleBOL THEN 
                                MESSAGE "Ship To warehouse/bin location does not exist for BOL# " STRING(oe-bolh.bol-no)
                                    VIEW-AS ALERT-BOX ERROR.
                            ELSE 
                                RUN create-nopost(
                                    INPUT "Ship To warehouse/bin location does not exist"
                                    ).
                            DELETE w-bolh.
                            NEXT mainblok.
                        END.                                                           
                    END.    
                END.
                FIND FIRST oe-ordl WHERE oe-ordl.company = oe-boll.company  AND
                    oe-ordl.ord-no = oe-boll.ord-no  AND
                    oe-ordl.line   = oe-boll.line NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    IF lSingleBOL THEN
                        MESSAGE "Order Lines Were Not Found for BOL " + STRING(w-bolh.bol-no)
                            VIEW-AS ALERT-BOX ERROR .  
                    ELSE 
                        RUN create-nopost(
                            INPUT "Order Lines Were Not Found"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.
                END.
                RUN oe/custxship.p (oe-bolh.company,
                    oe-bolh.cust-no,
                    oe-bolh.ship-id,
                    BUFFER shipto).
          
                IF NOT AVAILABLE shipto THEN 
                DO:
                    IF lSingleBOL THEN 
                        MESSAGE "Invalid Shipto Address or Shipto does not exists for BOL " + STRING(w-bolh.bol-no)
                            VIEW-AS ALERT-BOX ERROR.  
                    ELSE 
                        RUN create-nopost(
                            INPUT "Invalid Shipto Address"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.            
                END. 
        
                FIND FIRST oe-rell WHERE oe-rell.company = oe-boll.company AND
                    oe-rell.r-no = oe-boll.r-no AND
                    oe-rell.i-no = oe-boll.i-no AND
                    oe-rell.line = oe-boll.line
                    USE-INDEX r-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN 
                DO:
                    IF lSingleBOL THEN
                        MESSAGE "Release Lines  Not Found For BOL " + STRING(w-bolh.bol-no)
                            VIEW-AS ALERT-BOX ERROR. 
                   
                    ELSE 
                        RUN create-nopost(
                            INPUT "Release Lines Were Not Found"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.
                END.

                FIND FIRST itemfg WHERE itemfg.company = oe-boll.company AND
                    itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO:
                    IF lSingleBOL THEN
                        MESSAGE "Finish Good Item Not Found For BOL " + STRING(w-bolh.bol-no) 
                            VIEW-AS ALERT-BOX ERROR. 
                   
                    ELSE 
                        RUN create-nopost(
                            INPUT "Finish Good Item Not Found"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.
                END.
            
                IF oe-boll.loc EQ "" OR oe-boll.loc-bin EQ "" THEN 
                DO:
                    IF lSingleBOL THEN
                        MESSAGE "Warehouse or Bin is Blank for BOL " + STRING(w-bolh.bol-no) 
                            VIEW-AS ALERT-BOX ERROR.
                  
                    ELSE
                        RUN create-nopost(
                            INPUT "Warehouse or Bin is Blank"
                            ).
          
                    DELETE w-bolh.
                    NEXT mainblok.
                END.

                IF NOT CAN-FIND(FIRST b-oe-boll
                    WHERE b-oe-boll.company EQ oe-bolh.company
                    AND b-oe-boll.b-no    EQ oe-bolh.b-no
                    AND b-oe-boll.qty     NE 0)
                    THEN 
                DO:
                    IF lSingleBOL THEN
                        MESSAGE "Quantity is zero for all lines for BOL# " + STRING(w-bolh.bol-no) 
                            VIEW-AS ALERT-BOX ERROR.  
                    ELSE 
                        RUN create-nopost(
                            INPUT "Quantity is zero for all lines"
                            ).
          
                    DELETE w-bolh.
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
            END.

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
                SUBSTR(oe-boll.tag,16,8) 
                WHEN SUBSTR(oe-boll.tag,1,15) EQ oe-boll.i-no
                @ oe-boll.tag
                oe-boll.CASES
                oe-boll.qty-CASE
                oe-boll.PARTIAL
                oe-boll.WEIGHT
                WITH FRAME boll.
            DOWN WITH FRAME boll.

            IF AVAILABLE oe-ord                                 AND
                AVAILABLE oe-ordl                                AND
                oe-ordl.ship-qty + oe-boll.qty GT
                oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
                PUT SPACE(10)
                    "*** Qty Shipped will exceed Qty Ordered + Allowable Overrun"
                    SKIP.
        END. /* each oe-boll */

        PUT SKIP(1).
    END. /* each oe-bolh */

    v-no-post = 0.

    FOR EACH w-nopost BREAK BY w-nopost.bol-no:
        IF FIRST(w-nopost.bol-no) THEN
            PUT SKIP(1)
                "** Bills Of Lading Unable To Be Posted. **" SKIP.

        DISPLAY w-nopost.bol-no     COLUMN-LABEL "BOL.#"
            w-nopost.bol-date   COLUMN-LABEL "Date"
            w-nopost.ord-no     COLUMN-LABEL "Order#"
            STRING(w-nopost.rel-no,">>>9") + "-" +
            string(w-nopost.b-ord-no,"99")
            COLUMN-LABEL "Rel#-BO#"     FORMAT "X(7)"
            w-nopost.cust-no    COLUMN-LABEL "Cust.#"
            w-nopost.po-no      COLUMN-LABEL "PO#"
            w-nopost.i-no       COLUMN-LABEL "Item"
            w-nopost.i-name     COLUMN-LABEL "Name"         FORMAT  "X(20)"
            w-nopost.reason     COLUMN-LABEL "Reason"       FORMAT  "X(50)" SKIP
            WITH DOWN STREAM-IO WIDTH 180 FRAME nopost2.
        DOWN WITH FRAME nopost2.

        v-no-post = v-no-post + 1.

        DELETE w-nopost.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
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

