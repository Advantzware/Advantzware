&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:               jcrep/r-schtck.w

  Description:        Schedule Card Boards Selector

  Input Parameters:   <none>

  Output Parameters:  <none>

  Author:             Dennis G. Dizon

  Created:            Mar 30, 2007

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/*  Create an unnamed pool to store all the widgets created 
    by this procedure. This is a good default which assures
    that this procedure's triggers and internal procedures 
    will execute in this procedure's storage, and that proper
    cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */    

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Shared Variables */
DEFINE NEW SHARED VARIABLE lv-qty                 AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty                    AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-fgimage          AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-mstandard        AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-shipto           AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-sellprc          AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-run-speed            AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-committed-board-only AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-set-header       AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-ship-split       AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-label            AS LOG       NO-UNDO.

/* Variables */
DEFINE NEW SHARED VARIABLE list-name              AS CHARACTER NO-UNDO.
DEFINE            VARIABLE init-dir               AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-format-f            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-format-c            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-int-f               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-int-c               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE is-xprint-form         AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-save-spec           AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-pdf-file            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-dir                  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-print-fmt            AS CHARACTER NO-UNDO .

/* Includes */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{jcrep/r-ticket.i "new shared"}

{custom/xprint.i}

{sys/inc/var.i new shared}

/* {cerep/jc-keyst.i   "NEW"} */
/* {cerep/jc-keys2.i   "NEW"} */
/* {cecrep/jc-prem.i   "NEW"} */
/* {cecrep/jc-fibre.i  "NEW"} */
/* {cecrep/tt-artios.i "NEW"} */

/* FIND FIRST users                                               */
/*     WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR. */
/* IF AVAIL users AND users.user_program[2] NE "" THEN            */
/*    v-dir = users.user_program[2] + "\".                        */
/* ELSE                                                           */
/*    v-dir = "c:\tmp\".                                          */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 end_ord-no begin_ord-no ~
begin_job1 begin_job2 end_job1 end_job2 begin_mach end_mach end_form ~
begin_form begin_blank end_blank tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS end_ord-no begin_ord-no begin_job1 ~
begin_job2 end_job1 end_job2 begin_mach end_mach end_form begin_form ~
begin_blank end_blank tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_job1 begin_job2 end_job1 end_job2 begin_mach ~
end_mach end_form begin_form begin_blank end_blank tb_reprint tb_box ~
tb_fgimage tb_prt-mch tb_prt-shipto tb_prt-sellprc tb_prt-label ~
td-show-parm 

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

DEFINE VARIABLE begin_blank    AS INTEGER   FORMAT ">>9" INITIAL 0 
    LABEL "From Blank" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE begin_form     AS INTEGER   FORMAT ">>9" INITIAL 0 
    LABEL "From Form" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE begin_job1     AS CHARACTER FORMAT "x(9)" 
    LABEL "From Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE begin_job2     AS INTEGER   FORMAT ">>9" INITIAL 0 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1.

DEFINE VARIABLE begin_mach     AS CHARACTER FORMAT "x(6)" 
    LABEL "From Machine" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE begin_ord-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "From Order#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE end_blank      AS INTEGER   FORMAT ">>9" INITIAL 99 
    LABEL "to Blank" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE end_form       AS INTEGER   FORMAT ">>9" INITIAL 99 
    LABEL "to Form" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE end_job1       AS CHARACTER FORMAT "x(9)" INITIAL "zzzzzzzzz" 
    LABEL "to Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE end_job2       AS INTEGER   FORMAT ">>9" INITIAL 999 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1.

DEFINE VARIABLE end_mach       AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz" 
    LABEL "to Machine" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE end_ord-no     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "to Order#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1.

DEFINE VARIABLE fi_speed1      AS CHARACTER FORMAT "X(256)":U INITIAL "Print Machine's Speed or Run Hour ?" 
    VIEW-AS FILL-IN 
    SIZE 37.2 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE spec_codes     AS CHARACTER FORMAT "X(256)":U INITIAL "QA" 
    LABEL "Spec Codes" 
    VIEW-AS FILL-IN 
    SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 18 BY 1.67 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To File", 3
    SIZE 16.6 BY 4.43 NO-UNDO.

DEFINE VARIABLE rd_print-speed AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Speed", "S",
    "Run Hour", "H"
    SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 14.52.

DEFINE VARIABLE tbAutoClose       AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_box            AS LOGICAL INITIAL YES 
    LABEL "Print Box Design?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_committed      AS LOGICAL INITIAL NO 
    LABEL "Print Only Committed Board?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_corr           AS LOGICAL INITIAL NO 
    LABEL "Corrugated" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fgimage        AS LOGICAL INITIAL NO 
    LABEL "Print FG Item Image?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fold           AS LOGICAL INITIAL NO 
    LABEL "Folding Carton" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prompt-ship    AS LOGICAL INITIAL NO 
    LABEL "Prompt Split Shipment or Split Order?" 
    VIEW-AS TOGGLE-BOX
    SIZE 39.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-label      AS LOGICAL INITIAL NO 
    LABEL "Print Label Info?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-mch        AS LOGICAL INITIAL NO 
    LABEL "Print Machine Standard?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-sellprc    AS LOGICAL INITIAL NO 
    LABEL "Print Sell Price in place of UPC#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-set-header AS LOGICAL INITIAL NO 
    LABEL "Print Set Unitization Page?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-shipto     AS LOGICAL INITIAL NO 
    LABEL "Print Shipto?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint        AS LOGICAL INITIAL NO 
    LABEL "Reprint Tickets?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm      AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    end_ord-no AT ROW 2.48 COL 65 COLON-ALIGNED HELP
    "Enter Ending Order Number" WIDGET-ID 14
    begin_ord-no AT ROW 2.52 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Order Number" WIDGET-ID 12
    begin_job1 AT ROW 3.62 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job2 AT ROW 3.62 COL 39 COLON-ALIGNED HELP
    "Enter Beginning Run#"
    end_job1 AT ROW 3.62 COL 65 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job2 AT ROW 3.62 COL 81 COLON-ALIGNED HELP
    "Enter Ending Run#"
    begin_mach AT ROW 4.76 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Machine Number"
    end_mach AT ROW 4.76 COL 65 COLON-ALIGNED HELP
    "Enter Ending Machine Number"
    end_form AT ROW 5.91 COL 65 COLON-ALIGNED HELP
    "Enter Ending Form Number"
    begin_form AT ROW 5.95 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Form Number"
    begin_blank AT ROW 7.1 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Blank Number"
    end_blank AT ROW 7.1 COL 65 COLON-ALIGNED HELP
    "Enter Ending Blank Number"
    tb_reprint AT ROW 7.91 COL 25.2
    tb_fold AT ROW 7.91 COL 67.2
    tb_box AT ROW 8.86 COL 46.2 RIGHT-ALIGNED
    tb_corr AT ROW 8.86 COL 67.2
    tb_fgimage AT ROW 9.81 COL 51.2 RIGHT-ALIGNED
    tb_prt-mch AT ROW 9.81 COL 67.2
    spec_codes AT ROW 11 COL 23 COLON-ALIGNED
    fi_speed1 AT ROW 12.19 COL 10 COLON-ALIGNED NO-LABELS
    rd_print-speed AT ROW 12.19 COL 52.4 NO-LABELS
    tb_prt-shipto AT ROW 13.38 COL 12.8
    tb_prt-sellprc AT ROW 13.38 COL 52.8
    tb_prt-label AT ROW 14.33 COL 12.8
    tb_committed AT ROW 14.33 COL 52.8
    tb_prt-set-header AT ROW 15.29 COL 12.8
    tb_prompt-ship AT ROW 15.29 COL 52.8
    lv-ornt AT ROW 16.95 COL 56 NO-LABELS
    lines-per-page AT ROW 16.95 COL 89 COLON-ALIGNED
    lv-font-no AT ROW 17.19 COL 45 COLON-ALIGNED
    rd-dest AT ROW 17.29 COL 5.4 NO-LABELS
    lv-font-name AT ROW 18.86 COL 43 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 20.76 COL 31.4
    tbAutoClose AT ROW 22 COL 31.4 WIDGET-ID 64
    btn-ok AT ROW 23.14 COL 31
    btn-cancel AT ROW 23.14 COL 52
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.48 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 16.48 COL 4
    RECT-6 AT ROW 16.71 COL 3
    RECT-7 AT ROW 1.95 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 97.6 BY 23.62
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
        TITLE              = "Schedule Board Cards"
        HEIGHT             = 23.62
        WIDTH              = 97.6
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
/* SETTINGS FOR FILL-IN begin_blank IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN begin_form IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN begin_job1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_job1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_job2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_mach IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_blank IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_form IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_job1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_job1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_job2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_mach IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_speed1 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    fi_speed1:HIDDEN IN FRAME FRAME-A = TRUE.

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

/* SETTINGS FOR RADIO-SET rd-dest IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd-dest:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET rd_print-speed IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_print-speed:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN spec_codes IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    spec_codes:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_box IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
    tb_box:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_box:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_committed IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_committed:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_corr IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_corr:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_fgimage IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
    tb_fgimage:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_fgimage:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_fold IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_fold:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_prompt-ship IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_prompt-ship:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_prt-label IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    tb_prt-label:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_prt-label:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-mch IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    tb_prt-mch:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_prt-mch:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-sellprc IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    tb_prt-sellprc:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_prt-sellprc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-set-header IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_prt-set-header:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_prt-shipto IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    tb_prt-shipto:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_prt-shipto:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_reprint IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    tb_reprint:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_reprint:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Schedule Board Cards */
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
ON WINDOW-CLOSE OF C-Win /* Schedule Board Cards */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON RETURN OF FRAME FRAME-A
    ANYWHERE
    DO:

        IF SELF:TYPE <> "Button" THEN  
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.
        ELSE 
        DO:
            APPLY "choose" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 C-Win
ON LEAVE OF begin_job1 IN FRAME FRAME-A /* From Job# */
    DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
    DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* From Machine */
    DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* From Order# */
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
        DEFINE VARIABLE hold-title AS CHARACTER NO-UNDO.


        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        RUN run-report.

        /*   IF tb_fold THEN DO: */
        /*lines-per-page = IF lv-format-f EQ "HOP" THEN 64 ELSE 58. */

        /*     RUN run-report ("Fold"). */

        /*     c-win:TITLE = "Folding Carton " + TRIM(c-win:TITLE). */

        /*     case rd-dest:                                                                   */
        /*        when 1 then run output-to-printer.                                           */
        /*        when 2 then run output-to-screen.                                            */
        /*        when 3 then run output-to-file.                                              */
        /*        when 4 then do:                                                              */
        /*            /*run output-to-fax.*/                                                   */
        /*            {custom/asifax.i &begin_cust=begin_job1                                  */
        /*                             &END_cust=END_job1                                      */
        /*                             &fax-subject=c-win:title                                */
        /*                             &fax-body="c-win title"                                 */
        /*                             &fax-file=list-name }                                   */
        /*        END.                                                                         */
        /*        when 5 then do:                                                              */
        /*            IF is-xprint-form THEN DO:                                               */
        /*               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22"). */
        /*               {custom/asimail.i &TYPE = "CUSTOMER"                                  */
        /*                              &begin_cust= begin_job1                                */
        /*                              &END_cust=end_job1                                     */
        /*                              &mail-subject="Factory Ticket"                         */
        /*                              &mail-body="Factory Ticket"                            */
        /*                              &mail-file=lv-pdf-file + ".pdf" }                      */
        /*            END.                                                                     */
        /*            ELSE DO:                                                                 */
        /*                {custom/asimailr.i &TYPE = ''                                        */
        /*                                   &begin_cust= begin_job1                           */
        /*                                   &END_cust=end_job1                                */
        /*                                   &mail-subject=c-win:title                         */
        /*                                   &mail-body=c-win:title                            */
        /*                                   &mail-file=list-name }                            */
        /*                                                                                     */
        /*            END.                                                                     */
        /*                                                                                     */
        /*        END.                                                                         */
        /*        WHEN 6 THEN run output-to-port.                                              */
        /*   end case.                                                                         */
        /*                                                                                     */
        /*     c-win:TITLE = hold-title.                                                       */
        /*   END.                                                                              */
        /*                                                                                     */
        /*   IF tb_corr THEN DO:                                                               */
        /*     /*lines-per-page = 0. ??? */                                                    */
        /*                                                                                     */
        /*     RUN run-report ("Corr").                                                        */
        /*                                                                                     */
        /*     c-win:TITLE = "Corrugated " + TRIM(c-win:TITLE).                                */
        /*                                                                                     */
        /*     case rd-dest:                                                                   */
        /*        when 1 then run output-to-printer.                                           */
        /*        when 2 then run output-to-screen.                                            */
        /*        when 3 then run output-to-file.                                              */
        /*        when 4 then run output-to-fax.                                               */
        /*        when 5 then run output-to-mail.                                              */
        /*        when 6 then run output-to-port.                                              */
        /*   end case.                                                                         */
        /*       c-win:TITLE = hold-title.                                                     */
        /*   END. */
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 C-Win
ON LEAVE OF end_job1 IN FRAME FRAME-A /* to Job# */
    DO:
        IF {&self-name}:MODIFIED THEN 
        DO:
            IF {&self-name}:screen-value < begin_job1:screen-value THEN
            DO:
                MESSAGE 'Ending Job cannot be less than Beginning Job.'
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.

            ELSE 
            DO:
                RUN new-job-no.
                ASSIGN {&self-name}.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
    DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* to Machine */
    DO:
        IF {&self-name}:MODIFIED THEN 
        DO:
            IF {&self-name}:screen-value < begin_mach:screen-value THEN
            DO:
                MESSAGE 'Ending Machine cannot be less than Beginning Machine.'
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.

            ELSE 
            DO:
                RUN new-job-no.
                ASSIGN {&self-name}.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* to Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_speed1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_speed1 C-Win
ON LEAVE OF fi_speed1 IN FRAME FRAME-A
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
        lines-per-page = IF SELF:SCREEN-VALUE = "L" THEN 48 ELSE 99.
        DISPLAY lines-per-page WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME spec_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_codes C-Win
ON LEAVE OF spec_codes IN FRAME FRAME-A /* Spec Codes */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_box C-Win
ON VALUE-CHANGED OF tb_box IN FRAME FRAME-A /* Print Box Design? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
    DO:
        ASSIGN {&self-name}.
    /*
      IF {&self-name} THEN spec_codes:HIDDEN = NO.
      ELSE
      IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
    */  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fgimage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fgimage C-Win
ON VALUE-CHANGED OF tb_fgimage IN FRAME FRAME-A /* Print FG Item Image? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding Carton */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-label C-Win
ON VALUE-CHANGED OF tb_prt-label IN FRAME FRAME-A /* Print Label Info? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-mch C-Win
ON VALUE-CHANGED OF tb_prt-mch IN FRAME FRAME-A /* Print Machine Standard? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-sellprc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-sellprc C-Win
ON VALUE-CHANGED OF tb_prt-sellprc IN FRAME FRAME-A /* Print Sell Price in place of UPC#? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-shipto C-Win
ON VALUE-CHANGED OF tb_prt-shipto IN FRAME FRAME-A /* Print Shipto? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Tickets? */
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
ASSIGN CURRENT-WINDOW                 = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}
       current-window:height          = 10
       btn-ok:row                     = 9.3
       btn-cancel:row                 = 9.3
       tbAutoClose:row                = 8.3
       cocode                         = gcompany
       locode                         = gloc.

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
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "SCHDCARD" NO-ERROR.
    IF AVAILABLE sys-ctrl THEN 
    DO:
        IF sys-ctrl.char-fld EQ "Indiana"
            THEN ASSIGN is-xprint-form = NO.
        ELSE ASSIGN is-xprint-form = YES.
        ASSIGN 
            v-print-fmt = sys-ctrl.char-fld .
    END.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OU11" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:

        {custom/usrprint.i}

        IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld NE "ScheduleCard1" THEN 
        DO:
            ASSIGN 
                begin_ord-no:HIDDEN = YES
                end_ord-no:HIDDEN   = YES .
            APPLY "entry" TO begin_job1 .
        END.
        ELSE 
        DO: 
            APPLY "entry" TO begin_ord-no .
        END.
    
    END.

    /*   RUN new-job-no. */

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
    DISPLAY end_ord-no begin_ord-no begin_job1 begin_job2 end_job1 end_job2 
        begin_mach end_mach end_form begin_form begin_blank end_blank 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 end_ord-no begin_ord-no begin_job1 begin_job2 end_job1 
        end_job2 begin_mach end_mach end_form begin_form begin_blank end_blank 
        tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no C-Win 
PROCEDURE new-job-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-fold AS LOG NO-UNDO.
    DEFINE VARIABLE ll-corr AS LOG NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN set-job-vars.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "CEMENU"
            NO-LOCK NO-ERROR.

        ASSIGN
            ll-fold = NOT AVAILABLE sys-ctrl           OR
               sys-ctrl.char-fld EQ "Both"  OR
               sys-ctrl.char-fld EQ "Foldware"
            ll-corr = AVAILABLE sys-ctrl AND
               (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").

        IF ll-fold AND ll-corr THEN
            FOR EACH job-hdr
                WHERE job-hdr.company               EQ cocode                   
                AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
                TRIM(job-hdr.job-no) GE fjob-no

                AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
                TRIM(job-hdr.job-no) LE tjob-no
                AND job-hdr.job-no2 GE int(begin_job2)
                AND job-hdr.job-no2 LE int(end_job2)
                NO-LOCK,

                FIRST job
                WHERE job.company                   EQ cocode
                AND job.job                       EQ job-hdr.job
                AND job.job-no                    EQ job-hdr.job-no
                AND job.job-no2                   EQ job-hdr.job-no2
                AND job.stat                      NE "H"
                NO-LOCK,

                FIRST est
                WHERE est.company = job.company
                AND est.est-no                    EQ job.est-no
                NO-LOCK

                BREAK BY job-hdr.company:

                IF FIRST(job-hdr.company) THEN
                    ASSIGN
                        ll-fold = NO
                        ll-corr = NO.

                IF est.est-type LE 4 THEN ll-fold = YES.
                ELSE ll-corr = YES.

                IF ll-fold AND ll-corr THEN LEAVE.
            END.

        ASSIGN
            tb_fold:SCREEN-VALUE = STRING(ll-fold)
            tb_corr:SCREEN-VALUE = STRING(ll-corr).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*run output-to-fax.*/
    DO WITH FRAME {&FRAME-NAME}:


        {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
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
    {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF is-xprint-form THEN 
        DO:
            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
            {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject="Factory Ticket"
                             &mail-body="Factory Ticket"
                             &mail-file=lv-pdf-file + ".pdf" }  

        END.
        ELSE 
        DO:
            {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

        END.

    END.
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
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.

    FILE-INFO:FILE-NAME = list-name.    
    RUN printfile (FILE-INFO:FILE-NAME).

/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     /*RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 11, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
*/

   IF tb_corr THEN DO:
       FILE-INFO:FILE-NAME = list-name.
       RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE DO:
      IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0 */
          lookup(lv-format-f, 
"Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OttPkg,MWFibre,Shelby,CCC,Accord") > 0 THEN
     DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
      END.
      ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
   END.
*/  
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
    DEFINE VARIABLE lv-cmd    AS cha NO-UNDO.
    DEFINE VARIABLE lv-file   AS cha NO-UNDO.
    DEFINE VARIABLE lv-xpfile AS cha NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
/*   ELSE                                                                                      */
/*      RUN scr-rpt.w (list-name,c-win:TITLE,lv-font-no,lv-ornt)). /* open file-name, title */ */

/* for xprint view    not working, print automatically
   FILE-INFO:FILE-NAME = "custom\vpxprint.exe".
   lv-cmd = FILE-INFO:FILE-NAME.
   FILE-INFO:FILE-NAME = list-name.
   lv-file = FILE-INFO:FILE-NAME.
   lv-xpfile = lv-file + ".xpr".


   OS-COPY VALUE(lv-file) VALUE(lv-xpfile).
   OS-COMMAND VALUE(lv-cmd + " " + lv-xpfile) .


 IF tb_corr THEN DO:
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).   
 END.
 ELSE DO:

     IF  /*index("Interpac,FibreFC,Dayton,Livngstn",lv-format-f) > 0 */
        lookup(lv-format-f, "Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Accord") > 0 THEN
     DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
     END.
     ELSE 
       RUN scr-rpt.w (list-name,c-win:TITLE,lv-font-no,lv-ornt). /* open file-name, title */
 END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* DEF INPUT PARAM ip-industry AS CHAR NO-UNDO. */

    /* {sys/form/r-top.i}  */

    ASSIGN
        s-prt-mstandard = tb_prt-mch
        s-prt-shipto    = tb_prt-shipto
        s-prt-sellprc   = tb_prt-sellprc
        s-run-speed     = rd_print-speed = "S".


    RUN set-job-vars.

    ASSIGN 
        print-box              = tb_box
        reprint                = tb_reprint
        s-prt-fgimage          = tb_fgimage
        s-prt-label            = tb_prt-label
        s-committed-board-only = tb_committed
        s-prt-set-header       = tb_prt-set-header
        spec-list              = spec_codes
        s-prt-ship-split       = tb_prompt-ship.

    FIND FIRST users 
        WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
    IF AVAILABLE users AND users.user_program[2] NE "" 
        THEN init-dir = users.user_program[2].
    ELSE init-dir = "c:\tmp".

    lv-pdf-file = INIT-dir + "\Job" + STRING(begin_job1).

    SESSION:SET-WAIT-STATE("general").

    IF NOT is-xprint-form THEN 
    DO:

        RUN cerep/jobtickindxl.p (INPUT  YES, 
            INPUT  55,
            INPUT  fjob-no,
            INPUT  fjob-no2,
            INPUT  tjob-no,
            INPUT  tjob-no2,
            INPUT  begin_mach:screen-value IN FRAME {&frame-name},
            INPUT  end_mach:screen-value,
            INPUT  begin_form:screen-value,
            INPUT  end_form:screen-value,
            INPUT  begin_blank:screen-value,
            INPUT  end_blank:SCREEN-VALUE /*,
                                 INPUT begin_ord-no:SCREEN-VALUE,
                                 INPUT end_ord-no:SCREEN-VALUE*/ ).
    END.
    ELSE
        IF is-xprint-form THEN  
        DO:

            IF v-print-fmt EQ "ScheduleCard1" THEN 
                RUN cerep/jobSchrd1.p (INPUT  YES, 
                    INPUT  55,
                    INPUT fjob-no,
                    INPUT fjob-no2,
                    INPUT tjob-no,
                    INPUT tjob-no2,
                    INPUT begin_mach:screen-value IN FRAME {&frame-name},
                    INPUT end_mach:screen-value,
                    INPUT begin_form:screen-value,
                    INPUT end_form:screen-value,
                    INPUT begin_blank:screen-value,
                    INPUT end_blank:screen-value,
                    INPUT begin_ord-no:SCREEN-VALUE,
                    INPUT end_ord-no:SCREEN-VALUE) .
            ELSE
                RUN cerep/jobSLdee.p    (INPUT  YES, 
                    INPUT  55,
                    INPUT fjob-no,
                    INPUT fjob-no2,
                    INPUT tjob-no,
                    INPUT tjob-no2,
                    INPUT begin_mach:screen-value IN FRAME {&frame-name},
                    INPUT end_mach:screen-value,
                    INPUT begin_form:screen-value,
                    INPUT end_form:screen-value,
                    INPUT begin_blank:screen-value,
                    INPUT end_blank:SCREEN-VALUE /*,
                             INPUT begin_ord-no:SCREEN-VALUE,
                             INPUT end_ord-no:SCREEN-VALUE*/ ) .
    


            RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

            RUN output-to-screen.

        END.
    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-job-vars C-Win 
PROCEDURE set-job-vars :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fjob-no  = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1)) 
            tjob-no  = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job1)) 
            fjob-no2 = INT(begin_job2:SCREEN-VALUE)
            tjob-no2 = INT(end_job2:SCREEN-VALUE)
            fform    = int (begin_form:screen-value)
            tform    = int (end_form:screen-value)
            fblnk    = int (begin_blank:screen-value)
            tblnk    = int (end_blank:screen-value).
    END.

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

