&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: est\globquot.w

  Description: Global Quote Price Change

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/02/2002

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
DEFINE VARIABLE ll-new-file AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE lQuotePriceMatrix AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName         AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt-rowid NO-UNDO 
    FIELD row-id AS ROWID
    INDEX row-id row-id.
DEFINE TEMP-TABLE tt-quoteqty LIKE quoteqty .

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

DEFINE STREAM excel.

RUN sys/ref/nk1look.p (INPUT cocode, "QuotePriceMatrix", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lQuotePriceMatrix = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust end_cust begin_date ~
end_date begin_part-no end_part-no begin_fg-cat end_fg-cat begin_rm-no ~
end_rm-no td_only-fgitem td_include-exp percent_chg td_imported rd_i-code ~
rd_pur-man rd_round-EA tb_RoundDownEA rd_round tb_RoundDown tb_prmtx ~
tb_undo fi_file tbAutoClose btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_date end_date ~
begin_part-no end_part-no begin_fg-cat end_fg-cat begin_rm-no end_rm-no ~
td_only-fgitem td_include-exp percent_chg td_imported rd_i-code lbl_i-code ~
rd_pur-man lbl_pur-man rd_round-EA tb_RoundDownEA rd_round tb_RoundDown ~
tb_prmtx tb_undo fi_file tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust    AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Quote Date" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fg-cat  AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Product Category" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_part-no AS CHARACTER FORMAT "X(32)":U 
    LABEL "Beginning Cust Part#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no   AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Board Code" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust      AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Quote Date" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_fg-cat    AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Product Category" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_part-no   AS CHARACTER FORMAT "x(32)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Cust Part#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no     AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Board Code" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\GlobalQuotePrice.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_i-code    AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_pur-man   AS CHARACTER FORMAT "X(256)":U INITIAL "Est Purch/Manuf?" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg   AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "Percent Change" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE rd_i-code     AS CHARACTER INITIAL "A" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Stock", "S",
    "Custom", "C",
    "All", "A"
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_pur-man    AS LOGICAL 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Purchased", YES,
    "Manufactured", NO,
    "All", ?
    SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE rd_round      AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Penny", "P",
    "Dime", "D",
    "Dollar", "B",
    "No Round", "N"
    SIZE 45.6 BY 1 NO-UNDO.

DEFINE VARIABLE rd_round-EA   AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Penny", "P",
    "Dime", "D",
    "Dollar", "B",
    "No Round", "N"
    SIZE 45.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 16.19.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prmtx       AS LOGICAL INITIAL NO 
    LABEL "Update Price Matrix?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_RoundDown   AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 3.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_RoundDownEA AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_undo        AS LOGICAL INITIAL NO 
    LABEL "UNDO a prior price change?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE td_imported    AS LOGICAL INITIAL NO 
    LABEL "Include Contract Pricing Customers?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40.8 BY .81 NO-UNDO.

DEFINE VARIABLE td_include-exp AS LOGICAL INITIAL NO 
    LABEL "Include Expired Quotes" 
    VIEW-AS TOGGLE-BOX
    SIZE 28.4 BY .81 NO-UNDO.

DEFINE VARIABLE td_only-fgitem AS LOGICAL INITIAL NO 
    LABEL "Only Quotes with FG Item?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust AT ROW 2.43 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 2.43 COL 74 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_date AT ROW 3.38 COL 30 COLON-ALIGNED
    end_date AT ROW 3.38 COL 74 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_part-no AT ROW 4.33 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Customer Part Number"
    end_part-no AT ROW 4.33 COL 74 COLON-ALIGNED HELP
    "Enter Ending Customer Part Number"
    begin_fg-cat AT ROW 5.29 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Product Category"
    end_fg-cat AT ROW 5.29 COL 74 COLON-ALIGNED HELP
    "Enter Ending Product Category"
    begin_rm-no AT ROW 6.24 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Board Code" WIDGET-ID 16
    end_rm-no AT ROW 6.24 COL 74 COLON-ALIGNED HELP
    "Enter Ending Board Code" WIDGET-ID 18
    td_only-fgitem AT ROW 7.91 COL 10 WIDGET-ID 20
    td_include-exp AT ROW 7.91 COL 55.6 WIDGET-ID 30
    percent_chg AT ROW 9 COL 70 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    td_imported AT ROW 9.05 COL 10
    rd_i-code AT ROW 10.19 COL 38 NO-LABELS
    lbl_i-code AT ROW 10.24 COL 23 COLON-ALIGNED NO-LABELS
    rd_pur-man AT ROW 11.38 COL 38 NO-LABELS
    lbl_pur-man AT ROW 11.43 COL 19 COLON-ALIGNED NO-LABELS
    rd_round-EA AT ROW 12.62 COL 31.8 NO-LABELS WIDGET-ID 2
    tb_RoundDownEA AT ROW 12.81 COL 77.6 WIDGET-ID 22
    rd_round AT ROW 13.76 COL 32 NO-LABELS
    tb_RoundDown AT ROW 13.86 COL 77.6 WIDGET-ID 24
    tb_prmtx AT ROW 14.91 COL 35
    tb_undo AT ROW 14.95 COL 34
    fi_file AT ROW 16.19 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tbAutoClose AT ROW 18.05 COL 28.2 WIDGET-ID 64
    btn-process AT ROW 18.95 COL 28
    btn-cancel AT ROW 18.95 COL 55
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 1.29 COL 4
    "For UOM=EA, round  to:" VIEW-AS TEXT
    SIZE 23.6 BY .71 AT ROW 12.67 COL 6.8 WIDGET-ID 14
    "Round Down" VIEW-AS TEXT
    SIZE 13 BY .62 AT ROW 13.91 COL 81.6 WIDGET-ID 28
    "Round Down" VIEW-AS TEXT
    SIZE 12.8 BY .95 AT ROW 12.71 COL 81.8 WIDGET-ID 26
    "For all other UOM, round to:" VIEW-AS TEXT
    SIZE 28 BY .71 AT ROW 13.86 COL 4 WIDGET-ID 12
    RECT-17 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.8 BY 19.48
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
        TITLE              = "Global Quote Price Change"
        HEIGHT             = 19.48
        WIDTH              = 96.8
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
ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_part-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_part-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_i-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_i-code:PRIVATE-DATA IN FRAME FRAME-A = "rd_i-code".

/* SETTINGS FOR FILL-IN lbl_pur-man IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_pur-man:PRIVATE-DATA IN FRAME FRAME-A = "rd_pur-man".

ASSIGN 
    percent_chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_i-code:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_pur-man:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_round:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_round-EA:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prmtx:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_RoundDown:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_RoundDownEA:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_undo:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    td_imported:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    td_include-exp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    td_only-fgitem:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Quote Price Change */
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
ON WINDOW-CLOSE OF C-Win /* Global Quote Price Change */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Quote Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning Product Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_part-no C-Win
ON HELP OF begin_part-no IN FRAME FRAME-A /* Beginning Cust Part# */
    DO:
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN est/l-ebrfqP.w (cocode, locode, begin_part-no:screen-value, OUTPUT lv-eb-tmpid) .
        FIND FIRST eb NO-LOCK WHERE RECID(eb) = lv-eb-tmpid NO-ERROR.
        IF AVAILABLE eb THEN ASSIGN begin_part-no:SCREEN-VALUE = eb.part-no.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Board Code */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DEFINE VARIABLE v-process AS LOG INIT NO NO-UNDO.


        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
    
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
    
        END.

        /*message "Are you sure you want to change the Quote Price(s) within the " +
                "selection parameters?"
                view-as alert-box question button yes-no update v-process.*/

        /*if v-process then*/ RUN run-process.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Quote Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending Product Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_part-no C-Win
ON HELP OF end_part-no IN FRAME FRAME-A /* Ending Cust Part# */
    DO:
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN est/l-ebrfqP.w (cocode, locode, end_part-no:screen-value, OUTPUT lv-eb-tmpid) .
        FIND FIRST eb NO-LOCK WHERE RECID(eb) = lv-eb-tmpid NO-ERROR.
        IF AVAILABLE eb THEN ASSIGN end_part-no:SCREEN-VALUE = eb.part-no.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Board Code */
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME percent_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON LEAVE OF percent_chg IN FRAME FRAME-A /* Percent Change */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pur-man C-Win
ON VALUE-CHANGED OF rd_pur-man IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_round
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_round C-Win
ON VALUE-CHANGED OF rd_round IN FRAME FRAME-A
    DO:
        ASSIGN {&SELF-NAME}.
  
        IF {&SELF-NAME} = "N" THEN 
        DO:
            IF tb_RoundDown:CHECKED THEN
                tb_RoundDown:CHECKED = NO.   
            tb_RoundDown:SENSITIVE = FALSE.
        END.   
    
        ELSE tb_RoundDown:Sensitive = TRUE.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_round-EA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_round-EA C-Win
ON VALUE-CHANGED OF rd_round-EA IN FRAME FRAME-A
    DO: 
        ASSIGN {&SELF-NAME}.
        IF {&SELF-NAME} = "N" THEN 
        DO:
            IF tb_RoundDownEA:CHECKED THEN
                tb_RoundDownEA:CHECKED = NO.
            tb_RoundDownEA:SENSITIVE = FALSE.    
        END.
      
        ELSE tb_RoundDownEA:SENSITIVE = TRUE. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prmtx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prmtx C-Win
ON VALUE-CHANGED OF tb_prmtx IN FRAME FRAME-A /* Update Price Matrix? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td_only-fgitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td_only-fgitem C-Win
ON VALUE-CHANGED OF td_only-fgitem IN FRAME FRAME-A /* Only Quotes with FG Item? */
    DO:
        ASSIGN {&self-name}.
        IF td_only-fgitem:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                begin_fg-cat:SENSITIVE    = NO 
                end_fg-cat:SENSITIVE      = NO 
                begin_fg-cat:SCREEN-VALUE = "" 
                end_fg-cat:SCREEN-VALUE   = "zzzzz" 
                .
        ELSE
            ASSIGN
                begin_fg-cat:SENSITIVE = YES 
                end_fg-cat:SENSITIVE   = YES   .
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        tb_undo:HIDDEN  = YES .
        IF td_only-fgitem:SCREEN-VALUE EQ "No" THEN
            ASSIGN
                begin_fg-cat:SENSITIVE    = NO 
                end_fg-cat:SENSITIVE      = NO 
                begin_fg-cat:SCREEN-VALUE = "" 
                end_fg-cat:SCREEN-VALUE   = "zzzzz" .
        IF lQuotePriceMatrix THEN
            ASSIGN
                tb_prmtx:SENSITIVE    = NO
                tb_prmtx:SCREEN-VALUE = "NO" .
     
        APPLY "entry" TO begin_cust.
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
    DISPLAY begin_cust end_cust begin_date end_date begin_part-no end_part-no 
        begin_fg-cat end_fg-cat begin_rm-no end_rm-no td_only-fgitem 
        td_include-exp percent_chg td_imported rd_i-code lbl_i-code rd_pur-man 
        lbl_pur-man rd_round-EA tb_RoundDownEA rd_round tb_RoundDown tb_prmtx 
        tb_undo fi_file tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_cust end_cust begin_date end_date begin_part-no 
        end_part-no begin_fg-cat end_fg-cat begin_rm-no end_rm-no 
        td_only-fgitem td_include-exp percent_chg td_imported rd_i-code 
        rd_pur-man rd_round-EA tb_RoundDownEA rd_round tb_RoundDown tb_prmtx 
        tb_undo fi_file tbAutoClose btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-params C-Win 
PROCEDURE get-params :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-fld-list AS cha NO-UNDO.

    DEFINE VARIABLE lv-frame-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.


    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.

        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0 THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                op-fld-list = TRIM(op-fld-list) + " " +
                    lv-field-hdl:LABEL + ":" +
                    lv-field-hdl:SCREEN-VALUE + ",".

            ELSE 
            DO:  /* radio set */
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE.

                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                        op-fld-list = TRIM(op-fld-list) + " " +
                            lv-field2-hdl:SCREEN-VALUE + ":".

                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END. 

                op-fld-list = TRIM(op-fld-list) +
                    lv-field-hdl:SCREEN-VALUE + ",".      
            END.                 
        END.

        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE round-up C-Win 
PROCEDURE round-up :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipd-new-value AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipd-calc-value AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipc-type AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opd-out-dec AS DECIMAL NO-UNDO.
    /*
    1. ROUND
    2. IF the truncated old value is same as the new value,
       it was rounded down, so then move it up 
    */

    CASE ipc-type:
        WHEN "B" THEN 
            DO:
                IF ipd-new-value - TRUNC(ipd-new-value, 0) GT 0 THEN
                    ipd-calc-value = ipd-calc-value + 1.
            END.
        WHEN "D" THEN 
            DO:
                IF ipd-new-value - TRUNC(ipd-new-value, 1) GT 0 THEN
                    ipd-calc-value = ipd-calc-value + .1.
            END.
    END CASE.
    opd-out-dec = ipd-calc-value.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    /* ------------------------------------------------- cec/quoprchg.p 08/96 FWK */
    /* Global Price Change for Quote Sell Price                                   */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-pct        AS DECIMAL   FORMAT "->>9.99".
    DEFINE VARIABLE v-round      AS CHARACTER FORMAT "!" INIT "P".
    DEFINE VARIABLE v-round-EA   AS CHARACTER FORMAT "!" INIT "P".
    DEFINE VARIABLE v-undo       AS LOG       FORMAT "yes/no" INIT NO.
    DEFINE VARIABLE v            AS INTEGER.
    DEFINE VARIABLE v-EA         AS INTEGER.
    DEFINE VARIABLE v-orig-price AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE fcust        AS ch        INIT "" NO-UNDO.
    DEFINE VARIABLE tcust        LIKE fcust INIT "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE fdate        AS DATE      INIT TODAY NO-UNDO.
    DEFINE VARIABLE tdate        LIKE fdate INIT TODAY NO-UNDO.

    DEFINE VARIABLE lv-part-no   LIKE quoteitm.part-no NO-UNDO.
    DEFINE VARIABLE lv-rowid     AS ROWID     NO-UNDO.
    DEFINE VARIABLE ll           AS LOG       EXTENT 2 NO-UNDO.
    DEFINE VARIABLE lv-reft      LIKE reftable.reftable NO-UNDO.
    DEFINE VARIABLE lv-dscr      LIKE reftable.dscr NO-UNDO.
    DEFINE VARIABLE excelheader  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-process    AS LOG       INIT NO NO-UNDO.
    DEFINE BUFFER bf-quoteitm FOR quoteitm.

    DEFINE VARIABLE lRoundDown   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRoundDownEA AS LOGICAL NO-UNDO.

    ASSIGN
        fcust        = begin_cust
        tcust        = end_cust
        fdate        = begin_date
        tdate        = end_date
        v-pct        = percent_chg
        v-round      = rd_round
        v-round-EA   = rd_round-EA
        v-undo       = /*_undo*/ NO
        lRoundDown   = tb_RoundDown
        lRoundDownEA = tb_RoundDownEA .
 
    SESSION:SET-WAIT-STATE("General").

    EMPTY TEMP-TABLE tt-rowid.
    EMPTY TEMP-TABLE tt-quoteqty .

    RUN get-params (OUTPUT lv-dscr).

    v = INDEX("DP",v-round).
    v-EA = INDEX("DP",v-round-EA).

    OUTPUT STREAM excel TO VALUE(cFileName).
    excelheader = "Quote,Customer,Estimate,cust part,Item No,Product Category,Qty,Old Price,New Price"
        .

    PUT STREAM excel UNFORMATTED 
        '"' REPLACE(excelheader,',','","') '"' SKIP.
    MAIN:
    FOR EACH quotehd
        WHERE quotehd.company  EQ cocode
        AND quotehd.loc      EQ locode
        AND quotehd.cust-no  GE fcust
        AND quotehd.cust-no  LE tcust
        AND quotehd.quo-date GE fdate
        AND quotehd.quo-date LE tdate
        AND quotehd.approved EQ FALSE
        AND ((quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?) OR td_include-exp EQ YES)
        USE-INDEX cust2,

        EACH quoteitm
        WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.loc     EQ quotehd.loc
        AND quoteitm.q-no    EQ quotehd.q-no
        AND quoteitm.part-no GE begin_part-no
        AND quoteitm.part-no LE end_part-no
        NO-LOCK

        BREAK BY quotehd.q-no:

        RELEASE eb .

        FIND FIRST cust  NO-LOCK WHERE cust.company EQ cocode 
            AND cust.cust-no EQ  quotehd.cust-no NO-ERROR .

        IF AVAILABLE cust AND cust.imported EQ YES AND NOT td_imported THEN NEXT MAIN.

        IF quotehd.est-no NE "" THEN
            FIND FIRST eb
                WHERE eb.company EQ quotehd.company
                AND eb.est-no EQ quotehd.est-no
                AND eb.part-no EQ  quoteitm.part-no
                AND (eb.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                NO-LOCK NO-ERROR.
                
        IF NOT AVAILABLE eb AND quotehd.est-no NE "" THEN NEXT.        
               
        IF begin_rm-no NE "" AND NOT END_rm-no BEGINS "zzzzz" AND quotehd.est-no NE "" THEN 
        DO:             
            IF NOT CAN-FIND(FIRST ef OF eb
                WHERE ef.board GE begin_rm-no
                AND ef.board LE END_rm-no) 
                THEN NEXT.
        END.
        IF FIRST-OF(quotehd.q-no) THEN ll[1] = NO.

        ASSIGN
            ll[2]    = NO
            lv-rowid = ?.

        RELEASE itemfg.

        IF ll-new-file AND NOT td_only-fgitem THEN 
        DO:
            lv-part-no = quoteitm.part-no.
            RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
        END.
        ELSE 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company  EQ quoteitm.company
                AND itemfg.i-no  EQ quoteitm.i-no
                AND itemfg.i-no  NE "" NO-ERROR.
            IF AVAILABLE itemfg THEN
                lv-rowid = ROWID(itemfg) .
        END.

        IF lv-rowid EQ ? AND NOT td_only-fgitem THEN
            FIND FIRST itemfg
                WHERE itemfg.company  EQ quoteitm.company
                AND itemfg.part-no  EQ quoteitm.part-no
                AND itemfg.part-no  NE ""
                AND (itemfg.cust-no EQ quotehd.cust-no OR
                itemfg.i-code  EQ "S")
                NO-LOCK NO-ERROR.
        ELSE
            FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.

        IF ( NOT AVAILABLE itemfg AND NOT td_only-fgitem AND
            CAN-FIND(FIRST eb
            WHERE eb.company EQ quotehd.company
            AND eb.est-no  EQ quotehd.est-no
            AND eb.part-no EQ  quoteitm.part-no 
            AND (eb.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)) )               OR
            CAN-FIND(FIRST itemfg
            WHERE ROWID(itemfg)   EQ lv-rowid
            AND (itemfg.procat   GE begin_fg-cat OR NOT td_only-fgitem)
            AND (itemfg.procat   LE end_fg-cat OR NOT td_only-fgitem)
            AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")             
            AND lv-rowid        NE ?)                             OR
            ( NOT td_only-fgitem AND
            CAN-FIND(FIRST itemfg
            WHERE itemfg.company  EQ quoteitm.company
            AND itemfg.part-no  EQ quoteitm.part-no
            AND itemfg.part-no  NE ""
            AND (itemfg.cust-no EQ quotehd.cust-no OR
            itemfg.i-code  EQ "S")
            AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")             
            AND lv-rowid        EQ ?))                             THEN
            FOR EACH quoteqty
                WHERE quoteqty.company EQ quoteitm.company
                AND quoteqty.loc     EQ quoteitm.loc
                AND quoteqty.q-no    EQ quoteitm.q-no
                AND quoteqty.line    EQ quoteitm.line
                AND NOT CAN-FIND(FIRST tt-rowid
                WHERE tt-rowid.row-id EQ ROWID(quoteqty))
                BREAK BY quoteqty.qty:

                ll = YES.
    
                BUFFER-COPY  quoteqty TO  tt-quoteqty.
      
                v-orig-price = quoteqty.price.
                IF v-undo THEN
                    tt-quoteqty.price = tt-quoteqty.price / (1 - (v-pct / 100)).
                ELSE
                    tt-quoteqty.price = tt-quoteqty.price + (tt-quoteqty.price * v-pct / 100).
                v-orig-price = tt-quoteqty.price.
    
                /* Perform rounding */
                IF NOT v-round = "N" AND tt-quoteqty.uom NE "EA" THEN 
                DO:
                    IF lRoundDown THEN /* Perform RoundDown */
                        tt-quoteqty.price = TRUNCATE(tt-quoteqty.price,v).
            
                    ELSE 
                    DO: /* Else do RoundUp */
                        tt-quoteqty.price = ROUND(tt-quoteqty.price, v). 
                        IF tt-quoteqty.price LT v-orig-price THEN
                            RUN round-up (INPUT v-orig-price, INPUT tt-quoteqty.price , INPUT v-round, OUTPUT tt-quoteqty.price).
                    END.    
                END.
    
                IF NOT v-round-EA = "N" AND tt-quoteqty.uom EQ "EA" THEN 
                DO:
                    IF lRoundDownEA THEN /* Perform Rounddown */
                        tt-quoteqty.price = TRUNCATE(tt-quoteqty.price,v-EA).
        
                    ELSE 
                    DO: /* Perform RoundUp */
                        tt-quoteqty.price = ROUND(tt-quoteqty.price, v-EA).    
                        IF tt-quoteqty.price LT v-orig-price THEN
                            RUN round-up (INPUT v-orig-price, INPUT tt-quoteqty.price, INPUT v-round-EA, OUTPUT tt-quoteqty.price).
                    END.    
                END.

                PUT STREAM excel UNFORMATTED
                    '"' tt-quoteqty.q-no                              '",'
                    '"' quotehd.cust-no                                '",'
                    '"' quotehd.est-no                                 '",'
                    '"' quoteitm.part-no                               '",'
                    '"' quoteitm.i-no                                  '",'
                    '"' IF AVAILABLE itemfg THEN  itemfg.procat  ELSE IF AVAILABLE eb THEN eb.procat ELSE  ""        '",'
                    '"' quoteqty.qty                                   '",'
                    '"' quoteqty.price                                 '",'
                    '"' tt-quoteqty.price                              '",'
                    SKIP.
     
            END.
    END.

    OUTPUT STREAM excel CLOSE.
    OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).

    MESSAGE "Are you sure you want to commit these changes"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
 
    IF NOT v-process THEN RETURN .

    IF v-process THEN
        FOR EACH quotehd
            WHERE quotehd.company  EQ cocode
            AND quotehd.loc      EQ locode
            AND quotehd.cust-no  GE fcust
            AND quotehd.cust-no  LE tcust
            AND quotehd.quo-date GE fdate
            AND quotehd.quo-date LE tdate
            AND quotehd.approved EQ FALSE
            AND ((quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?) OR td_include-exp EQ YES)
            USE-INDEX cust2,

            EACH quoteitm
            WHERE quoteitm.company EQ quotehd.company
            AND quoteitm.loc     EQ quotehd.loc
            AND quoteitm.q-no    EQ quotehd.q-no
            AND quoteitm.part-no GE begin_part-no
            AND quoteitm.part-no LE end_part-no
            NO-LOCK

            BREAK BY quotehd.q-no:

            RELEASE eb .
            IF quotehd.est-no NE "" THEN
                FIND FIRST eb
                    WHERE eb.company EQ quotehd.company
                    AND eb.est-no EQ quotehd.est-no
                    AND eb.part-no EQ  quoteitm.part-no
                    AND (eb.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                    NO-LOCK NO-ERROR.
                    
            IF NOT AVAILABLE eb AND quotehd.est-no NE "" THEN NEXT.
            
            IF begin_rm-no NE "" AND NOT END_rm-no BEGINS "zzzzz" AND quotehd.est-no NE "" THEN 
            DO:                                 
                IF NOT CAN-FIND(FIRST ef OF eb
                    WHERE ef.board GE begin_rm-no
                    AND ef.board LE END_rm-no) 
                    THEN NEXT.
            END.
            IF FIRST-OF(quotehd.q-no) THEN ll[1] = NO.

            ASSIGN
                ll[2]    = NO
                lv-rowid = ?.

            RELEASE itemfg.

            IF ll-new-file AND NOT td_only-fgitem THEN 
            DO:
                lv-part-no = quoteitm.part-no.
                RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                    INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
            END.
            ELSE 
            DO:
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company  EQ quoteitm.company
                    AND itemfg.i-no  EQ quoteitm.i-no
                    AND itemfg.i-no  NE "" NO-ERROR.
                IF AVAILABLE itemfg THEN
                    lv-rowid = ROWID(itemfg) .
            END.

            IF lv-rowid EQ ? AND NOT td_only-fgitem THEN
                FIND FIRST itemfg
                    WHERE itemfg.company  EQ quoteitm.company
                    AND itemfg.part-no  EQ quoteitm.part-no
                    AND itemfg.part-no  NE ""
                    AND (itemfg.cust-no EQ quotehd.cust-no OR
                    itemfg.i-code  EQ "S")
                    NO-LOCK NO-ERROR.
            ELSE
                FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.

            IF ( NOT AVAILABLE itemfg AND NOT td_only-fgitem AND 
                CAN-FIND(FIRST eb
                WHERE eb.company EQ quotehd.company
                AND eb.est-no  EQ quotehd.est-no
                AND eb.part-no EQ  quoteitm.part-no 
                AND (eb.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)) )               OR
                CAN-FIND(FIRST itemfg
                WHERE ROWID(itemfg)   EQ lv-rowid
                AND (itemfg.procat   GE begin_fg-cat OR NOT td_only-fgitem)
                AND (itemfg.procat   LE end_fg-cat OR NOT td_only-fgitem)
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")                
                AND lv-rowid        NE ?)                             OR
                (NOT td_only-fgitem AND
                CAN-FIND(FIRST itemfg
                WHERE itemfg.company  EQ quoteitm.company
                AND itemfg.part-no  EQ quoteitm.part-no
                AND itemfg.part-no  NE ""
                AND (itemfg.cust-no EQ quotehd.cust-no OR
                itemfg.i-code  EQ "S")
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")                
                AND lv-rowid        EQ ?))                             THEN
                FOR EACH quoteqty
                    WHERE quoteqty.company EQ quoteitm.company
                    AND quoteqty.loc     EQ quoteitm.loc
                    AND quoteqty.q-no    EQ quoteitm.q-no
                    AND quoteqty.line    EQ quoteitm.line
                    AND NOT CAN-FIND(FIRST tt-rowid
                    WHERE tt-rowid.row-id EQ ROWID(quoteqty))
                    BREAK BY quoteqty.qty:

                    ll = YES.

                    /* In case they mix buck and penny - task 10151205 */

                    CREATE tt-rowid.
                    tt-rowid.row-id = ROWID(quoteqty).

                    v-orig-price = quoteqty.price.
                    IF v-undo THEN
                        quoteqty.price = quoteqty.price / (1 - (v-pct / 100)).
                    ELSE
                        quoteqty.price = quoteqty.price + (quoteqty.price * v-pct / 100).
                    v-orig-price = quoteqty.price.

                    /* Perform rounding */
                    IF NOT v-round = "N" AND quoteqty.uom NE "EA" THEN 
                    DO:
                        IF lRoundDown THEN /* Perform RoundDown */
                            quoteqty.price = TRUNCATE(quoteqty.price,v).
            
                        ELSE 
                        DO:    /* Perform RoundUp */ 
                            quoteqty.price = ROUND(quoteqty.price, v).    
                            IF quoteqty.price LT v-orig-price THEN
                                RUN round-up (INPUT v-orig-price, INPUT quoteqty.price, INPUT v-round, OUTPUT quoteqty.price).
                        END.  
                    END.
                    IF NOT v-round-EA = "N" AND quoteqty.uom EQ "EA" THEN 
                    DO:
                        IF lRoundDownEA THEN /* Perform RoundDown */
                            quoteqty.price = TRUNCATE(quoteqty.price,v-EA).
        
                        ELSE 
                        DO:  /* Perform RoundUp */   
                            quoteqty.price = ROUND(quoteqty.price, v-EA).    
                            IF quoteqty.price LT v-orig-price THEN
                                RUN round-up (INPUT v-orig-price, INPUT quoteqty.price, INPUT v-round-EA, OUTPUT quoteqty.price).
                        END.    
                    END.
     
                END.

            FOR EACH quoteqty
                WHERE quoteqty.company EQ quoteitm.company
                AND quoteqty.loc     EQ quoteitm.loc
                AND quoteqty.q-no    EQ quoteitm.q-no
                AND quoteqty.line    EQ quoteitm.line
                BY quoteqty.qty:

                /* 11021209 - itm price should be first qty price per Joe */
                FIND bf-quoteitm WHERE ROWID(bf-quoteitm) EQ ROWID(quoteitm)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bf-quoteitm THEN 
                DO:
                    bf-quoteitm.price = quoteqty.price.

                    RELEASE bf-quoteitm.
                    LEAVE.
                END.

            END.
            IF tb_prmtx AND ll[2] THEN RUN oe/updprmtx.p (ROWID(quoteitm), "", 0, "", 0).

            IF LAST-OF(quotehd.q-no) AND ll[1] THEN quotehd.quo-date = TODAY.
        END. 

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").  

    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

