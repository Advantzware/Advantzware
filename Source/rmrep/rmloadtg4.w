&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep/rmloadtg4.w

  Description: RM Loadtag Creation

  Author: Ron Stark (copied from oerep/r-loadtg.w)

  Created: 3.4.2005

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ip-param AS LOG NO-UNDO.
DEFINE INPUT PARAMETER ip-tag-no AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE list-name     AS cha       NO-UNDO.
DEFINE VARIABLE init-dir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE correct-error AS LOGICAL   NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
IF access-close THEN 
DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
END.

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{custom/xprint.i}
ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE tt-po NO-UNDO
    FIELD po-no             AS INTEGER
    FIELD line              AS INTEGER
    FIELD tot-rec-qty       AS DECIMAL
    FIELD cons-uom          AS CHARACTER
    FIELD overrun-qty       AS DECIMAL
    FIELD tot-converted-qty AS DECIMAL
    INDEX po po-no line
    .
&IF DEFINED(AutoReprint) EQ 0 &THEN
&Scoped-define NEW NEW
&ENDIF
{rmrep/ttLoadTag.i}

/*DEFINE VARIABLE lInteractive AS LOGICAL NO-UNDO.*/
DEFINE VARIABLE lines-per-page     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-overrun          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-mch-cod          AS CHARACTER INIT " " NO-UNDO.
DEFINE VARIABLE cFirstInternalMach AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFirstMachPress    AS CHARACTER NO-UNDO .
DEFINE VARIABLE save_id            AS RECID.

DEFINE VARIABLE time_stamp         AS ch.
ASSIGN 
    time_stamp = STRING(TIME, "hh:mmam").

DEFINE VARIABLE v-fpo-no       AS INTEGER   FORMAT ">>>>>>" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-orders       AS CHARACTER FORMAT "x(78)" EXTENT 10.
DEFINE VARIABLE v-fitem        AS CHARACTER FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEFINE VARIABLE v-po-no-source AS CHARACTER FORMAT "!" INIT "R".
DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "O".

DEFINE VARIABLE v-out          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE v-init-dir     AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE v-job          AS CHARACTER FORMAT "x(13)" NO-UNDO.
DEFINE VARIABLE num-rec        AS INTEGER   INIT 0 NO-UNDO.
DEFINE VARIABLE by-release     AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE lv-rd_print    AS CHARACTER NO-UNDO.
DEFINE VARIABLE UserlabelPath  AS cha       NO-UNDO.
DEFINE VARIABLE v-rmbardir     AS LOG       NO-UNDO.

/* 9812 CAH: */
DEFINE VARIABLE v-loadtag      AS CHARACTER NO-UNDO INIT "ASI". /* sys ctrl option */
DEFINE VARIABLE v-mult         AS INTEGER   NO-UNDO INIT 0. /* sys ctrl option */
DEFINE VARIABLE v-cas-lab      AS LOG       NO-UNDO. /* sys ctrl option */
DEFINE VARIABLE v-tags         AS DECIMAL   NO-UNDO INIT 0. /* sys ctrl option */
DEFINE VARIABLE v-count        AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE v-rmrecpt      AS LOG       NO-UNDO. /* sys ctrl option */
DEFINE VARIABLE v-calc         AS LOG       NO-UNDO. /* sys ctrl cption */
DEFINE VARIABLE v-bin          AS CHARACTER NO-UNDO. /* sys ctrl cption */
DEFINE VARIABLE ext-cost       AS DECIMAL   NO-UNDO.

/* 9812 CAH: Variables for Intermec Support */
DEFINE VARIABLE stx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~002".
DEFINE VARIABLE etx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~003".
DEFINE VARIABLE esc            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~033".
DEFINE VARIABLE etb            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~027".
DEFINE VARIABLE cr             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~015".
DEFINE VARIABLE can            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~030".
DEFINE VARIABLE rs             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~036".
DEFINE VARIABLE us             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~037".

DEFINE STREAM s-form.
DEFINE STREAM s-bar.

DEFINE VARIABLE form_fid         AS CHARACTER NO-UNDO INITIAL "barcode.frm" FORMAT "X(40)".
DEFINE VARIABLE form#            AS INTEGER   NO-UNDO FORMAT "9" INITIAL 3.
DEFINE VARIABLE char_units       AS CHARACTER NO-UNDO.
DEFINE VARIABLE copy_count       AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE n                AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE v-avgcost        AS LOG       NO-UNDO.
DEFINE VARIABLE cBarCodeProgram  AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdJobProcs       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iWarehouseLength AS INTEGER   NO-UNDO.

RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
{rm/avgcost.i}

DEFINE BUFFER b-company FOR company.
DEFINE BUFFER bf-po-ord FOR po-ord .
DEFINE TEMP-TABLE w-file NO-UNDO 
    FIELD w-key AS ROWID.
DEFINE TEMP-TABLE tt-tag NO-UNDO 
    FIELD tt-recid AS RECID.
DEFINE TEMP-TABLE ttblJob NO-UNDO
    FIELD job-no  AS CHARACTER
    FIELD job-no2 AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE
    job-no
    job-no2.

DEFINE TEMP-TABLE tt-mat NO-UNDO 
    FIELD frm LIKE job-mat.frm
    FIELD qty LIKE job-mat.qty
    INDEX frm frm. 

{rmrep/rmloadtg.i NEW}
{sys/form/r-top3.f}

DEFINE TEMP-TABLE tt-po-print LIKE w-po 
    FIELD tag-no   AS CHARACTER 
    FIELD vend-tag AS CHARACTER.

tmpstore = FILL("_",50).
RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

DEFINE VARIABLE hdPoProcs AS HANDLE NO-UNDO.

RUN po/POProcs.p PERSISTENT SET hdPoProcs.
                          
DEFINE VARIABLE cReturnValue        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCheckClosedStatus AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (
    INPUT cocode,           /* Company Code */ 
    INPUT "RMReceiptRules", /* sys-ctrl name */
    INPUT "I",              /* Output return value */
    INPUT NO,               /* Use ship-to */
    INPUT NO,               /* ship-to vendor */
    INPUT "",               /* ship-to vendor value */
    INPUT "",               /* shi-id value */
    OUTPUT cReturnValue, 
    OUTPUT lRecFound
    ). 

glCheckClosedStatus = IF (lRecFound AND INTEGER(cReturnValue) EQ 1) THEN YES ELSE NO.
    
DO TRANSACTION:
/*{sys/inc/bardir.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 reprintTag v-po-list v-job-list ~
begin_po-no end_po-no begin_job begin_job2 end_job end_job2 begin_rm-i-no ~
end_rm-i-no scr-auto-print scr-label-file rd_status tb_16ths begin_form ~
begin_labels begin_filename tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS reprintTag v-po-list v-job-list ~
begin_po-no end_po-no begin_job begin_job2 end_job end_job2 begin_rm-i-no ~
end_rm-i-no scr-auto-print scr-label-file rd_status tb_16ths ~
scr-text-file-path begin_form begin_labels begin_filename tbAutoClose 

/* Custom List Definitions                                              */
/* paramFields,triad,List-3,List-4,List-5,F1                            */
&Scoped-define paramFields v-po-list v-job-list begin_po-no end_po-no ~
begin_job begin_job2 end_job end_job2 begin_rm-i-no end_rm-i-no rd_status ~
tb_16ths begin_form begin_labels begin_filename 
&Scoped-define triad begin_form begin_labels begin_filename 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
    (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE v-job-list         AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 42 BY 4.52 NO-UNDO.

DEFINE VARIABLE v-po-list          AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 42 BY 4.52 NO-UNDO.

DEFINE VARIABLE begin_filename     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Form File" 
    VIEW-AS FILL-IN 
    SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form         AS INTEGER   FORMAT ">>>":U INITIAL 3 
    LABEL "Printer Form#" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job          AS CHARACTER FORMAT "X(9)":U 
    LABEL "From Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2         AS INTEGER   FORMAT "999":U INITIAL 0 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels       AS INTEGER   FORMAT ">>>>":U INITIAL 2 
    LABEL "# of Labels/Pallet" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no        AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "From PO#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-i-no      AS CHARACTER FORMAT "X(15)":U 
    LABEL "From RM Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job            AS CHARACTER FORMAT "X(9)":U 
    LABEL "To Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2           AS INTEGER   FORMAT "999":U INITIAL 999 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no          AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "To PO#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no        AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To RM Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE reprintLoadtag     AS CHARACTER FORMAT "X(256)":U 
    LABEL "&Tag#" 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Label Matrix Label File" 
    VIEW-AS FILL-IN 
    SIZE 74 BY 1 NO-UNDO.

DEFINE VARIABLE scr-text-file-path AS CHARACTER FORMAT "X(256)":U 
    LABEL "Text File/Path" 
    VIEW-AS FILL-IN 
    SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE rd_status          AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "O",
    "Close", "C",
    "All", "A"
    SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 17.62.

DEFINE VARIABLE reprintTag     AS LOGICAL INITIAL NO 
    LABEL "&Reprint Tag" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE scr-auto-print AS LOGICAL INITIAL NO 
    LABEL "Auto Print Label?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_16ths       AS LOGICAL INITIAL NO 
    LABEL "Show LWD in 16ths?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    reprintTag AT ROW 1.76 COL 28
    reprintLoadtag AT ROW 1.76 COL 54 COLON-ALIGNED HELP
    "Enter Loadtag # or Press Help"
    v-po-list AT ROW 3.67 COL 8 NO-LABELS
    v-job-list AT ROW 3.67 COL 56 NO-LABELS
    begin_po-no AT ROW 8.43 COL 21 COLON-ALIGNED HELP
    "Enter Beginning PO Number"
    end_po-no AT ROW 8.43 COL 65 COLON-ALIGNED HELP
    "Enter Ending PO Number"
    begin_job AT ROW 9.38 COL 21 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job2 AT ROW 9.38 COL 35.6 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job AT ROW 9.38 COL 65 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job2 AT ROW 9.38 COL 79.6 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_rm-i-no AT ROW 10.33 COL 21 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_rm-i-no AT ROW 10.33 COL 65 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    scr-auto-print AT ROW 11.52 COL 25 WIDGET-ID 4
    scr-label-file AT ROW 12.48 COL 23.8 COLON-ALIGNED WIDGET-ID 6
    rd_status AT ROW 13.67 COL 23 NO-LABELS
    tb_16ths AT ROW 13.67 COL 67
    scr-text-file-path AT ROW 14.71 COL 21 COLON-ALIGNED WIDGET-ID 2
    begin_form AT ROW 15.86 COL 21 COLON-ALIGNED
    begin_labels AT ROW 16.81 COL 21 COLON-ALIGNED
    begin_filename AT ROW 17.76 COL 21 COLON-ALIGNED
    tbAutoClose AT ROW 19.19 COL 34.8 WIDGET-ID 64
    btn-ok AT ROW 20.14 COL 34.6
    btn-cancel AT ROW 20.14 COL 55.6
    "PO/Job Status:" VIEW-AS TEXT
    SIZE 15 BY 1 AT ROW 13.67 COL 7
    " Enter Job(s) separated by comma" VIEW-AS TEXT
    SIZE 42 BY .62 AT ROW 2.95 COL 56
    BGCOLOR 14 
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    " Enter POs separated by comma" VIEW-AS TEXT
    SIZE 42 BY .62 AT ROW 2.95 COL 8
    BGCOLOR 14 
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 105 BY 20.62
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
        TITLE              = "Raw Material Loadtag Creation"
        HEIGHT             = 20.62
        WIDTH              = 105
        MAX-HEIGHT         = 20.62
        MAX-WIDTH          = 105
        VIRTUAL-HEIGHT     = 20.62
        VIRTUAL-WIDTH      = 105
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
/* SETTINGS FOR FILL-IN begin_filename IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    begin_filename:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_form IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    begin_form:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_job IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_job:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_job2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_labels IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    begin_labels:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_po-no IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_rm-i-no IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_job IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_job:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_job2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_po-no IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_rm-i-no IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_status IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN reprintLoadtag IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    reprintLoadtag:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    scr-auto-print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-label-file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN scr-text-file-path IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    scr-text-file-path:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_16ths IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    tb_16ths:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR EDITOR v-job-list IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    v-job-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR EDITOR v-po-list IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    v-po-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raw Material Loadtag Creation */
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
ON WINDOW-CLOSE OF C-Win /* Raw Material Loadtag Creation */
    DO:
        /* This event will close the window and terminate the procedure.  */
        IF INDEX(PROGRAM-NAME(4),"asiLogin") <> 0 THEN
            RUN system/userLogOut.p (NO, 0).
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Form File */
    DO:
        ASSIGN begin_filename.

        IF begin_filename GT "" AND LASTKEY NE -1 THEN 
        DO:
            IF SEARCH(begin_filename) EQ ? THEN 
            DO:
                MESSAGE "Form file does not exist"
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.

            begin_filename = SEARCH(begin_filename).
            DISPLAY begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_form C-Win
ON LEAVE OF begin_form IN FRAME FRAME-A /* Printer Form# */
    DO:
        ASSIGN begin_form.

        begin_filename = "barcode" + string(begin_form) + ".frm".

        DISPLAY begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-i-no C-Win
ON HELP OF begin_rm-i-no IN FRAME FRAME-A /* From RM Item# */
    DO:
        DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
        DEFINE VARIABLE opRecID   AS RECID     NO-UNDO.

        RUN windows/l-itmpo.w (cocode,'',begin_po-no:SCREEN-VALUE,end_po-no:SCREEN-VALUE,OUTPUT opCharVal,OUTPUT opRecID).
        IF opCharVal NE '' THEN
            SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
        APPLY 'ENTRY':U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        IF INDEX(PROGRAM-NAME(4),"asiLogin") <> 0 THEN
            RUN system/userLogOut.p (NO, 0).
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        IF reprintTag AND reprintLoadTag:SCREEN-VALUE NE "" THEN
            APPLY "LEAVE":U TO reprintLoadTag.
        RUN ok-button.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-i-no C-Win
ON HELP OF end_rm-i-no IN FRAME FRAME-A /* To RM Item# */
    DO:
        DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
        DEFINE VARIABLE opRecID   AS RECID     NO-UNDO.

        RUN windows/l-itmpo.w (cocode,'',begin_po-no:SCREEN-VALUE,end_po-no:SCREEN-VALUE,OUTPUT opCharVal,OUTPUT opRecID).
        IF opCharVal NE '' THEN
            SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
        APPLY 'ENTRY':U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reprintLoadtag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintLoadtag C-Win
ON HELP OF reprintLoadtag IN FRAME FRAME-A /* Tag# */
    DO:
        DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
        DEFINE VARIABLE opRecID   AS RECID     NO-UNDO.

        RUN addon/windows/l-ldtag6.w (cocode,YES,'',OUTPUT opCharVal,OUTPUT opRecID).
        IF opCharVal NE '' THEN
            SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
        APPLY 'ENTRY':U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintLoadtag C-Win
ON LEAVE OF reprintLoadtag IN FRAME FRAME-A /* Tag# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            DEFINE VARIABLE op-valid-lt AS LOG NO-UNDO.

            RUN validLoadtag(OUTPUT op-valid-lt).
            IF NOT op-valid-lt THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reprintTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintTag C-Win
ON VALUE-CHANGED OF reprintTag IN FRAME FRAME-A /* Reprint Tag */
    DO:
        ASSIGN {&SELF-NAME}.
        IF {&SELF-NAME} THEN 
        DO WITH FRAME {&FRAME-NAME}:
            DISABLE {&paramFields}.
            ENABLE reprintLoadtag.
            APPLY 'ENTRY':U TO reprintLoadtag.
        END.
        ELSE 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                reprintLoadtag:SCREEN-VALUE = ''
                reprintLoadtag:HIDDEN       = YES.
            ENABLE {&paramFields}.
            IF v-loadtag NE "TRIAD" THEN
                HIDE {&triad}.
            APPLY 'ENTRY':U TO v-po-list.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file C-Win
ON HELP OF scr-label-file IN FRAME FRAME-A /* Label Matrix Label File */
    DO:
        DEFINE VARIABLE chFile AS CHARACTER FORMAT "X(80)" NO-UNDO.
        DEFINE VARIABLE ll-ok  AS LOG       NO-UNDO.
        DEFINE VARIABLE v-path AS CHARACTER NO-UNDO.

        v-path = TRIM(scr-label-file:SCREEN-VALUE).

        IF TRIM(v-path) EQ "" THEN
            FIND FIRST sys-ctrl WHERE 
                sys-ctrl.company EQ cocode AND
                sys-ctrl.name EQ "CASLABEL"
                NO-LOCK NO-ERROR.

        IF AVAILABLE sys-ctrl THEN
            v-path = TRIM(sys-ctrl.char-fld).

        RUN sys\ref\char-fld-help.w(INPUT cocode,
            INPUT v-path,
            OUTPUT chFile).

        scr-label-file:SCREEN-VALUE = chFile.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-text-file-path
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-text-file-path C-Win
ON HELP OF scr-text-file-path IN FRAME FRAME-A /* Text File/Path */
    DO:
        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.

        SYSTEM-DIALOG GET-DIR ls-filename 
            TITLE "Select Path to Save"
            INITIAL-DIR scr-text-file-path
            UPDATE ll-ok.

        IF ll-ok THEN SELF:screen-value = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-text-file-path C-Win
ON LEAVE OF scr-text-file-path IN FRAME FRAME-A /* Text File/Path */
    DO:
        ASSIGN scr-text-file-path.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_16ths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_16ths C-Win
ON VALUE-CHANGED OF tb_16ths IN FRAME FRAME-A /* Show LWD in 16ths? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
    DEFINE INPUT  PARAMETER VisualStyle AS LONG.
    DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.



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

    FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    tb_16ths  = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = gcompany
            sys-ctrl.name     = "LOADTAG"
            sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
            sys-ctrl.char-fld = "ASI".
        MESSAGE "System control record NOT found. Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
        FIND CURRENT sys-ctrl NO-LOCK.
    END.
    ASSIGN
        v-loadtag = sys-ctrl.char-fld
        v-cas-lab = sys-ctrl.log-fld
        v-tags    = sys-ctrl.dec-fld.

    IF v-loadtag EQ "TRIAD" THEN begin_form = 4.

    DO TRANSACTION:
        {sys/inc/rmrecpt.i}
        v-rmrecpt = rmrecpt-cha EQ "RMTAG" AND rmrecpt-log.
        {sys/inc/rmbardir.i}     
    END.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "RMTAGS" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = gcompany
            sys-ctrl.name    = "RMTAGS"
            sys-ctrl.descrip = "Integer # of RM Loadtags to Print?  Logical to Calculate # of Tags?".
        MESSAGE "System control record NOT found. Number of RM LoadTags to Print?"
            UPDATE sys-ctrl.int-fld.
        MESSAGE "System control record NOT found. Auto Calculate RM LoadTags to Print?"
            UPDATE sys-ctrl.log-fld.
        FIND CURRENT sys-ctrl NO-LOCK.
    END.
    ASSIGN
        v-mult = sys-ctrl.int-fld
        v-calc = sys-ctrl.log-fld.
    IF v-mult LE 0 THEN v-mult = 1.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = gcompany
            sys-ctrl.name     = "RMWHSBIN"
            sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
            sys-ctrl.char-fld = "RMITEM".
        FIND CURRENT sys-ctrl NO-LOCK.
    END.
    v-bin = sys-ctrl.char-fld.

    DO WITH FRAME {&FRAME-NAME}:

        IF ip-param = YES THEN
            ASSIGN
                reprintTag              = YES
                reprintTag:SCREEN-VALUE = "YES".
        btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
        btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
        RUN enable_UI.

        /* {custom/usrprint.i} */

        IF v-loadtag NE "TRIAD" THEN HIDE {&triad}.

        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ gcompany 
            AND sys-ctrl.name    EQ "RMBARDIR" NO-ERROR.

        IF AVAILABLE sys-ctrl THEN
            ASSIGN
                scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld)
                scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
        ASSIGN
            scr-text-file-path:SCREEN-VALUE = rmbardir-desc
            userLabelPath                   = rmbardir-desc.

        IF rmbardir-int = 1 THEN 
        DO:
            FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
            IF AVAILABLE users AND users.user_program[3] NE "" THEN
                ASSIGN scr-text-file-path:SCREEN-VALUE = users.user_program[3]
                    userLabelPath                   = users.USER_program[3].       
        END.

        {methods/nowait.i}

        IF ip-param = YES THEN
        DO:
            APPLY "VALUE-CHANGED":U TO reprintTag IN FRAME {&FRAME-NAME}.
            reprintLoadtag:SCREEN-VALUE = ip-tag-no. 
        END.

        APPLY "entry" TO v-po-list.
    &IF DEFINED(AutoReprint) NE 0 &THEN
        {&WINDOW-NAME}:WINDOW-STATE = 2.
        RUN ok-button.
        RUN disable_UI.
    &ENDIF
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint C-Win 
PROCEDURE AutoPrint :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBarDir       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDB           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUserSpecific AS LOGICAL   NO-UNDO.

    IF scr-auto-print THEN 
    DO:
        RUN sys/ref/GetBarDir.p (INPUT cocode,
            INPUT "rmtag",
            OUTPUT cBarDir,
            OUTPUT cDB,
            OUTPUT lUserSpecific).

        IF lUserSpecific THEN 
            RUN custom/lmprint.p (INPUT cocode, 
                INPUT scr-label-file, 
                INPUT cDB,
                INPUT cBarDir).
        ELSE
            RUN custom/lmprint.p (INPUT cocode,
                INPUT scr-label-file,
                INPUT "",
                INPUT "").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr C-Win 
PROCEDURE convert-vend-comp-curr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DECIMAL DECIMALS 4 NO-UNDO.

    FIND FIRST vend WHERE
        vend.company EQ po-ord.company AND
        vend.vend-no EQ po-ord.vend-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE vend THEN
    DO:
        FIND FIRST b-company WHERE
            b-company.company EQ cocode
            NO-LOCK.

        IF vend.curr-code NE b-company.curr-code THEN
        DO:
            FIND FIRST currency WHERE
                currency.company EQ po-ord.company AND
                currency.c-code EQ vend.curr-code
                NO-LOCK NO-ERROR.

            IF AVAILABLE currency THEN
            DO:
                ip-cost = ip-cost * currency.ex-rate.

                RELEASE currency.
            END.
        END.

        RELEASE b-company.
        RELEASE vend.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag C-Win 
PROCEDURE create-loadtag :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipTagNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipItemOnly AS LOG NO-UNDO.

    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE tagNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld    AS DECIMAL   NO-UNDO.


    DO WHILE TRUE:
        tagNo = IF ipItemOnly THEN STRING(CAPS(w-po.i-no),'X(15)') + STRING(ipTagNo,'99999')
        ELSE STRING(w-po.po-no,'99999999') + STRING(w-po.line,'999') + STRING(ipTagNo,'9999999').
        IF NOT CAN-FIND(FIRST loadtag
            WHERE loadtag.company EQ cocode
            AND loadtag.item-type EQ YES
            AND loadtag.tag-no EQ tagNo) THEN LEAVE.
        ipTagNo = ipTagNo + 1.
    END. /* do while */

    CREATE loadtag.
    ASSIGN
        loadtag.company      = cocode
        loadtag.tag-no       = tagNo
        loadtag.item-type    = YES
        loadtag.po-no        = w-po.po-no
        loadtag.line         = w-po.line
        loadtag.job-no       = w-po.job-no
        loadtag.job-no2      = w-po.job-no2
        loadtag.form-no      = w-po.s-num
        loadtag.blank-no     = w-po.b-num
        loadtag.ord-no       = w-po.ord-no
        loadtag.i-no         = CAPS(w-po.i-no)
        loadtag.i-name       = w-po.i-name
        loadtag.qty          = w-po.ord-qty
        loadtag.qty-case     = ipQty
        loadtag.case-bundle  = 1
        loadtag.pallet-count = ipQty
        loadtag.loc          = w-po.loc
        loadtag.loc-bin      = w-po.loc-bin
        loadtag.tot-cases    = 0
        loadtag.sts          = "Printed"
        loadtag.tag-date     = TODAY
        loadtag.tag-time     = TIME.
   
    FIND CURRENT loadtag NO-LOCK NO-ERROR.

    IF v-rmrecpt THEN 
    DO:
        EMPTY TEMP-TABLE tt-mat.
  
        IF glCheckClosedStatus THEN 
        DO:

            RUN CheckPOLineStatus IN hdPoProcs(
                INPUT cocode,
                INPUT w-po.po-no,
                INPUT w-po.line
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
                LEAVE.
            
            RUN CheckJobStatus IN hdJobProcs(
                INPUT cocode,
                INPUT w-po.job-no,
                INPUT w-po.job-no2
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                LEAVE. 
        END.      

        /*RELEASE job.
        IF w-po.job-no NE "" AND w-po.s-num EQ ? THEN
        FIND FIRST job
            WHERE job.company EQ cocode
              AND job.job-no  EQ w-po.job-no
              AND job.job-no2 EQ w-po.job-no2
            NO-LOCK NO-ERROR.
    
        IF AVAIL job THEN DO:
          ld = 0.
    
          FOR EACH job-mat
              WHERE job-mat.company EQ job.company
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ job.job-no
                AND job-mat.job-no2 EQ job.job-no2
                AND job-mat.rm-i-no EQ w-po.i-no
              NO-LOCK:
            CREATE tt-mat.
            ASSIGN
             tt-mat.frm = job-mat.frm
             tt-mat.qty = job-mat.qty
             ld         = ld + job-mat.qty.
          END.
    
          FOR EACH tt-mat:
            tt-mat.qty = w-po.rcpt-qty * (tt-mat.qty / ld).
          END.
        END.
    
        ELSE DO: */
        CREATE tt-mat.
        ASSIGN
            tt-mat.frm = w-po.s-num
            tt-mat.qty = ipQty.
        /*        tt-mat.qty = w-po.rcpt-qty. */
        /*END.*/

        FOR EACH tt-mat:
            i = 0.
            RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT i) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


            CREATE rm-rctd.
            ASSIGN
                rm-rctd.r-no      = i
                rm-rctd.rct-date  = TODAY
                rm-rctd.company   = cocode
                rm-rctd.rita-code = "R"
                rm-rctd.i-name    = w-po.i-name
                rm-rctd.i-no      = loadtag.i-no
                rm-rctd.job-no    = loadtag.job-no
                rm-rctd.job-no2   = loadtag.job-no2
                rm-rctd.po-no     = STRING(loadtag.po-no)
                rm-rctd.po-line   = loadtag.line
                rm-rctd.s-num     = tt-mat.frm
                rm-rctd.b-num     = w-po.b-num
                rm-rctd.qty       = tt-mat.qty
                rm-rctd.pur-uom   = w-po.cons-uom
                rm-rctd.cost      = w-po.cost
                rm-rctd.cost-uom  = w-po.pr-uom
                rm-rctd.loc       = loadtag.loc
                rm-rctd.loc-bin   = loadtag.loc-bin
                rm-rctd.tag       = loadtag.tag-no
                rm-rctd.user-id   = USERID("nosweat")
                rm-rctd.upd-date  = TODAY
                rm-rctd.upd-time  = TIME.

            RELEASE rm-bin.
            IF w-po.po-no EQ 0 THEN
                FIND FIRST rm-bin NO-LOCK
                    WHERE rm-bin.company EQ item.company
                    AND rm-bin.loc     EQ loadtag.loc
                    AND rm-bin.i-no    EQ loadtag.i-no
                    AND rm-bin.loc-bin EQ loadtag.loc-bin
                    NO-ERROR.

            IF AVAILABLE rm-bin THEN 
            DO:
                FIND FIRST item NO-LOCK
                    WHERE item.company EQ rm-rctd.company
                    AND item.i-no    EQ rm-rctd.i-no
                    USE-INDEX i-no NO-ERROR.
                ASSIGN
                    rm-rctd.cost     = rm-bin.cost
                    rm-rctd.cost-uom = item.cons-uom.
            END.

            RUN get-matrix.

            DELETE tt-mat.
        END.
    END.  /* if v-rmrecpt */

    FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWPO C-Win 
PROCEDURE createWPO :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-qty AS DECIMAL NO-UNDO.

    FIND FIRST vend NO-LOCK WHERE
        vend.company EQ cocode AND
        vend.vend-no EQ po-ord.vend-no
        NO-ERROR.

    CREATE w-po.
    ASSIGN
        w-po.acknowledge    = po-ord.acknowledge
        w-po.actnum         = po-ordl.actnum
        w-po.adders         = po-ordl.adders
        w-po.addr[1]        = po-ord.addr[1]
        w-po.addr[2]        = po-ord.addr[2]
        w-po.b-num          = po-ordl.b-num
        w-po.bill-to        = po-ord.bill-to
        w-po.buyer          = po-ord.buyer
        w-po.carrier        = po-ord.carrier
        w-po.city           = po-ord.city
        w-po.company        = po-ord.company
        w-po.company-name   = company.name
        w-po.cons-cost      = po-ordl.cons-cost
        w-po.cons-qty       = po-ordl.cons-qty
        w-po.cons-uom       = po-ordl.cons-uom
        w-po.pr-uom         = po-ordl.pr-uom
        w-po.contact        = po-ord.contact
        w-po.curr-code[1]   = po-ord.curr-code[1]
        w-po.curr-code[2]   = po-ord.curr-code[2]
        w-po.cust-no        = po-ord.cust-no
        w-po.deleted        = po-ord.deleted
        w-po.disc           = po-ordl.disc
        w-po.dscr[1]        = po-ordl.dscr[1]
        w-po.dscr[2]        = po-ordl.dscr[2]
        w-po.due-date       = po-ord.due-date
        w-po.ex-rate        = po-ord.ex-rate
        w-po.fob-code       = po-ord.fob-code
        w-po.frt-pay        = po-ord.frt-pay
        w-po.i-name         = po-ordl.i-name
        w-po.i-no           = po-ordl.i-no
        w-po.item-type      = po-ordl.item-type
        w-po.j-no           = po-ordl.j-no
        w-po.job-no         = po-ordl.job-no
        w-po.job-no2        = po-ordl.job-no2
        w-po.last-ship-date = po-ord.last-ship-date
        w-po.line           = po-ordl.line
        w-po.opened         = po-ord.opened
        w-po.ord-no         = po-ordl.ord-no
        w-po.ord-qty        = po-ordl.ord-qty
        w-po.over-pct       = po-ord.over-pct
        w-po.po-change-date = po-ord.po-change-date
        w-po.po-date        = po-ord.po-date
        w-po.po-no          = po-ord.po-no
        w-po.pr-qty-uom     = po-ordl.pr-qty-uom
        w-po.pr-uom         = po-ordl.pr-uom
        w-po.printed        = po-ord.printed
        w-po.pur-cnt        = po-ordl.pur-cnt
        w-po.received       = po-ord.received
        w-po.rel-qty        = po-ordl.rel-qty
        w-po.s-len          = /* IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE */ po-ordl.s-len
        w-po.s-num          = po-ordl.s-num
        w-po.s-wid          = po-ordl.s-wid
        w-po.setup          = po-ordl.setup
        w-po.ship-addr[1]   = po-ord.ship-addr[1]
        w-po.ship-addr[2]   = po-ord.ship-addr[2]
        w-po.ship-city      = po-ord.ship-city
        w-po.ship-i[1]      = po-ordl.ship-i[1]
        w-po.ship-i[2]      = po-ordl.ship-i[2]
        w-po.ship-i[3]      = po-ordl.ship-i[3]
        w-po.ship-i[4]      = po-ordl.ship-i[4]
        w-po.ship-id        = po-ord.ship-id
        w-po.ship-name      = po-ord.ship-name
        w-po.ship-no        = po-ord.ship-no
        w-po.ship-state     = po-ord.ship-state
        w-po.ship-zip       = po-ord.ship-zip
        w-po.spec-i[1]      = po-ord.spec-i[1]
        w-po.spec-i[2]      = po-ord.spec-i[2]
        w-po.spec-i[3]      = po-ord.spec-i[3]
        w-po.spec-i[4]      = po-ord.spec-i[4]
        w-po.stat           = po-ord.stat
        w-po.state          = po-ord.state
        w-po.t-cost         = po-ord.t-cost
        w-po.t-freight      = po-ord.t-freight
        w-po.t-inv-qty      = po-ordl.t-inv-qty
        w-po.t-rec-qty      = po-ordl.t-rec-qty
        w-po.t-rel-qty      = po-ordl.t-rel-qty
        w-po.tag-date       = TODAY
        w-po.tax            = po-ord.tax
        w-po.tax-gr         = po-ord.tax-gr
        w-po.tax-id         = po-ord.tax-id
        w-po.terms          = po-ord.terms
        w-po.type           = po-ord.type
        w-po.under-pct      = po-ord.under-pct
        w-po.upd-date       = po-ord.upd-date
        w-po.upd-time       = po-ord.upd-time
        w-po.vend-i-no      = po-ordl.vend-i-no
        w-po.vend-name      = IF AVAILABLE vend THEN vend.name ELSE ''
        w-po.vend-no        = po-ord.vend-no
        w-po.zip            = po-ord.zip
        num-rec             = num-rec + 1
        /* w-po.mch-cod = v-mch-cod*/ .  

    /*IF po-ordl.pr-uom EQ "L" THEN w-po.cost = po-ordl.t-cost / po-ordl.cons-qty.
  
    ELSE*/ 
    w-po.cost = po-ordl.cost /*+
                   (po-ordl.setup /
                    ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)) */ .

    RUN rm/getpocst.p (BUFFER po-ordl, w-po.pr-uom, INPUT-OUTPUT w-po.cost).

    RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cost).
    RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.setup).
    RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cons-cost).

    FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
        ASSIGN
            w-po.loc      = item.loc
            w-po.loc-bin  = item.loc-bin
            w-po.cons-uom = item.cons-uom
            w-po.case-l   = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-l ELSE 0 
            w-po.case-w   = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-w ELSE 0 
            w-po.case-d   = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-d ELSE 0 .

    ASSIGN
        v-overrun = IF AVAILABLE po-ordl THEN po-ordl.over-pct
                  ELSE IF AVAILABLE po-ord  THEN po-ord.over-pct
                  ELSE IF AVAILABLE vend    THEN vend.over-pct
                  ELSE 0
        v-qty     = po-ordl.ord-qty.
    IF w-po.cons-uom NE po-ordl.pr-qty-uom THEN
        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
            w-po.cons-uom, IF AVAILABLE ITEM THEN ITEM.basis-w ELSE 0,
            w-po.s-len,  w-po.s-wid, IF AVAILABLE ITEM THEN item.s-dep ELSE 0,
            v-qty, OUTPUT v-qty).
    w-po.overrun-qty = v-qty + (v-qty * (v-overrun / 100)).

    IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
    DO:
        RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
            INPUT  cocode,
            OUTPUT iWarehouseLength
            ).
        IF v-bin NE 'RMITEM' THEN
            ASSIGN
                w-po.loc     = SUBSTR(v-bin,1,iWarehouseLength)
                w-po.loc-bin = SUBSTR(v-bin,iWarehouseLength + 1).
        IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
        DO:
            FIND FIRST rm-bin WHERE rm-bin.company EQ cocode
                AND rm-bin.loc EQ locode
                AND rm-bin.i-no EQ ''
                AND rm-bin.loc-bin NE '' NO-ERROR.
            ASSIGN
                w-po.loc     = IF AVAILABLE loc THEN loc.loc ELSE ''
                w-po.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
        END.
    END.
    /* Add .49 to round up and add 1 for extra tag   */

    IF w-po.rcpt-qty NE 0 THEN
        w-po.total-tags = ((po-ordl.ord-qty / w-po.rcpt-qty) + .49).
    IF NOT CAN-DO("SSLABEL,CentBox",v-loadtag) THEN
        w-po.total-tags = w-po.total-tags + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWPOfromItem C-Win 
PROCEDURE createWPOfromItem :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipBeginItem AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEndItem AS CHARACTER NO-UNDO.

    FOR EACH item NO-LOCK WHERE
        item.company EQ cocode AND
        item.i-no GE ipBeginItem AND
        ITEM.i-no LE ipEndItem:

        FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
            AND vend.vend-no EQ item.vend-no NO-ERROR.
        CREATE w-po.
        ASSIGN
            w-po.company      = item.company
            w-po.company-name = company.name
            w-po.cons-uom     = item.cons-uom
            w-po.dscr[1]      = item.i-dscr
            w-po.i-name       = item.i-name
            w-po.i-no         = CAPS(item.i-no)
            w-po.item-type    = YES
            w-po.loc          = item.loc
            w-po.loc-bin      = item.loc-bin
            w-po.s-len        = IF item.s-wid NE 0 THEN item.s-len ELSE 12
            w-po.s-num        = item.s-dep
            w-po.s-wid        = IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid
            w-po.total-tags   = IF CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 0 ELSE 1
            w-po.vend-i-no    = IF AVAILABLE vend THEN item.vend-item ELSE ''
            w-po.vend-name    = IF AVAILABLE vend THEN vend.name ELSE ''
            w-po.vend-no      = item.vend-no
            w-po.cost         = IF v-avgcost THEN ITEM.avg-cost ELSE ITEM.last-cost
            w-po.pr-uom       = item.cons-uom
            w-po.case-l       = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-l ELSE 0 
            w-po.case-w       = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-w ELSE 0 
            w-po.case-d       = IF LOOKUP(ITEM.mat-type,"C,D") GT 0 THEN item.case-d ELSE 0
            num-rec           = num-rec + 1.

        IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
        DO:
            RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
                INPUT  cocode,
                OUTPUT iWarehouseLength
                ).
            IF v-bin NE 'RMITEM' THEN
                ASSIGN
                    w-po.loc     = SUBSTR(v-bin,1,iWarehouseLength)
                    w-po.loc-bin = SUBSTR(v-bin,iWarehouseLength + 1).
            IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
            DO:
                FIND FIRST rm-bin NO-LOCK WHERE rm-bin.company EQ cocode
                    AND rm-bin.loc EQ locode
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin NE '' NO-ERROR.
                ASSIGN
                    w-po.loc     = IF AVAILABLE loc THEN loc.loc ELSE ''
                    w-po.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
            END. /* if not */
        END. /* if not */
    END. /* each item */

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
    DISPLAY reprintTag v-po-list v-job-list begin_po-no end_po-no begin_job 
        begin_job2 end_job end_job2 begin_rm-i-no end_rm-i-no scr-auto-print 
        scr-label-file rd_status tb_16ths scr-text-file-path begin_form 
        begin_labels begin_filename tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 reprintTag v-po-list v-job-list begin_po-no end_po-no begin_job 
        begin_job2 end_job end_job2 begin_rm-i-no end_rm-i-no scr-auto-print 
        scr-label-file rd_status tb_16ths begin_form begin_labels 
        begin_filename tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-po C-Win 
PROCEDURE from-po :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.i-no      GE v-fitem[1]
        AND po-ordl.i-no      LE v-fitem[2]
        AND po-ordl.item-type EQ YES
        AND (v-stat EQ "A"                         OR
        (v-stat EQ "C" AND NOT po-ord.opened) OR
        (v-stat EQ "O" AND po-ord.opened))
        AND (CAN-FIND(ttblJob WHERE trim(ttblJob.job-no) EQ trim(po-ordl.job-no)
        AND ttblJob.job-no2 EQ po-ordl.job-no2) OR
        NOT CAN-FIND(FIRST ttblJob))
        USE-INDEX po-no /*BREAK BY po-ordl.i-no*/:
        /*IF FIRST-OF(po-ordl.i-no) THEN*/ RUN createWPO.
    END. /* each po-ordl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix C-Win 
PROCEDURE get-matrix :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  LIKE rm-rctd.qty NO-UNDO.
    DEFINE VARIABLE lv-out-cost LIKE rm-rctd.cost NO-UNDO.
    DEFINE VARIABLE lv-qty-uom  LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.

    FIND item NO-LOCK WHERE item.company EQ cocode
        AND item.i-no EQ rm-rctd.i-no
        USE-INDEX i-no NO-ERROR.
    IF NOT AVAILABLE item THEN LEAVE.

    IF item.cons-uom EQ '' THEN
        item.cons-uom = rm-rctd.pur-uom.

    ASSIGN
        lv-qty-uom  = item.cons-uom
        lv-cost-uom = item.cons-uom
        v-dep       = item.s-dep.

    FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ rm-rctd.company
        AND po-ordl.po-no EQ INTEGER(rm-rctd.po-no)
        AND po-ordl.i-no EQ rm-rctd.i-no
        AND po-ordl.job-no  EQ rm-rctd.job-no
        AND po-ordl.job-no2 EQ rm-rctd.job-no2
        AND po-ordl.item-type EQ YES
        AND po-ordl.s-num EQ rm-rctd.s-num NO-ERROR.
    IF AVAILABLE po-ordl THEN
    DO:
        ASSIGN
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid
            v-bwt = 0.
        {rm/pol-dims.i}
    END.
    ELSE
    DO:
        FIND FIRST job NO-LOCK WHERE job.company EQ cocode
            AND job.job-no  EQ rm-rctd.job-no
            AND job.job-no2 EQ rm-rctd.job-no2 NO-ERROR.
        IF AVAILABLE job THEN
        DO:
            FIND FIRST job-mat NO-LOCK WHERE job-mat.company EQ rm-rctd.company
                AND job-mat.job EQ job.job
                AND job-mat.i-no EQ rm-rctd.i-no
                AND job-mat.frm EQ rm-rctd.s-num NO-ERROR.
            IF AVAILABLE job-mat THEN
                ASSIGN 
                    v-len = job-mat.len
                    v-wid = job-mat.wid
                    v-bwt = job-mat.basis-w.
        END.
        IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
        IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid
            ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
        IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.

        ASSIGN
            lv-qty-uom  = item.cons-uom
            lv-cost-uom = item.cons-uom.
    END.

    /* convert qty */
    RUN custom/convquom.p(gcompany,rm-rctd.pur-uom,lv-qty-uom,v-bwt,v-len,
        INPUT v-wid,INPUT v-dep,INPUT rm-rctd.qty,OUTPUT lv-out-qty).

    /* convert cost */
    IF rm-rctd.cost-uom EQ 'L' THEN
        lv-out-cost = DEC(rm-rctd.cost) / lv-out-qty.
    ELSE
        RUN custom/convcuom.p(gcompany,rm-rctd.cost-uom,lv-cost-uom,
            v-bwt,v-len,v-wid,v-dep,rm-rctd.cost,
            OUTPUT lv-out-cost).

    IF w-po.add-setup AND w-po.type NE "S" AND lv-out-qty NE 0 THEN
        lv-out-cost = lv-out-cost + (w-po.setup / lv-out-qty).

    ASSIGN
        ext-cost         = ROUND(lv-out-qty * lv-out-cost,2)
        rm-rctd.cost     = lv-out-cost
        rm-rctd.cost-uom = lv-cost-uom
        rm-rctd.qty      = lv-out-qty
        rm-rctd.pur-uom  = lv-qty-uom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button C-Win 
PROCEDURE ok-button :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE op-valid-lt AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF scr-text-file-path:SCREEN-VALUE = "" AND
            userLabelPath <> "" THEN        
            scr-text-file-path:SCREEN-VALUE = userLabelPath.

        ASSIGN {&displayed-objects}.
    END.
    ASSIGN
        cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" ELSE "" .
    FOR EACH tt-po-print:
        DELETE tt-po-print .
    END.

    ASSIGN
        v-out      = scr-text-file-path
        v-init-dir = v-out.

    IF scr-auto-print AND scr-label-file = "" THEN
    DO:
        MESSAGE "Label Matrix Label File cannot be blank."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.

    IF reprintTag THEN 
    DO:

        RUN validLoadtag(OUTPUT op-valid-lt).

        IF op-valid-lt THEN
            RUN reprintTag.

        APPLY 'ENTRY':U TO reprintLoadtag IN FRAME {&FRAME-NAME}.
    END.
    ELSE 
    DO:
        RUN run-report NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN RETURN.
        APPLY "entry" TO v-po-list IN FRAME {&FRAME-NAME}.
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
        SAVE-AS
        USE-FILENAME

        UPDATE OKpressed.
    IF NOT OKpressed THEN RETURN NO-APPLY.

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
    RUN scr-rpt.w (list-name,c-win:TITLE). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputTagHeader C-Win 
PROCEDURE outputTagHeader :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    PUT UNFORMATTED 
        'Tag#,~
Acknowledgement,~
Account No,~
Ship To Address[1],~
Ship To Address[2],~
Blank #,~
Bill to,~
Buyer,~
Shipping Carrier,~
City,~
Company,~
Company Name,~
Costs,~
Consumption Quantity,~
Unit of Measure,~
Contact,~
Unit Cost,~
Currency Code[1],~
Currency Code[2],~
Customer Number,~
Deleted?,~
Discount,~
Description[1],~
Description[2],~
Required Date,~
ex-rate,~
FOB Origin/Dest,~
Freight Payment,~
Name,~
Item#,~
Item Type,~
Internial Job Number,~
Job Number,~
Run #,~
Last Ship Date,~
Line,~
Whse,~
Bin,~
opened,~
Customer Order Number,~
Order Quantity,~
Overrun %,~
Partial Qty,~
Date Changed,~
PO Date,~
PO Number,~
Purchase Quantity Uom,~
Purchased UOM,~
Printed?,~
Purchase Count,~
Receipt Qty,~
Received,~
Release Quantity,~
Sheet Len,~
Sheet #,~
Sheet Wid,~
Setup Charge,~
Shipping Address[1],~
Shipping Address[2],~
Shipping City,~
Shipping Instructions[1],~
Shipping Instructions[2],~
Shipping Instructions[3],~
Shipping Instructions[4],~
Ship To,~
Shipping Name,~
Ship To Number,~
Shipping State,~
Shipping Zip,~
Special Instructions[1],~
Special Instructions[2],~
Special Instructions[3],~
Special Instructions[4],~
Status,~
State,~
Total Cost,~
Total Freight,~
Total Invoiced,~
Total Received,~
Total Quantity Released,~
Tag Date,~
Tax,~
Tax Group,~
Tax Exempt No.,~
Payment Terms,~
Total Tags,~
Type,~
Underrun %,~
Updated Date,~
Updated Time,~
Vendor Item #,~
Vendor Name,~
Vendor,~
Zip Code,~
First Machine,~
First Internal Machine,~
First Press,~
CasePalletLength,~
CasePalletWidth,~
CasePalletDepth,~
CreatedBy,~
CreateDate,~
CreateTime,~
PrintDate,~
PrintTime,~
Vendor Tag' SKIP 
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputTagLine C-Win 
PROCEDURE outputTagLine :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipQty AS INTEGER NO-UNDO.

    ASSIGN 
        v-mch-cod          = ""
        cFirstMachPress    = "" 
        cFirstInternalMach = "" . 

    RUN GetOperation IN hdJobProcs (cocode, w-po.job-no, INTEGER(w-po.job-no2),INTEGER(w-po.s-num),"First", INPUT-OUTPUT v-mch-cod).
    RUN GetOperation IN hdJobProcs (cocode, w-po.job-no, INTEGER(w-po.job-no2),INTEGER(w-po.s-num),"Press", INPUT-OUTPUT cFirstMachPress).
    RUN GetOperation IN hdJobProcs (cocode, w-po.job-no, INTEGER(w-po.job-no2),INTEGER(w-po.s-num),"Internal", INPUT-OUTPUT cFirstInternalMach).
  
    PUT UNFORMATTED
        '"' removeChars(loadtag.tag-no)
        '","' w-po.acknowledge
        '","' removeChars(w-po.actnum)
        '","' removeChars(w-po.addr[1])
        '","' removeChars(w-po.addr[2])
        '","' w-po.b-num
        '","' removeChars(w-po.bill-to)
        '","' removeChars(w-po.buyer)
        '","' removeChars(w-po.carrier)
        '","' removeChars(w-po.city)
        '","' removeChars(w-po.company)
        '","' removeChars(w-po.company-name)
        '","' w-po.cons-cost
        '","' w-po.cons-qty
        '","' w-po.cons-uom
        '","' removeChars(w-po.contact)
        '","' w-po.cost
        '","' removeChars(w-po.curr-code[1])
        '","' removeChars(w-po.curr-code[2])
        '","' removeChars(w-po.cust-no)
        '","' w-po.deleted
        '","' w-po.disc
        '","' removeChars(w-po.dscr[1])
        '","' removeChars(w-po.dscr[2])
        '","' w-po.due-date
        '","' w-po.ex-rate
        '","' removeChars(w-po.fob-code)
        '","' w-po.frt-pay
        '","' removeChars(w-po.i-name)
        '","' removeChars(w-po.i-no)
        '","' w-po.item-type
        '","' w-po.j-no
        '","' removeChars(w-po.job-no)
        '","' w-po.job-no2
        '","' w-po.last-ship-date
        '","' w-po.line
        '","' removeChars(w-po.loc)
        '","' removeChars(w-po.loc-bin)
        '","' w-po.opened
        '","' w-po.ord-no
        '","' w-po.ord-qty
        '","' w-po.over-pct
        '","' w-po.partial
        '","' w-po.po-change-date
        '","' w-po.po-date
        '","' w-po.po-no
        '","' w-po.pr-qty-uom
        '","' w-po.pr-uom
        '","' w-po.printed
        '","' w-po.pur-cnt
        '","' w-po.rcpt-qty
        '","' w-po.received
        '","' w-po.rel-qty
        '","' w-po.s-len
        '","' w-po.s-num
        '","' w-po.s-wid
        '","' w-po.setup
        '","' removeChars(w-po.ship-addr[1])
        '","' removeChars(w-po.ship-addr[2])
        '","' removeChars(w-po.ship-city)
        '","' removeChars(w-po.ship-i[1])
        '","' removeChars(w-po.ship-i[2])
        '","' removeChars(w-po.ship-i[3])
        '","' removeChars(w-po.ship-i[4])
        '","' w-po.ship-id
        '","' removeChars(w-po.ship-name)
        '","' w-po.ship-no
        '","' w-po.ship-state
        '","' w-po.ship-zip
        '","' removeChars(w-po.spec-i[1])
        '","' removeChars(w-po.spec-i[2])
        '","' removeChars(w-po.spec-i[3])
        '","' removeChars(w-po.spec-i[4])
        '","' w-po.stat
        '","' w-po.state
        '","' w-po.t-cost
        '","' w-po.t-freight
        '","' w-po.t-inv-qty
        '","' w-po.t-rec-qty
        '","' w-po.t-rel-qty
        '","' w-po.tag-date
        '","' w-po.tax
        '","' w-po.tax-gr
        '","' w-po.tax-id
        '","' w-po.terms
        '","' w-po.total-tags
        '","' w-po.type
        '","' w-po.under-pct
        '","' w-po.upd-date
        '","' w-po.upd-time
        '","' removeChars(w-po.vend-i-no)
        '","' removeChars(w-po.vend-name)
        '","' w-po.vend-no
        '","' w-po.zip
        '","' removeChars(v-mch-cod)   
        '","' removeChars(cFirstInternalMach)
        '","' removeChars(cFirstMachPress)
        '","' w-po.case-l
        '","' w-po.case-w
        '","' w-po.case-d    
        '","' loadtag.createUser
        '","' STRING(loadtag.tag-date,"99/99/9999")
        '","' STRING(loadtag.tag-time,"HH:MM") IF loadtag.tag-time LT 43140 THEN " AM" ELSE " PM"
        '","' STRING(TODAY,"99/99/9999")
        '","' STRING(TIME,"HH:MM") IF TIME LT 43140 THEN " AM" ELSE " PM"
        '","' loadtag.misc-char[1]
        '"'   SKIP .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-cost-proc C-Win 
PROCEDURE po-cost-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-len      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep      LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty LIKE rm-rctd.qty NO-UNDO.
    DEFINE VARIABLE lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE lv-cost    AS DECIMAL DECIMALS 10 NO-UNDO.

    FOR EACH w-po WHERE w-po.po-no NE 0,
        FIRST po-ordl WHERE
        po-ordl.company EQ cocode AND
        po-ordl.po-no   EQ w-po.po-no AND
        po-ordl.LINE EQ w-po.line
        NO-LOCK:

        FIND FIRST ITEM WHERE
            item.company EQ po-ordl.company AND
            item.i-no EQ po-ordl.i-no
            USE-INDEX i-no
            NO-LOCK NO-ERROR.

        ASSIGN
            lv-qty-uom = po-ordl.pr-qty-uom
            v-len      = po-ordl.s-len
            v-wid      = po-ordl.s-wid
            v-bwt      = 0.

        IF AVAILABLE ITEM THEN
        DO:
            {rm/pol-dims.i}
        END.

        IF w-po.cons-uom EQ lv-qty-uom THEN
            lv-out-qty = w-po.rcpt-qty.
        ELSE
            RUN custom/convquom.p (INPUT po-ordl.company,
                INPUT w-po.cons-uom,
                INPUT lv-qty-uom,
                INPUT v-bwt,
                INPUT v-len,
                INPUT v-wid,
                INPUT v-dep,
                INPUT w-po.rcpt-qty,
                OUTPUT lv-out-qty).

        FIND FIRST po-ord WHERE
            po-ord.company EQ po-ordl.company AND
            po-ord.po-no EQ po-ordl.po-no
            NO-LOCK NO-ERROR.

        IF lv-out-qty LT po-ordl.ord-qty THEN
            lv-cost = po-ordl.cost +
                (po-ordl.setup /
                ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
        ELSE
            ASSIGN
                lv-cost        = po-ordl.cost
                w-po.add-setup = IF AVAILABLE po-ord AND po-ord.type NE "S" THEN YES
                                    ELSE NO.

        IF lv-cost EQ ? THEN lv-cost = 0.
        w-po.cost = lv-cost.

        RUN rm/getpocst.p (BUFFER po-ordl, w-po.pr-uom, INPUT-OUTPUT w-po.cost).

        RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cost).

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidatePo C-Win 
PROCEDURE pValidatePo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiPoNo AS INTEGER NO-UNDO .
    DEFINE OUTPUT PARAMETER oplCheckError AS LOGICAL NO-UNDO .
    {methods/lValidateError.i YES}    
    FIND FIRST bf-po-ord NO-LOCK
        WHERE bf-po-ord.company EQ cocode
        AND bf-po-ord.po-no EQ ipiPoNo
        AND bf-po-ord.stat EQ "H" NO-ERROR .
    IF AVAILABLE bf-po-ord THEN 
    DO:
        MESSAGE "PO# '" + STRING(ipiPoNo) + "' is on hold and cannot print load tags for a PO on hold"
            VIEW-AS ALERT-BOX ERROR .
        oplCheckError = YES .
    END.
    {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprintTag C-Win 
PROCEDURE reprintTag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE numberTags AS INTEGER NO-UNDO.

    FOR EACH ttLoadTag
        WHERE ttLoadTag.loadTag GT ""
        :
        reprintLoadTag = ttLoadTag.loadTag.
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company      EQ cocode
            AND loadtag.item-type    EQ YES
            AND (loadtag.tag-no      EQ reprintLoadTag
            OR loadtag.misc-char[1] EQ reprintLoadTag)
            NO-ERROR.
        IF NOT AVAILABLE loadtag THEN 
        DO:
        &IF DEFINED(AutoReprint) EQ 0 &THEN
            MESSAGE 'Loadtag Record Not Found!' VIEW-AS ALERT-BOX ERROR.
        &ENDIF
            RETURN.
        END.
        FIND FIRST po-ord NO-LOCK
            WHERE po-ord.company EQ loadtag.company
            AND po-ord.po-no   EQ loadtag.po-no
            NO-ERROR.
        IF AVAILABLE po-ordl THEN
            RELEASE po-ordl.
        IF AVAILABLE po-ord THEN
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company EQ po-ord.company
                AND po-ordl.po-no   EQ po-ord.po-no
                AND po-ordl.i-no    EQ loadtag.i-no
                NO-ERROR.
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ po-ord.company
            AND cust.cust-no EQ po-ord.cust-no
            NO-ERROR.
        IF AVAILABLE po-ord AND AVAILABLE po-ordl THEN
            RUN createWPO.
        ELSE
            RUN createWPOfromItem (loadtag.i-no,loadtag.i-no).
    
        FIND FIRST rm-bin NO-LOCK
            WHERE rm-bin.company EQ loadtag.company
            AND rm-bin.tag     EQ loadtag.tag-no
            AND rm-bin.i-no    EQ loadtag.i-no
            AND rm-bin.loc     EQ loadtag.loc
            AND rm-bin.loc-bin EQ loadtag.loc-bin 
            NO-ERROR.
        ASSIGN
        &IF DEFINED(AutoReprint) EQ 0 &THEN
            w-po.rcpt-qty   = IF AVAILABLE rm-bin THEN rm-bin.qty ELSE loadtag.pallet-count
        &ELSE
            w-po.rcpt-qty   = ttLoadTag.qty
        &ENDIF
            w-po.tag-date   = loadtag.tag-date
            w-po.total-tags = IF AVAILABLE cust AND cust.int-field[1] GT 0 THEN cust.int-field[1]
                          ELSE IF v-mult GT 0 THEN v-mult ELSE 1.
        CASE cBarCodeProgram:
            WHEN "" THEN 
                DO:
                    ERROR-STATUS:ERROR = NO.
                    RUN setOutputFile.
                    IF ERROR-STATUS:ERROR THEN RETURN.
                    OUTPUT TO VALUE(v-out) .
                    RUN outputTagHeader.
                    DO numberTags = 1 TO w-po.total-tags:
                        RUN outputTagLine (w-po.rcpt-qty).
                    END.
                    OUTPUT CLOSE.
                END.
            WHEN "xprint" THEN 
                DO:
                    CREATE tt-po-print.
                    BUFFER-COPY w-po TO tt-po-print.
                    ASSIGN 
                        tt-po-print.tag-no   = IF AVAILABLE loadtag THEN loadtag.tag-no       ELSE ""
                        tt-po-print.vend-tag = IF AVAILABLE loadtag THEN loadtag.misc-char[1] ELSE ""
                        .    
                END.
        END CASE.
    END. /* each ttloadtag */
    CASE cBarCodeProgram:
        WHEN "" THEN 
            DO:
                RUN AutoPrint.
            END.
        WHEN "xprint" THEN
        RUN xprint-tag.
    END CASE.
    EMPTY TEMP-TABLE ttLoadTag.
  &IF DEFINED(AutoReprint) EQ 0 &THEN
    MESSAGE 'Loadtag Reprint Complete!' VIEW-AS ALERT-BOX.
    APPLY 'ENTRY':U TO reprintLoadtag IN FRAME {&FRAME-NAME}.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-barone C-Win 
PROCEDURE run-barone :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-TagText AS cha NO-UNDO.

    DEFINE VARIABLE iReturnResult AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cProgramName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.

    ASSIGN
        cProgramName = "c:\program files\bar-one 6 pro-plus\labels.exe "
        cFileName    = "R:\ASI_GUI9\SOURCE\custom\century.lab".

    RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
        iReturnResult).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-lmw C-Win 
PROCEDURE run-lmw :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-TagText AS cha NO-UNDO.

    DEFINE VARIABLE iReturnResult AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cProgramName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.

    ASSIGN
        cFileName           = "custom\interpack.qdf"
        FILE-INFO:FILE-NAME = cFileName
        cFileName           = FILE-INFO:FULL-PATHNAME.

    RUN custom/runlmw.p (OUTPUT cprogramname).

    RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
        iReturnResult).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-po-no         LIKE po-ord.po-no NO-UNDO.
    DEFINE VARIABLE lv-job-no        LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE lv-job-no2       LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE ll               AS LOG       NO-UNDO.
    DEFINE VARIABLE li               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE choice           AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-middlesex-job AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-middlesex-po  AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-itemOnly      AS LOG       NO-UNDO.

    DEFINE BUFFER b-item FOR item.
    DEFINE BUFFER b-w-po FOR w-po.
    DEFINE VARIABLE lv-tag-no        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-how-many-tags AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-b-wpo-created  AS LOG     NO-UNDO.
    DEFINE VARIABLE choice2          AS LOGICAL INIT YES NO-UNDO.
    DEFINE VARIABLE lCheckError      AS LOGICAL NO-UNDO .

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        v-fpo-no[1] = begin_po-no
        v-fpo-no[2] = end_po-no
        v-fitem[1]  = begin_rm-i-no
        v-fitem[2]  = end_rm-i-no
        form#       = begin_form
        copy_count  = begin_labels
        form_fid    = begin_filename
        v-stat      = rd_status.

    EMPTY TEMP-TABLE w-po.
    EMPTY TEMP-TABLE tt-tag.
    EMPTY TEMP-TABLE ttblJob.

    IF v-job-list NE "" AND
        (ASC(SUBSTRING(v-job-list,LENGTH(v-job-list),1)) = 10 OR
        ASC(SUBSTRING(v-job-list,LENGTH(v-job-list),1)) = 13)
        THEN v-job-list = SUBSTRING(v-job-list,1,LENGTH(v-job-list) - 1).

    DO i = 1 TO NUM-ENTRIES(v-job-list).
        ASSIGN
            ll         = YES
            lv-job-no  = ""
            lv-job-no2 = "".

        DO li = 1 TO LENGTH(ENTRY(i,v-job-list)):
            IF INDEX("/:-",SUBSTR(ENTRY(i,v-job-list),li,1)) GT 0 THEN
                IF ll THEN ll = NO.
                ELSE LEAVE.
            ELSE
                IF ll THEN lv-job-no = lv-job-no + SUBSTR(ENTRY(i,v-job-list),li,1).
                ELSE lv-job-no2 = lv-job-no2 + SUBSTR(ENTRY(i,v-job-list),li,1).
        END. /* do li */

        lv-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', lv-job-no, lv-job-no2)) NO-ERROR.

        IF NOT ERROR-STATUS:ERROR AND
            lv-job-no NE "" THEN
            RUN temp-job (lv-job-no).
    END. /* do i */

    IF begin_job NE '' OR end_job NE '' THEN
        FOR EACH job NO-LOCK
            WHERE job.company EQ cocode
            AND FILL(" ", iJobLen - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no)  GE STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job)) 
            AND FILL(" ", iJobLen - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no)  LE STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job)) 
            AND FILL(" ", iJobLen - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"999")
            GE
            STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job, begin_job2)) 
            AND FILL(" ", iJobLen - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"999")
            LE
            STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job, end_job2)) 
            AND (v-stat EQ "A"                      OR
            (v-stat EQ "C" AND NOT job.opened) OR
            (v-stat EQ "O" AND job.opened))
            AND NOT CAN-FIND(FIRST ttblJob
            WHERE trim(ttblJob.job-no)  EQ trim(job.job-no)
            AND ttblJob.job-no2 EQ job.job-no2): 
            CREATE ttblJob.
            ASSIGN
                ttblJob.job-no  = job.job-no
                ttblJob.job-no2 = job.job-no2.
        END. /* each job */

    FOR EACH w-file:
        DELETE w-file.
    END.

    IF v-po-list NE "" AND 
        (ASC(SUBSTRING(v-po-list,LENGTH(v-po-list),1)) = 10 OR
        asc(SUBSTRING(v-po-list,LENGTH(v-po-list),1)) = 13 )
        THEN v-po-list = SUBSTRING(v-po-list,1,LENGTH(v-po-list) - 1).

    DO i = 1 TO NUM-ENTRIES(v-po-list).
        lv-po-no = INT(ENTRY(i,v-po-list)) NO-ERROR.
  
        IF NOT ERROR-STATUS:ERROR AND
            lv-po-no NE 0 THEN 
        DO:
            RUN pValidatePo(lv-po-no ,OUTPUT lCheckError) NO-ERROR .
            IF lCheckError THEN RETURN NO-APPLY.

            RUN temp-po (lv-po-no).
        END.
    END. /* do i */

    FOR EACH w-file:
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ w-file.w-key NO-ERROR.
        IF AVAILABLE po-ord THEN RUN from-po.
    END. /* each w-file */

    IF v-fpo-no[1] EQ v-fpo-no[2] THEN 
    DO:
        RUN pValidatePo(v-fpo-no[1], OUTPUT lCheckError) NO-ERROR .
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    FOR EACH po-ord NO-LOCK
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   GE v-fpo-no[1]
        AND po-ord.po-no   LE v-fpo-no[2]
        AND po-ord.stat    NE "H" 
        AND (v-stat EQ "A"                         OR
        (v-stat EQ "C" AND NOT po-ord.opened) OR
        (v-stat EQ "O" AND po-ord.opened)):
        RUN from-po.
    END. /* each po-ord */

    IF NOT CAN-FIND(FIRST w-po) AND
        v-fpo-no[1] EQ 0 AND v-fpo-no[2] EQ 0 AND
        v-po-list EQ '' AND v-job-list EQ '' AND
        begin_job EQ '' AND end_job EQ '' THEN 
    DO:
        lv-itemOnly = YES.
        RUN createWPOfromItem (v-fitem[1],v-fitem[2]).
    END. /* if not can-find */
    ELSE lv-itemOnly = NO.

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "DOWNLOAD LOADTAG DATA"
        x        = (56 - LENGTH(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (56 - LENGTH(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2.

    SESSION:SET-WAIT-STATE ("").

    IF CAN-FIND(FIRST w-po) THEN
    DO:
        RUN tot-rec-qty-proc.
        RUN porep/d-loadtg.w.
        RUN po-cost-proc.
        RUN tot-rec-qty-proc.
    END.
    choice = NO.
    IF CAN-FIND(FIRST tt-po WHERE
        tt-po.tot-rec-qty GT tt-po.overrun-qty) THEN
        MESSAGE  "Receipt Qty Exceeds P.O Qty + Allowed Overrun%,  Continue?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice2.

    IF choice2 = YES AND CAN-FIND(FIRST w-po) THEN
        MESSAGE "Are you Sure you Want to Create Loadtag File? " 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
    IF NOT choice THEN RETURN ERROR.

    SESSION:SET-WAIT-STATE ("general").
    IF cBarCodeProgram EQ ""  THEN 
    DO:
        {sys/inc/print1.i}

        {sys/inc/outprint.i value(lines-per-page)} 

        VIEW FRAME r-top.
        VIEW FRAME top.

        RUN setOutputFile.

        OUTPUT TO VALUE(v-out).
        RUN outputTagHeader.
    END.
    FOR EACH w-po EXCLUSIVE-LOCK:
        IF NOT lv-itemOnly THEN
            FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ cocode
                AND po-ord.po-no EQ w-po.po-no NO-ERROR.
        IF tb_16ths THEN
            ASSIGN
                w-po.s-len = ROUND((w-po.s-len - TRUNC(w-po.s-len,0)) / 6.25,2) + TRUNC(w-po.s-len,0)
                w-po.s-wid = ROUND((w-po.s-wid - TRUNC(w-po.s-wid,0)) / 6.25,2) + TRUNC(w-po.s-wid,0).

        v-job = w-po.job-no + "-" + STRING(w-po.job-no2,"999").
        IF v-job BEGINS "-" THEN v-job = "".

        ASSIGN
            lv-middlesex-po  = SUBSTR(TRIM(w-po.job-no),1,iJobLen)
            lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                        "%MX" +
                        FILL("0",iJobLen - LENGTH(TRIM(lv-middlesex-job))) +
                        TRIM(lv-middlesex-job)
            lv-middlesex-po  = SUBSTR(STRING(w-po.po-no),1,6)
            lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                        "BNJ" +
                        FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                        TRIM(lv-middlesex-po).

        lv-how-many-tags = w-po.total-tags - (IF w-po.partial NE 0 THEN 1 ELSE 0).
        IF w-po.total-tags GT 0 THEN
        DO j = 1 TO lv-how-many-tags: /* loadtags generation */
            /*       lv-how-many-tags = w-po.total-tags * v-mult - (IF w-po.partial NE 0 THEN 1 ELSE 0). */
            DO i = 1 TO v-mult:
                IF i EQ 1 THEN RUN create-loadtag (j,w-po.rcpt-qty,lv-itemOnly).
                IF cBarCodeProgram EQ "" THEN
                    RUN outputTagLine (w-po.rcpt-qty).
                ELSE 
                    IF cBarCodeProgram EQ "xprint" THEN 
                    DO:
                        CREATE tt-po-print .
                        BUFFER-COPY w-po TO tt-po-print .
                        ASSIGN 
                            tt-po-print.tag-no   = IF AVAILABLE loadtag THEN loadtag.tag-no ELSE ""
                            tt-po-print.vend-tag = IF AVAILABLE loadtag THEN loadtag.misc-char[1] ELSE "" .
                    END.
            END. /* do i */
        END. /* do j */
        IF w-po.partial NE 0 THEN
        DO i = 1 TO v-mult: /* for partial print */
            IF i EQ 1 THEN RUN create-loadtag (j,w-po.partial,lv-itemOnly).
            IF cBarCodeProgram EQ "" THEN
                RUN outputTagLine (w-po.rcpt-qty).
            ELSE 
                IF cBarCodeProgram EQ "xprint" THEN 
                DO:
                    CREATE tt-po-print .
                    BUFFER-COPY w-po TO tt-po-print .
                    ASSIGN 
                        tt-po-print.tag-no   = IF AVAILABLE loadtag THEN loadtag.tag-no ELSE ""
                        tt-po-print.vend-tag = IF AVAILABLE loadtag THEN loadtag.misc-char[1] ELSE "" .
                END.
        END. /* do i */
        DELETE w-po.
    END. /* each w-po */
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").


    /*** auto print ***/
    IF cBarCodeProgram EQ ""  THEN
        RUN AutoPrint.
    ELSE IF cBarCodeProgram EQ "xprint" THEN 
            RUN xprint-tag .

/*   IF scr-auto-print THEN                                                  */
/*   DO:                                                                     */
/*      DEF VAR v-int AS INT NO-UNDO.                                        */
/*      DEF VAR cFileName AS CHAR NO-UNDO.                                   */
/*      DEF VAR v-path AS CHARACTER NO-UNDO.                                 */
/*                                                                           */
/*      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".                       */
/*      USE "SOFTWARE".                                                      */
/*      GET-KEY-VALUE SECTION "Teklynx\Label Matrix"                         */
/*                    KEY "PATH"                                             */
/*                    VALUE v-path.                                          */
/*      UNLOAD "SOFTWARE".                                                   */
/*                                                                           */
/*      ASSIGN                                                               */
/*         v-path = v-path + "\lmwprint.exe "                                */
/*         cFileName = "/L=" + scr-label-file.                               */
/*                                                                           */
/*         RUN WinExec (INPUT v-path + CHR(32) + cFileName , INPUT 1, OUTPUT */
/*                      v-int).                                              */
/*   END.                                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOutputFile C-Win 
PROCEDURE setOutputFile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF v-out = "" THEN v-out = "c:~\ba~\label~\rmtag.txt".
    ELSE 
    DO:
        v-out = v-init-dir.

        IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
            SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
        ELSE v-out = v-out + "/".
        v-out = v-out + "rmtag.txt".
    END.
    IF OPSYS EQ "UNIX" AND v-loadtag NE "TRIAD" THEN
    DO:
        MESSAGE "Unable to Create Loadtag File for Non MSDos Platform." VIEW-AS ALERT-BOX.
        RETURN ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    CREATE w-file.
    w-file.w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-job C-Win 
PROCEDURE temp-job :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-job-no LIKE job.job-no NO-UNDO.

    IF NOT CAN-FIND(FIRST ttblJob
        WHERE ttblJob.job-no  EQ SUBSTR(ip-job-no,1,iJobLen)
        AND ttblJob.job-no2 EQ INT(SUBSTR(ip-job-no,(iJobLen + 1),3))) AND
        CAN-FIND(FIRST job
        WHERE job.company EQ cocode
        AND job.job-no  EQ SUBSTR(ip-job-no,1,iJobLen)
        AND job.job-no2 EQ INT(SUBSTR(ip-job-no,(iJobLen + 1),3))
        AND (v-stat EQ "A"                      OR
        (v-stat EQ "C" AND NOT job.opened) OR
        (v-stat EQ "O" AND job.opened)))
        THEN 
    DO:
        CREATE ttblJob.
        ASSIGN
            ttblJob.job-no  = SUBSTR(ip-job-no,1,iJobLen)
            ttblJob.job-no2 = INT(SUBSTR(ip-job-no,(iJobLen + 1),3)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-po C-Win 
PROCEDURE temp-po :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-po-no LIKE po-ord.po-no NO-UNDO.

    FOR EACH po-ord NO-LOCK
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ ip-po-no
        AND (v-stat EQ "A"                         OR
        (v-stat EQ "C" AND NOT po-ord.opened) OR
        (v-stat EQ "O" AND po-ord.opened)):
        RUN temp-create (ROWID(po-ord)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-rec-qty-proc C-Win 
PROCEDURE tot-rec-qty-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-qty-2 AS DECIMAL NO-UNDO.

    EMPTY TEMP-TABLE tt-po.

    FOR EACH w-po WHERE
        w-po.po-no NE 0:

        FIND FIRST tt-po WHERE
            tt-po.po-no EQ w-po.po-no AND
            tt-po.LINE  EQ w-po.LINE
            NO-ERROR.

        IF NOT AVAILABLE tt-po THEN
        DO:
            FIND FIRST po-ordl WHERE
                po-ordl.company EQ cocode AND
                po-ordl.po-no EQ w-po.po-no AND
                po-ordl.LINE EQ w-po.LINE
                NO-LOCK NO-ERROR.

            CREATE tt-po.
            ASSIGN 
                tt-po.po-no       = w-po.po-no
                tt-po.LINE        = w-po.LINE
                tt-po.cons-uom    = w-po.cons-uom
                tt-po.tot-rec-qty = IF AVAILABLE po-ordl THEN po-ordl.t-rec-qty
                                     ELSE 0
                tt-po.overrun-qty = w-po.overrun-qty.

            IF AVAILABLE po-ordl AND w-po.cons-uom NE po-ordl.cons-uom THEN
            DO:
                FIND FIRST ITEM WHERE 
                    ITEM.company EQ cocode AND
                    ITEM.i-no EQ w-po.i-no
                    NO-LOCK NO-ERROR.

                RUN sys/ref/convquom.p(po-ordl.cons-uom,
                    w-po.cons-uom, IF AVAILABLE ITEM THEN ITEM.basis-w ELSE 0,
                    w-po.s-len, w-po.s-wid, IF AVAILABLE ITEM THEN item.s-dep ELSE 0,
                    tt-po.tot-rec-qty, OUTPUT tt-po.tot-rec-qty).             
            END.
        END.

        v-qty-2 = ((w-po.rcpt-qty * w-po.total-tags) + w-po.partial).

        IF w-po.cons-uom NE tt-po.cons-uom THEN
        DO:
            IF w-po.cons-uom NE tt-po.cons-uom THEN
            DO:
                FIND FIRST ITEM WHERE 
                    ITEM.company EQ cocode AND
                    ITEM.i-no EQ w-po.i-no
                    NO-LOCK NO-ERROR.

                RUN sys/ref/convquom.p(w-po.cons-uom,
                    tt-po.cons-uom, IF AVAILABLE ITEM THEN ITEM.basis-w ELSE 0,
                    w-po.s-len, w-po.s-wid, IF AVAILABLE ITEM THEN item.s-dep ELSE 0,
                    v-qty-2, OUTPUT v-qty-2).
            END.
        END.


        tt-po.tot-rec-qty = tt-po.tot-rec-qty + v-qty-2.
    END.

    FOR EACH tt-po,
        EACH w-po WHERE
        w-po.po-no EQ tt-po.po-no AND
        w-po.LINE  EQ tt-po.LINE:

        w-po.tot-rec-qty = tt-po.tot-rec-qty   .       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validLoadtag C-Win 
PROCEDURE validLoadtag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-valid AS LOG INIT YES NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN reprintLoadtag.

        IF NOT CAN-FIND(FIRST loadtag WHERE
            loadtag.company EQ cocode AND
            loadtag.item-type EQ YES AND
            (loadtag.tag-no EQ reprintLoadtag OR loadtag.misc-char[1] EQ reprintLoadtag) ) THEN 
        DO:
            op-valid = NO.
            MESSAGE 'Invalid Loadtag, Please Try Again ...' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY':U TO reprintLoadtag.
        END.
        ELSE 
        DO:
            CREATE ttLoadTag.
            ttLoadTag.loadTag = reprintLoadTag.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xprint-tag C-Win 
PROCEDURE xprint-tag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cEmail       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhone       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFax         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTagno       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorTagno AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSize        AS CHARACTER NO-UNDO.
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}
 
    PUT "<PREVIEW>".  
    RELEASE tt-po-print .
    FOR EACH tt-po-print  NO-LOCK
        BREAK BY tt-po-print.ord-no :
        IF scr-label-file EQ "Loadtag2.xpr" THEN
        DO:           
            {rm/rep/rmtagxprnt2.i}
        END.
        ELSE 
        DO:
            {rm/rep/rmtagxprnt.i}
        END.
        IF NOT LAST(tt-po-print.ord-no) THEN PAGE .
    END.

    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
    (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    RETURN CAN-FIND(FIRST loc
        WHERE loc.company EQ ipCompany
        AND loc.loc EQ ipLoc) AND
        CAN-FIND(FIRST rm-bin
        WHERE rm-bin.company EQ ipCompany
        AND rm-bin.loc EQ ipLoc
        AND rm-bin.i-no EQ ''
        AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
    DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k            AS INTEGER   NO-UNDO.

    k = NUM-ENTRIES(invalidChars).
    DO i = 1 TO k:
        ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
    END.
    RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

