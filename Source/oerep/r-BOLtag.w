&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-loadtg.w

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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE scanAgain AS LOG       NO-UNDO.


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

DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.

DEFINE VARIABLE save_id        AS RECID.

DEFINE VARIABLE time_stamp     AS ch.
ASSIGN 
    time_stamp = STRING(TIME, "hh:mmam").

DEFINE VARIABLE v-fbol-no      AS INTEGER   FORMAT ">>>>>>>>" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-ford-no      AS INTEGER   FORMAT ">>>>>>" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-orders       AS CHARACTER FORMAT "x(78)" EXTENT 10.
DEFINE VARIABLE v-fitem        AS CHARACTER FORMAT "x(15)" EXTENT 2 
    INIT ["","zzzzzzzzzzzzzzz"].
DEFINE VARIABLE v-po-no-source AS CHARACTER FORMAT "!" 
    INIT "R".
DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" 
    INIT "A".

DEFINE VARIABLE v-out          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE v-job          AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE num-rec        AS INTEGER   INIT 0 NO-UNDO.
DEFINE VARIABLE by-release     AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-printdetail  AS LOG       NO-UNDO.

/* 9812 CAH: */
DEFINE VARIABLE v-loadtag      AS CHARACTER NO-UNDO INIT "ASI". /* sys ctrl option */
DEFINE VARIABLE v-mult         AS INTEGER   NO-UNDO INIT 0.     /* sys ctrl option */
DEFINE VARIABLE v-cas-lab      AS LOG       NO-UNDO.            /* sys ctrl option */
DEFINE VARIABLE v-tags         AS DECIMAL   NO-UNDO INIT 0.     /* sys ctrl option */
DEFINE VARIABLE v-count        AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE v-fgrecpt      AS LOG       NO-UNDO.            /* sys ctrl option */


/* mdp VAR used for posting to finish goods */

DEFINE VARIABLE lv-r-no        LIKE rm-rctd.r-no NO-UNDO.

/* 9812 CAH: VARiables for Intermec Support */
DEFINE VARIABLE stx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~002".
DEFINE VARIABLE etx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~003".
DEFINE VARIABLE esc            AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~033".
DEFINE VARIABLE etb            AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~027".
DEFINE VARIABLE cr             AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~015".
DEFINE VARIABLE can            AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~030".
DEFINE VARIABLE rs             AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~036".
DEFINE VARIABLE us             AS CHARACTER FORMAT 'x(01)' NO-UNDO INIT "~037".

DEFINE STREAM s-form.
DEFINE STREAM s-bar.

DEFINE            VARIABLE form_fid            AS CHARACTER NO-UNDO INIT "barcode.frm" FORMAT "X(40)".
DEFINE            VARIABLE form#               AS INTEGER   NO-UNDO FORMAT "9" INIT 3.
DEFINE            VARIABLE char_units          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE copy_count          AS INTEGER   NO-UNDO INIT 2.
DEFINE            VARIABLE n                   AS INTEGER   NO-UNDO INIT 0.
DEFINE            VARIABLE var-display-warning AS LOG       NO-UNDO.
DEFINE            VARIABLE begin_date          AS DATE      NO-UNDO INIT "01/01/0001".
DEFINE            VARIABLE end_date            AS DATE      NO-UNDO INIT "12/31/9999".

DEFINE            VARIABLE v-tag#              AS INTEGER   NO-UNDO.

DEFINE NEW SHARED VARIABLE choice              AS LOG       NO-UNDO.

DEFINE TEMP-TABLE w-file NO-UNDO
    FIELD w-key  AS ROWID
    FIELD w-fbol AS INTEGER
    FIELD w-tbol AS INTEGER.


DEFINE TEMP-TABLE tt-tag NO-UNDO
    FIELD tt-recid AS RECID.

DEFINE TEMP-TABLE w-shipto LIKE shipto
    FIELD stat   AS CHARACTER
    FIELD row-id AS ROWID.

DEFINE BUFFER b-oe-rel   FOR oe-rel.
DEFINE BUFFER ref-lot-no FOR reftable.
DEFINE BUFFER bf-oe-boll FOR oe-boll.

DEFINE TEMP-TABLE ttblJob NO-UNDO
    FIELD company AS CHARACTER
    FIELD job-no  AS CHARACTER
    FIELD job-no2 AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE company job-no job-no2.

{oerep/r-BOLtg.i NEW}

{fg/fullset.i NEW}

ASSIGN 
    tmpstore = FILL("_",50).

{sys/form/r-top3.f}

DEFINE VARIABLE lv-ok-ran AS LOG NO-UNDO.

{custom/formtext.i NEW}

DEFINE TEMP-TABLE w-fg-rctd LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD invoiced AS LOG   INIT NO.

{fg/fg-post3.i NEW}

DEFINE VARIABLE v-fgpostgl AS CHARACTER NO-UNDO.

{jc/jcgl-sh.i NEW}

DEFINE VARIABLE ordNo    AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo    AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_16ths AS LOG       NO-UNDO.

ASSIGN
    v-ford-no[1] = 0
    v-ford-no[2] = 99999999
    v-fitem[1]   = ""
    v-fitem[2]   = "zzzzzzzzzzzzzzz".

DEFINE VARIABLE v-cust-no AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 v-bol-list begin_bolno end_bolno ~
scr-auto-print scr-label-file begin_form begin_labels begin_filename ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS v-bol-list begin_bolno end_bolno ~
scr-auto-print scr-label-file begin_form begin_labels begin_filename ~
tbAutoClose 

/* Custom List Definitions                                              */
/* jobFields,NonReprint,List-3,List-4,List-5,F1                         */
&Scoped-define NonReprint v-bol-list begin_bolno end_bolno begin_form ~
begin_labels begin_filename 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fgBin C-Win 
FUNCTION fgBin RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE v-bol-list     AS CHARACTER 
    VIEW-AS EDITOR
    SIZE 90 BY 5.14 NO-UNDO.

DEFINE VARIABLE begin_bolno    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "From BOL#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U INITIAL "ccc" 
    LABEL "Label File" 
    VIEW-AS FILL-IN 
    SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form     AS INTEGER   FORMAT ">>>":U INITIAL 3 
    LABEL "Printer Form#" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels   AS INTEGER   FORMAT ">>>>":U INITIAL 2 
    LABEL "# of Labels/Pallet" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE end_bolno      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "To BOL#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file AS CHARACTER FORMAT "X(256)":U 
    LABEL "Label Matrix Label File" 
    VIEW-AS FILL-IN 
    SIZE 63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 13.76.

DEFINE VARIABLE scr-auto-print AS LOGICAL INITIAL NO 
    LABEL "Auto Print Label?" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    v-bol-list AT ROW 2.95 COL 8.6 NO-LABELS
    begin_bolno AT ROW 8.71 COL 25.2 COLON-ALIGNED HELP
    "Enter Beginning Order Number" WIDGET-ID 2
    end_bolno AT ROW 8.71 COL 69.2 COLON-ALIGNED HELP
    "Enter Ending Order Number" WIDGET-ID 4
    scr-auto-print AT ROW 9.86 COL 27.2 WIDGET-ID 8
    scr-label-file AT ROW 11 COL 25.2 COLON-ALIGNED WIDGET-ID 10
    begin_form AT ROW 12.29 COL 25.2 COLON-ALIGNED
    begin_labels AT ROW 12.29 COL 57.2 COLON-ALIGNED
    begin_filename AT ROW 13.67 COL 25.2 COLON-ALIGNED
    tbAutoClose AT ROW 15.57 COL 33.4 WIDGET-ID 64
    btn-ok AT ROW 16.52 COL 33.2
    btn-cancel AT ROW 16.52 COL 55.8
    " Enter BOL(s) separated by comma" VIEW-AS TEXT
    SIZE 42 BY .62 AT ROW 2.29 COL 8.8 WIDGET-ID 6
    BGCOLOR 8 
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 104.4 BY 20.71
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
        TITLE              = "Pallet Tag Creation"
        HEIGHT             = 16.91
        WIDTH              = 104.6
        MAX-HEIGHT         = 320
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 320
        VIRTUAL-WIDTH      = 256
        RESIZE             = NO
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
/* SETTINGS FOR FILL-IN begin_bolno IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_bolno:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_filename IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_filename:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_form IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_form:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_labels IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_labels:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_bolno IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    end_bolno:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-auto-print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-label-file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR EDITOR v-bol-list IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    v-bol-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Pallet Tag Creation */
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
ON WINDOW-CLOSE OF C-Win /* Pallet Tag Creation */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bolno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bolno C-Win
ON HELP OF begin_bolno IN FRAME FRAME-A /* From BOL# */
    DO:
        DEFINE VARIABLE char-val AS cha   NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.

        RUN windows/l-bolh2.w (g_company,NO,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
        IF char-val <> "" THEN
            ASSIGN begin_bolno:SCREEN-VALUE = ENTRY(1,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bolno C-Win
ON LEAVE OF begin_bolno IN FRAME FRAME-A /* From BOL# */
    DO:
        ASSIGN {&self-name}.    

        IF {&self-name} NE 0 AND 
            end_bolno NE 0 
            THEN 
            ASSIGN v-bol-list:SENSITIVE = NO.
        ELSE 
            ASSIGN v-bol-list:SENSITIVE = YES.

        FIND FIRST oe-bolh WHERE oe-bolh.company = cocode
            AND oe-bolh.bol-no = begin_bolno NO-LOCK NO-ERROR.
        v-cust-no = IF AVAILABLE oe-bolh THEN oe-bolh.cust-no ELSE "".
        RUN getBolTagLabel.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Label File */
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
            DISPLAY begin_filename WITH FRAME {&FRAME-NAME}.
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

        ASSIGN scr-auto-print scr-label-file.

        IF TRIM(v-bol-list) EQ ""AND
            (begin_bolno EQ 0 AND
            end_bolno EQ 0)
            THEN 
        DO:
            MESSAGE 
                "Please enter a BOL #."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO v-bol-list.
            RETURN NO-APPLY.
        END.

        IF scr-auto-print AND scr-label-file = "" THEN
        DO:
            MESSAGE "Label Matrix Label File cannot be blank."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.


        IF NOT lv-ok-ran THEN RUN ok-button.

        lv-ok-ran = NO.
  
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bolno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bolno C-Win
ON HELP OF end_bolno IN FRAME FRAME-A /* To BOL# */
    DO:
        DEFINE VARIABLE char-val AS cha   NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.

        RUN windows/l-bolh2.w (g_company,NO,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
        IF char-val <> "" THEN
            ASSIGN end_bolno:SCREEN-VALUE = ENTRY(1,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bolno C-Win
ON LEAVE OF end_bolno IN FRAME FRAME-A /* To BOL# */
    DO:
        ASSIGN {&self-name}.

        IF begin_bolno NE 0 AND 
            {&self-name} NE 0 
            THEN v-bol-list:SENSITIVE = NO.
        ELSE v-bol-list:SENSITIVE = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file C-Win
ON HELP OF scr-label-file IN FRAME FRAME-A /* Label Matrix Label File */
    DO:
        DEFINE VARIABLE chFile AS CHARACTER FORMAT "X(80)" NO-UNDO.
        DEFINE VARIABLE ll-ok  AS LOG       NO-UNDO.

        /* gdm - 11050804 */
        DEFINE VARIABLE v-path AS CHARACTER NO-UNDO.


        ASSIGN 
            v-path = TRIM(scr-label-file:SCREEN-VALUE).

        IF TRIM(v-path) EQ "" 
            THEN
            FIND FIRST sys-ctrl NO-LOCK 
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAILABLE sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).


        RUN sys\ref\char-fld-help.w(INPUT cocode,
            INPUT v-path,
            OUTPUT chFile).


        /* gdm - 11050804 end
     
        DO WITH FRAME {&FRAME-NAME}:
           system-dialog get-file chFile 
                         title "Select Label Matrix Label File"
                         filters "Label Matrix (*.qdf) " "*.qdf"
                         initial-dir v_path
                         MUST-EXIST
                         USE-FILENAME
                         UPDATE ll-ok.
     
           IF ll-ok THEN
        */   
        ASSIGN 
            scr-label-file:SCREEN-VALUE = chFile.


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bol-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bol-list C-Win
ON HELP OF v-bol-list IN FRAME FRAME-A
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID     NO-UNDO.

        RUN windows/l-bolh3.w (g_company,NO,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
        IF char-val <> "" THEN
            ASSIGN v-bol-list:SCREEN-VALUE = char-val.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bol-list C-Win
ON LEAVE OF v-bol-list IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.

        IF {&self-name} NE "" 
            THEN
            ASSIGN
                begin_bolno:SENSITIVE = NO
                end_bolno:SENSITIVE   = NO.
        ELSE
            ASSIGN
                begin_bolno:SENSITIVE = YES
                end_bolno:SENSITIVE   = YES.

        IF ENTRY(1,v-bol-list) <> "" THEN 
        DO:
            FIND FIRST oe-bolh WHERE oe-bolh.company = cocode
                AND oe-bolh.bol-no = int(ENTRY(1,v-bol-list)) NO-LOCK NO-ERROR.
            v-cust-no = IF AVAILABLE oe-bolh THEN oe-bolh.cust-no ELSE "".
            RUN getBolTagLabel.
        END.

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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    ASSIGN
        tb_16ths = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

    DO TRANSACTION:
        {sys/inc/closejob.i FGPost}
        {sys/inc/fgpostgl.i}   
        {sys/ref/oecount.i}
        {sys/inc/sspostfg.i}

        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ gcompany
            AND sys-ctrl.name    EQ "BOLTagFile" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN
        DO:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = gcompany
                sys-ctrl.name    = "BOLTagFile"
                sys-ctrl.descrip = "C:\BA\Label\BOL".
            FIND CURRENT sys-ctrl NO-LOCK.
        END.

        ASSIGN 
            v-out                       = sys-ctrl.descrip
            v-tag#                      = IF sys-ctrl.int-fld EQ 0 THEN 1
                     ELSE sys-ctrl.int-fld
            v-Printdetail               = sys-ctrl.char-fld = "Detail" 
            scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld).
        RUN getBolTagLabel.

    END.

    ASSIGN 
        v-fgpostgl = fgpostgl.

    IF v-loadtag EQ "TRIAD" THEN begin_form = 4.

    IF v-mult LE 0 THEN v-mult = 1.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "FGRECPT"
        NO-LOCK NO-ERROR.
    ASSIGN
        v-fgrecpt = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

    DO WITH FRAME {&FRAME-NAME}:
        btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
        btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
        RUN enable_UI.
        {custom/usrprint.i}
        ASSIGN
            v-bol-list:SCREEN-VALUE     = ""
            begin_bolno:SCREEN-VALUE    = ""
            end_bolno:SCREEN-VALUE      = ""
            begin_filename:SCREEN-VALUE = v-out.

        IF v-loadtag NE "TRIAD" THEN
            DISABLE begin_form begin_labels begin_filename.

        {methods/nowait.i}

        APPLY "entry" TO v-bol-list.

    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-ext-cost C-Win 
PROCEDURE calc-ext-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-rec-qty   AS INTEGER NO-UNDO.

    /* no records */
    IF NOT AVAILABLE fg-rctd THEN RETURN. 

    FIND itemfg NO-LOCK
        WHERE itemfg.company EQ cocode 
        AND itemfg.i-no  EQ fg-rctd.i-no USE-INDEX i-no  NO-ERROR.

    ASSIGN
        lv-cost-uom = itemfg.prod-uom
        v-bwt       = 0
        v-len       = itemfg.t-len
        v-wid       = itemfg.t-wid
        v-dep       = 0.

    FIND FIRST po-ordl 
        WHERE po-ordl.company   = fg-rctd.company
        AND po-ordl.po-no     = INT(fg-rctd.po-no)
        AND po-ordl.i-no      = fg-rctd.i-no
        AND po-ordl.job-no    = fg-rctd.job-no
        AND po-ordl.job-no2   = fg-rctd.job-no2
        AND po-ordl.item-type = NO NO-LOCK NO-ERROR.
    IF AVAILABLE po-ordl THEN 
    DO:
        ASSIGN
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid.
    END.

    ASSIGN 
        lv-out-qty  = fg-rctd.t-qty
        lv-out-cost = fg-rctd.std-cost.

    IF fg-rctd.cost-uom NE lv-cost-uom 
        THEN
        RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
            v-bwt, v-len, v-wid, v-dep,
            fg-rctd.std-cost, OUTPUT lv-out-cost).

    IF lv-cost-uom NE "EA" 
        THEN
        RUN rm/convquom.p("EA", lv-cost-uom,                   
            v-bwt, v-len, v-wid, v-dep,
            lv-out-qty, OUTPUT lv-out-qty).

/*     ASSIGN fg-rctd.ext-cost = (lv-out-qty * lv-out-cost) + fg-rctd.frt-cost. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag C-Win 
PROCEDURE create-loadtag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-tag-no AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-total-unit LIKE w-ord.total-unit NO-UNDO.

    DEFINE BUFFER b-loadtag FOR loadtag.

    DEFINE VARIABLE li         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-got-job AS LOG     NO-UNDO.


    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no NO-ERROR.

    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ cocode
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS w-ord.i-no 
        AND SUBSTR(loadtag.tag-no,1,15) EQ w-ord.i-no USE-INDEX tag NO-ERROR.
    IF AVAILABLE loadtag THEN
        ASSIGN io-tag-no = INT(SUBSTR(loadtag.tag-no,16,5)).


    FIND CURRENT loadtag NO-LOCK NO-ERROR.
    FIND CURRENT fg-rctd NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reg-txtfile C-Win 
PROCEDURE create-reg-txtfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-text          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-dept-note      AS CHARACTER FORMAT "x(80)" EXTENT 18 NO-UNDO.
    DEFINE VARIABLE lv-middlesex-job AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-middlesex-po  AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-tag-no        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-how-many-tags AS INTEGER   NO-UNDO.

    DEFINE VARIABLE v-totpcs         LIKE w-ord.pcs NO-UNDO.
       
    OUTPUT TO VALUE(v-out).

    PUT UNFORMATTED
        "CUSTOMER,BOL#,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,PARTIAL," +
        "BUNDLE,# Unit,TOTAL,SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,"      +
        "SHIPSTATE,SHIPCOUNTRY,SHIPZIP,SOLDCODE,SOLDNAME,SOLDADD1,"       +
        "SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,INAME,DUEDATE,"  +
        "RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"    +
        "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,"           +
        "MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,TAG#,CASECODE,"     +
        "SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,DN5,DN6,"   +
        "DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".

    IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 
        THEN PUT UNFORMATTED ",COUNTER#".

    PUT UNFORMATTED 
        ",DUEDATEJOBLINE,DUEDATEJOB".

    /* gdm - 08130804 */
    PUT UNFORMATTED 
        ",LINE#".

    PUT SKIP.
          
    FOR EACH w-ord 
        BREAK BY w-ord.bol-no BY w-ord.i-no: 

        ASSIGN 
            v-totpcs = v-totpcs + w-ord.pcs.    

        IF tb_16ths 
            THEN               
            ASSIGN
                w-ord.box-len = ROUND((w-ord.box-len - 
                               TRUNC(w-ord.box-len,0)) / 6.25,2) +
                               TRUNC(w-ord.box-len,0)
                w-ord.box-wid = ROUND((w-ord.box-wid - 
                               TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                               TRUNC(w-ord.box-wid,0)
                w-ord.box-dep = ROUND((w-ord.box-dep - 
                               TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                               TRUNC(w-ord.box-dep,0).

        ASSIGN 
            lv-text     = "" 
            v-dept-note = "".

        FIND FIRST itemfg 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO: 
            ASSIGN
                w-ord.net-wt       = itemfg.weight-100 * w-ord.total-unit / 100
                w-ord.sheet-wt     = itemfg.weight-100 / 100 
                w-ord.cust-part-no = itemfg.part-no.

            FOR EACH tt-formtext:  
                DELETE tt-formtext. 
            END.

            FOR EACH notes NO-LOCK 
                WHERE notes.rec_key = itemfg.rec_key
                AND notes.note_code = "SN":

                lv-text = lv-text + " " + 
                    TRIM(notes.note_text) + CHR(10).
            END.

            DO li = 1 TO 8:
                CREATE tt-formtext.
                ASSIGN 
                    tt-line-no = li
                    tt-length  = 80.
            END.

            RUN custom/formtext.p (lv-text).

            i = 0.           

            FOR EACH tt-formtext:
                i = i + 1.
                IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.
            END.

        END. /* avail itemfg */

        ASSIGN
            w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
            v-job          = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', w-ord.job-no, w-ord.job-no2))  .

        IF v-job BEGINS "-" THEN v-job = "".

        ASSIGN
            lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,iJobLen)
            lv-middlesex-job = IF lv-middlesex-job EQ "" 
                             THEN "" 
                             ELSE "%MX" +
                                   FILL("0",iJobLen - LENGTH(TRIM(lv-middlesex-job))) 
                                 + TRIM(lv-middlesex-job)
            lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,9)
            lv-middlesex-po  = IF lv-middlesex-po EQ "" 
                             THEN "" 
                             ELSE "BNJ" +
                                  FILL("0",9 - LENGTH(TRIM(lv-middlesex-po))) 
                                + TRIM(lv-middlesex-po).
               
        IF w-ord.total-tags GT 0 THEN 
        DO:

            ASSIGN 
                lv-how-many-tags = IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 OR
                                       w-ord.total-tags = 1 
                                 THEN w-ord.total-tags
                                 ELSE (w-ord.total-tags - 1).

            /*         DO i = 1 TO (lv-how-many-tags * w-ord.mult): */
            DO i = 1 TO v-tag#:

                /* getting loadtags # */
                IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  
                    THEN 
                DO:
                    IF i = 1 THEN lv-tag-no = i.

                    RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
                END.
                           
                PUT UNFORMATTED 
                    "~""  removeChars(w-ord.cust-name)  "~","
                    w-ord.bol-no  ","
                    w-ord.ord-no  ","
                    "~""  v-job  "~","
                    "~""  CAPS(removeChars(w-ord.i-no))  FORM "x(15)" "~","
                    "~""  removeChars(w-ord.cust-part-no) "~","          
                    "~""  removeChars(w-ord.cust-po-no)  "~"," 
                    w-ord.pcs  "," 
                    loadtag.partial ","                                      
                    w-ord.bundle  "," 
                    w-ord.total-unit FORM ">,>>>,>>9"   ","
                    w-ord.ord-qty FORM ">>>>>>>9" ","                 
                    "~""  removeChars(w-ord.ship-code)  "~","            
                    "~""  removeChars(w-ord.ship-name)  "~","            
                    "~""  removeChars(w-ord.ship-add1)  "~","            
                    "~""  removeChars(w-ord.ship-add2)  "~","            
                    "~""  removeChars(w-ord.ship-city)  "~","            
                    "~""  removeChars(w-ord.ship-state) "~","            
                    "~""  removeChars(w-ord.ship-ctry)  "~","            
                    "~""  removeChars(w-ord.ship-zip)   "~","            
                    "~""  removeChars(w-ord.sold-code)  "~","                        
                    "~""  removeChars(w-ord.sold-name)  "~","                        
                    "~""  removeChars(w-ord.sold-add1)  "~","                        
                    "~""  removeChars(w-ord.sold-add2)  "~","                        
                    "~""  removeChars(w-ord.sold-city)  "~","                        
                    "~""  removeChars(w-ord.sold-state) "~","                        
                    "~""  removeChars(w-ord.sold-ctry)  "~","                        
                    "~""  removeChars(w-ord.sold-zip)   "~","                        
                    "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","            
                    "~""  w-ord.due-date  "~","                                      
                    "~""  w-ord.rel-date  "~","                                      
                    "~""  w-ord.upc-no  "~","                                        
                    "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.flute  "~","                      
                    "~""  w-ord.test  "~","                       
                    "~""  w-ord.vendor  "~","                     
                    w-ord.gross-wt  ","                           
                    w-ord.tare-wt  ","                            
                    w-ord.net-wt  ","                             
                    w-ord.sheet-wt  ","                           
                    "~""  w-ord.uom  "~","                        
                    "~""  removeChars(w-ord.style) "~","          
                    "~""  removeChars(w-ord.style-desc) "~","     
                    "~""  removeChars(w-ord.rel-lot#) "~","       
                    "~""  lv-middlesex-job  "~","                 
                    "~""  lv-middlesex-po  "~","                  
                    "~""  loadtag.tag-no "~","                    
                    "~""  w-ord.cas-no  "~","                     
                    "~""  removeChars(v-dept-note[1]) "~","  
                    "~""  removeChars(v-dept-note[2]) "~","  
                    "~""  removeChars(v-dept-note[3]) "~","  
                    "~""  removeChars(v-dept-note[4]) "~","  
                    "~""  removeChars(v-dept-note[5]) "~","  
                    "~""  removeChars(v-dept-note[6]) "~","  
                    "~""  removeChars(v-dept-note[7]) "~","  
                    "~""  removeChars(v-dept-note[8]) "~","  
                    w-ord.po-no ","                          
                    "~""  removeChars(v-dept-note[9]) "~","  
                    "~""  removeChars(v-dept-note[10]) "~"," 
                    "~""  removeChars(v-dept-note[11]) "~"," 
                    "~""  removeChars(v-dept-note[12]) "~"," 
                    "~""  removeChars(v-dept-note[13]) "~"," 
                    "~""  removeChars(v-dept-note[14]) "~"," 
                    "~""  removeChars(v-dept-note[15]) "~"," 
                    "~""  removeChars(v-dept-note[16]) "~"," 
                    "~""  removeChars(v-dept-note[17]) "~"," 
                    "~""  removeChars(v-dept-note[18]) "~"," 
                    "~""  removeChars(w-ord.est-no)    "~"," 
                    "~""  removeChars(w-ord.ord-desc1) "~"," 
                    "~""  removeChars(w-ord.ord-desc2) "~"," 
                    "~"" SUBSTR(loadtag.tag-no,16,5)   "~"," 
                    "~"" w-ord.due-date-jobhdr         "~"," 
                    "~"" w-ord.due-date-job            "~"," 
                    "~"" w-ord.linenum                 "~"," 
                    SKIP.


            END.  /* FULL TAG */

        /* for partial print 
        IF LOOKUP(v-loadtag,"SSLABEL,CentBox") = 0 
          THEN  
            DO v-count = 1 TO w-ord.mult: 

            /* loadtags generation */
            IF v-count EQ 1 
                THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).

            MESSAGE "First download " SKIP
                "v cnt " v-count SKIP                 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            PUT UNFORMATTED                                             
                "~""  removeChars(w-ord.cust-name)  "~","               
                w-ord.bol-no  ","                                       
                w-ord.ord-no  ","                                       
                "~""  v-job  "~","                                      
                "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~"   
                "~""  removeChars(w-ord.cust-part-no)  "~","            
                "~""  removeChars(w-ord.cust-po-no)  "~","              
                w-ord.pcs  ","                                          
                loadtag.partial ","                                     
                w-ord.bundle  ", ,"                                     
                w-ord.total-unit FORM ">,>>>,>>9"   ","                 
                w-ord.ord-qty FORM ">>>>>>>9" ","                       
                "~""  removeChars(w-ord.ship-code)  "~","               
                "~""  removeChars(w-ord.ship-name)  "~","               
                "~""  removeChars(w-ord.ship-add1)  "~","               
                "~""  removeChars(w-ord.ship-add2)  "~","               
                "~""  removeChars(w-ord.ship-city)  "~","               
                "~""  removeChars(w-ord.ship-state) "~","               
                "~""  removeChars(w-ord.ship-ctry)  "~","               
                "~""  removeChars(w-ord.ship-zip)   "~","               
                "~""  removeChars(w-ord.sold-code)  "~","               
                "~""  removeChars(w-ord.sold-name)  "~","               
                "~""  removeChars(w-ord.sold-add1)  "~","               
                "~""  removeChars(w-ord.sold-add2)  "~","               
                "~""  removeChars(w-ord.sold-city)  "~","               
                "~""  removeChars(w-ord.sold-state) "~","               
                "~""  removeChars(w-ord.sold-ctry)  "~","               
                "~""  removeChars(w-ord.sold-zip)   "~","               
                "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","  
                "~""  w-ord.due-date  "~","                                                             
                "~""  w-ord.rel-date  "~","                                                             
                "~""  w-ord.upc-no  "~","                                                               
                "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.flute  "~","                                                                
                "~""  w-ord.test  "~","                                                                 
                "~""  w-ord.vendor  "~","                                                               
                w-ord.gross-wt  ","                                                                     
                w-ord.tare-wt  ","                                                                      
                w-ord.net-wt  ","                                                                       
                w-ord.sheet-wt  ","                                                                     
                "~""  w-ord.uom  "~","                                                                  
                "~""  removeChars(w-ord.style) "~","                                                    
                "~""  removeChars(w-ord.style-desc) "~","                                               
                "~""  removeChars(w-ord.rel-lot#) "~","                                                 
                "~""  lv-middlesex-job  "~","                                                           
                "~""  lv-middlesex-po  "~","                                                            
                "~""  loadtag.tag-no "~","                                                              
                "~""  w-ord.cas-no  "~","                                                               
                "~""  removeChars(v-dept-note[1]) "~","   
                "~""  removeChars(v-dept-note[2]) "~","   
                "~""  removeChars(v-dept-note[3]) "~","   
                "~""  removeChars(v-dept-note[4]) "~","   
                "~""  removeChars(v-dept-note[5]) "~","   
                "~""  removeChars(v-dept-note[6]) "~","   
                "~""  removeChars(v-dept-note[7]) "~","   
                "~""  removeChars(v-dept-note[8]) "~","   
                w-ord.po-no ","                           
                "~""  removeChars(v-dept-note[9]) "~","   
                "~""  removeChars(v-dept-note[10]) "~","  
                "~""  removeChars(v-dept-note[11]) "~","  
                "~""  removeChars(v-dept-note[12]) "~","  
                "~""  removeChars(v-dept-note[13]) "~","  
                "~""  removeChars(v-dept-note[14]) "~","  
                "~""  removeChars(v-dept-note[15]) "~","  
                "~""  removeChars(v-dept-note[16]) "~","  
                "~""  removeChars(v-dept-note[17]) "~","  
                "~""  removeChars(v-dept-note[18]) "~","  
                "~""  removeChars(w-ord.est-no)    "~","  
                "~""  removeChars(w-ord.ord-desc1) "~","  
                "~""  removeChars(w-ord.ord-desc2) "~","  
                "~"" SUBSTR(loadtag.tag-no,16,5)   "~","  
                "~"" w-ord.due-date-jobhdr         "~","  
                "~"" w-ord.due-date-job            "~","  
                "~"" w-ord.linenum                 "~","  
                SKIP.





        END.  PArtial */

        END.  /* IF w-ord.total-tags gt 0 */

        DELETE w-ord.

    END. /* FOR EACH w-ord*/
          
    OUTPUT CLOSE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-regDetail-txtfile C-Win 
PROCEDURE create-regDetail-txtfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-text          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-dept-note      AS CHARACTER FORMAT "x(80)" EXTENT 18 NO-UNDO.
    DEFINE VARIABLE lv-middlesex-job AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-middlesex-po  AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-tag-no        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-how-many-tags AS INTEGER   NO-UNDO.

    DEFINE VARIABLE v-totpcs         LIKE w-ord.pcs NO-UNDO.

    OUTPUT TO VALUE(v-out).

    PUT UNFORMATTED
        "CUSTOMER,BOL#,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,PARTIAL," +
        "BUNDLE,# Unit,TOTAL,SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,"      +
        "SHIPSTATE,SHIPCOUNTRY,SHIPZIP,SOLDCODE,SOLDNAME,SOLDADD1,"       +
        "SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,INAME,DUEDATE,"  +
        "RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"    +
        "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,"           +
        "MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,TAG#,CASECODE,"     +
        "SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,DN5,DN6,"   +
        "DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".

    IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 
        THEN PUT UNFORMATTED ",COUNTER#".

    PUT UNFORMATTED 
        ",DUEDATEJOBLINE,DUEDATEJOB".

    /* gdm - 08130804 */
    PUT UNFORMATTED 
        ",LINE#".

    PUT SKIP.

    FOR EACH w-ord 
        BREAK BY w-ord.bol-no BY w-ord.i-no: 

        ASSIGN 
            v-totpcs = v-totpcs + w-ord.pcs.    

        IF tb_16ths 
            THEN               
            ASSIGN
                w-ord.box-len = ROUND((w-ord.box-len - 
                               TRUNC(w-ord.box-len,0)) / 6.25,2) +
                               TRUNC(w-ord.box-len,0)
                w-ord.box-wid = ROUND((w-ord.box-wid - 
                               TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                               TRUNC(w-ord.box-wid,0)
                w-ord.box-dep = ROUND((w-ord.box-dep - 
                               TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                               TRUNC(w-ord.box-dep,0).

        ASSIGN 
            lv-text     = "" 
            v-dept-note = "".

        FIND FIRST itemfg 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO: 
            ASSIGN
                w-ord.net-wt       = itemfg.weight-100 * w-ord.total-unit / 100
                w-ord.sheet-wt     = itemfg.weight-100 / 100 
                w-ord.cust-part-no = itemfg.part-no.

            FOR EACH tt-formtext:  
                DELETE tt-formtext. 
            END.

            FOR EACH notes NO-LOCK 
                WHERE notes.rec_key = itemfg.rec_key
                AND notes.note_code = "SN":

                lv-text = lv-text + " " + 
                    TRIM(notes.note_text) + CHR(10).
            END.

            DO li = 1 TO 8:
                CREATE tt-formtext.
                ASSIGN 
                    tt-line-no = li
                    tt-length  = 80.
            END.

            RUN custom/formtext.p (lv-text).

            i = 0.           

            FOR EACH tt-formtext:
                i = i + 1.
                IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.
            END.

        END. /* avail itemfg */

        ASSIGN
            w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
            v-job          = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', w-ord.job-no, w-ord.job-no2)) .

        IF v-job BEGINS "-" THEN v-job = "".

        ASSIGN
            lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,iJobLen)
            lv-middlesex-job = IF lv-middlesex-job EQ "" 
                             THEN "" 
                             ELSE "%MX" +
                                   FILL("0",iJobLen - LENGTH(TRIM(lv-middlesex-job))) 
                                 + TRIM(lv-middlesex-job)
            lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,9)
            lv-middlesex-po  = IF lv-middlesex-po EQ "" 
                             THEN "" 
                             ELSE "BNJ" +
                                  FILL("0",9 - LENGTH(TRIM(lv-middlesex-po))) 
                                + TRIM(lv-middlesex-po).

        IF w-ord.total-tags GT 0 THEN 
        DO:

            ASSIGN 
                lv-how-many-tags = IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 OR
                                       w-ord.total-tags = 1 
                                 THEN w-ord.total-tags
                                 ELSE (w-ord.total-tags - 1).

            /*         DO i = 1 TO (lv-how-many-tags * w-ord.mult): */
            DO i = 1 TO v-tag#:

                /* getting loadtags # */
                IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  
                    THEN 
                DO:
                    IF i = 1 THEN lv-tag-no = i.

                    RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
                END.

                PUT UNFORMATTED 
                    "~""  removeChars(w-ord.cust-name)  "~","
                    w-ord.bol-no  ","
                    w-ord.ord-no  ","
                    "~""  v-job  "~","
                    "~""  CAPS(removeChars(w-ord.i-no))  FORM "x(15)" "~","
                    "~""  removeChars(w-ord.cust-part-no) "~","          
                    "~""  removeChars(w-ord.cust-po-no)  "~"," 
                    w-ord.pcs  "," 
                    loadtag.partial ","                                      
                    w-ord.bundle  "," 
                    w-ord.total-unit FORM ">,>>>,>>9"   ","
                    w-ord.ord-qty FORM ">>>>>>>9" ","                 
                    "~""  removeChars(w-ord.ship-code)  "~","            
                    "~""  removeChars(w-ord.ship-name)  "~","            
                    "~""  removeChars(w-ord.ship-add1)  "~","            
                    "~""  removeChars(w-ord.ship-add2)  "~","            
                    "~""  removeChars(w-ord.ship-city)  "~","            
                    "~""  removeChars(w-ord.ship-state) "~","            
                    "~""  removeChars(w-ord.ship-ctry)  "~","            
                    "~""  removeChars(w-ord.ship-zip)   "~","            
                    "~""  removeChars(w-ord.sold-code)  "~","                        
                    "~""  removeChars(w-ord.sold-name)  "~","                        
                    "~""  removeChars(w-ord.sold-add1)  "~","                        
                    "~""  removeChars(w-ord.sold-add2)  "~","                        
                    "~""  removeChars(w-ord.sold-city)  "~","                        
                    "~""  removeChars(w-ord.sold-state) "~","                        
                    "~""  removeChars(w-ord.sold-ctry)  "~","                        
                    "~""  removeChars(w-ord.sold-zip)   "~","                        
                    "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","            
                    "~""  w-ord.due-date  "~","                                      
                    "~""  w-ord.rel-date  "~","                                      
                    "~""  w-ord.upc-no  "~","                                        
                    "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~"," 
                    "~""  w-ord.flute  "~","                      
                    "~""  w-ord.test  "~","                       
                    "~""  w-ord.vendor  "~","                     
                    w-ord.gross-wt  ","                           
                    w-ord.tare-wt  ","                            
                    w-ord.net-wt  ","                             
                    w-ord.sheet-wt  ","                           
                    "~""  w-ord.uom  "~","                        
                    "~""  removeChars(w-ord.style) "~","          
                    "~""  removeChars(w-ord.style-desc) "~","     
                    "~""  removeChars(w-ord.rel-lot#) "~","       
                    "~""  lv-middlesex-job  "~","                 
                    "~""  lv-middlesex-po  "~","                  
                    "~""  w-ord.tag-no "~","                    
                    "~""  w-ord.cas-no  "~","                     
                    "~""  removeChars(v-dept-note[1]) "~","  
                    "~""  removeChars(v-dept-note[2]) "~","  
                    "~""  removeChars(v-dept-note[3]) "~","  
                    "~""  removeChars(v-dept-note[4]) "~","  
                    "~""  removeChars(v-dept-note[5]) "~","  
                    "~""  removeChars(v-dept-note[6]) "~","  
                    "~""  removeChars(v-dept-note[7]) "~","  
                    "~""  removeChars(v-dept-note[8]) "~","  
                    w-ord.po-no ","                          
                    "~""  removeChars(v-dept-note[9]) "~","  
                    "~""  removeChars(v-dept-note[10]) "~"," 
                    "~""  removeChars(v-dept-note[11]) "~"," 
                    "~""  removeChars(v-dept-note[12]) "~"," 
                    "~""  removeChars(v-dept-note[13]) "~"," 
                    "~""  removeChars(v-dept-note[14]) "~"," 
                    "~""  removeChars(v-dept-note[15]) "~"," 
                    "~""  removeChars(v-dept-note[16]) "~"," 
                    "~""  removeChars(v-dept-note[17]) "~"," 
                    "~""  removeChars(v-dept-note[18]) "~"," 
                    "~""  removeChars(w-ord.est-no)    "~"," 
                    "~""  removeChars(w-ord.ord-desc1) "~"," 
                    "~""  removeChars(w-ord.ord-desc2) "~"," 
                    "~"" SUBSTR(loadtag.tag-no,16,5)   "~"," 
                    "~"" w-ord.due-date-jobhdr         "~"," 
                    "~"" w-ord.due-date-job            "~"," 
                    "~"" w-ord.linenum                 "~"," 
                    SKIP.


            END.  /* FULL TAG */

        /* for partial print 
        IF LOOKUP(v-loadtag,"SSLABEL,CentBox") = 0 
          THEN  
            DO v-count = 1 TO w-ord.mult: 

            /* loadtags generation */
            IF v-count EQ 1 
                THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).

            MESSAGE "First download " SKIP
                "v cnt " v-count SKIP                 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            PUT UNFORMATTED                                             
                "~""  removeChars(w-ord.cust-name)  "~","               
                w-ord.bol-no  ","                                       
                w-ord.ord-no  ","                                       
                "~""  v-job  "~","                                      
                "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~"   
                "~""  removeChars(w-ord.cust-part-no)  "~","            
                "~""  removeChars(w-ord.cust-po-no)  "~","              
                w-ord.pcs  ","                                          
                loadtag.partial ","                                     
                w-ord.bundle  ", ,"                                     
                w-ord.total-unit FORM ">,>>>,>>9"   ","                 
                w-ord.ord-qty FORM ">>>>>>>9" ","                       
                "~""  removeChars(w-ord.ship-code)  "~","               
                "~""  removeChars(w-ord.ship-name)  "~","               
                "~""  removeChars(w-ord.ship-add1)  "~","               
                "~""  removeChars(w-ord.ship-add2)  "~","               
                "~""  removeChars(w-ord.ship-city)  "~","               
                "~""  removeChars(w-ord.ship-state) "~","               
                "~""  removeChars(w-ord.ship-ctry)  "~","               
                "~""  removeChars(w-ord.ship-zip)   "~","               
                "~""  removeChars(w-ord.sold-code)  "~","               
                "~""  removeChars(w-ord.sold-name)  "~","               
                "~""  removeChars(w-ord.sold-add1)  "~","               
                "~""  removeChars(w-ord.sold-add2)  "~","               
                "~""  removeChars(w-ord.sold-city)  "~","               
                "~""  removeChars(w-ord.sold-state) "~","               
                "~""  removeChars(w-ord.sold-ctry)  "~","               
                "~""  removeChars(w-ord.sold-zip)   "~","               
                "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","  
                "~""  w-ord.due-date  "~","                                                             
                "~""  w-ord.rel-date  "~","                                                             
                "~""  w-ord.upc-no  "~","                                                               
                "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","                                           
                "~""  w-ord.flute  "~","                                                                
                "~""  w-ord.test  "~","                                                                 
                "~""  w-ord.vendor  "~","                                                               
                w-ord.gross-wt  ","                                                                     
                w-ord.tare-wt  ","                                                                      
                w-ord.net-wt  ","                                                                       
                w-ord.sheet-wt  ","                                                                     
                "~""  w-ord.uom  "~","                                                                  
                "~""  removeChars(w-ord.style) "~","                                                    
                "~""  removeChars(w-ord.style-desc) "~","                                               
                "~""  removeChars(w-ord.rel-lot#) "~","                                                 
                "~""  lv-middlesex-job  "~","                                                           
                "~""  lv-middlesex-po  "~","                                                            
                "~""  loadtag.tag-no "~","                                                              
                "~""  w-ord.cas-no  "~","                                                               
                "~""  removeChars(v-dept-note[1]) "~","   
                "~""  removeChars(v-dept-note[2]) "~","   
                "~""  removeChars(v-dept-note[3]) "~","   
                "~""  removeChars(v-dept-note[4]) "~","   
                "~""  removeChars(v-dept-note[5]) "~","   
                "~""  removeChars(v-dept-note[6]) "~","   
                "~""  removeChars(v-dept-note[7]) "~","   
                "~""  removeChars(v-dept-note[8]) "~","   
                w-ord.po-no ","                           
                "~""  removeChars(v-dept-note[9]) "~","   
                "~""  removeChars(v-dept-note[10]) "~","  
                "~""  removeChars(v-dept-note[11]) "~","  
                "~""  removeChars(v-dept-note[12]) "~","  
                "~""  removeChars(v-dept-note[13]) "~","  
                "~""  removeChars(v-dept-note[14]) "~","  
                "~""  removeChars(v-dept-note[15]) "~","  
                "~""  removeChars(v-dept-note[16]) "~","  
                "~""  removeChars(v-dept-note[17]) "~","  
                "~""  removeChars(v-dept-note[18]) "~","  
                "~""  removeChars(w-ord.est-no)    "~","  
                "~""  removeChars(w-ord.ord-desc1) "~","  
                "~""  removeChars(w-ord.ord-desc2) "~","  
                "~"" SUBSTR(loadtag.tag-no,16,5)   "~","  
                "~"" w-ord.due-date-jobhdr         "~","  
                "~"" w-ord.due-date-job            "~","  
                "~"" w-ord.linenum                 "~","  
                SKIP.





        END.  PArtial */

        END.  /* IF w-ord.total-tags gt 0 */

        DELETE w-ord.

    END. /* FOR EACH w-ord*/

    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file C-Win 
PROCEDURE create-text-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
       

    IF v-loadtag = "TRIAD" 
        THEN RUN create-TRIAD-txtfile.
    ELSE IF v-PrintDetail THEN RUN create-regDetail-txtfile.
        ELSE RUN create-reg-txtfile.       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-TRIAD-txtfile C-Win 
PROCEDURE create-TRIAD-txtfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF v-loadtag = "TRIAD" THEN 
    DO:

        /* download the form file into the printer ~*/
        IF form_fid > "" THEN 
        DO:   
            INPUT STREAM s-form from VALUE(form_fid) NO-ECHO.
            _form: 
            DO WHILE TRUE:

                READKEY STREAM s-form.              

                IF LASTKEY < 0 THEN LEAVE _form.             

                PUT STREAM s-bar CONTROL CHR(LASTKEY).

            END.

            INPUT STREAM s-form CLOSE.
        END.  /* if form_fid > "" */

        FOR EACH w-ord BY w-ord.bol-no:

            ASSIGN 
                v-job = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', w-ord.job-no, w-ord.job-no2)).

            /* 9901 CAH */
            IF v-job BEGINS "-" OR v-job = ? 
                THEN v-job = STRING(W-ORD.ORD-NO).   

            /* 9812 CAH in case blank */
            FIND FIRST itemfg 
                WHERE itemfg.company = cocode
                AND itemfg.i-no = w-ord.i-no NO-LOCK NO-ERROR.

            IF w-ord.total-tags GT -1 THEN 
            DO:

                DO i = 1 TO (w-ord.total-tags + 1):

                    /* select the form */
                    PUT STREAM s-bar CONTROL stx esc "E" STRING(form#) ",1" can etx.

                    /* 
                      9901 CAH: done above ... 
                    /* clear the variable data fields */
                       put stream s-bar control stx can etx.                  
                    */

                    ASSIGN 
                        char_units = (IF i <= w-ord.total-tags
                                    THEN STRING(w-ord.total-unit) 
                                    ELSE "    " ).

                    DEFINE VARIABLE char_date AS CHARACTER FORMAT 'x(10)' NO-UNDO.

                    ASSIGN 
                        char_date = STRING(TODAY,"99/99/9999").

                    /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
                    IF LENGTH(w-ord.ship-name) > 19
                        THEN w-ord.ship-name = SUBSTR(w-ord.ship-name,1,19).

                    DEFINE VARIABLE vcFGItem AS CHARACTER NO-UNDO.

                    ASSIGN  
                        vcFGItem = IF AVAILABLE itemfg 
                                  THEN itemfg.i-no ELSE w-ord.i-no.

                    DO n = copy_count TO 1 BY -1:

                        /* send the variable data to the printer */
                        PUT STREAM s-bar UNFORMATTED 
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-part-no  cr etx                                    
                            stx w-ord.cust-part-no  cr etx                                    
                            stx char_units          cr etx                                    
                            stx char_units          cr etx                                    
                            stx char_date           cr etx                                    
                            stx v-job               cr etx 
                            /* 9902 CAH was total-unit */ 
                            stx w-ord.ord-qty       cr etx 
                            /* 08.20 was n */
                            stx STRING(i)           cr etx 
                            /* 08.20 was copy_count */
                            stx STRING(w-ord.total-tags + 1) cr etx 
                            stx w-ord.ship-name     cr etx                                    
                            stx vcFGItem            cr etx.                                   

                        /* issue the print command */    
                        PUT STREAM s-bar CONTROL
                            stx rs "1" us "1" etb etx.
                    END. /* DO n = */
                END. /* tag count loop */
            END. /* non zero */  
        END. /* each w-ord */

        /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.

    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-ord C-Win 
PROCEDURE create-w-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*
   DEF VAR lv-rel-date AS DATE NO-UNDO.
   DEF BUFFER b-job FOR job.
   DEF BUFFER b-job-hdr FOR job-hdr.

   MESSAGE "Im here create-w-ord"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

   FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
   FIND FIRST itemfg WHERE itemfg.company = loadtag.company
                       AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   FIND FIRST oe-ord WHERE oe-ord.company = loadtag.company
                       AND oe-ord.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.
   IF AVAIL oe-ord THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
                           AND oe-ordl.ord-no = loadtag.ord-no
                           AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.

      FIND FIRST cust WHERE cust.company = loadtag.company
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.

      FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
                                 AND b-job.job-no  = loadtag.job-no
                                 AND b-job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL b-job THEN
         FIND FIRST b-job-hdr WHERE b-job-hdr.company EQ b-job.company
                                AND b-job-hdr.job     EQ b-job.job
                                AND b-job-hdr.job-no  EQ b-job.job-no
                                AND b-job-hdr.job-no2 EQ b-job.job-no2
                                AND b-job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.

      FIND FIRST oe-bolh NO-LOCK 
          WHERE oe-bolh.company EQ oe-ordl.company
            AND oe-bolh.ord-no EQ oe-ordl.ord-no NO-ERROR.
      IF AVAIL oe-bolh THEN
          FIND FIRST oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-ordl.company
              AND oe-boll.b-no EQ oe-bolh.b-no
              AND oe-boll.i-no EQ oe-ordl.i-no NO-ERROR.


      CREATE w-ord.
      ASSIGN 
            w-ord.bol-no       = oe-boll.bol-no
            w-ord.ord-no       = oe-boll.ord-no
            w-ord.job-no       = oe-boll.job-no
            w-ord.job-no2      = oe-boll.job-no2
            w-ord.cust-no      = oe-boll.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-boll.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.ord-qty      = oe-boll.qty
            w-ord.po-no        = oe-boll.po-no
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            /* gdm - 08130804*/
            w-ord.linenum      = oe-ordl.e-num
            /* gdm - 03200921 */
            w-ord.bol-no       = oe-boll.bol-no
            .

      IF AVAIL b-job-hdr THEN
         w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
      IF AVAIL b-job THEN
         w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".

      RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#).

      IF AVAIL itemfg THEN
         ASSIGN w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = oe-boll.qty-case
             w-ord.bundle  = oe-boll.cases-unit
             w-ord.style   = itemfg.style.

      IF w-ord.style NE "" THEN
      DO:
         FIND FIRST style WHERE
              style.company EQ cocode AND
              style.style EQ w-ord.style
              NO-LOCK NO-ERROR.

         IF AVAIL style THEN
         DO:
            w-ord.style-desc = style.dscr.
            RELEASE style.
         END.
      END.

      FIND FIRST shipto WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq oe-ord.cust-no
            USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
         ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute  = eb.flute
             w-ord.test   = eb.test
             w-ord.pcs    = eb.cas-cnt
             w-ord.bundle = eb.cas-pal
             w-ord.cas-no = eb.cas-no.

          ASSIGN 
              w-ord.total-tags = 1
              w-ord.ord-qty    = oe-boll.qty
              w-ord.pcs     = oe-boll.qty-case
              w-ord.bundle  = oe-boll.cases-unit
              w-ord.partial    = loadtag.partial
              w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial.

   END.  /* avail oe-ord*/
   ELSE IF loadtag.job-no <> "" THEN DO:
      FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                               AND job.job-no = loadtag.job-no
                               AND job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN DO:

         FIND FIRST cust WHERE cust.company eq cocode
                          AND cust.cust-no eq job-hdr.cust-no NO-LOCK NO-ERROR.
         FIND FIRST itemfg WHERE itemfg.company eq cocode
                            AND itemfg.i-no    eq job-hdr.i-no NO-LOCK NO-ERROR.

         FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ job-hdr.company
               AND oe-boll.job-no  EQ job-hdr.job-no 
               AND oe-boll.job-no2 EQ job-hdr.job-no2 NO-ERROR.

         CREATE w-ord.
         ASSIGN
            w-ord.bol-no       = oe-boll.bol-no
            w-ord.ord-no       = oe-boll.ord-no
            w-ord.job-no       = oe-boll.job-no
            w-ord.job-no2      = oe-boll.job-no2
            w-ord.cust-no      = oe-boll.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = oe-boll.i-no
            w-ord.ord-qty      = oe-boll.qty
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            /* gdm - 03200921 */
            w-ord.bol-no       = oe-boll.bol-no.

          IF AVAIL itemfg THEN
             ASSIGN
                w-ord.cust-part-no = itemfg.part-no
                w-ord.style        = itemfg.style
                w-ord.i-name       = itemfg.i-name
                w-ord.upc-no       = itemfg.upc-no
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.

          FIND FIRST shipto
              WHERE shipto.company eq cocode
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no.

          ASSIGN 
              w-ord.total-tags = 1
              w-ord.ord-qty    = oe-boll.qty
              w-ord.pcs        = oe-boll.qty-case
              w-ord.bundle     = oe-boll.cases-unit
              w-ord.partial    = loadtag.partial
              w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial.

       END.  /* avail job*/
   END. /* job-no <> "" */
   ELSE IF loadtag.po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                           AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
         FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                                    AND po-ordl.po-no EQ po-ord.po-no
                                    AND po-ordl.i-no = loadtag.i-no
                                    USE-INDEX po-no  NO-ERROR.
      IF AVAIL po-ordl THEN DO:
         FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
         FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
         FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.

         FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ po-ord.company
               AND oe-boll.po-no   EQ STRING(po-ordl.po-no) 
               AND oe-boll.i-no    EQ po-ordl.i-no NO-ERROR.

         CREATE w-ord.
         ASSIGN
            w-ord.bol-no       = oe-boll.bol-no
            w-ord.cust-name    = IF AVAILABLE cust THEN cust.name ELSE ''
            w-ord.cust-no      = oe-boll.cust-no
            w-ord.due-date     = po-ord.due-date
            w-ord.i-no         = oe-boll.i-no
            w-ord.i-name       = po-ordl.i-name
            w-ord.mult         = IF AVAILABLE cust AND 
                                    cust.int-field[1] NE 0 
                                   THEN cust.int-field[1] ELSE v-mult
            w-ord.ord-qty       = oe-boll.qty
            w-ord.po-no         = oe-boll.po-no
            w-ord.tare-wt       = 10
            w-ord.uom           = 'EA'
            w-ord.vendor        = IF AVAILABLE vend THEN vend.name ELSE ''
            . 

         IF AVAILABLE itemfg THEN
            ASSIGN 
                w-ord.est-no  = itemfg.est-no
                w-ord.upc-no  = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute   = itemfg.flute
                w-ord.test    = itemfg.test
                w-ord.pcs     = itemfg.case-count
                w-ord.bundle  = IF itemfg.case-pall NE 0 
                                  THEN itemfg.case-pall ELSE 1
                w-ord.style = itemfg.style.

         IF w-ord.style NE "" THEN
         DO:
            FIND FIRST style WHERE
                 style.company EQ cocode AND
                 style.style EQ w-ord.style
                 NO-LOCK NO-ERROR.

            IF AVAIL style THEN
            DO:
               w-ord.style-desc = style.dscr.
               RELEASE style.
            END.
         END.

         IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
            FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                              AND eb.est-no EQ itemfg.est-no
                              AND eb.stock-no EQ itemfg.i-no NO-ERROR.
         IF AVAILABLE eb THEN
             ASSIGN w-ord.flute  = eb.flute
                    w-ord.test   = eb.test
                    w-ord.pcs    = eb.cas-cnt
                    w-ord.bundle = eb.cas-pal
                    w-ord.cas-no = eb.cas-no.

         FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                                  AND shipto.cust-no EQ po-ord.cust-no
                                  AND shipto.ship-id EQ po-ord.cust-no
                                USE-INDEX ship-id NO-ERROR.
         IF AVAILABLE shipto THEN
            ASSIGN w-ord.ship-name   = shipto.ship-name
                    w-ord.ship-add1  = shipto.ship-add[1]
                    w-ord.ship-add2  = shipto.ship-add[2]
                    w-ord.ship-city  = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-zip   = shipto.ship-zip.

         ASSIGN 
             w-ord.total-tags = 1
              w-ord.ord-qty    = oe-boll.qty
              w-ord.pcs        = oe-boll.qty-case
              w-ord.bundle     = oe-boll.cases-unit
              w-ord.partial    = loadtag.partial
              w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial.

    END. /* AVAIL PO-ORDL */
   END. /* po-no <> ""*/
   ELSE DO:
       FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                 AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
       IF AVAIL itemfg THEN DO:
          FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
          FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ itemfg.cust-no NO-ERROR.

          CREATE w-ord.
          ASSIGN w-ord.i-no         = itemfg.i-no
                 w-ord.i-name       = itemfg.i-name
                 w-ord.cust-no      = itemfg.cust-no
                 w-ord.cust-name    = itemfg.cust-name
                 w-ord.cust-part-no = itemfg.part-no
                 w-ord.mult         = IF AVAILABLE cust AND 
                                         cust.int-field[1] NE 0 
                                        THEN cust.int-field[1] ELSE v-mult
                 w-ord.box-len      = itemfg.l-score[50]
                 w-ord.box-wid      = itemfg.w-score[50]
                 w-ord.box-dep      = itemfg.d-score[50]
                 w-ord.flute        = itemfg.flute
                 w-ord.upc-no       = itemfg.upc-no
                 w-ord.test         = itemfg.test
                 w-ord.vendor       = IF AVAILABLE vend THEN vend.name ELSE company.name
                 w-ord.tare-wt      = 10
                 w-ord.uom          = "EA"
                 w-ord.pcs          = itemfg.case-count
                 w-ord.bundle       = itemfg.case-pall
                 w-ord.total-tags   = 1
                 w-ord.ord-qty      = loadtag.qty 
                 w-ord.pcs          = loadtag.qty-case
                 w-ord.bundle       = loadtag.case-bundle
                 w-ord.partial      = loadtag.partial
                 w-ord.total-unit   = w-ord.pcs * w-ord.bundle + w-ord.partial
                 w-ord.style        = itemfg.style.

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
       END. /* avail itemfg */
   END.
*/   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateWOrdFromItem C-Win 
PROCEDURE CreateWOrdFromItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*
  DEFINE INPUT PARAMETER ipBeginItem AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndItem AS CHARACTER NO-UNDO.
  MESSAGE "Im here "
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ cocode
                            AND itemfg.i-no GE ipBeginItem
                            AND itemfg.i-no LE ipEndItem:
    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.
    CREATE w-ord.
    ASSIGN
      w-ord.i-no = itemfg.i-no
      w-ord.i-name = itemfg.i-name
      w-ord.cust-no = itemfg.cust-no
      w-ord.cust-name = itemfg.cust-name
      w-ord.cust-part-no = itemfg.part-no
      w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                   cust.int-field[1] ELSE v-mult
      w-ord.box-len = itemfg.l-score[50]
      w-ord.box-wid = itemfg.w-score[50]
      w-ord.box-dep = itemfg.d-score[50]
      w-ord.flute = itemfg.flute
      w-ord.upc-no = itemfg.upc-no
      w-ord.test = itemfg.test
      w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE company.name
      w-ord.tare-wt = 10
      w-ord.uom = "EA"
      w-ord.pcs = itemfg.case-count
      w-ord.bundle = itemfg.case-pall
      w-ord.style   = itemfg.style.

    IF w-ord.style NE "" THEN
    DO:
       FIND FIRST style WHERE
            style.company EQ cocode AND
            style.style EQ w-ord.style
            NO-LOCK NO-ERROR.

       IF AVAIL style THEN
       DO:
          w-ord.style-desc = style.dscr.
          RELEASE style.
       END.
    END.
  END. * each itemfg */

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
    DISPLAY v-bol-list begin_bolno end_bolno scr-auto-print scr-label-file 
        begin_form begin_labels begin_filename tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 v-bol-list begin_bolno end_bolno scr-auto-print scr-label-file 
        begin_form begin_labels begin_filename tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-update C-Win 
PROCEDURE final-update :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FOR EACH w-ord:
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ w-ord.cust-no
            NO-LOCK NO-ERROR.

        IF v-tags EQ 0 THEN
            w-ord.total-tags = IF AVAILABLE cust AND cust.int-field[1] GT 1 THEN cust.int-field[1] ELSE 1.
        ELSE
            IF v-tags EQ ? THEN w-ord.total-tags = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord C-Win 
PROCEDURE from-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE lv-got-shipto AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-stat       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rel-date   AS DATE      NO-UNDO.


    DEFINE BUFFER b-job-hdr FOR job-hdr. /* rtc */
    DEFINE BUFFER b-job     FOR job.         /* rtc */
    DEFINE BUFFER b-oe-ordl FOR oe-ordl. 

    DEFINE VARIABLE v-tot-pcs  LIKE w-ord.pcs NO-UNDO.
    DEFINE VARIABLE v-tot-bdl  LIKE w-ord.bundle NO-UNDO.
    DEFINE VARIABLE v-tot-qty  LIKE w-ord.ord-qty NO-UNDO.
    DEFINE VARIABLE v-tot-par  LIKE w-ord.partial NO-UNDO.
    DEFINE VARIABLE v-tot-unt  LIKE w-ord.total-unit NO-UNDO.    

    DEFINE VARIABLE v-qty-case LIKE oe-boll.qty-case NO-UNDO. 
    DEFINE VARIABLE v-cases    LIKE oe-boll.cases NO-UNDO.   

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
        AND (v-stat EQ "A"                                   OR
        (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
        (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord 
        THEN
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord 
        THEN
        FIND FIRST soldto NO-LOCK
            WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ oe-ord.cust-no
            AND soldto.sold-id EQ oe-ord.sold-id USE-INDEX sold-id NO-ERROR. 

    IF AVAILABLE cust THEN
    FOR EACH oe-ordl NO-LOCK
       WHERE oe-ordl.company EQ oe-ord.company
/*           AND oe-ordl.i-no   GE v-fitem[1] */
/*           AND oe-ordl.i-no   LE v-fitem[2] */
          AND oe-ordl.ord-no EQ oe-ord.ord-no
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
          AND (NOT CAN-FIND(FIRST ttblJob) OR 
               CAN-FIND(FIRST ttblJob 
                           WHERE ttblJob.company EQ oe-ordl.company
                             AND ttblJob.job-no  EQ oe-ordl.job-no
                             AND ttblJob.job-no2 EQ oe-ordl.job-no2)
               ),
      EACH oe-boll  
       WHERE oe-boll.company EQ oe-ordl.company 
         AND oe-boll.ord-no  EQ oe-ordl.ord-no
         AND oe-boll.i-no    EQ oe-ordl.i-no
         AND oe-boll.bol-no  GE v-fbol-no[1]
         AND oe-boll.bol-no  LE v-fbol-no[2]
       USE-INDEX ord-no 
       NO-LOCK 
      BREAK BY oe-ordl.ord-no 
            BY oe-boll.i-no
            BY oe-boll.qty-case
            BY oe-boll.cases DESC 
        :    

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.

    FIND FIRST b-job-hdr 
        WHERE b-job-hdr.company = cocode 
        AND b-job-hdr.ord-no  = oe-ordl.ord-no  
        AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
    IF AVAILABLE b-job-hdr 
        THEN
        FIND FIRST b-job 
            WHERE b-job.company = b-job-hdr.company
            AND b-job.job     = b-job-hdr.job
            AND b-job.job-no  = b-job-hdr.job-no
            AND b-job.job-no2 = b-job-hdr.job-no2 NO-LOCK NO-ERROR.

    IF oe-ordl.est-no NE "" 
        THEN
        FIND FIRST eb
            WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

    ASSIGN 
        v-tot-pcs = v-tot-pcs + oe-boll.qty-case
        v-tot-bdl = v-tot-bdl + INT(fgBin())
        v-tot-qty = v-tot-qty + oe-boll.qty
        v-tot-par = v-tot-par + oe-boll.partial
        v-tot-unt = v-tot-unt + oe-boll.cases
        v-cases   = v-cases   + oe-boll.cases.


    IF /*NOT by-release OR */
        NOT v-printdetail OR
        NOT AVAILABLE oe-ordl THEN 
    DO:
        IF LAST-OF(oe-boll.qty-case) THEN 
        DO:

            FIND FIRST w-ord 
                WHERE w-ord.ord-no       = oe-boll.ord-no AND
                w-ord.i-no         = oe-boll.i-no   AND
                w-ord.pcs = oe-boll.qty-case NO-ERROR.
            IF NOT AVAILABLE w-ord THEN 
            DO:
                CREATE w-ord.
                ASSIGN
                    w-ord.bol-no       = oe-boll.bol-no
                    w-ord.ord-no       = oe-boll.ord-no  
                    w-ord.job-no       = oe-boll.job-no
                    w-ord.job-no2      = oe-boll.job-no2   
                    w-ord.cust-no      = oe-ord.cust-no    
                    w-ord.cust-name    = oe-ord.cust-name  
                    w-ord.i-no         = oe-boll.i-no      
                    w-ord.cust-part-no = oe-ordl.part-no   
                    w-ord.qty-before   = oe-boll.qty          
                    w-ord.po-no        = oe-boll.po-no     
                    w-ord.sold-code    = oe-ord.sold-id    
                    w-ord.sold-name    = oe-ord.sold-name  
                    w-ord.sold-add1    = oe-ord.sold-add[1]
                    w-ord.sold-add2    = oe-ord.sold-add[2]
                    w-ord.sold-city    = oe-ord.sold-city  
                    w-ord.sold-state   = oe-ord.sold-state 
                    w-ord.sold-zip     = oe-ord.sold-zip   
                    w-ord.i-name       = oe-ordl.i-name               
                    w-ord.due-date     = IF oe-ord.due-date NE ? 
                                     THEN oe-ord.due-date
                                     ELSE IF oe-ordl.req-date NE ? 
                                            THEN oe-ordl.req-date ELSE TODAY
                    w-ord.est-no       = oe-ordl.est-no                 
                    w-ord.form-no      = oe-ordl.form-no                
                    w-ord.vendor       = company.name                   
                    w-ord.tare-wt      = 10                             
                    w-ord.uom          = "EA"                           
                    w-ord.mult         = 1 /*if cust.int-field[1] ne 0 then 
                                     cust.int-field[1] else v-mult*/
                    w-ord.dont-run-set = oe-ordl.is-a-component         
                    w-ord.ord-desc1    = oe-ordl.part-dscr1             
                    w-ord.ord-desc2    = oe-ordl.part-dscr2             
                    w-ord.linenum      = oe-ordl.e-num                  
                    w-ord.tag-no       = oe-boll.tag
                    num-rec            = num-rec + 1.

                IF AVAILABLE b-job-hdr 
                    THEN w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? 
                        THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
                IF AVAILABLE b-job 
                    THEN w-ord.due-date-job = IF b-job.due-date <> ? 
                        THEN STRING(b-job.due-date, "99/99/9999") ELSE "".

                RUN get-rel-info (OUTPUT w-ord.cust-po-no, OUTPUT w-ord.rel-date, OUTPUT w-ord.rel-lot#).

                IF AVAILABLE itemfg 
                    THEN ASSIGN
                        w-ord.upc-no  = itemfg.upc-no     
                        w-ord.box-len = itemfg.l-score[50]
                        w-ord.box-wid = itemfg.w-score[50]
                        w-ord.box-dep = itemfg.d-score[50]
                        w-ord.flute   = itemfg.flute      
                        w-ord.test    = itemfg.test       
                        w-ord.bundle  = itemfg.case-pall  
                        w-ord.style   = itemfg.style.     

                IF w-ord.style NE "" THEN 
                DO:
                    FIND FIRST style 
                        WHERE style.company EQ cocode 
                        AND style.style EQ w-ord.style NO-LOCK NO-ERROR.
                    IF AVAILABLE style 
                        THEN ASSIGN w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.

                IF NOT AVAILABLE eb AND AVAILABLE itemfg AND itemfg.est-no NE "" 
                    THEN 
                    FIND FIRST eb
                        WHERE eb.company  EQ itemfg.company
                        AND eb.est-no   EQ itemfg.est-no
                        AND eb.stock-no EQ itemfg.i-no NO-LOCK NO-ERROR.
                IF AVAILABLE eb 
                    THEN ASSIGN
                        w-ord.flute  = eb.flute
                        w-ord.test   = eb.test
                        w-ord.bundle = eb.cas-pal
                        w-ord.cas-no = eb.cas-no.

                ASSIGN 
                    w-ord.bundle  = INT(fgBin())
                    lv-got-shipto = NO.

                EMPTY TEMP-TABLE w-shipto.

                FOR EACH oe-rel NO-LOCK
                    WHERE oe-rel.company EQ oe-ordl.company
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.line    EQ oe-ordl.line:

                    RUN oe/custxship.p (oe-rel.company, oe-rel.cust-no,
                        oe-rel.ship-id, BUFFER shipto).

                    IF AVAILABLE shipto THEN 
                    DO:
                        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

                        IF NOT CAN-FIND(FIRST w-shipto 
                            WHERE w-shipto.company EQ shipto.company
                            AND w-shipto.cust-no EQ shipto.cust-no
                            AND w-shipto.ship-no EQ shipto.ship-no) 
                            THEN 
                            CREATE w-shipto.
                        BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
                            ASSIGN
                            w-shipto.stat   = lv-stat
                            w-shipto.row-id = ROWID(oe-rel).
                    END.
                END.

                FOR EACH w-shipto,
                    FIRST oe-rel WHERE ROWID(oe-rel) EQ w-shipto.row-id NO-LOCK
                    BREAK BY oe-rel.rel-date
                    BY oe-rel.po-no
                    BY oe-rel.ship-no 
                    BY oe-rel.qty:
                    IF CAN-DO("L,S,I",w-shipto.stat) OR
                        LAST(oe-rel.rel-date) THEN 
                    DO:
                        ASSIGN
                            lv-got-shipto    = YES                 
                            w-ord.ship-code  = w-shipto.ship-id    
                            w-ord.ship-name  = w-shipto.ship-name  
                            w-ord.ship-add1  = w-shipto.ship-add[1]
                            w-ord.ship-add2  = w-shipto.ship-add[2]
                            w-ord.ship-city  = w-shipto.ship-city  
                            w-ord.ship-state = w-shipto.ship-state 
                            w-ord.ship-ctry  = w-shipto.country    
                            w-ord.ship-zip   = w-shipto.ship-zip.  
                        LEAVE.
                    END.
                END. 
                EMPTY TEMP-TABLE w-shipto.

                IF NOT lv-got-shipto THEN  
                    FOR EACH shipto
                        WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ oe-ord.cust-no 
                        USE-INDEX ship-id NO-LOCK
                        BREAK BY shipto.ship-no DESCENDING:
                        IF shipto.ship-id EQ oe-ord.cust-no OR
                            LAST(shipto.ship-no) THEN 
                        DO:
                            ASSIGN
                                w-ord.ship-code  = shipto.ship-id    
                                w-ord.ship-name  = shipto.ship-name  
                                w-ord.ship-add1  = shipto.ship-add[1]
                                w-ord.ship-add2  = shipto.ship-add[2]
                                w-ord.ship-city  = shipto.ship-city  
                                w-ord.ship-state = shipto.ship-state 
                                w-ord.ship-ctry  = shipto.country    
                                w-ord.ship-zip   = shipto.ship-zip.  
                            LEAVE.
                        END.
                    END.

                FIND FIRST soldto NO-LOCK
                    WHERE soldto.company EQ cocode
                    AND soldto.cust-no EQ oe-ord.cust-no
                    AND soldto.sold-id EQ oe-ord.sold-id USE-INDEX sold-id NO-ERROR.
                IF AVAILABLE soldto THEN w-ord.sold-ctry = soldto.country.

                ASSIGN
                    w-ord.total-unit = v-cases
                    w-ord.pcs        = oe-boll.qty-case
                    w-ord.ord-qty    = (w-ord.total-unit * w-ord.pcs)
                    v-cases          = 0
                    w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) +  
                                  (IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 
                                     THEN 0 ELSE 1).

            END. /* FIRST w-ord */
        END. /* LAST-of qty-case */
    END. /* NOT by-release */
    ELSE
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ cocode
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.line    EQ oe-ordl.line
            AND oe-rel.link-no NE 0:                    

            CREATE w-ord.
            ASSIGN
                w-ord.bol-no       = oe-boll.bol-no
                w-ord.ord-no       = oe-boll.ord-no
                w-ord.job-no       = oe-boll.job-no            
                w-ord.job-no2      = oe-boll.job-no2
                w-ord.cust-no      = oe-boll.cust-no
                w-ord.cust-name    = oe-ord.cust-name            
                w-ord.i-no         = oe-boll.i-no
                w-ord.cust-part-no = oe-ordl.part-no           
                w-ord.cust-po-no   = IF v-po-no-source EQ "L" 
                                      THEN oe-ordl.po-no
                                      ELSE
                                       IF v-po-no-source EQ "R" 
                                         THEN oe-rel.po-no
                                         ELSE oe-ord.po-no        

                w-ord.qty-before   = oe-boll.qty
                w-ord.ord-qty      = w-ord.qty-before *
                                    (1 + (w-ord.over-pct / 100))
                w-ord.po-no        = STRING(oe-ordl.po-no-po)
                w-ord.ship-code    = oe-rel.ship-id    
                w-ord.ship-add1    = oe-rel.ship-add[1]
                w-ord.ship-add2    = oe-rel.ship-add[2]
                w-ord.ship-city    = oe-rel.ship-city  
                w-ord.ship-state   = oe-rel.ship-state 
                w-ord.ship-zip     = oe-rel.ship-zip   
                w-ord.sold-code    = oe-ord.sold-id    
                w-ord.sold-name    = oe-ord.sold-name  
                w-ord.sold-add1    = oe-ord.sold-add[1]
                w-ord.sold-add2    = oe-ord.sold-add[2]
                w-ord.sold-city    = oe-ord.sold-city  
                w-ord.sold-state   = oe-ord.sold-state 
                w-ord.sold-zip     = oe-ord.sold-zip   
                w-ord.i-name       = oe-ordl.i-name    
                w-ord.due-date     = (IF oe-ord.due-date <> ? 
                                      THEN oe-ord.due-date
                                      ELSE
                                       IF oe-ordl.req-date <> ?  /* 9901 CAH */
                                         THEN oe-ordl.req-date
                                         ELSE TODAY )
                w-ord.rel-date     = oe-rel.rel-date                
                w-ord.est-no       = oe-ordl.est-no                 
                w-ord.form-no      = oe-ordl.form-no                
                w-ord.vendor       = company.name                   
                w-ord.tare-wt      = 10                             
                w-ord.uom          = "EA"                           
                w-ord.mult         = 1 /*IF cust.int-field[1] ne 0 
                                     THEN cust.int-field[1] ELSE v-mult*/
                w-ord.dont-run-set = oe-ordl.is-a-component         
                w-ord.ord-desc1    = oe-ordl.part-dscr1             
                w-ord.ord-desc2    = oe-ordl.part-dscr2             

                /* gdm - 08130804*/                                 
                w-ord.linenum      = oe-ordl.e-num
                w-ord.tag-no       = oe-boll.tag                                                    
                num-rec            = num-rec + 1.                   

            ASSIGN 
                w-ord.rel-lot# = oe-rel.lot-no.

            IF AVAILABLE itemfg 
                THEN
                ASSIGN
                    w-ord.upc-no  = itemfg.upc-no     
                    w-ord.box-len = itemfg.l-score[50]
                    w-ord.box-wid = itemfg.w-score[50]
                    w-ord.box-dep = itemfg.d-score[50]
                    w-ord.flute   = itemfg.flute      
                    w-ord.test    = itemfg.test       
                    w-ord.pcs     = itemfg.case-count 
                    w-ord.bundle  = itemfg.case-pall  
                    w-ord.style   = itemfg.style.     

            IF w-ord.style NE "" THEN 
            DO:

                FIND FIRST style 
                    WHERE style.company EQ cocode 
                    AND style.style EQ w-ord.style NO-LOCK NO-ERROR.
                IF AVAILABLE style THEN 
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF NOT AVAILABLE eb AND 
                AVAILABLE itemfg AND itemfg.est-no NE "" 
                THEN
                FIND FIRST eb
                    WHERE eb.company  EQ itemfg.company
                    AND eb.est-no   EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE eb 
                THEN
                ASSIGN
                    w-ord.flute  = eb.flute  
                    w-ord.test   = eb.test   
                    w-ord.cas-no = eb.cas-no.

            RUN oe/custxship.p (oe-rel.company,
                oe-rel.cust-no,
                oe-rel.ship-id,
                BUFFER shipto).

            IF AVAILABLE shipto 
                THEN
                ASSIGN
                    w-ord.ship-code  = shipto.ship-id     
                    w-ord.ship-name  = shipto.ship-name   
                    w-ord.ship-add1  = shipto.ship-add[1] 
                    w-ord.ship-add2  = shipto.ship-add[2] 
                    w-ord.ship-city  = shipto.ship-city   
                    w-ord.ship-state = shipto.ship-state  
                    w-ord.ship-ctry  = shipto.country     
                    w-ord.ship-zip   = shipto.ship-zip.   

            IF AVAILABLE soldto 
                THEN w-ord.sold-ctry = soldto.country.


            ASSIGN
                w-ord.pcs        = w-ord.pcs + oe-boll.qty-case
                w-ord.bundle     = oe-boll.cases-unit
                w-ord.total-unit = oe-boll.cases
                /* Add .49 to round up and add 1 for extra tag   */
                w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) +  
                                  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 
                                    THEN 0 ELSE 1.
        END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord-old C-Win 
PROCEDURE from-ord-old :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE lv-got-shipto AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-stat       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rel-date   AS DATE      NO-UNDO.


    DEFINE BUFFER b-job-hdr FOR job-hdr. /* rtc */
    DEFINE BUFFER b-job     FOR job.         /* rtc */
    DEFINE BUFFER b-oe-ordl FOR oe-ordl. 

    DEFINE VARIABLE v-tot-pcs LIKE w-ord.pcs NO-UNDO.
    DEFINE VARIABLE v-tot-bdl LIKE w-ord.bundle NO-UNDO.
    DEFINE VARIABLE v-tot-qty LIKE w-ord.ord-qty NO-UNDO.
    DEFINE VARIABLE v-tot-par LIKE w-ord.partial NO-UNDO.
    DEFINE VARIABLE v-tot-unt LIKE w-ord.total-unit NO-UNDO.    

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
        AND (v-stat EQ "A"                                   OR
        (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
        (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord 
        THEN
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord 
        THEN
        FIND FIRST soldto NO-LOCK
            WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ oe-ord.cust-no
            AND soldto.sold-id EQ oe-ord.sold-id USE-INDEX sold-id NO-ERROR. 

    IF AVAILABLE cust THEN
    FOR EACH oe-ordl NO-LOCK
       WHERE oe-ordl.company EQ oe-ord.company
/*           AND oe-ordl.i-no   GE v-fitem[1] */
/*           AND oe-ordl.i-no   LE v-fitem[2] */
          AND oe-ordl.ord-no EQ oe-ord.ord-no
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
          AND (NOT CAN-FIND(FIRST ttblJob) OR 
               CAN-FIND(FIRST ttblJob 
                           WHERE ttblJob.company EQ oe-ordl.company
                             AND ttblJob.job-no  EQ oe-ordl.job-no
                             AND ttblJob.job-no2 EQ oe-ordl.job-no2)
               ),
      EACH oe-boll  
       WHERE oe-boll.company EQ oe-ordl.company 
         AND oe-boll.ord-no  EQ oe-ordl.ord-no
         AND oe-boll.i-no    EQ oe-ordl.i-no
         AND oe-boll.bol-no  GE v-fbol-no[1]
         AND oe-boll.bol-no  LE v-fbol-no[2]
       USE-INDEX ord-no 
       NO-LOCK 
       BREAK BY oe-ordl.i-no:    

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.

    FIND FIRST b-job-hdr 
        WHERE b-job-hdr.company = cocode 
        AND b-job-hdr.ord-no  = oe-ordl.ord-no  
        AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
    IF AVAILABLE b-job-hdr 
        THEN
        FIND FIRST b-job 
            WHERE b-job.company = b-job-hdr.company
            AND b-job.job     = b-job-hdr.job
            AND b-job.job-no  = b-job-hdr.job-no
            AND b-job.job-no2 = b-job-hdr.job-no2 NO-LOCK NO-ERROR.

    IF oe-ordl.est-no NE "" 
        THEN
        FIND FIRST eb
            WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.



    ASSIGN 
        v-tot-pcs = v-tot-pcs + oe-boll.qty-case
        v-tot-bdl = v-tot-bdl + INT(fgBin())
        v-tot-qty = v-tot-qty + oe-boll.qty
        v-tot-par = v-tot-par + oe-boll.partial
        v-tot-unt = v-tot-unt +  oe-boll.cases.

    IF NOT by-release OR 
        NOT AVAILABLE oe-ordl THEN 
    DO:

        IF FIRST-OF(oe-ordl.i-no) THEN 
        DO:            


            CREATE w-ord.
            ASSIGN
                w-ord.bol-no       = oe-boll.bol-no
                w-ord.ord-no       = oe-boll.ord-no  
                w-ord.job-no       = oe-boll.job-no
                w-ord.job-no2      = oe-boll.job-no2 
                w-ord.cust-no      = oe-ord.cust-no
                w-ord.cust-name    = oe-ord.cust-name
                w-ord.i-no         = oe-boll.i-no
                w-ord.cust-part-no = oe-ordl.part-no

                w-ord.qty-before   = oe-boll.qty                 
                w-ord.ord-qty      = oe-boll.qty 
                w-ord.po-no        = oe-boll.po-no
                w-ord.sold-code    = oe-ord.sold-id
                w-ord.sold-name    = oe-ord.sold-name
                w-ord.sold-add1    = oe-ord.sold-add[1]
                w-ord.sold-add2    = oe-ord.sold-add[2]
                w-ord.sold-city    = oe-ord.sold-city
                w-ord.sold-state   = oe-ord.sold-state
                w-ord.sold-zip     = oe-ord.sold-zip
                w-ord.i-name       = oe-ordl.i-name                 
                w-ord.due-date     = IF oe-ord.due-date NE ? THEN
                                   oe-ord.due-date
                                 ELSE
                                 IF oe-ordl.req-date NE ? THEN
                                   oe-ordl.req-date
                                 ELSE TODAY
                w-ord.est-no       = oe-ordl.est-no
                w-ord.form-no      = oe-ordl.form-no
                w-ord.vendor       = company.name
                w-ord.tare-wt      = 10
                w-ord.uom          = "EA"
                w-ord.mult         = IF cust.int-field[1] NE 0 THEN
                                   cust.int-field[1] ELSE v-mult
                w-ord.dont-run-set = oe-ordl.is-a-component
                w-ord.ord-desc1    = oe-ordl.part-dscr1
                w-ord.ord-desc2    = oe-ordl.part-dscr2
                w-ord.linenum      = oe-ordl.e-num 
                num-rec            = num-rec + 1.

            IF AVAILABLE b-job-hdr THEN
                w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
            IF AVAILABLE b-job THEN
                w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".

            RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                OUTPUT w-ord.rel-date,
                OUTPUT w-ord.rel-lot#).


            IF AVAILABLE itemfg THEN
                ASSIGN
                    w-ord.upc-no  = itemfg.upc-no
                    w-ord.box-len = itemfg.l-score[50]
                    w-ord.box-wid = itemfg.w-score[50]
                    w-ord.box-dep = itemfg.d-score[50]
                    w-ord.flute   = itemfg.flute
                    w-ord.test    = itemfg.test
                    w-ord.pcs     = itemfg.case-count
                    w-ord.bundle  = itemfg.case-pall
                    w-ord.style   = itemfg.style.

            IF w-ord.style NE "" THEN
            DO:
                FIND FIRST style WHERE
                    style.company EQ cocode AND
                    style.style EQ w-ord.style
                    NO-LOCK NO-ERROR.

                IF AVAILABLE style THEN
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF NOT AVAILABLE eb AND AVAILABLE itemfg AND itemfg.est-no NE "" THEN
                FIND FIRST eb
                    WHERE eb.company  EQ itemfg.company
                    AND eb.est-no   EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN
                ASSIGN
                    w-ord.flute  = eb.flute
                    w-ord.test   = eb.test
                    w-ord.pcs    = eb.cas-cnt
                    w-ord.bundle = eb.cas-pal
                    w-ord.cas-no = eb.cas-no.

            ASSIGN 
                w-ord.pcs     = oe-boll.qty-case
                w-ord.bundle  = INT(fgBin())
                lv-got-shipto = NO.

            EMPTY TEMP-TABLE w-shipto.

            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.line    EQ oe-ordl.line:

                RUN oe/custxship.p (oe-rel.company,
                    oe-rel.cust-no,
                    oe-rel.ship-id,
                    BUFFER shipto).

                IF AVAILABLE shipto THEN 
                DO:
                    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

                    IF NOT CAN-FIND(FIRST w-shipto 
                        WHERE w-shipto.company EQ shipto.company
                        AND w-shipto.cust-no EQ shipto.cust-no
                        AND w-shipto.ship-no EQ shipto.ship-no) 
                        THEN
                        CREATE w-shipto.
                    BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
                        ASSIGN
                        w-shipto.stat   = lv-stat
                        w-shipto.row-id = ROWID(oe-rel).
                END.
            END.

            FOR EACH w-shipto,
                FIRST oe-rel WHERE ROWID(oe-rel) EQ w-shipto.row-id NO-LOCK
                BREAK BY oe-rel.rel-date
                BY oe-rel.po-no
                BY oe-rel.ship-no 
                BY oe-rel.qty:

                IF CAN-DO("L,S,I",w-shipto.stat) OR
                    LAST(oe-rel.rel-date)          THEN 
                DO:
                    ASSIGN
                        lv-got-shipto    = YES
                        w-ord.ship-code  = w-shipto.ship-id
                        w-ord.ship-name  = w-shipto.ship-name
                        w-ord.ship-add1  = w-shipto.ship-add[1]
                        w-ord.ship-add2  = w-shipto.ship-add[2]
                        w-ord.ship-city  = w-shipto.ship-city
                        w-ord.ship-state = w-shipto.ship-state
                        w-ord.ship-ctry  = w-shipto.country
                        w-ord.ship-zip   = w-shipto.ship-zip.
                    LEAVE.
                END.
            END.

            EMPTY TEMP-TABLE w-shipto.

            IF NOT lv-got-shipto THEN
                FOR EACH shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ oe-ord.cust-no
                    USE-INDEX ship-id NO-LOCK
                    BREAK BY shipto.ship-no DESCENDING:
                    IF shipto.ship-id EQ oe-ord.cust-no OR
                        LAST(shipto.ship-no)             THEN 
                    DO:
                        ASSIGN
                            w-ord.ship-code  = shipto.ship-id
                            w-ord.ship-name  = shipto.ship-name
                            w-ord.ship-add1  = shipto.ship-add[1]
                            w-ord.ship-add2  = shipto.ship-add[2]
                            w-ord.ship-city  = shipto.ship-city
                            w-ord.ship-state = shipto.ship-state
                            w-ord.ship-ctry  = shipto.country
                            w-ord.ship-zip   = shipto.ship-zip.
                        LEAVE.
                    END.
                END.

            FIND FIRST soldto NO-LOCK
                WHERE soldto.company EQ cocode
                AND soldto.cust-no EQ oe-ord.cust-no
                AND soldto.sold-id EQ oe-ord.sold-id
                USE-INDEX sold-id NO-ERROR.

            IF AVAILABLE soldto THEN w-ord.sold-ctry = soldto.country.

            ASSIGN
                w-ord.total-unit = w-ord.pcs * w-ord.bundle
                /* Add .49 to round up and add 1 for extra tag   */
                w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) +  
                                (IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 
                                    THEN 0 ELSE 1).
        END.  /* first-of */

    END.  /* not by-release */
    ELSE
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ cocode
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.line    EQ oe-ordl.line
            AND oe-rel.link-no NE 0:                    

            CREATE w-ord.
            ASSIGN
                w-ord.bol-no       = oe-boll.bol-no
                w-ord.ord-no       = oe-boll.ord-no
                w-ord.job-no       = oe-boll.job-no            
                w-ord.job-no2      = oe-boll.job-no2
                w-ord.cust-no      = oe-boll.cust-no
                w-ord.cust-name    = oe-ord.cust-name            
                w-ord.i-no         = oe-boll.i-no
                w-ord.cust-part-no = oe-ordl.part-no           
                w-ord.cust-po-no   = IF v-po-no-source EQ "L" 
                                      THEN oe-ordl.po-no
                                      ELSE
                                       IF v-po-no-source EQ "R" 
                                         THEN oe-rel.po-no
                                         ELSE oe-ord.po-no        

                w-ord.qty-before   = oe-boll.qty
                w-ord.ord-qty      = w-ord.qty-before *
                                    (1 + (w-ord.over-pct / 100))
                w-ord.po-no        = STRING(oe-ordl.po-no-po)
                w-ord.ship-code    = oe-rel.ship-id    
                w-ord.ship-add1    = oe-rel.ship-add[1]
                w-ord.ship-add2    = oe-rel.ship-add[2]
                w-ord.ship-city    = oe-rel.ship-city  
                w-ord.ship-state   = oe-rel.ship-state 
                w-ord.ship-zip     = oe-rel.ship-zip   
                w-ord.sold-code    = oe-ord.sold-id    
                w-ord.sold-name    = oe-ord.sold-name  
                w-ord.sold-add1    = oe-ord.sold-add[1]
                w-ord.sold-add2    = oe-ord.sold-add[2]
                w-ord.sold-city    = oe-ord.sold-city  
                w-ord.sold-state   = oe-ord.sold-state 
                w-ord.sold-zip     = oe-ord.sold-zip   
                w-ord.i-name       = oe-ordl.i-name    
                w-ord.due-date     = (IF oe-ord.due-date <> ? 
                                      THEN oe-ord.due-date
                                      ELSE
                                       IF oe-ordl.req-date <> ?  /* 9901 CAH */
                                         THEN oe-ordl.req-date
                                         ELSE TODAY )
                w-ord.rel-date     = oe-rel.rel-date                
                w-ord.est-no       = oe-ordl.est-no                 
                w-ord.form-no      = oe-ordl.form-no                
                w-ord.vendor       = company.name                   
                w-ord.tare-wt      = 10                             
                w-ord.uom          = "EA"                           
                w-ord.mult         = IF cust.int-field[1] NE 0 
                                     THEN cust.int-field[1] ELSE v-mult
                w-ord.dont-run-set = oe-ordl.is-a-component         
                w-ord.ord-desc1    = oe-ordl.part-dscr1             
                w-ord.ord-desc2    = oe-ordl.part-dscr2             

                /* gdm - 08130804*/                                 
                w-ord.linenum      = oe-ordl.e-num.                 

            num-rec            = num-rec + 1.                   

            ASSIGN 
                w-ord.rel-lot# = oe-rel.lot-no.

            IF AVAILABLE itemfg 
                THEN
                ASSIGN
                    w-ord.upc-no  = itemfg.upc-no     
                    w-ord.box-len = itemfg.l-score[50]
                    w-ord.box-wid = itemfg.w-score[50]
                    w-ord.box-dep = itemfg.d-score[50]
                    w-ord.flute   = itemfg.flute      
                    w-ord.test    = itemfg.test       
                    w-ord.pcs     = itemfg.case-count 
                    w-ord.bundle  = itemfg.case-pall  
                    w-ord.style   = itemfg.style.     

            IF w-ord.style NE "" THEN 
            DO:

                FIND FIRST style 
                    WHERE style.company EQ cocode 
                    AND style.style EQ w-ord.style NO-LOCK NO-ERROR.
                IF AVAILABLE style THEN 
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF NOT AVAILABLE eb AND 
                AVAILABLE itemfg AND itemfg.est-no NE "" 
                THEN
                FIND FIRST eb
                    WHERE eb.company  EQ itemfg.company
                    AND eb.est-no   EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE eb 
                THEN
                ASSIGN
                    w-ord.flute  = eb.flute  
                    w-ord.test   = eb.test   
                    w-ord.cas-no = eb.cas-no.

            RUN oe/custxship.p (oe-rel.company,
                oe-rel.cust-no,
                oe-rel.ship-id,
                BUFFER shipto).

            IF AVAILABLE shipto 
                THEN
                ASSIGN
                    w-ord.ship-code  = shipto.ship-id     
                    w-ord.ship-name  = shipto.ship-name   
                    w-ord.ship-add1  = shipto.ship-add[1] 
                    w-ord.ship-add2  = shipto.ship-add[2] 
                    w-ord.ship-city  = shipto.ship-city   
                    w-ord.ship-state = shipto.ship-state  
                    w-ord.ship-ctry  = shipto.country     
                    w-ord.ship-zip   = shipto.ship-zip.   

            IF AVAILABLE soldto 
                THEN w-ord.sold-ctry = soldto.country.


            ASSIGN
                w-ord.pcs        = w-ord.pcs + oe-boll.qty-case
                w-ord.bundle     = oe-boll.cases-unit
                w-ord.total-unit = oe-boll.cases
                /* Add .49 to round up and add 1 for extra tag   */
                w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) +  
                                  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 
                                    THEN 0 ELSE 1.
        END.

    IF LAST-OF(oe-ordl.i-no) THEN 
    DO:

        ASSIGN
            w-ord.pcs        = v-tot-pcs
            w-ord.bundle     = v-tot-bdl
            w-ord.ord-qty    = v-tot-qty
            w-ord.partial    = v-tot-par
            w-ord.total-unit = v-tot-unt 
            v-tot-pcs        = 0
            v-tot-bdl        = 0
            v-tot-qty        = 0
            v-tot-par        = 0
            v-tot-unt        = 0.

    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info C-Win 
PROCEDURE get-rel-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-pono LIKE w-ord.cust-po-no NO-UNDO.
    DEFINE OUTPUT PARAMETER op-date LIKE w-ord.rel-date NO-UNDO.
    DEFINE OUTPUT PARAMETER op-lot# LIKE w-ord.rel-lot# NO-UNDO.

    RELEASE oe-rell.
    RELEASE oe-rel.

    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line,
        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
        AND oe-relh.posted   EQ NO
        AND oe-relh.rel-date GE begin_date
        AND oe-relh.rel-date LE end_date
        BY oe-relh.rel-date
        BY oe-relh.r-no:

        ASSIGN
            op-pono = oe-rell.po-no
            op-date = oe-relh.rel-date.
        LEAVE.
    END.

    IF AVAILABLE oe-rell 
        THEN FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.
    ELSE
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            AND oe-rel.rel-no   EQ 0
            AND oe-rel.rel-date GE begin_date
            AND oe-rel.rel-date LE end_date
            BY oe-rel.rel-date
            BY oe-rel.r-no:

            ASSIGN
                op-pono = oe-rel.po-no
                op-date = oe-rel.rel-date.
            LEAVE.
        END.

    IF NOT AVAILABLE oe-rel 
        THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            BY oe-rel.rel-date
            BY oe-rel.r-no:

            ASSIGN 
                op-date = oe-rel.rel-date.
            LEAVE.
        END.

    ASSIGN 
        op-lot# = oe-rel.lot-no.
    ASSIGN 
        op-pono = oe-ord.po-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBolTagLabel C-Win 
PROCEDURE GetBolTagLabel :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FIND FIRST sys-ctrl-shipto NO-LOCK
        WHERE sys-ctrl-shipto.company      EQ cocode 
        AND sys-ctrl-shipto.NAME         EQ "BOlTagFile" 
        AND sys-ctrl-shipto.cust-vend    EQ YES 
        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
        AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
    IF AVAILABLE sys-ctrl-shipto AND 
        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
        scr-label-file:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sys-ctrl-shipto.char-fld.
    ELSE 
    DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK 
            WHERE sys-ctrl-shipto.company      EQ cocode 
            AND sys-ctrl-shipto.NAME         EQ "BolTagFile" 
            AND sys-ctrl-shipto.cust-vend    EQ YES 
            AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND 
            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
        ELSE 
        DO:
            FIND FIRST sys-ctrl-shipto NO-LOCK 
                WHERE sys-ctrl-shipto.company      EQ cocode 
                AND sys-ctrl-shipto.NAME         EQ "BolTagFile"
                AND sys-ctrl-shipto.cust-vend-no EQ ""
                AND sys-ctrl-shipto.cust-vend    EQ YES 
                NO-ERROR.
            IF AVAILABLE sys-ctrl-shipto AND 
                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
        /*  ELSE DO:
             FIND FIRST sys-ctrl WHERE
                  sys-ctrl.company EQ cocode AND
                  sys-ctrl.name    EQ "BolTagFile" 
                  NO-LOCK NO-ERROR.
             IF AVAIL sys-ctrl THEN
                scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
             ELSE
                scr-label-file:SCREEN-VALUE = "".
          END.
        */  
        END.
    END.


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

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    /*
     FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ gcompany
                           AND sys-ctrl.name    EQ "BOLTagFile" NO-LOCK NO-ERROR.
     IF NOT AVAIL sys-ctrl THEN
     DO TRANSACTION:
       CREATE sys-ctrl.
       ASSIGN
        sys-ctrl.company  = gcompany
        sys-ctrl.name     = "BOLTagFile"
        sys-ctrl.descrip  = "C:\BA\Label\BOL".
       FIND CURRENT sys-ctrl NO-LOCK.
     END.
   
     ASSIGN v-out  = sys-ctrl.descrip
            v-tag# = IF sys-ctrl.int-fld EQ 0 
                       THEN 1
                       ELSE sys-ctrl.int-fld
            v-Printdetail = sys-ctrl.char-fld = "Detail" 
            .
            scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld)
   
     */
    RUN run-report NO-ERROR. 

    IF NOT ERROR-STATUS:ERROR THEN lv-ok-ran = YES.  

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

    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
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
    RUN scr-rpt.w (list-name,c-win:TITLE). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-return C-Win 
PROCEDURE post-return :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-fg-recid AS RECID NO-UNDO.

    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-fg-bin   FOR fg-bin.
    DEFINE BUFFER b-itemfg   FOR itemfg.

    DEFINE VARIABLE li             AS INTEGER NO-UNDO.   
    DEFINE VARIABLE v-dec          AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-overrun-qty  LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-reduce-qty   AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-newhdr       AS LOG     NO-UNDO. 
    DEFINE VARIABLE v-fin-qty      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-est-no       AS cha     NO-UNDO.
    DEFINE VARIABLE v-one-item     AS LOG     NO-UNDO.
    DEFINE VARIABLE ld-cvt-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld-cvt-cost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-binqty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty          AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-tagcost      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE choice         AS LOG     NO-UNDO.
    DEFINE VARIABLE v-post-date    AS DATE    INIT TODAY NO-UNDO.
    DEFINE VARIABLE li-tag-no      AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-qty-changed AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-whs-item    AS LOG     NO-UNDO.


    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.

    /* create w/h transfer record*/   
    FIND FIRST itemfg WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ loadtag.i-no NO-ERROR.

    li = 1.
    FOR EACH bf-fg-rctd NO-LOCK BY bf-fg-rctd.r-no DESCENDING:
        LEAVE.
    END.
    IF AVAILABLE bf-fg-rctd THEN li = bf-fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
    FIND FIRST fg-bin WHERE fg-bin.company = cocode
        AND fg-bin.i-no = loadtag.i-no
        AND fg-bin.tag = ""
        AND fg-bin.qty >= loadtag.pallet-count NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fg-bin THEN RETURN.  

    CREATE bf-fg-rctd.
    ASSIGN
        bf-fg-rctd.r-no       = li + 1
        bf-fg-rctd.rct-date   = TODAY
        bf-fg-rctd.trans-time = TIME
        bf-fg-rctd.company    = cocode
        bf-fg-rctd.rita-code  = "I"
        bf-fg-rctd.i-name     = itemfg.i-name
        bf-fg-rctd.i-no       = loadtag.i-no
        bf-fg-rctd.job-no     = loadtag.job-no
        bf-fg-rctd.job-no2    = loadtag.job-no2
        bf-fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
        bf-fg-rctd.pur-uom    = itemfg.prod-uom
        bf-fg-rctd.cost-uom   = itemfg.prod-uom
        /*     bf-fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
        bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
        bf-fg-rctd.qty-case   = loadtag.qty-case

        bf-fg-rctd.partial    = loadtag.partial
        bf-fg-rctd.cases      = TRUNC(bf-fg-rctd.t-qty / bf-fg-rctd.qty-case,0)
        bf-fg-rctd.cases-unit = loadtag.case-bundle
        bf-fg-rctd.loc        = loadtag.loc
        bf-fg-rctd.loc-bin    = loadtag.loc-bin
        bf-fg-rctd.tag        = loadtag.tag-no
        bf-fg-rctd.loc2       = ""
        bf-fg-rctd.loc-bin2   = ""
        bf-fg-rctd.tag2       = ""
        .
    /* post later*/
    CREATE w-fg-rctd.
    BUFFER-COPY bf-fg-rctd TO w-fg-rctd.
    ASSIGN 
        w-fg-rctd.row-id = ROWID(bf-fg-rctd).
    {fg/fg-post.i w-fg-rctd w-fg-rctd}

    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT fg-bin NO-LOCK NO-ERROR.

    ASSIGN 
        bf-fg-rctd.rita-code = "P"  /* posted */
        bf-fg-rctd.post-date = v-post-date
        bf-fg-rctd.tag2      = w-fg-rctd.tag2. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    {oerep/r-BOLtag.i}

    IF scr-auto-print THEN
    DO:
        DEFINE VARIABLE v-int     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-path    AS CHARACTER NO-UNDO.

        LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
        USE "SOFTWARE".
        GET-KEY-VALUE SECTION "Teklynx\Label Matrix"
            KEY "PATH"
            VALUE v-path.
        UNLOAD "SOFTWARE".

        ASSIGN
            v-path    = v-path + "\lmwprint.exe "
            cFileName = "/L=" + scr-label-file.

        RUN WinExec (INPUT v-path + CHR(32) + cFileName , INPUT 1, OUTPUT
            v-int).
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
    DEFINE INPUT PARAMETER ip-fbolno LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-tbolno LIKE oe-boll.bol-no NO-UNDO.

    CREATE w-file.
    ASSIGN 
        w-key  = ip-rowid
        w-fbol = ip-fbolno
        w-tbol = ip-tbolno.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-ord C-Win 
PROCEDURE temp-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-ord-no LIKE oe-ord.ord-no  NO-UNDO.
    DEFINE INPUT PARAMETER ip-fbolno LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-tbolno LIKE oe-boll.bol-no NO-UNDO.

    FOR EACH oe-ord
        WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ ip-ord-no
        AND (v-stat EQ "A"                         OR
        (v-stat EQ "C" AND oe-ord.opened EQ NO) OR
        (v-stat EQ "O" AND oe-ord.opened EQ YES))
        NO-LOCK:

        RUN temp-create (ROWID(oe-ord),
            ip-fbolno,
            ip-tbolno).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fgBin C-Win 
FUNCTION fgBin RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ oe-boll.company
        AND fg-bin.job-no EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.i-no EQ oe-boll.i-no
        AND fg-bin.loc EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag EQ oe-boll.tag NO-ERROR.
    RETURN IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 0.

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

