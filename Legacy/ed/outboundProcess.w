&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-estmar.w

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
&GLOBAL-DEFINE summary-sheet 1
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}
{ed/edivars.i       "new shared"}
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

ASSIGN
    cocode = gcompany
    locode = gloc.

{ed/sharedv.i "new"}
{rc/ercvars.i 10 " " "new"}     /* 9808 CAH */
{rc/stringv.i}                  /* 9808 CAH */
{rc/statvars.i "new shared"}
DEFINE NEW SHARED STREAM s-out.
DEFINE NEW SHARED STREAM s-err.
DEFINE NEW SHARED FRAME hdg-std.
/* {rc/hdg-wide.i "ed/driver.p" "EDI PROCESSING EDIT LIST" "(s-out)" } */
/*DEF VAR ws_test-prod LIKE edcode.test-prod INITIAL FALSE NO-UNDO.
DEF VAR partner_list AS CHAR NO-UNDO    INITIAL "*"
  FORMAT "x(60)" LABEL "Partners".
DEF VAR setid_list AS CHAR NO-UNDO  INITIAL "850,860,852,832"
  FORMAT "x(60)" LABEL "Tran Sets".
DEF VAR ws_status AS CHAR NO-UNDO FORMAT 'x(30)' LABEL "Status".
DEF VAR process_tran AS LOGICAL NO-UNDO EXTENT 10 INITIAL FALSE
  LABEL "OK?".
DEF VAR keyfile AS CHAR NO-UNDO INITIAL '' EXTENT 10
  LABEL "Header File"
  FORMAT 'x(30)'.
DEF VAR status_msg AS CHAR NO-UNDO FORMAT 'x(15)' EXTENT 10
  LABEL "Status".
  */
DEFINE VARIABLE fill_date    AS DATE.
DEFINE VARIABLE fill_time    AS INTEGER     NO-UNDO.
DEFINE VARIABLE at_least_one AS LOGICAL NO-UNDO.
DEFINE VARIABLE f-det-title  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE did_some     AS LOGICAL NO-UNDO.
DEFINE VARIABLE run_ok       AS LOGICAL NO-UNDO.
FORM
    WITH FRAME f-current WIDTH 155 20 DOWN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 ws_company1 ws_test-prod ~
ws_print-opt1 setid_list partner_list top-debug1 edProcessStat btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS ws_company1 ws_test-prod ws_print-opt1 ~
setid_list partner_list ws_status top-debug1 edProcessStat 

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

DEFINE VARIABLE edProcessStat AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 105 BY 7.38 NO-UNDO.

DEFINE VARIABLE partner_list  AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
    LABEL "Partners" 
    VIEW-AS FILL-IN 
    SIZE 100.4 BY 1 NO-UNDO.

DEFINE VARIABLE setid_list    AS CHARACTER FORMAT "X(256)":U INITIAL "832,855,860,810,856" 
    LABEL "Trans Sets" 
    VIEW-AS FILL-IN 
    SIZE 100.6 BY 1 NO-UNDO.

DEFINE VARIABLE ws_company1   AS CHARACTER FORMAT "X(256)":U 
    LABEL "Company" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 TOOLTIP "Enter Company code to process" NO-UNDO.

DEFINE VARIABLE ws_status     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Status" 
    VIEW-AS FILL-IN 
    SIZE 60 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 120 BY 12.86.

DEFINE VARIABLE top-debug1    AS LOGICAL INITIAL NO 
    LABEL "Debug?" 
    VIEW-AS TOGGLE-BOX
    SIZE 13.4 BY .81 TOOLTIP "Enter Y to enable debugging output to rpro_dbg.txt" NO-UNDO.

DEFINE VARIABLE ws_print-opt1 AS LOGICAL INITIAL NO 
    LABEL "Print?" 
    VIEW-AS TOGGLE-BOX
    SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE ws_test-prod  AS LOGICAL INITIAL NO 
    LABEL "Test?" 
    VIEW-AS TOGGLE-BOX
    SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    ws_company1 AT ROW 3.38 COL 13 COLON-ALIGNED WIDGET-ID 2
    ws_test-prod AT ROW 5.05 COL 15 WIDGET-ID 18
    ws_print-opt1 AT ROW 6.48 COL 15 WIDGET-ID 16
    setid_list AT ROW 7.91 COL 13.4 COLON-ALIGNED WIDGET-ID 12
    partner_list AT ROW 9.57 COL 13.6 COLON-ALIGNED WIDGET-ID 14
    ws_status AT ROW 10.76 COL 13 COLON-ALIGNED WIDGET-ID 20
    top-debug1 AT ROW 12.24 COL 15 WIDGET-ID 10
    edProcessStat AT ROW 15.05 COL 9 NO-LABELS WIDGET-ID 22
    btn-ok AT ROW 23.86 COL 22
    btn-cancel AT ROW 23.86 COL 52
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 121 BY 24.57.


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
        TITLE              = "EDI Outbound Processing"
        HEIGHT             = 25
        WIDTH              = 122.4
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN ws_status IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* EDI Outbound Processing */
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
ON WINDOW-CLOSE OF C-Win /* EDI Outbound Processing */
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
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        RUN processOutbound.
        STATUS DEFAULT "Processing Complete".
   
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
    SUBSCRIBE TO "EDIEVENT" ANYWHERE.
    ws_direction = "O".
    /*
    setid_list = "".
    FOR EACH edcode
        WHERE edcode.direction = ws_direction 
        AND edcode.test-prod = yes /* ws_test-prod */
        AND CAN-DO(partner_list, edcode.partner):
      IF NOT CAN-DO(setid_list, edcode.setid) THEN
      DO:
        {rc/listadd.i setid_list edcode.setid}
      END.
    END.
    */
    RUN enable_UI.
    ws_company1:screen-value = cocode.
    {methods/nowait.i}
    /*
        DO WITH FRAME {&FRAME-NAME}:
            {custom/usrprint.i}
    
    END.
    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EdiEvent C-Win 
PROCEDURE EdiEvent :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipMessage AS CHARACTER NO-UNDO.
    IF edProcessStat:screen-value IN FRAME {&frame-name} EQ "" THEN 
        edProcessStat:insert-string("Processing..." + chr(10)) IN FRAME {&frame-name} 
            .

    edProcessStat:insert-string(ipMessage + chr(10)) IN FRAME {&frame-name} .
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
    DISPLAY ws_company1 ws_test-prod ws_print-opt1 setid_list partner_list 
        ws_status top-debug1 edProcessStat 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 ws_company1 ws_test-prod ws_print-opt1 setid_list partner_list 
        top-debug1 edProcessStat btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processOutbound C-Win
PROCEDURE processOutbound:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    ASSIGN
        ws_company   = ws_company1
        ws_print-opt = ws_print-opt1
        ws_direction = "O"
        ws_test-prod = ?
        .
    FIND FIRST edco WHERE edco.company EQ ws_company NO-LOCK.       
    FOR EACH edmast NO-LOCK
        WHERE CAN-DO(partner_list, edmast.partner):
        ws_partner = edmast.partner.
        IF top-debug THEN
            RUN rc/debugmsg.p ("Processing partner: " + edmast.partner).
        FOR EACH edcode NO-LOCK
           WHERE (EDCode.Partner = EDMast.partner
                  OR EDCode.Partner = EDMast.PartnerGrp)
            AND CAN-DO(setid_list, edcode.setid)
            AND (IF ws_test-prod = ? THEN TRUE ELSE edcode.test-prod = ws_test-prod)
            AND edcode.direction = ws_direction
            /* 9809 CAH: Added proc-order to sorting sequence */
            BREAK BY edcode.partner /* BY edcode.proc-order */BY edcode.setid:
      
            /*    MESSAGE "TEST  each edcode" edcode.setid view-as alert-box . */
      
            IF top-debug THEN
                RUN rc/debugmsg.p
                    ("Processing Code: "
                    + edcode.setid + '/'
                    + string(edcode.version) + '/'
                    + string(edcode.test-prod) + '/').
            ASSIGN 
                ws_erc      = 0 
                ws_erc_desc = ""   /* 9808 CAH */
                ws_edcode_rec = RECID(edcode)
                ws_setid = edcode.setid
                .
            IF ws_direction = "O" AND FIRST-OF (edcode.setid)
                THEN
            DO:
                /* run outbound touchups pre-interface if available */
                next_program = "ed" + dirsep + edco.system + dirsep +
                    "o" + edcode.setid + "fix.p".
      
                RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
                IF run_ok THEN
                DO:
                    IF top-debug THEN
                        RUN rc/debugmsg.p ("Running outbound preprocessor: "
                            + next_program).
                    RUN VALUE(next_program).
                    PAUSE 2.
                    next_program = "".
                END.
            END.
            run_ok = FALSE.
            _search:
            DO i = 1 TO 2:
                next_program =
                    "ed" + dirsep + (IF NOT customized THEN
                    (edco.translator + dirsep)
                    ELSE ""
                    )
                    +
                    (IF edcode.customized THEN
                    custom-proc
                    ELSE
                    ws_direction +
                    TRIM(edcode.setid) +
                    (IF version > 0
                    AND i = 1 THEN TRIM(STRING(edcode.version,"9999"))
                    ELSE ""))
                    + ".p".
                PAUSE 0.
                /*      DISPLAY next_program WITH FRAME f-current. */
                RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
                IF run_ok THEN 
                DO:
                    LEAVE _search.
                END.
                ELSE
                DO:
                    ws_status:screen-value IN FRAME {&frame-name} = "Program Not Found".
                    IF top-debug THEN
                        RUN rc/debugmsg.p
                            ( ws_status + ':
        ' + next_program).
                    PAUSE 0.
                    /* DISPLAY ws_status WITH FRAME f-view. */
                    PAUSE 1.
                END.
            END.    /* search i loop */
            IF run_ok THEN
            DO:
                PAUSE 0.
                ws_status:screen-value = "Processing: " + next_program.
                /*      DISPLAY ws_status WITH FRAME f-view. */
                PAUSE 0.
                STATUS DEFAULT (IF ws_direction = "I" THEN "Importing " ELSE "Exporting ")
                    + edco.translator + " for " + edcode.partner
                    + " set " + edcode.setid + " v " + STRING(edcode.version).
                IF top-debug THEN
                    RUN rc/debugmsg.p ("Before running: " +  next_program).
                HIDE FRAME f-current NO-PAUSE.
                    {rc/statdisp.i}
                IF INDEX(next_program, "write810") GT 0 THEN 
                    RUN VALUE(next_program) (INPUT cocode).
                ELSE 
                    RUN VALUE(next_program).
                IF top-debug THEN
                    RUN rc/debugmsg.p ("After running: " + next_program).
                did_some = TRUE.
                /*      DISPLAY ws_erc WITH FRAME f-current. */
                PAUSE 2.
                next_program = "".
                STATUS DEFAULT.
            END.
            IF ws_direction = "I" AND LAST-OF (edcode.setid)
                AND did_some THEN
            DO:
                /* run app system interface if available */
                next_program = "ed" + dirsep + edco.system + dirsep +
                    "i" + edcode.setid + ".p".
                /* 9809 CAH: Check for general interface/print ... */
                RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
                IF run_ok = FALSE THEN 
                DO:
                    next_program = "ed" + dirsep + "i" + edcode.setid + ".p".
                    RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
                END.
                IF top-debug THEN
                    RUN rc/debugmsg.p
                        ("Last of inbound code: " + edcode.setid + " looking for proc: "
                        + next_program).
                IF run_ok <> ? THEN
                DO:
                    PAUSE 0.
                    ws_status:screen-value = "Interfacing: " + next_program.
                    /*        DISPLAY ws_status WITH FRAME f-view. */
                    HIDE FRAME f-current NO-PAUSE.
                    RUN VALUE(next_program).
                    /*        DISPLAY ws_erc WITH FRAME f-current. */
                    PAUSE 2.
                    next_program = "".
                    STATUS DEFAULT.
                END.
            END.
            IF ws_erc <> 0 THEN
            DO:
                IF ws_erc_desc > ""
                    THEN
                DO erctokenx = 1 TO NUM-ENTRIES(ws_erc_desc):
                    erctoken[erctokenx] = ENTRY(erctokenx, ws_erc_desc).
                END.
                erclist = STRING(ws_erc).
                    {rc/ercput.i "stream s-out"}
            END.
        END.
    END.

    STATUS DEFAULT "Processing Complete".

   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


