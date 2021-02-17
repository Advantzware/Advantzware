&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: oe\InvoiceRegister.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: sewa.singh

  Created: 06/23/20

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
/*&SCOPED-DEFINE UIB_is_Running*/
&SCOPED-DEFINE CommonFile_is_Running

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name     AS cha       NO-UNDO.
DEFINE VARIABLE init-dir      AS CHA       NO-UNDO.
DEFINE VARIABLE lv-comp-curr  AS cha       NO-UNDO.
DEFINE VARIABLE oeprep-char   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-prof        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hPostInvoices AS HANDLE    NO-UNDO.
DEFINE VARIABLE iProcessed    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iValid        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPosted       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lError        AS LOG       NO-UNDO.
DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{oe\PostInvoice.i}

{sys/inc/VAR.i new shared}
DEFINE VARIABLE hNotesProcs     AS HANDLE NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.

RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN oe/PostInvoices.p PERSISTENT SET hPostInvoices.

ASSIGN
    cocode = gcompany
    locode = gloc.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE NEW SHARED BUFFER xoe-relh FOR oe-relh.
DEFINE NEW SHARED BUFFER yoe-relh FOR oe-relh.
DEFINE NEW SHARED BUFFER xoe-rell FOR oe-rell.
DEFINE NEW SHARED BUFFER inv-line FOR inv-line.  

DEFINE NEW SHARED VARIABLE v-ar-acct         LIKE ar-ctrl.receivables.
DEFINE NEW SHARED VARIABLE v-ar-freight      LIKE ar-ctrl.freight.
DEFINE NEW SHARED VARIABLE v-ar-stax         LIKE ar-ctrl.stax.
DEFINE NEW SHARED VARIABLE v-ar-sales        LIKE ar-ctrl.sales.
DEFINE NEW SHARED VARIABLE v-ar-disc         LIKE ar-ctrl.discount.
DEFINE NEW SHARED VARIABLE v-return          AS LOG       INIT NO.
DEFINE NEW SHARED VARIABLE v-start2-compress AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-end2-compress   AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-post            AS LOG       INIT NO.
DEFINE NEW SHARED VARIABLE v-trnum           AS INTEGER.
DEFINE NEW SHARED VARIABLE v-back            LIKE itemfg.q-back.
DEFINE NEW SHARED VARIABLE v-balance         AS DECIMAL   FORMAT ">>>,>>>,>>9.99cr".
DEFINE NEW SHARED VARIABLE v-reduce-ord-bal  LIKE cust.ord-bal NO-UNDO.
DEFINE NEW SHARED VARIABLE v-invline         AS RECID.
DEFINE NEW SHARED VARIABLE v-invhead         AS RECID.
DEFINE NEW SHARED VARIABLE v-detail          AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-gldetail        AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.


DEFINE            VARIABLE v-postable        AS LOG       INIT NO.
DEFINE            VARIABLE dInvQty           LIKE oe-ordl.inv-qty.
DEFINE            VARIABLE iOrdNo            LIKE inv-line.ord-no.
DEFINE            VARIABLE dtOrdDate         AS DATE.
DEFINE            VARIABLE ld-temp-amt       AS DECIMAL.
DEFINE            VARIABLE v-tax-rate        LIKE stax.tax-rate1.
DEFINE            VARIABLE v-uninv-ordl-amt  LIKE oe-ordl.t-price NO-UNDO INIT 0.
DEFINE            VARIABLE v-tmp-tax-rate    AS DECIMAL   FORMAT ">,>>9.99<<<".
DEFINE            VARIABLE v-line-tot        LIKE inv-line.t-price.
DEFINE            VARIABLE dMiscTot          LIKE inv-misc.amt.
DEFINE            VARIABLE v-line-tot-w      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-post-zero-cgs   AS LOG       NO-UNDO.


DEFINE            VARIABLE v-s-inv-no        LIKE inv-head.inv-no INIT 0 NO-UNDO FORMAT ">>>>>>9".
DEFINE            VARIABLE v-e-inv-no        LIKE v-s-inv-no INIT 9999999.
DEFINE            VARIABLE v-s-date          LIKE inv-head.inv-date FORMAT "99/99/9999"
    INIT 01/01/0001 NO-UNDO.
DEFINE            VARIABLE v-e-date          LIKE v-s-date INIT TODAY.
DEFINE            VARIABLE v-cost            AS DECIMAL   EXTENT 4.
DEFINE            VARIABLE v-cas-cnt         LIKE itemfg.case-count.
DEFINE            VARIABLE v-close-qty       LIKE oe-ordl.qty.
DEFINE            VARIABLE v-tax             AS DECIMAL.
DEFINE            VARIABLE v-invalid         AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-list-name      LIKE list-name NO-UNDO.
DEFINE            VARIABLE v-print-fmt       AS cha       NO-UNDO.
DEFINE            VARIABLE ll-warned         AS LOG       NO-UNDO.
DEFINE            VARIABLE v-ttl-tax         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ttl-rate        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE cItemFgCat        LIKE itemfg.procat NO-UNDO.

DEFINE VARIABLE cFieldInProcess AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldPostType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldUserId    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldDateTime  AS CHARACTER NO-UNDO.


{oe/ttSaveLine.i}
  
{oe/invwork.i new}

{oe/closchk.i new}

RUN oe/getacct.p.

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

IF v-return THEN RETURN.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPOST"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    MESSAGE "Creating new System Control record (INVPOST).".
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "INVPOST"
        sys-ctrl.log-fld = NO
        sys-ctrl.descrip = "Post cost-of-goods sold when cost is zero?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
v-post-zero-cgs = sys-ctrl.log-fld.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
v-print-fmt = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "". 
    
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
  
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.
DEFINE VARIABLE lv-audit-dir   AS CHARACTER NO-UNDO.

DO TRANSACTION:
    {sys/inc/postdate.i}
    {sys/inc/oeprep.i}
    {sys/inc/oeclose.i}
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS".
    END.

    lv-audit-dir = sys-ctrl.char-fld.

    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

    RELEASE sys-ctrl.
END.



&SCOPED-DEFINE use-factored

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_cust end_cust ~
begin_inv end_inv begin_date end_date tb_detailed tb_detailed-2 tb_ton ~
tb_export rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
begin_inv end_inv begin_date end_date tb_detailed tb_detailed-2 tb_ton ~
tb_export rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Cust No" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Ending Cust No" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 999999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.38.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Invoice Report Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_detailed-2 AS LOGICAL INITIAL no 
     LABEL "G/L Report Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_cust AT ROW 5.05 COL 27 COLON-ALIGNED
     end_cust AT ROW 5.05 COL 69 COLON-ALIGNED
     begin_inv AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 7.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 7.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_detailed AT ROW 8.76 COL 36
     tb_detailed-2 AT ROW 9.71 COL 36
     tb_ton AT ROW 10.67 COL 36
     tb_export AT ROW 11.62 COL 36
     rd-dest AT ROW 14.81 COL 5 NO-LABEL
     lv-ornt AT ROW 15.05 COL 29 NO-LABEL
     lines-per-page AT ROW 15.05 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 17.43 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 18.38 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.81 COL 30
     btn-ok AT ROW 22.19 COL 24
     btn-cancel AT ROW 22.19 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 13.86 COL 3
     RECT-6 AT ROW 14.33 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 22.91.


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
         TITLE              = "Invoice Register"
         HEIGHT             = 23.24
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ton:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
ON END-ERROR OF C-Win /* Invoice Register */
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
ON WINDOW-CLOSE OF C-Win /* Invoice Register */
DO:         
        RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
                                        INPUT "postStartDtTm", INPUT cocode, INPUT STRING("OB4-" + STRING(cocode)), INPUT YES, 
                                        OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime). 
        /* This event will close the window and terminate the procedure.  */
        DELETE OBJECT hNotesProcs.
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
        ASSIGN {&self-name}.
        IF LASTKEY NE -1 AND MONTH(begin_date) NE MONTH(tran-date)  THEN
        DO:
            RUN displayMessage("42").
            APPLY "entry" TO  begin_date.
            RETURN NO-APPLY.
        END.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    
        RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
                                        INPUT "postStartDtTm", INPUT cocode, INPUT STRING("OB4-" + STRING(cocode)), INPUT YES, 
                                        OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime). 
                                        
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
        DEFINE VARIABLE lv-post      AS LOG       NO-UNDO.
        DEFINE VARIABLE v-close-line AS LOG       NO-UNDO.
        DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cReason      AS CHARACTER NO-UNDO.
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.
        IF (MONTH(begin_date) NE MONTH(tran-date)) OR (MONTH(end_date) NE MONTH(tran-date)) THEN
        DO:
            RUN displayMessage("42").
            APPLY "entry" TO  begin_date.
            RETURN NO-APPLY.
        END.

        SESSION:SET-WAIT-STATE ("general").   

        ASSIGN
            rd-dest
            tran-period
            tran-date.

        EMPTY TEMP-TABLE ttInvoiceToPost .
        EMPTY TEMP-TABLE ttInvoiceLineToPost.
        EMPTY TEMP-TABLE ttInvoiceMiscToPost.
        EMPTY TEMP-TABLE ttGLTransaction.

        RUN run-report.
      
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_inv
                            &END_cust=END_inv
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE=''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE=''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        IF v-postable THEN 
        DO:

            lv-post = NO.
            
            IF v-balance = 0 THEN
                MESSAGE "Post Invoices?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE lv-post.

            IF lv-post THEN 
            DO:   
                RUN PostInvoices IN hPostInvoices (
                    cocode,
                    INT(begin_inv),
                    INT(end_inv),
                    DATE(begin_date),
                    DATE(end_date),
                    begin_cust,
                    end_cust,
                    DATE(tran-date),
                    "export,post",                        
                    OUTPUT iProcessed,
                    OUTPUT iValid,
                    OUTPUT iPosted,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).                     
                    
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX. 
                                
                IF oeclose-log THEN
                DO:
                   RUN pGetInvOrder.
                
                   RUN pCreateWOrd.
                
                   IF CAN-FIND (FIRST w-ord) THEN
                   RUN oe/d-close.w.
                END.
                
            END.
        END.

        ELSE MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.  
        
        SESSION:SET-WAIT-STATE("").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
        ASSIGN {&self-name}.
        
        IF LASTKEY NE -1 AND MONTH(end_date) NE MONTH(tran-date) THEN
        DO:
            RUN displayMessage("42").
            APPLY "entry" TO  end_date.
            RETURN NO-APPLY.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Invoice Report Detailed? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed-2 C-Win
ON VALUE-CHANGED OF tb_detailed-2 IN FRAME FRAME-A /* G/L Report Detailed? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Invoices? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ton C-Win
ON VALUE-CHANGED OF tb_ton IN FRAME FRAME-A /* Print $/Ton? */
DO:
        IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
            lv-ornt:SCREEN-VALUE = "L".
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
            RUN valid-date NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON VALUE-CHANGED OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
        ll-warned = NO.
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

    DO TRANSACTION:
        {sys/inc/inexport.i}
    END.

    FIND FIRST inv-head
        WHERE inv-head.company EQ cocode
        AND inv-head.posted  EQ NO
        AND inv-head.printed EQ YES
        AND inv-head.stat    NE "H"
        USE-INDEX prnt NO-LOCK NO-ERROR.
    IF AVAILABLE inv-head THEN begin_inv = inv-head.inv-no.

    end_date = TODAY.

    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}

        IF postdate-log THEN 
        DO:
            ASSIGN
                tran-date:SCREEN-VALUE = STRING(TODAY)
                tran-date              = TODAY.
            RUN check-date.
        END.

        ELSE
            ASSIGN
                tran-date:SCREEN-VALUE   = ""
                tran-period:SCREEN-VALUE = "".

        IF LOOKUP(v-print-fmt,"Frankstn,MIRPKG,ContSrvc,CSC-GA") GT 0
            THEN tb_export:SENSITIVE = YES.
        ELSE ASSIGN tb_export              = NO
                tb_export:SCREEN-VALUE = "NO" 
                tb_export:SENSITIVE    = NO.

        APPLY "entry" TO tran-date.
    END.
    
    RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
                                        INPUT "postStartDtTm", INPUT cocode, INPUT STRING("OB4-" + cocode), INPUT NO, 
                                        OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).
    IF cFieldInProcess EQ "Yes" THEN
    DO:    
        MESSAGE "Another user " cFieldUserId " started posting from " cFieldPostType " at " cFieldDateTime " and this process does not " 
        "support multiple people posting at the same time. Please try again later." VIEW-AS ALERT-BOX INFO.
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
              
    RUN displayMessage("41").    
  

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tax-gr C-Win 
PROCEDURE calc-tax-gr :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-head-row AS ROWID.
    DEFINE INPUT PARAMETER ipi-inv-no LIKE inv-head.inv-no NO-UNDO.

    /*DEF VAR v-ttl-tax AS DEC NO-UNDO.
    DEF VAR v-ttl-rate AS DEC NO-UNDO.
    
    def var v-tax-rate as dec extent 4.     */
    DEFINE BUFFER bf-currency FOR currency.
    DEFINE BUFFER bf-inv-head FOR inv-head.

    DEFINE VARIABLE k      AS INTEGER.
    DEFINE VARIABLE dAccum AS DECIMAL NO-UNDO.

    FIND bf-inv-head WHERE ROWID(bf-inv-head) = ipr-head-row NO-LOCK NO-ERROR.

    IF NOT AVAILABLE bf-inv-head THEN
        RETURN.

    FIND FIRST bf-currency NO-LOCK
        WHERE bf-currency.company     EQ bf-inv-head.company
        AND bf-currency.c-code      EQ bf-inv-head.curr-code[1]
        AND bf-currency.ar-ast-acct NE ""
        AND bf-currency.ex-rate     GT 0
        NO-ERROR.

    ASSIGN 
        v-ttl-tax  = 0
        v-ttl-rate = 0.
    FIND FIRST stax
        {sys/ref/stax1W.i}
        AND {sys/ref/taxgroup.i stax} EQ bf-inv-head.tax-gr
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax
            WHERE stax.company = bf-inv-head.company AND
            stax.tax-group EQ bf-inv-head.tax-gr
            NO-LOCK NO-ERROR.
    dAccum = 1.
    IF AVAILABLE stax THEN 
    DO:
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            v-tax-rate[i] = stax.tax-rate1[i].
            IF stax.accum-tax THEN 
            DO: 
                /*##PN - must find effective rate since this is accumulated*/
                dAccum = dAccum  * (1 + v-tax-rate[i] / 100).
                v-tax-rate[i] = 100 * (dAccum - (v-ttl-rate / 100) - 1).
            END.
            IF stax.company EQ "yes" AND i GT 1 THEN
            DO k = 1 TO i - 1:
                v-tax-rate[i] = v-tax-rate[i] +
                    (v-tax-rate[i] * (stax.tax-rate1[k] / 100)).
            END.
            v-ttl-rate = v-ttl-rate + v-tax-rate[i].
        END.

        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            ASSIGN 
                v-tax-rate[i] = ROUND(v-tax-rate[i] / v-ttl-rate *
                                     bf-inv-head.t-inv-tax,2)
                v-ttl-tax     = v-ttl-tax + v-tax-rate[i].
        END.

        IF bf-inv-head.t-inv-tax NE v-ttl-tax THEN
            v-tax-rate[1] = v-tax-rate[1] +
                (bf-inv-head.t-inv-tax - v-ttl-tax).

        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ stax.tax-acc1[i]
                NO-LOCK NO-ERROR.

            IF AVAILABLE account AND v-tax-rate[i] NE 0 THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(ipi-inv-no,"9999999")
                    tt-report.key-04  = bf-inv-head.tax-gr
                    tt-report.key-05  = STRING(v-tax-rate[i] *
                                      (IF AVAILABLE bf-currency  THEN
                                         bf-currency.ex-rate ELSE 1))
                    tt-report.weight  = v-line-tot-w *
                               (v-tax-rate[i] / bf-inv-head.t-inv-tax).
            END. /* avail account */

        END. /* 1 to 3 */

    END. /* avail stax */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tons C-Win 
PROCEDURE calc-tons :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-i-no LIKE itemfg.i-no NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-weight AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-itemfg FOR itemfg.


    FIND FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ ip-i-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE b-itemfg AND b-itemfg.weight-100 NE 0 THEN
        op-weight = b-itemfg.weight-100 * ip-qty / 100.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
        IF AVAILABLE period THEN 
        DO:
            IF NOT period.pstat THEN 
            DO:
                MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            tran-period:SCREEN-VALUE = STRING(period.pnum).
        END.

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
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
  DISPLAY tran-date tran-period begin_cust end_cust begin_inv end_inv begin_date 
          end_date tb_detailed tb_detailed-2 tb_ton tb_export rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust begin_inv end_inv 
         begin_date end_date tb_detailed tb_detailed-2 tb_ton tb_export rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-gl C-Win 
PROCEDURE list-gl :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    {sys/form/r-top3w.f} 

    DEFINE VARIABLE v-gl-sales    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-dscr        LIKE account.dscr NO-UNDO.
    DEFINE VARIABLE v-disp-actnum LIKE account.actnum NO-UNDO.
    DEFINE VARIABLE v-disp-amt    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-tmp-amt     AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-empty       AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE ld-t          AS DECIMAL   FORMAT "->>>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton       AS DECIMAL   FORMAT "->>>>>>9.999" NO-UNDO.
    DEFINE VARIABLE lv-label-ton  AS CHARACTER FORMAT "x(22)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-recid       AS RECID     INIT ?.
    DEFINE VARIABLE lv-rowid      AS ROWID     NO-UNDO.
    DEFINE VARIABLE dRecTot       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotOffSet    AS DECIMAL   NO-UNDO.

    DEFINE BUFFER b-tt-report FOR tt-report.

    FORMAT HEADER
        "G/L ACCOUNT NUMBER       "
        "DESCRIPTION                                  "
        "DATE      "
        "         AMOUNT"
        lv-label-ton[1]
        SKIP
        "-------------------------"
        "---------------------------------------------"
        "----------"
        "---------------"
        lv-label-ton[2]

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-s PAGE-TOP WIDTH 200 STREAM-IO.

    FORMAT HEADER
        "G/L ACCOUNT NUMBER       "
        "DESCRIPTION                                  "
        "INVOICE#"
        "ITEM#          "
        "         AMOUNT"
        "          TOTAL"
        lv-label-ton[1]
        SKIP
        "-------------------------"
        "---------------------------------------------"
        "--------"
        "---------------"
        "---------------"
        "---------------"
        lv-label-ton[2]

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-d PAGE-TOP WIDTH 200 STREAM-IO.

    FORM v-disp-actnum
        v-dscr
        tran-date      FORMAT "99/99/9999"
        v-disp-amt
        ld-pton FORMAT "->>>>>>9.999"
        ld-t[2]
        SKIP
        WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 200 FRAME gl-sum.

    FORM account.actnum     
        v-dscr                  
        inv-head.inv-no   FORMAT ">>>>>>>>"           
        inv-line.i-no                  
        v-tmp-amt                     
        v-empty 
        ld-pton FORMAT "->>>>>>9.999"
        ld-t[1]  
        SKIP                   

        WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 200 FRAME gl-det.


    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = TRIM(c-win:TITLE) + " - GL POSTING REGISTER - RUN# " + TRIM(STRING(v-trnum))
        {sys/inc/ctrtext.i str-tit2 112}

        str-tit3 = "Period " + string(tran-period,"99") + " - " +
              IF AVAILABLE period THEN
                (STRING(period.pst) + " to " + string(period.pend)) ELSE ""
        {sys/inc/ctrtext.i str-tit3 132}.

    IF tb_ton THEN
        ASSIGN
            lv-label-ton[1] = "       $/TON      TONS"
            lv-label-ton[2] = "------------ ---------".

    post-print: DO WHILE TRUE.
        SESSION:SET-WAIT-STATE ("general").

        list-name = TRIM(lv-list-name) + ".001".

        {sys/inc/outprint.i value(lines-per-page)}

        VIEW FRAME r-top.

        IF v-gldetail THEN VIEW FRAME f-top-d.
        ELSE VIEW FRAME f-top-s.

        PAGE.
                 
        /** LIST G/L FOR LINE ITEMS **/
        {oe/InvoiceRegister.i LINE "ITEMS"}

        /** LIST G/L FOR MISC. **/
        {oe/InvoiceRegister.i MISC "MISC."}

        /** LIST G/L FOR SALES TAX **/
        {oe/InvoiceRegister.i TAX "SALES TAX"}

        /** LIST G/L FOR CURRENCY GAIN/LOSS **/
        {oe/InvoiceRegister.i work-curr "CURRENCY GAIN/LOSS"}

        /** LIST G/L FOR FG/COGS **/
        IF v-gldetail THEN 
        DO:
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.

            FOR EACH ttGLTransaction
                WHERE (ttGLTransaction.transactionType EQ "COGS" OR ttGLTransaction.transactionType EQ "FG")
                BREAK BY ttGLTransaction.account
                BY ttGLTransaction.invoiceID:
                FIND FIRST account WHERE account.company = cocode AND
                    account.actnum  = ttGLTransaction.account
                    NO-LOCK NO-ERROR.
                IF AVAILABLE account THEN
                    ASSIGN v-dscr = account.dscr.
                ELSE
                    ASSIGN v-dscr = "ACCOUNT NOT FOUND - " + ttGLTransaction.account.

                ACCUMULATE ttGLTransaction.amount (TOTAL BY ttGLTransaction.account).
                ld-t[1] = ttGLTransaction.quantityWeight / 2000.

                /*IF tmp-work-job.fg THEN
                    ASSIGN v-tmp-amt  = - ttGLTransaction.amount
                        v-disp-amt = v-disp-amt - ttGLTransaction.amount
                        ld-t[1]    = - ld-t[1].
                ELSE */
                ASSIGN 
                    v-tmp-amt  = ttGLTransaction.amount
                    v-disp-amt = v-disp-amt + ttGLTransaction.amount.

                ASSIGN
                    ld-t[2] = ld-t[2] + ld-t[1]
                    ld-pton = v-tmp-amt / ld-t[1].

                IF ld-pton EQ ? THEN ld-pton = 0.
                         
                DISPLAY ttGLTransaction.account @ account.actnum
                    v-dscr
                    ttGLTransaction.invoiceID @ inv-head.inv-no FORMAT ">>>>>>9"
                    ttGLTransaction.itemID   @ inv-line.i-no
                    v-tmp-amt
                    ld-pton FORMAT "->>>>>>9.999" 
                    WHEN tb_ton 
                    ld-t[1] 
                    WHEN tb_ton
                    WITH FRAME gl-det.
                DOWN WITH FRAME gl-det.

                IF LAST-OF(ttGLTransaction.account) THEN 
                DO:
                    PUT v-disp-amt TO 128.
                    IF tb_ton THEN 
                    DO:
                        ld-pton = v-disp-amt / ld-t[2].
                        IF ld-pton EQ ? THEN ld-pton = 0.
                        PUT ld-pton FORMAT "->>>>>>9.999" TO 141 ld-t[2] TO 151 SKIP(1).
                    END.
                    ELSE PUT SKIP.
                    ASSIGN
                        v-disp-amt = 0
                        ld-t[2]    = 0.
                END.
            END.
        END.

        FOR EACH ttGLTransaction 
            WHERE (ttGLTransaction.transactionType EQ "FG" OR ttGLTransaction.transactionType EQ "COGS")
            BREAK BY ttGLTransaction.account:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = ttGLTransaction.account
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN
                ASSIGN v-dscr = account.dscr.
            ELSE
                ASSIGN v-dscr = "ACCOUNT NOT FOUND - " + ttGLTransaction.account.

            ASSIGN 
                v-disp-actnum = ttGLTransaction.account
                ld-t[2]       = ttGLTransaction.quantityWeight / 2000.

            /*IF work-job.fg THEN
                ASSIGN v-disp-amt = - work-job.amt
                    ld-t[2]    = - ld-t[2].
            ELSE*/
            IF FIRST-OF(ttGLTransaction.account) THEN
            v-disp-amt = 0.
            
            ASSIGN 
                v-disp-amt = v-disp-amt + ttGLTransaction.amount.

            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

            IF NOT v-gldetail AND LAST-OF(ttGLTransaction.account) THEN 
            DO:
                DISPLAY v-disp-actnum
                    v-dscr
                    tran-date
                    v-disp-amt
                    ld-pton FORMAT "->>>>>>9.999" 
                    WHEN tb_ton 
                    ld-t[2] 
                    WHEN tb_ton
                    WITH FRAME gl-sum.
                DOWN WITH FRAME gl-sum.
            END.
            ASSIGN
                ld-t[3] = ld-t[3] + ld-t[2].
        END. /* each work-job */
        
        
        /** POST FREIGHT TO G/L **/
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-freight
            NO-LOCK NO-ERROR.
        ASSIGN
            v-dscr     = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - FREIGHT"
            v-disp-amt = 0
            ld-t[2]    = 0.

        
        FOR EACH ttGLTransaction
            WHERE ttGLTransaction.transactionType  EQ "FREIGHT"
            NO-LOCK
            BREAK BY ttGLTransaction.account:

            ASSIGN
                ld-t[1]    = ttGLTransaction.quantityWeight / 2000
                v-disp-amt = v-disp-amt + dec(ttGLTransaction.amount)
                ld-t[2]    = ld-t[2] + ld-t[1].
                    
                    
            IF v-gldetail THEN              
                IF dec(ttGLTransaction.amount) NE 0 THEN 
                DO:
                    ld-pton = dec(ttGLTransaction.amount) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                    DISPLAY ttGLTransaction.account  @ account.actnum
                        v-dscr
                        int(ttGLTransaction.invoiceID) @ inv-head.inv-no FORMAT ">>>>>>9"
                        "FREIGHT"             @ inv-line.i-no
                        dec(ttGLTransaction.amount) @ v-tmp-amt
                        ld-pton FORMAT "->>>>>>9.999" 
                        WHEN tb_ton 
                        ld-t[1] 
                        WHEN tb_ton
                        WITH FRAME gl-det.
                    DOWN WITH FRAME gl-det.
                END.
        END.

        IF v-disp-amt NE 0 AND v-gldetail THEN 
        DO:
            PUT v-disp-amt TO 128.
            IF tb_ton THEN 
            DO:
                ld-pton = v-disp-amt / ld-t[2].
                IF ld-pton EQ ? THEN ld-pton = 0.
                PUT ld-pton FORMAT "->>>>>>9.999" TO 141 ld-t[2] TO 151 SKIP(1).
            END.
            ELSE PUT SKIP.
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.
        END.  


        IF NOT v-gldetail THEN 
        DO:             
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.
            IF v-disp-amt EQ ? THEN v-disp-amt = 0.

            DISPLAY v-disp-actnum
                v-dscr
                tran-date
                v-disp-amt
                ld-pton FORMAT "->>>>>>9.999" 
                WHEN tb_ton 
                ld-t[2] 
                WHEN tb_ton
                WITH FRAME gl-sum.
            DOWN WITH FRAME gl-sum.
        END.
                     
        /** POST DISCOUNT TO G/L **/
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-disc
            NO-LOCK NO-ERROR.
        ASSIGN
            v-dscr     = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - DISCOUNT"
            v-disp-amt = 0
            ld-t[2]    = 0.          
        
        FOR EACH ttGLTransaction
            WHERE ttGLTransaction.transactionType  EQ "DISC"
            NO-LOCK
            BREAK BY ttGLTransaction.account:

            ASSIGN
                ld-t[1]    = ttGLTransaction.quantityWeight / 2000
                v-disp-amt = v-disp-amt + dec(ttGLTransaction.amount)
                ld-t[2]    = ld-t[2] + ld-t[1].
                    
            IF v-gldetail THEN
                IF dec(ttGLTransaction.amount) NE 0 THEN 
                DO:
                    ld-pton = dec(ttGLTransaction.amount) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                    DISPLAY ttGLTransaction.account             @ account.actnum
                        v-dscr
                        int(ttGLTransaction.invoiceID) @ inv-head.inv-no FORMAT ">>>>>>9"
                        "DISCOUNT"            @ inv-line.i-no
                        dec(ttGLTransaction.amount) @ v-tmp-amt
                        ld-pton FORMAT "->>>>>>9.999" 
                        WHEN tb_ton 
                        ld-t[1] 
                        WHEN tb_ton
                        WITH FRAME gl-det.
                    DOWN WITH FRAME gl-det.
                END.
        END.

        IF v-disp-amt NE 0 AND v-gldetail THEN 
        DO:
            PUT v-disp-amt TO 128.
            IF tb_ton THEN 
            DO:
                ld-pton = v-disp-amt / ld-t[2].
                IF ld-pton EQ ? THEN ld-pton = 0.
                PUT ld-pton FORMAT "->>>>>>9.999" TO 141 ld-t[2] TO 151 SKIP(1).
            END.
            ELSE PUT SKIP.
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.
        END.        

        ASSIGN
            v-disp-actnum = v-ar-disc
            v-disp-amt    = v-post-disc
            ld-t[2]       = v-post-disc-w / 2000.

        IF NOT v-gldetail THEN 
        DO:                         
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.
            IF v-disp-amt EQ ? THEN v-disp-amt = 0.
            
            DISPLAY v-disp-actnum
                v-dscr
                tran-date
                v-disp-amt
                ld-pton FORMAT "->>>>>>9.999" 
                WHEN tb_ton 
                ld-t[2] 
                WHEN tb_ton
                WITH FRAME gl-sum.
            DOWN WITH FRAME gl-sum.
        END.
                
        /** POST CASH TO G/L **/
        IF v-post-cash NE 0 THEN 
        DO:
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ ar-ctrl.cash-act
                NO-LOCK NO-ERROR.
            v-dscr = IF AVAILABLE account THEN account.dscr
            ELSE "ACCOUNT NOT FOUND - CASH".              
            
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.

            FOR EACH ttGLTransaction
                WHERE ttGLTransaction.transactionType  EQ "CASH"
                NO-LOCK
                BREAK BY ttGLTransaction.account:

                ASSIGN
                    ld-t[1]    = ttGLTransaction.quantityWeight / 2000
                    v-disp-amt = v-disp-amt + dec(ttGLTransaction.amount)
                    ld-t[2]    = ld-t[2] + ld-t[1].
                    
                IF v-gldetail THEN
                    IF dec(ttGLTransaction.amount) NE 0 THEN 
                    DO:
                        ld-pton = dec(ttGLTransaction.amount) / ld-t[1].

                        IF ld-pton EQ ? THEN ld-pton = 0.

                        DISPLAY ttGLTransaction.account    @ account.actnum
                            v-dscr
                            int(ttGLTransaction.invoiceID)  @ inv-head.inv-no FORMAT ">>>>>>9"
                            "CASH INVOICE"      @ inv-line.i-no
                            dec(ttGLTransaction.amount)  @ v-tmp-amt
                            ld-pton FORMAT "->>>>>>9.999" 
                            WHEN tb_ton 
                            ld-t[1] 
                            WHEN tb_ton
                            WITH FRAME gl-det.
                        DOWN WITH FRAME gl-det.
                    END.
            END.

            IF v-disp-amt NE 0 AND v-gldetail THEN 
            DO:
                PUT v-disp-amt TO 128.
                IF tb_ton THEN 
                DO:
                    ld-pton = v-disp-amt / ld-t[2].
                    IF ld-pton EQ ? THEN ld-pton = 0.
                    PUT ld-pton FORMAT "->>>>>>9.999" TO 141 ld-t[2] TO 151 SKIP(1).
                END.
                ELSE PUT SKIP.
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.
            END.             

            ASSIGN
                v-disp-actnum = ar-ctrl.cash-act
                v-disp-amt    = v-post-cash
                ld-t[2]       = v-post-cash-w / 2000.

            IF NOT v-gldetail THEN 
            DO:                  
                ld-pton = v-disp-amt / ld-t[2].

                IF ld-pton EQ ? THEN ld-pton = 0.

                DISPLAY v-disp-actnum
                    v-dscr
                    tran-date
                    v-disp-amt
                    ld-pton FORMAT "->>>>>>9.999" 
                    WHEN tb_ton 
                    ld-t[2] 
                    WHEN tb_ton 
                    WITH FRAME gl-sum.
                DOWN WITH FRAME gl-sum.
            END.
            IF v-disp-amt EQ ? THEN v-disp-amt = 0.           
        END.  
        /** OFFSET ENTRY TO G/L **/ 
        dTotOffSet = 0.
        FOR EACH ttGLTransaction
            WHERE ttGLTransaction.transactionType  EQ "AR"
            NO-LOCK
            BREAK BY ttGLTransaction.account:
                       
            FIND FIRST account
                WHERE account.company = cocode
                AND account.actnum  = ttGLTransaction.account
                NO-LOCK NO-ERROR.
                     
            IF FIRST-OF(ttGLTransaction.account) THEN
                ASSIGN
                    v-disp-amt = 0
                    dRecTot    = 0.
                         
            ASSIGN
                v-dscr        = IF AVAILABLE account THEN account.dscr
                                       ELSE "ACCOUNT NOT FOUND - OFFSET"
                v-disp-actnum = ttGLTransaction.account
                v-disp-amt    = v-disp-amt + dec(ttGLTransaction.amount)
                /*dRecTot       = dRecTot + dec(tt-report.key-06).                 */
                dTotOffSet    = dTotOffSet + dec(ttGLTransaction.amount) .
                
            IF v-gldetail THEN 
                DO:
                    ASSIGN
                        ld-t[1] = dRecTot / 2000
                        ld-pton = v-disp-amt / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                    DISPLAY ttGLTransaction.account     @ account.actnum
                        v-dscr
                        ttGLTransaction.amount    @ v-tmp-amt
                        ld-pton FORMAT "->>>>>>9.999" 
                        WHEN tb_ton 
                        ld-t[1] 
                        WHEN tb_ton 
                        WITH FRAME gl-det.
                    DOWN WITH FRAME gl-det.  
                END.    
                
            IF LAST-OF(ttGLTransaction.account) THEN 
            DO:
                
                IF NOT v-gldetail THEN 
                DO:
                    ASSIGN
                        ld-t[2] = dRecTot / 2000
                        ld-pton = v-disp-amt / ld-t[2].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                    DISPLAY v-disp-actnum
                        v-dscr
                        tran-date
                        v-disp-amt
                        ld-pton FORMAT "->>>>>>9.999" 
                        WHEN tb_ton 
                        ld-t[2] 
                        WHEN tb_ton 
                        WITH FRAME gl-sum.
                    DOWN WITH FRAME gl-sum.
                END.
                    
            END.
        END.    /* for each "act-rece"*/ 
        v-balance = 0 .
        FOR EACH ttGLTransaction NO-LOCK
                BREAK BY ttGLTransaction.account:  
           ASSIGN                     
            v-balance = v-balance + dec(ttGLTransaction.amount) .
        END.     
        
        IF v-gldetail THEN
            PUT dTotOffSet FORMAT "->>,>>>,>>9.99" TO 126 SKIP 
                "---------------"  TO 128 SKIP
                "Total:" AT 86 v-balance TO 128 SKIP.
        ELSE
            PUT "---------------"  TO 104 SKIP
                "Total:" AT 79 v-balance TO 104 SKIP.

        SESSION:SET-WAIT-STATE ("").

       
        LEAVE.
    END. /* post-print */

    OUTPUT CLOSE.

    IF OPSYS EQ "unix" THEN 
    DO:
        UNIX SILENT cat VALUE(list-name) >> VALUE(lv-list-name).
        UNIX SILENT rm  VALUE(list-name).
    END.

    ELSE 
    DO:
        DOS SILENT TYPE VALUE(list-name) >> VALUE(lv-list-name).
        DOS SILENT DEL  VALUE(list-name).
    END.

    list-name = lv-list-name.

    SESSION:SET-WAIT-STATE ("general").

    
    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-post-inv C-Win 
PROCEDURE list-post-inv :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-list-post AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-oe-boll FOR oe-boll.

    DEFINE VARIABLE ld-t            AS DECIMAL FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton         AS DECIMAL FORMAT "->>>>>>9.999" NO-UNDO.
    DEFINE VARIABLE v-close-line-ok AS LOGICAL INITIAL NO.
    DEFINE VARIABLE v-first         AS LOG     INIT YES.
    DEFINE VARIABLE v-tot-frt       AS DECIMAL NO-UNDO.
  
    DEFINE VARIABLE dWeight         AS DECIMAL NO-UNDO.
  
    DEFINE BUFFER bf-inv-line FOR inv-line .
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
  
    FORMAT
        inv-head.inv-no FORMAT ">>>>>>9" AT 1
        inv-head.inv-date AT 9 FORMAT "99/99/99"
        inv-head.cust-no AT 18
        inv-head.cust-name FORMAT "x(25)" AT 27
        iOrdNo TO 60
        dInvQty
        inv-head.t-inv-freight FORMAT "->,>>9.99"
        inv-head.t-inv-tax FORMAT "->,>>>,>>9.99"
        dMiscTot FORMAT "->>>>9.99"
        v-line-tot FORMAT "->>>>>>9.99"
        inv-head.t-inv-rev FORMAT "->>,>>>,>>9.99" TO 139
        ld-pton FORMAT "->>>>>>9.999"
        ld-t[2]
        WITH STREAM-IO WIDTH 180 NO-LABELS NO-BOX NO-UNDERLINE FRAME inv.

    FORMAT
        bf-inv-line.i-no AT 10 LABEL "Item"
        bf-inv-line.i-name FORMAT "x(25)" LABEL "Description"
        bf-inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"
        bf-inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "
        bf-inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"
        bf-inv-line.t-cost FORMAT "->>>,>>9.99<<<<" LABEL "Cost"
        bf-inv-line.price FORMAT "->>>,>>9.999999" LABEL "Price"
        bf-inv-line.pr-uom LABEL "UOM"
        bf-inv-line.t-price FORMAT "->>,>>>,>>9.999999" COLUMN-LABEL "Extended! Price"
        v-prof  FORMAT "->>>>>9.99%" COLUMN-LABEL "Profit"
        WITH DOWN NO-BOX STREAM-IO WIDTH 180 FRAME invl.

    FORMAT
        bf-inv-line.i-no AT 10 LABEL "Item"
        bf-inv-line.i-name FORMAT "x(25)" LABEL "Description"
        bf-inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"
        bf-inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "
        bf-inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"
        bf-inv-line.t-cost FORMAT "->>>,>>9.99<<<<" LABEL "Cost"
        bf-inv-line.price FORMAT "->>>,>>9.999999" LABEL "Price"
        bf-inv-line.pr-uom LABEL "UOM"
        bf-inv-line.t-price FORMAT "->>,>>>,>>9.999999" COLUMN-LABEL "Extended! Price"
        ld-pton FORMAT "->>>>>>9.999" COLUMN-LABEL "!     $/Ton"
        ld-t[1] COLUMN-LABEL "!      Tons"
        v-prof  FORMAT "->>>>>9.99%" COLUMN-LABEL "Profit"
        WITH DOWN NO-BOX STREAM-IO WIDTH 180 FRAME invlt.

    FORMAT
        bf-inv-misc.charge AT 10 LABEL "Charge"
        bf-inv-misc.dscr LABEL "Description"
        bf-inv-misc.amt FORMAT "->>>,>>9.999999" TO 71 LABEL "Price" SKIP
        WITH STREAM-IO DOWN NO-BOX FRAME invm.

    SESSION:SET-WAIT-STATE ("general").

    RUN oe/invpostd.p ("").

    v-post = ip-list-post EQ "post".

  
    ordblock:
    FOR EACH ttInvoiceToPost NO-LOCK 
        WHERE ttInvoiceToPost.isOKToPost , 
        FIRST inv-head WHERE ROWID(inv-head) EQ ttInvoiceToPost.riInvHead

        TRANSACTION

        BY ttInvoiceToPost.invoiceID:
      
        ASSIGN
            v-postable       = YES
            v-reduce-ord-bal = 0
            dInvQty          = 0
         
            v-line-tot       = 0
            dMiscTot         = 0
            v-line-tot-w     = 0
         
            iOrdNo           = 0
            dtOrdDate        = ?.
         
      
        FOR EACH ttInvoiceLineToPost 
            WHERE ttInvoiceLineToPost.rNo EQ ttInvoiceToPost.rNo 
            AND ttInvoiceLineToPost.isOKToPost,
            FIRST bf-inv-line EXCLUSIVE-LOCK 
            WHERE ROWID(bf-inv-line) EQ ttInvoiceLineToPost.riInvLine BREAK BY ttInvoiceLineToPost.orderID:
                
            ASSIGN
                dInvQty = dInvQty + bf-inv-line.inv-qty .

            IF FIRST(ttInvoiceLineToPost.orderID) THEN
                ASSIGN
                    iOrdNo    = bf-inv-line.ord-no
                    dtOrdDate = bf-inv-line.ord-date.
             
            v-line-tot   = v-line-tot   + bf-inv-line.t-price.
             
            RUN calc-tons (bf-inv-line.i-no, bf-inv-line.inv-qty, OUTPUT dWeight).
            v-line-tot-w = v-line-tot-w + dWeight. 
        END.
        
        FOR EACH ttInvoiceMiscToPost NO-LOCK
            WHERE ttInvoiceMiscToPost.rNo EQ ttInvoiceToPost.rNo
            AND  ttInvoiceMiscToPost.isOKToPost:
            
            dMiscTot = dMiscTot + ttInvoiceMiscToPost.amountBilled.
            
        END.
          
          
        ASSIGN
            ld-t[2] = v-line-tot-w / 2000
            ld-t[3] = ld-t[3] + v-line-tot-w
            ld-pton = inv-head.t-inv-rev / ld-t[2].

        IF ld-pton EQ ? THEN ld-pton = 0.   
          
        ASSIGN
            v-post-total   = v-post-total   + inv-head.t-inv-rev
            v-post-total-w = v-post-total-w + v-line-tot-w.
         
        IF inv-head.terms EQ "CASH" AND inv-head.t-inv-rev NE 0 THEN 
        DO:
            ASSIGN
                v-post-cash    = v-post-cash    + inv-head.t-inv-rev
                v-post-total   = v-post-total   - inv-head.t-inv-rev
                v-post-cash-w  = v-post-cash-w  + v-line-tot-w
                v-post-total-w = v-post-total-w - v-line-tot-w.           
        END.
      
        DISPLAY inv-head.inv-no inv-head.inv-date
            inv-head.cust-no inv-head.cust-name iOrdNo
            dInvQty inv-head.t-inv-freight inv-head.t-inv-tax
            dMiscTot v-line-tot inv-head.t-inv-rev
            ld-pton FORMAT "->>>>>>9.999" 
            WHEN tb_ton
            ld-t[2] 
            WHEN tb_ton
            WITH FRAME inv.
        DOWN WITH FRAME inv.
        
        cItemFgCat = "" .
      
        IF v-detail THEN 
        DO:
            FOR EACH ttInvoiceLineToPost 
                WHERE ttInvoiceLineToPost.rNo EQ ttInvoiceToPost.rNo 
                AND ttInvoiceLineToPost.isOKToPost,
                FIRST bf-inv-line EXCLUSIVE-LOCK 
                WHERE ROWID(bf-inv-line) EQ ttInvoiceLineToPost.riInvLine BREAK BY ttInvoiceLineToPost.orderID:

                FIND FIRST itemfg
                    WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ ttInvoiceLineToPost.itemID
                    NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN
                    ASSIGN cItemFgCat = itemfg.procat.
                    
                RUN calc-tons (bf-inv-line.i-no, bf-inv-line.inv-qty, OUTPUT dWeight).

                IF tb_ton THEN 
                DO WITH FRAME invlt:
                    ASSIGN
                        ld-t[1] = dWeight / 2000
                        ld-pton = ttInvoiceLineToPost.amountBilled / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.
                    v-prof = (ttInvoiceLineToPost.amountBilled - bf-inv-line.t-cost) / ttInvoiceLineToPost.amountBilled * 100.
                    DISPLAY bf-inv-line.i-no bf-inv-line.i-name bf-inv-line.qty
                        bf-inv-line.inv-qty bf-inv-line.ship-qty 
                        bf-inv-line.price bf-inv-line.pr-uom bf-inv-line.t-price 
                        WHEN bf-inv-line.t-price GT 0
                        ld-pton FORMAT "->>>>>>9.999" 
                        WHEN tb_ton 
                        ld-t[1] 
                        WHEN tb_ton
                        v-prof 
                        WHEN v-prof NE ?.
                    DOWN.
                END.
                ELSE
                DO WITH FRAME invl:
                    v-prof = (ttInvoiceLineToPost.amountBilled - bf-inv-line.t-cost) / ttInvoiceLineToPost.amountBilled * 100.
                    DISPLAY bf-inv-line.i-no bf-inv-line.i-name bf-inv-line.qty
                        bf-inv-line.inv-qty bf-inv-line.ship-qty bf-inv-line.t-cost
                        bf-inv-line.price bf-inv-line.pr-uom bf-inv-line.t-price 
                        WHEN bf-inv-line.t-price GT 0
                        v-prof 
                        WHEN v-prof NE ?.
                    DOWN.
                END.

                IF cItemFgCat NE "" THEN
                    PUT SKIP
                        "FG Category: " AT 10 cItemFgCat AT 26 .

                /*delete w-inv-line.*/
                IF LAST(ttInvoiceLineToPost.orderID) THEN
                    PUT SKIP(1).
            END.

            FOR EACH ttInvoiceMiscToPost
                WHERE ttInvoiceMiscToPost.rNo EQ ttInvoiceToPost.rNo ,
                FIRST bf-inv-misc
                WHERE ROWID(bf-inv-misc) EQ ttInvoiceMiscToPost.riInvMisc 
                BREAK BY ttInvoiceMiscToPost.orderID WITH FRAME invm:
                IF FIRST(ttInvoiceMiscToPost.orderID) THEN
                    PUT "Miscellaneous" AT 10 SKIP.
                DISPLAY bf-inv-misc.charge bf-inv-misc.dscr bf-inv-misc.amt.
                IF bf-inv-misc.bill EQ "N" THEN
                    DISPLAY "       N/C" @ bf-inv-misc.amt.
                DOWN.              
                IF LAST(ttInvoiceMiscToPost.orderID) THEN
                    PUT SKIP(1).
            END. /* each w-inv-line */
         

            /*else*/
            DOWN WITH FRAME inv.
        END. /* not v-post */        
    
    
    END.

    SESSION:SET-WAIT-STATE ("").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateWOrd C-Win 
PROCEDURE pCreateWOrd :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
    ------------------------------------------------------------------------------*/        
    
    FOR EACH w-ord :
     DELETE w-ord.
    END.     
    
    FOR EACH ttOrderToUpdate 
        WHERE ttOrderToUpdate.isClose :  
         CREATE w-ord.
         ASSIGN 
             w-ord.ord-no = ttOrderToUpdate.orderID
             w-ord.rec-id = ttOrderToUpdate.reOeOrd.
    END.       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetHeader C-Win 
PROCEDURE pGetHeader :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        -----------------------------------------------------------------------------*/
       
    DEFINE VARIABLE httSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE httTarget AS HANDLE NO-UNDO.
    
    
    ASSIGN          
        httTarget = TEMP-TABLE ttInvoiceToPost:HANDLE.
    httSource = DYNAMIC-FUNCTION("fGetInvoiceToPostHandle" IN hPostInvoices).       
    httTarget:COPY-TEMP-TABLE( httSource,?,?,?,?).       
    
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetInvLine C-Win 
PROCEDURE pGetInvLine :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE httSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE httTarget AS HANDLE NO-UNDO.     
    
    ASSIGN          
        httTarget = TEMP-TABLE ttInvoiceLineToPost:HANDLE.

    httSource = DYNAMIC-FUNCTION("fGetInvoiceLineToPostHandle" IN hPostInvoices).

    /* Use the COPY-TEMP-TABLE( ) method without any options */
    httTarget:COPY-TEMP-TABLE( httSource,?,?,?,?).       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetInvOrder C-Win 
PROCEDURE pGetInvOrder :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE httSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE httTarget AS HANDLE NO-UNDO.  
        
    ASSIGN          
        httTarget = TEMP-TABLE ttOrderToUpdate:HANDLE.

    httSource = DYNAMIC-FUNCTION("fGetInvoiceOrderPostHandle" IN hPostInvoices).

    /* Use the COPY-TEMP-TABLE( ) method without any options */
    httTarget:COPY-TEMP-TABLE( httSource,?,?,?,?).  
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMiscLine C-Win 
PROCEDURE pGetMiscLine :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE httSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE httTarget AS HANDLE NO-UNDO.    
    
    ASSIGN          
        httTarget = TEMP-TABLE ttInvoiceMiscToPost:HANDLE.

    httSource = DYNAMIC-FUNCTION("fGetInvoiceMiscToPostHandle" IN hPostInvoices).

    /* Use the COPY-TEMP-TABLE( ) method without any options */
    httTarget:COPY-TEMP-TABLE( httSource,?,?,?,?).       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGLTransaction C-Win 
PROCEDURE pGLTransaction :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE httSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE httTarget AS HANDLE NO-UNDO.    
    
    ASSIGN          
        httTarget = TEMP-TABLE ttGLTransaction:HANDLE.

    httSource = DYNAMIC-FUNCTION("fGetGLTransactionHandle" IN hPostInvoices).

    /* Use the COPY-TEMP-TABLE( ) method without any options */
    httTarget:COPY-TEMP-TABLE( httSource,?,?,?,?).       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunApiOutboundTrigger C-Win 
PROCEDURE pRunApiOutboundTrigger :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.

    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
   

    IF AVAILABLE ipbf-inv-head THEN 
    DO:
    
        ASSIGN 
            cAPIID       = "SendInvoice"
            cTriggerID   = "PostInvoice"
            cPrimaryID   = STRING(ipbf-inv-head.inv-no)
            cDescription = cAPIID + " triggered by " + cTriggerID + " from r-inve&pN.w for Invoice: " + cPrimaryID
            . 

        RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
            INPUT  ipbf-inv-head.company,         /* Company Code (Mandatory) */
            INPUT  locode,                        /* Location Code (Mandatory) */
            INPUT  cAPIID,                        /* API ID (Mandatory) */
            INPUT  ipbf-inv-head.cust-no,         /* Scope ID */
            INPUT  "Customer",                    /* Scope Type */
            INPUT  cTriggerID,                    /* Trigger ID (Mandatory) */
            INPUT  "inv-head",                    /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  STRING(ROWID(ipbf-inv-head)),  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  cPrimaryID,                    /* Primary ID for which API is called for (Mandatory) */   
            INPUT  cDescription,                  /* Event's description (Optional) */
            OUTPUT lSuccess,                      /* Success/Failure flag */
            OUTPUT cMessage                       /* Status message */
            ) NO-ERROR.

        RUN Outbound_ResetContext IN hdOutboundProcs.
    END. /*avail inv-head*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER xinv-head FOR inv-head.
    DEFINE VARIABLE str-tit4                AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE lv-label-ton            AS CHARACTER FORMAT "x(20)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-contsrvc-export-found AS LOG       NO-UNDO.
    DEFINE VARIABLE v-goodman-export-found  AS LOG       NO-UNDO.
    
   
    {sys/form/r-top3w.f}

    FORMAT HEADER
        str-tit4 AT 58
        SKIP(1)
        "  - Invoice - " SKIP
        "Number"  "Date" AT 11  "Cust#" AT 18 "Customer Name" AT 27 "Order#" TO 60
        "Quantity" TO 75 "Frt" TO 85 "Tax" TO 99
        "Misc" TO 109 "Items" TO 121
        "Total" TO 139 
        lv-label-ton[1] TO 163
        FILL("=",142) FORMAT "x(142)"
        lv-label-ton[2] TO 163
        WITH FRAME r-top WIDTH 180.

    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.

    ASSIGN         
        v-s-inv-no = begin_inv
        v-e-inv-no = end_inv
        v-s-date   = begin_date
        v-e-date   = end_date
        v-detail   = tb_detailed
        v-gldetail = tb_detailed-2
        v-postable = NO.

    IF tb_ton THEN
        ASSIGN
            lv-label-ton[1] = "     $/Ton      Tons"
            lv-label-ton[2] = "===========================".
            
    
    RUN PostInvoices IN hPostInvoices (
        cocode,
        v-s-inv-no,
        v-e-inv-no,
        v-s-date,
        v-e-date,
        begin_cust,
        end_cust,
        TODAY,
        "",
        OUTPUT iProcessed,
        OUTPUT iValid,
        OUTPUT iPosted,
        OUTPUT lError,
        OUTPUT cMessage
        ).
        
    RUN pGetHeader .
      
    RUN pGetInvLine .
      
    RUN pGetMiscLine .
      
    RUN pGLTransaction .
    
    FIND FIRST ttInvoiceLineToPost NO-LOCK NO-ERROR.
    IF AVAIL ttInvoiceLineToPost THEN
    v-trnum = ttInvoiceLineToPost.runID.
    ELSE DO:
      FIND FIRST ttInvoiceMiscToPost NO-LOCK NO-ERROR.
        IF AVAIL ttInvoiceMiscToPost THEN
        v-trnum = ttInvoiceMiscToPost.runID.      
    END.
    
    ASSIGN
    str-tit2 = TRIM(c-win:TITLE) + " - " +
               TRIM(STRING(tb_detailed,"Detail/Summary")) +
               " - RUN# " + TRIM(STRING(v-trnum))
    {sys/inc/ctrtext.i str-tit2 112}

    str-tit3 = "Period " + string(tran-period,"99") + " - " +
               IF AVAILABLE period THEN
               (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
    {sys/inc/ctrtext.i str-tit3 132}

    str-tit4   = "Post Date " + STRING(tran-date, "99/99/99").
        
    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    lv-list-name = list-name.

    DISPLAY WITH FRAME r-top.

    RUN list-post-inv ("list").

    OUTPUT CLOSE. 
        
    IF v-postable THEN RUN list-gl.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    EMPTY TEMP-TABLE tt-report.

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
    DEFINE VARIABLE lv-label      AS cha     NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-save-line C-Win 
PROCEDURE undo-save-line :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    
    FIND FIRST bf-inv-line 
        WHERE ROWID(bf-inv-line) EQ ttSaveLine.invRowID 
        NO-ERROR.
  
    IF AVAILABLE bf-inv-line THEN 
        bf-inv-line.r-no = ttSaveLine.invLineRNo.
    ELSE
        FIND FIRST bf-inv-misc 
            WHERE ROWID(bf-inv-misc) EQ ttSaveLine.invRowID 
            NO-ERROR.

    IF AVAILABLE bf-inv-misc THEN 
        bf-inv-misc.r-no = ttSaveLine.invMiscRNo.
    DELETE ttSaveLine.
    
    RELEASE bf-inv-line.
    RELEASE bf-inv-misc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll AS LOG NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT ll-warned THEN 
        DO:
            ll = NO.

            FOR EACH period NO-LOCK
                WHERE period.company EQ cocode
                AND period.pst     LE TODAY
                AND period.pend    GE TODAY
                BY period.pst:

                IF period.pst  GT DATE(tran-date:SCREEN-VALUE) OR
                    period.pend LT DATE(tran-date:SCREEN-VALUE) THEN 
                DO:
                    ll = YES.
                    MESSAGE TRIM(tran-date:LABEL) + " is not in current period, " +
                        "would you like to re-enter..."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
                END.

                IF ll THEN 
                DO:
                    APPLY "entry" TO tran-date.
                    RETURN ERROR.
                END.

                LEAVE.
            END.

            ll-warned = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

