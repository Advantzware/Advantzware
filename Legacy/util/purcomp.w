&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
DEF VAR lcTableList AS CHAR NO-UNDO.
DEF VAR hStatus AS HANDLE NO-UNDO.
DEF VAR hDelProcedure AS HANDLE NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_company fiBackupLocation ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_company fiBackupLocation 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiBackupLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "Backup Location" 
     VIEW-AS FILL-IN 
     SIZE 63.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(4)" 
     LABEL "Company To Remove" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_company AT ROW 7.67 COL 22 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     fiBackupLocation AT ROW 9.81 COL 22 COLON-ALIGNED WIDGET-ID 2
     btn-process AT ROW 14.1 COL 21
     btn-cancel AT ROW 14.1 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95 BY 15.33.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94 BY 3.81
         BGCOLOR 11 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Company"
         HEIGHT             = 15.33
         WIDTH              = 95
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       fi_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Company */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Company */
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
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to delete this company? "
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBackupLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBackupLocation C-Win
ON LEAVE OF fiBackupLocation IN FRAME FRAME-A /* Backup Location */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON LEAVE OF fi_company IN FRAME FRAME-A /* Company To Remove */
DO:
   assign {&self-name}.
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

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll": 
DEFINE INPUT PARAMETER hWndLock AS LONG NO-UNDO. 
END PROCEDURE. /* LockWindowUpdate */ 

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
SUBSCRIBE "NUMDEL" ANYWHERE.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.
  {methods/nowait.i}
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
  DISPLAY fi_company fiBackupLocation 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_company fiBackupLocation btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numdel C-Win 
PROCEDURE numdel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiCnt AS INT NO-UNDO.
Run LockWindowUpdate(input CURRENT-WINDOW:HWND). 
IF VALID-HANDLE(hStatus) THEN
  RUN process-message IN hStatus (INPUT ipcTable + ": " + STRING(ipiCnt)).
Run LockWindowUpdate(input 0). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/arinvdel.p 03/98 JLF */
/* Purge Paid AR Invoices                                                     */
/* -------------------------------------------------------------------------- */




DEF VAR i AS INT NO-UNDO.


session:set-wait-state("General").

RUN windows/w-message.w PERSISTENT SET hStatus.
RUN setWindowTitle IN hStatus (INPUT "Company Purge").

RUN util/del-comp.p PERSISTENT SET hDelProcedure.

/* Build table list in lcTableList */
RUN setTableList.
PAUSE BEFORE-HIDE.
DO i = 1 TO NUM-ENTRIES(lcTableList):
    RUN delete-table IN hDelProcedure
        (INPUT fi_company,
         INPUT ENTRY(i, lcTableList),
         INPUT fiBackupLocation).
    PAUSE 3.
END.
PAUSE 0 BEFORE-HIDE.

session:set-wait-state("").

IF VALID-HANDLE(hStatus) THEN
    DELETE OBJECT hStatus.
IF VALID-HANDLE(hDelProcedure) THEN
    DELETE OBJECT hDelProcedure.

message trim(c-win:title) + " Process Is Completed." view-as alert-box.
apply "close" to this-procedure.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTableList C-Win 
PROCEDURE setTableList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       check manually: edi tables, reftable, gl-jrnl, links, mch-srt,
                               mf tables, notes, phone, jobseq
------------------------------------------------------------------------------*/
lcTableList = 
"ar-ctrl," +
"acctcost," +
"ap-buy," +
"ap-chk," +
"ap-ctrl," +
"ap-dis," +
"ap-disl," +
"ap-inv," +
"ap-invl," +
"ap-invlr," +
"ap-ledger," +
"ap-payl," +
"ap-pay," +
"ap-sel," +
"aphist," +
"ar-invm," +
"ar-cash," +
"ar-cashl," +
"ar-inv," +
"ar-invl," +
"ar-ledger," +
"ar-mcash," +
"asinotes," +
"attach," +
"bank," +
"box-design-line," +
"box-design-hdr," +
"buyer," +
"company," +
"carr-mtx," +
"ce-ctrl," +
"carrier," +
"credit-hold-type," +
"crew," +
"currency," +
"cust-markup," +
"cust-part," +
"cust-prod-sales," +
"custype," +
"cust," +
"db-ctrl," +
"dept," +
"down-type," +
"e-item," +
"e-item-cust," +
"e-item-vend," +
"e-itemfg," +
"e-itemfg-vend," +
"eb," +
"ed," +
"EDAPCheck," +
"EDCo," +
"EDICXref," +
"EDIVAddon," +
"EDIVLine," +
"EDIVTran," +
"ef," +
"ef-nsh," +
"emp," +
"er," +
"est," +
"est-qty," +
"est-flm," +
"est-inst," +
"est-op," + 
"est-prep," +
"est-summ," +
"est-pf," +
"itemfg-bom," +
"fg-act," +
"fg-bin," +
"fg-ctrl," +
"fg-hist," +
"fg-rcpth," +
"fg-rcpts," +
"fg-rctd," +
"fg-rdtl," +
"fg-rdtlh," +
"fg-set," +
"fgcat," +
"flute," +
"account," +
"gl-rptd," +
"gl-ctrl," +
"gl-freq," +
"gl-jrn," +
"gl-rpt," +
"glhist," +
"gltrans," +
"inv-head," +
"inv-line," +
"inv-misc," +
"item-bom," +
"item-comm," +
"item-spec," +
"itemfg," +
"itemfg-ink," +
"itemfg-loc," +
"itemfgdtl," +
"jc-ctrl," +
"job," +
"job-sch," +
"job-all," +
"job-brd," +
"job-hdr," +
"job-mat," +
"job-mch," +
"job-prep," +
"job-set," +
"loadtag," +
"mach," +
"mach-adder," +
"mach-attach-pat," +
"mach-calendar," +
"mach-panel," +
"mach-part," +
"mach-attach," +
"mmtx," +
"mat-act," +
"costtype," +
"mat," +
"mch-act," +
"misc-act," +
"mmtx2," +
"mmty," +
"mstd," +
"oe-bolh," +
"oe-boll," +
"oe-boll-qty," +
"oe-ctrl," +
"oe-ord," +
"oe-ordl," +
"oe-ordm," +
"oe-prmtx," +
"oe-rel," +
"oe-relh," +
"oe-rell," +
"oe-reth," +
"oe-retl," +
"oe-ship," +
"pc-misc," +
"pc-prdd," +
"pc-prdd-wip," +
"pc-prdh," +
"period," +
"permg," +
"loc," +
"po-all," +
"po-ctrl," +
"po-ord," +
"po-ordl," +
"po-ordl-add," +
"po-rcpts," +
"prep," +
"matprep," +
"printer," +
"probe," +
"probeit," +
"probeit-price," +
"procat," +
"prodl," +
"prod," +
"quote," +
"quotehd," +
"quotechg," +
"quoteitm," +
"quoteqty," +
"quoteit," +
"item," +
"reftable," +
"rfidtag," +
"rm-bin," +
"rm-ctrl," +
"rm-rcpt," +
"rm-rcpth," +
"rm-rctd," +
"rm-rcth," +
"rm-rdtl," +
"rm-rdtlh," +
"rm-receipts," +
"routing," +
"routing-mtx," +
"sman," +
"smanbugt," +
"smanbcat," +
"smanbcst," +
"smanmtrx," +
"sman-mtx," +
"scores," +
"shift," +
"shipto," +
"soldto," +
"ssrelbol," +
"stack-flute," +
"stack-size," +
"std-code," +
"style," +
"style-score," +
"surcharge," +
"sys-ctrl," +
"sys-ctrl-shipto," +
"stax-group," +
"stax," +
"terms," +
"terr," +
"test-red," +
"truck," +
"truck-run-print," +
"usercomp," +
"user-batch," +
"user-print," +
"usr," +
"usr-grp," +
"usrx," +
"vend," +
"ventype," +
"waste-type," +
"wip-bin," +
"wiptag," +
"wiptag-mch," +
"usercust," +
"usersman," +
"uservend," +
"asi," +
"cmpltjob," +
"contact," +
"empmach," +
"emplogin," +
"rate," +
"employee," +
"machchrg," +
"machshft," +
"machseq," +
"machtran," +
"shifts," +
"shift_break," +
"vend-code-cust-xref," +
"vend-plant," +
"vend-whse-item," +
"vend-whse-trans," +
"vend-whse-trans-hist" +
"bolh" +
"boll" +
"cust-itm" +
"rfq" +
"rfq-ctrl" +
"rfqitem" .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

