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

DEFINE VARIABLE v-process AS LOG NO-UNDO.
DEF VAR cDumpLoc AS CHAR NO-UNDO.

DEF STREAM soe-ord.
DEF STREAM soe-rel.
DEF STREAM soe-rell.
DEF STREAM soe-relh.
DEF STREAM soe-boll.
DEF STREAM soe-bolh.
DEF STREAM sinv-line.
DEF STREAM sinv-misc.
DEF STREAM sinv-head.
DEF STREAM soe-ordl.
DEF STREAM sjob.
DEF STREAM sjob-hdr.
DEF STREAM sjob-mat.
DEF STREAM sjob-mch.
DEF STREAM sjob-prep.
DEF STREAM sjob-farm.
DEF STREAM sjobfarmrctd.
DEF STREAM spc-prdd.
DEF STREAM sfg-act.
DEF STREAM smat-act.
DEF STREAM smch-act.
DEF STREAM smisc-act.
DEF STREAM sfg-bin.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 purge_date begin_order end_order ~
tbInvoices tbArchive btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS purge_date begin_order end_order ~
tbInvoices tbArchive fiDumpLoc 

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

DEFINE VARIABLE begin_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiDumpLoc AS CHARACTER FORMAT "X(256)":U INITIAL "(Records to recover purged information will be stored in C:~\tmp~\OrderPurgeYYMMDD)" 
      VIEW-AS TEXT 
     SIZE 84 BY .62 NO-UNDO.

DEFINE VARIABLE purge_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Purge Orders Prior To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 8.57.

DEFINE VARIABLE tbArchive AS LOGICAL INITIAL no 
     LABEL "Create Recovery Records?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbInvoices AS LOGICAL INITIAL no 
     LABEL "Purge Related Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     purge_date AT ROW 6.71 COL 27 COLON-ALIGNED
     begin_order AT ROW 8.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_order AT ROW 8.14 COL 63 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     tbInvoices AT ROW 9.57 COL 29
     tbArchive AT ROW 10.76 COL 29
     btn-process AT ROW 15.29 COL 21
     btn-cancel AT ROW 15.29 COL 53
     fiDumpLoc AT ROW 11.95 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     "Selection Parameters:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
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
         TITLE              = "Purge Orders"
         HEIGHT             = 17.71
         WIDTH              = 90.2
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

/* SETTINGS FOR FILL-IN fiDumpLoc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiDumpLoc:HIDDEN IN FRAME FRAME-A           = TRUE
       fiDumpLoc:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Orders */
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
ON WINDOW-CLOSE OF C-Win /* Purge Orders */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbArchive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbArchive C-Win
ON VALUE-CHANGED OF tbArchive IN FRAME FRAME-A /* Create Recovery Records? */
DO:
        ASSIGN 
            fiDumpLoc:VISIBLE = SELF:CHECKED.
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
    /* check security */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.

    purge_date = TODAY.

    RUN enable_UI.
    
    APPLY 'value-changed' TO tbArchive.

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
  DISPLAY purge_date begin_order end_order tbInvoices tbArchive fiDumpLoc 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 purge_date begin_order end_order tbInvoices tbArchive 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    DEFINE VARIABLE v-post-date AS DATE INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-first-ord LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE v-last-ord LIKE oe-ord.ord-no NO-UNDO.
    
/*  DISABLE TRIGGERS FOR LOAD OF oe-ord.                                                                      */
/*  Leaving triggers to fire for oe-ord so that audit is written                                              */
/*  Since ALL other records should have been deleted prior to order deletion, the triggers should not cascade */

    DISABLE TRIGGERS FOR LOAD OF oe-rel.
    DISABLE TRIGGERS FOR LOAD OF oe-rell.
    DISABLE TRIGGERS FOR LOAD OF oe-relh.
    DISABLE TRIGGERS FOR LOAD OF oe-boll.
    DISABLE TRIGGERS FOR LOAD OF oe-bolh.
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.
    DISABLE TRIGGERS FOR LOAD OF inv-head.
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF job.
    DISABLE TRIGGERS FOR LOAD OF job-hdr.
    DISABLE TRIGGERS FOR LOAD OF job-mat.
    DISABLE TRIGGERS FOR LOAD OF job-mch.
    DISABLE TRIGGERS FOR LOAD OF job-prep.
    DISABLE TRIGGERS FOR LOAD OF job-farm.
    DISABLE TRIGGERS FOR LOAD OF job-farm-rctd.
    DISABLE TRIGGERS FOR LOAD OF pc-prdd.
    DISABLE TRIGGERS FOR LOAD OF fg-act.
    DISABLE TRIGGERS FOR LOAD OF mat-act.
    DISABLE TRIGGERS FOR LOAD OF mch-act.
    DISABLE TRIGGERS FOR LOAD OF misc-act.
    DISABLE TRIGGERS FOR LOAD OF fg-bin.

    DEFINE BUFFER b-boll FOR oe-boll.
    DEFINE BUFFER b-rell FOR oe-rell.

    DO WITH FRAME {&frame-name}:
        ASSIGN
            purge_date
            begin_order
            end_order
            tbInvoices
            tbArchive.
    END.
    
    ASSIGN 
        cDumpLoc = "C:\tmp\OrderPurge-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99" + "-" + STRING(TIME,"99999")). 
    
    IF tbArchive THEN DO:
        OS-CREATE-DIR VALUE(cDumpLoc).
        OUTPUT STREAM soe-ord       TO VALUE (cDumpLoc + "/oe-ord.d") APPEND.    
        OUTPUT STREAM soe-rel       TO VALUE (cDumpLoc + "/oe-rel.d") APPEND.    
        OUTPUT STREAM soe-rell      TO VALUE (cDumpLoc + "/oe-rell.d") APPEND.   
        OUTPUT STREAM soe-relh      TO VALUE (cDumpLoc + "/oe-relh.d") APPEND.   
        OUTPUT STREAM soe-boll      TO VALUE (cDumpLoc + "/oe-boll.d") APPEND.   
        OUTPUT STREAM soe-bolh      TO VALUE (cDumpLoc + "/oe-bolh.d") APPEND.   
        OUTPUT STREAM sinv-line     TO VALUE (cDumpLoc + "/inv-line.d") APPEND.  
        OUTPUT STREAM sinv-misc     TO VALUE (cDumpLoc + "/inv-misc.d") APPEND.  
        OUTPUT STREAM sinv-head     TO VALUE (cDumpLoc + "/inv-head.d") APPEND.  
        OUTPUT STREAM soe-ordl      TO VALUE (cDumpLoc + "/oe-ordl.d") APPEND.   
        OUTPUT STREAM sjob          TO VALUE (cDumpLoc + "/job.d") APPEND.       
        OUTPUT STREAM sjob-hdr      TO VALUE (cDumpLoc + "/job-hdr.d") APPEND.   
        OUTPUT STREAM sjob-mat      TO VALUE (cDumpLoc + "/job-mat.d") APPEND.   
        OUTPUT STREAM sjob-mch      TO VALUE (cDumpLoc + "/job-mch.d") APPEND.   
        OUTPUT STREAM sjob-prep     TO VALUE (cDumpLoc + "/job-prep.d") APPEND.  
        OUTPUT STREAM sjob-farm     TO VALUE (cDumpLoc + "/job-farm.d") APPEND.  
        OUTPUT STREAM sjobfarmrctd  TO VALUE (cDumpLoc + "/job-farm-rctd.d") APPEND.           
        OUTPUT STREAM spc-prdd      TO VALUE (cDumpLoc + "/pc-prdd.d") APPEND.   
        OUTPUT STREAM sfg-act       TO VALUE (cDumpLoc + "/fg-act.d") APPEND.    
        OUTPUT STREAM smat-act      TO VALUE (cDumpLoc + "/mat-act.d") APPEND.   
        OUTPUT STREAM smch-act      TO VALUE (cDumpLoc + "/mch-act.d") APPEND.   
        OUTPUT STREAM smisc-act     TO VALUE (cDumpLoc + "/misc-act.d") APPEND.  
        OUTPUT STREAM sfg-bin       TO VALUE (cDumpLoc + "/fg-bin.d") APPEND.
    END.    

    ASSIGN
        v-post-date = purge_date
        v-first-ord = begin_order
        v-last-ord  = end_order
        v-process   = NO.

    MESSAGE 
        "Are you sure you want to delete the orders within the selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    IF v-process THEN DO:
        FOR EACH oe-ord NO-LOCK 
            WHERE oe-ord.company  EQ cocode
            AND oe-ord.ord-date LT v-post-date
            AND oe-ord.ord-no   GE v-first-ord
            AND oe-ord.ord-no   LE v-last-ord
            TRANSACTION:

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...".
            
            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release records".
            FOR EACH oe-rel EXCLUSIVE WHERE 
                oe-rel.company EQ cocode AND 
                oe-rel.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN EXPORT STREAM soe-rel      oe-rel.
                DELETE oe-rel.
            END. /* oe-rel */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release line records".
            FOR EACH oe-rell EXCLUSIVE WHERE 
                oe-rell.company EQ oe-ord.company AND 
                oe-rell.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN EXPORT STREAM soe-rell     oe-rell.
                DELETE oe-rell.
            END. /* oe-rell */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release header records".
            FIND FIRST oe-relh EXCLUSIVE WHERE 
                oe-relh.company EQ oe-ord.company AND 
                oe-relh.ord-no EQ oe-ord.ord-no 
                NO-ERROR.
            IF AVAIL oe-relh 
            AND NOT CAN-FIND(FIRST b-rell WHERE 
                            b-rell.r-no EQ oe-relh.r-no AND 
                            ROWID(b-rell) NE ROWID(oe-relh)) THEN DO:
                IF tbArchive THEN EXPORT STREAM soe-relh     oe-relh.
                DELETE oe-relh.
            END.
            
            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing BOL line records".
            FOR EACH oe-boll EXCLUSIVE WHERE 
                oe-boll.company EQ oe-ord.company AND 
                oe-boll.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN EXPORT STREAM soe-boll     oe-boll.
                DELETE oe-boll.
            END. /* oe-boll */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing BOL Header records".
            FIND FIRST oe-bolh EXCLUSIVE WHERE 
                oe-bolh.company EQ oe-ord.company AND 
                oe-bolh.b-no EQ oe-ord.ord-no
                NO-ERROR.

            IF AVAIL oe-bolh 
            AND tbInvoices:CHECKED THEN FOR EACH inv-head EXCLUSIVE WHERE 
                inv-head.company EQ cocode AND  
                inv-head.bol-no EQ oe-bolh.bol-no:

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Invoice Line records".
                FOR EACH inv-line EXCLUSIVE WHERE 
                    inv-line.r-no    EQ inv-head.r-no AND 
                    inv-line.company EQ inv-head.company:
                    IF tbArchive THEN EXPORT STREAM sinv-line    inv-line.
                    DELETE inv-line.
                END. /* inv-line */

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Invoice Misc records".
                FOR EACH inv-misc EXCLUSIVE WHERE 
                    inv-misc.r-no    EQ inv-head.r-no AND 
                    inv-misc.company EQ inv-head.company:
                    IF tbArchive THEN EXPORT STREAM sinv-misc    inv-misc.
                    DELETE inv-misc.
                END. /* inv-misc */

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Invoice Headers records".
                IF tbArchive THEN EXPORT STREAM sinv-head    inv-head.
                DELETE inv-head.
            END. /* inv-head */

            IF AVAIL oe-bolh
            AND NOT CAN-FIND(FIRST b-boll WHERE 
                            b-boll.b-no EQ oe-bolh.b-no AND 
                            ROWID(b-boll) NE ROWID(oe-bolh)) THEN DO:
                IF tbArchive THEN EXPORT STREAM soe-bolh     oe-bolh.
                DELETE oe-bolh.
            END.


            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Order line records".
            FOR EACH oe-ordl NO-LOCK WHERE 
                oe-ordl.company EQ cocode AND 
                oe-ordl.ord-no  EQ oe-ord.ord-no:

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job records".
                FOR EACH job EXCLUSIVE WHERE 
                    job.company EQ cocode AND 
                    job.job-no  EQ oe-ordl.job-no AND 
                    job.job-no2 EQ oe-ordl.job-no2:

                    RUN jc/jc-dall.p (RECID(job)).

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job header records".
                    FOR EACH job-hdr EXCLUSIVE WHERE 
                        job-hdr.company EQ cocode AND 
                        job-hdr.job     EQ job.job AND 
                        job-hdr.job-no  EQ job.job-no AND 
                        job-hdr.job-no2 EQ job.job-no2:

                        {util/dljobkey.i}

                        IF tbArchive THEN EXPORT STREAM sjob-hdr     job-hdr.
                        DELETE job-hdr.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job material records".
                    FOR EACH job-mat EXCLUSIVE WHERE 
                        job-mat.company EQ job.company AND 
                        job-mat.job     EQ job.job AND 
                        job-mat.job-no  EQ job.job-no AND 
                        job-mat.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sjob-mat     job-mat.
                        DELETE job-mat.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job machine records".
                    FOR EACH job-mch EXCLUSIVE WHERE 
                        job-mch.company EQ job.company AND 
                        job-mch.job     EQ job.job AND 
                        job-mch.job-no  EQ job.job-no AND 
                        job-mch.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sjob-mch     job-mch.
                        DELETE job-mch.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job prep records".
                    FOR EACH job-prep EXCLUSIVE WHERE 
                        job-prep.company EQ job.company AND 
                        job-prep.job     EQ job.job AND 
                        job-prep.job-no  EQ job.job-no AND 
                        job-prep.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sjob-prep    job-prep.
                        DELETE job-prep.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job farm records".
                    FOR EACH job-farm EXCLUSIVE WHERE 
                        job-farm.company EQ job.company AND 
                        job-farm.job-no  EQ job.job-no AND 
                        job-farm.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sjob-farm    job-farm.
                        DELETE job-farm.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job farm receipts records".
                    FOR EACH job-farm-rctd EXCLUSIVE WHERE 
                        job-farm-rctd.company EQ job.company AND 
                        job-farm-rctd.job-no  EQ job.job-no AND 
                        job-farm-rctd.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sjobfarmrctd job-farm-rctd.
                        DELETE job-farm-rctd.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing ProdCtrl detail records".
                    FOR EACH pc-prdd EXCLUSIVE WHERE 
                        pc-prdd.company EQ cocode AND 
                        pc-prdd.job     EQ job.job AND 
                        pc-prdd.job-no  EQ job.job-no AND 
                        pc-prdd.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM spc-prdd     pc-prdd.
                        DELETE pc-prdd.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing FG activity records".
                    FOR EACH fg-act EXCLUSIVE WHERE 
                        fg-act.company EQ cocode AND 
                        fg-act.job     EQ job.job AND 
                        fg-act.job-no  EQ job.job-no AND 
                        fg-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM sfg-act      fg-act.
                        DELETE fg-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing RM activity records".
                    FOR EACH mat-act EXCLUSIVE WHERE 
                        mat-act.company EQ cocode AND 
                        mat-act.job     EQ job.job AND 
                        mat-act.job-no  EQ job.job-no AND 
                        mat-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM smat-act     mat-act.
                        DELETE mat-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Machine activity records".
                    FOR EACH mch-act EXCLUSIVE WHERE 
                        mch-act.company EQ cocode AND 
                        mch-act.job     EQ job.job AND 
                        mch-act.job-no  EQ job.job-no AND 
                        mch-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM smch-act     mch-act.
                        DELETE mch-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Misc activity records".
                    FOR EACH misc-act EXCLUSIVE WHERE 
                        misc-act.company EQ cocode AND 
                        misc-act.job     EQ job.job AND 
                        misc-act.job-no  EQ job.job-no AND 
                        misc-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN EXPORT STREAM smisc-act    misc-act.
                        DELETE misc-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing FG Bin (empty) records".
                    FOR EACH fg-bin EXCLUSIVE WHERE 
                        fg-bin.company    EQ cocode AND 
                        fg-bin.job-no     EQ job.job-no AND 
                        fg-bin.job-no2    EQ job.job-no2 AND 
                        fg-bin.i-no       NE "" AND 
                        fg-bin.qty        EQ 0:
                        /* Moved here so indexing ok on for each */
                        IF TRIM(fg-bin.i-no) EQ "" THEN NEXT.
                        IF tbArchive THEN EXPORT STREAM sfg-bin      fg-bin.
                        DELETE fg-bin.
                    END.

                    IF job.exported THEN DO:
                        job.stat = "X".
                        RUN jc/kiwiexp2.p (RECID(job)).
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting job".
                    IF tbArchive THEN EXPORT STREAM sjob         job.
                    DELETE job.
                END.

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting order line".
                FIND CURRENT oe-ordl EXCLUSIVE.
                IF tbArchive THEN EXPORT STREAM soe-ordl     oe-ordl.
                DELETE oe-ordl.
            END.

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting order header".
            FIND CURRENT oe-ord EXCLUSIVE.
            IF tbArchive THEN EXPORT STREAM soe-ord      oe-ord.
            DELETE oe-ord.
        END. /* oe-ord */

        IF tbArchive THEN DO:
            OUTPUT STREAM soe-ord       CLOSE. 
            OUTPUT STREAM soe-rel       CLOSE. 
            OUTPUT STREAM soe-rell      CLOSE. 
            OUTPUT STREAM soe-relh      CLOSE. 
            OUTPUT STREAM soe-boll      CLOSE. 
            OUTPUT STREAM soe-bolh      CLOSE. 
            OUTPUT STREAM sinv-line     CLOSE. 
            OUTPUT STREAM sinv-misc     CLOSE. 
            OUTPUT STREAM sinv-head     CLOSE. 
            OUTPUT STREAM soe-ordl      CLOSE. 
            OUTPUT STREAM sjob          CLOSE. 
            OUTPUT STREAM sjob-hdr      CLOSE. 
            OUTPUT STREAM sjob-mat      CLOSE. 
            OUTPUT STREAM sjob-mch      CLOSE. 
            OUTPUT STREAM sjob-prep     CLOSE. 
            OUTPUT STREAM sjob-farm     CLOSE. 
            OUTPUT STREAM sjobfarmrctd  CLOSE.         
            OUTPUT STREAM spc-prdd      CLOSE. 
            OUTPUT STREAM sfg-act       CLOSE. 
            OUTPUT STREAM smat-act      CLOSE. 
            OUTPUT STREAM smch-act      CLOSE. 
            OUTPUT STREAM smisc-act     CLOSE. 
            OUTPUT STREAM sfg-bin       CLOSE.
        END.
        
        MESSAGE 
            TRIM(c-win:TITLE) + " Process Is Completed." 
            VIEW-AS ALERT-BOX.
        
        APPLY "close" TO THIS-PROCEDURE.
    
    END.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

