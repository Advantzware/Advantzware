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

DEF VAR lProcess AS LOG NO-UNDO.
DEF VAR lSimulate AS LOG NO-UNDO.
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
DEF STREAM soe-ordm.
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
&Scoped-Define ENABLED-OBJECTS purge_date begin_cust end_cust begin_order ~
end_order rsClosed tbInvoices tbArchive btn-Simulate btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS purge_date begin_cust end_cust begin_order ~
end_order rsClosed tbInvoices tbArchive fiDumpLoc fiDumpLoc2 

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
     LABEL "&Start Purge" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-Simulate 
     LABEL "Simulate Purge" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning CustNo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Ending CustNo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiDumpLoc AS CHARACTER FORMAT "X(256)":U INITIAL "Records to view or recover purged information will be stored in directory:" 
      VIEW-AS TEXT 
     SIZE 74 BY .62 NO-UNDO.

DEFINE VARIABLE fiDumpLoc2 AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\tmp~\OrderPurge-YYMMDD-99999~\" 
      VIEW-AS TEXT 
     SIZE 43 BY .62 NO-UNDO.

DEFINE VARIABLE purge_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Purge Orders Prior To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rsClosed AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purge Closed Orders Only", "C",
"ALL Orders", "A"
     SIZE 46 BY .95 NO-UNDO.

DEFINE VARIABLE tbArchive AS LOGICAL INITIAL yes 
     LABEL "Create Recovery Records?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbInvoices AS LOGICAL INITIAL no 
     LABEL "Purge Related Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     purge_date AT ROW 6.24 COL 27 COLON-ALIGNED
     begin_cust AT ROW 7.91 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_cust AT ROW 7.91 COL 63 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     begin_order AT ROW 9.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_order AT ROW 9.33 COL 63 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     rsClosed AT ROW 10.76 COL 29 NO-LABEL
     tbInvoices AT ROW 11.95 COL 29
     tbArchive AT ROW 13.14 COL 29
     btn-Simulate AT ROW 17.67 COL 7
     btn-process AT ROW 17.67 COL 34
     btn-cancel AT ROW 17.67 COL 63
     fiDumpLoc AT ROW 14.57 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiDumpLoc2 AT ROW 15.29 COL 4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     "Selection Parameters:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 19.43.

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
         HEIGHT             = 19.43
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

/* SETTINGS FOR FILL-IN fiDumpLoc2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiDumpLoc2:HIDDEN IN FRAME FRAME-A           = TRUE
       fiDumpLoc2:READ-ONLY IN FRAME FRAME-A        = TRUE.

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
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Purge */
DO:
        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Simulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Simulate C-Win
ON CHOOSE OF btn-Simulate IN FRAME FRAME-A /* Simulate Purge */
DO:
        ASSIGN
            lSimulate = TRUE  
            tbArchive:CHECKED = TRUE.
        APPLY 'value-changed' TO tbArchive IN FRAME {&frame-name}.
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

    ASSIGN 
        purge_date = TODAY
        cDumpLoc = "C:\tmp\OrderPurge-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999"). 

    RUN enable_UI.
    
    ASSIGN 
        fiDumpLoc2:SCREEN-VALUE = cDumpLoc.
    
    APPLY 'value-changed' TO tbArchive.

    {methods/nowait.i}
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildHeaders C-Win 
PROCEDURE buildHeaders :
DEF VAR cTableList AS CHAR NO-UNDO.
    DEF VAR cTableName AS CHAR NO-UNDO.
    DEF VAR cLabelList AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR lv-count AS INT NO-UNDO.
    
    ASSIGN 
        cTableList = "oe-ord,oe-rel,oe-rell,oe-relh,oe-boll,oe-bolh,inv-line,inv-misc,inv-head,oe-ordl,job,job-hdr,job-mat," + 
                     "job-mch,job-prep,job-farm,job-farm-rctd,pc-prdd,fg-act,mat-act,mch-act,misc-act,fg-bin,oe-ordm".
        
    DO iCtr = 1 TO NUM-ENTRIES(cTableList):
        ASSIGN 
            cTableName = ENTRY(iCtr,cTableList)
            cLabelList = "".
        FIND FIRST _file NO-LOCK WHERE  
            _file._file-name EQ cTableName
            NO-ERROR.
        FOR EACH _field OF _file NO-LOCK BY _field._Order:
            ASSIGN 
                lv-count = 1.
            IF _field._extent = 0 THEN
                ASSIGN cLabelList = cLabelList + "," + (IF _field._label NE "" AND _field._label NE ? THEN _field._label ELSE _field._field-name).
            ELSE 
                ASSIGN cLabelList = cLabelList + "," + (IF _field._label NE "" AND _field._label NE ? THEN _field._label ELSE _field._field-name) + "[1]".
            DO WHILE lv-count < _field._extent:
                ASSIGN 
                    lv-count    = lv-count + 1
                    cLabelList = cLabelList + "," + (IF _field._label NE "" AND _field._label NE ? THEN _field._label ELSE _field._field-name) + "[" + STRING(lv-count) + "]".
            END.
        END.        
        ASSIGN 
            cLabelList = TRIM(cLabelList,",").
        
        CASE cTableName:
            WHEN "oe-ord" THEN 
                PUT STREAM soe-ord UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-rel" THEN 
                PUT STREAM soe-rel UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-rell" THEN 
                PUT STREAM soe-rell UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-relh" THEN 
                PUT STREAM soe-relh UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-boll" THEN 
                PUT STREAM soe-boll UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-bolh" THEN 
                PUT STREAM soe-bolh UNFORMATTED cLabelList + CHR(10).
            WHEN "inv-line" THEN 
                PUT STREAM sinv-line UNFORMATTED cLabelList + CHR(10).
            WHEN "inv-misc" THEN 
                PUT STREAM sinv-misc UNFORMATTED cLabelList + CHR(10).
            WHEN "inv-head" THEN 
                PUT STREAM sinv-head UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-ordl" THEN 
                PUT STREAM soe-ordl UNFORMATTED cLabelList + CHR(10).
            WHEN "job" THEN 
                PUT STREAM sjob UNFORMATTED cLabelList + CHR(10).
            WHEN "job-hdr" THEN 
                PUT STREAM sjob-hdr UNFORMATTED cLabelList + CHR(10).
            WHEN "job-mat" THEN 
                PUT STREAM sjob-mat UNFORMATTED cLabelList + CHR(10).
            WHEN "job-mch" THEN 
                PUT STREAM sjob-mch UNFORMATTED cLabelList + CHR(10).
            WHEN "job-prep" THEN 
                PUT STREAM sjob-prep UNFORMATTED cLabelList + CHR(10).
            WHEN "job-farm" THEN 
                PUT STREAM sjob-farm UNFORMATTED cLabelList + CHR(10).
            WHEN "job-farm-rctd" THEN 
                PUT STREAM sjobfarmrctd UNFORMATTED cLabelList + CHR(10).
            WHEN "pc-prdd" THEN 
                PUT STREAM spc-prdd UNFORMATTED cLabelList + CHR(10).
            WHEN "fg-act" THEN 
                PUT STREAM sfg-act UNFORMATTED cLabelList + CHR(10).
            WHEN "mat-act" THEN 
                PUT STREAM smat-act UNFORMATTED cLabelList + CHR(10).
            WHEN "mch-act" THEN 
                PUT STREAM smch-act UNFORMATTED cLabelList + CHR(10).
            WHEN "misc-act" THEN 
                PUT STREAM smisc-act UNFORMATTED cLabelList + CHR(10).
            WHEN "fg-bin" THEN 
                PUT STREAM sfg-bin UNFORMATTED cLabelList + CHR(10).
            WHEN "oe-ordm" THEN 
                PUT STREAM soe-ordm UNFORMATTED cLabelList + CHR(10).
        END CASE.            
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
  DISPLAY purge_date begin_cust end_cust begin_order end_order rsClosed 
          tbInvoices tbArchive fiDumpLoc fiDumpLoc2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE purge_date begin_cust end_cust begin_order end_order rsClosed 
         tbInvoices tbArchive btn-Simulate btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteOrphanRecords C-Win
PROCEDURE pDeleteOrphanRecords PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Delete orphan oe-boll or oe-bolh records (Either a company is blank
        or no header/parent Record)   
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyPrefix   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplDeleteInvoices AS LOGICAL   NO-UNDO.    
    
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    
    FOR EACH bf-oe-boll NO-LOCK 
        WHERE (bf-oe-boll.company EQ ipcCompany OR bf-oe-boll.company EQ "") 
          AND (IF bf-oe-boll.company EQ "" THEN YES ELSE (bf-oe-boll.b-no EQ 0 OR bf-oe-boll.b-no EQ ?)):
        IF DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-oe-boll.rec_key) LT ipcRecKeyPrefix THEN DO:   
            IF tbArchive THEN DO:
                IF lProcess THEN 
                    EXPORT STREAM soe-boll bf-oe-boll.
                ELSE IF lSimulate 
                    THEN EXPORT STREAM soe-boll DELIMITER "," bf-oe-boll.
            END.
            IF lProcess THEN DO: 
                FIND CURRENT bf-oe-boll EXCLUSIVE-LOCK NO-ERROR.
                DELETE bf-oe-boll.
            END.
        END.                           
    END.
    
    FOR EACH bf-oe-bolh NO-LOCK
        WHERE bf-oe-bolh.company EQ ipcCompany 
          AND NOT CAN-FIND(FIRST bf-oe-boll
                           WHERE bf-oe-boll.company EQ bf-oe-bolh.company
                             AND bf-oe-boll.b-no    EQ bf-oe-bolh.b-no):
        IF DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-oe-bolh.rec_key) LT ipcRecKeyPrefix THEN DO:
            IF iplDeleteInvoices THEN DO:
                FOR EACH bf-inv-head NO-LOCK  
                    WHERE bf-inv-head.company EQ bf-oe-bolh.company 
                      AND bf-inv-head.bol-no  EQ bf-oe-bolh.bol-no:
    
                    FOR EACH bf-inv-line NO-LOCK 
                        WHERE bf-inv-line.r-no    EQ bf-inv-head.r-no 
                          AND bf-inv-line.company EQ bf-inv-head.company:
                        IF tbArchive THEN DO:
                            IF lProcess THEN 
                                EXPORT STREAM sinv-line bf-inv-line.
                            ELSE IF lSimulate THEN 
                                EXPORT STREAM sinv-line DELIMITER "," bf-inv-line.
                        END.
                        IF lProcess THEN DO: 
                            FIND CURRENT bf-inv-line EXCLUSIVE-LOCK NO-ERROR.
                            DELETE bf-inv-line.
                        END.    
                    END. /* inv-line */
                    FOR EACH bf-inv-misc NO-LOCK  
                        WHERE bf-inv-misc.r-no    EQ bf-inv-head.r-no 
                          AND bf-inv-misc.company EQ bf-inv-head.company:
                        IF tbArchive THEN DO:
                            IF lProcess THEN 
                                EXPORT STREAM sinv-misc bf-inv-misc.
                            ELSE IF lSimulate THEN 
                                EXPORT STREAM sinv-misc DELIMITER "," bf-inv-misc.
                        END.
                        IF lProcess THEN DO:
                            FIND CURRENT bf-inv-misc EXCLUSIVE-LOCK NO-ERROR.
                            DELETE bf-inv-misc.
                        END.    
                    END. /* inv-misc */
                    IF tbArchive THEN DO:
                        IF lProcess THEN 
                            EXPORT STREAM sinv-head bf-inv-head.
                        ELSE IF lSimulate THEN 
                            EXPORT STREAM sinv-head DELIMITER "," bf-inv-head.
                    END.
                    IF lProcess THEN DO:
                        FIND CURRENT bf-inv-head EXCLUSIVE-LOCK NO-ERROR.
                        DELETE bf-inv-head.
                    END.    
                END. /* inv-head */
            END.                          
            IF tbArchive THEN DO:
                IF lProcess THEN 
                    EXPORT STREAM soe-bolh bf-oe-bolh.
                ELSE IF lSimulate 
                    THEN EXPORT STREAM soe-bolh DELIMITER "," bf-oe-bolh.
            END.
            IF lProcess THEN DO:
                FIND CURRENT bf-oe-bolh EXCLUSIVE-LOCK NO-ERROR.
                DELETE bf-oe-bolh.
            END.  
        END.                    
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE v-post-date AS DATE INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-first-ord LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE v-last-ord LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE cRecKeyPrefix AS CHARACTER NO-UNDO.
    
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
    DISABLE TRIGGERS FOR LOAD OF oe-ordm.
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

    IF NOT lSimulate THEN MESSAGE 
        "Are you sure you want to delete the orders within the selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lProcess.
    IF NOT lSimulate 
    AND NOT lProcess THEN 
        RETURN.

    DO WITH FRAME {&frame-name}:
        ASSIGN
            purge_date
            begin_order
            end_order
            tbInvoices
            tbArchive.
    END.
    cRecKeyPrefix = STRING(YEAR(purge_date),"9999") +
                    STRING(MONTH(purge_date),"99")  +
                    STRING(DAY(purge_date),"99").
                    
    IF tbArchive THEN DO:
        OS-CREATE-DIR VALUE(cDumpLoc).
        OUTPUT STREAM soe-ord       TO VALUE (cDumpLoc + "/oe-ord" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.    
        OUTPUT STREAM soe-rel       TO VALUE (cDumpLoc + "/oe-rel" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.    
        OUTPUT STREAM soe-rell      TO VALUE (cDumpLoc + "/oe-rell" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM soe-relh      TO VALUE (cDumpLoc + "/oe-relh" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM soe-boll      TO VALUE (cDumpLoc + "/oe-boll" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM soe-bolh      TO VALUE (cDumpLoc + "/oe-bolh" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sinv-line     TO VALUE (cDumpLoc + "/inv-line" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM sinv-misc     TO VALUE (cDumpLoc + "/inv-misc" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM sinv-head     TO VALUE (cDumpLoc + "/inv-head" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM soe-ordl      TO VALUE (cDumpLoc + "/oe-ordl" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM soe-ordm      TO VALUE (cDumpLoc + "/oe-ordm" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sjob          TO VALUE (cDumpLoc + "/job" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.       
        OUTPUT STREAM sjob-hdr      TO VALUE (cDumpLoc + "/job-hdr" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sjob-mat      TO VALUE (cDumpLoc + "/job-mat" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sjob-mch      TO VALUE (cDumpLoc + "/job-mch" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sjob-prep     TO VALUE (cDumpLoc + "/job-prep" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM sjob-farm     TO VALUE (cDumpLoc + "/job-farm" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM sjobfarmrctd  TO VALUE (cDumpLoc + "/job-farm-rctd" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.           
        OUTPUT STREAM spc-prdd      TO VALUE (cDumpLoc + "/pc-prdd" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM sfg-act       TO VALUE (cDumpLoc + "/fg-act" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.    
        OUTPUT STREAM smat-act      TO VALUE (cDumpLoc + "/mat-act" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM smch-act      TO VALUE (cDumpLoc + "/mch-act" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.   
        OUTPUT STREAM smisc-act     TO VALUE (cDumpLoc + "/misc-act" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.  
        OUTPUT STREAM sfg-bin       TO VALUE (cDumpLoc + "/fg-bin" + (IF lSimulate THEN ".csv" ELSE ".d")) APPEND.
        IF lSimulate THEN 
            RUN buildHeaders.
    END.    

    ASSIGN
        v-post-date = purge_date
        v-first-ord = begin_order
        v-last-ord  = end_order.

    IF lProcess 
    OR lSimulate THEN DO:
        FOR EACH oe-ord NO-LOCK 
            WHERE oe-ord.company  EQ cocode
            AND oe-ord.ord-date LT v-post-date
            AND oe-ord.ord-no   GE v-first-ord
            AND oe-ord.ord-no   LE v-last-ord
            AND oe-ord.cust-no GE begin_cust:SCREEN-VALUE
            AND oe-ord.cust-no LE end_cust:SCREEN-VALUE
            TRANSACTION:
                
            IF rsClosed:SCREEN-VALUE EQ "C" 
            AND oe-ord.stat NE "C" THEN NEXT.                

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...".
            
            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing order misc records".
            FOR EACH oe-ordm EXCLUSIVE WHERE 
                oe-ordm.company EQ cocode AND 
                oe-ordm.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-ordm     oe-ordm.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-ordm DELIMITER "," oe-ordm.
                END.
                IF lProcess THEN DELETE oe-ordm.
            END. /* oe-ordm */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release records".
            FOR EACH oe-rel EXCLUSIVE WHERE 
                oe-rel.company EQ cocode AND 
                oe-rel.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN DO:
                    IF lProcess THEN EXPORT STREAM soe-rel      oe-rel.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-rel DELIMITER "," oe-rel.
                END.
                IF lProcess THEN DELETE oe-rel.
            END. /* oe-rel */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release line records".
            FOR EACH oe-rell EXCLUSIVE WHERE 
                oe-rell.company EQ oe-ord.company AND 
                oe-rell.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-rell      oe-rell.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-rell DELIMITER "," oe-rell.
                END.
                IF lProcess THEN DELETE oe-rell.
            END. /* oe-rell */

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing release header records".
            FIND FIRST oe-relh EXCLUSIVE WHERE 
                oe-relh.company EQ oe-ord.company AND 
                oe-relh.ord-no EQ oe-ord.ord-no 
                NO-ERROR.
            IF AVAIL oe-relh 
            AND NOT CAN-FIND(FIRST b-rell WHERE 
                            b-rell.r-no EQ oe-relh.r-no AND 
                            b-rell.ord-no NE oe-relh.ord-no) THEN DO:
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-relh      oe-relh.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-relh DELIMITER "," oe-relh.
                END.
                IF lProcess THEN DELETE oe-relh.
            END.
            
            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing BOL line records".
            FOR EACH oe-boll EXCLUSIVE WHERE 
                oe-boll.company EQ oe-ord.company AND 
                oe-boll.ord-no  EQ oe-ord.ord-no:
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-boll      oe-boll.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-boll DELIMITER "," oe-boll.
                END.
                IF lProcess THEN DELETE oe-boll.
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
                    IF tbArchive THEN 
                    DO:
                        IF lProcess THEN EXPORT STREAM sinv-line      inv-line.
                        ELSE IF lSimulate THEN EXPORT STREAM sinv-line DELIMITER "," inv-line.
                    END.
                    IF lProcess THEN DELETE inv-line.
                END. /* inv-line */

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Invoice Misc records".
                FOR EACH inv-misc EXCLUSIVE WHERE 
                    inv-misc.r-no    EQ inv-head.r-no AND 
                    inv-misc.company EQ inv-head.company:
                    IF tbArchive THEN 
                    DO:
                        IF lProcess THEN EXPORT STREAM sinv-misc      inv-misc.
                        ELSE IF lSimulate THEN EXPORT STREAM sinv-misc DELIMITER "," inv-misc.
                    END.
                    IF lProcess THEN DELETE inv-misc.
                END. /* inv-misc */

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Invoice Headers records".
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM sinv-head      inv-head.
                    ELSE IF lSimulate THEN EXPORT STREAM sinv-head DELIMITER "," inv-head.
                END.
                IF lProcess THEN DELETE inv-head.
            END. /* inv-head */

            IF AVAIL oe-bolh
            AND NOT CAN-FIND(FIRST b-boll WHERE 
                            b-boll.b-no EQ oe-bolh.b-no AND 
                            b-boll.ord-no NE oe-bolh.ord-no) THEN DO:
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-bolh      oe-bolh.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-bolh DELIMITER "," oe-bolh.
                END.
                IF lProcess THEN DELETE oe-bolh.
            END.


            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Order line records".
            FOR EACH oe-ordl NO-LOCK WHERE 
                oe-ordl.company EQ cocode AND 
                oe-ordl.ord-no  EQ oe-ord.ord-no:

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job records".
                FOR EACH job EXCLUSIVE WHERE 
                    job.company EQ cocode AND 
                    job.job-no  NE "" AND 
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

                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjob-hdr      job-hdr.
                            ELSE IF lSimulate THEN EXPORT STREAM sjob-hdr DELIMITER "," job-hdr.
                        END.
                        IF lProcess THEN DELETE job-hdr.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job material records".
                    FOR EACH job-mat EXCLUSIVE WHERE 
                        job-mat.company EQ job.company AND 
                        job-mat.job     EQ job.job AND 
                        job-mat.job-no  EQ job.job-no AND 
                        job-mat.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjob-mat      job-mat.
                            ELSE IF lSimulate THEN EXPORT STREAM sjob-mat DELIMITER "," job-mat.
                        END.
                        IF lProcess THEN DELETE job-mat.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job machine records".
                    FOR EACH job-mch EXCLUSIVE WHERE 
                        job-mch.company EQ job.company AND 
                        job-mch.job     EQ job.job AND 
                        job-mch.job-no  EQ job.job-no AND 
                        job-mch.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjob-mch      job-mch.
                            ELSE IF lSimulate THEN EXPORT STREAM sjob-mch DELIMITER "," job-mch.
                        END.
                        IF lProcess THEN DELETE job-mch.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job prep records".
                    FOR EACH job-prep EXCLUSIVE WHERE 
                        job-prep.company EQ job.company AND 
                        job-prep.job     EQ job.job AND 
                        job-prep.job-no  EQ job.job-no AND 
                        job-prep.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjob-prep      job-prep.
                            ELSE IF lSimulate THEN EXPORT STREAM sjob-prep DELIMITER "," job-prep.
                        END.
                        IF lProcess THEN DELETE job-prep.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job farm records".
                    FOR EACH job-farm EXCLUSIVE WHERE 
                        job-farm.company EQ job.company AND 
                        job-farm.job-no  EQ job.job-no AND 
                        job-farm.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjob-farm      job-farm.
                            ELSE IF lSimulate THEN EXPORT STREAM sjob-farm DELIMITER "," job-farm.
                        END.
                        IF lProcess THEN DELETE job-farm.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Job farm receipts records".
                    FOR EACH job-farm-rctd EXCLUSIVE WHERE 
                        job-farm-rctd.company EQ job.company AND 
                        job-farm-rctd.job-no  EQ job.job-no AND 
                        job-farm-rctd.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sjobfarmrctd      job-farm-rctd.
                            ELSE IF lSimulate THEN EXPORT STREAM sjobfarmrctd DELIMITER "," job-farm-rctd.
                        END.
                        IF lProcess THEN DELETE job-farm-rctd.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing ProdCtrl detail records".
                    FOR EACH pc-prdd EXCLUSIVE WHERE 
                        pc-prdd.company EQ cocode AND 
                        pc-prdd.job     EQ job.job AND 
                        pc-prdd.job-no  EQ job.job-no AND 
                        pc-prdd.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM spc-prdd      pc-prdd.
                            ELSE IF lSimulate THEN EXPORT STREAM spc-prdd DELIMITER "," pc-prdd.
                        END.
                        IF lProcess THEN DELETE pc-prdd.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing FG activity records".
                    FOR EACH fg-act EXCLUSIVE WHERE 
                        fg-act.company EQ cocode AND 
                        fg-act.job     EQ job.job AND 
                        fg-act.job-no  EQ job.job-no AND 
                        fg-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sfg-act      fg-act.
                            ELSE IF lSimulate THEN EXPORT STREAM sfg-act DELIMITER "," fg-act.
                        END.
                        IF lProcess THEN DELETE fg-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing RM activity records".
                    FOR EACH mat-act EXCLUSIVE WHERE 
                        mat-act.company EQ cocode AND 
                        mat-act.job     EQ job.job AND 
                        mat-act.job-no  EQ job.job-no AND 
                        mat-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM smat-act      mat-act.
                            ELSE IF lSimulate THEN EXPORT STREAM smat-act DELIMITER "," mat-act.
                        END.
                        IF lProcess THEN DELETE mat-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Machine activity records".
                    FOR EACH mch-act EXCLUSIVE WHERE 
                        mch-act.company EQ cocode AND 
                        mch-act.job     EQ job.job AND 
                        mch-act.job-no  EQ job.job-no AND 
                        mch-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM smch-act      mch-act.
                            ELSE IF lSimulate THEN EXPORT STREAM smch-act DELIMITER "," mch-act.
                        END.
                        IF lProcess THEN DELETE mch-act.
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...removing Misc activity records".
                    FOR EACH misc-act EXCLUSIVE WHERE 
                        misc-act.company EQ cocode AND 
                        misc-act.job     EQ job.job AND 
                        misc-act.job-no  EQ job.job-no AND 
                        misc-act.job-no2 EQ job.job-no2:
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM smisc-act      misc-act.
                            ELSE IF lSimulate THEN EXPORT STREAM smisc-act DELIMITER "," misc-act.
                        END.
                        IF lProcess THEN DELETE misc-act.
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
                        IF tbArchive THEN 
                        DO:
                            IF lProcess THEN EXPORT STREAM sfg-bin      fg-bin.
                            ELSE IF lSimulate THEN EXPORT STREAM sfg-bin DELIMITER "," fg-bin.
                        END.
                        IF lProcess THEN DELETE fg-bin.
                    END.

                    IF job.exported THEN DO:
                        job.stat = "X".
                        RUN jc/kiwiexp2.p (RECID(job)).
                    END.

                    STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting job".
                    IF tbArchive THEN 
                    DO:
                        IF lProcess THEN EXPORT STREAM sjob      job.
                        ELSE IF lSimulate THEN EXPORT STREAM sjob DELIMITER "," job.
                    END.
                    IF lProcess THEN DELETE job.
                END.

                STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting order line".
                FIND CURRENT oe-ordl EXCLUSIVE.
                IF tbArchive THEN 
                DO:
                    IF lProcess THEN EXPORT STREAM soe-ordl     oe-ordl.
                    ELSE IF lSimulate THEN EXPORT STREAM soe-ordl DELIMITER "," oe-ordl.
                END.
                IF lProcess THEN DELETE oe-ordl.
            END.

            STATUS DEFAULT "Processing order# " + STRING(oe-ord.ord-no) + "...deleting order header".
            FIND CURRENT oe-ord EXCLUSIVE.
            IF tbArchive THEN 
            DO:
                IF lProcess THEN EXPORT STREAM soe-ord      oe-ord.
                ELSE IF lSimulate THEN EXPORT STREAM soe-ord DELIMITER "," oe-ord.
            END.
            IF lProcess THEN DELETE oe-ord.
        END. /* oe-ord */
        RUN pDeleteOrphanRecords(
            INPUT cocode,
            INPUT cRecKeyPrefix,
            INPUT tbArchive
            ).
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
            OUTPUT STREAM soe-ordm      CLOSE. 
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

