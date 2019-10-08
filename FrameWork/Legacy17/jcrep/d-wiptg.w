&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: jcrep\d-wiptg.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{jcrep/wiptg.i}
{jcrep/wip-tt.i}
{custom/globdefs.i}

/*DEF VAR calcTags AS LOG NO-UNDO INIT YES. /* sys ctrl option */ */
DEF VAR autoCopy AS LOG NO-UNDO.
DEF VAR copyRowID AS ROWID NO-UNDO.

DEF VAR receiptQty LIKE w-job.tag-qty NO-UNDO.
DEF VAR partialQty LIKE w-job.partial NO-UNDO.
DEF VAR totalTags LIKE w-job.total-tags NO-UNDO.
DEF VAR consUOM AS CHAR FORMAT "X(4)" NO-UNDO.
DEF VAR v-sht-len AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sht-wid AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-tag-date AS DATE NO-UNDO.
DEF VAR v-tag-timex AS CHAR NO-UNDO.
DEF VAR v-wip-whs AS CHAR NO-UNDO.
DEF VAR v-wip-bin AS CHAR NO-UNDO.
DEF VAR v-machine AS CHAR NO-UNDO.
DEF VAR op-error AS LOG NO-UNDO.

DEF BUFFER bu-w-job FOR w-job.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-job

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 w-job.job-no w-job.job-no2 NO-LABEL w-job.cust-no w-job.fg-i-no w-job.frm w-job.blank-no w-job.rm-i-no w-job.wid w-job.len w-job.tag-date w-job.tag-timex w-job.sht-wid w-job.sht-len w-job.wip-whs w-job.wip-bin w-job.first-mach-code w-job.qty /* w-job.tag-no btr */ /*w-job.rmtag2 btr*/ w-job.sheets-tag w-job.qty-uom w-job.tag-qty w-job.partial w-job.cons-uom w-job.total-tags   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 w-job.sht-wid   w-job.sht-len   w-job.wip-whs   w-job.wip-bin   w-job.first-mach-code   w-job.tag-qty   w-job.partial   w-job.cons-uom   w-job.total-tags   w-job.tag-date   w-job.tag-timex   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 w-job
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 w-job
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH w-job BY w-job.job-no BY w-job.job-no2 BY w-job.frm BY w-job.blank-no
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH w-job BY w-job.job-no BY w-job.job-no2 BY w-job.frm BY w-job.blank-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 w-job
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 w-job


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btnCancel btn_save btn_copy ~
btn_delete Btn_OK btnAutoCopy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAutoCopy 
     LABEL "&Auto Copy Off" 
     SIZE 20 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON btnCancel 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.62.

DEFINE BUTTON btn_copy 
     LABEL "&Copy" 
     SIZE 15 BY 1.62.

DEFINE BUTTON btn_delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_OK 
     LABEL "&Create Tags" 
     SIZE 18 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON btn_save 
     LABEL "&Save" 
     SIZE 15 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      w-job SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      w-job.job-no  LABEL '  Job#'
  w-job.job-no2 NO-LABEL FORMAT '99'
  w-job.cust-no LABEL "Cust #"
  w-job.fg-i-no LABEL "FG Item #"
  w-job.frm LABEL 'Form #' FORMAT '>>9'
  w-job.blank-no LABEL 'Blank #' FORMAT '>9'
  w-job.rm-i-no LABEL 'RM Item #'
  w-job.wid LABEL 'Width'  FORMAT '>>9.99<<'
  w-job.len LABEL 'Length' FORMAT '>>9.99<<'
  w-job.tag-date LABEL 'Tag Date'
  w-job.tag-timex LABEL 'Tag Time'
  w-job.sht-wid LABEL 'Sht Wid' FORMAT '>>9.99<<'
  w-job.sht-len LABEL 'Sht Len' FORMAT '>>9.99<<'
  w-job.wip-whs LABEL 'WIP Whs.' FORMAT "X(5)"
  w-job.wip-bin LABEL 'WIP Bin' FORMAT "X(8)"
  w-job.first-mach-code LABEL 'First Mach.' FORMAT "X(8)"
  w-job.qty LABEL 'Total MRP'
  /* w-job.tag-no LABEL 'RM Tag Used'  btr */
  /*w-job.rmtag2 LABEL '2nd RM Tag Used' btr*/
  w-job.sheets-tag LABEL 'Sheets From RMTags'
  w-job.qty-uom LABEL 'UOM'
  w-job.tag-qty COLUMN-LABEL 'Tag Qty'
  w-job.partial COLUMN-LABEL 'Partial!Tag'
  w-job.cons-uom COLUMN-LABEL 'Stock!UOM'
  w-job.total-tags LABEL 'Tags'
  
  ENABLE
    w-job.sht-wid
    w-job.sht-len
    w-job.wip-whs
    w-job.wip-bin
    w-job.first-mach-code
    w-job.tag-qty 
    w-job.partial
    w-job.cons-uom
    w-job.total-tags
    w-job.tag-date
    w-job.tag-timex
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 14.05
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     btnCancel AT ROW 15.29 COL 5
     btn_save AT ROW 15.29 COL 27
     btn_copy AT ROW 15.29 COL 49
     btn_delete AT ROW 15.29 COL 71
     Btn_OK AT ROW 15.29 COL 93
     btnAutoCopy AT ROW 15.29 COL 118
     SPACE(5.19) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 0
         TITLE "WIP Tag Creation Detail".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-job BY w-job.job-no BY w-job.job-no2 BY w-job.frm BY w-job.blank-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* WIP Tag Creation Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  APPLY "choose" TO btn_save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ENTRY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN saveValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAutoCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAutoCopy Dialog-Frame
ON CHOOSE OF btnAutoCopy IN FRAME Dialog-Frame /* Auto Copy Off */
DO:
  ASSIGN
    autoCopy = NOT autoCopy
    SELF:LABEL = '~&Auto Copy ' + STRING(autoCopy,'On/Off').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  IF NOT AVAIL w-job THEN RETURN NO-APPLY.
  IF autoCopy AND copyRowID EQ ROWID(w-job) THEN DELETE w-job.
  ELSE RUN resetValues.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  APPLY 'CHOOSE' TO btn_save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_copy Dialog-Frame
ON CHOOSE OF btn_copy IN FRAME Dialog-Frame /* Copy */
DO:
  IF NOT AVAIL w-job THEN RETURN NO-APPLY.
  RUN copy-wjob.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  REPOSITION {&BROWSE-NAME} TO ROWID copyRowID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_delete Dialog-Frame
ON CHOOSE OF btn_delete IN FRAME Dialog-Frame /* Delete */
DO:
  IF NOT AVAIL w-job THEN RETURN NO-APPLY.
  MESSAGE "Are you sure you want to delete? " VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-ans AS LOG.             
  IF v-ans THEN RUN del-wjob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Create Tags */
DO:
  APPLY 'GO' TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_save Dialog-Frame
ON CHOOSE OF btn_save IN FRAME Dialog-Frame /* Save */
DO:
  IF NOT AVAIL w-job THEN RETURN NO-APPLY.
  RUN update-wjob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 

ON HELP OF w-job.wip-whs IN BROWSE {&BROWSE-NAME} DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   RUN windows/l-loc.w (g_company,SELF:SCREEN-VALUE,OUTPUT char-val).
   IF char-val <> "" THEN
      ASSIGN w-job.wip-whs:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).

END.

ON HELP OF w-job.wip-bin IN BROWSE {&BROWSE-NAME} DO:
   DEF VAR char-val AS cha NO-UNDO.
    run rm/l-wipbin.w (g_company,w-job.wip-whs:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, output char-val).
   IF char-val <> "" THEN
      ASSIGN w-job.wip-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).

END.

ON 'RETURN' OF w-job.tag-qty IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-job.tag-qty IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.tag-qty IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-job.partial IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-job.partial IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.partial IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-job.cons-uom IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-uom(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.cons-uom IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-uom(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.total-tags IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-job.total-tags IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.sht-wid IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-job.sht-wid IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.sht-len IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-job.sht-len IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.tag-date IN BROWSE {&BROWSE-NAME} DO:
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.tag-timex IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-time(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.tag-timex IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-time(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.wip-whs IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-wip-whs(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.wip-whs IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-wip-whs(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.wip-bin IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-wip-bin(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.wip-bin IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-wip-bin(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
END.

ON 'RETURN' OF w-job.first-mach-code IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-mach-code(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-job.first-mach-code IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-mach-code(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /*FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name    EQ 'RMTAGS' NO-ERROR.
  IF AVAIL sys-ctrl THEN
  calcTags = sys-ctrl.log-fld.*/

    /* btr test */
/*   FOR EACH tt-rmtags:                                  */
/*       DISP tt-rmtags WITH FRAME dd WIDTH 360 15 DOWN.  */
/*   END.                                                 */
/*   PAUSE.                                               */
/*   FOR EACH tt-used-tags:                                   */
/*       DISP tt-used-tags WITH FRAME ddd WIDTH 360 15 DOWN.  */
/*   END.                                                     */
/*   PAUSE.                                                   */
/* btr test end */


  w-job: FOR EACH w-job BY w-job.frm:
      IF w-job.processed THEN NEXT w-job.
      FOR EACH tt-used-tags WHERE tt-used-tags.rm-i-no = w-job.rm-i-no AND
                                    tt-used-tags.frm = w-job.frm AND
                                    tt-used-tags.used < w-job.tag-qty:

          BUFFER-COPY w-job TO bu-w-job
              ASSIGN bu-w-job.tag-no = tt-used-tags.rmtag
                     bu-w-job.processed = TRUE
                     bu-w-job.partial = tt-used-tags.usedshts
                     bu-w-job.sheets-tag = tt-used-tags.usedshts
                     bu-w-job.total-tags = 1.

          ASSIGN w-job.tag-no = tt-used-tags.rmtag                        
                 w-job.total-tags = w-job.total-tags - 1                  
                 w-job.sheets-tag = w-job.tag-qty * w-job.total-tags 
                 w-job.processed = TRUE.

      END.
/*       FIND FIRST tt-used-tags WHERE tt-used-tags.rm-i-no = w-job.rm-i-no AND  */
/*                                     tt-used-tags.frm = w-job.frm              */
/*           USE-INDEX idx1 NO-ERROR.                                            */
/*       IF AVAIL tt-used-tags THEN DO:                                          */
/*           IF w-job.qty / w-job.tag-qty < w-job.tag-qty THEN DO:               */
/*               ASSIGN w-job.tag-no = tt-used-tags.rmtag                        */
/*                      w-job.total-tags = w-job.total-tags - 1                  */
/*                      w-job.sheets-tag = w-job.tag-qty * w-job.total-tags      */
/*                      w-job.processed = TRUE.                                  */
/*                                                                               */
/*                                                                               */
/*           END.                                                                */
/*       END.                                                                    */
/*       FIND NEXT tt-used-tags WHERE tt-used-tags.rm-i-no = w-job.rm-i-no AND  */
/*                                     tt-used-tags.frm = w-job.frm             */
/*           USE-INDEX idx1 NO-ERROR.                                           */
/*       IF AVAIL tt-used-tags THEN DO:                                         */
/*           BUFFER-COPY w-job TO bu-w-job                                      */
/*               ASSIGN bu-w-job.tag-no = tt-used-tags.rmtag                    */
/*                      bu-w-job.processed = TRUE                               */
/*                      bu-w-job.partial = tt-used-tags.usedshts                */
/*                      bu-w-job.sheets-tag = tt-used-tags.usedshts             */
/*                      bu-w-job.total-tags = 1.                                */
/*       END.                                                                   */

  END.


  /* btr test start */

  DEF STREAM xout.
  OUTPUT STREAM xout TO VALUE("C:\tmp\w-job-" + STRING(TIME,"99999") + 
                               ".txt").
EXPORT STREAM xout DELIMITER ","
  "tag-no " 
  " company "
  " location "
  " cust-no "
  " blank-no "
  " rm-i-name "
  " rm-i-no "
  " fg-i-name"
  " fg-i-no "
  " job-no "
  " job-no2 "
  " qty "
  " qty-uom "
  " cons-uom " 
  " frm "
  " wid "
  " len "
  " partial-tags "
  " partial-tag-qty "
  " expected-sheet-qty "
  " partial "
  " tag-qty "
  " total-tags"
  " upd-date "
  " upd-time "
  " tag-date"
  " tag-time "
  " num-out "
  " num-up "
  " rm-whs "
  " rm-bin "
  " job-due-date "
  " last-mach-code "
  " last-prod-qty "
  " sheets-tag "
  " sht-wid"
  " sht-len"
  " wip-whs "
  " wip-bin "
  " first-mach-code "
  " tag-timex "
  " processed".
  FOR EACH w-job:
      EXPORT STREAM xout DELIMITER "," w-job. 

  END.
/* btr test end */

/*   FOR EACH tt-used-tags BY tt-used-tags.rm-i-no BY tt-used-tags.frm  */
/*       BY tt-used-tags.usedshts:                                      */
/*       FOR EACH w-job WHERE w-job.rm-i-no = tt-used-tags.rm-i-no AND  */
/*                            w-job.frm = tt-used-tags.frm:             */
/*           IF tt-used-tags.usedsheets < w-job.qty THEN DO:            */
/*               BUFFER-COPY w-job TO bu-w-job                          */
/*                   ASSIGN bu-w-job.tag-qty =                          */
/*           END.                                                       */
/*                                                                      */
/*       END.                                                           */
/*                                                                      */
/*   END.                                                               */


  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoCopy Dialog-Frame 
PROCEDURE autoCopy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    APPLY 'CHOOSE' TO btn_copy.
    APPLY 'ENTRY' TO w-job.tag-qty IN BROWSE {&BROWSE-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total Dialog-Frame 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL w-job THEN
  DO:
    ASSIGN
      w-job.partial = INT(w-job.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      w-job.tag-qty
      w-job.total-tags = 1

    /*IF calcTags THEN*/
    w-job.total-tags = (w-job.qty / w-job.tag-qty + .49) +
                       (IF w-job.partial GT 0 THEN 1 ELSE 0).
    DISPLAY w-job.total-tags WITH BROWSE {&BROWSE-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-wjob Dialog-Frame 
PROCEDURE copy-wjob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-wjob FOR w-job.

  CREATE bf-wjob.
  BUFFER-COPY w-job TO bf-wjob
     ASSIGN
        bf-wjob.total-tags = 1
        bf-wjob.partial = 0
        bf-wjob.tag-qty = 0
        copyRowID = ROWID(bf-wjob).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-wjob Dialog-Frame 
PROCEDURE del-wjob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DELETE w-job.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE BROWSE-1 btnCancel btn_save btn_copy btn_delete Btn_OK btnAutoCopy 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetValues Dialog-Frame 
PROCEDURE resetValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    w-job.tag-qty = receiptQty
    w-job.partial = partialQty
    w-job.total-tags = totalTags
    w-job.cons-uom = consUOM
    w-job.sht-len = v-sht-len
    w-job.sht-wid = v-sht-wid
    w-job.tag-date = v-tag-date
    w-job.tag-timex = v-tag-timex 
    w-job.wip-whs = v-wip-whs
    w-job.wip-bin = v-wip-bin
    w-job.first-mach-code = v-machine.

  DISPLAY w-job.tag-qty w-job.partial w-job.cons-uom
          w-job.total-tags w-job.sht-len w-job.sht-wid
          w-job.tag-date w-job.tag-timex w-job.wip-whs
          w-job.wip-bin w-job.first-mach-code WITH BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveValues Dialog-Frame 
PROCEDURE saveValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    receiptQty = w-job.tag-qty
    partialQty = w-job.partial
    totalTags = w-job.total-tags
    consUOM = w-job.cons-uom
    v-sht-len = w-job.sht-len
    v-sht-wid = w-job.sht-wid
    v-tag-date  = w-job.tag-date
    v-tag-timex = w-job.tag-timex
    v-wip-whs = w-job.wip-whs
    v-wip-bin = w-job.wip-bin
    v-machine = w-job.first-mach-code.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-wjob Dialog-Frame 
PROCEDURE update-wjob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF btn_save:LABEL IN FRAME {&FRAME-NAME} EQ '&Save' THEN DO:
    ASSIGN
      w-job.partial = INT(w-job.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      w-job.tag-qty
      w-job.total-tags
      w-job.tag-qty:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
      w-job.partial:READ-ONLY = YES
      w-job.cons-uom:READ-ONLY = YES
      w-job.total-tags:READ-ONLY = YES
      w-job.sht-len
      w-job.sht-wid
      w-job.sht-len:READ-ONLY = YES
      w-job.sht-wid:READ-ONLY = YES
      w-job.tag-date
      w-job.tag-date:READ-ONLY = YES
      w-job.tag-timex
      w-job.tag-timex:READ-ONLY = YES
      w-job.first-mach-code
      w-job.first-mach-code:READ-ONLY = YES
      w-job.wip-whs
      w-job.wip-whs:READ-ONLY = YES
      w-job.wip-bin
      w-job.wip-bin:READ-ONLY = YES
      btn_save:LABEL IN FRAME {&FRAME-NAME} = '&Update'
      btnCancel:HIDDEN = YES
      btnCancel:SENSITIVE = NO.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  END.
  ELSE
  DO:
    ASSIGN
      btn_save:LABEL = '&Save'
      w-job.tag-qty:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
      w-job.partial:READ-ONLY = NO
      w-job.cons-uom:READ-ONLY = NO
      w-job.total-tags:READ-ONLY = NO
      w-job.sht-len:READ-ONLY = NO
      w-job.sht-wid:READ-ONLY = NO
      w-job.tag-date:READ-ONLY = NO
      w-job.tag-timex:READ-ONLY = NO
      w-job.first-mach-code:READ-ONLY = NO
      w-job.wip-whs:READ-ONLY = NO
      w-job.wip-bin:READ-ONLY = NO
      btnCancel:HIDDEN = NO
      btnCancel:SENSITIVE = YES.
    APPLY 'ENTRY' TO w-job.tag-qty IN BROWSE {&BROWSE-NAME}.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mach-code Dialog-Frame 
PROCEDURE valid-mach-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   IF LASTKEY EQ -1 THEN
      LEAVE.

   DO WITH FRAME {&FRAME-NAME}:
    
      IF w-job.first-mach-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" AND
         NOT CAN-FIND(FIRST mach WHERE
         mach.company EQ w-job.company AND
         mach.loc     EQ w-job.location AND
         mach.m-code  EQ w-job.first-mach-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN
         DO:
            MESSAGE "Invalid First Machine." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO w-job.first-mach-code IN BROWSE {&BROWSE-NAME}.
            op-error = YES.
         END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-time Dialog-Frame 
PROCEDURE valid-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   DEF VAR v-int AS INT NO-UNDO.

   IF LASTKEY EQ -1 THEN
      LEAVE.

   DO WITH FRAME {&FRAME-NAME}:
    
      ERROR-STATUS:ERROR = NO.

      v-int = INT(SUBSTRING(w-job.tag-timex:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},1,2)) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
         op-error = YES.

      IF op-error = NO THEN
         v-int = INT(SUBSTRING(w-job.tag-timex:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},4,2)) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
         op-error = YES.

      IF op-error = NO AND
         INT(SUBSTRING(w-job.tag-timex:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},1,2)) GT 23 OR
         SUBSTRING(w-job.tag-timex:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},3,1) NE ":" OR
         INT(SUBSTRING(w-job.tag-timex:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},4,2)) GT 59 THEN
         op-error = YES.

      IF op-error THEN
      DO:
         MESSAGE "Invalid Time. (Format HH:MM)" VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO w-job.tag-timex IN BROWSE {&BROWSE-NAME}.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Dialog-Frame 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
  
  DEF VAR lv-uom-list AS CHAR INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.

  IF LASTKEY EQ -1 THEN
      LEAVE.

  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = w-job.cons-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

    FIND FIRST item NO-LOCK
        WHERE item.company EQ w-job.company
          AND item.i-no EQ w-job.rm-i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type,OUTPUT lv-uom-list).

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list + ".".

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
       MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO w-job.cons-uom IN BROWSE {&BROWSE-NAME}.
       op-error = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-wip-bin Dialog-Frame 
PROCEDURE valid-wip-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   IF LASTKEY EQ -1 THEN
      LEAVE.

   DO WITH FRAME {&FRAME-NAME}:
   
      IF w-job.wip-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
         MESSAGE "WIP Bin can not blank" VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO w-job.wip-bin IN BROWSE {&browse-name}.
         op-error = YES.
      END.

      ELSE IF  NOT CAN-FIND(FIRST wip-bin WHERE
             wip-bin.company EQ w-job.company AND
             wip-bin.loc     EQ w-job.wip-whs:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
             wip-bin.loc-bin EQ w-job.wip-bin:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
             DO:
                MESSAGE "Invalid WIP Bin." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO w-job.wip-bin IN BROWSE {&browse-name}.
                op-error = YES.
             END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-wip-whs Dialog-Frame 
PROCEDURE valid-wip-whs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   IF LASTKEY EQ -1 THEN
      LEAVE.

   DO WITH FRAME {&FRAME-NAME}:
    
     IF w-job.wip-whs:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN DO:
         MESSAGE "WIP Warehouse can not blank." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO w-job.wip-whs IN BROWSE {&BROWSE-NAME}.
            op-error = YES.
      END.

       ELSE IF NOT CAN-FIND(FIRST loc WHERE
         loc.company EQ w-job.company AND
         loc.loc     EQ w-job.wip-whs:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN
         DO:
            MESSAGE "Invalid WIP Warehouse." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO w-job.wip-whs IN BROWSE {&BROWSE-NAME}.
            op-error = YES.
         END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

