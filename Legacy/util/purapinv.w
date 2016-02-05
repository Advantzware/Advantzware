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

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.
DEFINE STREAM st-apinv .
DEFINE STREAM st-apinvl.
DEFINE STREAM st-appay.
DEFINE STREAM st-appayl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_date end_date tb_open ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date tb_open 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 8.57.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL no 
     LABEL "Purge Unpaid/Open AP Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 7.67 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 7.67 COL 60 COLON-ALIGNED HELP
          "Enter Ending Date"
     tb_open AT ROW 9.81 COL 31
     btn-process AT ROW 15.29 COL 21
     btn-cancel AT ROW 15.29 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 17.67.

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
         TITLE              = "Purge Paid AP Invoices"
         HEIGHT             = 17.67
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
                                                                        */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Paid AP Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Paid AP Invoices */
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


  MESSAGE "Are you sure you want to delete the Paid AP Invoices within the " +
          "selection parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
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
  DISPLAY begin_date end_date tb_open 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_date end_date tb_open btn-process btn-cancel 
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
/* ------------------------------------------------ util/apinvdel.p 03/98 JLF */
/* Purge Paid AP Invoices                                                     */
/* -------------------------------------------------------------------------- */

def buffer b-ap-payl for ap-payl.

def var fdate like ap-inv.inv-date init 01/01/0001 format "99/99/9999".
def var tdate like fdate           init 01/01/0001 format "99/99/9999".
def var v-open as log              init no         format "Yes/No".
def var amt as dec.

DEFINE VARIABLE v-apinv-file AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-apinvl-file AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-appay-file AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-appayl-file AS CHARACTER  NO-UNDO.




ASSIGN 
   v-apinv-file = "c:/tmp/"  + "ap-inv" + STRING (TODAY, "999999") + string(TIME) + ".d"  .
   v-apinvl-file = "c:/tmp/"  + "ap-invl" + STRING (TODAY, "999999") + string(TIME) + ".d"  .
   v-appay-file = "c:/tmp/"  + "ap-pay" + STRING (TODAY, "999999") + string(TIME) + ".d"  .
   v-appayl-file = "c:/tmp/"  + "ap-payl" + STRING (TODAY, "999999") + string(TIME) + ".d"  .


OUTPUT STREAM st-apinv  TO VALUE(v-apinv-file) .
OUTPUT STREAM st-apinvl TO VALUE(v-apinvl-file) .
OUTPUT STREAM st-appay  TO VALUE(v-appay-file) .
OUTPUT STREAM st-appayl TO VALUE(v-appayl-file) .

session:set-wait-state("General").

do with frame {&frame-name}:
  assign
   begin_date
   end_date 
   tb_open  .
end.
 
assign
 fdate     = begin_date
 tdate     = end_date
 v-process = no
 v-open = tb_open.


DISABLE TRIGGERS FOR LOAD OF ap-inv.
DISABLE TRIGGERS FOR LOAD OF ap-pay.

for each ap-inv
    where ap-inv.company  eq cocode
      and ap-inv.posted   eq yes
      and ap-inv.inv-date ge fdate
      and ap-inv.inv-date le tdate
    use-index posted:

        amt       = 0 .
  
  for each ap-payl
      where ap-payl.inv-no   eq ap-inv.inv-no
        and ap-payl.vend-no  eq ap-inv.vend-no
        and ap-payl.posted   eq yes
        and ap-payl.due-date eq ap-inv.due-date
        use-index inv-no no-lock,
      first ap-pay where ap-pay.c-no eq ap-payl.c-no
      use-index c-no no-lock:

    amt = amt - ap-payl.amt-paid +
          (ap-payl.amt-disc * if ap-payl.memo then 1 else -1) .
  
  end. /* for each ap-payl */

  amt = amt + ap-inv.net.
  
  IF v-open AND ( (ap-inv.due - ap-inv.paid ) > 0 OR 
                  (ap-inv.due - ap-inv.paid ) < 0 ) THEN amt = 0.
  
  if amt eq 0 then do:
    for each ap-payl
        where ap-payl.inv-no   eq ap-inv.inv-no
          and ap-payl.vend-no  eq ap-inv.vend-no
          and ap-payl.posted   eq yes
          and ap-payl.due-date eq ap-inv.due-date
        use-index inv-no:

      find ap-pay
          where ap-pay.c-no eq ap-payl.c-no
          use-index c-no no-error.
      if avail ap-pay then do:
        find first b-ap-payl
            where b-ap-payl.c-no   eq ap-payl.c-no
              and recid(b-ap-payl) ne recid(ap-payl)
            no-lock no-error.
        if not avail b-ap-payl then  DO :
         EXPORT STREAM st-appay ap-pay.
         delete ap-pay.
        END.
      end.
      
      EXPORT STREAM st-appayl ap-payl.
      delete ap-payl.
    end.

    for each ap-invl where ap-invl.company eq cocode
                       and ap-invl.i-no eq ap-inv.i-no:

      EXPORT STREAM st-apinvl ap-invl.
      delete ap-invl.
    end.

    EXPORT STREAM st-apinv ap-inv.
    delete ap-inv.
  end.
end.

OUTPUT STREAM st-apinv CLOSE. 
OUTPUT STREAM st-apinvl CLOSE. 
OUTPUT STREAM st-appay CLOSE. 
OUTPUT STREAM st-appayl CLOSE. 


session:set-wait-state("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.
apply "close" to this-procedure.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

