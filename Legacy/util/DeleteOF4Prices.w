&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\globpric.w

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
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

def var start-cust-no   like oe-prmtx.cust-no NO-UNDO.
def var end-cust-no     like oe-prmtx.cust-no init "zzzzzzzz" NO-UNDO.

def var start-cust-type like oe-prmtx.custype NO-UNDO.
def var end-cust-type   like oe-prmtx.custype init "zzzzzzzz" NO-UNDO.

def var start-item-no   like oe-prmtx.i-no NO-UNDO.
def var end-item-no     like oe-prmtx.i-no init "zzzzzzzzzzzzzzz" NO-UNDO.

def var start-prod-cat  like oe-prmtx.procat NO-UNDO.
def var end-prod-cat    like oe-prmtx.procat init "zzzzz" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust end_cust begin_cust-type ~
end_cust-type begin_i-no end_i-no begin_cat end_cat btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_cust-type ~
end_cust-type begin_i-no end_i-no begin_cat end_cat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
  ( BUFFER ipb-oe-prmtx FOR oe-prmtx )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Type" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Type" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 12.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 6.71 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 6.71 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 7.91 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 7.91 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     begin_i-no AT ROW 9.1 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 9.1 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cat AT ROW 10.29 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 10.29 COL 65 COLON-ALIGNED HELP
          "Enter Ending Category"
     btn-process AT ROW 14.1 COL 21
     btn-cancel AT ROW 14.1 COL 53
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 73 BY .95 AT ROW 2.91 COL 8 WIDGET-ID 12
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 5.52 COL 5
     RECT-17 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.4 BY 17.52.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This procedure will create a backup file in the C:~\tmp~\ directory." VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 1.71 COL 3
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.6 ROW 1
         SIZE 82 BY 3.81
         BGCOLOR 11  WIDGET-ID 100.


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
         TITLE              = "Global Price Matrix Change"
         HEIGHT             = 14.67
         WIDTH              = 90.2
         MAX-HEIGHT         = 20.33
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 20.33
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


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Price Matrix Change */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Global Price Matrix Change */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to change the Price Matrix(es) within the " +
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
/*   IF access-close THEN DO:            */
/*      APPLY "close" TO THIS-PROCEDURE. */
/*      RETURN.                          */
/*   END.                                */


  RUN enable_UI.
  {methods/nowait.i}

  APPLY "ENTRY":U TO begin_cust IN FRAME {&FRAME-NAME}.

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
  DISPLAY begin_cust end_cust begin_cust-type end_cust-type begin_i-no end_i-no 
          begin_cat end_cat 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust end_cust begin_cust-type end_cust-type begin_i-no 
         end_i-no begin_cat end_cat btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE makeBackup C-Win 
PROCEDURE makeBackup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lcOutputFile AS CHAR NO-UNDO.

lcOutputFile = "C:\tmp\OF4backup" + 
    STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME) + ".d".
OUTPUT TO VALUE(lcOutputFile).
FOR EACH oe-prmtx NO-LOCK:
    EXPORT  
        asi.oe-prmtx.company 
        asi.oe-prmtx.cust-no 
        asi.oe-prmtx.custype 
        asi.oe-prmtx.eff-date 
        asi.oe-prmtx.meth 
        asi.oe-prmtx.i-no
        asi.oe-prmtx.procat 
        asi.oe-prmtx.exp-date 
        asi.oe-prmtx.ex-rate 
        asi.oe-prmtx.curr-code[1] 
        asi.oe-prmtx.curr-code[2] 
        asi.oe-prmtx.upd-time 
        asi.oe-prmtx.upd-date 
        asi.oe-prmtx.setup 
        asi.oe-prmtx.rec_key 
        asi.oe-prmtx.qty[1] 
        asi.oe-prmtx.qty[2] 
        asi.oe-prmtx.qty[3] 
        asi.oe-prmtx.qty[4] 
        asi.oe-prmtx.qty[5] 
        asi.oe-prmtx.qty[6] 
        asi.oe-prmtx.qty[7] 
        asi.oe-prmtx.qty[8] 
        asi.oe-prmtx.qty[9] 
        asi.oe-prmtx.qty[10] 
        asi.oe-prmtx.price[1] 
        asi.oe-prmtx.price[2] 
        asi.oe-prmtx.price[3] 
        asi.oe-prmtx.price[4] 
        asi.oe-prmtx.price[5] 
        asi.oe-prmtx.price[6] 
        asi.oe-prmtx.price[7] 
        asi.oe-prmtx.price[8] 
        asi.oe-prmtx.price[9] 
        asi.oe-prmtx.price[10]
        asi.oe-prmtx.uom[1] 
        asi.oe-prmtx.uom[2] 
        asi.oe-prmtx.uom[3] 
        asi.oe-prmtx.uom[4] 
        asi.oe-prmtx.uom[5] 
        asi.oe-prmtx.uom[6] 
        asi.oe-prmtx.uom[7] 
        asi.oe-prmtx.uom[8] 
        asi.oe-prmtx.uom[9] 
        asi.oe-prmtx.uom[10] 
        asi.oe-prmtx.discount[1] 
        asi.oe-prmtx.discount[2] 
        asi.oe-prmtx.discount[3] 
        asi.oe-prmtx.discount[4] 
        asi.oe-prmtx.discount[5] 
        asi.oe-prmtx.discount[6] 
        asi.oe-prmtx.discount[7] 
        asi.oe-prmtx.discount[8] 
        asi.oe-prmtx.discount[9] 
        asi.oe-prmtx.discount[10] 
        asi.oe-prmtx.cnt[1] 
        asi.oe-prmtx.cnt[2] 
        asi.oe-prmtx.cnt[3] 
        asi.oe-prmtx.cnt[4] 
        asi.oe-prmtx.cnt[5] 
        asi.oe-prmtx.cnt[6] 
        asi.oe-prmtx.cnt[7] 
        asi.oe-prmtx.cnt[8] 
        asi.oe-prmtx.cnt[9] 
        asi.oe-prmtx.cnt[10] 
        asi.oe-prmtx.updated-id[1] 
        asi.oe-prmtx.updated-id[2] 
        asi.oe-prmtx.updated-id[3] 
        asi.oe-prmtx.updated-id[4] 
        asi.oe-prmtx.updated-id[5] 
        asi.oe-prmtx.updated-id[6] 
        asi.oe-prmtx.updated-id[7] 
        asi.oe-prmtx.updated-id[8] 
        asi.oe-prmtx.updated-id[9] 
        asi.oe-prmtx.updated-id[10] 
        asi.oe-prmtx.updated-date[1] 
        asi.oe-prmtx.updated-date[2] 
        asi.oe-prmtx.updated-date[3] 
        asi.oe-prmtx.updated-date[4] 
        asi.oe-prmtx.updated-date[5] 
        asi.oe-prmtx.updated-date[6] 
        asi.oe-prmtx.updated-date[7] 
        asi.oe-prmtx.updated-date[8] 
        asi.oe-prmtx.updated-date[9] 
        asi.oe-prmtx.updated-date[10]
        asi.oe-prmtx.adder[1] 
        asi.oe-prmtx.adder[2] 
        asi.oe-prmtx.adder[3] 
        asi.oe-prmtx.adder[4] 
        asi.oe-prmtx.adder[5] 
        asi.oe-prmtx.adder[6] 
        asi.oe-prmtx.adder[7] 
        asi.oe-prmtx.adder[8] 
        asi.oe-prmtx.adder[9] 
        asi.oe-prmtx.adder[10] 
        asi.oe-prmtx.adder[11] 
        asi.oe-prmtx.adder[12] 
        asi.oe-prmtx.adder[13] 
        asi.oe-prmtx.adder[14] 
        asi.oe-prmtx.adder[15] .
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ---------------------------------------------------oe/oe-pmgpc.p 05/00 DJK */
/* Global Price Matrix Change                                                 */
/* -------------------------------------------------------------------------- */

SESSION:SET-WAIT-STATE("General").

DEF VAR v-date AS DATE NO-UNDO.
DEF VAR v-date-str AS CHAR NO-UNDO.
DEF VAR v-start-i-no AS CHAR FORMAT "X(108)" NO-UNDO.

RUN makeBackup.

ASSIGN
 start-cust-no   = begin_cust
 end-cust-no     = end_cust
 start-cust-type = begin_cust-type
 end-cust-type   = end_cust-type
 start-item-no   = begin_i-no
 end-item-no     = end_i-no
 start-prod-cat  = begin_cat
 end-prod-cat    = end_cat.

FOR EACH oe-prmtx EXCLUSIVE-LOCK
    WHERE oe-prmtx.company    EQ cocode 
      AND oe-prmtx.cust-no    GE start-cust-no
      AND oe-prmtx.cust-no    LE end-cust-no
      AND oe-prmtx.custype    GE start-cust-type
      AND oe-prmtx.custype    LE end-cust-type
      AND oe-prmtx.procat     GE start-prod-cat
      AND oe-prmtx.procat     LE end-prod-cat
      AND oe-prmtx.i-no       GE start-item-no
      AND oe-prmtx.i-no       LE end-item-no
/*       ,                                             */
/*     FIRST reftable WHERE                            */
/*           reftable.rec_key  EQ oe-prmtx.rec_key AND */
/*           reftable.company  EQ "oe-prmtx"           */
/*           USE-INDEX rec_key                         */
/*           NO-LOCK                                   */
    :
/*     v-start-i-no = SUBSTR(oe-prmtx.i-no,1,100).              */
/*     IF v-start-i-no GE start-item-no AND                     */
/*        v-start-i-no LE end-item-no THEN DO:                  */
/*         IF AVAIL reftable THEN v-date = DATE(reftable.CODE). */
        IF NOT isLatestEffDate(BUFFER oe-prmtx) THEN DO:
/*             MESSAGE "Delete " oe-prmtx.cust-no reftable.CODE */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.           */
            DELETE oe-prmtx.
        END.
/*     END. */

END.


SESSION:SET-WAIT-STATE("").

MESSAGE trim(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
  ( BUFFER ipb-oe-prmtx FOR oe-prmtx ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF BUFFER lb-oe-prmtx FOR oe-prmtx.
    DEF BUFFER lb-reftable FOR reftable.

    FOR EACH lb-oe-prmtx NO-LOCK
        WHERE lb-oe-prmtx.company    EQ cocode 
            AND lb-oe-prmtx.cust-no    EQ ipb-oe-prmtx.cust-no
            AND lb-oe-prmtx.custype    EQ ipb-oe-prmtx.custype
            AND lb-oe-prmtx.procat     EQ ipb-oe-prmtx.procat
            AND SUBSTR(lb-oe-prmtx.i-no,1,100) 
                EQ SUBSTR(ipb-oe-prmtx.i-no,1,100)
            AND lb-oe-prmtx.rec_key NE ipb-oe-prmtx.rec_key
/*         ,                                                 */
/*         FIRST lb-reftable WHERE                           */
/*           lb-reftable.rec_key  EQ lb-oe-prmtx.rec_key AND */
/*           lb-reftable.company  EQ "oe-prmtx"              */
/*           USE-INDEX rec_key                               */
/*           NO-LOCK                                         */
        :
          IF lb-oe-prmtx.eff-date > ipb-oe-prmtx.eff-date THEN 
            RETURN NO.
    END. /*each price matrix for same item*/

    RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

