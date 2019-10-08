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

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF BUFFER b-item FOR item.
DEF BUFFER b-reftable FOR reftable.

DEF VAR v-height AS DEC FORMAT ">>>9.9<<<" INIT 5 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_flute end_flute begin_test end_test ~
begin_pallet end_pallet fi_height tb_delete btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_flute end_flute begin_test end_test ~
begin_pallet end_pallet fi_height lbl_delete tb_delete 

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

DEFINE VARIABLE begin_flute AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Flute" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_pallet AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_test AS CHARACTER FORMAT "x(6)":U 
     LABEL "Beginning Test" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_flute AS CHARACTER FORMAT "x(3)":U INITIAL "zzz" 
     LABEL "Ending Flute" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_pallet AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_test AS CHARACTER FORMAT "x(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Test" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_height AS DECIMAL FORMAT ">>>9.9<<<":U INITIAL 5 
     LABEL "Pallet Height" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_delete AS CHARACTER FORMAT "X(256)":U INITIAL "Delete Existing?" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 10.48.

DEFINE VARIABLE tb_delete AS LOGICAL INITIAL no 
     LABEL "Delete Existing?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_flute AT ROW 7.19 COL 28 COLON-ALIGNED
     end_flute AT ROW 7.19 COL 62 COLON-ALIGNED
     begin_test AT ROW 8.38 COL 28 COLON-ALIGNED
     end_test AT ROW 8.38 COL 62 COLON-ALIGNED
     begin_pallet AT ROW 9.57 COL 28 COLON-ALIGNED
     end_pallet AT ROW 9.57 COL 62 COLON-ALIGNED
     fi_height AT ROW 11.24 COL 45 COLON-ALIGNED
     lbl_delete AT ROW 12.43 COL 28 COLON-ALIGNED NO-LABEL
     tb_delete AT ROW 12.43 COL 47
     btn-process AT ROW 16.24 COL 21
     btn-cancel AT ROW 16.24 COL 53
     RECT-17 AT ROW 4.81 COL 1
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
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
         TITLE              = "Build Flute/Test Counts"
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lbl_delete IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Build Flute/Test Counts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Build Flute/Test Counts */
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
    run run-process.
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

  FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.
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
  DISPLAY begin_flute end_flute begin_test end_test begin_pallet end_pallet 
          fi_height lbl_delete tb_delete 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_flute end_flute begin_test end_test begin_pallet end_pallet 
         fi_height tb_delete btn-process btn-cancel RECT-17 
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
/* ------------------------------------------------ util/bldstack.p 08/99 JLF */
/*  Build Stacking Matrices                                                   */
/* -------------------------------------------------------------------------- */

def var fflute      like item.flute         format "x(3)".
def var tflute      like fflute             init "zzz".
def var ftest       like item.reg-no        format "x(6)".
def var ttest       like ftest              init "zzzzzz".
def var fpall       like item.i-no          format "x(10)".
def var tpall       like fpall              init "zzzzzzzzzz".
def var v-delete    as   log                format "yes/no".

DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-page-no LIKE stack-flute.page-no NO-UNDO.

session:set-wait-state("General").

do with frame {&frame-name}:
  assign
   begin_flute
   end_flute
   begin_test
   end_test
   begin_pallet
   end_pallet
   fi_height
   tb_delete.
end.

assign
 fflute   = begin_flute
 tflute   = end_flute
 ftest    = begin_test
 ttest    = end_test
 fpall    = begin_pallet
 tpall    = end_pallet
 v-height = fi_height
 v-delete = tb_delete.

IF v-delete THEN
FOR EACH stack-flute
    WHERE stack-flute.company EQ cocode
      AND stack-flute.loc     EQ locode
      AND stack-flute.code    GE fflute
      AND stack-flute.code    LE tflute
      AND stack-flute.pallet  GE fpall
      AND stack-flute.pallet  LE tpall:
  DELETE stack-flute.
END.

FOR EACH item
    WHERE item.company  EQ cocode
      AND item.mat-type EQ "B"
      AND item.flute    GE fflute
      AND item.flute    LE tflute
      AND item.flute    NE ""
      AND item.reg-no   GE ftest
      AND item.reg-no   LE ttest
      AND item.reg-no   NE ""
      AND item.cal      NE 0
    USE-INDEX mat-type NO-LOCK,

    FIRST flute
    WHERE flute.company eq cocode
      AND flute.code    eq item.flute
    NO-LOCK,

    EACH b-item
    WHERE b-item.company  EQ cocode
      AND b-item.mat-type EQ "D"
      AND b-item.i-no     GE fpall
      AND b-item.i-no     LE tpall
    USE-INDEX mat-type NO-LOCK

    BREAK BY item.flute
          BY b-item.i-no
          BY item.reg-no:

  IF FIRST-OF(item.reg-no) THEN DO:
    STATUS DEFAULT " Processing...    Flute: " + TRIM(item.flute) +
                                    "  Pallet: " + TRIM(b-item.i-no) +
                                    "  Test: " + TRIM(ITEM.reg-no).

    ASSIGN
     lv-page-no = 0
     ll         = NO.

    FOR EACH stack-flute
        WHERE stack-flute.company EQ cocode
          AND stack-flute.loc     EQ locode
          AND stack-flute.code    EQ flute.code
          AND stack-flute.pallet  EQ b-item.i-no
        BY stack-flute.page-no:

      lv-page-no = stack-flute.page-no.

      RUN update-stack-flute (OUTPUT ll).

      IF ll THEN LEAVE.
    END.

    IF NOT ll THEN DO:
      {sys/ref/stack-fl.a}

      ASSIGN
       stack-flute.pallet  = b-item.i-no
       stack-flute.page-no = lv-page-no + 1.

      RUN update-stack-flute (OUTPUT ll).
    END.
  END.
END.

session:set-wait-state("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.
apply "close" to this-procedure.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-stack-flute C-Win 
PROCEDURE update-stack-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-updated AS LOG NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.


  DO li = 1 TO 15:
    IF stack-flute.row-value[li] EQ ""          OR
       stack-flute.row-value[li] EQ item.reg-no THEN DO:

      stack-flute.row-value[li] = item.reg-no.

      DO lj = 1 TO 10:
        IF stack-flute.col-value[lj]        NE "" AND
           stack-flute.vals[(li * 10) + lj] EQ 0  THEN DO:

          FIND FIRST stackPattern
              WHERE stackPattern.stackCode     EQ stack-flute.col-value[lj]
              NO-LOCK NO-ERROR.
          IF AVAIL stackPattern THEN
            ASSIGN
             stack-flute.vals[(li * 10) + lj] = (b-item.case-d - v-height) / (item.cal * 2)
             stack-flute.vals[(li * 10) + lj] = (stack-flute.vals[(li * 10) + lj] -
                                                 stack-flute.vals[(li * 10) + lj] MODULO 5) *
                                                stackPattern.stackCount.
        END.
      END.

      op-updated = YES.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

