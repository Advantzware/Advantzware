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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 purge_date begin_order end_order ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS purge_date begin_order end_order 

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

DEFINE VARIABLE purge_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Purge Orders Prior To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     purge_date AT ROW 7.19 COL 44 COLON-ALIGNED
     begin_order AT ROW 9.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_order AT ROW 9.57 COL 63 COLON-ALIGNED HELP
          "Enter Ending Order Number"
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
                                                                        */
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
ON END-ERROR OF C-Win /* Purge Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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

  purge_date = today.

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
  DISPLAY purge_date begin_order end_order 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 purge_date begin_order end_order btn-process btn-cancel 
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
/***************************************************** util/cln-ord.p 5/96  */
/****************************************************************************/

def var v-post-date as date init today no-undo.
def var v-first-ord like oe-ord.ord-no no-undo.
def var v-last-ord like oe-ord.ord-no no-undo.

DEF BUFFER b-boll FOR oe-boll.
DEF BUFFER b-rell FOR oe-rell.


session:set-wait-state("General").

do with frame {&frame-name}:
  assign
   purge_date
   begin_order
   end_order.
end.

session:set-wait-state("").

assign
 v-post-date = purge_date
 v-first-ord = begin_order
 v-last-ord  = end_order
 v-process   = no.

message "Are you sure you want to delete the orders within the " +
        "selection parameters?"
        view-as alert-box question button yes-no update v-process.

if v-process then do:
  for each oe-ord
      where oe-ord.company  eq cocode
        and oe-ord.ord-date lt v-post-date
        and oe-ord.ord-no   ge v-first-ord
        and oe-ord.ord-no   le v-last-ord
      EXCLUSIVE transaction:

    for each oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-ord.ord-no:
        delete oe-rel.
    end. /* oe-rell */

    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-ord.company
          AND oe-rell.ord-no  EQ oe-ord.ord-no:

      DELETE oe-rell.

      FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-relh.r-no NO-ERROR.
      IF NOT CAN-FIND(b-rell WHERE b-rell.r-no EQ oe-relh.r-no) THEN
        DELETE oe-relh.
    END. /* oe-rell */

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-ord.company
          AND oe-boll.ord-no  EQ oe-ord.ord-no:

      DELETE oe-boll.

      FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-bolh.b-no NO-ERROR.
      IF NOT CAN-FIND(b-boll WHERE b-boll.b-no EQ oe-bolh.b-no) THEN DELETE oe-bolh.
    END. /* oe-boll */

    for each inv-head
          where inv-head.company eq cocode
            and  inv-head.bol-no eq oe-bolh.bol-no:

        for each inv-line
            where inv-line.company eq cocode
              and inv-line.r-no    eq inv-head.r-no:

          delete inv-line.
        end. /* inv-line */

        for each inv-misc
            where inv-misc.company eq cocode
              and inv-misc.r-no    eq inv-head.r-no:

          delete inv-misc.
        end. /* inv-misc */

        delete inv-head.
    end. /* inv-head */

    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
        exclusive:

      for each job
          where job.company eq cocode
            and job.job-no  eq oe-ordl.job-no
            and job.job-no2 eq oe-ordl.job-no2
          exclusive:

        run jc/jc-dall.p (recid(job)).

        for each job-hdr
            where job-hdr.company eq cocode
              and job-hdr.job     eq job.job
              and job-hdr.job-no  eq job.job-no
              and job-hdr.job-no2 eq job.job-no2
            exclusive:

          {util/dljobkey.i}

          delete job-hdr.
        end.

        for each job-mat
            where job-mat.company eq job.company
              and job-mat.job     eq job.job
              and job-mat.job-no  eq job.job-no
              and job-mat.job-no2 eq job.job-no2
            exclusive:

          delete job-mat.
        end.

        for each job-mch
            where job-mch.company eq job.company
              and job-mch.job     eq job.job
              and job-mch.job-no  eq job.job-no
              and job-mch.job-no2 eq job.job-no2
            exclusive:

          delete job-mch.
        end.

        for each job-prep
            where job-prep.company eq job.company
              and job-prep.job     eq job.job
              and job-prep.job-no  eq job.job-no
              and job-prep.job-no2 eq job.job-no2
            exclusive:

          delete job-prep.
        end.

        FOR EACH job-farm
            WHERE job-farm.company EQ job.company
              AND job-farm.job-no  EQ job.job-no
              AND job-farm.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm.
        END.

        FOR EACH job-farm-rctd
            WHERE job-farm-rctd.company EQ job.company
              AND job-farm-rctd.job-no  EQ job.job-no
              AND job-farm-rctd.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm-rctd.
        END.

        for each pc-prdd
            where pc-prdd.company eq cocode
              and pc-prdd.job     eq job.job
              and pc-prdd.job-no  eq job.job-no
              and pc-prdd.job-no2 eq job.job-no2
            exclusive:

          delete pc-prdd.
        end.

        for each fg-act
            where fg-act.company eq cocode
              and fg-act.job     eq job.job
              and fg-act.job-no  eq job.job-no
              and fg-act.job-no2 eq job.job-no2
            exclusive:

          delete fg-act.
        end.

        for each mat-act
            where mat-act.company eq cocode
              and mat-act.job     eq job.job
              and mat-act.job-no  eq job.job-no
              and mat-act.job-no2 eq job.job-no2
            exclusive:

          delete mat-act.
        end.

        for each mch-act
            where mch-act.company eq cocode
              and mch-act.job     eq job.job
              and mch-act.job-no  eq job.job-no
              and mch-act.job-no2 eq job.job-no2
            exclusive:

          delete mch-act.
        end.

        for each misc-act
            where misc-act.company eq cocode
              and misc-act.job     eq job.job
              and misc-act.job-no  eq job.job-no
              and misc-act.job-no2 eq job.job-no2
            exclusive:

          delete misc-act.
        end.

        for each fg-bin
            where fg-bin.company    eq cocode
              and fg-bin.job-no     eq job.job-no
              and fg-bin.job-no2    eq job.job-no2
              and trim(fg-bin.i-no) ne ""    
              and fg-bin.qty        eq 0
            exclusive:

          delete fg-bin.
        end.

        if job.exported then do:
          job.stat = "X".
          run jc/kiwiexp2.p (recid(job)).
        end.

        delete job.
      end.

      delete oe-ordl.
    end.

    delete oe-ord.
  end. /* oe-ord */

  message trim(c-win:title) + " Process Is Completed." view-as alert-box.
  apply "close" to this-procedure.
end.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

