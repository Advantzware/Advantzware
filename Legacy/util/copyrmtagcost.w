&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/copyrmtagcost.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = gcompany.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_rm-no end_rm-no begin_tag-no ~
end_tag-no btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_tag-no ~
end_tag-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_tag-no AS CHARACTER FORMAT "X(20)":U 
     LABEL "From Tag#" 
     VIEW-AS FILL-IN 
     SIZE 27.2 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "x(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_tag-no AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "To Tag#" 
     VIEW-AS FILL-IN 
     SIZE 27.2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     begin_rm-no AT ROW 4.19 COL 18 COLON-ALIGNED WIDGET-ID 6
     end_rm-no AT ROW 4.19 COL 47.2 COLON-ALIGNED WIDGET-ID 8
     begin_tag-no AT ROW 5.38 COL 17.8 COLON-ALIGNED WIDGET-ID 2
     end_tag-no AT ROW 6.52 COL 17.8 COLON-ALIGNED WIDGET-ID 10
     btnOk AT ROW 8.05 COL 19.6
     BtnCancel AT ROW 8.05 COL 41.6
     "Does not Copy Blank Tags" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 2.43 COL 21 WIDGET-ID 12
     "Copy RM Receipt Cost to Tag Issues, Transfers, Cycle Counts" VIEW-AS TEXT
          SIZE 63 BY .62 AT ROW 1.48 COL 7 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.2 BY 8.62.


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
         TITLE              = "Copy RM Receipt Cost to Tag Issues"
         HEIGHT             = 8.62
         WIDTH              = 71.2
         MAX-HEIGHT         = 8.62
         MAX-WIDTH          = 71.2
         VIRTUAL-HEIGHT     = 8.62
         VIRTUAL-WIDTH      = 71.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy RM Receipt Cost to Tag Issues */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy RM Receipt Cost to Tag Issues */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
   DEF BUFFER b-rm-rdtlh FOR rm-rdtlh.
   DEF BUFFER b-rm-rcpth FOR rm-rcpth.

   DEF VAR v-count AS INT NO-UNDO.
   DEF VAR v-bwt       LIKE item.basis-w  NO-UNDO.
   DEF VAR v-len       LIKE item.s-len    NO-UNDO.
   DEF VAR v-wid       LIKE item.s-wid    NO-UNDO.
   DEF VAR v-dep       LIKE item.s-dep    NO-UNDO.
   DEF VAR v-cost      LIKE rm-rdtlh.cost NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      SESSION:SET-WAIT-STATE ("general").

      ASSIGN begin_rm-no end_rm-no begin_tag-no end_tag-no.

      FOR EACH rm-rcpth WHERE
          rm-rcpth.company EQ cocode AND
          rm-rcpth.i-no GE begin_rm-no AND
          rm-rcpth.i-no LE end_rm-no AND
          rm-rcpth.rita-code EQ "R"
          NO-LOCK,
          EACH rm-rdtlh WHERE
               rm-rdtlh.r-no eq rm-rcpth.r-no AND
               rm-rdtlh.rita-code eq rm-rcpth.rita-code AND
               rm-rdtlh.tag GE begin_tag-no AND
               rm-rdtlh.tag LE end_tag-no AND
               rm-rdtlh.tag NE ""
               NO-LOCK:

               FOR EACH b-rm-rcpth WHERE
                   b-rm-rcpth.company EQ rm-rcpth.company AND
                   b-rm-rcpth.i-no EQ rm-rcpth.i-no AND
                   lookup(b-rm-rcpth.rita-code,"I,T,C") GT 0
                   NO-LOCK,
                   EACH b-rm-rdtlh WHERE
                        b-rm-rdtlh.r-no EQ b-rm-rcpth.r-no AND
                        b-rm-rdtlh.rita-code EQ b-rm-rcpth.rita-code AND
                        b-rm-rdtlh.tag EQ rm-rdtlh.tag:

                   ASSIGN
                      b-rm-rdtlh.cost = rm-rdtlh.cost.
                      v-count = v-count + 1.

                   IF b-rm-rcpth.rita-code EQ "I" THEN
                   DO:
                      RELEASE job-mat.

                      FIND FIRST job WHERE
                           job.company EQ b-rm-rcpth.company AND
                           job.job-no  EQ b-rm-rcpth.job-no AND
                           job.job-no2 EQ b-rm-rcpth.job-no2
                           NO-LOCK NO-ERROR.

                      FIND FIRST item WHERE
                           item.company  EQ b-rm-rcpth.company AND
                           item.i-no     EQ b-rm-rcpth.i-no
                           NO-LOCK NO-ERROR.

                      IF AVAIL job AND AVAIL ITEM THEN
                      DO:
                         FOR EACH job-mat WHERE
                             job-mat.company  EQ job.company AND
                             job-mat.job      EQ job.job AND
                             job-mat.job-no   EQ job.job-no AND
                             job-mat.job-no2  EQ job.job-no2 AND
                             job-mat.frm      EQ b-rm-rdtlh.s-num AND
                             job-mat.i-no     EQ b-rm-rcpth.i-no
                             NO-LOCK
                             BREAK BY job-mat.blank-no DESC:

                            IF LAST(job-mat.blank-no) OR
                               job-mat.blank-no EQ b-rm-rdtlh.b-num THEN
                               LEAVE.
                         END.

                         IF AVAIL job-mat THEN DO:
                            ASSIGN
                              v-bwt = job-mat.basis-w
                              v-len = job-mat.len
                              v-wid = job-mat.wid
                              v-dep = item.s-dep.

                            IF v-len EQ 0 THEN v-len = item.s-len.

                            IF v-wid EQ 0 THEN
                              v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

                            IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

                            IF item.cons-uom EQ b-rm-rcpth.pur-uom THEN
                              v-cost = b-rm-rdtlh.cost.
                            ELSE
                              RUN sys/ref/convcuom.p(item.cons-uom, b-rm-rcpth.pur-uom,
                                                     v-bwt, v-len, v-wid, v-dep,
                                                     b-rm-rdtlh.cost, OUTPUT v-cost).    

                            FIND FIRST mat-act
                                WHERE mat-act.company   EQ job-mat.company
                                  AND mat-act.mat-date  EQ b-rm-rcpth.post-date
                                  AND mat-act.job       EQ job.job
                                  AND mat-act.job-no    EQ job-mat.job-no
                                  AND mat-act.job-no2   EQ job-mat.job-no2
                                  AND mat-act.s-num     EQ job-mat.frm
                                  AND mat-act.b-num     EQ job-mat.blank-no
                                  AND mat-act.i-no      EQ job-mat.i-no
                                  AND mat-act.rm-i-no   EQ job-mat.i-no
                                  AND mat-act.tag       EQ b-rm-rdtlh.tag
                                  AND mat-act.loc       EQ b-rm-rdtlh.loc
                                  AND mat-act.loc-bin   EQ b-rm-rdtlh.loc-bin
                                NO-ERROR.

                            IF AVAIL mat-act THEN DO:

                               IF b-rm-rcpth.pur-uom NE mat-act.qty-uom THEN
                                  RUN sys/ref/convcuom.p(b-rm-rcpth.pur-uom, mat-act.qty-uom,
                                                         v-bwt, v-len, v-wid, v-dep,
                                                         v-cost, OUTPUT v-cost).

                               ASSIGN
                                  mat-act.cost = v-cost
                                  mat-act.ext-cost = v-cost * mat-act.qty.

                               RELEASE mat-act.
                            END.
                         END.
                      END.
                   END.
               END.

      END.

      MESSAGE v-count "Transaction(s) Updated."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      SESSION:SET-WAIT-STATE ("").
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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

   RUN enable_UI.

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
  DISPLAY begin_rm-no end_rm-no begin_tag-no end_tag-no 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE begin_rm-no end_rm-no begin_tag-no end_tag-no btnOk BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

