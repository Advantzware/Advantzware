&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/glinvdif.w

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

/* Variables */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS scr-invoice-date-from scr-invoice-date-to ~
btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS scr-invoice-date-from scr-invoice-date-to 

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

DEFINE VARIABLE scr-invoice-date-from AS DATE FORMAT "99/99/9999":U 
     LABEL "From Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE scr-invoice-date-to AS DATE FORMAT "99/99/9999":U 
     LABEL "To Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     scr-invoice-date-from AT ROW 3.14 COL 18.6 COLON-ALIGNED
     scr-invoice-date-to AT ROW 4.43 COL 18.6 COLON-ALIGNED
     btnOk AT ROW 12.19 COL 14
     BtnCancel AT ROW 12.19 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.2 BY 12.62.


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
         TITLE              = "A/R Invoice and G/L Amounts not equal"
         HEIGHT             = 12.62
         WIDTH              = 71.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
                                                                        */
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
ON END-ERROR OF C-Win /* A/R Invoice and G/L Amounts not equal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Invoice and G/L Amounts not equal */
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
   DEF VAR v-dscr LIKE gltrans.tr-dscr NO-UNDO.
   DEF VAR lines-per-page AS INT INIT 50 NO-UNDO.
   DEF VAR v-inv-amt AS DEC NO-UNDO.
   DEF VAR v-inv-disc AS DEC NO-UNDO.
   DEF VAR v-cas-cnt AS INT NO-UNDO.
   DEF VAR v-tr-amt AS DEC NO-UNDO.

   {sys/form/r-top.i}

   {sys/inc/print1.i}

   {sys/inc/outprint.i value(lines-per-page)}

   DO WITH FRAME {&FRAME-NAME}:

      SESSION:SET-WAIT-STATE ("general").

      ASSIGN scr-invoice-date-from
             scr-invoice-date-to.

      FOR EACH ar-inv FIELDS(company cust-no inv-no inv-date) WHERE
          ar-inv.company EQ cocode AND
          ar-inv.inv-date GE scr-invoice-date-from AND
          ar-inv.inv-date LE scr-invoice-date-to AND
          ar-inv.posted = YES
          NO-LOCK,
          EACH ar-invl WHERE
               ar-invl.company EQ cocode AND
               ar-invl.inv-no EQ ar-inv.inv-no AND
               ar-invl.misc EQ NO
               NO-LOCK
          BREAK BY ar-invl.inv-no:

          IF FIRST-OF(ar-invl.inv-no) THEN
             ASSIGN
                v-inv-amt = 0
                v-inv-disc = 0.

          v-inv-amt = v-inv-amt + ar-invl.amt.

          IF ar-invl.disc NE 0 THEN
          DO:
             find first uom WHERE
                  uom.uom  eq ar-invl.pr-uom AND
                  uom.mult ne 0
                  no-lock no-error.

             RELEASE oe-ordl.
             RELEASE itemfg.

             IF ar-invl.pr-uom EQ "CS" THEN
             DO:
                find first oe-ordl WHERE
                     oe-ordl.company eq cocode AND
                     oe-ordl.ord-no  eq ar-invl.ord-no AND
                     oe-ordl.i-no    eq ar-invl.i-no
                     NO-LOCK NO-ERROR.

                find first itemfg WHERE
                     itemfg.company EQ cocode AND
                     itemfg.i-no eq ar-invl.i-no
                     NO-LOCK NO-ERROR.

                v-cas-cnt = if ar-invl.cas-cnt ne 0 then
                            ar-invl.cas-cnt
                            else
                            if avail oe-ordl and oe-ordl.cas-cnt ne 0 then
                              oe-ordl.cas-cnt
                            else
                            if avail itemfg and itemfg.case-count ne 0 then
                              itemfg.case-count
                            else 1.
             END.

             IF ar-invl.disc NE 0 THEN 
                ASSIGN
                v-inv-disc = v-inv-disc
                           +  ROUND((IF ar-invl.pr-uom BEGINS "L" AND
                                      ar-invl.pr-uom NE "LB"    THEN
                                     IF ar-invl.inv-qty LT 0 THEN -1 ELSE 1
                                     ELSE
                                     IF ar-invl.pr-uom EQ "CS" THEN
                                        ar-invl.inv-qty / v-cas-cnt
                                     ELSE
                                     IF AVAIL uom THEN
                                        ar-invl.inv-qty / uom.mult
                                     ELSE
                                        ar-invl.inv-qty / 1000) *
                                     ar-invl.unit-pr,2) -
                              ar-invl.amt.
          END.

          IF LAST-OF(ar-invl.inv-no) THEN
          DO:
             FIND FIRST cust WHERE
                  cust.company EQ ar-inv.company AND
                  cust.cust-no EQ ar-inv.cust-no
                  NO-LOCK NO-ERROR.

             ASSIGN
                v-dscr = TRIM(IF AVAIL cust THEN cust.name ELSE "Cust not on file") +
                         " Inv# " + STRING(ar-inv.inv-no,"99999999") + " LINE"
                v-tr-amt = 0.

             FOR EACH gltrans FIELDS(tr-amt tr-dscr trnum) WHERE
                 gltrans.company = cocode AND
                 gltrans.jrnl    = "OEINV" AND
                 gltrans.tr-dscr = v-dscr
                 NO-LOCK
                 BREAK BY gltrans.tr-dscr:

                 v-tr-amt = v-tr-amt + (gltrans.tr-amt * -1).

                 IF LAST-OF(gltrans.tr-dscr) AND v-tr-amt NE v-inv-amt + v-inv-disc THEN
                 DO:
                    DISPLAY gltrans.tr-dscr COLUMN-LABEL "G/L Description"
                            ar-inv.inv-no COLUMN-LABEL "Invoice #"
                            ar-inv.inv-date COLUMN-LABEL "Invoice Date"
                            v-inv-amt + v-inv-disc COLUMN-LABEL "Invoice Amount"
                            gltrans.tr-amt * -1 COLUMN-LABEL "G/L Amount"
                            gltrans.trnum FORMAT "ZZZZ9" COLUMN-LABEL "G/L Run #"
                     with no-box frame frame-a down STREAM-IO width 200.

                    down with frame frame-a.
                 END.
             END.
          END.
      END.

      OUTPUT CLOSE.

      SESSION:SET-WAIT-STATE ("").

      run scr-rpt.w (list-name,c-win:title,11,"P").
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
  DISPLAY scr-invoice-date-from scr-invoice-date-to 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE scr-invoice-date-from scr-invoice-date-to btnOk BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

