&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-job-no AS cha NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{cecrep/jc-prem.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-bundle v-unit v-pattern v-pallet Btn_OK ~
Btn_Cancel RECT-40 RECT-41 
&Scoped-Define DISPLAYED-OBJECTS v-bundle v-unit v-pattern v-pallet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-bundle v-unit v-pattern v-pallet 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-bundle AS CHARACTER FORMAT "X(20)":U 
     LABEL "Set Qty / Bundle" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE v-pallet AS CHARACTER FORMAT "X(20)":U 
     LABEL "Set Pallet Code" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE v-pattern AS CHARACTER FORMAT "X(20)":U 
     LABEL "Set Pattern" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE v-unit AS CHARACTER FORMAT "X(20)":U 
     LABEL "Set Qty / Unit" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 5.48.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-bundle AT ROW 1.71 COL 26 COLON-ALIGNED
     v-unit AT ROW 2.67 COL 26 COLON-ALIGNED
     v-pattern AT ROW 3.62 COL 26 COLON-ALIGNED
     v-pallet AT ROW 4.57 COL 26 COLON-ALIGNED
     Btn_OK AT ROW 7.67 COL 15
     Btn_Cancel AT ROW 7.67 COL 45
     RECT-40 AT ROW 1.48 COL 3
     RECT-41 AT ROW 1 COL 1
     SPACE(0.00) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Set FG Item Unitization Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-bundle IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-pallet IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-pattern IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-unit IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Set FG Item Unitization Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    FIND FIRST tt-prem NO-ERROR.
    IF NOT AVAIL tt-prem THEN CREATE tt-prem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    FIND FIRST tt-prem
      WHERE tt-prem.tt-job-no  EQ ip-job-no
        AND tt-prem.tt-job-no2 EQ ip-job-no2
     /*  AND tt-prem.tt-frm     EQ ip-frm*/
        NO-ERROR.
    IF NOT AVAIL tt-prem THEN CREATE tt-prem.

    ASSIGN {&list-1}.

    ASSIGN  tt-prem.tt-job-no = ip-job-no
            tt-prem.tt-job-no2 = ip-job-no2
            /*tt-prem.tt-frm = ip-frm*/
            tt-prem.tt-#-bundle = v-bundle
            tt-prem.tt-#-unit = v-unit
            tt-prem.tt-pattern = v-pattern
            tt-prem.tt-pallet = v-pallet
            .
    RUN get-upd-reft (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ ip-job-no
        AND job.job-no2 EQ ip-job-no2
        AND job.est-no  NE ""
      NO-LOCK,
      FIRST eb
      WHERE eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        AND eb.form-no EQ 0
        AND eb.cas-no  NE ""
      NO-LOCK:
    LEAVE.
  END.

  RUN get-upd-reft (YES).

  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " for Job#/Form: " +
                              TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99") /*+
                              "/" + TRIM(STRING(ip-frm,">>>>"))*/.

  RUN enable_UI.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY v-bundle v-unit v-pattern v-pallet 
      WITH FRAME Dialog-Frame.
  ENABLE v-bundle v-unit v-pattern v-pallet Btn_OK Btn_Cancel RECT-40 RECT-41 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-upd-reft Dialog-Frame 
PROCEDURE get-upd-reft :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-get AS LOG NO-UNDO.

  DEF VAR li AS INT NO-UNDO.


  li = 0.

  DO li = 1 TO 4:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "cerep/d-prem.w"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ TRIM(ip-job-no)
          AND reftable.code     EQ STRING(ip-job-no2,"9999999999")
          AND reftable.code2    EQ ""
          AND reftable.val[1]   EQ li
          /*AND (reftable.val[2]  EQ ip-frm OR reftable.val[2] EQ 0)*/
        NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "cerep/d-prem.w"
       reftable.company  = cocode
       reftable.loc      = TRIM(ip-job-no)
       reftable.code     = STRING(ip-job-no2,"9999999999")
       reftable.code2    = ""
       reftable.val[1]   = li.
    END.

    CASE li:
      WHEN 01 THEN
        IF ip-get THEN v-bundle  = IF AVAIL eb AND reftable.dscr EQ "" THEN
                                     STRING(eb.cas-cnt) ELSE reftable.dscr.
                                   ELSE reftable.dscr = v-bundle.
      WHEN 02 THEN
        IF ip-get THEN v-unit    = IF AVAIL eb AND reftable.dscr EQ "" THEN
                                     STRING(eb.tr-cnt) ELSE reftable.dscr.
                                   ELSE reftable.dscr = v-unit.
      WHEN 03 THEN
        IF ip-get THEN v-pattern = IF AVAIL eb AND reftable.dscr EQ "" THEN
                                     eb.stack-code ELSE reftable.dscr.
                                   ELSE reftable.dscr = v-pattern.
      WHEN 04 THEN
        IF ip-get THEN v-pallet  = IF AVAIL eb AND reftable.dscr EQ "" THEN
                                     eb.tr-no ELSE reftable.dscr.
                                   ELSE reftable.dscr = v-pallet.
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

