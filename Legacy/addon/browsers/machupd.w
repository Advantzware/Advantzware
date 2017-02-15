&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: addon/browsers/machupd.w
  
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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR correct-error AS LOG NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS scr-machine scr-job-no scr-job-no-2 ~
scr-sheet scr-blank scr-pass scr-charge scr-start-date start_hour ~
start_minute start_ampm scr-end-date end_hour end_minute end_ampm scr-shift ~
scr-run-qty btn-ok btn-cancel btn-delete 
&Scoped-Define DISPLAYED-OBJECTS scr-machine scr-job-no scr-job-no-2 ~
scr-sheet scr-blank scr-pass scr-charge scr-start-date start_hour ~
start_minute start_ampm scr-end-date end_hour end_minute end_ampm scr-shift ~
scr-run-qty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-5 start_hour start_minute start_ampm end_hour ~
end_minute end_ampm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete AUTO-GO 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE end_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE scr-blank AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Blank" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE scr-charge AS CHARACTER FORMAT "X(5)":U 
     LABEL "Charge" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE scr-end-date AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE scr-job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-job-no-2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE scr-machine AS CHARACTER FORMAT "X(8)":U 
     LABEL "Machine" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-pass AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Pass" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-run-qty AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Run Qty" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-sheet AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Sheet" 
     VIEW-AS FILL-IN 
     SIZE 6.8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-shift AS CHARACTER FORMAT "X(2)":U 
     LABEL "Shift" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-start-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE start_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     scr-machine AT ROW 2 COL 14 COLON-ALIGNED WIDGET-ID 2
     scr-job-no AT ROW 3.19 COL 14 COLON-ALIGNED WIDGET-ID 4
     scr-job-no-2 AT ROW 3.19 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     scr-sheet AT ROW 4.43 COL 14.2 COLON-ALIGNED WIDGET-ID 8
     scr-blank AT ROW 4.43 COL 29 COLON-ALIGNED WIDGET-ID 12
     scr-pass AT ROW 4.43 COL 44 COLON-ALIGNED WIDGET-ID 14
     scr-charge AT ROW 6 COL 14 COLON-ALIGNED WIDGET-ID 16
     scr-start-date AT ROW 7.19 COL 14 COLON-ALIGNED WIDGET-ID 18
     start_hour AT ROW 7.19 COL 44.2 COLON-ALIGNED HELP
          "Enter Starting Hour" WIDGET-ID 36
     start_minute AT ROW 7.19 COL 50.2 COLON-ALIGNED HELP
          "Enter Starting Minute" WIDGET-ID 38
     start_ampm AT ROW 7.19 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     scr-end-date AT ROW 8.33 COL 14 COLON-ALIGNED WIDGET-ID 20
     end_hour AT ROW 8.33 COL 44.2 COLON-ALIGNED HELP
          "Enter Ending Hour" WIDGET-ID 28
     end_minute AT ROW 8.33 COL 50.2 COLON-ALIGNED HELP
          "Enter Ending Minute" WIDGET-ID 30
     end_ampm AT ROW 8.33 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     scr-shift AT ROW 9.57 COL 14 COLON-ALIGNED WIDGET-ID 22
     scr-run-qty AT ROW 10.71 COL 14 COLON-ALIGNED WIDGET-ID 46
     btn-ok AT ROW 12.43 COL 16 WIDGET-ID 42
     btn-cancel AT ROW 12.43 COL 32 WIDGET-ID 44
     btn-delete AT ROW 12.43 COL 60 WIDGET-ID 48
     SPACE(3.59) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE ""
         DEFAULT-BUTTON btn-ok CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX end_ampm IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR FILL-IN end_hour IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR FILL-IN end_minute IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR COMBO-BOX start_ampm IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR FILL-IN start_hour IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR FILL-IN start_minute IN FRAME D-Dialog
   5                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete D-Dialog
ON CHOOSE OF btn-delete IN FRAME D-Dialog /* Delete */
DO:
   DEF VAR m_response AS LOG NO-UNDO.

   MESSAGE "Do you wish to delete this transaction?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE m_response.

   IF m_response THEN
   DO:
      FIND FIRST machtran WHERE
           ROWID(machtran) EQ ip-rowid.

      DELETE machtran.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok D-Dialog
ON CHOOSE OF btn-ok IN FRAME D-Dialog /* OK */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
      FIND FIRST machtran WHERE
           ROWID(machtran) EQ ip-rowid.
      
      ASSIGN
         scr-machine
         scr-job-no
         scr-job-no-2
         scr-sheet
         scr-blank
         scr-pass
         scr-charge
         scr-start-date
         scr-end-date
         scr-shift
         scr-run-qty
         start_hour
         start_minute
         start_ampm
         end_hour
         end_minute
         end_ampm
         machtran.machine = scr-machine
         machtran.job_number = FILL(" ",6 - LENGTH(scr-job-no)) + scr-job-no
         machtran.job_sub = scr-job-no-2
         machtran.form_number = scr-sheet
         machtran.blank_number = scr-blank
         machtran.pass_sequence = scr-pass
         machtran.charge_code = scr-charge
         machtran.start_date = scr-start-date
         machtran.end_date = scr-end-date
         machtran.shift = scr-shift
         machtran.run_qty = scr-run-qty.
     
      {custom/set_time.i
        &field="machtran.start_time"
        &hour="start_hour"
        &minute="start_minute"
        &ampm="start_ampm"}
        
      {custom/set_time.i
        &field="machtran.end_time"
        &hour="end_hour"
        &minute="end_minute"
        &ampm="end_ampm"}
        
      {custom/calctime.i &file="machtran"}

      RELEASE machtran.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_hour D-Dialog
ON LEAVE OF end_hour IN FRAME D-Dialog /* End Time */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  IF correct-error THEN
  DO:
     MESSAGE "Invalid Hour, range = 0 to 12"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY" TO end_hour IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_minute D-Dialog
ON LEAVE OF end_minute IN FRAME D-Dialog
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  IF correct-error THEN
  DO:
     MESSAGE "Invalid Minute, range = 0 to 59"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY" TO end_minute IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_hour D-Dialog
ON LEAVE OF start_hour IN FRAME D-Dialog /* Start Time */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.

  IF correct-error THEN
  DO:
     MESSAGE "Invalid Hour, range = 0 to 12"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY" TO start_hour IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_minute D-Dialog
ON LEAVE OF start_minute IN FRAME D-Dialog
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  IF correct-error THEN
  DO:
     MESSAGE "Invalid Minute, range = 0 to 59"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY" TO start_minute IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FIND FIRST machtran WHERE
     ROWID(machtran) EQ ip-rowid
     NO-LOCK.

ASSIGN
   scr-machine = machtran.machine
   scr-job-no = machtran.job_number
   scr-job-no-2 = machtran.job_sub
   scr-sheet = machtran.form_number
   scr-blank = machtran.blank_number
   scr-pass = machtran.pass_sequence
   scr-charge = machtran.charge_code
   scr-start-date = machtran.start_date
   scr-end-date = machtran.end_date
   scr-shift = machtran.shift
   scr-run-qty = machtran.run_qty.

{custom/get_time.i
      &field="machtran.start_time"
      &hour="start_hour"
      &minute="start_minute"
      &ampm="start_ampm"}
      
{custom/get_time.i
      &field="machtran.end_time"
      &hour="end_hour"
      &minute="end_minute"
      &ampm="end_ampm"}

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY scr-machine scr-job-no scr-job-no-2 scr-sheet scr-blank scr-pass 
          scr-charge scr-start-date start_hour start_minute start_ampm 
          scr-end-date end_hour end_minute end_ampm scr-shift scr-run-qty 
      WITH FRAME D-Dialog.
  ENABLE scr-machine scr-job-no scr-job-no-2 scr-sheet scr-blank scr-pass 
         scr-charge scr-start-date start_hour start_minute start_ampm 
         scr-end-date end_hour end_minute end_ampm scr-shift scr-run-qty btn-ok 
         btn-cancel btn-delete 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

