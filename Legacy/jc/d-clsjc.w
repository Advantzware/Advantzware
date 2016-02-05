&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: po\d-clspo.w

  Description: Close/Reopen POs

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
{sys/inc/var.i SHARED}
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
DEF VAR ll-close AS LOG NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
/*   period_pos = INDEX(PROGRAM-NAME(1),".")                                             */
/*   v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 10) + 1) */
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

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
&Scoped-Define ENABLED-OBJECTS RECT-1 begin_job begin_job2 end_job end_job2 ~
begin_ord end_ord begin_date end_date tb_only close_date btn_ok 
&Scoped-Define DISPLAYED-OBJECTS begin_job begin_job2 end_job end_job2 ~
begin_ord end_ord begin_date end_date tb_only close_date fi_status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Job Start Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE close_date AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Close Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Job Start Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(6)":U 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT "99":U INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.05.

DEFINE VARIABLE tb_only AS LOGICAL INITIAL yes 
     LABEL "Close only those jobs with WIP posted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_job AT ROW 2.19 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Job#"
     begin_job2 AT ROW 2.19 COL 41 COLON-ALIGNED HELP
          "Enter Beginning Job#"
     end_job AT ROW 2.19 COL 73 COLON-ALIGNED HELP
          "Enter Endng Job#"
     end_job2 AT ROW 2.19 COL 85 COLON-ALIGNED HELP
          "Enter Ending Job#"
     begin_ord AT ROW 4.1 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Order#"
     end_ord AT ROW 4.1 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order#"
     begin_date AT ROW 5.76 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Order#"
     end_date AT ROW 5.76 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order#"
     tb_only AT ROW 7.52 COL 29
     close_date AT ROW 8.81 COL 29 COLON-ALIGNED HELP
          "Enter Close Date" WIDGET-ID 2
     fi_status AT ROW 10.48 COL 1 COLON-ALIGNED NO-LABEL
     btn_ok AT ROW 10.48 COL 42
     RECT-1 AT ROW 1.24 COL 1
     SPACE(0.00) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Close Orders"
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
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

/* SETTINGS FOR FILL-IN fi_status IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Close Orders */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date D-Dialog
ON LEAVE OF begin_date IN FRAME D-Dialog /* Beginning Job Start Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job D-Dialog
ON LEAVE OF begin_job IN FRAME D-Dialog /* Beginning Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 D-Dialog
ON LEAVE OF begin_job2 IN FRAME D-Dialog /* - */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord D-Dialog
ON LEAVE OF begin_ord IN FRAME D-Dialog /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR fstat LIKE job.stat NO-UNDO.
  DEF VAR tstat LIKE fstat INIT "" NO-UNDO.
  DEF VAR v-fin-qty AS INT NO-UNDO.

  DISABLE TRIGGERS FOR LOAD OF job.
  DISABLE TRIGGERS FOR LOAD OF job-hdr.
  DISABLE TRIGGERS FOR LOAD OF itemfg.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " +
          TRIM(STRING(ll-close,"close/reopen")) +
          " the selected Jobs?"
     VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
  
  IF v-process THEN DO WITH FRAME {&FRAME-NAME}:
    SESSION:SET-WAIT-STATE("general").

    ASSIGN
     begin_job = FILL(" ",6 - LENGTH(TRIM(begin_job))) +
                 TRIM(begin_job) + STRING(begin_job2,"99")
     end_job   = FILL(" ",6 - LENGTH(TRIM(end_job))) +
                 TRIM(end_job) + STRING(end_job2,"99").

    IF ll-close THEN
    FOR EACH job
        WHERE job.company                               EQ cocode
          AND job.opened                                EQ ll-close

          AND (job.stat                                 EQ "W" OR
               NOT tb_only                                     OR
               CAN-FIND(FIRST mat-act
                        WHERE mat-act.company EQ job.company
                          AND mat-act.job     EQ job.job
                          AND mat-act.job-no  EQ job.job-no
                          AND mat-act.job-no2 EQ job.job-no2)  OR
               CAN-FIND(FIRST mch-act
                        WHERE mch-act.company EQ job.company
                          AND mch-act.job     EQ job.job
                          AND mch-act.job-no  EQ job.job-no
                          AND mch-act.job-no2 EQ job.job-no2)  OR
               CAN-FIND(FIRST misc-act
                        WHERE misc-act.company EQ job.company
                          AND misc-act.job     EQ job.job
                          AND misc-act.job-no  EQ job.job-no
                          AND misc-act.job-no2 EQ job.job-no2))

          AND job.job-no                                GE SUBSTR(begin_job,1,6)
          AND job.job-no                                LE SUBSTR(end_job,1,6)

          AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
              TRIM(job.job-no) +
              STRING(job.job-no2,"99")                  GE begin_job
          AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
              TRIM(job.job-no) +
              STRING(job.job-no2,"99")                  LE end_job

          AND job.start-date                            GE begin_date
          AND job.start-date                            LE end_date

        USE-INDEX opened,

      {jc/jc-close.i}

        FOR EACH rm-rctd WHERE rm-rctd.company = job.company
              AND rm-rctd.job-no = FILL(" ",6 - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no)  
              AND rm-rctd.job-no2 = job.job-no2
              AND rm-rctd.rita-code = "I" EXCLUSIVE-LOCK:
             ASSIGN
                rm-rctd.rct-date = close_date .
          END.
  
      DISPLAY "Job Closing: " +
              TRIM(job.job-no) + "-" +
              STRING(job.job-no2,"99") FORMAT "x(30)" @ fi_status
        WITH FRAME {&FRAME-NAME}.
    END.

    ELSE
    FOR EACH job NO-LOCK
        WHERE job.company                               EQ cocode
          AND job.opened                                EQ ll-close

          AND job.job-no                                GE SUBSTR(begin_job,1,6)
          AND job.job-no                                LE SUBSTR(end_job,1,6)

          AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
              TRIM(job.job-no) +
              STRING(job.job-no2,"99")                  GE begin_job
              AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
              TRIM(job.job-no) +
              STRING(job.job-no2,"99")                  LE end_job

          AND job.start-date                            GE begin_date
          AND job.start-date                            LE end_date

        USE-INDEX opened:

      RUN jc/jc-reopn.p (ROWID(job)).

      DISPLAY "Job Opening: " +
              TRIM(job.job-no) + "-" +
              STRING(job.job-no2,"99") FORMAT "x(30)" @ fi_status
        WITH FRAME {&FRAME-NAME}.
    END.
    
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
     
    DISPLAY "" @ fi_status WITH FRAME {&FRAME-NAME}.

    SESSION:SET-WAIT-STATE("").
        
    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME close_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL close_date D-Dialog
ON LEAVE OF close_date IN FRAME D-Dialog /* Close Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date D-Dialog
ON LEAVE OF end_date IN FRAME D-Dialog /* Ending Job Start Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job D-Dialog
ON LEAVE OF end_job IN FRAME D-Dialog /* Ending Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 D-Dialog
ON LEAVE OF end_job2 IN FRAME D-Dialog /* - */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord D-Dialog
ON LEAVE OF end_ord IN FRAME D-Dialog /* Ending Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_only D-Dialog
ON VALUE-CHANGED OF tb_only IN FRAME D-Dialog /* Close only those jobs with WIP posted? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL job THEN RETURN.

RUN enable_UI.

DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
        ASSIGN
        begin_ord:SCREEN-VALUE  =  ""
        end_ord:SCREEN-VALUE    = "99999999" .

ASSIGN
 begin_job:SCREEN-VALUE  = job.job-no
 end_job:SCREEN-VALUE    = job.job-no
 begin_job2:SCREEN-VALUE = string(job.job-no2)
 end_job2:SCREEN-VALUE   = string(job.job-no2)
 begin_date:SCREEN-VALUE = string(job.start-date)
 end_date:SCREEN-VALUE   = string(job.start-date)
 ll-close   = job.opened
 close_date:SCREEN-VALUE = string(TODAY)

 FRAME {&FRAME-NAME}:TITLE = (IF ll-close THEN "Close" ELSE "Reopen") + " Jobs".
END.

ASSIGN
 begin_job  = job.job-no
 end_job    = job.job-no
 begin_job2 = job.job-no2
 end_job2   = job.job-no2
 begin_date = job.start-date
 end_date   = job.start-date
 ll-close   = job.opened
 close_date = TODAY

 FRAME {&FRAME-NAME}:TITLE = (IF ll-close THEN "Close" ELSE "Reopen") + " Jobs".

    {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

 /* {src/adm/template/dialogmn.i}*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  DISPLAY begin_job begin_job2 end_job end_job2 begin_ord end_ord begin_date 
          end_date tb_only close_date fi_status 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 begin_job begin_job2 end_job end_job2 begin_ord end_ord 
         begin_date end_date tb_only close_date btn_ok 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-close THEN DISABLE tb_only.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

