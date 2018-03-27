&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: jc\d-jclose.w

  Description: Close Orders at end of FG Posting

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

/* Local Variable Definitions ---                                       */
{sys/inc/var.i SHARED}

def shared workfile w-job
   field job-no   like job.job-no
   field rec-id   as   recid.

DEF TEMP-TABLE w-file NO-UNDO
   FIELD job-no   LIKE job.job-no
   FIELD prod-qty AS   INT
   FIELD rec-id   AS   RECID
   FIELD cloze    AS   LOG.

find first jc-ctrl where jc-ctrl.company eq cocode no-lock no-error.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS close-list open-list btn_open btn_close-all ~
btn_open-all btn_ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS close-list open-list 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_close 
     LABEL "<< &Close" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_close-all 
     LABEL "<< C&lose All" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn_open 
     LABEL "&Open >>" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_open-all 
     LABEL "O&pen All >>" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 115 BY 15.48.

DEFINE VARIABLE close-list AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 46 BY 13.57 NO-UNDO.

DEFINE VARIABLE open-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 46 BY 13.57 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     close-list AT ROW 2.67 COL 3 HELP
          "Jobs to be closed" NO-LABEL
     open-list AT ROW 2.67 COL 68 HELP
          "Jobs to remain open" NO-LABEL
     btn_close AT ROW 4.57 COL 50
     btn_open AT ROW 6.24 COL 50
     btn_close-all AT ROW 11.48 COL 50
     btn_open-all AT ROW 13.14 COL 50
     btn_ok AT ROW 17.19 COL 51
     RECT-1 AT ROW 1.24 COL 1
     "Jobs To Be Closed" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 1.48 COL 15
          FONT 6
     "Jobs To Remain Open" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 1.48 COL 78
          FONT 6
     SPACE(12.19) SKIP(16.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Close Jobs"
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btn_close IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Close Jobs */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "choose" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close D-Dialog
ON CHOOSE OF btn_close IN FRAME D-Dialog /* << Close */
DO:
  RUN close-buttons (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close-all D-Dialog
ON CHOOSE OF btn_close-all IN FRAME D-Dialog /* << Close All */
DO:
  RUN close-buttons (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR v-fin-qty AS INT NO-UNDO.
  DEF VAR v AS INT NO-UNDO.
  DEF VAR li-time AS INT NO-UNDO.
  DEF VAR close_date AS DATE NO-UNDO.

  li-time = TIME.

  FOR EACH w-file,
      FIRST job-hdr NO-LOCK WHERE RECID(job-hdr) EQ w-file.rec-id,
      FIRST job NO-LOCK
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
      USE-INDEX job:


  END.


  v-process = CAN-FIND(FIRST w-file WHERE w-file.cloze EQ YES).

  IF v-process THEN DO:
    v-process = NO.

    MESSAGE "Are you sure you want to close all the selected jobs?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    SESSION:SET-WAIT-STATE("general").

    RELEASE w-file.
    RELEASE job-hdr.
    RELEASE job.

    close_date = TODAY.

    FOR EACH w-file WHERE w-file.cloze EQ YES,
        FIRST job-hdr NO-LOCK WHERE RECID(job-hdr) EQ w-file.rec-id:

      FIND FIRST job NO-LOCK
          WHERE job.company EQ job-hdr.company
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
          USE-INDEX job NO-ERROR.

      IF AVAIL job THEN DO TRANSACTION:

        IF v-process THEN DO:
          FIND CURRENT job EXCLUSIVE.
          {jc/job-clos.i}
        END.

      END.  
    END.

    SESSION:SET-WAIT-STATE("").
        
    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_open D-Dialog
ON CHOOSE OF btn_open IN FRAME D-Dialog /* Open >> */
DO:
  RUN open-buttons (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_open-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_open-all D-Dialog
ON CHOOSE OF btn_open-all IN FRAME D-Dialog /* Open All >> */
DO:
  RUN open-buttons (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-buttons D-Dialog 
PROCEDURE close-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-all AS LOG NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO open-list:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
      IF open-list:IS-SELECTED(i) OR ip-all THEN DO:
        FOR EACH w-file
            WHERE w-file.job-no EQ SUBSTR(open-list:ENTRY(i),1,9):
          w-file.cloze = YES.
        END.
      END.
    END.
  END.

  RUN init-screen.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-buttons D-Dialog 
PROCEDURE enable-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&frame-name}:
    DISABLE btn_close btn_open btn_close-all btn_open-all.

    IF close-list:NUM-ITEMS GT 0 THEN ENABLE btn_open  btn_open-all.
    IF open-list:NUM-ITEMS  GT 0 THEN ENABLE btn_close btn_close-all.
  END.

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
  DISPLAY close-list open-list 
      WITH FRAME D-Dialog.
  ENABLE close-list open-list btn_open btn_close-all btn_open-all btn_ok RECT-1 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-screen D-Dialog 
PROCEDURE init-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN load-list (YES).

  RUN load-list (NO).

  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY close-list open-list.
  END.

  RUN enable-buttons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-list D-Dialog 
PROCEDURE load-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-close AS LOG no-undo.
  DEF VAR v-list AS CHAR NO-UNDO.

  
  v-list = "".

  FOR EACH w-file WHERE w-file.cloze EQ ip-close,

      FIRST job-hdr NO-LOCK
      WHERE RECID(job-hdr) EQ w-file.rec-id
        AND job-hdr.opened EQ YES,

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no
      NO-LOCK    

      BY w-file.job-no
      BY job-hdr.i-no:

     v-list = v-list +
              STRING(w-file.job-no,"x(9)")                             + "  " +
              "ITEM: " + TRIM(job-hdr.i-no)
                 + " " + TRIM(itemfg.i-name)                           + "  " +
              "JOB QTY: " + TRIM(STRING(job-hdr.qty,"->>>>>>>>"))      + "  " +
              "PROD QTY: " + TRIM(STRING(w-file.prod-qty,"->>>>>>>>")) + ",".
  END.

  IF v-list NE "" THEN
    IF SUBSTR(v-list,LENGTH(TRIM(v-list)),1) EQ "," THEN
      SUBSTR(v-list,LENGTH(TRIM(v-list)),1) = "".
  
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-close THEN
      close-list:LIST-ITEMS = v-list.
    ELSE
      open-list:LIST-ITEMS  = v-list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH w-job BREAK BY w-job.job-no:
    IF LAST-OF(w-job.job-no) THEN DO:
      FIND job WHERE RECID(job) EQ w-job.rec-id NO-LOCK NO-ERROR.

      IF AVAIL job AND job.opened THEN
      FOR EACH job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
          NO-LOCK
          BREAK BY job-hdr.job
                BY job-hdr.frm
                BY job-hdr.blank-no:

        CREATE w-file.
        ASSIGN
         w-file.job-no = FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
                         TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99")
         w-file.rec-id = RECID(job-hdr)
         w-file.cloze  = YES.

        FOR EACH fg-act
            WHERE fg-act.company EQ job-hdr.company
              AND fg-act.job     EQ job-hdr.job
              AND fg-act.job-no  EQ job-hdr.job-no
              AND fg-act.job-no2 EQ job-hdr.job-no2
              AND fg-act.i-no    EQ job-hdr.i-no
            NO-LOCK:

          w-file.prod-qty = w-file.prod-qty + fg-act.qty.
        END.
      END.
    END.

    DELETE w-job.
  END.

  RUN init-screen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-buttons D-Dialog 
PROCEDURE open-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-all AS LOG NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO close-list:NUM-ITEMS:
      IF close-list:IS-SELECTED(i) OR ip-all THEN DO:
        FOR EACH w-file
            WHERE w-file.job-no EQ SUBSTR(close-list:ENTRY(i),1,9):
          w-file.cloze = NO.
        END.
      END.
    END.
  END.

  RUN init-screen.

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

