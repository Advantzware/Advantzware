&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
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

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions --- */
DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE tt-rec FIELD selekt AS LOG FORMAT "yes/no" LABEL "Selected"
                      FIELD row-id AS ROWID
                      .

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-rec est

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-rec.trans-date tt-rec.qty-rec tt-rec.selekt tt-rec.qty-inv   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-rec.selekt tt-rec.qty-inv   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-rec NO-LOCK, ~
                                   FIRST est WHERE ROWID(est) EQ tt-rec.row-id NO-LOCK                             BY est.est-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-rec NO-LOCK, ~
                                   FIRST est WHERE ROWID(est) EQ tt-rec.row-id NO-LOCK                             BY est.est-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-rec est
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 est


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-rec, 
      est SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-rec.trans-date tt-rec.qty-rec tt-rec.selekt tt-rec.qty-inv
     ENABLE tt-rec.selekt tt-rec.qty-inv
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 12.38
         BGCOLOR 8  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 1.24 COL 2
     Btn_OK AT ROW 6.71 COL 54
     SPACE(1.99) SKIP(5.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Group Board Cost".


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
                                                                        */
/* BROWSE-TAB BROWSE-2 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rec NO-LOCK,
                            FIRST est WHERE ROWID(est) EQ tt-rec.row-id NO-LOCK
                            BY est.est-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Group Board Cost */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  FOR EACH reftable
      WHERE reftable.reftable EQ "est/d-grpcst.w"
        AND reftable.company  EQ est.company
        AND reftable.loc      EQ est.loc
        AND reftable.code     EQ est.est-no:
    DELETE reftable.
  END.

  FOR EACH reftable
      WHERE reftable.reftable EQ "est/d-grpcst.w"
        AND reftable.company  EQ est.company
        AND reftable.loc      EQ est.loc
        AND reftable.code2    EQ est.est-no:
    DELETE reftable.
  END.

  FOR EACH tt-rec WHERE selekt EQ YES,
      :

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


ON 'mouse-select-click':U OF tt-rec.selekt
DO:
  tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} =
      STRING(NOT (tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} EQ "yes"),"yes/no").
END.

/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  li = 0.

  FIND est WHERE ROWID(est) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL est THEN DO:
    FIND FIRST eb OF est WHERE eb.form-no EQ 0 NO-LOCK NO-ERROR.

    IF NOT AVAIL eb THEN
    FIND FIRST eb OF est NO-LOCK NO-ERROR.
  END.

  IF AVAIL eb THEN DO:
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " " +
                                "Estimate/Part#: " +
                                TRIM(eb.est-no) + "/" +
                                TRIM(eb.part-no).

    RUN build-table.
  END.

  IF li GT 0 THEN DO WITH FRAME {&FRAME-NAME}:
    RUN enable_UI.
  
    WAIT-FOR GO OF FRAME.
  END.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR b-eb.
  DEF BUFFER b-ef FOR b-ef.


  FOR EACH tt-rec:
    DELETE tt-rec.
  END.

  FOR EACH ef OF est NO-LOCK,
      EACH eb OF ef  NO-LOCK:

    
  END.

  FOR EACH tt-rec BREAK BY tt-rec.row-id:
    IF LAST-OF(tt-rec.row-id) THEN
      ASSIGN
       li             = li + 1
       tt-rec.qty-rec = ld[1]
       tt-rec.qty-inv = ld[2]
       ld             = 0.

    ELSE DELETE tt-rec.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE BROWSE-2 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

