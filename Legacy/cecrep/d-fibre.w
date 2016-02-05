&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
DEF INPUT PARAM ip-ref-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{cecrep/jc-fibre.i}

DEF BUFFER job-reft FOR reftable.
DEF BUFFER d-fibre FOR reftable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-mat

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH job-mat SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH job-mat SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame job-mat
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame job-mat


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-sqty-1 v-sqty-2 v-sqty-3 v-sqty-4 ~
v-order-1 v-oqty-1 v-order-2 v-oqty-2 v-order-3 v-oqty-3 v-order-4 v-oqty-4 ~
Btn_OK Btn_Cancel RECT-43 RECT-44 RECT-45 
&Scoped-Define DISPLAYED-OBJECTS v-itemfg fi_job-no v-form-no v-blank-no ~
v-sqty-1 v-sqty-2 v-sqty-3 v-sqty-4 v-order-1 v-oqty-1 v-order-2 v-oqty-2 ~
v-order-3 v-oqty-3 v-order-4 v-oqty-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-blank-no AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Blank" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE v-form-no AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Form" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE v-itemfg AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE v-oqty-1 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-oqty-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-oqty-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-oqty-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-order-1 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-order-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-order-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-order-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sqty-1 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Loc1" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sqty-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Loc2" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sqty-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Loc3" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sqty-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Loc4" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 6.43.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 6.43.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 10.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      job-mat SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-itemfg AT ROW 2.43 COL 41 COLON-ALIGNED
     fi_job-no AT ROW 1.24 COL 10 COLON-ALIGNED HELP
          "Enter Job Number."
     v-form-no AT ROW 1.29 COL 41 COLON-ALIGNED
     v-blank-no AT ROW 1.24 COL 61 COLON-ALIGNED
     v-sqty-1 AT ROW 6 COL 11 COLON-ALIGNED
     v-sqty-2 AT ROW 7.1 COL 11 COLON-ALIGNED
     v-sqty-3 AT ROW 8.14 COL 11 COLON-ALIGNED
     v-sqty-4 AT ROW 9.1 COL 11 COLON-ALIGNED
     v-order-1 AT ROW 6 COL 42 COLON-ALIGNED NO-LABEL
     v-oqty-1 AT ROW 6 COL 64.6 COLON-ALIGNED NO-LABEL
     v-order-2 AT ROW 7.1 COL 42 COLON-ALIGNED NO-LABEL
     v-oqty-2 AT ROW 7.1 COL 64.6 COLON-ALIGNED NO-LABEL
     v-order-3 AT ROW 8.14 COL 42 COLON-ALIGNED NO-LABEL
     v-oqty-3 AT ROW 8.14 COL 64.6 COLON-ALIGNED NO-LABEL
     v-order-4 AT ROW 9.1 COL 42 COLON-ALIGNED NO-LABEL
     v-oqty-4 AT ROW 9.1 COL 64.6 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 11.24 COL 14
     Btn_Cancel AT ROW 11.33 COL 62
     "Split Shipment/Qty" VIEW-AS TEXT
          SIZE 24 BY .71 AT ROW 4.57 COL 9
     "Split Order / Qty" VIEW-AS TEXT
          SIZE 24 BY .71 AT ROW 4.48 COL 54
     "Order" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.29 COL 50
     "Quantity" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 5.29 COL 71
     RECT-43 AT ROW 4.33 COL 2
     RECT-44 AT ROW 4.33 COL 41
     RECT-45 AT ROW 1 COL 1
     SPACE(0.79) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Split Shipment or Order?"
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

/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-blank-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-form-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-itemfg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.job-mat"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Split Shipment or Order? */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN {&displayed-objects}.
   FIND FIRST tt-fibre WHERE tt-fibre.tt-job-no = job-hdr.job-no
                         AND tt-fibre.tt-job-no2 = job-hdr.job-no2
                         AND tt-fibre.tt-frm = v-form-no
                         AND tt-fibre.tt-blank = v-blank-no NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-fibre THEN DO:
      CREATE tt-fibre.
      ASSIGN tt-fibre.tt-job-no = job-hdr.job-no
             tt-fibre.tt-job-no2 = job-hdr.job-no2
             tt-fibre.tt-frm = v-form-no
             tt-fibre.tt-blank = v-blank-no
              tt-fibre.tt-sqty1 = v-sqty-1
              tt-fibre.tt-sqty2 = v-sqty-2
              tt-fibre.tt-sqty3 = v-sqty-3
              tt-fibre.tt-sqty4 = v-sqty-4
              tt-fibre.tt-oqty1 = v-oqty-1
              tt-fibre.tt-oqty2 = v-oqty-2
              tt-fibre.tt-oqty3 = v-oqty-3
              tt-fibre.tt-oqty4 = v-oqty-4
              tt-fibre.tt-order1 = v-order-1
              tt-fibre.tt-order2 = v-order-2
              tt-fibre.tt-order3 = v-order-3
              tt-fibre.tt-order4 = v-order-4.
   END.

   FIND FIRST d-fibre
        WHERE d-fibre.reftable EQ "cecrep/d-fibre.w"
          AND d-fibre.company  EQ cocode
          AND d-fibre.loc      EQ TRIM(job-hdr.job-no)
          AND d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999")
          AND d-fibre.code2    EQ STRING(v-form-no,"999")
          AND d-fibre.val[1]   EQ v-blank-no
          /*AND (d-fibre.val[2]  EQ ip-frm OR d-fibre.val[2] EQ 0)*/
        NO-ERROR.
    IF NOT AVAIL d-fibre THEN DO:
      CREATE d-fibre.
      ASSIGN
       d-fibre.reftable = "cecrep/d-fibre.w"
       d-fibre.company  = cocode
       d-fibre.loc      = TRIM(job-hdr.job-no)
       d-fibre.code     = STRING(job-hdr.job-no2,"9999999999")
       d-fibre.code2    = STRING(v-form-no,"999")
       d-fibre.val[1]   = v-blank-no.
    END.
    ASSIGN d-fibre.val[2] = tt-fibre.tt-sqty1
           d-fibre.val[3] = tt-fibre.tt-sqty2
           d-fibre.val[4] = tt-fibre.tt-sqty3
           d-fibre.val[5] = tt-fibre.tt-sqty4
           d-fibre.val[6] = tt-fibre.tt-oqty1
           d-fibre.val[7] = tt-fibre.tt-oqty2
           d-fibre.val[8] = tt-fibre.tt-oqty3
           d-fibre.val[9] = tt-fibre.tt-oqty4
           d-fibre.val[10] = tt-fibre.tt-order1
           d-fibre.val[11] = tt-fibre.tt-order2
           d-fibre.val[12] = tt-fibre.tt-order3
           d-fibre.val[13] = tt-fibre.tt-order4
           .

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

  FIND job-hdr WHERE ROWID(job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.
  FIND job-reft WHERE ROWID(job-reft) EQ ip-ref-rowid NO-LOCK NO-ERROR.
  IF AVAIL job-hdr THEN
     fi_job-no = TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99").

  IF AVAIL job-hdr THEN FIND FIRST d-fibre
        WHERE d-fibre.reftable EQ "cecrep/d-fibre.w"
          AND d-fibre.company  EQ cocode
          AND d-fibre.loc      EQ TRIM(job-hdr.job-no)
          AND d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999")
          AND d-fibre.code2    EQ string(job-reft.val[12],"999")  /*form-no*/
          AND d-fibre.val[1]   EQ job-reft.val[13]          /*blank-no*/
          NO-LOCK NO-ERROR.
  IF NOT AVAIL d-fibre THEN
     FIND FIRST d-fibre
        WHERE d-fibre.reftable EQ "cecrep/d-fibre.w"
          AND d-fibre.company  EQ cocode
          AND d-fibre.loc      EQ TRIM(job-hdr.job-no)
          AND d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999")
          AND d-fibre.code2    EQ string(1,"999")  /*form-no*/
          AND d-fibre.val[1]   EQ 1          /*blank-no*/
          NO-LOCK NO-ERROR.

  IF AVAIL d-fibre THEN
     ASSIGN v-sqty-1 = d-fibre.val[2]
            v-sqty-2 = d-fibre.val[3]
            v-sqty-3 = d-fibre.val[4]
            v-sqty-4 = d-fibre.val[5]
            v-oqty-1 = d-fibre.val[6]
            v-oqty-2 = d-fibre.val[7]
            v-oqty-3 = d-fibre.val[8]
            v-oqty-4 = d-fibre.val[9]
            v-order-1 = d-fibre.val[10]
            v-order-2 = d-fibre.val[11]
            v-order-3 = d-fibre.val[12]
            v-order-4 = d-fibre.val[13].
  

  IF AVAIL job-reft THEN
     ASSIGN v-form-no = job-reft.val[12]
            v-blank-no = job-reft.val[13]
            v-itemfg = job-reft.CODE2.


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
  DISPLAY v-itemfg fi_job-no v-form-no v-blank-no v-sqty-1 v-sqty-2 v-sqty-3 
          v-sqty-4 v-order-1 v-oqty-1 v-order-2 v-oqty-2 v-order-3 v-oqty-3 
          v-order-4 v-oqty-4 
      WITH FRAME Dialog-Frame.
  ENABLE v-sqty-1 v-sqty-2 v-sqty-3 v-sqty-4 v-order-1 v-oqty-1 v-order-2 
         v-oqty-2 v-order-3 v-oqty-3 v-order-4 v-oqty-4 Btn_OK Btn_Cancel 
         RECT-43 RECT-44 RECT-45 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

