&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\d-oexfer.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ipr-ord-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER op-ord-no AS INT NO-UNDO.


/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
/*
DEF TEMP-TABLE tt-bud NO-UNDO
    FIELD sman AS CHAR
    FIELD cust-no AS CHAR
    FIELD procat AS CHAR
    FIELD amt AS DEC EXTENT 13
    FIELD msf AS INT EXTENT 13
    FIELD tons AS INT EXTENT 13
    INDEX tt-bud-idx cust-no procat. */
DEF VAR ll-secure AS LOGICAL NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 fiSoldTo btn-import btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCust fiSoldTo fiCustName fiSoldName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-import 
     LABEL "&OK" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiCust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bill To" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE fiSoldName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE fiSoldTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiCust AT ROW 2.19 COL 10 COLON-ALIGNED WIDGET-ID 10
     fiSoldTo AT ROW 2.19 COL 59 COLON-ALIGNED WIDGET-ID 12
     fiCustName AT ROW 3.38 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiSoldName AT ROW 3.38 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     btn-import AT ROW 6.95 COL 26
     btn-cancel AT ROW 6.95 COL 60
     "Click OK to create a transfer order for this assembled set" VIEW-AS TEXT
          SIZE 55 BY .95 AT ROW 5.29 COL 29 WIDGET-ID 22
     RECT-41 AT ROW 1 COL 2
     SPACE(0.59) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Transfer Order for this Assembled Set?".


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN fiCust IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSoldName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Create Transfer Order for this Assembled Set? */
DO:
    DEF VAR char-val AS CHAR NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    RUN windows/l-shipt3.w (g_company, g_loc, fiCust:screen-value, fiSoldTo:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
    if char-val <> "" then
       ASSIGN 
         fiSoldTo:screen-value      = entry(1,char-val)
         fiSoldName:screen-value      = entry(2,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Create Transfer Order for this Assembled Set? */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:

    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-import Dialog-Frame
ON CHOOSE OF btn-import IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN fiCust fiSoldTo.
      RUN oe/oe-xfer.p (INPUT fiCust, INPUT fiSoldTo,
                       INPUT "T", INPUT ipr-ord-row /* order from */,
                       INPUT "Components" /* item types */,
                       INPUT YES /* use defaults */,
                       OUTPUT op-ord-no /* new order # */).

  APPLY 'GO' TO FRAME {&FRAME-NAME}.      
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSoldTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSoldTo Dialog-Frame
ON LEAVE OF fiSoldTo IN FRAME Dialog-Frame /* Ship To */
DO:
  FIND FIRST shipto WHERE shipto.company = g_company
                      AND shipto.ship-id = fiSoldTo:SCREEN-VALUE
                    NO-LOCK NO-ERROR.
  IF NOT AVAIL shipto THEN DO:
      MESSAGE "Please enter a valid ship to code (F1 for list)"
          VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  fiSoldName:SCREEN-VALUE = shipto.ship-name.
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
 
  RUN enable_UI.
  FIND FIRST oe-ord WHERE ROWID(oe-ord) EQ ipr-ord-row
                    NO-LOCK NO-ERROR.
  IF AVAIL oe-ord THEN DO:
      find first cust where cust.company = oe-ord.company 
                        AND cust.active  = "X"    
                      no-lock no-error.
      if avail cust then do:
        ASSIGN fiCust:SCREEN-VALUE = cust.cust-no
               fiCustName:SCREEN-VALUE = cust.NAME.

        find first shipto of cust no-lock no-error.
        find first soldto where soldto.company = oe-ord.company and
                                soldto.cust-no = cust.cust-no
                            /*and trim(soldto.sold-id) = trim(oe-ord.sold-id:screen-value)*/
                            no-lock no-error.
        IF AVAIL(soldto) THEN
            ASSIGN fiSoldTo:SCREEN-VALUE = asi.soldto.sold-id
                   fiSoldName:SCREEN-VALUE = soldto.sold-name.
      END.
  END.

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
  DISPLAY fiCust fiSoldTo fiCustName fiSoldName 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-41 fiSoldTo btn-import btn-cancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

