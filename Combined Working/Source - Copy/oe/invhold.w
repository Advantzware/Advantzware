&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\invhold.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF OUTPUT PARAMETER op-choice AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER op-date   AS DATE NO-UNDO.
DEF INPUT PARAMETER ipr-inv-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opc-rowid-list AS CHAR NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR rowid-list AS CHAR .

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rs-choice fiDate btn-invoice-list ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS rs-choice fiDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valid-date Dialog-Frame 
FUNCTION valid-date RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-invoice-list 
     LABEL "Choose Invoices" 
     SIZE 17 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiDate AS DATE FORMAT "99/99/99":U 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rs-choice AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "This Invoice Only", "This Invoice",
"All Invoices for First Finished Good Item", "FG",
"All Invoices for This Customer", "Customer"
     SIZE 51 BY 4.48 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rs-choice AT ROW 2.95 COL 3.4 NO-LABEL WIDGET-ID 8
     fiDate AT ROW 8.14 COL 17 COLON-ALIGNED WIDGET-ID 16
     btn-invoice-list AT ROW 8.14 COL 36 WIDGET-ID 18
     Btn_OK AT ROW 10.76 COL 20
     "Approve Invoices:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.48 COL 17.4 WIDGET-ID 12
          FONT 6
     RECT-1 AT ROW 2.91 COL 1.8 WIDGET-ID 14
     SPACE(2.99) SKIP(3.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Approve Invoices"
         DEFAULT-BUTTON Btn_OK.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Approve Invoices */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-invoice-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-invoice-list Dialog-Frame
ON CHOOSE OF btn-invoice-list IN FRAME Dialog-Frame /* Choose Invoices */
DO:
  FIND inv-head WHERE ROWID(inv-head) EQ ipr-inv-row NO-ERROR.
  IF AVAIL inv-head THEN DO:
      rowid-list = string(ROWID(inv-head)).
      RUN oe/d-invapr.w (INPUT-OUTPUT rowid-list, INPUT op-date, OUTPUT opc-rowid-list).      
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   DO WITH FRAME {&FRAME-NAME}:

       IF NOT valid-date() THEN RETURN NO-APPLY.
   
      ASSIGN rs-choice
             op-choice = rs-choice
             op-date  = DATE(fiDate:SCREEN-VALUE).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-choice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-choice Dialog-Frame
ON VALUE-CHANGED OF rs-choice IN FRAME Dialog-Frame
DO:
 ASSIGN rs-choice.
 IF rs-choice = "Customer" THEN DO:
   btn-invoice-list:SENSITIVE = YES.
 END.
 ELSE DO:
   btn-invoice-list:SENSITIVE = NO.
 END.

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
  btn-invoice-list:SENSITIVE = NO.
  RUN init-prog.
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
  DISPLAY rs-choice fiDate 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 rs-choice fiDate btn-invoice-list Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-prog Dialog-Frame 
PROCEDURE init-prog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN fiDate:SCREEN-VALUE = string(op-date).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valid-date Dialog-Frame 
FUNCTION valid-date RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Validate the invoice date.
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      IF date(fiDate:SCREEN-VALUE) = ? THEN DO:
          MESSAGE "Please enter a valid invoice date."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN FALSE.
      END.
  END.


  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

