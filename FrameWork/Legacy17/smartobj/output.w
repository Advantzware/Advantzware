&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: output.w

  Description: from SMART.W - Template for basic SmartObject

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

{methods/defines/hndlset.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Quick_Print RECT-1 RECT-2 output-option ~
spool lines-per-page 
&Scoped-Define DISPLAYED-OBJECTS output-option spool spool-date spool-time ~
spool-ampm lines-per-page 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 spool-date spool-time spool-ampm 
&Scoped-define List-2 output-option spool spool-date spool-time spool-ampm ~
lines-per-page 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Quick_Print 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.8 BY 1.81 TOOLTIP "Quick Print".

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>9":U INITIAL 55 
     LABEL "&Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE spool-ampm AS LOGICAL FORMAT "am/pm":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE spool-date AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE spool-time AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE output-option AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&Printer", 1,
"&Screen", 2,
"&Disk File", 3
     SIZE 12 BY 5.24 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 5.71.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 2.38.

DEFINE VARIABLE spool AS LOGICAL INITIAL no 
     LABEL "Spool" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Quick_Print AT ROW 1.24 COL 1
     output-option AT ROW 1.48 COL 13 HELP
          "Select Output Option" NO-LABEL
     spool AT ROW 1.95 COL 32 HELP
          "Spool Print Indicator"
     spool-date AT ROW 1.95 COL 42 COLON-ALIGNED HELP
          "Enter Spool Print Date" NO-LABEL
     spool-time AT ROW 1.95 COL 59 COLON-ALIGNED HELP
          "Enter Spool Time (HH:MM)" NO-LABEL
     spool-ampm AT ROW 1.95 COL 69 COLON-ALIGNED HELP
          "Enter Spool Print AM/PM" NO-LABEL
     lines-per-page AT ROW 5.29 COL 68 COLON-ALIGNED HELP
          "Enter Lines Per Page"
     "Output" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 11
     "Spool Information" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1 COL 30
     RECT-1 AT ROW 1.24 COL 10
     RECT-2 AT ROW 1.24 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 5.95
         WIDTH              = 76.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME F-Main
   2                                                                    */
ASSIGN 
       lines-per-page:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR RADIO-SET output-option IN FRAME F-Main
   2                                                                    */
ASSIGN 
       output-option:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX spool IN FRAME F-Main
   2                                                                    */
ASSIGN 
       spool:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR FILL-IN spool-ampm IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
ASSIGN 
       spool-ampm:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR FILL-IN spool-date IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
ASSIGN 
       spool-date:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR FILL-IN spool-time IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
ASSIGN 
       spool-time:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Quick_Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Quick_Print s-object
ON CHOOSE OF Btn_Quick_Print IN FRAME F-Main
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "Quick-Print"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME output-option
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL output-option s-object
ON VALUE-CHANGED OF output-option IN FRAME F-Main
DO:
  ASSIGN
    {&SELF-NAME}
    spool:SENSITIVE = IF {&SELF-NAME} = 1 THEN yes ELSE no
    lines-per-page:SCREEN-VALUE = IF {&SELF-NAME} = 2 THEN "33" ELSE "55".
  IF {&SELF-NAME} NE 1 THEN
  DO:
    spool:SCREEN-VALUE = "no".
    APPLY "VALUE-CHANGED" TO spool.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spool
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spool s-object
ON VALUE-CHANGED OF spool IN FRAME F-Main /* Spool */
DO:
  ASSIGN
    {&SELF-NAME}
    spool-date:SENSITIVE = {&SELF-NAME}
    spool-date:SCREEN-VALUE = "  /  /"
    spool-time:SENSITIVE = {&SELF-NAME}
    spool-time:SCREEN-VALUE = "  :"
    spool-ampm:SENSITIVE = {&SELF-NAME}.
  IF {&SELF-NAME} THEN
  DO:
    ASSIGN
      spool-date = TODAY
      spool-time = "0500"
      spool-ampm = FALSE.
    DISPLAY {&LIST-1} WITH FRAME {&FRAME-NAME}.
    APPLY "ENTRY" TO spool-date.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spool-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spool-time s-object
ON LEAVE OF spool-time IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 1 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 46740 THEN
  DO:
    MESSAGE "Spool Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame-Handle s-object 
PROCEDURE Frame-Handle :
/* ---------------------------------------------------------------------------
  Purpose:     Called externally to get THIS-PROCEDURE's Frame handle
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER fhandle AS WIDGET-HANDLE NO-UNDO.

  fhandle = FRAME {&FRAME-NAME}:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISPLAY {&LIST-2} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-Where s-object 
PROCEDURE Output-Where :
/*------------------------------------------------------------------------------
  Purpose:     Give Output value to calling program.
  Parameters:  OUTPUT output-where
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER output-where AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER spooled AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      output-option
      output-where = output-option
      spool
      spooled = spool.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

