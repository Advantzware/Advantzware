&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\d-smtrx3.w
   
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-smtrx-recid AS RECID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

DEF BUFFER bf-smtrx FOR smanmtrx.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_slsmn-start ~
begin_cust-type-start begin_prod-cat-start-from begin_prod-cat-start-to ~
begin_slsmn end_slsmn begin_cust-type end_cust-type btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_slsmn-start begin_cust-type-start ~
begin_prod-cat-start-from begin_prod-cat-start-to begin_slsmn end_slsmn ~
begin_cust-type end_cust-type 

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

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "From Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-type-start AS CHARACTER FORMAT "X(8)" 
     LABEL "Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_prod-cat-start-from AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Product Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_prod-cat-start-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "To Product Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX":U 
     LABEL "From Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn-start AS CHARACTER FORMAT "XXX":U 
     LABEL "Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "To Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX":U 
     LABEL "To Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 9.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_slsmn-start AT ROW 2.52 COL 23.8 COLON-ALIGNED HELP
          "Enter Sales Rep Number"
     begin_cust-type-start AT ROW 3.52 COL 23.8 COLON-ALIGNED HELP
          "Enter Customer Type"
     begin_prod-cat-start-from AT ROW 4.52 COL 23.8 COLON-ALIGNED HELP
          "Enter Category"
     begin_prod-cat-start-to AT ROW 4.52 COL 61.4 COLON-ALIGNED HELP
          "Enter Category"
     begin_slsmn AT ROW 7.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 7.1 COL 65 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_cust-type AT ROW 8.14 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 8.14 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     btn-process AT ROW 11.33 COL 21
     btn-cancel AT ROW 11.33 COL 60
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 6
     "Copy To:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 6.29 COL 4
          FONT 6
     RECT-17 AT ROW 1.24 COL 2
     SPACE(2.39) SKIP(2.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Sales Rep Matrix".


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Copy Sales Rep Matrix */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "begin_cat" OR WHEN "end_cat" THEN DO:
      /*RUN lookups/procat.p.
      IF g_lookup-var NE '' THEN
      ASSIGN
        smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = g_lookup-var
        procatDscr:SCREEN-VALUE = procatDscr(smanmtrx.company,smanmtrx.procat:SCREEN-VALUE).
      APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}.
    END.                     */
      run windows/l-fgcat.w (g_company,focus:SCREEN-VALUE, output char-val).
      if char-val <> "" then 
         assign focus:SCREEN-VALUE = entry(1,char-val).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Sales Rep Matrix */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type Dialog-Frame
ON LEAVE OF begin_cust-type IN FRAME Dialog-Frame /* From Cust Type */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-type-start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type-start Dialog-Frame
ON LEAVE OF begin_cust-type-start IN FRAME Dialog-Frame /* Cust Type */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prod-cat-start-from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prod-cat-start-from Dialog-Frame
ON LEAVE OF begin_prod-cat-start-from IN FRAME Dialog-Frame /* From Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prod-cat-start-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prod-cat-start-to Dialog-Frame
ON LEAVE OF begin_prod-cat-start-to IN FRAME Dialog-Frame /* To Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn Dialog-Frame
ON LEAVE OF begin_slsmn IN FRAME Dialog-Frame /* From Sales Rep# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn-start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn-start Dialog-Frame
ON LEAVE OF begin_slsmn-start IN FRAME Dialog-Frame /* SalesRep# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process Dialog-Frame
ON CHOOSE OF btn-process IN FRAME Dialog-Frame /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.

    RUN validate-top NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.

    MESSAGE "Are you sure you wish to start copy? " VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO UPDATE v-process.

    IF v-process THEN RUN run-process.
    ELSE APPLY "choose" TO btn-cancel.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type Dialog-Frame
ON LEAVE OF end_cust-type IN FRAME Dialog-Frame /* To Cust Type */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn Dialog-Frame
ON LEAVE OF end_slsmn IN FRAME Dialog-Frame /* To SalesRep# */
DO:
  assign {&self-name}.
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
  
  FIND FIRST smanmtrx WHERE RECID(smanmtrx) = ip-smtrx-recid NO-LOCK.
  ASSIGN begin_prod-cat-start-from = smanmtrx.procat
         begin_prod-cat-start-to = smanmtrx.procat
         begin_slsmn-start = smanmtrx.sman
         begin_cust-type-start = smanmtrx.custype.

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
  DISPLAY begin_slsmn-start begin_cust-type-start begin_prod-cat-start-from 
          begin_prod-cat-start-to begin_slsmn end_slsmn begin_cust-type 
          end_cust-type 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 begin_slsmn-start begin_cust-type-start 
         begin_prod-cat-start-from begin_prod-cat-start-to begin_slsmn 
         end_slsmn begin_cust-type end_cust-type btn-process btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b2-smtrx FOR smanmtrx.

SESSION:SET-WAIT-STATE("general").

DO WITH FRAME {&FRAME-NAME}:

FOR EACH bf-smtrx WHERE
    bf-smtrx.company EQ g_company AND
    bf-smtrx.sman EQ begin_slsmn-start AND
    bf-smtrx.custype EQ begin_cust-type-start AND
    bf-smtrx.procat GE begin_prod-cat-start-from AND
    bf-smtrx.procat LE begin_prod-cat-start-to
    NO-LOCK,
    EACH sman FIELDS(sman) WHERE
         sman.company EQ g_company AND
         sman.sman GE begin_slsmn AND
         sman.sman LE END_slsmn
         NO-LOCK,
    EACH custype FIELDS(custype) WHERE
         custype.company EQ g_company AND
         custype.custype GE begin_cust-type AND
         custype.custype LE end_cust-type
         NO-LOCK,
    EACH b2-smtrx WHERE
         b2-smtrx.company EQ g_company AND
         b2-smtrx.sman EQ sman.sman AND
         b2-smtrx.custype EQ custype.custype AND
         b2-smtrx.procat EQ bf-smtrx.procat
         EXCLUSIVE-LOCK:

    /*don't delete copy from record*/
    IF NOT(b2-smtrx.sman EQ bf-smtrx.sman AND
       b2-smtrx.custype EQ bf-smtrx.custype AND
       b2-smtrx.procat EQ bf-smtrx.procat) THEN
       DELETE b2-smtrx.
END.


FOR EACH bf-smtrx WHERE
    bf-smtrx.company EQ g_company AND
    bf-smtrx.sman EQ begin_slsmn-start AND
    bf-smtrx.custype EQ begin_cust-type-start AND
    bf-smtrx.procat GE begin_prod-cat-start-from AND
    bf-smtrx.procat LE begin_prod-cat-start-to
    NO-LOCK,
    EACH sman FIELDS(sman) WHERE
         sman.company EQ g_company AND
         sman.sman GE begin_slsmn AND
         sman.sman LE END_slsmn
         NO-LOCK,
    EACH custype FIELDS(custype) WHERE
         custype.company EQ g_company AND
         custype.custype GE begin_cust-type AND
         custype.custype LE end_cust-type
         NO-LOCK:
    
    /*don't create another record for original salesrep, product category*/
    IF sman.sman EQ begin_slsmn-start AND
       custype.custype EQ begin_cust-type-start THEN
       NEXT.

    CREATE b2-smtrx.
    BUFFER-COPY bf-smtrx EXCEPT bf-smtrx.sman bf-smtrx.custype bf-smtrx.rec_key TO b2-smtrx.
    ASSIGN b2-smtrx.sman = sman.sman
           b2-smtrx.custype = custype.custype.
    RELEASE b2-smtrx.
  END.

SESSION:SET-WAIT-STATE("").

MESSAGE " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "go" TO FRAME {&frame-name}.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-top Dialog-Frame 
PROCEDURE validate-top :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
   
   IF NOT can-find(FIRST sman WHERE
      sman.company EQ g_company AND
      sman.sman EQ begin_slsmn-start)  THEN
      DO:
         MESSAGE "Invalid Sales Rep #. Try help." VIEW-AS ALERT-BOX ERROR.
         APPLY 'entry' TO begin_slsmn-start.
         RETURN ERROR.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

