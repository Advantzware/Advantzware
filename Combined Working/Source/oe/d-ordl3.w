&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def input param ip-recid as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ordl

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame oe-ordl.price oe-ordl.cost ~
oe-ordl.cas-cnt oe-ordl.disc oe-ordl.t-price 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame oe-ordl.price ~
oe-ordl.cost oe-ordl.cas-cnt oe-ordl.disc oe-ordl.t-price 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame oe-ordl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame oe-ordl

&Scoped-define FIELD-PAIRS-IN-QUERY-Dialog-Frame~
 ~{&FP1}price ~{&FP2}price ~{&FP3}~
 ~{&FP1}cost ~{&FP2}cost ~{&FP3}~
 ~{&FP1}cas-cnt ~{&FP2}cas-cnt ~{&FP3}~
 ~{&FP1}disc ~{&FP2}disc ~{&FP3}~
 ~{&FP1}t-price ~{&FP2}t-price ~{&FP3}
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-ordl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-ordl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.price oe-ordl.cost oe-ordl.cas-cnt ~
oe-ordl.disc oe-ordl.t-price 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}price ~{&FP2}price ~{&FP3}~
 ~{&FP1}cost ~{&FP2}cost ~{&FP3}~
 ~{&FP1}cas-cnt ~{&FP2}cas-cnt ~{&FP3}~
 ~{&FP1}disc ~{&FP2}disc ~{&FP3}~
 ~{&FP1}t-price ~{&FP2}t-price ~{&FP3}
&Scoped-define ENABLED-TABLES oe-ordl
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.price oe-ordl.cost oe-ordl.cas-cnt ~
oe-ordl.disc oe-ordl.t-price 

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      oe-ordl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     oe-ordl.price AT ROW 2.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-ordl.cost AT ROW 3.19 COL 19 COLON-ALIGNED
          LABEL "Cost/M"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.cas-cnt AT ROW 4.19 COL 19 COLON-ALIGNED
          LABEL "Count"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-ordl.disc AT ROW 5.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     oe-ordl.t-price AT ROW 6.19 COL 19 COLON-ALIGNED
          LABEL "Ext. Price"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     Btn_OK AT ROW 10.52 COL 16
     Btn_Cancel AT ROW 10.52 COL 40
     SPACE(10.13) SKIP(0.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Item Detail"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-ordl.cas-cnt IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.t-price IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.oe-ordl"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Item Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    do with frame {&frame-name}:
       assign {&ENABLED-FIELDS}.
    end.
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
  
  find oe-ordl where recid(oe-ordl) = ip-recid .
   
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  IF AVAILABLE oe-ordl THEN 
    DISPLAY oe-ordl.price oe-ordl.cost oe-ordl.cas-cnt oe-ordl.disc 
          oe-ordl.t-price 
      WITH FRAME Dialog-Frame.
  ENABLE oe-ordl.price oe-ordl.cost oe-ordl.cas-cnt oe-ordl.disc 
         oe-ordl.t-price Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


