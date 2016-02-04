&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
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
&Scoped-define INTERNAL-TABLES rfqitem

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame rfqitem.qty[1] rfqitem.qty[2] ~
rfqitem.qty[3] rfqitem.qty[9] rfqitem.qty[4] rfqitem.qty[10] rfqitem.qty[5] ~
rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame rfqitem.qty[1] ~
rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[9] rfqitem.qty[4] rfqitem.qty[10] ~
rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame rfqitem

&Scoped-define FIELD-PAIRS-IN-QUERY-Dialog-Frame~
 ~{&FP1}qty[1] ~{&FP2}qty[1] ~{&FP3}~
 ~{&FP1}qty[2] ~{&FP2}qty[2] ~{&FP3}~
 ~{&FP1}qty[3] ~{&FP2}qty[3] ~{&FP3}~
 ~{&FP1}qty[9] ~{&FP2}qty[9] ~{&FP3}~
 ~{&FP1}qty[4] ~{&FP2}qty[4] ~{&FP3}~
 ~{&FP1}qty[10] ~{&FP2}qty[10] ~{&FP3}~
 ~{&FP1}qty[5] ~{&FP2}qty[5] ~{&FP3}~
 ~{&FP1}qty[6] ~{&FP2}qty[6] ~{&FP3}~
 ~{&FP1}qty[7] ~{&FP2}qty[7] ~{&FP3}~
 ~{&FP1}qty[8] ~{&FP2}qty[8] ~{&FP3}
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH rfqitem SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rfqitem


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] ~
rfqitem.qty[9] rfqitem.qty[4] rfqitem.qty[10] rfqitem.qty[5] rfqitem.qty[6] ~
rfqitem.qty[7] rfqitem.qty[8] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}qty[1] ~{&FP2}qty[1] ~{&FP3}~
 ~{&FP1}qty[2] ~{&FP2}qty[2] ~{&FP3}~
 ~{&FP1}qty[3] ~{&FP2}qty[3] ~{&FP3}~
 ~{&FP1}qty[9] ~{&FP2}qty[9] ~{&FP3}~
 ~{&FP1}qty[4] ~{&FP2}qty[4] ~{&FP3}~
 ~{&FP1}qty[10] ~{&FP2}qty[10] ~{&FP3}~
 ~{&FP1}qty[5] ~{&FP2}qty[5] ~{&FP3}~
 ~{&FP1}qty[6] ~{&FP2}qty[6] ~{&FP3}~
 ~{&FP1}qty[7] ~{&FP2}qty[7] ~{&FP3}~
 ~{&FP1}qty[8] ~{&FP2}qty[8] ~{&FP3}
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-FIELDS rfqitem.qty[1] rfqitem.qty[2] ~
rfqitem.qty[3] rfqitem.qty[9] rfqitem.qty[4] rfqitem.qty[10] rfqitem.qty[5] ~
rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] 

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
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rfqitem.qty[1] AT ROW 1 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[2] AT ROW 2 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[3] AT ROW 3 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[9] AT ROW 3 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[4] AT ROW 4 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[10] AT ROW 4 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[5] AT ROW 5 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[6] AT ROW 6 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[7] AT ROW 7 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[8] AT ROW 8 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Btn_Cancel AT ROW 9.57 COL 47
     Btn_OK AT ROW 9.81 COL 12
     SPACE(38.13) SKIP(1.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "rfq.rfqitem"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
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
  find rfqitem where recid(rfqitem) = ip-recid.
  
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
  IF AVAILABLE rfqitem THEN 
    DISPLAY rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[9] 
          rfqitem.qty[4] rfqitem.qty[10] rfqitem.qty[5] rfqitem.qty[6] 
          rfqitem.qty[7] rfqitem.qty[8] 
      WITH FRAME Dialog-Frame.
  ENABLE rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[9] 
         rfqitem.qty[4] rfqitem.qty[10] rfqitem.qty[5] rfqitem.qty[6] 
         rfqitem.qty[7] rfqitem.qty[8] Btn_Cancel Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


