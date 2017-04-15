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
&Scoped-define INTERNAL-TABLES mstd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame mstd.board-cal[1] ~
mstd.spd-reduc[1] mstd.board-cal[2] mstd.spd-reduc[2] mstd.spd-reduc[3] ~
mstd.board-cal[3] mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] ~
mstd.spd-reduc[5] mstd.board-cal[6] mstd.spd-reduc[6] mstd.board-cal[7] ~
mstd.spd-reduc[7] mstd.board-cal[8] mstd.spd-reduc[8] mstd.board-cal[9] ~
mstd.spd-reduc[9] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame mstd.board-cal[1] ~
mstd.spd-reduc[1] mstd.board-cal[2] mstd.spd-reduc[2] mstd.spd-reduc[3] ~
mstd.board-cal[3] mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] ~
mstd.spd-reduc[5] mstd.board-cal[6] mstd.spd-reduc[6] mstd.board-cal[7] ~
mstd.spd-reduc[7] mstd.board-cal[8] mstd.spd-reduc[8] mstd.board-cal[9] ~
mstd.spd-reduc[9] 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame mstd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame mstd

&Scoped-define FIELD-PAIRS-IN-QUERY-Dialog-Frame~
 ~{&FP1}board-cal[1] ~{&FP2}board-cal[1] ~{&FP3}~
 ~{&FP1}spd-reduc[1] ~{&FP2}spd-reduc[1] ~{&FP3}~
 ~{&FP1}board-cal[2] ~{&FP2}board-cal[2] ~{&FP3}~
 ~{&FP1}spd-reduc[2] ~{&FP2}spd-reduc[2] ~{&FP3}~
 ~{&FP1}spd-reduc[3] ~{&FP2}spd-reduc[3] ~{&FP3}~
 ~{&FP1}board-cal[3] ~{&FP2}board-cal[3] ~{&FP3}~
 ~{&FP1}board-cal[4] ~{&FP2}board-cal[4] ~{&FP3}~
 ~{&FP1}spd-reduc[4] ~{&FP2}spd-reduc[4] ~{&FP3}~
 ~{&FP1}board-cal[5] ~{&FP2}board-cal[5] ~{&FP3}~
 ~{&FP1}spd-reduc[5] ~{&FP2}spd-reduc[5] ~{&FP3}~
 ~{&FP1}board-cal[6] ~{&FP2}board-cal[6] ~{&FP3}~
 ~{&FP1}spd-reduc[6] ~{&FP2}spd-reduc[6] ~{&FP3}~
 ~{&FP1}board-cal[7] ~{&FP2}board-cal[7] ~{&FP3}~
 ~{&FP1}spd-reduc[7] ~{&FP2}spd-reduc[7] ~{&FP3}~
 ~{&FP1}board-cal[8] ~{&FP2}board-cal[8] ~{&FP3}~
 ~{&FP1}spd-reduc[8] ~{&FP2}spd-reduc[8] ~{&FP3}~
 ~{&FP1}board-cal[9] ~{&FP2}board-cal[9] ~{&FP3}~
 ~{&FP1}spd-reduc[9] ~{&FP2}spd-reduc[9] ~{&FP3}
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH mstd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame mstd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame mstd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mstd.board-cal[1] mstd.spd-reduc[1] ~
mstd.board-cal[2] mstd.spd-reduc[2] mstd.spd-reduc[3] mstd.board-cal[3] ~
mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] mstd.spd-reduc[5] ~
mstd.board-cal[6] mstd.spd-reduc[6] mstd.board-cal[7] mstd.spd-reduc[7] ~
mstd.board-cal[8] mstd.spd-reduc[8] mstd.board-cal[9] mstd.spd-reduc[9] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}board-cal[1] ~{&FP2}board-cal[1] ~{&FP3}~
 ~{&FP1}spd-reduc[1] ~{&FP2}spd-reduc[1] ~{&FP3}~
 ~{&FP1}board-cal[2] ~{&FP2}board-cal[2] ~{&FP3}~
 ~{&FP1}spd-reduc[2] ~{&FP2}spd-reduc[2] ~{&FP3}~
 ~{&FP1}spd-reduc[3] ~{&FP2}spd-reduc[3] ~{&FP3}~
 ~{&FP1}board-cal[3] ~{&FP2}board-cal[3] ~{&FP3}~
 ~{&FP1}board-cal[4] ~{&FP2}board-cal[4] ~{&FP3}~
 ~{&FP1}spd-reduc[4] ~{&FP2}spd-reduc[4] ~{&FP3}~
 ~{&FP1}board-cal[5] ~{&FP2}board-cal[5] ~{&FP3}~
 ~{&FP1}spd-reduc[5] ~{&FP2}spd-reduc[5] ~{&FP3}~
 ~{&FP1}board-cal[6] ~{&FP2}board-cal[6] ~{&FP3}~
 ~{&FP1}spd-reduc[6] ~{&FP2}spd-reduc[6] ~{&FP3}~
 ~{&FP1}board-cal[7] ~{&FP2}board-cal[7] ~{&FP3}~
 ~{&FP1}spd-reduc[7] ~{&FP2}spd-reduc[7] ~{&FP3}~
 ~{&FP1}board-cal[8] ~{&FP2}board-cal[8] ~{&FP3}~
 ~{&FP1}spd-reduc[8] ~{&FP2}spd-reduc[8] ~{&FP3}~
 ~{&FP1}board-cal[9] ~{&FP2}board-cal[9] ~{&FP3}~
 ~{&FP1}spd-reduc[9] ~{&FP2}spd-reduc[9] ~{&FP3}
&Scoped-define ENABLED-TABLES mstd
&Scoped-define FIRST-ENABLED-TABLE mstd
&Scoped-Define ENABLED-OBJECTS Btn_update Btn_Cancel Btn_Done 
&Scoped-Define DISPLAYED-FIELDS mstd.board-cal[1] mstd.spd-reduc[1] ~
mstd.board-cal[2] mstd.spd-reduc[2] mstd.spd-reduc[3] mstd.board-cal[3] ~
mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] mstd.spd-reduc[5] ~
mstd.board-cal[6] mstd.spd-reduc[6] mstd.board-cal[7] mstd.spd-reduc[7] ~
mstd.board-cal[8] mstd.spd-reduc[8] mstd.board-cal[9] mstd.spd-reduc[9] 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      mstd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     mstd.board-cal[1] AT ROW 2.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[1] AT ROW 2.67 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[2] AT ROW 3.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[2] AT ROW 3.67 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.spd-reduc[3] AT ROW 4.62 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[3] AT ROW 4.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.board-cal[4] AT ROW 5.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[4] AT ROW 5.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[5] AT ROW 6.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[5] AT ROW 6.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[6] AT ROW 7.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[6] AT ROW 7.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[7] AT ROW 8.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[7] AT ROW 8.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[8] AT ROW 9.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[8] AT ROW 9.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-cal[9] AT ROW 10.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mstd.spd-reduc[9] AT ROW 10.76 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     Btn_update AT ROW 12.19 COL 7
     Btn_Cancel AT ROW 12.19 COL 23
     Btn_Done AT ROW 12.19 COL 41
     "Board" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 22
     "Caliper" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 22
     "Run Speed" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 35
     "% Reduction" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.95 COL 35
     SPACE(17.19) SKIP(11.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "REDUCTION BY CALIPER".


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
     _TblList          = "ASI.mstd"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* REDUCTION BY CALIPER */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    display mstd.board-cal[1 for 9]
            mstd.spd-reduc[1 for 9]
            with frame {&frame-name}
            .
    disable all except btn_update /*btn_cancel*/ btn_done with frame {&frame-name}.
    btn_update:label = "&Update".   
    enable btn_done with frame {&frame-name}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
      APPLY "Window-CLOSE":U TO frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_update Dialog-Frame
ON CHOOSE OF Btn_update IN FRAME Dialog-Frame /* Update */
DO:
    if self:label = "&Update" then do:
       btn_update:label = "&Save".
       enable all with frame {&frame-name}.
       disable btn_done with frame {&frame-name}.
       apply "entry" to mstd.board-cal[1] in frame {&frame-name}.
    end.
    else do:
       run assign-record.   
       disable all except btn_update /* btn_cancel*/ btn_done with frame {&frame-name}.
       btn_update:label = "&Update".
       enable btn_done with frame {&frame-name}.
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

   find mstd where recid(mstd) = ip-recid.

   /* RUN enable_UI. */
  /*disable all except btn_update /*btn_cancel*/ btn_done with frame {&frame-name}.*/
  display mstd.board-cal[1 for 9]
          mstd.spd-reduc[1 for 9]
          with frame {&frame-name}.
  enable btn_update btn_done with frame {&frame-name}.
  view frame {&frame-name}.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-record Dialog-Frame 
PROCEDURE assign-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  do with frame {&frame-name} :
     assign mstd.board-cal[1 for 9]
            mstd.spd-reduc[1 for 9].
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE mstd THEN 
    DISPLAY mstd.board-cal[1] mstd.spd-reduc[1] mstd.board-cal[2] 
          mstd.spd-reduc[2] mstd.spd-reduc[3] mstd.board-cal[3] 
          mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] 
          mstd.spd-reduc[5] mstd.board-cal[6] mstd.spd-reduc[6] 
          mstd.board-cal[7] mstd.spd-reduc[7] mstd.board-cal[8] 
          mstd.spd-reduc[8] mstd.board-cal[9] mstd.spd-reduc[9] 
      WITH FRAME Dialog-Frame.
  ENABLE mstd.board-cal[1] mstd.spd-reduc[1] mstd.board-cal[2] 
         mstd.spd-reduc[2] mstd.spd-reduc[3] mstd.board-cal[3] 
         mstd.board-cal[4] mstd.spd-reduc[4] mstd.board-cal[5] 
         mstd.spd-reduc[5] mstd.board-cal[6] mstd.spd-reduc[6] 
         mstd.board-cal[7] mstd.spd-reduc[7] mstd.board-cal[8] 
         mstd.spd-reduc[8] mstd.board-cal[9] mstd.spd-reduc[9] Btn_update 
         Btn_Cancel Btn_Done 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


