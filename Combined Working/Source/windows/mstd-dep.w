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
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mstd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame mstd.board-depth[1] ~
mstd.depth-reduc[1] mstd.board-depth[2] mstd.depth-reduc[2] ~
mstd.board-depth[3] mstd.depth-reduc[3] mstd.depth-reduc[4] ~
mstd.board-depth[4] mstd.depth-reduc[5] mstd.board-depth[5] ~
mstd.depth-reduc[6] mstd.board-depth[6] mstd.depth-reduc[7] ~
mstd.board-depth[7] mstd.depth-reduc[8] mstd.board-depth[8] ~
mstd.depth-reduc[9] mstd.board-depth[9] mstd.depth-reduc[10] ~
mstd.board-depth[10] mstd.depth-reduc[11] mstd.board-depth[11] ~
mstd.depth-reduc[12] mstd.board-depth[12] mstd.board-depth[13] ~
mstd.depth-reduc[13] mstd.board-depth[14] mstd.depth-reduc[14] ~
mstd.board-depth[15] mstd.depth-reduc[15] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame mstd.board-depth[1] ~
mstd.depth-reduc[1] mstd.board-depth[2] mstd.depth-reduc[2] ~
mstd.board-depth[3] mstd.depth-reduc[3] mstd.depth-reduc[4] ~
mstd.board-depth[4] mstd.depth-reduc[5] mstd.board-depth[5] ~
mstd.depth-reduc[6] mstd.board-depth[6] mstd.depth-reduc[7] ~
mstd.board-depth[7] mstd.depth-reduc[8] mstd.board-depth[8] ~
mstd.depth-reduc[9] mstd.board-depth[9] mstd.depth-reduc[10] ~
mstd.board-depth[10] mstd.depth-reduc[11] mstd.board-depth[11] ~
mstd.depth-reduc[12] mstd.board-depth[12] mstd.board-depth[13] ~
mstd.depth-reduc[13] mstd.board-depth[14] mstd.depth-reduc[14] ~
mstd.board-depth[15] mstd.depth-reduc[15] 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame mstd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame mstd
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH mstd SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH mstd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame mstd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame mstd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mstd.board-depth[1] mstd.depth-reduc[1] ~
mstd.board-depth[2] mstd.depth-reduc[2] mstd.board-depth[3] ~
mstd.depth-reduc[3] mstd.depth-reduc[4] mstd.board-depth[4] ~
mstd.depth-reduc[5] mstd.board-depth[5] mstd.depth-reduc[6] ~
mstd.board-depth[6] mstd.depth-reduc[7] mstd.board-depth[7] ~
mstd.depth-reduc[8] mstd.board-depth[8] mstd.depth-reduc[9] ~
mstd.board-depth[9] mstd.depth-reduc[10] mstd.board-depth[10] ~
mstd.depth-reduc[11] mstd.board-depth[11] mstd.depth-reduc[12] ~
mstd.board-depth[12] mstd.board-depth[13] mstd.depth-reduc[13] ~
mstd.board-depth[14] mstd.depth-reduc[14] mstd.board-depth[15] ~
mstd.depth-reduc[15] 
&Scoped-define ENABLED-TABLES mstd
&Scoped-define FIRST-ENABLED-TABLE mstd
&Scoped-Define ENABLED-OBJECTS Btn_update Btn_Cancel Btn_Done 
&Scoped-Define DISPLAYED-FIELDS mstd.board-depth[1] mstd.depth-reduc[1] ~
mstd.board-depth[2] mstd.depth-reduc[2] mstd.board-depth[3] ~
mstd.depth-reduc[3] mstd.depth-reduc[4] mstd.board-depth[4] ~
mstd.depth-reduc[5] mstd.board-depth[5] mstd.depth-reduc[6] ~
mstd.board-depth[6] mstd.depth-reduc[7] mstd.board-depth[7] ~
mstd.depth-reduc[8] mstd.board-depth[8] mstd.depth-reduc[9] ~
mstd.board-depth[9] mstd.depth-reduc[10] mstd.board-depth[10] ~
mstd.depth-reduc[11] mstd.board-depth[11] mstd.depth-reduc[12] ~
mstd.board-depth[12] mstd.board-depth[13] mstd.depth-reduc[13] ~
mstd.board-depth[14] mstd.depth-reduc[14] mstd.board-depth[15] ~
mstd.depth-reduc[15] 
&Scoped-define DISPLAYED-TABLES mstd
&Scoped-define FIRST-DISPLAYED-TABLE mstd


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
     mstd.board-depth[1] AT ROW 2.43 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[1] AT ROW 2.43 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[2] AT ROW 3.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[2] AT ROW 3.43 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[3] AT ROW 4.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[3] AT ROW 4.43 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.depth-reduc[4] AT ROW 5.29 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[4] AT ROW 5.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[5] AT ROW 6.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[5] AT ROW 6.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[6] AT ROW 7.19 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[6] AT ROW 7.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[7] AT ROW 8.19 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[7] AT ROW 8.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[8] AT ROW 9.19 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[8] AT ROW 9.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[9] AT ROW 10.19 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[9] AT ROW 10.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[10] AT ROW 11.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[10] AT ROW 11.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[11] AT ROW 12.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[11] AT ROW 12.33 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[12] AT ROW 13.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[12] AT ROW 13.33 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.board-depth[13] AT ROW 14.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[13] AT ROW 14.24 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     mstd.board-depth[14] AT ROW 15.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[14] AT ROW 15.29 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         NO-LABELS SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     mstd.board-depth[15] AT ROW 16.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     mstd.depth-reduc[15] AT ROW 16.29 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     Btn_update AT ROW 18.14 COL 10
     Btn_Cancel AT ROW 18.14 COL 31
     Btn_Done AT ROW 18.14 COL 51
     "Box Depth" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.48 COL 20
     "Speed Reduction" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.48 COL 35
     SPACE(25.19) SKIP(19.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         NO-LABELS SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "REDUCTION BY DEPTH".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* SETTINGS FOR FILL-IN mstd.board-depth[10] IN FRAME Dialog-Frame
   LABEL "Board Depth[10]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[11] IN FRAME Dialog-Frame
   LABEL "Board Depth[11]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[12] IN FRAME Dialog-Frame
   LABEL "Board Depth[12]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[13] IN FRAME Dialog-Frame
   LABEL "Board Depth[13]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[14] IN FRAME Dialog-Frame
   LABEL "Board Depth[14]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[15] IN FRAME Dialog-Frame
   LABEL "Board Depth[15]:"                                             */
/* SETTINGS FOR FILL-IN mstd.board-depth[2] IN FRAME Dialog-Frame
   LABEL "Board Depth[2]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[3] IN FRAME Dialog-Frame
   LABEL "Board Depth[3]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[4] IN FRAME Dialog-Frame
   LABEL "Board Depth[4]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[5] IN FRAME Dialog-Frame
   LABEL "Board Depth[5]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[6] IN FRAME Dialog-Frame
   LABEL "Board Depth[6]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[7] IN FRAME Dialog-Frame
   LABEL "Board Depth[7]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[8] IN FRAME Dialog-Frame
   LABEL "Board Depth[8]:"                                              */
/* SETTINGS FOR FILL-IN mstd.board-depth[9] IN FRAME Dialog-Frame
   LABEL "Board Depth[9]:"                                              */
/* SETTINGS FOR FILL-IN mstd.depth-reduc[1] IN FRAME Dialog-Frame
   LABEL "Depth Reduction %[1]:"                                        */
/* SETTINGS FOR FILL-IN mstd.depth-reduc[2] IN FRAME Dialog-Frame
   LABEL "Depth Reduction %[2]:"                                        */
/* SETTINGS FOR FILL-IN mstd.depth-reduc[3] IN FRAME Dialog-Frame
   LABEL "Depth Reduction %[3]:"                                        */
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* REDUCTION BY DEPTH */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    display mstd.board-depth[1 for 15]
            mstd.depth-reduc[1 for 15]
            with frame {&frame-name}
            .
    disable all except btn_update /*btn_cancel*/ btn_done with frame {&frame-name}.
    btn_update:label = "&Update".   
    enable btn_done with frame {&frame-name}.        
END.


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
      APPLY "window-CLOSE":U TO frame {&frame-name}.
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
       apply "entry" to mstd.board-depth[1] in frame {&frame-name}.
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
  display mstd.board-depth[1 for 15]
          mstd.depth-reduc[1 for 15]
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
     assign mstd.board-depth[1 for 15]
            mstd.depth-reduc[1 for 15].
  end.
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE mstd THEN 
    DISPLAY mstd.board-depth[1] mstd.depth-reduc[1] mstd.board-depth[2] 
          mstd.depth-reduc[2] mstd.board-depth[3] mstd.depth-reduc[3] 
          mstd.depth-reduc[4] mstd.board-depth[4] mstd.depth-reduc[5] 
          mstd.board-depth[5] mstd.depth-reduc[6] mstd.board-depth[6] 
          mstd.depth-reduc[7] mstd.board-depth[7] mstd.depth-reduc[8] 
          mstd.board-depth[8] mstd.depth-reduc[9] mstd.board-depth[9] 
          mstd.depth-reduc[10] mstd.board-depth[10] mstd.depth-reduc[11] 
          mstd.board-depth[11] mstd.depth-reduc[12] mstd.board-depth[12] 
          mstd.board-depth[13] mstd.depth-reduc[13] mstd.board-depth[14] 
          mstd.depth-reduc[14] mstd.board-depth[15] mstd.depth-reduc[15] 
      WITH FRAME Dialog-Frame.
  ENABLE mstd.board-depth[1] mstd.depth-reduc[1] mstd.board-depth[2] 
         mstd.depth-reduc[2] mstd.board-depth[3] mstd.depth-reduc[3] 
         mstd.depth-reduc[4] mstd.board-depth[4] mstd.depth-reduc[5] 
         mstd.board-depth[5] mstd.depth-reduc[6] mstd.board-depth[6] 
         mstd.depth-reduc[7] mstd.board-depth[7] mstd.depth-reduc[8] 
         mstd.board-depth[8] mstd.depth-reduc[9] mstd.board-depth[9] 
         mstd.depth-reduc[10] mstd.board-depth[10] mstd.depth-reduc[11] 
         mstd.board-depth[11] mstd.depth-reduc[12] mstd.board-depth[12] 
         mstd.board-depth[13] mstd.depth-reduc[13] mstd.board-depth[14] 
         mstd.depth-reduc[14] mstd.board-depth[15] mstd.depth-reduc[15] 
         Btn_update Btn_Cancel Btn_Done 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

