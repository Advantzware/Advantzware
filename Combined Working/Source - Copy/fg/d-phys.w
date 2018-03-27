&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEF INPUT-OUTPUT PARAM iop-wh-list AS cha NO-UNDO.
/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_loc begin_bin end_loc end_bin Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_loc begin_bin end_loc end_bin 

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

DEFINE VARIABLE begin_bin AS CHARACTER FORMAT "X(8)":U 
     LABEL " Bin" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "From Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_bin AS CHARACTER FORMAT "X(8)":U 
     LABEL " Bin" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "To Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_loc AT ROW 1.95 COL 22 COLON-ALIGNED
     begin_bin AT ROW 1.95 COL 45 COLON-ALIGNED
     end_loc AT ROW 4.1 COL 22 COLON-ALIGNED
     end_bin AT ROW 4.1 COL 45 COLON-ALIGNED
     Btn_OK AT ROW 6.48 COL 16
     Btn_Cancel AT ROW 6.48 COL 45
     "-------------------------------------------------------------------------" VIEW-AS TEXT
          SIZE 58 BY .62 AT ROW 3.14 COL 6
          FGCOLOR 9 
     SPACE(7.79) SKIP(4.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "FG Physical Count Transfer"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* FG Physical Count Transfer */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR v-loc AS cha NO-UNDO.
   CASE FOCUS:NAME:
       WHEN "begin_loc" OR WHEN "end_loc" THEN DO:
            RUN windows/l-loc.w (g_company,focus:screen-value,OUTPUT char-val).
            IF char-val <> "" THEN 
               FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       WHEN "begin_bin" OR WHEN "end_bin" THEN DO:
            v-loc = IF FOCUS:NAME BEGINS "begin" THEN begin_loc:SCREEN-VALUE
                    ELSE END_loc:SCREEN-VALUE.
            RUN windows/l-fgbin.w (g_company,v-loc,focus:screen-value,OUTPUT char-val).
            IF char-val <> "" THEN 
               FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* FG Physical Count Transfer */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   iop-wh-list = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&displayed-objects}.

    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ g_company
                      AND loc.loc     EQ begin_loc)
    THEN DO:
      MESSAGE "Invalid Warehouse, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_loc.
      RETURN NO-APPLY.
    END.
    IF NOT CAN-FIND(FIRST loc WHERE loc.company EQ g_company
                      AND loc.loc     EQ end_loc)
    THEN DO:
      MESSAGE "Invalid Warehouse, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_loc.
      RETURN NO-APPLY.
    END.

    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ g_company
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ begin_loc
                      AND fg-bin.loc-bin EQ begin_bin
                    USE-INDEX co-ino)
    THEN DO:
      MESSAGE "Invalid Bin#, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_bin.
      RETURN NO-APPLY.
    END.
    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ g_company
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ end_loc
                      AND fg-bin.loc-bin EQ end_bin
                    USE-INDEX co-ino)
    THEN DO:
      MESSAGE "Invalid Bin#, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_bin.
      RETURN NO-APPLY.
    END.

    iop-wh-list = begin_loc + "," + begin_bin + "," +
                  END_loc + "," + end_bin.

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

  ASSIGN begin_loc = ENTRY(1,iop-wh-list)
         begin_bin = ENTRY(2,iop-wh-list)
         end_loc = ENTRY(3,iop-wh-list)
         end_bin = ENTRY(4,iop-wh-list).

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
  DISPLAY begin_loc begin_bin end_loc end_bin 
      WITH FRAME Dialog-Frame.
  ENABLE begin_loc begin_bin end_loc end_bin Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

