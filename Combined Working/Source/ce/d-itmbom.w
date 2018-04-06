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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define SELF-NAME Dialog-Frame
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ef       WHERE rowid(ef) eq ip-rowid SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY {&SELF-NAME} FOR EACH ef       WHERE rowid(ef) eq ip-rowid SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ef
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ef


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.spare-dec-1 ef.medium ef.flute ef.lam-code ~
ef.adh-code 
&Scoped-define ENABLED-TABLES ef
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS rd_flute btn-done btn-cancel RECT-23 
&Scoped-Define DISPLAYED-FIELDS ef.spare-dec-1 ef.medium ef.flute ~
ef.lam-code ef.adh-code ef.adh-sqin 
&Scoped-define DISPLAYED-TABLES ef
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS rd_flute 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 ef.spare-dec-1 ef.medium ef.flute ef.lam-code ~
ef.adh-code rd_flute ef.adh-sqin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-done 
     LABEL "&Done" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE rd_flute AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "B  Shrink% = 25.23% (Take-up = 1.3375)", 125,
"C  Shrink% = 31.07% (Take-up = 1.4507)", 100,
"E  Shrink% = 25.43% (Take-up = 1.3410)", 78,
"F  Shrink% = 18.07% (Take-up = 1.2205)", 52,
"N  Shrink% = 12.82% (Take-up = 1.1470)", 0
     SIZE 51 BY 7.14 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 7.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ef SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ef.spare-dec-1 AT ROW 2 COL 55 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Shrink %" FORMAT "->>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1 TOOLTIP "Enter Shrink % Override for Medium (or select Flute)"
     ef.medium AT ROW 2 COL 22 COLON-ALIGNED
          LABEL "Medium"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ef.flute AT ROW 3.19 COL 22 COLON-ALIGNED
          LABEL "Liner"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ef.lam-code AT ROW 4.38 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ef.adh-code AT ROW 5.57 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     rd_flute AT ROW 1.48 COL 72 NO-LABEL
     ef.adh-sqin AT ROW 6.76 COL 22 COLON-ALIGNED FORMAT ">>,>>9.9<<<"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     btn-done AT ROW 9.57 COL 38.6
     btn-cancel AT ROW 9.57 COL 67.6
     "Flute Size" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.57 COL 59
     RECT-23 AT ROW 1.24 COL 1
     SPACE(1.59) SKIP(2.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Item Bill of Materials"
         CANCEL-BUTTON btn-cancel.


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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ef.adh-code IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN ef.adh-sqin IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.flute IN FRAME Dialog-Frame
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN ef.lam-code IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN ef.medium IN FRAME Dialog-Frame
   1 EXP-LABEL                                                          */
/* SETTINGS FOR RADIO-SET rd_flute IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN ef.spare-dec-1 IN FRAME Dialog-Frame
   1 EXP-LABEL EXP-FORMAT EXP-HELP                                      */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ef
      WHERE rowid(ef) eq ip-rowid SHARE-LOCK.
     _END_FREEFORM
     _Options          = "SHARE-LOCK"
     _Where[1]         = "rowid(ef) eq ip-rowid"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Item Bill of Materials */
DO:
  apply "choose" to btn-done. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Item Bill of Materials */
DO:
  DEF VAR char-val AS cha NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "medium" THEN DO:
      RUN windows/l-paper.w (ef.company, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "flute" THEN DO:
      RUN windows/l-paper.w (ef.company, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "lam-code" THEN DO:
      RUN windows/l-lamin.w (ef.company, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "adh-code" THEN DO:
      RUN windows/l-adhsve.w (ef.company, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Item Bill of Materials */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adh-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adh-code Dialog-Frame
ON LEAVE OF ef.adh-code IN FRAME Dialog-Frame /* Adhesive Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adh-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  apply "window-close" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-done Dialog-Frame
ON CHOOSE OF btn-done IN FRAME Dialog-Frame /* Done */
DO:      
  FIND CURRENT ef EXCLUSIVE NO-ERROR.

  IF AVAIL ef THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     {&LIST-1}
     ef.trim-pen = rd_flute.
  END.

  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.flute Dialog-Frame
ON LEAVE OF ef.flute IN FRAME Dialog-Frame /* Liner */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.lam-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.lam-code Dialog-Frame
ON LEAVE OF ef.lam-code IN FRAME Dialog-Frame /* Laminate Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-lam-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.medium
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.medium Dialog-Frame
ON LEAVE OF ef.medium IN FRAME Dialog-Frame /* Medium */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-medium NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_flute Dialog-Frame
ON VALUE-CHANGED OF rd_flute IN FRAME Dialog-Frame
DO:
  ASSIGN {&self-name}.
  CASE rd_flute:
      WHEN 125 THEN ef.spare-dec-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(25.2336).
      WHEN 100 THEN ef.spare-dec-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(31.0678).
      WHEN 78 THEN ef.spare-dec-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(25.4288).
      WHEN 52 THEN ef.spare-dec-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(18.0664).
      WHEN 0 THEN ef.spare-dec-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(12.8160).
  END CASE.
  
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

  FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.
  
  IF AVAIL ef THEN DO WITH FRAME {&FRAME-NAME}:
    RUN enable_UI.

    rd_flute = ef.trim-pen.

    DISPLAY {&LIST-1}
            ef.gsh-len * ef.gsh-wid @ ef.adh-sqin.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
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
  DISPLAY rd_flute 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ef THEN 
    DISPLAY ef.spare-dec-1 ef.medium ef.flute ef.lam-code ef.adh-code ef.adh-sqin 
      WITH FRAME Dialog-Frame.
  ENABLE ef.spare-dec-1 ef.medium ef.flute ef.lam-code ef.adh-code rd_flute 
         btn-done btn-cancel RECT-23 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-adh-code Dialog-Frame 
PROCEDURE valid-adh-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ef.adh-code:SCREEN-VALUE NE "" THEN DO:
      IF NOT CAN-FIND(FIRST ITEM
                      {sys/look/itemadhW.i}
                        AND item.industry EQ "1"
                        AND item.i-no EQ ef.adh-code:SCREEN-VALUE) THEN DO:
        MESSAGE "Invalid " + TRIM(ef.adh-code:LABEL) + ", try help..."
            VIEW-AS ALERT-BOX.
        APPLY "entry" TO ef.adh-code.
        RETURN ERROR.
      END.
      ef.adh-sqin:SCREEN-VALUE = STRING(ef.gsh-len * ef.gsh-wid).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute Dialog-Frame 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ef.flute:SCREEN-VALUE NE ""                           AND
       NOT CAN-FIND(FIRST ITEM
                   {sys/look/itempapW.i}
                     AND item.industry EQ "1"
                     AND item.i-no EQ ef.flute:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(ef.flute:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO ef.flute.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lam-code Dialog-Frame 
PROCEDURE valid-lam-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ef.lam-code:SCREEN-VALUE NE ""                           AND
       NOT CAN-FIND(FIRST ITEM
                   {sys/look/itemlamW.i}
                     AND item.industry EQ "1"
                     AND item.i-no EQ ef.lam-code:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(ef.lam-code:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO ef.lam-code.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-medium Dialog-Frame 
PROCEDURE valid-medium :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ef.medium:SCREEN-VALUE NE ""                           AND
       NOT CAN-FIND(FIRST ITEM
                   {sys/look/itempapW.i}
                     AND item.industry EQ "1"
                     AND item.i-no EQ ef.medium:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(ef.medium:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO ef.medium.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

