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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_spec end_spec begin_fgitem end_fgitem ~
begin_date end_date Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_spec begin_desc end_spec end_desc ~
begin_fgitem end_fgitem begin_date end_date 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fgitem AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_spec AS CHARACTER FORMAT "X(3)":U 
     LABEL "From Spec Code" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE end_fgitem AS CHARACTER FORMAT "X(15)":U 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_spec AS CHARACTER FORMAT "X(3)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_spec AT ROW 1.71 COL 22 COLON-ALIGNED
     begin_desc AT ROW 1.71 COL 33 COLON-ALIGNED NO-LABEL
     end_spec AT ROW 2.91 COL 22 COLON-ALIGNED
     end_desc AT ROW 2.91 COL 33 COLON-ALIGNED NO-LABEL
     begin_fgitem AT ROW 4.05 COL 22 COLON-ALIGNED
     end_fgitem AT ROW 5.14 COL 22 COLON-ALIGNED
     begin_date AT ROW 6.24 COL 22 COLON-ALIGNED
     end_date AT ROW 7.38 COL 22 COLON-ALIGNED
     Btn_OK AT ROW 9.38 COL 17
     Btn_Cancel AT ROW 9.38 COL 51
     SPACE(14.19) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Purge FG item spec notes"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_desc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_desc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Purge FG item spec notes */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    case FOCUS:NAME :
        WHEN "begin_spec" THEN DO:
           RUN cec/l-itspec.w (g_company, begin_spec:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              ASSIGN begin_spec:SCREEN-VALUE = ENTRY(1,char-val).
              begin_desc:SCREEN-VALUE = ENTRY(2,char-val).
           END.
        END.
        WHEN "end_spec" THEN DO:
           RUN cec/l-itspec.w (g_company, end_spec:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              ASSIGN end_spec:SCREEN-VALUE = ENTRY(1,char-val).
              end_desc:SCREEN-VALUE = ENTRY(2,char-val).
           END.
        END.
        WHEN "begin_fgitem" THEN DO:
            RUN windows/l-itmfg2.w (g_company, "", INPUT begin_fgitem:SCREEN-VALUE, INPUT 1, OUTPUT char-val).
            IF char-val <> "" THEN 
            DO:
                ASSIGN 
                    begin_fgitem:SCREEN-VALUE = ENTRY(1,char-val).
                APPLY "entry" TO begin_fgitem.
            END.                           
        END.     
        WHEN "end_fgitem" THEN DO:
            RUN windows/l-itmfg2.w (g_company, "", INPUT end_fgitem:SCREEN-VALUE, INPUT 1, OUTPUT char-val).
            IF char-val <> "" THEN 
            DO:
                ASSIGN 
                    end_fgitem:SCREEN-VALUE = ENTRY(1,char-val).
                APPLY "entry" TO end_fgitem.
            END.                           
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Purge FG item spec notes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_spec Dialog-Frame
ON LEAVE OF begin_spec IN FRAME Dialog-Frame /* From Spec Code */
DO:
   RUN display-desc ("begin_spec").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN {&displayed-objects}.
   MESSAGE "Are you sure you want to purge all spec notes you select?"
       VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.

   IF ll-ans THEN DO:
      SESSION:SET-WAIT-STATE("general").
      DISABLE TRIGGERS FOR LOAD OF notes.
      OUTPUT TO value("c:\tmp\specnote" + STRING(TIME) + ".txt").
      FOR EACH itemfg NO-LOCK WHERE itemfg.company = g_company
                    AND itemfg.i-no >= begin_fgitem
                    AND itemfg.i-no<= end_fgitem,
          EACH notes WHERE notes.rec_key = itemfg.rec_key
                    AND notes.note_code >= begin_spec
                    AND notes.note_code <= END_spec
                    AND notes.note_date >= begin_date
                    AND notes.note_date <= end_date:
          EXPORT notes.
          DELETE notes.
      END.
      OUTPUT CLOSE.
      SESSION:SET-WAIT-STATE("").
      MESSAGE "FG Item spec note purge is completed." VIEW-AS ALERT-BOX INFORMATION.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_spec Dialog-Frame
ON LEAVE OF end_spec IN FRAME Dialog-Frame /* To */
DO:
  RUN display-desc ("end_spec").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-desc Dialog-Frame 
PROCEDURE display-desc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-value AS cha NO-UNDO.

  FIND FIRST item-spec NO-LOCK WHERE item-spec.company = g_company
                                 AND item-spec.i-no = ""
                                 AND ITEM-spec.CODE = IF ip-value = "begin_spec" THEN begin_spec:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                                                      ELSE END_spec:SCREEN-VALUE
                                 NO-ERROR.  
  IF AVAIL item-spec THEN
      IF ip-value = "begin_spec" THEN begin_desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-spec.note[1].
      ELSE  end_desc:SCREEN-VALUE = item-spec.note[1].

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
  DISPLAY begin_spec begin_desc end_spec end_desc begin_fgitem end_fgitem 
          begin_date end_date 
      WITH FRAME Dialog-Frame.
  ENABLE begin_spec end_spec begin_fgitem end_fgitem begin_date end_date Btn_OK 
         Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

