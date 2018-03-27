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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS from-company to-company from-item to-item ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS from-company to-company from-item to-item ~
v-status 

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

DEFINE VARIABLE from-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE from-item AS CHARACTER FORMAT "X(10)":U 
     LABEL "From Item" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE to-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE to-item AS CHARACTER FORMAT "X(10)":U 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 25.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     from-company AT ROW 2.91 COL 22 COLON-ALIGNED
     to-company AT ROW 2.91 COL 61 COLON-ALIGNED
     from-item AT ROW 4.57 COL 22 COLON-ALIGNED
     to-item AT ROW 4.57 COL 60.8 COLON-ALIGNED
     Btn_OK AT ROW 7.67 COL 24
     Btn_Cancel AT ROW 7.67 COL 61
     v-status AT ROW 10.76 COL 1 NO-LABEL
     SPACE(0.79) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 2
         TITLE "Copy Raw Material from company to company"
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

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Raw Material from company to company */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&DISPLAYED-OBJECTS}.
    MESSAGE "Are you sure you want to copy Raw Materials?" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN RUN copy-rm.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-rm Dialog-Frame 
PROCEDURE copy-rm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-item FOR ITEM.
    DEF BUFFER bf-item-bom FOR ITEM-bom.
    DEF BUFFER bf-e-item FOR e-item.
    DEF BUFFER bf-e-item-vend FOR e-item-vend.

    DEF VAR v-rec_key AS cha NO-UNDO.
    SESSION:SET-WAIT-STATE("general").

    FOR EACH ITEM NO-LOCK WHERE ITEM.company = from-company
                            AND ITEM.i-no GE from-item
                            AND ITEM.i-no LE to-item:
        IF NOT CAN-FIND(FIRST bf-item WHERE bf-item.company = to-company 
                                        AND bf-item.i-no = ITEM.i-no)
        THEN DO:
            v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Copying Raw Material: " + ITEM.i-no + ", " +
                                                           ITEM.i-name.

            CREATE bf-item.
            BUFFER-COPY item EXCEPT ITEM.company item.rec_key TO bf-item.
            RUN util/reckey3.p ("Item",OUTPUT v-rec_key).
            ASSIGN bf-item.company = to-company
                   bf-item.rec_key = v-rec_key.
        END.

        FOR EACH item-bom NO-LOCK where item-bom.company = item.company 
                                    AND item-bom.parent-i = item.i-no :
            IF NOT CAN-FIND(FIRST bf-item-bom WHERE bf-item-bom.company = to-company
                                                AND bf-item-bom.parent-i = item-bom.parent-i
                                                AND bf-item-bom.LINE# = item-bom.line#) 
            THEN DO:
                CREATE bf-item-bom.
                BUFFER-COPY item-bom EXCEPT item-bom.company item-bom.rec_key TO bf-item-bom.
                RUN util/reckey3.p ("Item-bom",OUTPUT v-rec_key).
                ASSIGN bf-item-bom.company = to-company
                       bf-item-bom.rec_key = v-rec_key.
            END.
        END.
        FOR EACH e-item OF item NO-LOCK:
            IF NOT CAN-FIND(FIRST bf-e-item WHERE bf-e-item.company = to-company
                                     AND bf-e-item.i-no = e-item.i-no)
            THEN DO:
                CREATE bf-e-item.
                BUFFER-COPY e-item EXCEPT e-item.company e-item.rec_key TO bf-e-item.
                RUN util/reckey3.p ("E-Item",OUTPUT v-rec_key).
                ASSIGN bf-e-item.company = to-company
                       bf-e-item.rec_key = v-rec_key.
            END.
            FOR EACH e-item-vend OF e-item WHERE ASI.e-item-vend.item-type = YES NO-LOCK:
                IF NOT CAN-FIND(FIRST bf-e-item-vend WHERE bf-e-item-vend.company = to-company
                                      AND bf-e-item-vend.item-type = e-item-vend.item-type
                                      AND bf-e-item-vend.i-no = e-item-vend.i-no 
                                      AND bf-e-item-vend.vend-no = e-item-vend.vend-no)
                THEN DO:
                    CREATE bf-e-item-vend.
                    BUFFER-COPY e-item-vend EXCEPT e-item-vend.company e-item-vend.rec_key TO bf-e-item-vend.
                    RUN util/reckey3.p ("e-item-vend",OUTPUT v-rec_key).
                    ASSIGN bf-e-item-vend.company = to-company
                           bf-e-item-vend.rec_key = v-rec_key.
                END.
            END.
        END.

    END.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Raw Material Copy completed." VIEW-AS ALERT-BOX.
    APPLY "go" TO FRAME {&FRAME-NAME}.

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
  DISPLAY from-company to-company from-item to-item v-status 
      WITH FRAME Dialog-Frame.
  ENABLE from-company to-company from-item to-item Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

