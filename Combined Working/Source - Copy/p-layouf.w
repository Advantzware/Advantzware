&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"This SmartPanel sends update, add, 
copy, reset, delete, and cancel messages 
to its TABLEIO-TARGET. "
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-WIn 
/*------------------------------------------------------------------------

  File: p-updsav.w

        This is the standard version of the database
        update SmartPanel. It uses the TABLEIO link
        to communicate with SmartViewers and Smart-
        Browsers.

        There are two styles of this SmartPanel
        (instance attribute SmartPanelType):

          1). Save - the fields of the TABLEIO-TARGET
                       are always enabled and editable.

          2). Update - the fields of the TABLEIO-TARGET
                       are enabled and editable once the
                       Update push button is pressed.
                       The SmartPanel then functions like
                       the Save style

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&Scoped-define adm-attribute-dlg adm/support/u-paneld.w


DEFINE VARIABLE trans-commit AS LOGICAL NO-UNDO.  
DEFINE VARIABLE panel-type   AS CHARACTER NO-UNDO INIT 'SAVE':U.
DEFINE VARIABLE add-active   AS LOGICAL NO-UNDO INIT no.
DEF VAR lv-auto-calc AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartPanel
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Panel-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Save Btn-Cancel btn-auto-calc btn-bom ~
btn-flm btn-goto btn-sht-calc btn-stds btn-copy 

/* Custom List Definitions                                              */
/* Box-Rectangle,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define Box-Rectangle RECT-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-auto-calc 
     LABEL "&Auto-Calc" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-bom 
     LABEL "&Bom" 
     SIZE 12 BY 1.29.

DEFINE BUTTON Btn-Cancel 
     LABEL "Ca&ncel" 
     SIZE 9 BY 1.29
     FONT 4.

DEFINE BUTTON btn-copy 
     LABEL "&Copy" 
     SIZE 11 BY 1.14.

DEFINE BUTTON btn-flm 
     LABEL "&Leaf/Film" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btn-goto 
     LABEL "&Goto" 
     SIZE 12 BY 1.14.

DEFINE BUTTON Btn-Save 
     LABEL "&Save" 
     SIZE 9 BY 1.29
     FONT 4.

DEFINE BUTTON btn-sht-calc 
     LABEL "S&heet Calc" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn-stds 
     LABEL "&Job Stds" 
     SIZE 11 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Panel-Frame
     Btn-Save AT ROW 1.24 COL 2
     Btn-Cancel AT ROW 1.24 COL 11
     btn-auto-calc AT ROW 1.24 COL 20
     btn-bom AT ROW 1.24 COL 35
     btn-flm AT ROW 1.24 COL 48
     btn-goto AT ROW 1.24 COL 61
     btn-sht-calc AT ROW 1.24 COL 73
     btn-stds AT ROW 1.24 COL 89
     btn-copy AT ROW 1.24 COL 101
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartPanel
   Allow: Basic
   Frames: 1
   Add Fields to: NEITHER
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
  CREATE WINDOW C-WIn ASSIGN
         HEIGHT             = 2.81
         WIDTH              = 118.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-WIn 
/* ************************* Included-Libraries *********************** */

{src/adm/method/panel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-WIn
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Panel-Frame
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME Panel-Frame:SCROLLABLE       = FALSE
       FRAME Panel-Frame:HIDDEN           = TRUE.

ASSIGN 
       btn-auto-calc:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

ASSIGN 
       btn-bom:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

ASSIGN 
       btn-flm:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

ASSIGN 
       btn-goto:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

ASSIGN 
       btn-sht-calc:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

ASSIGN 
       btn-stds:PRIVATE-DATA IN FRAME Panel-Frame     = 
                "panel-image".

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Panel-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Panel-Frame
/* Query rebuild information for FRAME Panel-Frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Panel-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-auto-calc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-auto-calc C-WIn
ON CHOOSE OF btn-auto-calc IN FRAME Panel-Frame /* Auto-Calc */
DO:
  DO WITH FRAME Panel-Frame:
     def var source-str as cha no-undo.
     RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     enable btn-cancel with frame {&frame-name}.
     btn-save:label = "&Save" .
     btn-auto-calc:sensitive = no.
     btn-stds:sensitive = no.
     run auto-calc in widget-handle(source-str). 
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-bom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-bom C-WIn
ON CHOOSE OF btn-bom IN FRAME Panel-Frame /* Bom */
DO:
  DO WITH FRAME Panel-Frame:
     def var source-str as cha no-undo.
     RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     
     run update-bom in widget-handle(source-str). 
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel C-WIn
ON CHOOSE OF Btn-Cancel IN FRAME Panel-Frame /* Cancel */
DO:
  DO WITH FRAME Panel-Frame:
      add-active = no.
      btn-auto-calc:sensitive = yes.
      btn-stds:sensitive = yes.
      RUN notify ('cancel-record':U).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy C-WIn
ON CHOOSE OF btn-copy IN FRAME Panel-Frame /* Copy */
DO:
   DO WITH FRAME Panel-Frame:
      def var source-str as cha no-undo.
      RUN get-link-handle IN adm-broker-hdl 
          (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     
      IF VALID-HANDLE(widget-handle(source-str)) THEN 
          run run-form-copy in widget-handle(source-str). 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-flm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-flm C-WIn
ON CHOOSE OF btn-flm IN FRAME Panel-Frame /* Leaf/Film */
DO:
   DO WITH FRAME Panel-Frame:
      def var source-str as cha no-undo.
      RUN get-link-handle IN adm-broker-hdl 
          (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).      
      IF VALID-HANDLE(widget-handle(source-str)) THEN 
          run update-film in widget-handle(source-str). 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-goto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-goto C-WIn
ON CHOOSE OF btn-goto IN FRAME Panel-Frame /* Goto */
DO:
   DO WITH FRAME Panel-Frame:
      def var source-str as cha no-undo.
      RUN get-link-handle IN adm-broker-hdl 
          (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     
      IF VALID-HANDLE(widget-handle(source-str)) THEN 
          run run-goto in widget-handle(source-str). 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save C-WIn
ON CHOOSE OF Btn-Save IN FRAME Panel-Frame /* Save */
DO:
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
  /* If we're in a persistent add-mode then don't change any labels. Just make */
  /* a call to update the last record and then add another record.             */
  RUN get-attribute IN THIS-PROCEDURE ('AddFunction':U).
  IF (RETURN-VALUE = 'Multiple-Records':U) AND add-active THEN 
  DO:
     RUN notify ('update-record':U).
     IF RETURN-VALUE NE "ADM-ERROR":U THEN
         RUN notify ('add-record':U). 
  END.
  ELSE 
&ENDIF
  DO:
     IF panel-type = 'UPDATE':U THEN
     DO WITH FRAME Panel-Frame:
        IF Btn-Save:LABEL = '&Override' /*'&Update'*/ THEN 
        DO:
           RUN new-state('update-begin':U).
           ASSIGN add-active = no
                  btn-stds:sensitive = no.
        END.
        ELSE 
        DO: /* Save */
           RUN notify ('update-record':U).
           IF lv-auto-calc THEN assign btn-auto-calc:sensitive = yes.
           btn-stds:sensitive = yes.
        END.                              
     END.
     ELSE 
     DO: /* Normal 'Save'-style SmartPanel */
        RUN notify ('update-record':U).
        IF lv-auto-calc THEN assign btn-auto-calc:sensitive = yes.
        btn-stds:sensitive = yes.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-sht-calc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-sht-calc C-WIn
ON CHOOSE OF btn-sht-calc IN FRAME Panel-Frame /* Sheet Calc */
DO:
  DO WITH FRAME Panel-Frame:
     def var source-str as cha no-undo.
          
     RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     enable btn-cancel with frame {&frame-name}.
     btn-save:label = "&Save" .
     disable btn-sht-calc btn-auto-calc btn-bom 
             btn-flm btn-goto btn-stds btn-copy with frame {&frame-name}.
     run sheet-calc in widget-handle(source-str). 
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-stds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-stds C-WIn
ON CHOOSE OF btn-stds IN FRAME Panel-Frame /* Job Stds */
DO:
   DO WITH FRAME Panel-Frame:
      def var source-str as cha no-undo.
      RUN get-link-handle IN adm-broker-hdl 
          (THIS-PROCEDURE, 'Tableio-Target':U, OUTPUT source-str).
     
      IF VALID-HANDLE(widget-handle(source-str)) THEN 
          run run-job-stds in widget-handle(source-str). 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-WIn 


/* ***************************  Main Block  *************************** */

  /* Set the default SmartPanel to the one that has the Commit push */
  /* button displayed (the TABLEIO-TARGETS are not enabled/disabled */
  /* automatically with this type of SmartPanel).                   */
  
  RUN set-attribute-list ("SmartPanelType=Save, 
                           Edge-Pixels=2,
                           AddFunction=One-Record":U). 
                           
  /* If the application hasn't enabled the behavior that a RETURN in a frame = GO,
     then enable the usage of the Save button as the default button. (Note that in
     8.0, the Save button was *always* the default button.) */
  IF SESSION:DATA-ENTRY-RETURN NE yes THEN 
  ASSIGN
      Btn-Save:DEFAULT IN FRAME {&FRAME-NAME} = yes
      FRAME {&FRAME-NAME}:DEFAULT-BUTTON = Btn-Save:HANDLE.
  
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-sheet-calc C-WIn 
PROCEDURE apply-sheet-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* called from v-est2.w trigger leave of m-code */
  apply "choose" to btn-save in frame {&frame-name}.
  return no-apply.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-WIn  _DEFAULT-DISABLE
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
  HIDE FRAME Panel-Frame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-auto-calc C-WIn 
PROCEDURE enable-auto-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       It is called form size viewer rfqsize.w to reset button
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-enable AS LOG NO-UNDO.

  IF ip-enable AND v-can-update THEN DO:
      ENABLE btn-auto-calc  WITH FRAME {&FRAME-NAME}.
      lv-auto-calc = YES.
  END.
  ELSE DO:
      DISABLE btn-auto-calc WITH FRAME {&FRAME-NAME}.
      lv-auto-calc = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-copy C-WIn 
PROCEDURE enable-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-enable AS LOG NO-UNDO.

  IF NOT ip-enable AND v-can-create THEN DO:
      ENABLE btn-copy  WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
      DISABLE btn-copy WITH FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-leaf C-WIn 
PROCEDURE enable-leaf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-enable   AS LOG NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    btn-flm:LABEL = IF ip-est-type GT 4 THEN "&Wax/Label" ELSE "&Leaf/Film".
    IF ip-enable AND v-can-update THEN DO:
      ENABLE btn-flm btn-goto.
    END.
    ELSE do:
      DISABLE btn-flm btn-goto.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable C-WIn 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose: The SmartPanel's buttons sensitivities are re-set to whatever
           state they were in when they were disabled. This state is de-
           termined from the variable adm-panel-state.
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('enable':U).      /* Get all objects enabled to start. */
  RUN set-buttons (adm-panel-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-WIn 
PROCEDURE local-initialize :
/*--------------------------------------------------------------------------
  Purpose     : If the SmartPanel is type COMMIT, enable all the fields of
                the TABLEIO-TARGETS since they are defaulted to disabled.
  Notes       :
  ------------------------------------------------------------------------*/

  DEFINE VARIABLE query-position AS CHARACTER NO-UNDO.
  
  /* Insert pre-dispatch code here. */ 

  RUN dispatch IN THIS-PROCEDURE ( INPUT "adm-initialize":U ) .

  /* Insert post-dispatch code here. */

  RUN get-attribute IN THIS-PROCEDURE ('UIB-MODE':U).
  IF RETURN-VALUE <> 'DESIGN':U THEN DO:
     IF VALID-HANDLE (adm-broker-hdl) THEN DO:
       DEFINE VAR tab-target-link AS CHARACTER NO-UNDO.
       RUN get-link-handle IN adm-broker-hdl
           (INPUT THIS-PROCEDURE, 'TABLEIO-TARGET':U, OUTPUT tab-target-link).
       IF (tab-target-link EQ "":U) THEN
         adm-panel-state = 'disable-all':U.
       ELSE DO:
         RUN request-attribute IN adm-broker-hdl
            (INPUT THIS-PROCEDURE, INPUT 'TABLEIO-TARGET':U,
             INPUT 'Query-Position':U).
         query-position = RETURN-VALUE.
         IF query-position = 'no-record-available':U THEN 
           adm-panel-state = 'add-only':U.
         ELSE IF query-position = 'no-external-record-available':U THEN 
           adm-panel-state = 'disable-all':U.
         ELSE adm-panel-state = 'initial':U.
       END.
     END.
     RUN set-buttons (adm-panel-state).
  END.
 
  IF panel-type = 'SAVE':U AND /* Only enable a Save panel if there's a record */
    LOOKUP(query-position,'no-record-available,no-external-record-available':U) = 0
     THEN RUN notify ('enable-fields, TABLEIO-TARGET':U).
  /* otherwise disable in case they were already enabled during initialization*/
  ELSE RUN notify('disable-fields, TABLEIO-TARGET':U). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-buttons C-WIn 
PROCEDURE set-buttons :
/*------------------------------------------------------------------------------
  Purpose: Sets the sensitivity of the panel's buttons depending upon what
           sort of action is occuring to the TABLEIO-TARGET(s) of the panel.
  Parameters:  Character string that denotes which action to set the button
               sensitivities.
               
               The values are: initial - the panel is in a state where no record
                                         changes are occuring; i.e. it is possible
                                         to  Update, Add, Copy, or Delete a record.
                               action-chosen - the panel is in the state where
                                               Update/Save, Add, or Copy has
                                               been pressed.
                               disable-all - the panel has all its buttons 
                                             disabled, in the case a link is
                                             deactivated.
                               add-only - for the time that there are no records
                                          in the query, and the action that can be
                                          taken is an add.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER panel-state AS CHARACTER NO-UNDO.

DO WITH FRAME Panel-Frame:

  IF panel-state = 'disable-all':U THEN DO:

    /* All buttons are set to insensitive. This only should happen when */
    /* the link to the smartpanel is deactivated, but not destroyed.    */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
&ENDIF
      
  ASSIGN
   btn-auto-calc:SENSITIVE = NO
   btn-bom:SENSITIVE       = NO
   btn-flm:SENSITIVE       = NO
   btn-goto:SENSITIVE      = NO
   btn-stds:SENSITIVE      = NO
   btn-sht-calc:SENSITIVE  = NO
   btn-copy:SENSITIVE      = NO.

  END. /* panel-state = 'disable-all' */
  
  ELSE IF panel-state = 'initial':U THEN DO:
  
    /* The panel is not actively changing any of its TABLEIO-TARGET(s). */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = YES.
             IF panel-type = 'UPDATE':U THEN
                 Btn-Save:LABEL = "&Override".
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
       IF panel-type = 'Update':U THEN
             Btn-Reset:SENSITIVE = NO.
       ELSE
             Btn-Reset:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
&ENDIF
      
  ASSIGN
   btn-auto-calc:SENSITIVE = YES
   btn-bom:SENSITIVE       = YES
   btn-flm:SENSITIVE       = YES
   btn-goto:SENSITIVE      = YES
   btn-stds:SENSITIVE      = YES
   btn-sht-calc:SENSITIVE  = YES
   btn-copy:SENSITIVE      = YES.
      
  END. /* panel-state = 'initial' */

  ELSE IF panel-state = 'add-only':U THEN DO:

    /* All buttons are set to insensitive, except add. This only should */
    /* happen only when there are no records in the query and the only  */
    /* thing that can be done to it is add-record.                      */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
&ENDIF
      
  ASSIGN
   btn-auto-calc:SENSITIVE = NO
   btn-bom:SENSITIVE       = NO
   btn-flm:SENSITIVE       = NO
   btn-goto:SENSITIVE      = NO
   btn-stds:SENSITIVE      = NO
   btn-sht-calc:SENSITIVE  = NO
   btn-copy:SENSITIVE      = NO.

  END. /* panel-state = 'add-only' */
 
  ELSE DO: /* panel-state = action-chosen */ 
  
    /* The panel had one of the buttons capable of changing/adding a record */
    /* pressed. Always force the SAVE/UPDATE button to be sensitive in the  */
    /* the event that the smartpanel is disabled and later enabled prior to */
    /* the action being completed.                                          */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = YES.
             IF panel-type = 'UPDATE':U THEN
               Btn-Save:LABEL = "&Save".
&ENDIF    
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = YES.
&ENDIF
      
  ASSIGN
   btn-auto-calc:SENSITIVE = NO
   btn-bom:SENSITIVE       = NO
   btn-flm:SENSITIVE       = NO
   btn-goto:SENSITIVE      = NO
   btn-stds:SENSITIVE      = NO
   btn-sht-calc:SENSITIVE  = NO
   btn-copy:SENSITIVE      = NO.

  END. /* panel-state = action-chosen */

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT v-can-update THEN ASSIGN btn-save:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                                    btn-auto-calc:SENSITIVE = NO
                                    btn-bom:SENSITIVE = NO
                                    btn-flm:SENSITIVE = NO
                                    btn-goto:SENSITIVE = NO
                                    btn-stds:SENSITIVE = NO.
    IF NOT v-can-create THEN btn-copy:SENSITIVE = NO.
    IF NOT v-can-run THEN DISABLE ALL.
  END.

END. /* DO WITH FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-label C-WIn 
PROCEDURE set-label :
/*------------------------------------------------------------------------------
  Purpose: To change the label of the first button in the smartpanel when the
           smartpaneltype is changed from save to update, or vice versa,
           from outside the panel (e.g., from the Instance Attribute dialog. 
  Parameters: label-string - either "Save" or "Update".
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER label-string as CHARACTER NO-UNDO.

DO WITH FRAME panel-frame: 
  Btn-Save:LABEL = label-string.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-WIn 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/pustates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE use-smartpaneltype C-WIn 
PROCEDURE use-smartpaneltype :
/*------------------------------------------------------------------------------
  Purpose:     This procedure sets the value of the panel-type variable 
               whenever the SmartPanelType ADM attribute is set. This is
               used internally within this object to know whether the
               SmartPanel is in "Save" or "Update" mode.
  Parameters:  new attribute value.
  Notes:       This replaces code in local-initialize which set panel-type,
               but which did not always get executed early enough.
------------------------------------------------------------------------------*/
  define input parameter inval as character.
  panel-type = inval.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

