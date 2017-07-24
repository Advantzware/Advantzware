&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

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
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER b-item FOR item.

DEF VAR lv-first AS LOG INIT YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item.i-no 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS RECT-23 
&Scoped-Define DISPLAYED-FIELDS item.i-no 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS fi_medium rd_flute fi_flute fi_lam-code ~
fi_adh-code 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS fi_medium rd_flute fi_flute fi_lam-code ~
fi_adh-code 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_adh-code LIKE ef.adh-code
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_flute LIKE ef.flute
     LABEL "Paper 2" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_lam-code LIKE ef.lam-code
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_medium LIKE ef.medium
     LABEL "Paper 1" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd_flute AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "B", 125,
"E", 78,
"F", 52,
"None", 0
     SIZE 11 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 6.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_medium AT ROW 1.71 COL 21 COLON-ALIGNED
          LABEL "Paper 1"
     rd_flute AT ROW 1.71 COL 61 NO-LABEL
     fi_flute AT ROW 2.91 COL 21 COLON-ALIGNED
          LABEL "Paper 2"
     fi_lam-code AT ROW 4.1 COL 21 COLON-ALIGNED
     fi_adh-code AT ROW 5.29 COL 21 COLON-ALIGNED
     item.i-no AT ROW 6 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-23 AT ROW 1 COL 1
     "Flute Size" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.item
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.62
         WIDTH              = 75.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_adh-code IN FRAME F-Main
   NO-ENABLE 1 LIKE = asi.ef.adh-code EXP-SIZE                          */
/* SETTINGS FOR FILL-IN fi_flute IN FRAME F-Main
   NO-ENABLE 1 LIKE = asi.ef.flute EXP-LABEL EXP-SIZE                   */
/* SETTINGS FOR FILL-IN fi_lam-code IN FRAME F-Main
   NO-ENABLE 1 LIKE = asi.ef.lam-code EXP-SIZE                          */
/* SETTINGS FOR FILL-IN fi_medium IN FRAME F-Main
   NO-ENABLE 1 LIKE = asi.ef.medium EXP-LABEL EXP-SIZE                  */
ASSIGN 
       item.i-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET rd_flute IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "fi_medium" THEN DO:
      RUN windows/l-paper.w (cocode, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "fi_flute" THEN DO:
      RUN windows/l-paper.w (cocode, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "fi_lam-code" THEN DO:
      RUN windows/l-lamin.w (cocode, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "fi_adh-code" THEN DO:
      RUN windows/l-adhsve.w (cocode, "1",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN 
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_adh-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_adh-code V-table-Win
ON LEAVE OF fi_adh-code IN FRAME F-Main /* Adhesive Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adh-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_flute V-table-Win
ON LEAVE OF fi_flute IN FRAME F-Main /* Paper 2 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_lam-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_lam-code V-table-Win
ON LEAVE OF fi_lam-code IN FRAME F-Main /* Laminate Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-lam-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_medium
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_medium V-table-Win
ON LEAVE OF fi_medium IN FRAME F-Main /* Paper 1 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-medium NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_flute V-table-Win
ON VALUE-CHANGED OF rd_flute IN FRAME F-Main
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

IF AVAIL item THEN DO:
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
END.

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item-bom V-table-Win 
PROCEDURE create-item-bom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-i-no LIKE item-bom.i-no NO-UNDO.
  DEF INPUT PARAM ip-line LIKE item-bom.line# NO-UNDO.


  FIND FIRST item-bom
      WHERE item-bom.company  EQ item.company
        AND item-bom.parent-i EQ item.i-no
        AND item-bom.line#    EQ ip-line
      NO-ERROR.
  IF NOT AVAIL item-bom THEN CREATE item-bom.
  ASSIGN
   item-bom.company  = item.company
   item-bom.parent-i = item.i-no
   item-bom.line#    = ip-line
   item-bom.i-no     = ip-i-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN create-item-bom (fi_medium, 1).
  RUN create-item-bom (fi_flute, 2).
  RUN create-item-bom (fi_lam-code, 3).
  RUN create-item-bom (fi_adh-code, 4).
  RUN create-item-bom (STRING(rd_flute), 5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  RUN local-display-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable V-table-Win 
PROCEDURE local-disable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   fi_medium   = ""
   fi_flute    = ""
   fi_lam-code = ""
   fi_adh-code = ""
   rd_flute    = 0.

  FOR EACH item-bom
      WHERE item-bom.company  EQ item.company
        AND item-bom.parent-i EQ item.i-no
        AND item-bom.line#    GE 1
        AND item-bom.line#    LE 5
      NO-LOCK:
    CASE item-bom.line#:
      WHEN 1 THEN fi_medium   = item-bom.i-no.
      WHEN 2 THEN fi_flute    = item-bom.i-no.
      WHEN 3 THEN fi_lam-code = item-bom.i-no.
      WHEN 4 THEN fi_adh-code = item-bom.i-no.
      WHEN 5 THEN rd_flute    = INT(item-bom.i-no).
    END CASE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-first THEN lv-first = NO.
    ELSE ENABLE ALL EXCEPT item.i-no.
    APPLY "entry" TO fi_medium.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch ('enable').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-medium NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-flute NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-lam-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-adh-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
  RUN set-label IN WIDGET-HANDLE(char-hdl) ("&Update").  
  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("Initial").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-adh-code V-table-Win 
PROCEDURE valid-adh-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_adh-code:SCREEN-VALUE NE ""                              AND
       NOT CAN-FIND(FIRST b-item
                    {sys/look/itemadhW.i b-}
                      AND b-item.industry EQ "1"
                      AND b-item.i-no EQ fi_adh-code:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(fi_adh-code:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_adh-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute V-table-Win 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_flute:SCREEN-VALUE NE ""                             AND
       NOT CAN-FIND(FIRST b-item
                   {sys/look/itempapW.i b-}
                     AND b-item.industry EQ "1"
                     AND b-item.i-no EQ fi_flute:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(fi_flute:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_flute.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lam-code V-table-Win 
PROCEDURE valid-lam-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_lam-code:SCREEN-VALUE NE ""                             AND
       NOT CAN-FIND(FIRST b-item
                   {sys/look/itemlamW.i b-}
                     AND b-item.industry EQ "1"
                     AND b-item.i-no EQ fi_lam-code:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(fi_lam-code:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_lam-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-medium V-table-Win 
PROCEDURE valid-medium :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_medium:SCREEN-VALUE NE ""                             AND
       NOT CAN-FIND(FIRST b-item
                   {sys/look/itempapW.i b-}
                     AND b-item.industry EQ "1"
                     AND b-item.i-no EQ fi_medium:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid " + TRIM(fi_medium:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_medium.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

