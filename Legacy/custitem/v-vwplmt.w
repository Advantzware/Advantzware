&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.


{sys/inc/var.i new shared}

ASSIGN
   cocode = g_company
   locode = g_loc.
 
DEF VAR v-invalid       AS LOG  NO-UNDO.
DEF VAR v-cust-no       AS CHAR NO-UNDO.
DEF VAR v-vendor-code   AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES vend-plant
&Scoped-define FIRST-EXTERNAL-TABLE vend-plant


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vend-plant.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vend-plant.cust-no vend-plant.ship-id ~
vend-plant.plant-id vend-plant.vendor-dept-code vend-plant.plant-name 
&Scoped-define ENABLED-TABLES vend-plant
&Scoped-define FIRST-ENABLED-TABLE vend-plant
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS vend-plant.cust-no vend-plant.ship-id ~
vend-plant.vendor-code vend-plant.plant-id vend-plant.vendor-dept-code ~
vend-plant.plant-name 
&Scoped-define DISPLAYED-TABLES vend-plant
&Scoped-define FIRST-DISPLAYED-TABLE vend-plant


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vend-plant.cust-no AT ROW 1.38 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Suppliers A/R Code" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     vend-plant.ship-id AT ROW 2.48 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Suppliers Ship ID" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     vend-plant.vendor-code AT ROW 3.57 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 14
          LABEL "Customers A/P Code" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     vend-plant.plant-id AT ROW 4.67 COL 31 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     vend-plant.vendor-dept-code AT ROW 5.76 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 10
          LABEL "Customers Dept Code" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     vend-plant.plant-name AT ROW 6.81 COL 31 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Customers Plant Name" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: vend-plant
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
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vend-plant.cust-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN vend-plant.plant-name IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN vend-plant.ship-id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN vend-plant.vendor-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN vend-plant.vendor-dept-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
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

&Scoped-define SELF-NAME vend-plant.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-plant.cust-no V-table-Win
ON HELP OF vend-plant.cust-no IN FRAME F-Main /* Suppliers A/R Code */
DO:
   DEF VAR v-char-val   AS CHAR NO-UNDO.
   DEF VAR v-focus      AS WIDGET NO-UNDO.

   v-focus = FOCUS.

   RUN custitem/l-vwxref.w (cocode, v-focus:SCREEN-VALUE, OUTPUT v-char-val).
   IF v-char-val <> "" THEN DO:
      ASSIGN
         vend-plant.cust-no:SCREEN-VALUE = ENTRY(1,CAPS(v-char-val))
         vend-plant.vendor-code:SCREEN-VALUE = ENTRY(2,CAPS(v-char-val))
         v-vendor-code = ENTRY(2,CAPS(v-char-val)).
   END.

   APPLY "entry" TO v-focus.
   RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-plant.cust-no V-table-Win
ON LEAVE OF vend-plant.cust-no IN FRAME F-Main /* Suppliers A/R Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-plant.plant-id
&Scoped-define SELF-NAME vend-plant.plant-name
&Scoped-define SELF-NAME vend-plant.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-plant.ship-id V-table-Win
ON HELP OF vend-plant.ship-id IN FRAME F-Main /* Suppliers Ship ID */
DO:
   DEF VAR v-char-val   AS CHAR NO-UNDO.
   DEF VAR v-focus      AS WIDGET NO-UNDO.

   v-focus = FOCUS.
         
   RUN windows/l-shipto.w (g_company, "", vend-plant.cust-no:SCREEN-VALUE, v-focus:SCREEN-VALUE, OUTPUT v-char-val).
   IF v-char-val NE "" THEN DO:
      vend-plant.ship-id:SCREEN-VALUE = ENTRY(1,v-char-val).
      APPLY "value-changed" TO v-focus.
   END.

   APPLY "entry" TO v-focus.
   RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-plant.ship-id V-table-Win
ON LEAVE OF vend-plant.ship-id IN FRAME F-Main /* Suppliers Ship ID */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN val-ship-to NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-plant.vendor-dept-code
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  session:data-entry-return = yes.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
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
  {src/adm/template/row-list.i "vend-plant"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vend-plant"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
       

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
         vend-plant.vendor-code:SCREEN-VALUE = CAPS(v-vendor-code)
         vend-plant.vendor-code = CAPS(v-vendor-code).
   END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   vend-plant.company = cocode.
   vend-plant.cust-no = v-cust-no.

    IF adm-adding-record THEN
     ASSIGN
        vend-plant.rec_key = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN val-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   
  RUN val-ship-to NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  disable all with frame {&frame-name}.
  
  IF adm-adding-record THEN v-cust-no = vend-plant.cust-no:SCREEN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .


  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "vend-plant"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-cust-no V-table-Win 
PROCEDURE val-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.   

   DO WITH FRAME {&FRAME-NAME}:
      FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = cocode
                                         AND b-vend-code-cust-xref.cust-no = vend-plant.cust-no:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-code-cust-xref) THEN DO:
         MESSAGE "Invalid Suppliers A/R Code     " VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
      ELSE 
         ASSIGN 
            vend-plant.vendor-code:SCREEN-VALUE = CAPS(b-vend-code-cust-xref.vendor-code)
            v-vendor-code = b-vend-code-cust-xref.vendor-code.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-ship-to V-table-Win 
PROCEDURE val-ship-to :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-shipto FOR shipto.   

   DO WITH FRAME {&FRAME-NAME}:
      IF NOT CAN-FIND(FIRST b-shipto WHERE b-shipto.company = cocode
                                       AND b-shipto.cust-no = vend-plant.cust-no:SCREEN-VALUE
                                       AND b-shipto.ship-id = vend-plant.ship-id:SCREEN-VALUE) THEN DO:
         MESSAGE "Invalid Suppliers Ship To ID     " VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

