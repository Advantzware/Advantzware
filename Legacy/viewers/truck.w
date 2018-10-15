&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-carrier AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-loc AS CHARACTER NO-UNDO.

&SCOPED-DEFINE proc-enable RUN proc-enable.

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
&Scoped-define EXTERNAL-TABLES truck
&Scoped-define FIRST-EXTERNAL-TABLE truck


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR truck.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS truck.truck-desc truck.truck-type ~
truck.length-inches truck.width-inches truck.height-inches ~
truck.msf-c-flute truck.weight-limit truck.max-units truck.regist-no ~
truck.regist-expire-date truck.plate-no truck.axels truck.classification ~
truck.tires truck.tire-size truck.tire-date truck.msfLimit
&Scoped-define ENABLED-TABLES truck
&Scoped-define FIRST-ENABLED-TABLE truck
&Scoped-Define ENABLED-OBJECTS RECT-33 RECT-32 
&Scoped-Define DISPLAYED-FIELDS truck.truck-code truck.truck-desc ~
truck.truck-type truck.length-inches truck.width-inches truck.height-inches ~
truck.msf-c-flute truck.weight-limit truck.max-units truck.regist-no ~
truck.regist-expire-date truck.plate-no truck.axels truck.classification ~
truck.tires truck.tire-size truck.tire-date truck.msfLimit
&Scoped-define DISPLAYED-TABLES truck
&Scoped-define FIRST-DISPLAYED-TABLE truck

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS truck.truck-code 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.truck.company
Carrier||y|asi.truck.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.1.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     truck.truck-code AT ROW 1.24 COL 18 COLON-ALIGNED
          LABEL "Truck-Trailer#"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 FONT 4
     truck.truck-desc AT ROW 1.24 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
          BGCOLOR 15 FONT 4
     truck.truck-type AT ROW 2.38 COL 19.8 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Trailer", "Trailer":U,
"Straight Truck", "Straight Truck":U,
"Tractor", "Tractor":U,
"Other", "Other":U
          SIZE 55.6 BY .95
     truck.length-inches AT ROW 4.1 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     truck.width-inches AT ROW 4.14 COL 33.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 FONT 4
     truck.height-inches AT ROW 4.19 COL 54.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 FONT 4
     truck.msf-c-flute AT ROW 4.24 COL 79.2 COLON-ALIGNED FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 FONT 4
     truck.weight-limit AT ROW 5.14 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FONT 4
     truck.max-units AT ROW 5.14 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 4
     truck.msfLimit AT ROW 5.19 COL 63.2 COLON-ALIGNED WIDGET-ID 2
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 4
     truck.regist-no AT ROW 7.62 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 15 FONT 4
     truck.regist-expire-date AT ROW 7.62 COL 72.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 FONT 4
     truck.plate-no AT ROW 8.62 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 FONT 4
     truck.axels AT ROW 8.62 COL 47.2 COLON-ALIGNED
          LABEL "Axles"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 4
     truck.classification AT ROW 8.62 COL 72.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 FONT 4
     truck.tires AT ROW 9.71 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 4
     truck.tire-size AT ROW 9.71 COL 40.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 4
     truck.tire-date AT ROW 9.71 COL 72.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 FONT 4
     "Other" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 7 COL 2
     "Type:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 2.48 COL 13
     "Volume" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.38 COL 2
     RECT-33 AT ROW 7.24 COL 1
     RECT-32 AT ROW 3.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.truck
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
         HEIGHT             = 14.48
         WIDTH              = 117.4.
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

/* SETTINGS FOR FILL-IN truck.axels IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN truck.msfLimit IN FRAME F-Main
   EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN truck.msf-c-flute IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN truck.truck-code IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "truck"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "truck"}

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
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
  END.

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
  {methods/viewers/create/truck.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "truck" "company"}
  {src/adm/template/sndkycas.i "Carrier" "truck" "Carrier"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "truck"}

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

