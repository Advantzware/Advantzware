&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/jobsfold.w

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
&Scoped-define EXTERNAL-TABLES jobitems
&Scoped-define FIRST-EXTERNAL-TABLE jobitems


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR jobitems.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS jobitems.form_number jobitems.blank_number ~
jobitems.due_date jobitems.order_number jobitems.item_length ~
jobitems.fg_item jobitems.description jobitems.qty jobitems.item_width ~
jobitems.part_number jobitems.style jobitems.upc jobitems.po ~
jobitems.item_depth jobitems.number_up jobitems.style_description ~
jobitems.spc_qa jobitems.dust_flap jobitems.blank_length ~
jobitems.double_knife_length jobitems.blank_sq_inches jobitems.glue_lap ~
jobitems.blank_width jobitems.double_knife_width jobitems.linear_inches ~
jobitems.fifth_panel jobitems.adhesive jobitems.tuck jobitems.square_inch ~
jobitems.lock_tab 
&Scoped-define ENABLED-TABLES jobitems
&Scoped-define FIRST-ENABLED-TABLE jobitems
&Scoped-define DISPLAYED-TABLES jobitems
&Scoped-define FIRST-DISPLAYED-TABLE jobitems
&Scoped-Define ENABLED-OBJECTS panel-length panel-width RECT-2 
&Scoped-Define DISPLAYED-FIELDS jobitems.form_number jobitems.blank_number ~
jobitems.due_date jobitems.order_number jobitems.item_length ~
jobitems.fg_item jobitems.description jobitems.qty jobitems.item_width ~
jobitems.part_number jobitems.style jobitems.upc jobitems.po ~
jobitems.item_depth jobitems.number_up jobitems.style_description ~
jobitems.spc_qa jobitems.dust_flap jobitems.blank_length ~
jobitems.double_knife_length jobitems.blank_sq_inches jobitems.glue_lap ~
jobitems.blank_width jobitems.double_knife_width jobitems.linear_inches ~
jobitems.fifth_panel jobitems.adhesive jobitems.tuck jobitems.square_inch ~
jobitems.lock_tab 
&Scoped-Define DISPLAYED-OBJECTS panel-length panel-width 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE VARIABLE panel-length AS CHARACTER FORMAT "X(14)":U INITIAL "0" 
     LABEL "Length Panels" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "?"  
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE panel-width AS CHARACTER FORMAT "X(14)":U INITIAL "0" 
     LABEL "Width Panels" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "?"   
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 143 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     jobitems.form_number AT ROW 1.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     jobitems.blank_number AT ROW 1.24 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 
     jobitems.due_date AT ROW 1.24 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     jobitems.order_number AT ROW 1.24 COL 105 COLON-ALIGNED
          LABEL "Order Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     jobitems.item_length AT ROW 1.24 COL 129.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     jobitems.fg_item AT ROW 2.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     jobitems.description AT ROW 2.43 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
          BGCOLOR 15 
     jobitems.qty AT ROW 2.43 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 
     jobitems.item_width AT ROW 2.43 COL 129.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     jobitems.part_number AT ROW 3.62 COL 16 COLON-ALIGNED
          LABEL "Customer Part"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 
     jobitems.style AT ROW 3.62 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
          BGCOLOR 15 
     jobitems.upc AT ROW 3.62 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 
     jobitems.po AT ROW 3.62 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 
     jobitems.item_depth AT ROW 3.62 COL 129.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     jobitems.number_up AT ROW 4.81 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     jobitems.style_description AT ROW 4.81 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
          BGCOLOR 15 
     jobitems.spc_qa AT ROW 4.81 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 
     jobitems.dust_flap AT ROW 4.81 COL 128 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     panel-length AT ROW 6 COL 16 COLON-ALIGNED
     jobitems.blank_length AT ROW 6 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     jobitems.double_knife_length AT ROW 6 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     jobitems.blank_sq_inches AT ROW 6 COL 102 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
          BGCOLOR 15 
     jobitems.glue_lap AT ROW 6 COL 128 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     panel-width AT ROW 7.19 COL 16 COLON-ALIGNED
     jobitems.blank_width AT ROW 7.19 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     jobitems.double_knife_width AT ROW 7.19 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     jobitems.linear_inches AT ROW 7.19 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
          BGCOLOR 15 
     jobitems.fifth_panel AT ROW 7.19 COL 128 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     jobitems.adhesive AT ROW 8.38 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     jobitems.tuck AT ROW 8.38 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     jobitems.square_inch AT ROW 8.38 COL 104 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.4 BY 1
          BGCOLOR 15 
     jobitems.lock_tab AT ROW 8.38 COL 128 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     RECT-2 AT ROW 1 COL 1
     "Folding" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.62 COL 2
          FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: jobs.jobitems
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
         HEIGHT             = 8.67
         WIDTH              = 143.4.
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

/* SETTINGS FOR FILL-IN jobitems.order_number IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jobitems.part_number IN FRAME F-Main
   EXP-LABEL                                                            */
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
  {src/adm/template/row-list.i "jobitems"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "jobitems"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  IF AVAILABLE jobitems THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      panel-width:LIST-ITEMS = ''
      panel-width:INNER-LINES = 0
      panel-length:LIST-ITEMS = ''
      panel-length:INNER-LINES = 0.
    DO i = 1 TO EXTENT(jobitems.width_panel) WITH FRAME {&FRAME-NAME}:
      IF jobitems.width_panel[i] NE 0 THEN
      ASSIGN
        ldummy = panel-width:INSERT(STRING(i,'99') + ': ' +
                 STRING(jobitems.width_panel[i],'>>9.999999'),i)
        j = j + 1.
    END.
    IF j NE 0 THEN
    ASSIGN
      panel-width:INNER-LINES = j
      panel-width:SCREEN-VALUE = panel-width:ENTRY(1)
      j = 0.
    DO i = 1 TO EXTENT(jobitems.width_panel) WITH FRAME {&FRAME-NAME}:
      IF jobitems.length_panel[i] NE 0 THEN
      ASSIGN
        ldummy = panel-length:INSERT(STRING(i,'99') + ': ' +
                 STRING(jobitems.length_panel[i],'>>9.999999'),i)
        j = j + 1.
    END.
    IF j NE 0 THEN
    ASSIGN
      panel-length:INNER-LINES = j
      panel-length:SCREEN-VALUE = panel-length:ENTRY(1).
  END.

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
  {src/adm/template/snd-list.i "jobitems"}

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

