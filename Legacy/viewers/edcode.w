&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
&Scoped-define EXTERNAL-TABLES EDCode
&Scoped-define FIRST-EXTERNAL-TABLE EDCode


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDCode.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDCode.Partner EDCode.SetID EDCode.Agency ~
EDCode.Version EDCode.Test-prod EDCode.Direction EDCode.Custom-proc ~
EDCode.Customized EDCode.Path-in EDCode.Path-out EDCode.outputType ~
EDCode.sendFileOnPrint 
&Scoped-define ENABLED-TABLES EDCode
&Scoped-define FIRST-ENABLED-TABLE EDCode
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS EDCode.Partner EDCode.SetID EDCode.Agency ~
EDCode.Version EDCode.Test-prod EDCode.Direction EDCode.Custom-proc ~
EDCode.Customized EDCode.Path-in EDCode.Path-out EDCode.outputType ~
EDCode.sendFileOnPrint 
&Scoped-define DISPLAYED-TABLES EDCode
&Scoped-define FIRST-DISPLAYED-TABLE EDCode


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
     SIZE 144 BY 18.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     EDCode.Partner AT ROW 1.71 COL 16 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDCode.SetID AT ROW 1.71 COL 33.6 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     EDCode.Agency AT ROW 1.71 COL 54 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     EDCode.Version AT ROW 1.71 COL 65 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     EDCode.Test-prod AT ROW 3.1 COL 35.6 WIDGET-ID 30
          LABEL "Test Mode?"
          VIEW-AS TOGGLE-BOX
          SIZE 17.4 BY .81
     EDCode.Direction AT ROW 3.14 COL 16.2 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEM-PAIRS "In","I",
                     "Out","O"
          DROP-DOWN-LIST
          SIZE 9 BY 1
     EDCode.Custom-proc AT ROW 5.29 COL 39 COLON-ALIGNED WIDGET-ID 4 FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1
     EDCode.Customized AT ROW 5.38 COL 18.4 WIDGET-ID 24
          VIEW-AS TOGGLE-BOX
          SIZE 14.8 BY .81
     EDCode.Path-in AT ROW 6.24 COL 16.4 COLON-ALIGNED WIDGET-ID 12
          LABEL "Path In"
          VIEW-AS FILL-IN 
          SIZE 73.6 BY 1
     EDCode.Path-out AT ROW 7.24 COL 16.4 COLON-ALIGNED WIDGET-ID 14
          LABEL "Path Out"
          VIEW-AS FILL-IN 
          SIZE 73.6 BY 1
     EDCode.outputType AT ROW 8.62 COL 17 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "EDI","XML","Fixed","Delimited","CSV" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     EDCode.sendFileOnPrint AT ROW 10.05 COL 19 WIDGET-ID 38
          LABEL "Send File on Print?"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .81
     "(Include file prefix)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 7.43 COL 95 WIDGET-ID 34
          FGCOLOR 1 
     "(Include file prefix)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 6.48 COL 95 WIDGET-ID 32
          FGCOLOR 1 
     RECT-1 AT ROW 1.19 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDCode
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
         HEIGHT             = 18.57
         WIDTH              = 145.2.
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

/* SETTINGS FOR FILL-IN EDCode.Custom-proc IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN EDCode.Path-in IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDCode.Path-out IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX EDCode.sendFileOnPrint IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX EDCode.Test-prod IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME EDCode.Partner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDCode.Partner V-table-Win
ON LEAVE OF EDCode.Partner IN FRAME F-Main /* Partner */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN valid-partner NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDCode.SetID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDCode.SetID V-table-Win
ON LEAVE OF EDCode.SetID IN FRAME F-Main /* SetID */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN valid-setid NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
  {src/adm/template/row-list.i "EDCode"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDCode"}

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
  {src/adm/template/snd-list.i "EDCode"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-partner V-table-Win 
PROCEDURE valid-partner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}

  DEF VAR v-avail AS LOG INIT YES NO-UNDO.

  IF NOT CAN-FIND(FIRST edmast NO-LOCK 
    WHERE edmast.partner EQ edcode.partner:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      v-avail = FALSE.
  
  IF NOT v-avail THEN RETURN ERROR.

  {methods/lValidateError.i NO}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-setid V-table-Win 
PROCEDURE valid-setid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}

  DEF VAR v-avail AS LOG INIT YES NO-UNDO.

  IF NOT CAN-FIND(FIRST edsetid NO-LOCK 
    WHERE edsetid.setid EQ edcode.setid:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      v-avail = FALSE.
  
  IF NOT v-avail THEN RETURN ERROR.

  {methods/lValidateError.i NO}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

