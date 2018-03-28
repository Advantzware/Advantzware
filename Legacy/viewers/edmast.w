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
&Scoped-define EXTERNAL-TABLES EDMast
&Scoped-define FIRST-EXTERNAL-TABLE EDMast


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDMast.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDMast.Partner EDMast.PartnerGrp EDMast.Cust ~
EDMast.We-cust EDMast.Vendor EDMast.We-vend-no EDMast.Sf-code ~
EDMast.Ship-to-mask EDMast.RE-code EDMast.Path-in EDMast.Path-out ~
EDMast.Item-Prefix EDMast.ID-Out EDMast.Item-suffix EDMast.Id-trim ~
EDMast.Order-no-mask EDMast.Item-Length EDMast.ID-Len EDMast.Seq ~
EDMast.ASN-on-DS EDMast.Del-Days 
&Scoped-define ENABLED-TABLES EDMast
&Scoped-define FIRST-ENABLED-TABLE EDMast
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS EDMast.Partner EDMast.PartnerGrp ~
EDMast.Cust EDMast.We-cust EDMast.Vendor EDMast.We-vend-no EDMast.Sf-code ~
EDMast.Ship-to-mask EDMast.RE-code EDMast.Path-in EDMast.Path-out ~
EDMast.Item-Prefix EDMast.ID-Out EDMast.Item-suffix EDMast.Id-trim ~
EDMast.Order-no-mask EDMast.Item-Length EDMast.ID-Len EDMast.Seq ~
EDMast.ASN-on-DS EDMast.Del-Days 
&Scoped-define DISPLAYED-TABLES EDMast
&Scoped-define FIRST-DISPLAYED-TABLE EDMast


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
     EDMast.Partner AT ROW 1.48 COL 16 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDMast.PartnerGrp AT ROW 1.48 COL 81 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDMast.Cust AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.We-cust AT ROW 2.43 COL 81 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.Vendor AT ROW 3.38 COL 16 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.We-vend-no AT ROW 3.38 COL 81 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDMast.Sf-code AT ROW 4.33 COL 16 COLON-ALIGNED WIDGET-ID 34
          LABEL "Ship From"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDMast.Ship-to-mask AT ROW 4.33 COL 81 COLON-ALIGNED WIDGET-ID 36
          LABEL "Ship To Mask"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDMast.RE-code AT ROW 5.29 COL 16 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDMast.Path-in AT ROW 7.71 COL 15.8 COLON-ALIGNED WIDGET-ID 24
          LABEL "Path In"
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDMast.Path-out AT ROW 8.71 COL 15.8 COLON-ALIGNED WIDGET-ID 26
          LABEL "Path Out"
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDMast.Item-Prefix AT ROW 10.52 COL 81 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.ID-Out AT ROW 10.71 COL 27 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.Item-suffix AT ROW 11.52 COL 81 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.Id-trim AT ROW 11.71 COL 27 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDMast.Order-no-mask AT ROW 12.52 COL 81 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDMast.Item-Length AT ROW 12.67 COL 27 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.ID-Len AT ROW 13.48 COL 81 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDMast.Seq AT ROW 13.62 COL 27 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDMast.ASN-on-DS AT ROW 14.48 COL 83 WIDGET-ID 48
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     EDMast.Del-Days AT ROW 14.57 COL 27 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDMast
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

/* SETTINGS FOR FILL-IN EDMast.Path-in IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDMast.Path-out IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDMast.Sf-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDMast.Ship-to-mask IN FRAME F-Main
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

&Scoped-define SELF-NAME EDMast.Cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDMast.Cust V-table-Win
ON LEAVE OF EDMast.Cust IN FRAME F-Main /* Cust */
DO:
  if lastkey ne -1 then do:
    run valid-cust NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
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
  {src/adm/template/row-list.i "EDMast"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDMast"}

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
  {src/adm/template/snd-list.i "EDMast"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust V-table-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}


  DEF VAR v-avail AS LOG INIT YES NO-UNDO.


  IF NOT CAN-FIND(FIRST cust NO-LOCK 
    WHERE cust.cust-no EQ edmast.cust:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      v-avail = FALSE.
  
  IF NOT v-avail THEN RETURN ERROR.

  {methods/lValidateError.i NO}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

