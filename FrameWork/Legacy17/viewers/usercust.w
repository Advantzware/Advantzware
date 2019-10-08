&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/usercust.w

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

{sys/inc/var.i "new shared"}
{sys/inc/selcust.i NEW}
assign
 cocode = g_company
 locode = g_loc.

DEF VAR op-user_id AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES usercust
&Scoped-define FIRST-EXTERNAL-TABLE usercust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR usercust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS usercust.cust_default 
&Scoped-define ENABLED-TABLES usercust
&Scoped-define FIRST-ENABLED-TABLE usercust
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS usercust.cust-no usercust.cust_default 
&Scoped-define DISPLAYED-TABLES usercust
&Scoped-define FIRST-DISPLAYED-TABLE usercust


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS usercust.cust-no 

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
     SIZE 34 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     usercust.cust-no AT ROW 2.19 COL 13 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     usercust.cust_default AT ROW 3.43 COL 5 WIDGET-ID 8
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.usercust
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
         HEIGHT             = 4.05
         WIDTH              = 34.
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

/* SETTINGS FOR FILL-IN usercust.cust-no IN FRAME F-Main
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

&Scoped-define SELF-NAME usercust.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL usercust.cust-no V-table-Win
ON HELP OF usercust.cust-no IN FRAME F-Main /* Cust. # */
DO:
   DEF VAR lv-mode AS CHAR NO-UNDO.
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR op-user_id AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   EMPTY TEMP-TABLE tt-cust.

   RUN windows/l-custsel.w(g_company, "", OUTPUT char-val, OUTPUT lv-mode).

   IF lv-mode EQ "ADD" THEN
   DO:
      SESSION:SET-WAIT-STATE("general").

      RUN dispatch("cancel-record").

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
      RUN Get-Values in WIDGET-HANDLE(char-hdl) (OUTPUT op-user_id).

      FOR EACH tt-cust:

          IF NOT CAN-FIND(FIRST usercust WHERE
             usercust.user_id EQ op-user_id AND
             usercust.company EQ cocode AND
             usercust.cust-no EQ tt-cust.cust-no) THEN
             DO:
                CREATE usercust.
                ASSIGN
                   usercust.user_id = op-user_id
                   usercust.company = cocode
                   usercust.cust-no = tt-cust.cust-no
                   i = i + 1.
                RELEASE usercust.
             END.
      END.

      MESSAGE STRING(i) " User Customer Record(s) Created."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

      RUN open-query-proc IN WIDGET-HANDLE(char-hdl).
   END.
   ELSE IF lv-mode EQ "ADD ALL" THEN
   DO:
      SESSION:SET-WAIT-STATE("general").

      RUN dispatch("cancel-record").

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
      RUN Get-Values in WIDGET-HANDLE(char-hdl) (OUTPUT op-user_id).

      FOR EACH cust WHERE
          cust.company EQ cocode
          NO-LOCK:

          IF NOT CAN-FIND(FIRST usercust WHERE
             usercust.user_id EQ op-user_id AND
             usercust.company EQ cocode AND
             usercust.cust-no EQ cust.cust-no) THEN
             DO:
                CREATE usercust.
                ASSIGN
                   usercust.user_id = op-user_id
                   usercust.company = cocode
                   usercust.cust-no = cust.cust-no
                   i = i + 1.
                RELEASE usercust.
             END.
      END.

      RUN open-query-proc IN WIDGET-HANDLE(char-hdl).

      MESSAGE "All User Customers Created."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   ELSE
   IF lv-mode EQ "Select" AND char-val NE "" THEN
      usercust.cust-no:SCREEN-VALUE = char-val.

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
  {src/adm/template/row-list.i "usercust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "usercust"}

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
  {methods/viewers/create/usercust.i}
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
  DEF VAR op-user_id AS CHAR NO-UNDO.

  RUN valid-cust NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id)"}
  {&methods/lValidateError.i YES}
  IF adm-new-record AND CAN-FIND(FIRST usercust WHERE
     usercust.user_id EQ op-user_id AND
     usercust.company EQ g_company AND
     usercust.cust-no EQ usercust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
     DO:
        MESSAGE "Duplicate Record Being Created."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY" TO usercust.cust-no IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
    {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").
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
  {src/adm/template/snd-list.i "usercust"}

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
  IF NOT CAN-FIND(FIRST cust WHERE
     cust.company = g_company AND
     cust.cust-no = usercust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
     DO:
        MESSAGE "Invalid Customer. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO usercust.cust-no.
        RETURN error.
     END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

