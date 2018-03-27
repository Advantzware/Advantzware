&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/uservend.w

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
{sys/inc/selvend.i NEW}
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
&Scoped-define EXTERNAL-TABLES uservend
&Scoped-define FIRST-EXTERNAL-TABLE uservend


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR uservend.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS uservend.vend_default 
&Scoped-define ENABLED-TABLES uservend
&Scoped-define FIRST-ENABLED-TABLE uservend
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS uservend.vend-no uservend.vend_default 
&Scoped-define DISPLAYED-TABLES uservend
&Scoped-define FIRST-DISPLAYED-TABLE uservend


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS uservend.vend-no 

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
     uservend.vend-no AT ROW 2 COL 13 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     uservend.vend_default AT ROW 3.43 COL 4.4 WIDGET-ID 12
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.uservend
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

/* SETTINGS FOR FILL-IN uservend.vend-no IN FRAME F-Main
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

&Scoped-define SELF-NAME uservend.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uservend.vend-no V-table-Win
ON HELP OF uservend.vend-no IN FRAME F-Main /* Vend. # */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR op-mode AS CHAR NO-UNDO.

   EMPTY TEMP-TABLE tt-vend.

   RUN windows/l-vendsel.w (g_company, uservend.vend-no:SCREEN-VALUE,OUTPUT char-val,OUTPUT op-mode).

   IF op-mode EQ "ADD" THEN
   DO:
      DEF VAR op-user_id AS CHAR NO-UNDO.
      DEF VAR i AS INT NO-UNDO.

      SESSION:SET-WAIT-STATE("general").

      RUN dispatch("cancel-record").
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
      RUN Get-Values in WIDGET-HANDLE(char-hdl) (OUTPUT op-user_id).

      FOR EACH tt-vend:

          IF NOT CAN-FIND(FIRST uservend WHERE
             uservend.user_id EQ op-user_id AND
             uservend.company EQ cocode AND
             uservend.vend-no EQ tt-vend.vend-no) THEN
             DO:
                CREATE uservend.
                ASSIGN
                   uservend.user_id = op-user_id
                   uservend.company = cocode
                   uservend.vend-no = tt-vend.vend-no
                   i = i + 1.
                RELEASE uservend.
             END.
      END.

      MESSAGE STRING(i) " User Vendor Record(s) Created."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

      RUN open-query-proc IN WIDGET-HANDLE(char-hdl).
   END.
   ELSE IF op-mode EQ "ADD ALL" THEN
   DO:
      SESSION:SET-WAIT-STATE("general").

      RUN dispatch("cancel-record").
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
      RUN Get-Values in WIDGET-HANDLE(char-hdl) (OUTPUT op-user_id).

      FOR EACH vend WHERE
          vend.company EQ cocode
          NO-LOCK:

          IF NOT CAN-FIND(FIRST uservend WHERE
             uservend.user_id EQ op-user_id AND
             uservend.company EQ cocode AND
             uservend.vend-no EQ vend.vend-no) THEN
             DO:
                CREATE uservend.
                ASSIGN
                   uservend.user_id = op-user_id
                   uservend.company = cocode
                   uservend.vend-no = vend.vend-no
                   i = i + 1.
                RELEASE uservend.
             END.
      END.

      RUN open-query-proc IN WIDGET-HANDLE(char-hdl).

      MESSAGE "All User Vendors Created."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   ELSE
   IF op-mode EQ "SELECT" AND char-val NE "" THEN
      uservend.vend-no:SCREEN-VALUE = ENTRY(1,char-val).
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
  {src/adm/template/row-list.i "uservend"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "uservend"}

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
  {methods/viewers/create/uservend.i}
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

  RUN valid-vend NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id)"}
   {&methods/lValidateError.i YES}
  IF adm-new-record AND CAN-FIND(FIRST uservend WHERE
     uservend.user_id EQ op-user_id AND
     uservend.company EQ g_company AND
     uservend.vend-no EQ uservend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
     DO:
        MESSAGE "Duplicate Record Being Created."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY" TO uservend.vend-no IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "uservend"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend V-table-Win 
PROCEDURE valid-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST vend WHERE
     vend.company = g_company AND
     vend.vend-no = uservend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
     DO:
        MESSAGE "Invalid Vendor. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO uservend.vend-no.
        RETURN error.
     END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

