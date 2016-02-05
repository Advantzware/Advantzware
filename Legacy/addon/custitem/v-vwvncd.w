&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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

/* &scoped-def oe-prmtx-maint oe-prmtx */

{sys/inc/var.i new shared}

ASSIGN
   cocode = g_company
   locode = g_loc.
DEF VAR char-val   AS CHAR NO-UNDO. 
DEF VAR v-invalid  AS LOG  NO-UNDO.
DEF VAR v-cust-no  AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES vend-code-cust-xref
&Scoped-define FIRST-EXTERNAL-TABLE vend-code-cust-xref


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vend-code-cust-xref.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vend-code-cust-xref.cust-no ~
vend-code-cust-xref.vendor-code vend-code-cust-xref.cust-name 
&Scoped-define ENABLED-TABLES vend-code-cust-xref
&Scoped-define FIRST-ENABLED-TABLE vend-code-cust-xref
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS vend-code-cust-xref.cust-no ~
vend-code-cust-xref.vendor-code vend-code-cust-xref.cust-name 
&Scoped-define DISPLAYED-TABLES vend-code-cust-xref
&Scoped-define FIRST-DISPLAYED-TABLE vend-code-cust-xref


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
company|y|y|ASI.oe-prmtx.company
uom||y|ASI.oe-prmtx.uom[1]
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,uom"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 16.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vend-code-cust-xref.cust-no AT ROW 1.48 COL 34.6 COLON-ALIGNED HELP
          "Suppliers Customer Code in Suppliers A/R Software" WIDGET-ID 8
          LABEL "Suppliers A/R Code"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1 TOOLTIP "Suppliers Customer Code in Suppliers A/R Software"
     vend-code-cust-xref.vendor-code AT ROW 2.91 COL 34.6 COLON-ALIGNED HELP
          "Suppliers Vendor Code in Customers A/P Software" WIDGET-ID 6
          LABEL "Customers A/P Code"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1 TOOLTIP "Suppliers Vendor Code in Customers A/P Software"
     vend-code-cust-xref.cust-name AT ROW 4.1 COL 34.6 COLON-ALIGNED HELP
          "Suppliers Vendor Description in Customers A/P Software" WIDGET-ID 10
          LABEL "Customers A/P Description"
          VIEW-AS FILL-IN 
          SIZE 49.2 BY 1 TOOLTIP "Suppliers Vendor Description in Customers A/P Software"
     RECT-1 AT ROW 1.1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.vend-code-cust-xref
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
         HEIGHT             = 17.71
         WIDTH              = 136.8.
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

/* SETTINGS FOR FILL-IN vend-code-cust-xref.cust-name IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN vend-code-cust-xref.cust-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN vend-code-cust-xref.vendor-code IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
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
/*   def var char-val as cha no-undo.                                                    */
/*   def var lv-handle as handle no-undo.                                                */
/*                                                                                       */
/*                                                                                       */
/*   case focus:name :                                                                   */
/*     when "uom" then do:                                                               */
/*       run windows/l-stduom.w (cocode, uom-list, focus:screen-value, output char-val). */
/*       if char-val ne "" then                                                          */
/*         focus:screen-value in frame {&frame-name} = entry(1,char-val).                */
/*     end.                                                                              */
/*                                                                                       */
/*     otherwise do:                                                                     */
/*       lv-handle = focus:handle.                                                       */
/*       run applhelp.p.                                                                 */
/*                                                                                       */
/*       if g_lookup-var ne "" then lv-handle:screen-value = g_lookup-var.               */
/*                                                                                       */
/*       apply "entry" to lv-handle.                                                     */
/*       return no-apply.                                                                */
/*     end.                                                                              */
/*   end case.                                                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-code-cust-xref.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-code-cust-xref.cust-no V-table-Win
ON HELP OF vend-code-cust-xref.cust-no IN FRAME F-Main /* Suppliers A/R Code */
DO:
   DEF VAR v-recid      AS RECID NO-UNDO.
   DEF VAR v-char-val   AS CHAR NO-UNDO.

   RUN windows/l-custact.w (INPUT cocode, 
                            INPUT FOCUS:SCREEN-VALUE, 
                            OUTPUT v-char-val, 
                            OUTPUT v-recid).
   IF v-char-val <> "" THEN 
      ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
   RETURN NO-APPLY.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-code-cust-xref.cust-no V-table-Win
ON LEAVE OF vend-code-cust-xref.cust-no IN FRAME F-Main /* Suppliers A/R Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-code-cust-xref.vendor-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-code-cust-xref.vendor-code V-table-Win
ON LEAVE OF vend-code-cust-xref.vendor-code IN FRAME F-Main /* Customers A/P Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-vendor-code NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  {src/adm/template/row-list.i "vend-code-cust-xref"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vend-code-cust-xref"}

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
  disable all with frame {&frame-name}.
  
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
   vend-code-cust-xref.company = cocode.
   vend-code-cust-xref.cust-no = v-cust-no.


/*   IF adm-adding-record THEN                                                                                                     */
/*      ASSIGN                                                                                                                     */
/*         item-comm.create-date = TODAY                                                                                           */
/*         item-comm.create-time = TIME                                                                                            */
/*         item-comm.create-user-id = USERID("asi")                                                                                */
/*         item-comm.rec_key = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME). */

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
  
  RUN val-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN val-vendor-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DISABLE ALL WITH FRAME {&FRAME-NAME}.
  
  IF adm-adding-record THEN v-cust-no = vend-code-cust-xref.cust-no:SCREEN-VALUE. 


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
  {src/adm/template/snd-list.i "vend-code-cust-xref"}

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

   IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = g_company
                                             AND b-vend-code-cust-xref.cust-no = vend-code-cust-xref.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
      MESSAGE "Suppliers A/R Code already exists    " 
         VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   ELSE DO:
      IF NOT CAN-FIND(FIRST cust WHERE cust.company = g_company
                                   AND cust.cust-no = vend-code-cust-xref.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
         MESSAGE "Invalid Suppliers A/R Code     "
            VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vendor-code V-table-Win 
PROCEDURE val-vendor-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.

   IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = g_company
                                             AND b-vend-code-cust-xref.vendor-code = vend-code-cust-xref.vendor-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
      MESSAGE "Customers A/P Code already exists    " 
         VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

