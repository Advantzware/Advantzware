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

  File: viewers\mach-part.w

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

{sys/inc/var.i new shared}

&SCOPED-DEFINE enable-mach-part enable-mach-part

DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-m-code AS CHARACTER NO-UNDO.

DEF BUFFER b-mach-part FOR mach-part.

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
&Scoped-define EXTERNAL-TABLES mach-part
&Scoped-define FIRST-EXTERNAL-TABLE mach-part


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach-part.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mach-part.rm-part-desc-1 ~
mach-part.rm-part-desc-2 mach-part.serial-no mach-part.model-no ~
mach-part.date-purchased mach-part.date-installed mach-part.expected-days ~
mach-part.expected-hours mach-part.expected-impressions ~
mach-part.total-impressions-run mach-part.company-installed ~
mach-part.person-installed 
&Scoped-define ENABLED-TABLES mach-part
&Scoped-define FIRST-ENABLED-TABLE mach-part
&Scoped-Define ENABLED-OBJECTS lv-total-hours 
&Scoped-Define DISPLAYED-FIELDS mach-part.rm-part-code ~
mach-part.rm-part-desc-1 mach-part.rm-part-desc-2 mach-part.serial-no ~
mach-part.model-no mach-part.date-purchased mach-part.date-installed ~
mach-part.expected-days mach-part.expected-hours ~
mach-part.expected-impressions mach-part.total-impressions-run ~
mach-part.company-installed mach-part.person-installed 
&Scoped-define DISPLAYED-TABLES mach-part
&Scoped-define FIRST-DISPLAYED-TABLE mach-part
&Scoped-Define DISPLAYED-OBJECTS lv-total-hours 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,DISPLAY-FIELD,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS mach-part.rm-part-code 
&Scoped-define ADM-ASSIGN-FIELDS lv-total-hours 
&Scoped-define DISPLAY-FIELD lv-total-hours 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.mach-part.company
Carrier||y|asi.mach-part.m-code
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,m-code"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-total-hours AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Total Hours Of Use" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mach-part.rm-part-code AT ROW 1.24 COL 27.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-part.rm-part-desc-1 AT ROW 2.24 COL 27.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     mach-part.rm-part-desc-2 AT ROW 3.24 COL 27.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     mach-part.serial-no AT ROW 4.24 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     mach-part.model-no AT ROW 4.24 COL 66.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     mach-part.date-purchased AT ROW 5.24 COL 26.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-part.date-installed AT ROW 5.24 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-part.expected-days AT ROW 6.24 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
     mach-part.expected-hours AT ROW 6.24 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.4 BY 1
     mach-part.expected-impressions AT ROW 7.24 COL 48.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     lv-total-hours AT ROW 8.24 COL 27 COLON-ALIGNED
     mach-part.total-impressions-run AT ROW 8.24 COL 69.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     mach-part.company-installed AT ROW 9.24 COL 26.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     mach-part.person-installed AT ROW 10.29 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.mach-part
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
         HEIGHT             = 12.43
         WIDTH              = 95.4.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-total-hours IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN mach-part.rm-part-code IN FRAME F-Main
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
   DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR look-recid AS RECID NO-UNDO.

   lw-focus  = FOCUS.

   CASE lw-focus:NAME:

       WHEN "rm-part-code" THEN
       DO:
          RUN windows/l-itmall.w (g_company, "","", mach-part.rm-part-code:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
          IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN
             lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
       END.
   END CASE.
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
  {src/adm/template/row-list.i "mach-part"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach-part"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-mach-part V-table-Win 
PROCEDURE enable-mach-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

     IF adm-new-record AND adm-adding-record THEN
        lv-total-hours:SCREEN-VALUE = "0". 

     lv-total-hours:SENSITIVE = YES.
  END.
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

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN lv-total-hours.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (NO).

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
  lv-total-hours:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
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
  {methods/viewers/create/mach-part.i}

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
  /* Code placed here will execute AFTER standard behavior.    */
  /*need field to be decimal*/

  IF AVAIL mach-part THEN
  DO:
     IF mach-part.total-hours-of-use NE 0 THEN
     DO:
        FIND FIRST reftable WHERE
             reftable.reftable EQ "MACHPARTHOURS" AND
             reftable.company  EQ mach-part.company AND
             reftable.loc      EQ mach-part.m-code AND
             reftable.code     EQ mach-part.rm-part-code
             EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
               reftable.reftable = "MACHPARTHOURS"
               reftable.company  = mach-part.company
               reftable.loc      = mach-part.m-code
               reftable.code     = mach-part.rm-part-code.
        END.

        ASSIGN
           reftable.val[1] = reftable.val[1] + mach-part.total-hours-of-use
           lv-total-hours = reftable.val[1].

        DISPLAY lv-total-hours WITH FRAME {&FRAME-NAME}.

        FIND FIRST b-mach-part WHERE ROWID(b-mach-part) EQ ROWID(mach-part)
             EXCLUSIVE-LOCK.

        b-mach-part.total-hours-of-use  = 0.

        RELEASE b-mach-part.
     END.

     IF NOT adm-new-record THEN
        RUN reftable-values (YES).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  DISPLAY lv-total-hours WITH FRAME {&FRAME-NAME}.
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
  lv-total-hours:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
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
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    IF NOT CAN-FIND(FIRST ITEM WHERE
       ITEM.company EQ g_company AND
       ITEM.i-no EQ mach-part.rm-part-code:SCREEN-VALUE) THEN
       DO:
          MESSAGE "Invalid RM Part Code."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY":U TO mach-part.rm-part-code.
          RETURN NO-APPLY.
       END.
  END.
  {&methods/lValidateError.i YES}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  lv-total-hours:SENSITIVE = NO.
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.

  IF AVAIL mach-part THEN DO:

     FIND FIRST reftable WHERE
          reftable.reftable EQ "MACHPARTHOURS" AND
          reftable.company  EQ mach-part.company AND
          reftable.loc      EQ mach-part.m-code AND
          reftable.code     EQ mach-part.rm-part-code
          EXCLUSIVE-LOCK NO-ERROR.

     IF NOT AVAIL reftable THEN DO:
       CREATE reftable.
       ASSIGN
            reftable.reftable = "MACHPARTHOURS"
            reftable.company  = mach-part.company
            reftable.loc      = mach-part.m-code
            reftable.code     = mach-part.rm-part-code.
     END.

     IF ip-display THEN
        lv-total-hours = reftable.val[1].
     ELSE
       reftable.val[1] = lv-total-hours.
  END.
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
  {src/adm/template/sndkycas.i "company" "mach-part" "company"}
  {src/adm/template/sndkycas.i "Carrier" "mach-part" "m-code"}

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
  {src/adm/template/snd-list.i "mach-part"}

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

