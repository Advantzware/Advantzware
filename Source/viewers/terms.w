&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asitest166       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/terms.w

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
&scoped-define proc-enable proc-enable
&SCOPED-DEFINE CommonFile_is_Running
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}

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
&Scoped-define EXTERNAL-TABLES terms
&Scoped-define FIRST-EXTERNAL-TABLE terms


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR terms.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS terms.dscr terms.disc-rate terms.disc-days ~
terms.net-days terms.dueOnMonth terms.dueOnDay
&Scoped-define ENABLED-TABLES terms
&Scoped-define FIRST-ENABLED-TABLE terms
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS terms.t-code terms.dscr terms.disc-rate ~
terms.disc-days terms.net-days terms.dueOnMonth terms.dueOnDay
&Scoped-define DISPLAYED-TABLES terms
&Scoped-define FIRST-DISPLAYED-TABLE terms
&Scoped-Define DISPLAYED-OBJECTS termsCOD 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS terms.t-code 

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
     SIZE 71 BY 3.81.

DEFINE VARIABLE termsCOD AS LOGICAL INITIAL no 
     LABEL "COD" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     terms.t-code AT ROW 1.24 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     terms.dscr AT ROW 1.24 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     terms.disc-rate AT ROW 2.43 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     terms.disc-days AT ROW 2.43 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     terms.net-days AT ROW 2.43 COL 64 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FONT 4
     termsCOD AT ROW 3.62 COL 16.4 HELP
          "Is Terms Code COD?"
     terms.dueOnMonth AT ROW 3.62 COL 41.7 COLON-ALIGNED
          VIEW-AS FILL-IN
          SIZE 5 BY 1
          BGCOLOR 15 FONT 4
     terms.dueOnDay AT ROW 3.62 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN
          SIZE 5 BY 1
          BGCOLOR 15 FONT 4
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.terms
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
         HEIGHT             = 3.81
         WIDTH              = 71.
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

/* SETTINGS FOR FILL-IN terms.net-days IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN terms.t-code IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX termsCOD IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN terms.dueOnMonth IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN terms.dueOnDay IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME terms.t-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms.t-code V-table-Win
ON LEAVE OF terms.t-code IN FRAME F-Main /* Terms */
DO: 
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    IF LASTKEY EQ -1 THEN Return .
     {&methods/lValidateError.i YES}
     if terms.t-code:screen-value EQ "CASH" 
     then do:
        message "CASH is reserved for Cash Only process when entering an invoice through OB1 as a cash sale." view-as alert-box error.
        return no-apply.     
     end.
     {&methods/lValidateError.i NO}
     
   RUN valid-terms-code(OUTPUT lCheckError) NO-ERROR.
   IF lCheckError THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME termsCOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL termsCOD V-table-Win
ON VALUE-CHANGED OF termsCOD IN FRAME F-Main /* COD */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME terms.dueOnMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms.dueOnMonth V-table-Win
ON VALUE-CHANGED OF terms.dueOnMonth IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN Return .
     {&methods/lValidateError.i YES}
    IF INTEGER(terms.dueOnMonth:SCREEN-VALUE) GT 0 THEN do:
        ASSIGN terms.net-days:SCREEN-VALUE = "999" .
        DISABLE terms.net-days WITH FRAME {&FRAME-NAME} .
    END.
    ELSE DO:
         ENABLE terms.net-days WITH FRAME {&FRAME-NAME} . 
    END.
     
     {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME terms.dueOnDay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms.dueOnDay V-table-Win
ON VALUE-CHANGED OF terms.dueOnDay IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN Return .
      
     {&methods/lValidateError.i YES}
         IF INTEGER(terms.dueOnMonth:SCREEN-VALUE) LE 0 THEN do:
             ASSIGN terms.dueOnDay:SCREEN-VALUE = "" .
         END.
     {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME terms.dueOnDay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms.dueOnDay V-table-Win
ON LEAVE OF terms.dueOnDay IN FRAME F-Main
DO:
    DEFIN VARIABLE lCheckError AS LOGICAL NO-UNDO .
    IF LASTKEY EQ -1 THEN Return .
    RUN valid-day(OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "terms"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "terms"}

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
  DEFINE VARIABLE saveTermsCOD AS LOGICAL NO-UNDO.
  
  saveTermsCOD = termsCOD.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*  FIND FIRST reftable EXCLUSIVE-LOCK                */
/*       WHERE reftable.reftable EQ 'terms.cod'       */
/*         AND reftable.company EQ terms.company      */
/*         AND reftable.loc EQ ''                     */
/*         AND reftable.code EQ terms.t-code NO-ERROR.*/
/*  IF NOT AVAILABLE reftable THEN DO:                */
/*    CREATE reftable.                                */
/*    ASSIGN                                          */
/*      reftable.reftable = 'terms.cod'               */
/*      reftable.company = terms.company              */
/*      reftable.code = terms.t-code.                 */
/*  END.                                              */
/*  ASSIGN                                            */
/*    termsCOD = saveTermsCOD                         */
/*    reftable.val[1] = INT(termsCOD).                */
/*  FIND CURRENT reftable NO-LOCK.                    */
  
/*  IF terms is <> "" THEN*/
  ASSIGN
    termsCOD = saveTermsCOD
    terms.cod = IF INT(termsCOD) EQ 1 THEN TRUE ELSE FALSE.
    IF terms.dueOnMonth NE 0 AND terms.dueOnDay EQ 0 THEN do:
        ASSIGN terms.dueOnDay = DYNAMIC-FUNCTION("sfCommon_GetNumberOfDaysInMonth", terms.dueOnMonth ).
               terms.dueOnDay:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(terms.dueOnDay) .
    END.
  
  FIND CURRENT terms NO-LOCK.
  DISABLE termsCOD WITH FRAME {&FRAME-NAME}.
  DISPLAY termsCOD WITH FRAME {&FRAME-NAME}.

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
  {methods/viewers/create/terms.i}

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
  DO WITH FRAME {&FRAME-NAME}:
    {custom/getcmpny.i}
/*    FIND reftable NO-LOCK                                          */
/*         WHERE reftable.reftable EQ 'terms.cod'                    */
/*           AND reftable.company EQ gcompany                        */
/*           AND reftable.loc EQ ''                                  */
/*           AND reftable.code EQ terms.t-code:SCREEN-VALUE NO-ERROR.*/
/*    termsCOD = AVAILABLE reftable AND reftable.val[1] EQ 1.*/
      termsCOD = IF AVAILABLE terms THEN terms.cod ELSE FALSE.
    DISPLAY termsCOD.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFIN VARIABLE lCheckError AS LOGICAL NO-UNDO .
   DO WITH FRAME {&FRAME-NAME}:
   {&methods/lValidateError.i YES}
    IF terms.t-code:SCREEN-VALUE EQ "CASH" AND adm-new-record 
     THEN DO:
        MESSAGE "CASH is reserved for Cash Only process when entering an invoice through OB1 as a cash sale." view-as alert-box error.
        APPLY "entry" TO terms.t-code .
        RETURN NO-APPLY.     
     END.
    {&methods/lValidateError.i NO}
   END.
   
   RUN valid-terms-code(OUTPUT lCheckError) NO-ERROR.
   IF lCheckError THEN RETURN NO-APPLY.

   RUN valid-day(OUTPUT lCheckError) NO-ERROR.
   IF lCheckError THEN RETURN NO-APPLY.

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

  DO WITH FRAME {&FRAME-NAME}:    
    IF INTEGER(terms.dueOnMonth:SCREEN-VALUE) GT 0 THEN do:
        ASSIGN terms.net-days:SCREEN-VALUE = "999" .
        DISABLE terms.net-days WITH FRAME {&FRAME-NAME} .
    END.
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
  {src/adm/template/snd-list.i "terms"}

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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-day V-table-Win 
PROCEDURE valid-day :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE iCountDayInMonth AS INTEGER NO-UNDO .

    {&methods/lValidateError.i YES}
     DO WITH FRAME {&FRAME-NAME}:
         IF INTEGER(terms.dueOnMonth:SCREEN-VALUE) GT 0 THEN do:
             iCountDayInMonth = DYNAMIC-FUNCTION("sfCommon_GetNumberOfDaysInMonth", INTEGER(terms.dueOnMonth:SCREEN-VALUE) ).
             IF integer(terms.dueOnDay:SCREEN-VALUE) GT iCountDayInMonth  THEN DO:
                 MESSAGE "Day is not valid for this month" VIEW-AS ALERT-BOX INFO .
                 oplReturnError = YES .
             END.
          END.
          ELSE terms.dueOnDay:SCREEN-VALUE = "0" .
     END.
     {&methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-terms-code V-table-Win 
PROCEDURE valid-terms-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE iCountDayInMonth AS INTEGER NO-UNDO .
    DEFINE BUFFER bf-terms FOR terms.
           
    {&methods/lValidateError.i YES}
     DO WITH FRAME {&FRAME-NAME}:
      IF adm-new-record THEN
      DO:         
         IF terms.t-code:SCREEN-VALUE EQ "" THEN do:                
                 MESSAGE "Please enter terms code.." VIEW-AS ALERT-BOX INFO .
                 APPLY "entry" TO terms.t-code.
                 oplReturnError = YES .             
         END.
         ELSE DO:
             FIND FIRST bf-terms NO-LOCK
                  WHERE bf-terms.company EQ g_company 
                  AND bf-terms.t-code EQ  terms.t-code:SCREEN-VALUE
                  AND (ROWID(bf-terms) NE ROWID(terms) OR NOT adm-adding-record)  NO-ERROR .
                  
             IF AVAIL bf-terms THEN 
             DO:
                 RUN displayMessage ( INPUT "50").
                 oplReturnError = YES . 
                 APPLY "entry" TO terms.t-code.
             END.
         END.
      END.
     END.
     {&methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


