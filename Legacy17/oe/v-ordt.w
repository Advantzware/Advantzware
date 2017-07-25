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

  File: oe\v-ordt.w

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
{sys/inc/VAR.i NEW SHARED}
ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-fr-tax LIKE oe-ctrl.f-tax INIT NO NO-UNDO.
DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
DEF VAR lv-prev-value AS CHAR NO-UNDO.

{oe/oe-sysct1.i NEW}

&SCOPED-DEFINE proc-enable proc-enable

FIND FIRST ce-ctrl
    WHERE ce-ctrl.company EQ cocode
      AND ce-ctrl.loc     EQ locode
    NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN v-fr-tax = oe-ctrl.f-tax.

DO TRANSACTION:
   {sys/inc/fgsecur.i}
END.

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ord.t-weight oe-ord.t-freight ~
oe-ord.f-bill oe-ord.t-comm 
&Scoped-define ENABLED-TABLES oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ord
&Scoped-Define ENABLED-OBJECTS RECT-31 
&Scoped-Define DISPLAYED-FIELDS oe-ord.t-weight oe-ord.tax oe-ord.t-freight ~
oe-ord.t-revenue oe-ord.f-bill oe-ord.t-comm oe-ord.t-cost 
&Scoped-define DISPLAYED-TABLES oe-ord
&Scoped-define FIRST-DISPLAYED-TABLE oe-ord


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
DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ord.t-weight AT ROW 1.48 COL 17 COLON-ALIGNED
          LABEL "Weight"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.tax AT ROW 1.48 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     oe-ord.t-freight AT ROW 2.43 COL 17 COLON-ALIGNED
          LABEL "Freight"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.t-revenue AT ROW 2.43 COL 85 COLON-ALIGNED
          LABEL "Order Total"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-ord.f-bill AT ROW 2.67 COL 41
          LABEL "Bill Freight?"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     oe-ord.t-comm AT ROW 3.38 COL 17 COLON-ALIGNED
          LABEL "Commissions"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     oe-ord.t-cost AT ROW 3.38 COL 85 COLON-ALIGNED
          LABEL "Order Cost"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RECT-31 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ord
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
         HEIGHT             = 17.43
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX oe-ord.f-bill IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.t-comm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.t-cost IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.t-freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.t-revenue IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.t-weight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.tax IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME oe-ord.f-bill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.f-bill V-table-Win
ON VALUE-CHANGED OF oe-ord.f-bill IN FRAME F-Main /* Bill Freight? */
DO:
  oe-ord.t-revenue:SCREEN-VALUE =
      STRING(DEC(oe-ord.t-revenue:SCREEN-VALUE) +
             (DEC(oe-ord.t-freight:SCREEN-VALUE) *
              (IF oe-ord.f-bill:SCREEN-VALUE EQ "Y" THEN 1 ELSE -1))).

  IF v-fr-tax THEN
    oe-ord.tax:SCREEN-VALUE = 
        STRING(DEC(oe-ord.tax:SCREEN-VALUE) +
               (ROUND(DEC(oe-ord.t-freight:SCREEN-VALUE) * v-frt-tax-rate / 100,2) *
                (IF oe-ord.f-bill:SCREEN-VALUE EQ "Y" THEN 1 ELSE -1))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.t-comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.t-comm V-table-Win
ON ENTRY OF oe-ord.t-comm IN FRAME F-Main /* Commissions */
DO:
  lv-prev-value = oe-ord.t-comm:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.t-comm V-table-Win
ON VALUE-CHANGED OF oe-ord.t-comm IN FRAME F-Main /* Commissions */
DO:
  ASSIGN
   oe-ord.t-cost:SCREEN-VALUE = STRING(DEC(oe-ord.t-cost:SCREEN-VALUE) +
                                       DEC(oe-ord.t-comm:SCREEN-VALUE) -
                                       DEC(lv-prev-value))
   lv-prev-value              = oe-ord.t-comm:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.t-freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.t-freight V-table-Win
ON ENTRY OF oe-ord.t-freight IN FRAME F-Main /* Freight */
DO:
  lv-prev-value = oe-ord.t-freight:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.t-freight V-table-Win
ON VALUE-CHANGED OF oe-ord.t-freight IN FRAME F-Main /* Freight */
DO:
  oe-ord.t-cost:SCREEN-VALUE = STRING(DEC(oe-ord.t-cost:SCREEN-VALUE) +
                                      DEC(oe-ord.t-freight:SCREEN-VALUE) -
                                      DEC(lv-prev-value)).

  IF oe-ord.f-bill:SCREEN-VALUE EQ "Y" THEN DO:
    oe-ord.t-revenue:SCREEN-VALUE =
        STRING(DEC(oe-ord.t-revenue:SCREEN-VALUE) +
               DEC(oe-ord.t-freight:SCREEN-VALUE) -
               DEC(lv-prev-value)).

    IF v-fr-tax THEN
      oe-ord.tax:SCREEN-VALUE = 
          STRING(DEC(oe-ord.tax:SCREEN-VALUE) +
                 ROUND(DEC(oe-ord.t-freight:SCREEN-VALUE) * v-frt-tax-rate / 100,2) -
                 ROUND(DEC(lv-prev-value)                 * v-frt-tax-rate / 100,2)).
  END.

  lv-prev-value = oe-ord.t-freight:SCREEN-VALUE.

  /*IF INT(oe-ord.t-freight:SCREEN-VALUE) NE 0 AND ce-ctrl.shp-add THEN
    MESSAGE "Freight is already included in the factory cost..."
            VIEW-AS ALERT-BOX.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  RUN oe/oe-sysct.p.

  IF NOT v-oecomm-log  THEN RUN hide-comm (YES).

  IF fgsecurity-log THEN
  DO:
     FIND FIRST usergrps WHERE
          usergrps.usergrps = fgsecurity-char
          NO-LOCK NO-ERROR.

     IF AVAIL usergrps AND
        (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
         TRIM(usergrps.users) NE "*") THEN
        ASSIGN
           oe-ord.t-cost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
  END.

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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-freight V-table-Win 
PROCEDURE calc-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  IF AVAIL oe-ord THEN DO WITH FRAME {&FRAME-NAME}:
    lv-prev-value = oe-ord.t-freight:SCREEN-VALUE.
    oe-ord.t-freight:SCREEN-VALUE = "".
    APPLY "value-changed" TO oe-ord.t-freight.
    FOR EACH oe-ordl OF oe-ord:
      RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
      ld = ld + oe-ordl.t-freight.
    END.
    lv-prev-value = oe-ord.t-freight:SCREEN-VALUE.
    oe-ord.t-freight:SCREEN-VALUE = STRING(ld).
    APPLY "value-changed" TO oe-ord.t-freight.
    RUN dispatch ("assign-record").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE oe-ord.t-comm.
  END.

  IF NOT v-oecomm-log  THEN RUN hide-comm (YES).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-comm V-table-Win 
PROCEDURE hide-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-hidden AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    oe-ord.t-comm:HIDDEN = ip-hidden.

    IF NOT ip-hidden THEN DISPLAY oe-ord.t-comm.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  oe-ord.frt-pay = IF oe-ord.f-bill THEN "B" ELSE "P".

  FIND CURRENT oe-ord NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR lv-freight LIKE oe-ord.t-freight NO-UNDO.


  /* Code placed here will execute BEFORE standard behavior.    */
  lv-freight = oe-ord.t-freight.

  FIND FIRST cust
      {sys/ref/custW.i}
        AND cust.cust-no = oe-ord.cust-no
      NO-ERROR.
  IF AVAIL cust THEN
     cust.ord-bal = cust.ord-bal - oe-ord.tax - oe-ord.t-revenue.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&frame-name}:
    ASSIGN
     oe-ord.tax
     oe-ord.t-revenue
     oe-ord.t-cost.
  END.

  IF oe-ord.t-freight NE lv-freight THEN
    RUN oe/ordfrate.p (ROWID(oe-ord)).

  IF AVAIL cust THEN DO:
    IF INDEX("HA",oe-ord.stat) EQ 0 THEN DO:
      /*lv-msg = "".

      /* Check to see if over order limit */
      IF (oe-ord.t-revenue + oe-ord.tax) GT cust.ord-lim THEN
        lv-msg = "order".

      ELSE
      /* Check to see if over credit limit */
      IF (cust.ord-bal + cust.acc-bal + oe-ord.t-revenue + oe-ord.tax) GT cust.cr-lim THEN
        lv-msg = "credit".

      IF lv-msg NE "" THEN DO:
        MESSAGE "Customer (" + trim(cust.NAME) + ")"
                "has exceeded their"
                trim(lv-msg)
                "limit, order status will be set To HOLD..."
                VIEW-AS ALERT-BOX.

        ASSIGN
         cust.cr-hold = YES
         oe-ord.stat = "H".
      END.*/
      RUN oe/creditck.p (ROWID(oe-ord), NO).
    END.

    ELSE cust.ord-bal = cust.ord-bal + oe-ord.tax + oe-ord.t-revenue.

    FIND CURRENT cust NO-LOCK.
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
  RUN disable-fields.

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
  IF AVAIL oe-ord THEN
     RUN ar/cctaxrt.p (cocode, oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

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
  RUN disable-fields.

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

 IF NOT v-oecomm-log THEN  RUN hide-comm (YES).
  IF v-oecomm-log THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE oe-ord.t-comm.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Redisplay V-table-Win 
PROCEDURE Redisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND CURRENT oe-ord NO-LOCK NO-ERROR.
  IF AVAIL oe-ord THEN RUN dispatch ("display-fields").

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
  {src/adm/template/snd-list.i "oe-ord"}

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

