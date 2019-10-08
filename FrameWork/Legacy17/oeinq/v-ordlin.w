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
{custom/globdefs.i}
{sys/inc/var.i new shared }

assign cocode = g_company
       locode = g_loc.
{oe/oe-sysct1.i NEW}
DEF VAR lv-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.est-no oe-ordl.part-no oe-ordl.i-no ~
oe-ordl.i-name oe-ordl.qty oe-ordl.part-dscr1 oe-ordl.price oe-ordl.pr-uom ~
oe-ordl.part-dscr2 oe-ordl.cost oe-ordl.cas-cnt oe-ordl.po-no oe-ordl.tax ~
oe-ordl.disc oe-ordl.req-code oe-ordl.req-date oe-ordl.t-price ~
oe-ordl.prom-code oe-ordl.prom-date oe-ordl.po-no-po oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.vend-no oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.job-no oe-ordl.job-no2 ~
oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3]  ~
oe-ordl.ship-qty 
&Scoped-define ENABLED-TABLES oe-ordl
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-Define ENABLED-OBJECTS RECT-2 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.est-no oe-ordl.part-no oe-ordl.i-no ~
oe-ordl.i-name oe-ordl.qty oe-ordl.part-dscr1 oe-ordl.price oe-ordl.pr-uom ~
oe-ordl.part-dscr2 oe-ordl.cost oe-ordl.cas-cnt oe-ordl.po-no oe-ordl.tax ~
oe-ordl.disc oe-ordl.req-code oe-ordl.req-date oe-ordl.t-price ~
oe-ordl.prom-code oe-ordl.prom-date oe-ordl.po-no-po oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.vend-no oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.job-no oe-ordl.job-no2 ~
oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3] get-inv-qty () @ lv-inv-qty ~
oe-ordl.ship-qty 
&Scoped-define DISPLAYED-TABLES oe-ordl
&Scoped-define FIRST-DISPLAYED-TABLE oe-ordl


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-inv-qty V-table-Win 
FUNCTION get-inv-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.oe-ordl.company
j-no||y|ASI.oe-ordl.j-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,j-no"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 145 BY 13.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.est-no AT ROW 1.24 COL 24 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ordl.part-no AT ROW 1.24 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     oe-ordl.i-no AT ROW 2.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ordl.i-name AT ROW 2.43 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     oe-ordl.qty AT ROW 3.62 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ordl.part-dscr1 AT ROW 3.62 COL 81 COLON-ALIGNED
          LABEL "Part Description"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     oe-ordl.price AT ROW 4.81 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     oe-ordl.pr-uom AT ROW 4.81 COL 54 COLON-ALIGNED
          LABEL "UOM"
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     oe-ordl.part-dscr2 AT ROW 4.81 COL 81 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     oe-ordl.cost AT ROW 6 COL 24 COLON-ALIGNED
          LABEL "Cost/M"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     oe-ordl.cas-cnt AT ROW 6 COL 57 COLON-ALIGNED
          LABEL "Count"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ordl.po-no AT ROW 6 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.tax AT ROW 6 COL 133 COLON-ALIGNED
          LABEL "Taxable"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ordl.disc AT ROW 7.19 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     oe-ordl.req-code AT ROW 7.19 COL 81 COLON-ALIGNED
          LABEL "Requested"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     oe-ordl.req-date AT ROW 7.19 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ordl.t-price AT ROW 8.38 COL 24 COLON-ALIGNED
          LABEL "Extended Price"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     oe-ordl.prom-code AT ROW 8.38 COL 81 COLON-ALIGNED
          LABEL "Promised"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     oe-ordl.prom-date AT ROW 8.38 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ordl.po-no-po AT ROW 9.57 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.s-man[1] AT ROW 9.57 COL 81 COLON-ALIGNED
          LABEL "SalesRep 1"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ordl.s-pct[1] AT ROW 9.57 COL 103 COLON-ALIGNED
          LABEL "Sale% 1"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[1] AT ROW 9.57 COL 128 COLON-ALIGNED
          LABEL "Comm% 1"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.vend-no AT ROW 10.76 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.s-man[2] AT ROW 10.76 COL 81 COLON-ALIGNED
          LABEL "SalesRep 2"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ordl.s-pct[2] AT ROW 10.76 COL 103 COLON-ALIGNED
          LABEL "Sale% 2"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[2] AT ROW 10.76 COL 128 COLON-ALIGNED
          LABEL "Comm% 2"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ordl.job-no AT ROW 11.95 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ordl.job-no2 AT ROW 11.95 COL 43 COLON-ALIGNED
          LABEL "-"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-ordl.s-man[3] AT ROW 11.95 COL 81 COLON-ALIGNED
          LABEL "SalesRep 3"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ordl.s-pct[3] AT ROW 11.95 COL 103 COLON-ALIGNED
          LABEL "Sale% 3"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ordl.s-comm[3] AT ROW 11.95 COL 128 COLON-ALIGNED
          LABEL "Comm% 3"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     lv-inv-qty AT ROW 13.14 COL 24 COLON-ALIGNED
          LABEL "Quantity Invoiced"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     oe-ordl.ship-qty AT ROW 13.14 COL 81 COLON-ALIGNED
          LABEL "Quantity Shipped"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RECT-2 AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl
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
         HEIGHT             = 13.81
         WIDTH              = 146.2.
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

/* SETTINGS FOR FILL-IN oe-ordl.cas-cnt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ordl.inv-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.pr-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-man[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-man[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-man[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-pct[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-pct[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-pct[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.ship-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.t-price IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.tax IN FRAME F-Main
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
RUN oe/oe-sysct.p.

 IF NOT v-oecomm-log THEN RUN show-comm (NO).

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cocode AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

 cocode = oe-ordl.company.

 DO TRANSACTION:
    {sys/inc/fgsecur.i}
 END.

 IF fgsecurity-log THEN
  DO:
     FIND FIRST usergrps WHERE
          usergrps.usergrps = fgsecurity-char
          NO-LOCK NO-ERROR.

     IF AVAIL usergrps AND
        (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
         TRIM(usergrps.users) NE "*") THEN
        ASSIGN
           oe-ordl.cost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
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
  {src/adm/template/sndkycas.i "company" "oe-ordl" "company"}
  {src/adm/template/sndkycas.i "j-no" "oe-ordl" "j-no"}

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
  {src/adm/template/snd-list.i "oe-ordl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm B-table-Win 
PROCEDURE show-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-visible AS LOGICAL NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     oe-ordl.s-pct[1]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
     oe-ordl.s-pct[2]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
     oe-ordl.s-pct[3]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
     oe-ordl.s-comm[1]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
     oe-ordl.s-comm[2]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible
     oe-ordl.s-comm[3]:VISIBLE IN FRAME {&FRAME-NAME} = ip-visible.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-inv-qty V-table-Win 
FUNCTION get-inv-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lp-inv-qty AS INT NO-UNDO.

  ASSIGN lp-inv-qty = 0.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  FOR EACH ar-invl  WHERE
      ar-invl.company EQ oe-ordl.company AND
      ar-invl.ord-no EQ oe-ordl.ord-no AND
      ar-invl.i-no EQ oe-ordl.i-no
      NO-LOCK:

      lp-inv-qty = lp-inv-qty + ar-invl.inv-qty.
  END.

  RETURN lp-inv-qty.

  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
