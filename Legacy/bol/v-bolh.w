&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
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

{custom/gcompany.i}
{custom/gloc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES bolh
&Scoped-define FIRST-EXTERNAL-TABLE bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR bolh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bolh.bol-no bolh.bol-date bolh.carrier ~
bolh.bol-status bolh.sold-id bolh.frt-pay bolh.ship-id bolh.tot-pallets ~
bolh.cust-no bolh.freight bolh.cwt bolh.tot-qty bolh.tot-wt 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}bol-no ~{&FP2}bol-no ~{&FP3}~
 ~{&FP1}bol-date ~{&FP2}bol-date ~{&FP3}~
 ~{&FP1}carrier ~{&FP2}carrier ~{&FP3}~
 ~{&FP1}bol-status ~{&FP2}bol-status ~{&FP3}~
 ~{&FP1}sold-id ~{&FP2}sold-id ~{&FP3}~
 ~{&FP1}ship-id ~{&FP2}ship-id ~{&FP3}~
 ~{&FP1}tot-pallets ~{&FP2}tot-pallets ~{&FP3}~
 ~{&FP1}cust-no ~{&FP2}cust-no ~{&FP3}~
 ~{&FP1}freight ~{&FP2}freight ~{&FP3}~
 ~{&FP1}cwt ~{&FP2}cwt ~{&FP3}~
 ~{&FP1}tot-qty ~{&FP2}tot-qty ~{&FP3}~
 ~{&FP1}tot-wt ~{&FP2}tot-wt ~{&FP3}
&Scoped-define ENABLED-TABLES bolh
&Scoped-define FIRST-ENABLED-TABLE bolh
&Scoped-Define ENABLED-OBJECTS RECT-30 RECT-32 RECT-28 RECT-29 
&Scoped-Define DISPLAYED-FIELDS bolh.bol-no bolh.bol-date bolh.carrier ~
bolh.bol-status bolh.sold-id bolh.frt-pay bolh.ship-id bolh.tot-pallets ~
bolh.cust-no bolh.freight bolh.cwt bolh.tot-qty bolh.tot-wt 
&Scoped-Define DISPLAYED-OBJECTS lv-baddr1 lv-saddr1 lv-baddr2 lv-saddr2 ~
lv-baddr3 lv-saddr3 lv-baddr4 lv-saddr4 

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
DEFINE VARIABLE lv-baddr1 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-baddr2 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-baddr3 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-baddr4 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr1 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr2 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr3 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr4 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44 BY 5.71.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44 BY 5.71.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 5.71.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 134 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bolh.bol-no AT ROW 1.24 COL 24 COLON-ALIGNED
          LABEL "Bill of Lading No"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     bolh.bol-date AT ROW 1.24 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     bolh.carrier AT ROW 1.24 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     bolh.bol-status AT ROW 1.24 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     bolh.sold-id AT ROW 2.19 COL 24 COLON-ALIGNED
          LABEL "Distribution Center"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     bolh.frt-pay AT ROW 2.19 COL 82 COLON-ALIGNED
          LABEL "Freight Payment Method" FORMAT "x(12)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Prepaid","Collect","Bill","Third Party" 
          SIZE 16 BY 1
     bolh.ship-id AT ROW 3.86 COL 59 COLON-ALIGNED
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1.14
     bolh.tot-pallets AT ROW 3.86 COL 114 COLON-ALIGNED
          LABEL "Pallets"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     bolh.cust-no AT ROW 4.1 COL 15 COLON-ALIGNED
          LABEL "Bill To"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     bolh.freight AT ROW 4.81 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     lv-baddr1 AT ROW 5.05 COL 15 COLON-ALIGNED NO-LABEL
     lv-saddr1 AT ROW 5.05 COL 59 COLON-ALIGNED NO-LABEL
     bolh.cwt AT ROW 5.76 COL 114 COLON-ALIGNED
          LABEL "Rate / 100 Wt"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     lv-baddr2 AT ROW 6 COL 15 COLON-ALIGNED NO-LABEL
     lv-saddr2 AT ROW 6 COL 59 COLON-ALIGNED NO-LABEL
     bolh.tot-qty AT ROW 6.71 COL 114 COLON-ALIGNED
          LABEL "Qty"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY .95
     lv-baddr3 AT ROW 6.95 COL 15 COLON-ALIGNED NO-LABEL
     lv-saddr3 AT ROW 6.95 COL 59 COLON-ALIGNED NO-LABEL
     bolh.tot-wt AT ROW 7.67 COL 114 COLON-ALIGNED
          LABEL "Weight"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     lv-baddr4 AT ROW 7.91 COL 15 COLON-ALIGNED NO-LABEL
     lv-saddr4 AT ROW 7.91 COL 59 COLON-ALIGNED NO-LABEL
     "Total" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.38 COL 105
     RECT-30 AT ROW 3.62 COL 96
     RECT-32 AT ROW 1 COL 1
     RECT-28 AT ROW 3.62 COL 5
     RECT-29 AT ROW 3.62 COL 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.bolh
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN bolh.bol-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.cwt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX bolh.frt-pay IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN lv-baddr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-baddr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-baddr3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-baddr4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN bolh.ship-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.sold-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.tot-pallets IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.tot-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bolh.tot-wt IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  case bolh.frt-pay:screen-value in frame {&frame-name}:
          when "Bill" then oe-bolh.frt-pay = "B".
          when "Prepaid" then oe-bolh.frt-pay = "P".
          when "Collect" then oe-bolh.frt-pay = "C".
          when "Third Party" then oe-bolh.frt-pay = "T".
     end.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var x as int no-undo.
  def var nxt-bol as int no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  x = 1.
  find last bolh use-index b-no no-lock no-error.
  if avail bolh then x = bolh.b-no + 1.
  find first oe-ctrl where oe-ctrl.company = gcompany exclusive-lock no-wait no-error.
  if avail oe-ctrl and not locked oe-ctrl then do:
     assign nxt-bol = oe-ctrl.n-bol
            oe-ctrl.n-bol = oe-ctrl.n-bol + 1.
  end.
  else if locked oe-ctrl then do:
       message "Order Entry Control File is in use. Try again in a few seconds."
                view-as alert-box error.
       return.
              
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  assign bolh.company = gcompany
         bolh.loc = gloc
         bolh.b-no = x
         bolh.bol-no = nxt-bol
         bolh.bol-date = today
         .
  display bolh.bol-no bolh.bol-date with frame {&frame-name}.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "bolh"}

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


