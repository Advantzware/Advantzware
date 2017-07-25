&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
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
def var lv-cust-no like cust.cust-no no-undo.
def var lv-cust-name like cust.name no-undo.
{methods/defines/globdefs.i &new="new"}
{methods/defines/hndldefs.i &new="new"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rfq
&Scoped-define FIRST-EXTERNAL-TABLE rfq


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfq.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfq.req-date rfq.due-date rfq.cust-no ~
rfq.fob-code rfq.ship-addr[1] rfq.chg-method rfq.ship-addr[2] rfq.wh-month ~
rfq.ship-city rfq.ship-state rfq.ship-zip rfq.sman rfq.comm rfq.inst ~
rfq.rfq-no rfq.ship-name 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}req-date ~{&FP2}req-date ~{&FP3}~
 ~{&FP1}due-date ~{&FP2}due-date ~{&FP3}~
 ~{&FP1}cust-no ~{&FP2}cust-no ~{&FP3}~
 ~{&FP1}fob-code ~{&FP2}fob-code ~{&FP3}~
 ~{&FP1}ship-addr[1] ~{&FP2}ship-addr[1] ~{&FP3}~
 ~{&FP1}chg-method ~{&FP2}chg-method ~{&FP3}~
 ~{&FP1}ship-addr[2] ~{&FP2}ship-addr[2] ~{&FP3}~
 ~{&FP1}wh-month ~{&FP2}wh-month ~{&FP3}~
 ~{&FP1}ship-city ~{&FP2}ship-city ~{&FP3}~
 ~{&FP1}ship-state ~{&FP2}ship-state ~{&FP3}~
 ~{&FP1}ship-zip ~{&FP2}ship-zip ~{&FP3}~
 ~{&FP1}sman ~{&FP2}sman ~{&FP3}~
 ~{&FP1}comm ~{&FP2}comm ~{&FP3}~
 ~{&FP1}rfq-no ~{&FP2}rfq-no ~{&FP3}~
 ~{&FP1}ship-name ~{&FP2}ship-name ~{&FP3}
&Scoped-define ENABLED-TABLES rfq
&Scoped-define FIRST-ENABLED-TABLE rfq
&Scoped-Define ENABLED-OBJECTS lk-cust-no 
&Scoped-Define DISPLAYED-FIELDS rfq.req-date rfq.due-date rfq.cust-no ~
rfq.fob-code rfq.ship-addr[1] rfq.chg-method rfq.ship-addr[2] rfq.wh-month ~
rfq.ship-city rfq.ship-state rfq.ship-zip rfq.sman rfq.comm rfq.inst ~
rfq.rfq-no rfq.ship-name 
&Scoped-Define DISPLAYED-OBJECTS sman-name 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE BUTTON lk-cust-no 
     IMAGE-UP FILE "adeicon\brws":U
     IMAGE-DOWN FILE "adeicon\browse-i":U
     LABEL "Button 1" 
     SIZE 5 BY 1.14.

DEFINE VARIABLE sman-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfq.req-date AT ROW 1.24 COL 51 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfq.due-date AT ROW 1.24 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     lk-cust-no AT ROW 2.33 COL 29
     rfq.cust-no AT ROW 2.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     rfq.fob-code AT ROW 2.67 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     rfq.ship-addr[1] AT ROW 3.38 COL 17 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     rfq.chg-method AT ROW 3.62 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq.ship-addr[2] AT ROW 4.33 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     rfq.wh-month AT ROW 4.57 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     rfq.ship-city AT ROW 5.29 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     rfq.ship-state AT ROW 5.29 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     rfq.ship-zip AT ROW 5.29 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq.sman AT ROW 6.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     sman-name AT ROW 6.71 COL 27 COLON-ALIGNED NO-LABEL
     rfq.comm AT ROW 6.71 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq.inst AT ROW 9.1 COL 18 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 106 BY 5.71
     rfq.rfq-no AT ROW 1.24 COL 17 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 16 BY .95
     rfq.ship-name AT ROW 2.48 COL 31.8 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 32 BY .95
     "Special Instruction" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 8.14 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfq
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
         HEIGHT             = 14.52
         WIDTH              = 126.2.
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

ASSIGN 
       rfq.inst:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN rfq.ship-addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman-name IN FRAME F-Main
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




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  if valid-handle(persistent-handle) then 
     RUN Run_applhelp IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
  else run browsers/custlook.w  (output lv-cust-no, output lv-cust-name) .
  rfq.cust-no:screen-value = string(lv-cust-no).
  rfq.ship-name:screen-value = lv-cust-name.

  RETURN NO-APPLY.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lk-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lk-cust-no V-table-Win
ON CHOOSE OF lk-cust-no IN FRAME F-Main /* Button 1 */
DO:
    apply "help" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.sman
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
  {src/adm/template/row-list.i "rfq"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfq"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-company V-table-Win 
PROCEDURE assign-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-rfq for rfq.

  find bf-rfq of rfq .
  find first usr where usr.uid = userid(ldbname(1)) no-error.
  if avail usr then do:
     assign bf-rfq.company = usr.company
            bf-rfq.loc = usr.loc
            .


  end.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rfqno V-table-Win 
PROCEDURE get-rfqno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var new-rfq# like rfq.rfq-no no-undo.

  find first rfq-ctrl no-error.
  if avail rfq-ctrl then do:
     new-rfq# = rfq-ctrl.rfq-num.
     rfq-ctrl.rfq-num = rfq-ctrl.rfq-num + 1.
     return string(new-rfq#).
  end.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  find first rfq-ctrl no-lock no-error.
  if not avail rfq-ctrl then do:
     message "No RFQ Control Exist. Please Register RFQ Control File First."
             view-as alert-box.
     return error.
  end.      
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  run get-rfqno.
  rfq.rfq-no:screen-value in frame {&frame-name} = (return-value).
  run assign-company.  /* assign company,loc */

  /* Code placed here will execute AFTER standard behavior.    */
{&methods/lValidateError.i NO}
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
  {src/adm/template/snd-list.i "rfq"}

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


