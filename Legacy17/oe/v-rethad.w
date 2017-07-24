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
{sys/inc/VAR.i NEW SHARED}

DEF BUFFER bf-reth FOR oe-reth.

ASSIGN cocode = g_company
       locode = g_loc.


&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES oe-reth
&Scoped-define FIRST-EXTERNAL-TABLE oe-reth


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-reth.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-reth.return-date oe-reth.tot-freight oe-reth.spare-char-1 
&Scoped-define ENABLED-TABLES oe-reth
&Scoped-define FIRST-ENABLED-TABLE oe-reth
&Scoped-Define ENABLED-OBJECTS RECT-21 RECT-22 RECT-23 RECT-24 RECT-25 
&Scoped-Define DISPLAYED-FIELDS oe-reth.ra-no oe-reth.cust-no ~
oe-reth.inv-no oe-reth.return-date oe-reth.applied oe-reth.posted ~
oe-reth.tot-qty-return oe-reth.tot-tax oe-reth.qty-return-inv ~
oe-reth.tot-freight oe-reth.tot-return-amt oe-reth.tot-cost oe-reth.spare-char-1
&Scoped-define DISPLAYED-TABLES oe-reth
&Scoped-define FIRST-DISPLAYED-TABLE oe-reth
&Scoped-Define DISPLAYED-OBJECTS lv-soldto lv-cname lv-sname lv-caddr ~
lv-saddr lv-caddr2 lv-saddr2 lv-ccity lv-cstate lv-czip lv-scity lv-sstate ~
lv-szip lv-tot-line 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS oe-reth.cust-no oe-reth.inv-no 
&Scoped-define ADM-ASSIGN-FIELDS oe-reth.applied oe-reth.posted ~
oe-reth.tot-qty-return oe-reth.tot-tax oe-reth.qty-return-inv ~
oe-reth.tot-return-amt oe-reth.tot-cost 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.oe-reth.company
r-no||y|ASI.oe-reth.r-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,r-no"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-caddr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-caddr2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ccity AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cstate AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-czip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-saddr2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-scity AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE lv-soldto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sold To" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-sstate AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-szip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tot-line AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Line Item Total" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 5.95.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 5.95.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 5.24.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 5.24.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 14.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-reth.ra-no AT ROW 1.48 COL 13.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-reth.cust-no AT ROW 2.91 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-reth.inv-no AT ROW 1.48 COL 37.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-reth.return-date AT ROW 1.48 COL 68.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-reth.applied AT ROW 1.48 COL 113
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     oe-reth.posted AT ROW 1.48 COL 129
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     lv-soldto AT ROW 2.91 COL 89 COLON-ALIGNED
     lv-cname AT ROW 3.86 COL 19 COLON-ALIGNED NO-LABEL
     lv-sname AT ROW 3.86 COL 89 COLON-ALIGNED NO-LABEL
     lv-caddr AT ROW 4.81 COL 19 COLON-ALIGNED NO-LABEL
     lv-saddr AT ROW 4.81 COL 89 COLON-ALIGNED NO-LABEL
     lv-caddr2 AT ROW 5.76 COL 19 COLON-ALIGNED NO-LABEL
     lv-saddr2 AT ROW 5.76 COL 89 COLON-ALIGNED NO-LABEL
     lv-ccity AT ROW 6.71 COL 19 COLON-ALIGNED NO-LABEL
     lv-cstate AT ROW 6.71 COL 39 COLON-ALIGNED NO-LABEL
     lv-czip AT ROW 6.71 COL 45 COLON-ALIGNED NO-LABEL
     lv-scity AT ROW 6.71 COL 89 COLON-ALIGNED NO-LABEL
     lv-sstate AT ROW 6.71 COL 109 COLON-ALIGNED NO-LABEL
     lv-szip AT ROW 6.71 COL 115 COLON-ALIGNED NO-LABEL
     oe-reth.tot-qty-return AT ROW 9.57 COL 39 COLON-ALIGNED
          LABEL "Total Qty Returned"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     lv-tot-line AT ROW 9.57 COL 94 COLON-ALIGNED
     oe-reth.tot-tax AT ROW 10.52 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     oe-reth.qty-return-inv AT ROW 10.52 COL 39 COLON-ALIGNED
          LABEL "Returned To Inv"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-reth.spare-char-1 AT ROW 1.48 COL 94.2 COLON-ALIGNED
          LABEL "Ref #"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-reth.tot-freight AT ROW 11.48 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     oe-reth.tot-return-amt AT ROW 12.57 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     oe-reth.tot-cost AT ROW 12.57 COL 39 COLON-ALIGNED
          LABEL "Total Cost"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     RECT-21 AT ROW 2.67 COL 8
     RECT-22 AT ROW 2.67 COL 77
     RECT-23 AT ROW 9.33 COL 8
     RECT-24 AT ROW 9.33 COL 75
     RECT-25 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-reth
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 16.86
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX oe-reth.applied IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-reth.cust-no IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN oe-reth.inv-no IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-caddr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-caddr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-ccity IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-cname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-cstate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-czip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-saddr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-scity IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-soldto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-sstate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-szip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-tot-line IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX oe-reth.posted IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-reth.qty-return-inv IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-reth.ra-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-reth.tot-cost IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */ 
/* SETTINGS FOR FILL-IN oe-reth.spare-char-1 IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-reth.tot-qty-return IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-reth.tot-return-amt IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-reth.tot-tax IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
     def var lv-handle as handle no-undo.
     def var char-val as cha no-undo.
     DEF VAR rec-val AS RECID NO-UNDO.

     CASE Focus:name :
       when "inv-no" then do:
             run windows/l-arinv.w 
                (g_company,oe-reth.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} ,focus:screen-value, output char-val, OUTPUT rec-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
             return no-apply.  
       end.
       otherwise do:
             lv-handle = focus:handle.
             run applhelp.p.

             if g_lookup-var <> "" then do:
                lv-handle:screen-value = g_lookup-var.

             end.   /* g_lookup-var <> "" */
             apply "entry" to lv-handle.
             return no-apply.

       end.  /* otherwise */
    end case.  



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-reth.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-reth.cust-no V-table-Win
ON LEAVE OF oe-reth.cust-no IN FRAME F-Main /* Cust # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN validate-cust-no (0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-reth.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-reth.inv-no V-table-Win
ON LEAVE OF oe-reth.inv-no IN FRAME F-Main /* Invoice */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN validate-inv-no (0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-order V-table-Win 
PROCEDURE add-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "oe-reth"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-reth"}

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
 DEF VAR v-r-no LIKE bf-reth.r-no NO-UNDO.
 DEF VAR v-ra-no LIKE bf-reth.ra-no NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  find last bf-reth where bf-reth.company = cocode use-index r-no no-lock no-error.
  if available bf-reth THEN assign v-r-no  = bf-reth.r-no + 1
                                   v-ra-no = bf-reth.ra-no + 1.
  ELSE assign v-r-no  = 1
              v-ra-no = 1.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    assign oe-reth.company  = cocode
           oe-reth.r-no     = v-r-no
           oe-reth.ra-no    = v-ra-no
               oe-reth.applied  = yes
           /*  fil_id           = recid(oe-reth)
             nufile           = yes */ .


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
  IF AVAIL oe-reth THEN do:
      RUN oe/oe-retup.p (RECID(oe-reth)).
      FIND CURRENT oe-reth NO-LOCK.
      lv-tot-line = oe-reth.tot-return-amt - oe-reth.tot-freight - oe-reth.tot-tax.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISPLAY lv-tot-line WITH FRAME {&FRAME-NAME}.

  find first cust where cust.company eq cocode
                     and cust.cust-no eq input oe-reth.cust-no
                     use-index cust no-lock no-error.
   if avail cust then do:

      ASSIGN oe-reth.cust-no:SCREEN-VALUE = cust.cust-no
             lv-cname:SCREEN-VALUE = cust.name
             lv-caddr:SCREEN-VALUE = cust.addr[1]
             lv-caddr2:SCREEN-VALUE = cust.addr[2]
             lv-ccity:SCREEN-VALUE = cust.city
             lv-cstate:SCREEN-VALUE = cust.state
             lv-czip:SCREEN-VALUE = cust.zip
             .   
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
  DEF VAR ll-add-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-cust-no (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN validate-inv-no (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  ll-add-record = adm-adding-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-add-record THEN DO:
     /*{methods/RUN_link.i THIS-PROCEDURE,"container-source","select-page","(3)"} */
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     RUN select-page IN WIDGET-HANDLE(char-hdl) (3). 
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"add-line-target",OUTPUT char-hdl).

     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:

        RUN auto-line-add IN WIDGET-HANDLE(char-hdl).
     END.
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
  {src/adm/template/sndkycas.i "company" "oe-reth" "company"}
  {src/adm/template/sndkycas.i "r-no" "oe-reth" "r-no"}

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
  {src/adm/template/snd-list.i "oe-reth"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-cust-no V-table-Win 
PROCEDURE validate-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-int AS INT NO-UNDO.


  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
  IF ip-int EQ 1                        OR
     oe-reth.cust-no:SCREEN-VALUE NE "" THEN DO:
    find first cust where cust.company eq cocode
                      and cust.cust-no eq oe-reth.cust-no:SCREEN-VALUE
                      use-index cust no-lock no-error.
    if not avail cust then do:
      message "Must enter a valid Customer Number." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-reth.cust-no.
      RETURN ERROR.
    end.

    ASSIGN oe-reth.cust-no:SCREEN-VALUE = cust.cust-no
           lv-cname:SCREEN-VALUE = cust.name
           lv-caddr:SCREEN-VALUE = cust.addr[1]
           lv-caddr2:SCREEN-VALUE = cust.addr[2]
           lv-ccity:SCREEN-VALUE = cust.city
           lv-cstate:SCREEN-VALUE = cust.state
           lv-czip:SCREEN-VALUE = cust.zip.   
  END.
END.  /* frame */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-inv-no V-table-Win 
PROCEDURE validate-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-int AS INT NO-UNDO.


  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
  IF ip-int EQ 1                           OR
     INT(oe-reth.inv-no:SCREEN-VALUE) NE 0 THEN DO:
    find first ar-inv where ar-inv.company eq cocode
                        and ar-inv.posted  eq yes
                        and ar-inv.inv-no  eq INT(oe-reth.inv-no:SCREEN-VALUE)
                        USE-INDEX inv-no NO-LOCK no-error.
    if not avail ar-inv then do:
      message "Must enter a valid invoice" VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" to oe-reth.inv-no.
          RETURN ERROR.
    end.

    ASSIGN oe-reth.cust-no:SCREEN-VALUE = ar-inv.cust-no
           lv-soldto:SCREEN-VALUE = ar-inv.sold-id
           lv-sname:SCREEN-VALUE = ar-inv.sold-name
           lv-saddr:SCREEN-VALUE = ar-inv.sold-addr[1]
           lv-saddr2:SCREEN-VALUE = ar-inv.sold-addr[2]
           lv-scity:SCREEN-VALUE = ar-inv.sold-city
           lv-sstate:SCREEN-VALUE = ar-inv.sold-state
           lv-szip:SCREEN-VALUE = ar-inv.sold-zip.

    IF ip-int EQ 0 THEN RUN validate-cust-no (1).
  END.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

