&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
def var li-new-estnum like ce-ctrl.e-num no-undo.
def var ll-new-record as log no-undo.
DEF BUFFER recalc-mr FOR reftable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS est.est-no est.est-date est.mod-date ~
est.ord-no est.ord-date eb.cust-no eb.ship-id eb.ship-name eb.part-no ~
eb.ship-addr[1] eb.part-dscr1 eb.ship-addr[2] eb.part-dscr2 eb.ship-city ~
eb.ship-state eb.ship-zip eb.stock-no eb.plate-no eb.procat eb.die-no ~
eb.sman eb.comm eb.cad-no est.est-qty[1] est.est-qty[2] est.est-qty[3] ~
est.est-qty[4] eb.upc-no est.metric eb.spc-no eb.style eb.sty-lock eb.len ~
eb.wid eb.dep eb.dust eb.lock eb.fpanel eb.k-len eb.k-wid eb.adhesive ~
eb.tuck eb.gluelap eb.lin-in eb.t-len eb.t-wid eb.t-sqin 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}est-no ~{&FP2}est-no ~{&FP3}~
 ~{&FP1}est-date ~{&FP2}est-date ~{&FP3}~
 ~{&FP1}mod-date ~{&FP2}mod-date ~{&FP3}~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}ord-date ~{&FP2}ord-date ~{&FP3}~
 ~{&FP1}cust-no ~{&FP2}cust-no ~{&FP3}~
 ~{&FP1}ship-id ~{&FP2}ship-id ~{&FP3}~
 ~{&FP1}ship-name ~{&FP2}ship-name ~{&FP3}~
 ~{&FP1}part-no ~{&FP2}part-no ~{&FP3}~
 ~{&FP1}ship-addr[1] ~{&FP2}ship-addr[1] ~{&FP3}~
 ~{&FP1}part-dscr1 ~{&FP2}part-dscr1 ~{&FP3}~
 ~{&FP1}ship-addr[2] ~{&FP2}ship-addr[2] ~{&FP3}~
 ~{&FP1}part-dscr2 ~{&FP2}part-dscr2 ~{&FP3}~
 ~{&FP1}ship-city ~{&FP2}ship-city ~{&FP3}~
 ~{&FP1}ship-state ~{&FP2}ship-state ~{&FP3}~
 ~{&FP1}ship-zip ~{&FP2}ship-zip ~{&FP3}~
 ~{&FP1}stock-no ~{&FP2}stock-no ~{&FP3}~
 ~{&FP1}plate-no ~{&FP2}plate-no ~{&FP3}~
 ~{&FP1}procat ~{&FP2}procat ~{&FP3}~
 ~{&FP1}die-no ~{&FP2}die-no ~{&FP3}~
 ~{&FP1}sman ~{&FP2}sman ~{&FP3}~
 ~{&FP1}comm ~{&FP2}comm ~{&FP3}~
 ~{&FP1}cad-no ~{&FP2}cad-no ~{&FP3}~
 ~{&FP1}est-qty[1] ~{&FP2}est-qty[1] ~{&FP3}~
 ~{&FP1}est-qty[2] ~{&FP2}est-qty[2] ~{&FP3}~
 ~{&FP1}est-qty[3] ~{&FP2}est-qty[3] ~{&FP3}~
 ~{&FP1}est-qty[4] ~{&FP2}est-qty[4] ~{&FP3}~
 ~{&FP1}upc-no ~{&FP2}upc-no ~{&FP3}~
 ~{&FP1}metric ~{&FP2}metric ~{&FP3}~
 ~{&FP1}spc-no ~{&FP2}spc-no ~{&FP3}~
 ~{&FP1}style ~{&FP2}style ~{&FP3}~
 ~{&FP1}sty-lock ~{&FP2}sty-lock ~{&FP3}~
 ~{&FP1}len ~{&FP2}len ~{&FP3}~
 ~{&FP1}wid ~{&FP2}wid ~{&FP3}~
 ~{&FP1}dep ~{&FP2}dep ~{&FP3}~
 ~{&FP1}dust ~{&FP2}dust ~{&FP3}~
 ~{&FP1}lock ~{&FP2}lock ~{&FP3}~
 ~{&FP1}fpanel ~{&FP2}fpanel ~{&FP3}~
 ~{&FP1}k-len ~{&FP2}k-len ~{&FP3}~
 ~{&FP1}k-wid ~{&FP2}k-wid ~{&FP3}~
 ~{&FP1}adhesive ~{&FP2}adhesive ~{&FP3}~
 ~{&FP1}tuck ~{&FP2}tuck ~{&FP3}~
 ~{&FP1}gluelap ~{&FP2}gluelap ~{&FP3}~
 ~{&FP1}lin-in ~{&FP2}lin-in ~{&FP3}~
 ~{&FP1}t-len ~{&FP2}t-len ~{&FP3}~
 ~{&FP1}t-wid ~{&FP2}t-wid ~{&FP3}~
 ~{&FP1}t-sqin ~{&FP2}t-sqin ~{&FP3}
&Scoped-define ENABLED-TABLES est eb
&Scoped-define FIRST-ENABLED-TABLE est
&Scoped-define SECOND-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-7 
&Scoped-Define DISPLAYED-FIELDS est.est-no est.est-date est.mod-date ~
est.ord-no est.ord-date eb.cust-no eb.ship-id eb.ship-name eb.part-no ~
eb.ship-addr[1] eb.part-dscr1 eb.ship-addr[2] eb.part-dscr2 eb.ship-city ~
eb.ship-state eb.ship-zip eb.stock-no eb.plate-no eb.procat eb.die-no ~
eb.sman eb.comm eb.cad-no est.est-qty[1] est.est-qty[2] est.est-qty[3] ~
est.est-qty[4] eb.upc-no est.metric eb.spc-no eb.style eb.sty-lock eb.len ~
eb.wid eb.dep eb.dust eb.lock eb.fpanel eb.k-len eb.k-wid eb.adhesive ~
eb.tuck eb.gluelap eb.lin-in eb.t-len eb.t-wid eb.t-sqin 
&Scoped-Define DISPLAYED-OBJECTS procat_desc sman_name style_dscr 

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
DEFINE VARIABLE procat_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sman_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 17.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     est.est-no AT ROW 1.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     est.est-date AT ROW 1.24 COL 47 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     est.mod-date AT ROW 1.24 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     est.ord-no AT ROW 1.24 COL 113 COLON-ALIGNED
          LABEL "Last Order#"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     est.ord-date AT ROW 1.24 COL 127 COLON-ALIGNED
          LABEL "On"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     eb.cust-no AT ROW 2.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.ship-id AT ROW 2.19 COL 47 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.ship-name AT ROW 3.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.part-no AT ROW 3.38 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.ship-addr[1] AT ROW 4.1 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.part-dscr1 AT ROW 4.33 COL 91 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.ship-addr[2] AT ROW 5.05 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.part-dscr2 AT ROW 5.29 COL 91 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.ship-city AT ROW 6 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.ship-state AT ROW 6 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.ship-zip AT ROW 6 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.stock-no AT ROW 6.24 COL 91 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.plate-no AT ROW 7.19 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.procat AT ROW 7.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     procat_desc AT ROW 7.43 COL 25 COLON-ALIGNED NO-LABEL
     eb.die-no AT ROW 8.14 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.sman AT ROW 8.38 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     sman_name AT ROW 8.38 COL 23 COLON-ALIGNED NO-LABEL
     eb.comm AT ROW 8.62 COL 55.4 COLON-ALIGNED
          LABEL "%"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.cad-no AT ROW 9.1 COL 112 RIGHT-ALIGNED
          LABEL "CAD/Sample#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     est.est-qty[1] AT ROW 9.57 COL 17 COLON-ALIGNED
          LABEL "Qty"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     est.est-qty[2] AT ROW 9.57 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     est.est-qty[3] AT ROW 9.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     est.est-qty[4] AT ROW 9.57 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.upc-no AT ROW 10.05 COL 91 COLON-ALIGNED
          LABEL "UPC#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     est.metric AT ROW 10.52 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.spc-no AT ROW 11.05 COL 91 COLON-ALIGNED
          LABEL "SPC/QC Code"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.style AT ROW 12.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     style_dscr AT ROW 12.19 COL 24 COLON-ALIGNED NO-LABEL
     eb.sty-lock AT ROW 12.19 COL 91 COLON-ALIGNED
          LABEL "Override/lock?"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.len AT ROW 13.38 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.wid AT ROW 13.38 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dep AT ROW 13.38 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dust AT ROW 14.33 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lock AT ROW 14.33 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.fpanel AT ROW 14.33 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-len AT ROW 15.29 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-wid AT ROW 15.29 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.adhesive AT ROW 15.29 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.tuck AT ROW 16.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.gluelap AT ROW 16.24 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lin-in AT ROW 16.24 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     eb.t-len AT ROW 17.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-wid AT ROW 17.19 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-sqin AT ROW 17.19 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.eb
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
         HEIGHT             = 17.38
         WIDTH              = 144.
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

/* SETTINGS FOR FILL-IN eb.cad-no IN FRAME F-Main
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN eb.comm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.est-qty[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.ord-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.ord-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN procat_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.spc-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.sty-lock IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.upc-no IN FRAME F-Main
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
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-childrecord V-table-Win 
PROCEDURE create-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def buffer bb for eb.
  
  create ef.
  assign
   ef.est-type  = 1
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-len   = ce-ctrl.ls-length
   ef.lsh-wid   = ce-ctrl.ls-width.

  for each bb where bb.e-num = 0 or bb.e-num = est.e-num:
      message "BB crt1 : " bb.est-no bb.e-num view-as alert-box.
  end.



  create eb.
  assign  eb.est-type = 1
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = integer(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0.

  /* ???? bugs : 2 records are created  , delete one ========== */
  for each bb where bb.e-num = 0 :
      delete bb.
  end.
  /*========*/
  find first item where item.company = gcompany
                    and item.mat-type = "C"  /* Case/Bundle */
                    and item.i-no eq eb.cas-no
      no-lock no-error.
  if avail item then do:
     find first e-item where e-item.company eq item.company
                         and e-item.loc     eq item.loc
                         and e-item.i-no    eq item.i-no
        no-lock no-error.
     find first itemfg  where itemfg.company eq gcompany
                          and itemfg.i-no    eq eb.stock-no
        no-lock no-error.
     if avail e-item then
        assign  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = if avail itemfg then itemfg.case-count else e-item.box-case
                .
     if eb.cas-len eq 0 then eb.cas-len = item.case-l.
     if eb.cas-wid eq 0 then eb.cas-wid = item.case-w.
     if eb.cas-dep eq 0 then eb.cas-dep = item.case-d.
     if eb.cas-wt  eq 0 then eb.cas-wt  = item.avg-w.
     if eb.cas-pal eq 0 then eb.cas-pal = item.case-pall.
     if eb.cas-cnt eq 0 then eb.cas-cnt =
              if avail itemfg then itemfg.case-count else item.box-case.
  end.  /* avail item */

  i = 1.


  for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:
      create est-prep.
      assign est-prep.e-num  = est.e-num
             est-prep.est-no = est.est-no
             est-prep.line   = i
             est-prep.s-num  = 1
             est-prep.b-num  = 1
             est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in
                               else if prep.mat-type eq "b" and  avail ef   
                               then ef.adh-sqin
                        else 1  /* mat-type eq "m" */
            est-prep.code   = prep.code
            est-prep.dscr   = prep.dscr
            est-prep.cost   = prep.cost
            est-prep.ml     = prep.ml
            est-prep.simon  = prep.simon
            est-prep.mkup   = prep.mkup
            est-prep.amtz   = prep.amtz
            est-prep.mat-type = prep.mat-type.
            if lookup(est-prep.mat-type, "p,f") gt 0 then
               run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty).
            i = i + 1.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-enum like est.e-num no-undo.
  def var cocode as cha no-undo.
  def buffer bf-est for est.
  def buffer bb for eb.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  
  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           no-lock.
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = yes.
    
  assign est.est-type = 1
         est.company = gcompany
         est.loc = gloc
         est.e-num = li-enum + 1
         est.est-no = string(li-new-estnum,">>>>9")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         .
   display est.est-no est.est-date with frame {&frame-name}.
            
   assign cocode = gcompany
         .      

   {sys/ref/est-add.i est}     

   run create-childrecord.  /* create ef,eb,est-prep */

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

  if ll-new-record then do:
     find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           exclusive-lock.
     ce-ctrl.e-num = li-new-estnum.
  end.
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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "eb"}

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


