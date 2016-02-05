&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
&Scoped-define EXTERNAL-TABLES eb ef est-qty
&Scoped-define FIRST-EXTERNAL-TABLE eb


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb, ef, est-qty.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.stock-no eb.part-dscr1 eb.part-no eb.style ~
eb.procat est-qty.eqty ef.board ef.brd-dscr ef.cal eb.i-col eb.i-coat ~
eb.len eb.wid eb.dep 
&Scoped-define ENABLED-TABLES eb est-qty ef
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-define SECOND-ENABLED-TABLE est-qty
&Scoped-define THIRD-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS RECT-10 
&Scoped-Define DISPLAYED-FIELDS eb.stock-no eb.part-dscr1 eb.part-no ~
eb.style eb.procat est-qty.eqty ef.board ef.brd-dscr ef.cal eb.i-col ~
eb.i-coat eb.len eb.wid eb.dep 
&Scoped-define DISPLAYED-TABLES eb est-qty ef
&Scoped-define FIRST-DISPLAYED-TABLE eb
&Scoped-define SECOND-DISPLAYED-TABLE est-qty
&Scoped-define THIRD-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS style_dscr procat_dscr 

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
DEFINE VARIABLE procat_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144.8 BY 12.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     eb.stock-no AT ROW 1.24 COL 20 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.part-dscr1 AT ROW 1.24 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.part-no AT ROW 2.19 COL 20 COLON-ALIGNED
          LABEL "Customer Part #"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.style AT ROW 4.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     style_dscr AT ROW 4.1 COL 27 COLON-ALIGNED NO-LABEL
     eb.procat AT ROW 5.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     procat_dscr AT ROW 5.05 COL 28 COLON-ALIGNED NO-LABEL
     est-qty.eqty AT ROW 6 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.board AT ROW 6.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.brd-dscr AT ROW 6.95 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.cal AT ROW 7.91 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     eb.i-col AT ROW 8.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 8.86 COL 36 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.len AT ROW 9.81 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.wid AT ROW 10.76 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dep AT ROW 11.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     RECT-10 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.eb,ASI.ef,ASI.est-qty
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
         HEIGHT             = 17.38
         WIDTH              = 144.8.
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

/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN procat_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME F-Main
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
  {src/adm/template/row-list.i "eb"}
  {src/adm/template/row-list.i "ef"}
  {src/adm/template/row-list.i "est-qty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "eb"}
  {src/adm/template/row-find.i "ef"}
  {src/adm/template/row-find.i "est-qty"}

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

    RUN est/BuildDefaultPreps.p(BUFFER est,
                              BUFFER ef,
                              INPUT 1,
                              INPUT 1).
/*   i = 1.                                                                                */
/*                                                                                         */
/*                                                                                         */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:           */
/*       create est-prep.                                                                  */
/*       assign est-prep.e-num  = est.e-num                                                */
/*              est-prep.est-no = est.est-no                                               */
/*              est-prep.line   = i                                                        */
/*              est-prep.s-num  = 1                                                        */
/*              est-prep.b-num  = 1                                                        */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in      */
/*                                else if prep.mat-type eq "b" and  avail ef               */
/*                                then ef.adh-sqin                                         */
/*                         else 1  /* mat-type eq "m" */                                   */
/*             est-prep.code   = prep.code                                                 */
/*             est-prep.dscr   = prep.dscr                                                 */
/*             est-prep.cost   = prep.cost                                                 */
/*             est-prep.ml     = prep.ml                                                   */
/*             est-prep.simon  = prep.simon                                                */
/*             est-prep.mkup   = prep.mkup                                                 */
/*             est-prep.amtz   = prep.amtz                                                 */
/*             est-prep.mat-type = prep.mat-type.                                          */
/*             if lookup(est-prep.mat-type, "p,f") gt 0 then                               */
/*                run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty). */
/*             i = i + 1.                                                                  */
/*   end.                                                                                  */
/*                                                                                         */
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
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "est-qty"}

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

