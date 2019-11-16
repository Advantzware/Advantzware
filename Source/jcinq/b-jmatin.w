&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

&SCOPED-DEFINE yellowColumnsName b-jmatin
&SCOPED-DEFINE noSortByField
&SCOPED-DEFINE SORTBY-PHRASE BY tt-mat.form-no BY tt-mat.blank-no
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def new shared var v-avgcost as logical.

def TEMP-TABLE tt-mat
   FIELD mat-seq AS INT
   field form-no like job-mat.frm
   field blank-no like job-mat.blank-no
   field rm-i-no like job-mat.rm-i-no
   field updatable as log init no
   field qty-std as decimal format '>>>>>9.9'
   field qty-act as decimal format '>>>>>9.9'
   field qty-var as decimal format '->>>>>9.9'
   field cst-std as decimal format '>>>>>>9.99'
   field cst-act as decimal format '>>>>>>9.99'
   field cst-var as decimal format '->>>>>>9.99'
   field basis-w as dec
   field len as dec
   field wid as dec
   field cst-uom like job-mat.sc-uom
   INDEX tt-mat form-no blank-no rm-i-no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mat

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-mat.form-no tt-mat.blank-no tt-mat.rm-i-no tt-mat.qty-std tt-mat.qty-act tt-mat.qty-var tt-mat.cst-std tt-mat.cst-act tt-mat.cst-var   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-mat ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-mat ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-mat
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-mat


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-mat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-mat.form-no  COLUMN-LABEL "Sheet" FORMAT ">>>":U LABEL-BGCOLOR 14
      tt-mat.blank-no COLUMN-LABEL "Blank" FORMAT ">>>":U LABEL-BGCOLOR 14
      tt-mat.rm-i-no  COLUMN-LABEL "RM Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      tt-mat.qty-std  COLUMN-LABEL "Standard Qty" FORMAT "->>>,>>>.<<<<":U LABEL-BGCOLOR 14
      tt-mat.qty-act  COLUMN-LABEL "Actual Qty" FORMAT "->>>,>>>.<<<<":U LABEL-BGCOLOR 14
      tt-mat.qty-var  COLUMN-LABEL "Qty Variance" FORMAT "->>>,>>>,>>>.<<<<":U LABEL-BGCOLOR 14
      tt-mat.cst-std  COLUMN-LABEL "Standard Cost" FORMAT "->,>>>,>>9.99":U LABEL-BGCOLOR 14
      tt-mat.cst-act  COLUMN-LABEL "Actual Cost" FORMAT "->,>>>,>>9.99":U LABEL-BGCOLOR 14
      tt-mat.cst-var  COLUMN-LABEL "Cost Variance" FORMAT "->,>>>,>>9.99":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 13.57
         BGCOLOR 8 FONT 2
         TITLE BGCOLOR 8 "Material Variance".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.job
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 14.43
         WIDTH              = 129.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mat ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main /* Material Variance */
DO:
  DEF VAR li-seq LIKE tt-mat.mat-seq NO-UNDO.

  DEF BUFFER b-mat FOR tt-mat.


  IF AVAIL tt-mat AND
     NOT(tt-mat.rm-i-no:SCREEN-VALUE in BROWSE {&browse-name} EQ "ALL" AND
         INT(tt-mat.form-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
         INT(tt-mat.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0) THEN DO:

    li-seq = tt-mat.mat-seq.

    RUN jcinq\b-updmat.w(INPUT job.job-no,
                         INPUT job.job-no2,
                         INPUT tt-mat.form-no:SCREEN-VALUE in BROWSE {&browse-name},
                         INPUT tt-mat.blank-no:SCREEN-VALUE in BROWSE {&browse-name},
                         INPUT tt-mat.rm-i-no:SCREEN-VALUE in BROWSE {&browse-name}).

    RUN local-open-query.

    FIND FIRST b-mat WHERE b-mat.mat-seq EQ li-seq NO-ERROR.

    IF AVAIL b-mat THEN DO WITH FRAME {&FRAME-NAME}:
      REPOSITION {&browse-name} TO ROWID ROWID(b-mat) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN APPLY "row-changed" TO {&browse-name}.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Material Variance */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Material Variance */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Material Variance */
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Material Variance */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var v-pct as dec init 1.00 no-undo.
  def var v-cost as dec no-undo.
  def var v-qty as dec no-undo.
  def var v-std-tot like tt-mat.cst-std no-undo.
  def var v-act-tot like tt-mat.cst-act no-undo.
  def var v-var-tot like tt-mat.cst-var no-undo.
  DEF VAR li-seq LIKE tt-mat.mat-seq NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  find first rm-ctrl where rm-ctrl.company = cocode no-lock no-error.
  if rm-ctrl.avg-lst-cst = true then v-avgcost = true.
  else assign v-avgcost = false.
  release rm-ctrl.

  EMPTY TEMP-TABLE tt-mat.

  FOR EACH job-mat NO-LOCK
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2:
    find first item
        where item.company eq job-mat.company
          and item.i-no    eq job-mat.rm-i-no
        use-index i-no no-lock.
      
    v-pct = 1.
    
    create tt-mat.
    assign
     tt-mat.form-no  = job-mat.frm
     tt-mat.blank-no = job-mat.blank-no
     tt-mat.rm-i-no  = job-mat.rm-i-no
     tt-mat.basis-w  = job-mat.basis-w
     tt-mat.len      = job-mat.len
     tt-mat.wid      = job-mat.wid
     tt-mat.cst-uom  = job-mat.sc-uom.

    IF TRUE /* job-mat.j-no EQ 0  */ THEN DO:
      assign
       tt-mat.qty-std = job-mat.qty * v-pct
       v-cost      = job-mat.std-cost.

      IF job-mat.sc-uom NE job-mat.qty-uom THEN
      if item.r-wid eq 0 then do:
        run sys/ref/convcuom.p(job-mat.sc-uom,
                               job-mat.qty-uom,
                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                else item.basis-w),
                               (if job-mat.len      ne 0 then job-mat.len
                                else item.s-len),
                               (if job-mat.wid      ne 0 then job-mat.wid
                                else item.s-wid),
                               item.s-dep, 
                               job-mat.std-cost,
                               output v-cost).
      end.

      else do:
        run sys/ref/convcuom.p(job-mat.sc-uom,
                               job-mat.qty-uom,
                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                else item.basis-w),
                               job-mat.len,
                               (if job-mat.wid      ne 0 then job-mat.wid
                                else item.r-wid),
                               item.s-dep, 
                               job-mat.std-cost,
                               output v-cost).
      end.
      tt-mat.cst-std = job-mat.qty * v-cost * v-pct.
    END.
  end.

  FOR EACH mat-act NO-LOCK
      WHERE mat-act.company EQ job.company
        AND mat-act.job     EQ job.job
        AND mat-act.job-no  EQ job.job-no
        AND mat-act.job-no2 EQ job.job-no2:
    find first item
        where item.company eq mat-act.company
          and item.i-no    eq mat-act.rm-i-no
        use-index i-no no-lock.
      
    v-pct = 1.
    
    find first tt-mat
        where tt-mat.form-no  eq mat-act.s-num
          and tt-mat.blank-no eq mat-act.b-num
          and tt-mat.rm-i-no  eq mat-act.rm-i-no
        no-error.
    if not avail tt-mat then do:
      create tt-mat.
      assign
       tt-mat.form-no  = mat-act.s-num
       tt-mat.blank-no = mat-act.b-num
       tt-mat.rm-i-no  = mat-act.rm-i-no
       tt-mat.qty-std  = 0
       tt-mat.cst-std  = 0
       v-qty        = 0
       v-cost       = 0.
       
      for each rm-rcpth
          where rm-rcpth.company   eq cocode
            and rm-rcpth.job-no    eq mat-act.job-no
            and rm-rcpth.job-no2   eq mat-act.job-no2
            and rm-rcpth.i-no      eq mat-act.i-no
            and rm-rcpth.rita-code eq "I"
          no-lock,
          
          each rm-rdtlh
          where rm-rdtlh.r-no eq rm-rcpth.r-no
          no-lock:
        
        assign
         v-qty  = v-qty  + rm-rdtlh.qty
         v-cost = v-cost + (rm-rdtlh.qty * rm-rdtlh.cost).
      end.
      
      v-cost = v-cost / v-qty.
    end.
    
    ELSE DO:
     v-cost = mat-act.cost.

     IF tt-mat.cst-uom NE mat-act.qty-uom THEN
      if item.r-wid eq 0 then do:
        run sys/ref/convcuom.p(tt-mat.cst-uom,
                               mat-act.qty-uom,
                               (if tt-mat.basis-w   ne 0 then tt-mat.basis-w
                                else item.basis-w),
                               (if tt-mat.len       ne 0 then tt-mat.len
                                else item.s-len),
                               (if tt-mat.wid       ne 0 then tt-mat.wid
                                else item.s-wid),
                               item.s-dep, 
                               mat-act.cost,
                               output v-cost).
      end.

      else do:
        run sys/ref/convcuom.p(tt-mat.cst-uom,
                               mat-act.qty-uom,
                               (if tt-mat.basis-w   ne 0 then tt-mat.basis-w
                                else item.basis-w),
                               tt-mat.len,
                               (if tt-mat.wid       ne 0 then tt-mat.wid
                                else item.r-wid),
                               item.s-dep, 
                               mat-act.cost,
                               output v-cost).
      end.
    END.

    if v-cost eq ? then tt-mat.updatable = yes.

    tt-mat.qty-act = tt-mat.qty-act + (mat-act.qty * v-pct).

    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
      tt-mat.cst-act = tt-mat.cst-act + (mat-act.qty * v-cost * v-pct).
    ELSE
      tt-mat.cst-act = tt-mat.cst-act + (mat-act.ext-cost * v-pct).
  end.

  for each tt-mat:
     assign
      li-seq      = li-seq + 1
      tt-mat.mat-seq = li-seq
      tt-mat.qty-var = tt-mat.qty-std - tt-mat.qty-act
      tt-mat.cst-var = tt-mat.cst-std - tt-mat.cst-act
      v-std-tot   = v-std-tot + tt-mat.cst-std
      v-act-tot   = v-act-tot + tt-mat.cst-act
      v-var-tot   = v-var-tot + tt-mat.cst-var.
  end.

  FIND FIRST tt-mat
      WHERE tt-mat.form-no  EQ 0
        AND tt-mat.blank-no EQ 0
        AND tt-mat.rm-i-no  EQ "ALL"
      NO-ERROR.
  IF AVAIL tt-mat THEN DELETE tt-mat.

  CREATE tt-mat.
  ASSIGN
   li-seq       = li-seq + 1
   tt-mat.mat-seq  = li-seq
   tt-mat.form-no  = 0
   tt-mat.blank-no = 0
   tt-mat.rm-i-no  = "ALL"
   tt-mat.cst-std  = v-std-tot
   tt-mat.cst-act  = v-act-tot
   tt-mat.cst-var  = v-var-tot.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "tt-mat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

