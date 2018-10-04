&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
def new shared var cocode as cha no-undo.  /* for convquom.p */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartPanel

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rm-rctd
&Scoped-define FIRST-EXTERNAL-TABLE rm-rctd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rm-rctd.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-post 

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
DEFINE BUTTON bt-post 
     LABEL "&Post" 
     SIZE 33 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-post AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.rm-rctd
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
         HEIGHT             = 6.86
         WIDTH              = 66.
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

{Advantzware/WinKit/winkit-panel.i}
{src/adm/method/panel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-post
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-post V-table-Win
ON CHOOSE OF bt-post IN FRAME F-Main /* Post */
DO:
      def var out-hd-lst as cha no-undo.

      message "Are you ready to post" view-as alert-box question 
              button yes-no update ll-ans as log.

      if ll-ans then      run rm-post-receipt no-error.
      if not error-status:error then do:
         run get-link-handle in adm-broker-hdl (This-procedure,"Record-Source", output out-hd-lst).
         run dispatch in widget-handle(out-hd-lst) ("open-query").

      end.

  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
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
  {src/adm/template/row-list.i "rm-rctd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rm-rctd"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix V-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-out-qty as dec no-undo.
  def output parameter op-out-cost as dec no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.

  cocode = rm-rctd.company.

  find item  where item.company eq cocode and item.i-no  eq rm-rctd.i-no
                      use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.    
  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rctd.i-no
                       and po-ordl.job-no = rm-rctd.job-no
                       and po-ordl.job-no2 = (rm-rctd.job-no2)
                       and po-ordl.item-type = yes
                       and po-ordl.s-num = (rm-rctd.s-num)
                           no-lock no-error.

  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no
                         and job.job-no2 eq rm-rctd.job-no2
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no
                                  and job-mat.frm     eq rm-rctd.s-num
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          .
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else item.s-wid.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
  end.

  /* convert qty po-ordl.pr-qty-uom*/
  run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).

  /* convert cost po-ordl.pr-uom*/
  run rm/convcuom.p(po-ordl.cons-uom, rm-rctd.cost-uom,
                               v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
 /*
  message "new qty: " lv-out-qty "PO-uom:" po-ordl.pr-uom skip
          " cost:" lv-out-cost  .
 */  
  assign op-out-qty = lv-out-qty
         op-out-cost = lv-out-cost
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-receipt V-table-Win 
PROCEDURE rm-post-receipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var cocode as cha no-undo.
  def buffer ps-rctd for rm-rctd .

  cocode = rm-rctd.company.

/*for each rm-rctd of rm-rcth :
       /* where rm-rcpt.company   eq rm-rcth.company
          and rm-rcpt.rita-code eq "R"
          and rm-rcpt.job-no    ge v-from-job
          and rm-rcpt.job-no    le v-to-job
        use-index rita no-lock.
        */  
*/      
  find first item where item.company eq rm-rctd.company
                    and item.i-no    eq rm-rctd.i-no
                    no-error.


 /*     run get-matrix (output ld-cvt-qty, output ld-cvt-cost).
 */      
      find first rm-bin where rm-bin.company eq rm-rctd.company
              and rm-bin.loc     eq rm-rctd.loc
              and rm-bin.i-no    eq rm-rctd.i-no
              and rm-bin.loc-bin eq rm-rctd.loc-bin
              and rm-bin.tag     eq rm-rctd.tag
            no-error.
      if not avail rm-bin then do:
          create rm-bin.
          assign
           rm-bin.company = rm-rctd.company
           rm-bin.loc     = rm-rctd.loc
           rm-bin.loc-bin = rm-rctd.loc-bin
           rm-bin.tag     = rm-rctd.tag
           rm-bin.i-no    = rm-rctd.i-no.
        end. /* not avail rm-bin */

        if rm-rctd.rita-code eq "C" then do:        /** Physical Count **/

           assign  rm-bin.qty     =  rm-rctd.qty
                   item.last-count =  rm-rctd.qty
                   item.q-onh     =  rm-rctd.qty
                   item.last-date = rm-rctd.rct-date
                   .
        end. /* C */

/* ==========  not change rita-code's value*/
   find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
   assign ps-rctd.rita-code = "P".  /* posted */
          ps-rctd.post-date = today. 


  /*end.  /* for each rm-rctd */
  */


    /*
    find b-rm-rcth where recid(b-rm-rcth) = recid(rm-rcth).
    assign b-rm-rcth.rita-code = "P" /* posted */
           b-rm-rcth.trans-date = today
           .
   */
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
  {src/adm/template/snd-list.i "rm-rctd"}

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


