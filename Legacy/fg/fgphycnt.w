&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg\fgphycnt.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/hndlset.i}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/VAR.i "new shared" }
def new shared var v-trnum as int.

def TEMP-TABLE w-fg-rcpts NO-UNDO like fg-rcpts.
def TEMP-TABLE w-fg-rdtl  NO-UNDO like fg-rdtl.

{oe/invwork.i new}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-post-date Btn_OK Btn_Cancel tb_gl RECT-18 
&Scoped-Define DISPLAYED-OBJECTS v-post-date lbl_gl tb_gl 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lbl_gl AS CHARACTER FORMAT "X(256)":U INITIAL "Create GL Accounts?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Posting Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 6.67.

DEFINE VARIABLE tb_gl AS LOGICAL INITIAL no 
     LABEL "Create GL Accounts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-post-date AT ROW 6.48 COL 36 COLON-ALIGNED
     Btn_OK AT ROW 14.1 COL 17
     Btn_Cancel AT ROW 14.1 COL 46
     lbl_gl AT ROW 8.38 COL 17 COLON-ALIGNED NO-LABEL
     tb_gl AT ROW 8.38 COL 43
     RECT-18 AT ROW 5.05 COL 4
     "This Procedure Will Post All Finished Goods" VIEW-AS TEXT
          SIZE 50 BY 1.43 AT ROW 1.24 COL 11
          FONT 6
     "Physical Count Transactions." VIEW-AS TEXT
          SIZE 36 BY 1 AT ROW 2.43 COL 21
          FONT 6
     SPACE(23.39) SKIP(13.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Raw Material Post"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lbl_gl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tb_gl:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Raw Material Post */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO: 
  run rm-post-rpt.
  
  message "Are you ready to post to raw materials?" view-as alert-box question
           button yes-no update ll-ans as log.

  if ll-ans then run rm-post.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_gl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_gl Dialog-Frame
ON VALUE-CHANGED OF tb_gl IN FRAME Dialog-Frame /* Create GL Accounts? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date Dialog-Frame
ON LEAVE OF v-post-date IN FRAME Dialog-Frame /* Posting Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   ASSIGN cocode = gcompany
          locode = gloc.
   
   RUN init-values.
   RUN enable_UI.
 

  

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-issue Dialog-Frame 
PROCEDURE auto-issue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER cf-rctd FOR rm-rctd.
   
   find first cf-rctd where cf-rctd.company    eq cocode
                        and cf-rctd.loc        eq locode
                        and cf-rctd.i-no       eq rm-rctd.i-no
                        and cf-rctd.job-no     eq rm-rctd.job-no
                        and cf-rctd.job-no2    eq rm-rctd.job-no2
                        and cf-rctd.po-no      eq rm-rctd.po-no
                        and cf-rctd.rita-code  eq "I"
                        and cf-rctd.rct-date eq rm-rctd.rct-date
                         no-lock no-error.

   IF NOT AVAIL cf-rctd THEN DO:
      CREATE cf-rctd.
      BUFFER-COPY rm-rctd EXCEPT rm-rctd.rita-code TO cf-rctd.
      cf-rctd.rita-code = "I".
     /* ll-post-issue = YES. not post yet */

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-hist-adder Dialog-Frame 
PROCEDURE create-hist-adder :
/*------------------------------------------------------------------------------
  Purpose:  same but rm-rcpth.rita-code    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID.
  DEF BUFFER bf-rctd FOR rm-rctd.
  DEF VAR li-nxt-r-no AS INT NO-UNDO.
  FIND bf-rctd WHERE RECID(bf-rctd) = ip-recid.
  RUN sys/ref/asiseq.p (INPUT bf-rctd.company, 
                        INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.




  CREATE rm-rcpth.
  ASSIGN rm-rcpth.r-no = li-nxt-r-no
         rm-rcpth.trans-date = bf-rctd.rct-date
         rm-rcpth.company = bf-rctd.company
         rm-rcpth.loc = bf-rctd.loc
         rm-rcpth.po-no = bf-rctd.po-no
         rm-rcpth.i-no = bf-rctd.i-no
         rm-rcpth.i-name = bf-rctd.i-name
         rm-rcpth.job-no = bf-rctd.job-no
         rm-rcpth.job-no2 = bf-rctd.job-no2
         rm-rcpth.pur-uom = bf-rctd.pur-uom
         rm-rcpth.rita-code = bf-rctd.rita-code
         rm-rcpth.post-date = TODAY
         .
  CREATE rm-rdtlh.
  ASSIGN rm-rdtlh.r-no = li-nxt-r-no
         rm-rdtlh.company = bf-rctd.company
         rm-rdtlh.loc = bf-rctd.loc
         rm-rdtlh.loc-bin = bf-rctd.loc-bin
         rm-rdtlh.tag = bf-rctd.tag
         rm-rdtlh.job-no = bf-rctd.job-no
         rm-rdtlh.job-no2 = bf-rctd.job-no2
         rm-rdtlh.qty = bf-rctd.qty
         rm-rdtlh.cost = bf-rctd.cost
         rm-rdtlh.loc2 = bf-rctd.loc2
         rm-rdtlh.loc-bin2 = bf-rctd.loc-bin2
         rm-rdtlh.tag2 = bf-rctd.tag2
         rm-rdtlh.s-num = bf-rctd.s-num
         rm-rdtlh.rita-code = bf-rctd.rita-code
         .
         
     rm-rcpth.rita-code = "ADDER".

    /* DELETE bf-rctd. later */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-history Dialog-Frame 
PROCEDURE create-history :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID.
  DEF BUFFER bf-rctd FOR rm-rctd.
  DEF VAR li-nxt-r-no AS INT NO-UNDO.
  FIND bf-rctd WHERE RECID(bf-rctd) = ip-recid.
  RUN sys/ref/asiseq.p (INPUT bf-rctd.company, 
                        INPUT "rm_rcpt_seq", 
                        OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


  CREATE rm-rcpth.
  ASSIGN rm-rcpth.r-no = li-nxt-r-no
         rm-rcpth.trans-date = bf-rctd.rct-date
         rm-rcpth.company = bf-rctd.company
         rm-rcpth.loc = bf-rctd.loc
         rm-rcpth.po-no = bf-rctd.po-no
         rm-rcpth.i-no = bf-rctd.i-no
         rm-rcpth.i-name = bf-rctd.i-name
         rm-rcpth.job-no = bf-rctd.job-no
         rm-rcpth.job-no2 = bf-rctd.job-no2
         rm-rcpth.pur-uom = bf-rctd.pur-uom
         rm-rcpth.rita-code = bf-rctd.rita-code
         rm-rcpth.post-date = TODAY
         .
  CREATE rm-rdtlh.
  ASSIGN rm-rdtlh.r-no = li-nxt-r-no
         rm-rdtlh.company = bf-rctd.company
         rm-rdtlh.loc = bf-rctd.loc
         rm-rdtlh.loc-bin = bf-rctd.loc-bin
         rm-rdtlh.tag = bf-rctd.tag
         rm-rdtlh.job-no = bf-rctd.job-no
         rm-rdtlh.job-no2 = bf-rctd.job-no2
         rm-rdtlh.qty = bf-rctd.qty
         rm-rdtlh.cost = bf-rctd.cost
         rm-rdtlh.loc2 = bf-rctd.loc2
         rm-rdtlh.loc-bin2 = bf-rctd.loc-bin2
         rm-rdtlh.tag2 = bf-rctd.tag2
         rm-rdtlh.s-num = bf-rctd.s-num
         rm-rdtlh.rita-code = bf-rctd.rita-code
         .
         
    /* DELETE bf-rctd. later */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY v-post-date lbl_gl tb_gl 
      WITH FRAME Dialog-Frame.
  ENABLE v-post-date Btn_OK Btn_Cancel tb_gl RECT-18 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix Dialog-Frame 
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
  def var lv-cons-uom like po-ordl.cons-uom no-undo.
     
  cocode = rm-rctd.company.

  find item  where item.company eq cocode and item.i-no  eq rm-rctd.i-no
                      use-index i-no NO-LOCK no-error.
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
  lv-cons-uom = if avail po-ordl then po-ordl.cons-uom else item.cons-uom.
  run rm/convquom.p(rm-rctd.pur-uom,
                    lv-cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).
  
  /* convert cost po-ordl.pr-uom*/
  run rm/convcuom.p(lv-cons-uom, rm-rctd.cost-uom,
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values Dialog-Frame 
PROCEDURE init-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*FIND FIRST rm-rctd WHERE rm-rctd.company = gcompany AND
                            rm-rctd.rita-code = "R" NO-LOCK NO-ERROR.
  IF AVAIL rm-rctd THEN ASSIGN t-receipt = YES.
  FIND FIRST rm-rctd WHERE rm-rctd.company = gcompany AND
                            rm-rctd.rita-code = "I" NO-LOCK NO-ERROR.
  IF AVAIL rm-rctd THEN ASSIGN t-issue = YES.
  FIND FIRST rm-rctd WHERE rm-rctd.company = gcompany AND
                            rm-rctd.rita-code = "T" NO-LOCK NO-ERROR.
  IF AVAIL rm-rctd THEN ASSIGN t-trans = YES.
  FIND FIRST rm-rctd WHERE rm-rctd.company = gcompany AND
                            rm-rctd.rita-code = "A" NO-LOCK NO-ERROR.
  IF AVAIL rm-rctd THEN ASSIGN t-adj = YES.
  FIND FIRST rm-rctd WHERE rm-rctd.company = gcompany AND
                            rm-rctd.rita-code = "C" NO-LOCK NO-ERROR.
  IF AVAIL rm-rctd THEN ASSIGN t-phy = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post Dialog-Frame 
PROCEDURE rm-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first sys-ctrl  where sys-ctrl.company eq gcompany
                         and sys-ctrl.name    eq "AUTOISSU"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign   sys-ctrl.company = gcompany
              sys-ctrl.name    = "AUTOISSU"
              sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  /*v-autoissue = sys-ctrl.log-fld.

  if t-receipt then run rm-post-receipt.
  if t-issue  OR ll-post-issue then do: 
      run rm-post-issue.
      ll-post-issue = NO.
  END.
  if t-trans then run rm-post-trans.
  if t-adj then run rm-post-adj.
  if t-phy then run rm-post-phy.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-adj Dialog-Frame 
PROCEDURE rm-post-adj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-po-no like rm-rctd.po-no no-undo.
  def var x as int no-undo.
  def var cocode as cha no-undo.
  def var i as int no-undo.
  def var v-r-qty like rm-rctd.qty no-undo.
  def var v-i-qty like rm-rctd.qty no-undo.
  def var v-t-qty like rm-rctd.qty no-undo.
  def var v-overrun-qty like rm-rctd.qty no-undo.
  def var v-underrun-qty like rm-rctd.qty no-undo.
  def var v-recid as recid no-undo.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec no-undo.
  def buffer ps-rctd for rm-rctd .
  
  cocode = gcompany.

  /*for each rm-rctd where rm-rctd.company   eq rm-rcth.company
                     and rm-rctd.rita-code eq "A"
                     and rm-rctd.job-no    ge v-from-job
                     and rm-rctd.job-no    le v-to-job
          no-lock:
                
      find first item where item.company eq rm-rctd.company
                        and item.i-no    eq rm-rctd.i-no
                      no-error.

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

        if rm-rctd.rita-code eq "A" then do:        /** Adjustment**/
           if rm-rctd.cost <> 0 then do:
           {rm/rm-post.i "rm-bin" "rm-rctd.qty" "rm-rctd.cost"}
         /*  {rm/rm-post.i "rm-bin" "ld-cvt-qty" "ld-cvt-cost"} */
           end.
           assign  rm-bin.qty     = rm-bin.qty + rm-rctd.qty
                   item.last-cost = if rm-rctd.cost <> 0 then rm-rctd.cost else item.last-cost
                   item.q-onh     = item.q-onh + rm-rctd.qty
                   item.q-avail   = item.q-onh - item.q-comm.
        end. /* A */
        RUN create-history (RECID(rm-rctd) ).
        find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
        assign ps-rctd.rita-code = "P".  /* posted */
               ps-rctd.post-date = today. 

  end.  /* for each rm-rctd */ */
  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-issue Dialog-Frame 
PROCEDURE rm-post-issue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like rm-rctd.qty no-undo.
  def var v-i-qty like rm-rctd.qty no-undo.
  def var v-t-qty like rm-rctd.qty no-undo.
  def var v-overrun-qty like rm-rctd.qty no-undo.
  def var v-underrun-qty like rm-rctd.qty no-undo.
  def var v-recid as recid no-undo.
  def var locode as cha no-undo.
  def var v-bwt like job-mat.basis-w no-undo.
  def var v-len like job-mat.len no-undo.
  def var v-wid like job-mat.wid no-undo.
  def var v-dep like item.s-dep no-undo.
  def var out-qty like rm-rctd.qty no-undo.
  def var out-cost as dec no-undo.
  def buffer b-po-ordl for po-ordl.
  def buffer b-rm-rcth for rm-rcth.  
  def buffer b-rm-h for rm-rcth.
  def buffer b-rm-d for rm-rctd.      
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec no-undo.
  def buffer b-item for item.
  def buffer last-rm-rcth for rm-rcth.  
  def var ll-is-rcth-created as log no-undo.
  def buffer ps-rctd for rm-rctd .
  DEF BUFFER x-rm-rctd FOR rm-rctd.
  

  cocode = gcompany.

  find first sys-ctrl  where sys-ctrl.company eq gcompany
                         and sys-ctrl.name    eq "AUTOISSU"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign   sys-ctrl.company = gcompany
              sys-ctrl.name    = "AUTOISSU"
              sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  /*v-autoissue = sys-ctrl.log-fld.

  for each rm-rctd where rm-rctd.company   eq gcompany
                     and rm-rctd.rita-code = "I" 
                     and rm-rctd.job-no    ge v-from-job
                     and rm-rctd.job-no    le v-to-job
           no-lock.
 
      locode = rm-rctd.loc.           
      v-po-no = trim(rm-rctd.po-no).
      if v-po-no ne "" then 
         do x = 1 to length(v-po-no):
            if substr(v-po-no,x,1) lt "0" or substr(v-po-no,x,1) gt "9" then next .
         end.
    
     
      find first item where item.company eq rm-rctd.company
                        and item.i-no    eq rm-rctd.i-no
           no-error.
      if (item.i-code eq "E" and not avail po-ordl) or
          (item.i-code eq "R" and not v-autoissue)  then next .

      run get-matrix (output ld-cvt-qty, output ld-cvt-cost).
    
      FIND CURRENT ITEM NO-ERROR.
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
      

      find first job  where job.company eq rm-rctd.company
                and job.job-no  eq fill(" ",6 - length(trim(rm-rctd.job-no))) +
                                   trim(rm-rctd.job-no)
                and job.job-no2 eq rm-rctd.job-no2
              no-error.

      if avail job and job.job-no ne "" then do:
             run rm/mkjobmat.p (recid(rm-rctd),rm-rctd.company, output v-recid).
             find job-mat where recid(job-mat) eq v-recid no-error.          
             if not avail job-mat then do:
                message " Job Mat Record not found for "
                      string(job.job-no + "-" + string(job.job-no2,"99") +
                             "  " + rm-rcpt.i-no).
                return error.
             end.

             assign  v-bwt = job-mat.basis-w
                    v-len = job-mat.len
                    v-wid = job-mat.wid
                    v-dep = item.s-dep.

             if v-len eq 0 then v-len = item.s-len.
             if v-wid eq 0 then v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.
             if v-bwt eq 0 then v-bwt = item.basis-w.
             if index("RL",job.stat) ne 0 then job.stat = "W".

             {rm/rmmatact.i}            /* Create Actual Material */

             run rm/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.qty, output out-qty).

             run rm/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.cost, output out-cost).

             assign  mat-act.qty-uom = job-mat.qty-uom
                     mat-act.cost    = if mat-act.cost eq 0 then out-cost else mat-act.cost
                     mat-act.qty     = mat-act.qty     + out-qty
                     job-mat.qty-iss = job-mat.qty-iss + out-qty
                     job-mat.qty-all = job-mat.qty-all - out-qty
                     item.q-comm     = item.q-comm     - out-qty.

             /* Don't relieve more than were allocated */
             if job-mat.qty-all lt 0 then
                run rm/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                     v-bwt, v-len, v-wid, v-dep,
                                     job-mat.qty-all, output out-qty).
             assign  job-mat.qty-all = 0
                     item.q-comm     = item.q-comm - out-qty
                     job-mat.all-flg = (job-mat.qty-all gt 0).
             if item.q-comm lt 0 then item.q-comm = 0.          

             IF ITEM.mat-type = "B" THEN 
                         {rm/rm-addcr.i R rm-rctd x-rm-rctd b-}      
             END.   

       end.
          assign  rm-bin.qty     = rm-bin.qty - out-qty
                  item.q-onh     = item.q-onh - out-qty
                  item.qlast-iss = out-qty
                  item.dlast-iss = rm-rctd.post-date
                  item.q-ytd     = item.q-ytd + out-qty
                  item.q-ptd     = item.q-ptd + out-qty
                  item.u-ptd     = item.u-ptd + (out-cost * out-qty)
                  item.u-ytd     = item.u-ytd + (out-cost * out-qty)
                  item.q-avail   = item.q-onh - item.q-comm.                         

        RUN create-history (RECID(rm-rctd) ).

        find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
        assign  ps-rctd.rita-code = "P"  /* posted */
                ps-rctd.post-date = today.



  end.  /* for each rm-rctd */ */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-phy Dialog-Frame 
PROCEDURE rm-post-phy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-po-no like rm-rctd.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like rm-rctd.qty no-undo.
  def var v-i-qty like rm-rctd.qty no-undo.
  def var v-t-qty like rm-rctd.qty no-undo.
  def var v-overrun-qty like rm-rctd.qty no-undo.
  def var v-underrun-qty like rm-rctd.qty no-undo.
  def var v-recid as recid no-undo.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec no-undo.
  def buffer ps-rctd for rm-rctd .
  
  cocode = gcompany.

  /*for each rm-rctd where rm-rctd.company   eq rm-rcth.company
                     and rm-rctd.rita-code eq "C"
                     and rm-rctd.job-no    ge v-from-job
                     and rm-rctd.job-no    le v-to-job
          no-lock:
                
      find first item where item.company eq rm-rctd.company
                        and item.i-no    eq rm-rctd.i-no
                      no-error.

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

        if rm-rctd.rita-code eq "A" then do:        /** Adjustment**/
           if rm-rctd.cost <> 0 then do:
           {rm/rm-post.i "rm-bin" "rm-rctd.qty" "rm-rctd.cost"}
         /*  {rm/rm-post.i "rm-bin" "ld-cvt-qty" "ld-cvt-cost"} */
           end.
           assign  rm-bin.qty     = rm-bin.qty + rm-rctd.qty
                   item.last-cost = if rm-rctd.cost <> 0 then rm-rctd.cost else item.last-cost
                   item.q-onh     = item.q-onh + rm-rctd.qty
                   item.q-avail   = item.q-onh - item.q-comm.
        end. /* A */
        RUN create-history (RECID(rm-rctd) ).
        find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
        assign ps-rctd.rita-code = "P".  /* posted */
               ps-rctd.post-date = today. 

  end.  /* for each rm-rctd */ */
  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-receipt Dialog-Frame 
PROCEDURE rm-post-receipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like rm-rctd.qty no-undo.
  def var v-i-qty like rm-rctd.qty no-undo.
  def var v-t-qty like rm-rctd.qty no-undo.
  def var v-overrun-qty like rm-rctd.qty no-undo.
  def var v-underrun-qty like rm-rctd.qty no-undo.
  def var v-recid as recid no-undo.
  def buffer b-po-ordl for po-ordl.
  def buffer b-rm-rcth for rm-rcth.  
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec no-undo.
  def buffer ps-rctd for rm-rctd .
  def buffer b-item for item.
  DEF BUFFER x-rm-rctd FOR rm-rctd.

  cocode = gcompany.
  
  /*for each rm-rctd where rm-rctd.company   eq gcompany
                     and rm-rctd.rita-code = "R" 
                     and rm-rctd.job-no    ge v-from-job
                     and rm-rctd.job-no    le v-to-job NO-LOCK.
                         
      v-po-no = trim(rm-rctd.po-no).
      if v-po-no ne "" then 
      do x = 1 to length(v-po-no):
            if substr(v-po-no,x,1) lt "0" or substr(v-po-no,x,1) gt "9" then next .
      end.
      if rm-rctd.job-no ne "" then do:
          find first po-ordl
              where po-ordl.company   eq rm-rctd.company
                and po-ordl.i-no      eq rm-rctd.i-no
                and po-ordl.po-no     eq int(v-po-no)
                and po-ordl.job-no    eq rm-rctd.job-no
                and po-ordl.job-no2   eq rm-rctd.job-no2
                and po-ordl.item-type eq yes
              use-index item-ordno no-lock no-error.
          if not avail po-ordl then next.
      end.

      find first item where item.company eq rm-rctd.company
                        and item.i-no    eq rm-rctd.i-no
                      no-error.
      if not avail item or
        (item.i-code eq "E" and not avail po-ordl) or
        (item.i-code eq "R" and not v-autoissue)  then next .

      RUN auto-issue.
      IF ITEM.mat-type = "B" THEN DO:
                   {rm/rm-addcr.i E rm-rctd x-rm-rctd b-}      
         END.   
         ll-post-issue = YES.
      END.
      run get-matrix (output ld-cvt-qty, output ld-cvt-cost).
      FIND CURRENT ITEM NO-ERROR.
      
      /*======== from rm/rm-post.p   =========*/
      if (item.i-code eq "E" and rm-rctd.rita-code eq "R") then do:
           {rm/rm-poupd.i 1}
           /* deleted rm-rctd, rm-rcpt in Ch_prog */
      end.
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

        if rm-rctd.rita-code eq "R" then do:        /** RECEIPTS **/
          /*{rm/rm-post.i "rm-bin" "rm-rctd.qty" "rm-rctd.cost"}*/
           {rm/rm-post.i "rm-bin" "ld-cvt-qty" "ld-cvt-cost"}
          assign
           rm-bin.qty     = rm-bin.qty + ld-cvt-qty  /*rm-rctd.qty*/
           item.last-cost = ld-cvt-cost              /*rm-rctd.cost*/
           item.q-onh     = item.q-onh + ld-cvt-qty  /*rm-rctd.qty*/
           item.q-avail   = item.q-onh - item.q-comm.

          {rm/rm-poupd.i 2}
        end. /* R */

        RUN create-history (recid(rm-rctd)).       
        find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
        assign ps-rctd.rita-code = "P".  /* posted */
               ps-rctd.post-date = today. 
        


  end.  /* for each rm-rctd */
   
  if v-autoissue then run rm-post-issue. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-rpt Dialog-Frame 
PROCEDURE rm-post-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
      
  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2].
  ELSE
     v-dir = "c:\tmp".

  output to value(v-dir + "\rmpst.txt").
  put "***** Raw Material Posting List ******" skip
  "=========================================================================================================================" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-post-trans Dialog-Frame 
PROCEDURE rm-post-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like rm-rctd.qty no-undo.
  def var v-i-qty like rm-rctd.qty no-undo.
  def var v-t-qty like rm-rctd.qty no-undo.
  def var v-overrun-qty like rm-rctd.qty no-undo.
  def var v-underrun-qty like rm-rctd.qty no-undo.
  def var v-recid as recid no-undo.
  def buffer xrm-bin for rm-bin.
  def buffer b-rm-rcth for rm-rcth.  
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec no-undo.
  def buffer ps-rctd for rm-rctd .
  
  cocode = gcompany.
  /*for each rm-rctd where rm-rctd.company   eq gcompany
                     and rm-rctd.rita-code = "I" 
                     and rm-rctd.job-no    ge v-from-job
                     and rm-rctd.job-no    le v-to-job
           no-lock.

      find first item where item.company eq rm-rctd.company
                        and item.i-no    eq rm-rctd.i-no
                      no-error.
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

      if rm-rctd.rita-code eq "T" then do:        /** Transfer **/
         assign  rm-bin.qty  = rm-bin.qty - rm-rctd.qty
               /*  rm-rctd.cost = rm-bin.cost in last line */
                 .

         /* This code is to handel the Transfer to quantity to increase the BIN
            using a buffer record so current rm-bin record is not updated. */

          find first xrm-bin  where xrm-bin.company eq rm-rctd.company
                                and xrm-bin.loc     eq rm-rctd.loc2
                                and xrm-bin.i-no    eq rm-rctd.i-no
                                and xrm-bin.loc-bin eq rm-rctd.loc-bin2
                                and xrm-bin.tag     eq rm-rctd.tag2
               no-error.
          if not avail xrm-bin then do:
            create xrm-bin.
            assign
             xrm-bin.company = rm-rctd.company
             xrm-bin.loc     = rm-rctd.loc2
             xrm-bin.loc-bin = rm-rctd.loc-bin2
             xrm-bin.tag     = rm-rctd.tag2
             xrm-bin.i-no    = rm-rctd.i-no.
          end.

          {rm/rm-post.i "rm-bin" "rm-rctd.qty" "rm-rctd.cost"}
          xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      end. /* T */

   

   find ps-rctd where recid(ps-rctd) = recid(rm-rctd).
   RUN create-history (RECID(rm-rctd) ).
   assign ps-rctd.rita-code = "P"  /* posted */
          ps-rctd.post-date = today
          ps-rctd.cost = rm-bin.cost. 

   /** Delete Bins With Zero Quantities. **/
   if rm-bin.qty = 0 then delete rm-bin.

  end.  /* for each rm-rctd */ */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

