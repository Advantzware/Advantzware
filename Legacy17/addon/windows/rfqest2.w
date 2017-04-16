&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: addon\windows\rfqest2.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def input parameter ip-rfq-recid as recid no-undo.
def input parameter ip-rfqitem-recid as recid no-undo.
def var lv-seq-max as int no-undo.
def var ll-transfer as log no-undo.
def var li-est-type as int no-undo.
def var ls-seq-list as cha no-undo.
def var ll-selected as log no-undo.
def var li-current-row as int no-undo.
def var ls-find-list as cha no-undo.
def var ls-crt-list as cha no-undo.
def var ls-dest-code as cha no-undo.
DEF VAR viCount AS INT NO-UNDO.
DEF VAR v-side-count AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfqitem

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 rfqitem.rfq-no rfqitem.form-no rfqitem.blank-no rfqitem.seq rfqitem.stock-no rfqitem.i-name rfqitem.part-no rfqitem.style rfqitem.procat rfqitem.est-no /*rfqitem.len rfqitem.wid rfqitem.dep */ /*   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 rfqitem.form-no rfqitem.blank-no */   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 rfqitem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 rfqitem
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 if rd-trx-type = 1 then OPEN QUERY {&SELF-NAME} FOR EACH rfqitem of rfq NO-LOCK /*where rfqitem.est-no = ""*/                                                WHERE rfqitem.seq < 999. else OPEN QUERY {&SELF-NAME} FOR EACH rfqitem of rfq NO-LOCK where rfqitem.est-no <> ""     and rfqitem.seq < 999.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 rfqitem


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 cb-est-type rd-trx-type BROWSE-1 ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cb-est-type rd-trx-type 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 22 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "&Transfer To Estimate" 
     SIZE 24 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cb-est-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estimate Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Folding Single Item","Folding Two Piece Box","Folding Tandem Runs","Folding Combination","Corrugated Single Item","Corrugated Set" 
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE li-num-of-blank AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Blank" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE li-num-of-form AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Form" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rd-trx-type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "New Estimate", 1,
"Update", 2
     SIZE 34 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      rfqitem.rfq-no
      rfqitem.form-no
      rfqitem.blank-no
      rfqitem.seq label "#" form ">>9"
      rfqitem.stock-no label "FG Item#"
      rfqitem.i-name label "Item Name" form "x(30)"
      rfqitem.part-no
      rfqitem.style
      rfqitem.procat label "Category"
      rfqitem.est-no label "Est#" FORM "x(8)"
      /*rfqitem.len label "L"
      rfqitem.wid label "W"
      rfqitem.dep label "D"
      */
     /* enable rfqitem.form-no rfqitem.blank-no */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 129 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cb-est-type AT ROW 1.48 COL 20 COLON-ALIGNED
     rd-trx-type AT ROW 1.48 COL 68 NO-LABEL
     li-num-of-form AT ROW 1.48 COL 112 COLON-ALIGNED
     li-num-of-blank AT ROW 1.48 COL 113 COLON-ALIGNED
     BROWSE-1 AT ROW 3.62 COL 2
     Btn_OK AT ROW 15.29 COL 25
     Btn_Cancel AT ROW 15.29 COL 73
     RECT-1 AT ROW 1.24 COL 2
     SPACE(0.19) SKIP(14.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Transferring RFQ to Estimate"
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 li-num-of-blank Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN li-num-of-blank IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       li-num-of-blank:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN li-num-of-form IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       li-num-of-form:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
if rd-trx-type = 1 then
OPEN QUERY {&SELF-NAME} FOR EACH rfqitem of rfq NO-LOCK /*where rfqitem.est-no = ""*/
                                               WHERE rfqitem.seq < 999.
else
OPEN QUERY {&SELF-NAME} FOR EACH rfqitem of rfq NO-LOCK where rfqitem.est-no <> ""
    and rfqitem.seq < 999.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Transferring RFQ to Estimate */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON VALUE-CHANGED OF BROWSE-1 IN FRAME Dialog-Frame
DO:
 
     li-current-row = {&browse-name}:focused-row.   
     ll-selected = {&browse-name}:is-row-selected(li-current-row).
     
/* ========== not any more     
     if ll-selected then do :
 /*       if integer(rfqitem.form-no:screen-value in browse {&browse-name}) <> 1 then 
           rfqitem.form-no:screen-value = "1".
        if integer(rfqitem.blank-no:screen-value) <> 1 then 
           rfqitem.blank-no:screen-value = "1".   
           */
        case li-est-type :
             when 1 then assign rfqitem.form-no:screen-value in browse {&browse-name} = "1"
                                rfqitem.blank-no:screen-value in browse {&browse-name} = "1"
                                .  
             when 2 then do: /* two piece box MF-SB */
             
             end.                   
        end.       
     end.
     else do:  /* de-list */
/*        if integer(rfqitem.form-no:screen-value) = 1 then 
           rfqitem.form-no:screen-value = "0".
        if integer(rfqitem.blank-no:screen-value) = 1 then 
           rfqitem.blank-no:screen-value = "0".   
*/
        case li-est-type: 
             when 1 then  assign rfqitem.form-no:screen-value in browse {&browse-name} = "0"
                                 rfqitem.blank-no:screen-value in browse {&browse-name} = "0"
                                 .  
        end case.   
     end.
======================== */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Transfer To Estimate */
DO :
   def var li-cnt as int no-undo.
   def var ll-return as log no-undo.
   def var ll-is-transfered as log no-undo.
   def var ls-est-list as cha no-undo.
   DEF BUFFER bf-item FOR rfqitem.

   if li-est-type = 0 then do:
      message "Estimate type can't be blank. Please estimate type first!"
               view-as alert-box error.
      apply "entry" to cb-est-type.
      return no-apply.
   end.   
   IF li-est-type = 6 THEN DO:
      FIND FIRST rfqitem OF rfq where rfqitem.seq = 999 NO-LOCK NO-ERROR.
      IF NOT AVAIL rfqitem THEN DO: 
         FIND FIRST bf-item OF rfq NO-LOCK NO-ERROR.
         CREATE rfqitem.
         BUFFER-COPY bf-item EXCEPT bf-item.seq TO rfqitem.
         rfqitem.seq = 999.
         RUN rfq/d-rfqset.w  (RECID(rfqitem),6).
      END.
      ELSE RUN rfq/d-rfqset.w  (recid(rfqitem),6).
   END.
   assign li-num-of-form 
          li-num-of-blank
          ls-seq-list = ""
          ls-find-list = ""
          ls-crt-list = "" 
          ll-is-transfered = no.
   
   if {&browse-name}:num-selected-rows in frame {&frame-name} > 0 then
   do li-cnt = 1 to {&browse-name}:num-selected-rows in frame {&frame-name}:

      ASSIGN
        ll-return = {&browse-name}:fetch-selected-row(li-cnt)
        ls-seq-list = ls-seq-list + string(rfqitem.seq,">>9") + ",".  /* all list */
      /*ls-est-list = ls-est-list + string(rfqitem.est-no) + ",".*/
      
      /*   if rfqitem.est-no <> "" then 
         assign ls-find-list = ls-find-list + string(rfqitem.seq) + ","
                ll-is-transfered = yes.
         else ls-crt-list = ls-crt-list + string(rfqitem.seq) + ",".
      */ 
   end.
 
   if rd-trx-type = 1 then do: /* new */ 
      case li-est-type :
           when 1 or when 5 then run trx-rfq-to-est.
           when 2 or when 3 or when 4 then run trx-rfq-to-est-4.
           WHEN 6 THEN RUN trx-rfq-to-est-6.
           /*when 4 then run trx-rfq-to-est-4.
           when 5 then run trx-rfq-to-est-5.
           when 6 then run trx-rfq-to-est-6. */
      end case.
      
   end. 
   else do: /* update */
      
      ls-find-list = ls-seq-list.
      case li-est-type :
           when 1 or when 5 then run find-est-update.
           when 2 or when 3 or when 4 or when 7 or when 8 then run find-est-update4.
           WHEN 6 THEN RUN find-est-update6.
      end case.   
   end.
   

   apply "go" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-est-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-est-type Dialog-Frame
ON VALUE-CHANGED OF cb-est-type IN FRAME Dialog-Frame /* Estimate Type */
DO:
   assign cb-est-type.
   case cb-est-type:
        when "Folding Single Item" then li-est-type = 1.
        when "Folding Two Piece Box" then li-est-type = 2.
        when "Folding Tandem Runs" then li-est-type = 3.
        when "Folding Combination" then li-est-type = 4.
        when "Corrugated Single Item" then li-est-type = 5.
        when "Corrugated Set" then li-est-type = 6.        
   end.
/* form-no and blank-no in browser  
   if (li-est-type > 1 and li-est-type < 5) or li-est-type > 5 then do with frame {&frame-name}:
      assign /*li-num-of-form:visible = true
             li-num-of-blank:visible = true  */
             li-num-of-form:sensitive = if li-est-type = 3 then false else true
             li-num-of-blank:sensitive = if li-est-type = 6 then false else true  
             .
      if li-est-type = 3 then li-num-of-form:screen-value = "1".
      
      apply "entry" to li-num-of-form.
             
   end.
   else assign li-num-of-form:sensitive = false
               li-num-of-blank:sensitive = false  
               li-num-of-form:screen-value = "1"
               li-num-of-blank:screen-value = "1"
               .
*/

   run select-all. 
   def buffer bf-rfqitem for rfqitem.
   
   case li-est-type:
        when 1 then do:
             for each bf-rfqitem of rfq :
                 assign bf-rfqitem.form-no = 1
                        bf-rfqitem.blank-no = 1. 
             end.
        end.
   end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-trx-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-trx-type Dialog-Frame
ON VALUE-CHANGED OF rd-trx-type IN FRAME Dialog-Frame
DO:
    assign rd-trx-type.
    {&open-query-{&browse-name}}    
    run select-all.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  find rfq where recid(rfq) = ip-rfq-recid no-lock.
  find sys-ctrl where sys-ctrl.company = rfq.company and
                      sys-ctrl.name = "cemenu"
                      no-lock no-error.
  if avail sys-ctrl then do:
     case sys-ctrl.char-fld:
          when "Corrware" then do:
                cb-est-type:list-items = "Corrugated Single Item,Corrugated Set".
                cb-est-type:screen-value = entry(1,cb-est-type:list-items).   
                li-est-type = 5.
          end.
          when "foldware" then do:
                cb-est-type:list-items = "Folding Single Item,Folding Two Piece Box,Folding Tandem Runs,Folding Combination".
                cb-est-type:screen-value = entry(1,cb-est-type:list-items).   
                li-est-type = 1.
          end.
          when "both" then do:
                 cb-est-type:list-items = "Folding Single Item,Folding Two Piece Box,Folding Tandem Runs,Folding Combination," +
                                          "Corrugated Single Item,Corrugated Set".                 
                cb-est-type:screen-value = entry(1,cb-est-type:list-items).   
                li-est-type = 1.
          end.
     end.  
  end.
  rd-trx-type = 1.
  RUN enable_UI.
  run select-all.  /* don't select not but select when type changed*/
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-est-prep Dialog-Frame 
PROCEDURE create-est-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def input parameter ip-enum like est.e-num no-undo.
   def input parameter ip-est-no as cha no-undo.
   def var i as int no-undo.

   DISABLE TRIGGERS FOR LOAD OF est-prep.

   i = 1.
   for each prep where prep.company = rfqitem.company and prep.dfault NO-LOCK:
       create est-prep.
       assign   est-prep.e-num  = ip-enum
                est-prep.est-no = ip-est-no
                est-prep.company = rfqitem.company
                est-prep.eqty = rfqitem.qty[1]
                est-prep.line   = i
                est-prep.s-num  = 1
                est-prep.b-num  = 1
                est-prep.qty    = 1           
                est-prep.code   = prep.code
                est-prep.dscr   = prep.dscr
                est-prep.cost   = prep.cost
                est-prep.ml     = prep.ml
                est-prep.simon  = prep.simon
                est-prep.mkup   = prep.mkup
                est-prep.amtz   = prep.amtz
                est-prep.mat-type = prep.mat-type
                .
                i = i + 1.
                
   end.

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
  DISPLAY cb-est-type rd-trx-type 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 cb-est-type rd-trx-type BROWSE-1 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-est-update Dialog-Frame 
PROCEDURE find-est-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF est.

session:set-wait-state("COMPILER").


for each rfqitem of rfq where index(ls-find-list,string(rfqitem.seq,">>9")) > 0 :
    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no
                   .
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
    FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.
    IF AVAIL est-qty THEN do:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 98:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
    END.
    find first ef where ef.company = est.company
                    AND ef.est-no = est.est-no
                    AND ef.form-no = rfqitem.form-no.
    assign li-form-no = 1
           li-num-of-blank = 1 
           li-blank-no = 1.
    
    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.
           
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/upd-ef.i}

    RELEASE ITEM.

    find first eb where eb.company = rfqitem.company
                    AND eb.est-no = rfqitem.est-no
                    AND eb.form-no = rfqitem.form-no
                    AND eb.blank-no = rfqitem.blank-no NO-ERROR.
  

    IF AVAIL eb THEN DO:     
        {rfq/upd-eb.i}
    END.
    {rfq/upd-note.i}

end.  /* each rfqitem */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-est-update4 Dialog-Frame 
PROCEDURE find-est-update4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF est.
session:set-wait-state("COMPILER").


for each rfqitem of rfq where index(ls-find-list,string(rfqitem.seq,">>9")) > 0 :
    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no
                   .
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.

     IF AVAIL est-qty THEN do:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     find first ef where ef.company = est.company AND 
                         ef.est-no = est.est-no.
     /*assign li-form-no = 1
           li-num-of-blank = 1 
           li-blank-no = 1.
    */
           
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/upd-ef.i}

    RELEASE ITEM.
    
/*    do while true:
       li-cnt = li-cnt + 1.
*/      

    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.

       find first eb of ef where eb.blank-no = rfqitem.blank-no  /*li-cnt*/  no-error.
       if avail eb then do: 
          {rfq/upd-eb.i} 
       end.
       else do:
         leave.
       end.  
  /*  end.*/
    {rfq/upd-note.i}
end.  /* each rfqitem */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-est-update6 Dialog-Frame 
PROCEDURE find-est-update6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
DEF BUFFER bf-ritem FOR rfqitem.

DISABLE TRIGGERS FOR LOAD OF est.
session:set-wait-state("COMPILER").


for each rfqitem of rfq where index(ls-find-list,string(rfqitem.seq,">>9")) > 0 :
    find est where est.company = rfqitem.company and
                   est.est-no = rfqitem.est-no
                   .
    assign est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
     {rfq/upd-note.i}
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.

     IF AVAIL est-qty THEN do:
        /*est-qty.eqty = rfqitem.qty[1].*/
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     find first ef where ef.company = est.company AND 
                         ef.est-no = est.est-no AND
                         ef.form-no = rfqitem.form-no.
     /*assign li-form-no = 1
           li-num-of-blank = 1 
           li-blank-no = 1.
    */
           
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/upd-ef.i}

    RELEASE ITEM.
    
/*    do while true:
       li-cnt = li-cnt + 1.
*/      

    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.

    find first eb of ef where eb.blank-no = rfqitem.blank-no  /*li-cnt*/  no-error.
    if avail eb then do: 
          {rfq/upd-eb.i} 
    end.
    else do:
         leave.
    end.  
end.  /* each rfqitem */
/* update set header */
  FIND FIRST bf-ritem of rfq where bf-ritem.seq = 999 NO-LOCK NO-ERROR.
  IF AVAIL bf-ritem THEN DO:
    FIND FIRST est WHERE est.company = bf-ritem.company
                     AND est.est-no = bf-ritem.est-no NO-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.company = est.company
                   AND ef.est-no = bf-ritem.est-no NO-LOCK NO-ERROR.
    
    FIND FIRST eb WHERE eb.company = bf-ritem.company
                    AND eb.est-no = bf-ritem.est-no 
                    AND eb.form-no = 0
                    AND eb.blank-no = 0 NO-ERROR.
    IF NOT AVAIL eb THEN CREATE eb.
    {rfq/asn-ebs.i}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-all Dialog-Frame 
PROCEDURE select-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var li-count as int no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-seq AS CHAR NO-UNDO.
  
  ls-seq-list = "".
  /*do li-count = 1 to {&browse-name}:num-iterations in frame {&frame-name}:
     ll-selected = {&browse-name}:select-row(li-count).    
/* =========== not any more : assigned when transtered
     assign rfqitem.form-no:screen-value in browse {&browse-name} = "1"
            rfqitem.blank-no:screen-value in browse {&browse-name} = "1"
                 .
*/  
     ls-seq-list = ls-seq-list + rfqitem.seq:screen-value in browse {&browse-name}
                   + ",".
     if rfqitem.est-no = "" then
        ls-crt-list = ls-crt-list + rfqitem.seq:screen-value in browse {&browse-name}
                      + ",".
     else ls-find-list = ls-find-list + rfqitem.seq:screen-value in browse {&browse-name}
                   + ",".

 
  end.*/

  DO WITH FRAME {&FRAME-NAME}:
    {&browse-name}:SELECT-ALL ().
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

      ASSIGN
       lv-seq      = TRIM(STRING(rfqitem.seq,">>9"))
       ls-seq-list = ls-seq-list + lv-seq + ",".

      IF rfqitem.est-no EQ "" THEN
        ls-crt-list = ls-crt-list + lv-seq + ",".
      ELSE
        ls-find-list = ls-find-list + lv-seq + ",".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-est Dialog-Frame 
PROCEDURE trx-rfq-to-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.
DEF VAR i AS INT NO-UNDO.
DEF VAR ls-key AS cha NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF est-qty.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF reftable.

REPEAT:

find first ce-ctrl where
     ce-ctrl.company = rfqitem.company and
     ce-ctrl.loc = rfqitem.loc
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
find last est /*use-index e-num no in V9 */ no-lock no-error.
li-next-enum = if avail est then est.e-num else 0.

session:set-wait-state("COMPILER").

for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq,">>9")) > 0 :

    create est.
    assign
       li-next-est = li-next-est + 1
       li-next-enum = li-next-enum + 1
       li-form-no = 1
       li-blank-no = 1
       est.company  = rfqitem.company
       est.loc      = rfqitem.loc
       est.est-qty[1] = rfqitem.qty[1]
       est.est-qty[2] = rfqitem.qty[2]
       est.est-qty[3] = rfqitem.qty[3]
       est.est-qty[4] = rfqitem.qty[4]                                             
       est.est-type = li-est-type
       est.e-num    = li-next-enum
       est.est-no   = string(li-next-est,">>>>>>>>")                          
       est.form-qty = 1               
       est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
       est.est-date = TODAY
       ls-key = string(today,"99999999") +
                string(next-value(rec_key_seq,nosweat),"99999999")
       est.rec_key = ls-key.

    CREATE est-qty.
    ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1].

    DO i = 1 TO 99:
       est-qty.qty[i] = rfqitem.qty[i].
    END.

    run create-est-prep (li-next-enum,string(li-next-est,">>>>>>>>")).

    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.
    
    create ef.

    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/asn-ef.i}

    RELEASE ITEM.

    create eb.
    {rfq/asn-eb.i}

    {ce/updunit#.i eb 0}
    {ce/updunit#.i eb 1}

   assign rfqitem.est-no = est.est-no
          rfqitem.form-no = li-form-no
          rfqitem.blank-no = li-blank-no.
    {rfq/upd-note.i}

 end.  /* each rfqitem */

 ce-ctrl.e-num = li-next-est.
 FIND CURRENT ce-ctrl NO-LOCK.
 LEAVE.
END.
END. /*REPEAT*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-est-2 Dialog-Frame 
PROCEDURE trx-rfq-to-est-2 :
/*------------------------------------------------------------------------------
  Purpose:     est-type 2 : Folding Two Piece Box 1-est Multi-ef 1-eb per ef
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.
def var li-cnt as int no-undo.

session:set-wait-state("COMPILER").

REPEAT:

find first ce-ctrl where
     ce-ctrl.company = rfqitem.company and
     ce-ctrl.loc = rfqitem.loc
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0.
find last est /* use-index e-num */ no-lock no-error.
li-next-enum = if avail est then est.e-num else 0.

for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq,">>9")) > 0  
                        break by rfqitem.part-no:

/*    if first-of(rfqitem.part-no) then do: */
       create est.
       assign 
       li-next-est = li-next-est + 1
       li-next-enum = li-next-enum + 1
       est.company  = rfqitem.company                                     
       est.loc        = rfqitem.loc
       est.est-qty[1] = rfqitem.qty[1]
       est.est-qty[2] = rfqitem.qty[2]
       est.est-qty[3] = rfqitem.qty[3]
       est.est-qty[4] = rfqitem.qty[4]                                             
       est.est-type = li-est-type
       est.e-num    = li-next-enum
       est.est-no   = string(li-next-est,">>>>>>>>").
        FIND FIRST ITEM WHERE
             ITEM.company EQ rfqitem.company AND
             ITEM.i-no EQ rfqitem.board
             NO-LOCK NO-ERROR.

        do li-cnt = 1 to li-num-of-form:
           create ef.
           assign li-form-no = li-cnt
                  li-blank-no = 1.
           {rfq/asn-ef.i}

           create eb.
           {rfq/asn-eb.i}
           {ce/updunit#.i eb 0}
           {ce/updunit#.i eb 1}
        end.

        RELEASE ITEM.

        {rfq/upd-note.i}

 /*   end. /* first-of */  */

    rfqitem.est-no = est.est-no.
   end.  /* each rfqitem */

   ce-ctrl.e-num = li-next-est.
   FIND CURRENT ce-ctrl NO-LOCK.
   LEAVE.
END.
END. /*repeat*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-est-3 Dialog-Frame 
PROCEDURE trx-rfq-to-est-3 :
/*------------------------------------------------------------------------------
  Purpose:     est-type 3 : Folding Two Piece Box 1-est 1-ef Multi-eb per ef
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-next-est as int no-undo.
def var li-next-enum as int no-undo.
def var li-cnt as int no-undo.
def var li-form-no as int no-undo.
def var li-blank-no as int no-undo.

REPEAT:

find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

find last est /* use-index e-num */ no-lock no-error.

ASSIGN
   li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0
   li-next-enum = if avail est then est.e-num else 0
   li-form-no = 1.

for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq,">>9")) > 0  
                        break by rfqitem.part-no:
/*    if first-of(rfqitem.part-no) then do: */
       create est.
       assign li-next-est = li-next-est + 1
              li-next-enum = li-next-enum + 1
              est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>").

       create ef.

       FIND FIRST ITEM WHERE
            ITEM.company EQ rfqitem.company AND
            ITEM.i-no EQ rfqitem.board
            NO-LOCK NO-ERROR.

       {rfq/asn-ef.i}
       
       RELEASE ITEM.

/*    end.  /* first-of */  */
    do li-cnt = 1 to li-num-of-blank: 
       create eb.
       li-blank-no = li-cnt.
       {rfq/asn-eb.i}  
       {ce/updunit#.i eb 0}
       {ce/updunit#.i eb 1}
    end.  /* do */

    rfqitem.est-no = est.est-no.
   end.  /* each rfqitem */

   ce-ctrl.e-num = li-next-est.
   FIND CURRENT ce-ctrl NO-LOCK.
   LEAVE.
END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-est-4 Dialog-Frame 
PROCEDURE trx-rfq-to-est-4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-est as int no-undo.
  def var li-next-enum as int no-undo.
  def var li-cnt as int no-undo.
  def var li-cnt2 as int no-undo.
  def var li-form-no as int no-undo.
  def var li-blank-no as int no-undo.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-key AS cha NO-UNDO.

  DISABLE TRIGGERS FOR LOAD OF est.
  DISABLE TRIGGERS FOR LOAD OF reftable.

  REPEAT:
  
  find first ce-ctrl where
       ce-ctrl.company = rfqitem.company and
       ce-ctrl.loc = rfqitem.loc
       EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

  IF AVAIL ce-ctrl THEN
  DO:

  find last est /*use-index e-num */ no-lock no-error.

  ASSIGN
  li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0
  li-next-enum = if avail est then est.e-num else 0

  li-form-no = 1
  li-cnt = 0
  li-num-of-blank = num-entries(ls-seq-list) - 1. /* don't count last comma */
  
  for each rfqitem where
      rfqitem.company EQ rfq.company AND
      rfqitem.loc EQ rfq.loc AND
      rfqitem.rfq-no EQ rfq.rfq-no AND
      index(ls-seq-list,string(rfqitem.seq,">>9")) > 0 
      break by rfqitem.part-no:

      if first(rfqitem.part-no) then do:
         li-next-est = li-next-est + 1.
         li-next-enum = li-next-enum + 1.
         li-cnt2 = 0.
       create est.
       assign est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>")                          
              est.form-qty = li-num-of-form
              est.est-date = today 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
      ls-key = string(today,"99999999") +
             string(next-value(rec_key_seq,nosweat),"99999999")
      est.rec_key = ls-key.       

      CREATE est-qty.
      ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1]
           .
      DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
      END.
      run create-est-prep (li-next-enum, STRING(li-next-est,">>>>>>>>")).
    
      /* do li-cnt = 1 to li-num-of-form: */
          li-form-no = li-cnt + 1.
          create ef.

          FIND FIRST ITEM WHERE
               ITEM.company EQ rfqitem.company AND
               ITEM.i-no EQ rfqitem.board
               NO-LOCK NO-ERROR.

          {rfq/asn-ef.i}

          RELEASE ITEM.
      /* end.*/
      {rfq/upd-note.i}
    end.  /* first   i-est 1-ef */  
    if rfqitem.ship-id <> "" then do:
       find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
       if avail shipto then ls-dest-code = shipto.dest-code.
    end.    
    else do:
       find cust where cust.company = rfqitem.company and
                       cust.cust-no = rfq.cust-no 
                       no-lock no-error.
       if avail cust then ls-dest-code = cust.del-zone.
    end.
    /*   do li-cnt2 = 1 to li-num-of-blank: */
             create eb.         /* 1-eb per rfqitem */
             ASSIGN
             li-cnt2 = li-cnt2 + 1
             li-blank-no = li-cnt2.
             {rfq/asn-eb.i}
             {ce/updunit#.i eb 0}
             {ce/updunit#.i eb 1}
      /*    end.
       end.
      */
     assign rfqitem.est-no = est.est-no
            rfqitem.form-no = li-form-no
            rfqitem.blank-no = li-blank-no
            .
  end.  /* each rfqitem */

  ce-ctrl.e-num = li-next-est.
  FIND CURRENT ce-ctrl NO-LOCK.
  LEAVE.
  END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-est-6 Dialog-Frame 
PROCEDURE trx-rfq-to-est-6 :
/*------------------------------------------------------------------------------
  Purpose:     for Corrugated set single blank(eb) per ef 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def var li-next-est as int no-undo.
  def var li-next-enum as int no-undo.
  def var li-cnt as int no-undo.
  def var li-cnt2 as int no-undo.
  def var li-form-no as int no-undo.
  def var li-blank-no as int no-undo.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-est-recid AS RECID NO-UNDO.
  DEF VAR ls-key AS cha NO-UNDO.
  DEF BUFFER bf-ritem FOR rfqitem.
  DISABLE TRIGGERS FOR LOAD OF est.
  DISABLE TRIGGERS FOR LOAD OF est-qty.
  DISABLE TRIGGERS FOR LOAD OF ef.
  DISABLE TRIGGERS FOR LOAD OF eb.
  DISABLE TRIGGERS FOR LOAD OF reftable.

  REPEAT:
  
  find first ce-ctrl where ce-ctrl.company = rfqitem.company and
                         ce-ctrl.loc = rfqitem.loc
                         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  
  IF AVAIL ce-ctrl THEN
  DO:
  
  find last est /*use-index e-num */ no-lock no-error.
  ASSIGN
  li-next-est = if avail ce-ctrl then ce-ctrl.e-num else 0
  li-next-enum = if avail est then est.e-num else 0

  li-form-no = 0
  li-cnt = 0
  li-num-of-blank = num-entries(ls-seq-list) - 1. /* don't count last comma */
  
  for each rfqitem of rfq where index(ls-seq-list,string(rfqitem.seq,">>9")) > 0  
                        break by rfqitem.part-no:
      if first(rfqitem.part-no) then do:

         create est.
         assign
              li-next-est = li-next-est + 1
              li-next-enum = li-next-enum + 1
              li-cnt2 = 0
              est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = string(li-next-est,">>>>>>>>")                          
              est.form-qty = li-num-of-form
              est.est-date = today 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
        ls-key = string(today,"99999999") +
             string(next-value(rec_key_seq,nosweat),"99999999")
        est.rec_key = ls-key
        lv-est-recid = RECID(est).
        CREATE est-qty.
        ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1]
           .
        DO i = 1 TO 99:
           est-qty.qty[i] = rfqitem.qty[i].
        END.

        run create-est-prep (li-next-enum,string(li-next-est,">>>>>>>>")).
        {rfq/upd-note.i}  
      end.  /* first   i-est 1-ef */  
      IF NOT AVAIL est THEN FIND est WHERE RECID(est) = lv-est-recid .

      if rfqitem.ship-id <> "" then do:
         find shipto where shipto.company = rfqitem.company and
                         shipto.cust-no = rfq.cust-no and
                         shipto.ship-id = rfqitem.ship-id
                         no-lock no-error.
         if avail shipto then ls-dest-code = shipto.dest-code.
      end.    
      else do:
         find cust where cust.company = rfqitem.company and
                         cust.cust-no = rfq.cust-no 
                         no-lock no-error.
         if avail cust then ls-dest-code = cust.del-zone.
      end.
      li-form-no = li-form-no + 1.
      create ef.

      FIND FIRST ITEM WHERE
           ITEM.company EQ rfqitem.company AND
           ITEM.i-no EQ rfqitem.board
           NO-LOCK NO-ERROR.

      {rfq/asn-ef.i}

      RELEASE ITEM.

      li-cnt2 = 0.
      create eb.         /* 1-eb per rfqitem */
      ASSIGN
      li-cnt2 = li-cnt2 + 1
      li-blank-no = li-cnt2.
      {rfq/asn-eb.i}
      {ce/updunit#.i eb 0}
      {ce/updunit#.i eb 1}
      
      assign rfqitem.est-no = est.est-no
             rfqitem.form-no = li-form-no
             rfqitem.blank-no = li-blank-no
            .
      IF LAST(rfqitem.part-no) then do:
         est.form-qty = li-form-no.
         /* create set header */
         FIND FIRST bf-ritem of rfq where bf-ritem.seq = 999  NO-ERROR.
         IF AVAIL bf-ritem THEN DO:
            /* create ef.
           {rfq/asn-efs.i}  no ef for SET header */
           create eb.         /* 1-eb per rfqitem */
           {rfq/asn-ebs.i}
           bf-ritem.est-no = est.est-no.
         END.
      END.
  end.  /* each rfqitem */

  ce-ctrl.e-num = li-next-est.
  FIND CURRENT ce-ctrl NO-LOCK.
  LEAVE.
  END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trx-rfq-to-estc Dialog-Frame 
PROCEDURE trx-rfq-to-estc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

