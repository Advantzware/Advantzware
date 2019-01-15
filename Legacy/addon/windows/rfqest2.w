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

DEF INPUT PARAMETER ip-rfq-recid AS RECID NO-UNDO.
DEF INPUT PARAMETER ip-rfqitem-recid AS RECID NO-UNDO.
DEF VAR lv-seq-max AS INT NO-UNDO.
DEF VAR ll-transfer AS LOG NO-UNDO.
DEF VAR li-est-type AS INT NO-UNDO.
DEF VAR ls-seq-list AS cha NO-UNDO.
DEF VAR ll-selected AS LOG NO-UNDO.
DEF VAR li-current-row AS INT NO-UNDO.
DEF VAR ls-find-list AS cha NO-UNDO.
DEF VAR ls-crt-list AS cha NO-UNDO.
DEF VAR ls-dest-code AS cha NO-UNDO.
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
      rfqitem.seq LABEL "#" FORM ">>9"
      rfqitem.stock-no LABEL "FG Item#"
      rfqitem.i-name LABEL "Item Name" FORM "x(30)"
      rfqitem.part-no
      rfqitem.style
      rfqitem.procat LABEL "Category"
      rfqitem.est-no LABEL "Est#" FORM "x(8)"
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
   DEF VAR li-cnt AS INT NO-UNDO.
   DEF VAR ll-return AS LOG NO-UNDO.
   DEF VAR ll-is-transfered AS LOG NO-UNDO.
   DEF VAR ls-est-list AS cha NO-UNDO.
   DEF BUFFER bf-item FOR rfqitem.

   IF li-est-type = 0 THEN DO:
      MESSAGE "Estimate type can't be blank. Please estimate type first!"
               VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cb-est-type.
      RETURN NO-APPLY.
   END.   
   IF li-est-type = 6 THEN DO:
      FIND FIRST rfqitem OF rfq WHERE rfqitem.seq = 999 NO-LOCK NO-ERROR.
      IF NOT AVAIL rfqitem THEN DO: 
         FIND FIRST bf-item OF rfq NO-LOCK NO-ERROR.
         CREATE rfqitem.
         BUFFER-COPY bf-item EXCEPT bf-item.seq TO rfqitem.
         rfqitem.seq = 999.
         RUN rfq/d-rfqset.w  (RECID(rfqitem),6).
      END.
      ELSE RUN rfq/d-rfqset.w  (RECID(rfqitem),6).
   END.
   ASSIGN li-num-of-form 
          li-num-of-blank
          ls-seq-list = ""
          ls-find-list = ""
          ls-crt-list = "" 
          ll-is-transfered = NO.
   
   IF {&browse-name}:num-selected-rows IN FRAME {&frame-name} > 0 THEN
   DO li-cnt = 1 TO {&browse-name}:num-selected-rows IN FRAME {&frame-name}:

      ASSIGN
        ll-return = {&browse-name}:fetch-selected-row(li-cnt)
        ls-seq-list = ls-seq-list + string(rfqitem.seq,">>9") + ",".  /* all list */
      /*ls-est-list = ls-est-list + string(rfqitem.est-no) + ",".*/
      
      /*   if rfqitem.est-no <> "" then 
         assign ls-find-list = ls-find-list + string(rfqitem.seq) + ","
                ll-is-transfered = yes.
         else ls-crt-list = ls-crt-list + string(rfqitem.seq) + ",".
      */ 
   END.
 
   IF rd-trx-type = 1 THEN DO: /* new */ 
      CASE li-est-type :
           WHEN 1 OR WHEN 5 THEN RUN trx-rfq-to-est.
           WHEN 2 OR WHEN 3 OR WHEN 4 THEN RUN trx-rfq-to-est-4.
           WHEN 6 THEN RUN trx-rfq-to-est-6.
           /*when 4 then run trx-rfq-to-est-4.
           when 5 then run trx-rfq-to-est-5.
           when 6 then run trx-rfq-to-est-6. */
      END CASE.
      
   END. 
   ELSE DO: /* update */
      
      ls-find-list = ls-seq-list.
      CASE li-est-type :
           WHEN 1 OR WHEN 5 THEN RUN find-est-update.
           WHEN 2 OR WHEN 3 OR WHEN 4 OR WHEN 7 OR WHEN 8 THEN RUN find-est-update4.
           WHEN 6 THEN RUN find-est-update6.
      END CASE.   
   END.
   

   APPLY "go" TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-est-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-est-type Dialog-Frame
ON VALUE-CHANGED OF cb-est-type IN FRAME Dialog-Frame /* Estimate Type */
DO:
   ASSIGN cb-est-type.
   CASE cb-est-type:
        WHEN "Folding Single Item" THEN li-est-type = 1.
        WHEN "Folding Two Piece Box" THEN li-est-type = 2.
        WHEN "Folding Tandem Runs" THEN li-est-type = 3.
        WHEN "Folding Combination" THEN li-est-type = 4.
        WHEN "Corrugated Single Item" THEN li-est-type = 5.
        WHEN "Corrugated Set" THEN li-est-type = 6.        
   END.
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

   RUN select-all. 
   DEF BUFFER bf-rfqitem FOR rfqitem.
   
   CASE li-est-type:
        WHEN 1 THEN DO:
             FOR EACH bf-rfqitem OF rfq :
                 ASSIGN bf-rfqitem.form-no = 1
                        bf-rfqitem.blank-no = 1. 
             END.
        END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-trx-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-trx-type Dialog-Frame
ON VALUE-CHANGED OF rd-trx-type IN FRAME Dialog-Frame
DO:
    ASSIGN rd-trx-type.
    {&open-query-{&browse-name}}    
    RUN select-all.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND rfq WHERE RECID(rfq) = ip-rfq-recid NO-LOCK.
  FIND sys-ctrl WHERE sys-ctrl.company = rfq.company AND
                      sys-ctrl.name = "cemenu"
                      NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN DO:
     CASE sys-ctrl.char-fld:
          WHEN "Corrware" THEN DO:
                cb-est-type:list-items = "Corrugated Single Item,Corrugated Set".
                cb-est-type:screen-value = ENTRY(1,cb-est-type:list-items).   
                li-est-type = 5.
          END.
          WHEN "foldware" THEN DO:
                cb-est-type:list-items = "Folding Single Item,Folding Two Piece Box,Folding Tandem Runs,Folding Combination".
                cb-est-type:screen-value = ENTRY(1,cb-est-type:list-items).   
                li-est-type = 1.
          END.
          WHEN "both" THEN DO:
                 cb-est-type:list-items = "Folding Single Item,Folding Two Piece Box,Folding Tandem Runs,Folding Combination," +
                                          "Corrugated Single Item,Corrugated Set".                 
                cb-est-type:screen-value = ENTRY(1,cb-est-type:list-items).   
                li-est-type = 1.
          END.
     END.  
  END.
  rd-trx-type = 1.
  RUN enable_UI.
  RUN select-all.  /* don't select not but select when type changed*/
  
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
   DEF INPUT PARAMETER ip-enum LIKE est.e-num NO-UNDO.
   DEF INPUT PARAMETER ip-est-no AS cha NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   DISABLE TRIGGERS FOR LOAD OF est-prep.

   i = 1.
   FOR EACH prep WHERE prep.company = rfqitem.company AND prep.dfault NO-LOCK:
       CREATE est-prep.
       ASSIGN   est-prep.e-num  = ip-enum
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
                
   END.

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
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF est.

SESSION:SET-WAIT-STATE("COMPILER").


FOR EACH rfqitem OF rfq WHERE INDEX(ls-find-list,STRING(rfqitem.seq,">>9")) > 0 :
    FIND est WHERE est.company = rfqitem.company AND
                   est.est-no = rfqitem.est-no
                   .
    ASSIGN est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
    FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.
    IF AVAIL est-qty THEN DO:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 98:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
    END.
    FIND FIRST ef WHERE ef.company = est.company
                    AND ef.est-no = est.est-no
                    AND ef.form-no = rfqitem.form-no.
    ASSIGN li-form-no = 1
           li-num-of-blank = 1 
           li-blank-no = 1.
    
    IF rfqitem.ship-id <> "" THEN DO:
       FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
    END.    
    ELSE DO:
       FIND cust WHERE cust.company = rfqitem.company AND
                       cust.cust-no = rfq.cust-no 
                       NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ls-dest-code = cust.del-zone.
    END.
           
    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/upd-ef.i}

    RELEASE ITEM.

    FIND FIRST eb WHERE eb.company = rfqitem.company
                    AND eb.est-no = rfqitem.est-no
                    AND eb.form-no = rfqitem.form-no
                    AND eb.blank-no = rfqitem.blank-no NO-ERROR.
  

    IF AVAIL eb THEN DO:     
        {rfq/upd-eb.i}
    END.    

END.  /* each rfqitem */


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
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF est.
SESSION:SET-WAIT-STATE("COMPILER").


FOR EACH rfqitem OF rfq WHERE INDEX(ls-find-list,STRING(rfqitem.seq,">>9")) > 0 :
    FIND est WHERE est.company = rfqitem.company AND
                   est.est-no = rfqitem.est-no
                   .
    ASSIGN est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.

     IF AVAIL est-qty THEN DO:
        est-qty.eqty = rfqitem.qty[1].
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     FIND FIRST ef WHERE ef.company = est.company AND 
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

    IF rfqitem.ship-id <> "" THEN DO:
       FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
    END.    
    ELSE DO:
       FIND cust WHERE cust.company = rfqitem.company AND
                       cust.cust-no = rfq.cust-no 
                       NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ls-dest-code = cust.del-zone.
    END.

       FIND FIRST eb OF ef WHERE eb.blank-no = rfqitem.blank-no  /*li-cnt*/  NO-ERROR.
       IF AVAIL eb THEN DO: 
          {rfq/upd-eb.i} 
       END.
       ELSE DO:
         LEAVE.
       END.  
  /*  end.*/    
END.  /* each rfqitem */


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
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF BUFFER bf-ritem FOR rfqitem.

DISABLE TRIGGERS FOR LOAD OF est.
SESSION:SET-WAIT-STATE("COMPILER").


FOR EACH rfqitem OF rfq WHERE INDEX(ls-find-list,STRING(rfqitem.seq,">>9")) > 0 :
    FIND est WHERE est.company = rfqitem.company AND
                   est.est-no = rfqitem.est-no
                   .
    ASSIGN est.est-qty[1] = rfqitem.qty[1]
           est.est-qty[2] = rfqitem.qty[2]
           est.est-qty[3] = rfqitem.qty[3]
           est.est-qty[4] = rfqitem.qty[4]                                             
           est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
           .
     
     FIND FIRST est-qty WHERE est-qty.company = est.company
                       AND est-qty.est-no = est.est-no NO-ERROR.

     IF AVAIL est-qty THEN DO:
        /*est-qty.eqty = rfqitem.qty[1].*/
        DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
        END.
     END.

     FIND FIRST ef WHERE ef.company = est.company AND 
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

    IF rfqitem.ship-id <> "" THEN DO:
       FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
    END.    
    ELSE DO:
       FIND cust WHERE cust.company = rfqitem.company AND
                       cust.cust-no = rfq.cust-no 
                       NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ls-dest-code = cust.del-zone.
    END.

    FIND FIRST eb OF ef WHERE eb.blank-no = rfqitem.blank-no  /*li-cnt*/  NO-ERROR.
    IF AVAIL eb THEN DO: 
          {rfq/upd-eb.i} 
    END.
    ELSE DO:
         LEAVE.
    END.  
END.  /* each rfqitem */
/* update set header */
  FIND FIRST bf-ritem OF rfq WHERE bf-ritem.seq = 999 NO-LOCK NO-ERROR.
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
  DEF VAR li-count AS INT NO-UNDO.
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
DEF VAR li-next-est AS INT NO-UNDO.
DEF VAR li-next-enum AS INT NO-UNDO.
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR ls-key AS cha NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF est-qty.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF reftable.

REPEAT:

FIND FIRST ce-ctrl WHERE
     ce-ctrl.company = rfqitem.company AND
     ce-ctrl.loc = rfqitem.loc
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

li-next-est = IF AVAIL ce-ctrl THEN ce-ctrl.e-num ELSE 0.
FIND LAST est /*use-index e-num no in V9 */ NO-LOCK NO-ERROR.
li-next-enum = IF AVAIL est THEN est.e-num ELSE 0.

SESSION:SET-WAIT-STATE("COMPILER").

FOR EACH rfqitem OF rfq WHERE INDEX(ls-seq-list,STRING(rfqitem.seq,">>9")) > 0 :

    CREATE est.
    ASSIGN
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
       est.est-no   = STRING(li-next-est,">>>>>>>>")                          
       est.form-qty = 1               
       est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
       est.est-date = TODAY
       ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey")
       est.rec_key = ls-key.

    CREATE est-qty.
    ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1].

    DO i = 1 TO 99:
       est-qty.qty[i] = rfqitem.qty[i].
    END.

    RUN create-est-prep (li-next-enum,STRING(li-next-est,">>>>>>>>")).

    IF rfqitem.ship-id <> "" THEN DO:
       FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
    END.    
    ELSE DO:
       FIND cust WHERE cust.company = rfqitem.company AND
                       cust.cust-no = rfq.cust-no 
                       NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ls-dest-code = cust.del-zone.
    END.
    
    CREATE ef.

    FIND FIRST ITEM WHERE
         ITEM.company EQ rfqitem.company AND
         ITEM.i-no EQ rfqitem.board
         NO-LOCK NO-ERROR.

    {rfq/asn-ef.i}

    RELEASE ITEM.

    CREATE eb.
    {rfq/asn-eb.i}

    {ce/updunit#.i eb}

   ASSIGN rfqitem.est-no = est.est-no
          rfqitem.form-no = li-form-no
          rfqitem.blank-no = li-blank-no.
    

 END.  /* each rfqitem */

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
DEF VAR li-next-est AS INT NO-UNDO.
DEF VAR li-next-enum AS INT NO-UNDO.
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.

SESSION:SET-WAIT-STATE("COMPILER").

REPEAT:

FIND FIRST ce-ctrl WHERE
     ce-ctrl.company = rfqitem.company AND
     ce-ctrl.loc = rfqitem.loc
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

li-next-est = IF AVAIL ce-ctrl THEN ce-ctrl.e-num ELSE 0.
FIND LAST est /* use-index e-num */ NO-LOCK NO-ERROR.
li-next-enum = IF AVAIL est THEN est.e-num ELSE 0.

FOR EACH rfqitem OF rfq WHERE INDEX(ls-seq-list,STRING(rfqitem.seq,">>9")) > 0  
                        BREAK BY rfqitem.part-no:

/*    if first-of(rfqitem.part-no) then do: */
       CREATE est.
       ASSIGN 
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
       est.est-no   = STRING(li-next-est,">>>>>>>>").
        FIND FIRST ITEM WHERE
             ITEM.company EQ rfqitem.company AND
             ITEM.i-no EQ rfqitem.board
             NO-LOCK NO-ERROR.

        DO li-cnt = 1 TO li-num-of-form:
           CREATE ef.
           ASSIGN li-form-no = li-cnt
                  li-blank-no = 1.
           {rfq/asn-ef.i}

           CREATE eb.
           {rfq/asn-eb.i}
           {ce/updunit#.i eb}
        END.

        RELEASE ITEM.
        

 /*   end. /* first-of */  */

    rfqitem.est-no = est.est-no.
   END.  /* each rfqitem */

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
DEF VAR li-next-est AS INT NO-UNDO.
DEF VAR li-next-enum AS INT NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR li-form-no AS INT NO-UNDO.
DEF VAR li-blank-no AS INT NO-UNDO.

REPEAT:

FIND FIRST ce-ctrl WHERE ce-ctrl.company = rfqitem.company AND
                         ce-ctrl.loc = rfqitem.loc
                        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAIL ce-ctrl THEN
DO:

FIND LAST est /* use-index e-num */ NO-LOCK NO-ERROR.

ASSIGN
   li-next-est = IF AVAIL ce-ctrl THEN ce-ctrl.e-num ELSE 0
   li-next-enum = IF AVAIL est THEN est.e-num ELSE 0
   li-form-no = 1.

FOR EACH rfqitem OF rfq WHERE INDEX(ls-seq-list,STRING(rfqitem.seq,">>9")) > 0  
                        BREAK BY rfqitem.part-no:
/*    if first-of(rfqitem.part-no) then do: */
       CREATE est.
       ASSIGN li-next-est = li-next-est + 1
              li-next-enum = li-next-enum + 1
              est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = STRING(li-next-est,">>>>>>>>").

       CREATE ef.

       FIND FIRST ITEM WHERE
            ITEM.company EQ rfqitem.company AND
            ITEM.i-no EQ rfqitem.board
            NO-LOCK NO-ERROR.

       {rfq/asn-ef.i}
       
       RELEASE ITEM.

/*    end.  /* first-of */  */
    DO li-cnt = 1 TO li-num-of-blank: 
       CREATE eb.
       li-blank-no = li-cnt.
       {rfq/asn-eb.i}  
       {ce/updunit#.i eb}
    END.  /* do */

    rfqitem.est-no = est.est-no.
   END.  /* each rfqitem */

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
  DEF VAR li-next-est AS INT NO-UNDO.
  DEF VAR li-next-enum AS INT NO-UNDO.
  DEF VAR li-cnt AS INT NO-UNDO.
  DEF VAR li-cnt2 AS INT NO-UNDO.
  DEF VAR li-form-no AS INT NO-UNDO.
  DEF VAR li-blank-no AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-key AS cha NO-UNDO.

  DISABLE TRIGGERS FOR LOAD OF est.
  DISABLE TRIGGERS FOR LOAD OF reftable.

  REPEAT:
  
  FIND FIRST ce-ctrl WHERE
       ce-ctrl.company = rfqitem.company AND
       ce-ctrl.loc = rfqitem.loc
       EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

  IF AVAIL ce-ctrl THEN
  DO:

  FIND LAST est /*use-index e-num */ NO-LOCK NO-ERROR.

  ASSIGN
  li-next-est = IF AVAIL ce-ctrl THEN ce-ctrl.e-num ELSE 0
  li-next-enum = IF AVAIL est THEN est.e-num ELSE 0

  li-form-no = 1
  li-cnt = 0
  li-num-of-blank = NUM-ENTRIES(ls-seq-list) - 1. /* don't count last comma */
  
  FOR EACH rfqitem WHERE
      rfqitem.company EQ rfq.company AND
      rfqitem.loc EQ rfq.loc AND
      rfqitem.rfq-no EQ rfq.rfq-no AND
      index(ls-seq-list,STRING(rfqitem.seq,">>9")) > 0 
      BREAK BY rfqitem.part-no:

      IF FIRST(rfqitem.part-no) THEN DO:
         li-next-est = li-next-est + 1.
         li-next-enum = li-next-enum + 1.
         li-cnt2 = 0.
       CREATE est.
       ASSIGN est.company  = rfqitem.company                                     
              est.loc      = rfqitem.loc
              est.est-qty[1] = rfqitem.qty[1]
              est.est-qty[2] = rfqitem.qty[2]
              est.est-qty[3] = rfqitem.qty[3]
              est.est-qty[4] = rfqitem.qty[4]                                             
              est.est-type = li-est-type
              est.e-num    = li-next-enum
              est.est-no   = STRING(li-next-est,">>>>>>>>")                          
              est.form-qty = li-num-of-form
              est.est-date = TODAY 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
      ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey")
      est.rec_key = ls-key.       

      CREATE est-qty.
      ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = rfqitem.qty[1]
           .
      DO i = 1 TO 99:
          est-qty.qty[i] = rfqitem.qty[i].
      END.
      RUN create-est-prep (li-next-enum, STRING(li-next-est,">>>>>>>>")).
    
      /* do li-cnt = 1 to li-num-of-form: */
          li-form-no = li-cnt + 1.
          CREATE ef.

          FIND FIRST ITEM WHERE
               ITEM.company EQ rfqitem.company AND
               ITEM.i-no EQ rfqitem.board
               NO-LOCK NO-ERROR.

          {rfq/asn-ef.i}

          RELEASE ITEM.
      /* end.*/
      
    END.  /* first   i-est 1-ef */  
    IF rfqitem.ship-id <> "" THEN DO:
       FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
    END.    
    ELSE DO:
       FIND cust WHERE cust.company = rfqitem.company AND
                       cust.cust-no = rfq.cust-no 
                       NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ls-dest-code = cust.del-zone.
    END.
    /*   do li-cnt2 = 1 to li-num-of-blank: */
             CREATE eb.         /* 1-eb per rfqitem */
             ASSIGN
             li-cnt2 = li-cnt2 + 1
             li-blank-no = li-cnt2.
             {rfq/asn-eb.i}
             {ce/updunit#.i eb}
      /*    end.
       end.
      */
     ASSIGN rfqitem.est-no = est.est-no
            rfqitem.form-no = li-form-no
            rfqitem.blank-no = li-blank-no
            .
  END.  /* each rfqitem */

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

  DEF VAR li-next-est AS INT NO-UNDO.
  DEF VAR li-next-enum AS INT NO-UNDO.
  DEF VAR li-cnt AS INT NO-UNDO.
  DEF VAR li-cnt2 AS INT NO-UNDO.
  DEF VAR li-form-no AS INT NO-UNDO.
  DEF VAR li-blank-no AS INT NO-UNDO.
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
  
  FIND FIRST ce-ctrl WHERE ce-ctrl.company = rfqitem.company AND
                         ce-ctrl.loc = rfqitem.loc
                         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  
  IF AVAIL ce-ctrl THEN
  DO:
  
  FIND LAST est /*use-index e-num */ NO-LOCK NO-ERROR.
  ASSIGN
  li-next-est = IF AVAIL ce-ctrl THEN ce-ctrl.e-num ELSE 0
  li-next-enum = IF AVAIL est THEN est.e-num ELSE 0

  li-form-no = 0
  li-cnt = 0
  li-num-of-blank = NUM-ENTRIES(ls-seq-list) - 1. /* don't count last comma */
  
  FOR EACH rfqitem OF rfq WHERE INDEX(ls-seq-list,STRING(rfqitem.seq,">>9")) > 0  
                        BREAK BY rfqitem.part-no:
      IF FIRST(rfqitem.part-no) THEN DO:

         CREATE est.
         ASSIGN
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
              est.est-no   = STRING(li-next-est,">>>>>>>>")                          
              est.form-qty = li-num-of-form
              est.est-date = TODAY 
              est.mod-date = 01/01/1900  /* Indiana mods Task# 05110404 */
        ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey")
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

        RUN create-est-prep (li-next-enum,STRING(li-next-est,">>>>>>>>")).
          
      END.  /* first   i-est 1-ef */  
      IF NOT AVAIL est THEN FIND est WHERE RECID(est) = lv-est-recid .

      IF rfqitem.ship-id <> "" THEN DO:
         FIND shipto WHERE shipto.company = rfqitem.company AND
                         shipto.cust-no = rfq.cust-no AND
                         shipto.ship-id = rfqitem.ship-id
                         NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN ls-dest-code = shipto.dest-code.
      END.    
      ELSE DO:
         FIND cust WHERE cust.company = rfqitem.company AND
                         cust.cust-no = rfq.cust-no 
                         NO-LOCK NO-ERROR.
         IF AVAIL cust THEN ls-dest-code = cust.del-zone.
      END.
      li-form-no = li-form-no + 1.
      CREATE ef.

      FIND FIRST ITEM WHERE
           ITEM.company EQ rfqitem.company AND
           ITEM.i-no EQ rfqitem.board
           NO-LOCK NO-ERROR.

      {rfq/asn-ef.i}

      RELEASE ITEM.

      li-cnt2 = 0.
      CREATE eb.         /* 1-eb per rfqitem */
      ASSIGN
      li-cnt2 = li-cnt2 + 1
      li-blank-no = li-cnt2.
      {rfq/asn-eb.i}
      {ce/updunit#.i eb}
      
      ASSIGN rfqitem.est-no = est.est-no
             rfqitem.form-no = li-form-no
             rfqitem.blank-no = li-blank-no
            .
      IF LAST(rfqitem.part-no) THEN DO:
         est.form-qty = li-form-no.
         /* create set header */
         FIND FIRST bf-ritem OF rfq WHERE bf-ritem.seq = 999  NO-ERROR.
         IF AVAIL bf-ritem THEN DO:
            /* create ef.
           {rfq/asn-efs.i}  no ef for SET header */
           CREATE eb.         /* 1-eb per rfqitem */
           {rfq/asn-ebs.i}
           bf-ritem.est-no = est.est-no.
         END.
      END.
  END.  /* each rfqitem */

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

