&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\addon\fg\b-issue.i
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser

&Scoped-define ADM-SUPPORTED-LINKS                                                                           Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.barcode fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.partial ~
fg-rctd.t-qty fg-rctd.std-cost fg-rctd.pur-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.barcode fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.partial 
&Scoped-define FIELD-PAIRS-IN-QUERY-Browser-Table~
 ~{&FP1}rct-date ~{&FP2}rct-date ~{&FP3}~
 ~{&FP1}barcode ~{&FP2}barcode ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}i-name ~{&FP2}i-name ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}loc ~{&FP2}loc ~{&FP3}~
 ~{&FP1}loc-bin ~{&FP2}loc-bin ~{&FP3}~
 ~{&FP1}cases ~{&FP2}cases ~{&FP3}~
 ~{&FP1}qty-case ~{&FP2}qty-case ~{&FP3}~
 ~{&FP1}partial ~{&FP2}partial ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = cocode and ~
fg-rctd.rita-code = "S" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-4 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rctd.rct-date
      fg-rctd.barcode
      fg-rctd.i-no
      fg-rctd.i-name
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.partial
      fg-rctd.t-qty
      fg-rctd.std-cost
      fg-rctd.pur-uom
  ENABLE
      fg-rctd.rct-date
      fg-rctd.barcode
      fg-rctd.i-no
      fg-rctd.i-name
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 79 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 133 HELP
          "CLEAR AUTO FIND Value"
     RECT-5 AT ROW 1 COL 1
     RECT-4 AT ROW 16.48 COL 2
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 18.43
         WIDTH              = 154.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "JOBS.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "fg-rctd.company = cocode and
fg-rctd.rita-code = ""S"""
     _FldNameList[1]   > JOBS.fg-rctd.rct-date
"fg-rctd.rct-date" ? ? "date" ? ? ? ? ? ? yes ?
     _FldNameList[2]   > JOBS.fg-rctd.barcode
"fg-rctd.barcode" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[3]   > JOBS.fg-rctd.i-no
"fg-rctd.i-no" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[4]   > JOBS.fg-rctd.i-name
"fg-rctd.i-name" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[5]   > JOBS.fg-rctd.job-no
"fg-rctd.job-no" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[6]   > JOBS.fg-rctd.job-no2
"fg-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[7]   > JOBS.fg-rctd.loc
"fg-rctd.loc" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[8]   > JOBS.fg-rctd.loc-bin
"fg-rctd.loc-bin" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[9]   > JOBS.fg-rctd.cases
"fg-rctd.cases" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[10]   > JOBS.fg-rctd.qty-case
"fg-rctd.qty-case" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[11]   > JOBS.fg-rctd.partial
"fg-rctd.partial" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[12]   = JOBS.fg-rctd.t-qty
     _FldNameList[13]   = JOBS.fg-rctd.std-cost
     _FldNameList[14]   = JOBS.fg-rctd.pur-uom
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).

   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
 IF NOT avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid no-lock no-error. 

 def var ll-tag# as log no-undo.
 ll-help-run = yes.
 case focus:name :
  /*    when "po-no" then do:
           run windows/l-poordl.w (fg-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                     fg-rctd.i-no:screen-value in browse {&browse-name} = entry(2,char-val)
                     fg-rctd.i-name:screen-value in browse {&browse-name} = entry(3,char-val)
                     fg-rctd.job-no:screen-value in browse {&browse-name} = entry(4,char-val)
                     fg-rctd.job-no2:screen-value in browse {&browse-name} = entry(5,char-val)
                     .
             find po-ordl where po-ordl.company = fg-rctd.company and
                                po-ordl.po-no = integer(entry(1,char-val)) and
                                po-ordl.line = integer(entry(6,char-val))
                                no-lock no-error.
             if avail po-ordl then do:
                assign fg-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */
                       fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /* pr-uom */
                       fg-rctd.s-num:screen-value in browse {&browse-name} = string(po-ordl.s-num)
                       fg-rctd.qty:screen-value in browse {&browse-name} = string(po-ordl.ord-qty)
                       fg-rctd.cost:screen-value in browse {&browse-name} = string(po-ordl.cons-cost)  /* po-ordl.cost*/

                    /* need conversion
                       ext-cost:screen-value = string(decimal(fg-rctd.qty:screen-value) *
                                                      decimal(fg-rctd.cost:screen-value)
                                                      )
                    */                                 
                       .
             end.
             else do:
                find first item where item.company = fg-rctd.company and
                                      item.i-no = entry(2,char-val)
                                      no-lock no-error.
                assign fg-rctd.pur-uom:screen-value in browse {&browse-name} = item.cons-uom
                       fg-rctd.cost-uom:screen-value in browse {&browse-name} = item.cons-uom                       
/*                       fg-rctd.s-num:screen-value in browse {&browse-name} = string(item.s-num)*/
                       .                      
             end.
             if not avail item then find first item where item.company = fg-rctd.company and
                                                          item.i-no = entry(2,char-val)
                                      no-lock no-error.

             assign fg-rctd.loc:screen-value in browse {&browse-name} =  item.loc
                    fg-rctd.loc-bin:screen-value in browse {&browse-name} =  item.loc-bin
                    .
             if fg-rctd.loc-bin:screen-value in browse {&browse-name} eq "" then do:
                  find first sys-ctrl where sys-ctrl.company eq fg-rctd.company
                                          and sys-ctrl.name    eq "AUTOISSU"
                                no-lock no-error.
                  if not avail sys-ctrl then do:
                        create sys-ctrl.
                        assign sys-ctrl.company = fg-rctd.company
                                   sys-ctrl.name    = "AUTOISSU"
                                   sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs"
                                   sys-ctrl.log-fld = yes.
                        message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                           update sys-ctrl.char-fld.
                  end.
                  assign fg-rctd.loc-bin:screen-value in browse {&browse-name} = sys-ctrl.char-fld.
             end.
             run tag-method (output ll-tag#).
             if ll-tag# and fg-rctd.po-no:screen-value in browse {&browse-name} <> ""
             then do:
                 run tag-sequence.
             end.
             ext-cost = 0.
             disp ext-cost with browse {&browse-name}.
           end.  /* char-val <> "" */
           return no-apply.   
     end.
*/
     when "i-no" then do:
           run windows/l-itemFG.w (fg-rctd.company,output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                     fg-rctd.i-name:screen-value IN BROWSE {&BRowse-name} = entry(2,char-val)
                     fg-rctd.job-no:screen-value = entry(3,char-val)
                     fg-rctd.job-no2:screen-value = entry(4,char-val)
                     .
           end.
           return no-apply.   
     end.
     when "job-no" or when "job-no2" then do:
           run windows/l-job.w (fg-rctd.company,fg-rctd.i-no:screen-value, output char-val).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     fg-rctd.job-no:screen-value = entry(1,char-val)
                     fg-rctd.job-no2:screen-value = entry(2,char-val)
                     .

           end.
           return no-apply.   
     end.  
     when "loc" then do:
           run rm/l-loc.w (fg-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                     fg-rctd.loc-bin:screen-value in browse {&browse-name} = entry(2,char-val)
                     .

           end.
           return no-apply.   
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (fg-rctd.company,fg-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val)
                     fg-rctd.loc:screen-value = entry(2,char-val)
                     fg-rctd.cases:screen-value = entry(3,char-val)
              /*       fg-rctd.tag:screen-value = entry(4,char-val)*/
                     .

           end.
           return no-apply.   
     end.
   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  /* display calculated field */
  /* def var ii as int.
   ii = if avail rm-rctd then integer(rm-rctd.po-no) else 0.

   if avail rm-rctd then    run get-matrix (true).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i}*/
   {rfq/brsleave.i}  /* same as src but update will be same as add record*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign fg-rctd.company = cocode
         fg-rctd.r-no    = 0
         fg-rctd.rita-code = "S"
        /* fg-rctd.s-num  = 0
         fg-rctd.s-num:screen-value in browse {&browse-name} = "0" 
         */
         fg-rctd.rct-date = today
         fg-rctd.trans-time = TIME.

  disp fg-rctd.rct-date with browse {&browse-name}. 
  lv-recid = recid(fg-rctd).  

/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and fg-rctd:po-no:screen*/
  run tag-sequence.
*/  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  MESSAGE "Delete Currently Selected Record?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
  IF NOT response THEN RETURN "ADM-ERROR":U.

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
     hd-post-child = hd-post:current-window.    
    /*  
     do while valid-handle(hd-post-child):
        ii = ii + 1.
        hd-post-child = hd-post-child:first-child.  /* frame */
       /* if hd-post-child:type = "field-group" 
           then hd-next = hd-post-child:next-sibling.
       */
       message ii valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.   
     end. 
    */ 
     hd-post-child = hd-post-child:first-child.  /* frame */
     hd-post-child = hd-post-child:first-child. /* field-group */
     hd-post-child = hd-post-child:first-child.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:sensitive = no.
  end.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to fg-rctd.rct-date in browse {&browse-name}.
  return no-apply.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* when new record created from last row, get error "No rm-rctd" record ava */
  if not avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid no-error.

  if error-status:error then return no-apply.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "fg-rctd"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-tag# as log no-undo.

  cocode = rm-rctd.company.

  {rm/tag#.i}
  op-tag# = v-tag#.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-tag-seq as int no-undo.
  def var v-locode as cha no-undo.
  def buffer xfg-rctd for fg-rctd.

  assign v-tag-seq = 0
         v-locode  = "".

  do while true:
    find first xfg-rctd
        where xfg-rctd.company eq fg-rctd.company
          and xfg-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xfg-rctd then do:
      v-locode = xfg-rctd.loc.

      for each xfg-rctd where xfg-rctd.company eq fg-rctd.company
            and xfg-rctd.loc     eq v-locode
            and xfg-rctd.tag     begins string(int(fg-rctd.i-no:screen-value in browse {&browse-name}),"999999")
            use-index tag no-lock
            by xfg-rctd.tag desc:

           if int(substr(xfg-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xfg-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first fg-rctdh where fg-rctdh.company eq rm-rcth.company
          and fg-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail fg-rctdh then do:
      v-locode = fg-rctdh.loc.

      for each fg-rctdh
          where fg-rctdh.company eq cocode
            and fg-rctdh.loc     eq v-locode
            and fg-rctdh.tag     begins string(int(fg-rctd.po-no),"999999")
          use-index tag no-lock
          by fg-rctdh.tag desc:

        if int(substr(fg-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(fg-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign
   v-tag-seq   = v-tag-seq + 1.
/*   fg-rctd.tag:screen-value in browse {&browse-name}
          = string(int(fg-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
*/          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  run get-matrix (true).
  return ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


