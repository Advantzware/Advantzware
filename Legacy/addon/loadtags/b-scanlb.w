&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF VAR lv-fgfile AS LOG NO-UNDO. /* autopost */
def var ls-prev-po as cha no-undo.
DEF VAR lv-overrun-checked AS LOG NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.

DEF BUFFER bf-tmp FOR loadtag.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.

{pc/pcprdd4u.i NEW}

DEF VAR char-hdl AS cha NO-UNDO.

DEF VAR v-out AS INT NO-UNDO.

DEF VAR v-msgreturn AS INT NO-UNDO.
DEF TEMP-TABLE tt-tag LIKE loadtag USE-INDEX cast-tag.
DEF BUFFER bf-tag FOR tt-tag.
DEF BUFFER bf-tag2 FOR tt-tag.
DEF VAR v-first AS LOG NO-UNDO.

DO TRANSACTION:
  {sys/inc/fgrecpt.i}
END.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-tag.tag-no tt-tag.i-no tt-tag.job-no tt-tag.job-no2 tt-tag.loc tt-tag.loc-bin tt-tag.case-bundle tt-tag.pallet-count tt-tag.qty-case tt-tag.tot-cases tt-tag.qty tt-tag.partial tt-tag.tag-date tt-tag.shift   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-tag.tag-no   /* tt-tag.case-bundle ~
tt-tag.pallet-count ~
tt-tag.qty-case ~
tt-tag.tot-cases ~
tt-tag.partial ~
*/   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-tag
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-tag
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-tag WHERE ~{&KEY-PHRASE}      /* AND loadtag.company = g_company and loadtag.item-type = no and is-case-tag */ NO-LOCK     /*~{&SORTBY-PHRASE}*/
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-tag WHERE ~{&KEY-PHRASE}      /* AND loadtag.company = g_company and loadtag.item-type = no and is-case-tag */ NO-LOCK     /*~{&SORTBY-PHRASE}*/.
&Scoped-define TABLES-IN-QUERY-br_table tt-tag
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-tag


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
r-no|y|y|ASI.fg-rctd.r-no
company||y|ASI.fg-rctd.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "r-no",
     Keys-Supplied = "r-no,company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
      tt-tag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-tag.tag-no FORMAT "X(23)":U
      tt-tag.i-no FORMAT "x(15)":U
      tt-tag.job-no FORMAT "x(6)":U
      tt-tag.job-no2 FORMAT ">9":U
      tt-tag.loc FORMAT "x(5)":U
      tt-tag.loc-bin COLUMN-LABEL " Bin" FORMAT "x(8)":U
      tt-tag.case-bundle FORMAT "->,>>>,>>9":U
      tt-tag.pallet-count FORMAT "->,>>>,>>9":U
      tt-tag.qty-case FORMAT "->,>>>,>>9":U
      tt-tag.tot-cases FORMAT "->,>>>,>>9":U
      tt-tag.qty FORMAT "->>>>>>9.9<<":U
      tt-tag.partial FORMAT ">>>,>>9":U
      tt-tag.tag-date FORMAT "99/99/9999":U
      tt-tag.shift FORMAT "x":U
  ENABLE
      tt-tag.tag-no
    /*  tt-tag.case-bundle
      tt-tag.pallet-count
      tt-tag.qty-case
      tt-tag.tot-cases
      tt-tag.partial
      */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 112 BY 7.14
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 7.33
         WIDTH              = 112.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tag WHERE ~{&KEY-PHRASE}
     /* AND loadtag.company = g_company and
loadtag.item-type = no and
is-case-tag */ NO-LOCK
    /*~{&SORTBY-PHRASE}*/.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.loadtag.company = g_company and
loadtag.item-type = no and
is-case-tag"
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
ON HELP OF br_table IN FRAME F-Main
DO:
  def var ll-tag# as log no-undo.
  DEF VAR rec-val AS RECID NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.


  ll-help-run = yes.
  case focus:name :
      /*
       when "po-no" then do:
            run windows/l-pofg.w (loadtag.company,focus:screen-value, output char-val).
            if char-val <> "" then do:
               assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                      loadtag.i-no:screen-value in browse {&browse-name} = entry(2,char-val)
                      loadtag.i-name:screen-value in browse {&browse-name} = entry(3,char-val)
                      loadtag.job-no:screen-value in browse {&browse-name} = entry(4,char-val)
                      loadtag.job-no2:screen-value in browse {&browse-name} = entry(5,char-val)
                      .
              find po-ordl where po-ordl.company = loadtag.company and
                                 po-ordl.po-no = integer(entry(1,char-val)) and
                                 po-ordl.line = integer(entry(6,char-val))
                                 no-lock no-error.
              if avail po-ordl then do:
                 assign /*-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                        loadtag.cost-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /* pr-uom */
                        loadtag.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cons-cost)  /* po-ordl.cost*/
                        .
              end.

              find first itemfg where itemfg.company = loadtag.company and
                                       itemfg.i-no = entry(2,char-val)
                                       no-lock no-error.
              if avail itemfg then do:                         
                 assign loadtag.loc:screen-value in browse {&browse-name} =  itemfg.def-loc
                        loadtag.loc-bin:screen-value in browse {&browse-name} =  itemfg.def-loc-bin
                        loadtag.qty-case:screen-value in browse {&browse-name} = string(itemfg.case-count)
                      /*  loadtag.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                                           else itemfg.prod-uom  */                        
                        .
              end.          
              if loadtag.loc-bin:screen-value in browse {&browse-name} eq "" then do:
                   find first sys-ctrl where sys-ctrl.company eq loadtag.company
                                           and sys-ctrl.name    eq "AUTOISSU"
                                 no-lock no-error.
                   if not avail sys-ctrl then do:
                         create sys-ctrl.
                         assign sys-ctrl.company = loadtag.company
                                    sys-ctrl.name    = "AUTOISSU"
                                    sys-ctrl.descrip = "Automatically Issue RM Receipts to asi"
                                    sys-ctrl.log-fld = yes.
                         message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                            update sys-ctrl.char-fld.
                   end.
                   assign loadtag.loc-bin:screen-value in browse {&browse-name} = sys-ctrl.char-fld.
              end.
 /*
              run tag-method (output ll-tag#).
              if ll-tag# and loadtag.po-no:screen-value in browse {&browse-name} <> ""
              then do:
                  run tag-sequence.
              end.
   */
              loadtag.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
            end.  /* char-val <> "" */
            return no-apply.   
      end.
      
      when "i-no" then do:
            /*IF loadtag.po-no:SCREEN-VALUE <> "" THEN DO:
               RUN windows/l-poitem.w (loadtag.company,loadtag.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
               if char-val <> "" then do :
                  assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                      loadtag.i-name:screen-value = entry(2,char-val)
                      loadtag.job-no:screen-value = entry(3,char-val)
                      loadtag.job-no2:screen-value = entry(4,char-val)
                      .
               end.
            END.
            ELSE*/ IF loadtag.job-no:SCREEN-VALUE <> "" THEN DO:
                 RUN windows/l-jobit1.w (loadtag.company,loadtag.job-no:SCREEN-VALUE,loadtag.job-no2:screen-value, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
                 IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                 IF rec-val <> ? THEN DO:
                    FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.
                    IF AVAIL tt-job-hdr THEN 
                        ASSIGN loadtag.std-cost:SCREEN-VALUE = string(tt-job-hdr.std-mat-cost +
                                                             tt-job-hdr.std-lab-cost +
                                                             tt-job-hdr.std-fix-cost +
                                                             tt-job-hdr.std-var-cost)
                              .

                 END.

            END.
            ELSE DO:
                 RUN windows/l-itemf2.w (loadtag.company, loadtag.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                 IF rec-val <> ? THEN DO:
                    FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                    ASSIGN loadtag.i-no:SCREEN-VALUE  = itemfg.i-no
                      loadtag.i-name:SCREEN-VALUE = itemfg.i-name
                      loadtag.loc:SCREEN-VALUE = itemfg.def-loc
                      loadtag.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                      loadtag.std-cost:SCREEN-VALUE = string(itemfg.avg-cost)
                      loadtag.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

                 END.
            END.
            return no-apply.   
      end.
      when "job-no" /*or when "job-no2" */ then do:
            run windows/l-jobno.w (loadtag.company, focus:screen-value,output char-val, OUTPUT rec-val).
            if char-val <> "" THEN
               assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                      */ 
                      loadtag.job-no:screen-value = entry(1,char-val)
                      loadtag.job-no2:screen-value = entry(2,char-val)
                      loadtag.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                      .
            IF rec-val <> ? THEN DO:
               FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
               IF AVAIL job-hdr THEN 
                  ASSIGN loadtag.loc:SCREEN-VALUE = job-hdr.loc
                         loadtag.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                               job-hdr.std-lab-cost +
                                               job-hdr.std-fix-cost +
                                               job-hdr.std-var-cost)
                         .
            end.
            FIND FIRST itemfg WHERE itemfg.company = g_company
                          AND itemfg.i-no = loadtag.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
            IF AVAIL ITEMfg THEN
                ASSIGN loadtag.i-name:SCREEN-VALUE = itemfg.i-name
                       loadtag.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                       loadtag.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

            return no-apply.   
      end.  
      when "job-no2" then do:
            run windows/l-jobno2.w (loadtag.company, loadtag.job-no:screen-value,focus:screen-value,output char-val, OUTPUT rec-val).
            if char-val <> "" THEN
               assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                      loadtag.job-no:screen-value = entry(1,char-val) */
                      loadtag.job-no2:screen-value = entry(2,char-val)
                      loadtag.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                      .
            IF rec-val <> ? THEN DO:
               FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
               IF AVAIL job-hdr THEN 
                  ASSIGN loadtag.loc:SCREEN-VALUE = job-hdr.loc
                         loadtag.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                               job-hdr.std-lab-cost +
                                               job-hdr.std-fix-cost +
                                               job-hdr.std-var-cost)
                  .
            end.
            FIND itemfg WHERE itemfg.company = g_company
                          AND itemfg.i-no = loadtag.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
            IF AVAIL ITEMfg THEN
                ASSIGN loadtag.i-name:SCREEN-VALUE = itemfg.i-name
                       loadtag.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                       loadtag.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
            return no-apply.   
      end.  
*/      
      when "loc" then do:
            run windows/l-loc.w (g_company,focus:screen-value, output char-val).
            if char-val <> "" then do :
               assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                      .

            end.
            return no-apply.   
      end.
      when "loc-bin" then do:
            run windows/l-fgbin.w (g_company, tt-tag.loc:SCREEN-VALUE IN BROWSE {&browse-name}, tt-tag.loc-bin:screen-value,output char-val).
            if char-val <> "" then do :
               assign focus:screen-value  = entry(1,char-val)
                      /*loadtag.loc:screen-value = entry(2,char-val)
                       loadtag.tag:screen-value = entry(4,char-val)*/
                      .

            end.
            return no-apply.   
      end.
      WHEN "tag-no" THEN DO:
          run windows/l-ldtagc.w (g_company,no,focus:screen-value,output char-val,OUTPUT rec-val).
          if char-val <> "" then do :
             FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
             /*  ===*/
             FIND FIRST bf-tmp WHERE bf-tmp.tag-no = SELF:SCREEN-VALUE
                       NO-LOCK NO-ERROR.
             IF AVAIL bf-tmp THEN DO:
               /* MESSAGE "This Tag Number Has Already Been Used." skip
                        "Please Enter A Unique Tag Number." 
                        VIEW-AS ALERT-BOX ERROR. */
                RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number.","",1,"OK", OUTPUT v-msgreturn).
                RETURN NO-APPLY.
             END.
          /*   ELSE DO:
                find first xfg-rdtlh where xfg-rdtlh.company   eq g_company
                     and xfg-rdtlh.loc       eq loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                     and xfg-rdtlh.tag       eq loadtag.tag:SCREEN-VALUE
                     and xfg-rdtlh.qty       gt 0
                     and xfg-rdtlh.rita-code ne "S"
                     use-index tag no-lock no-error.
                if avail xfg-rdtlh THEN  DO:
                   MESSAGE "This Tag Number Has Already Been Used." skip
                           "Please Enter A Unique Tag Number." 
                           VIEW-AS ALERT-BOX ERROR.
                   RETURN NO-APPLY.
                END.
             END.
             */
             {addon/loadtags/disptag.i FOCUS:SCREEN-VALUE}
lv-prev-job2 = tt-tag.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
lv-job-no = tt-tag.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
lv-job-no2 = tt-tag.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
          /*   IF int(loadtag.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) = 0  THEN APPLY "entry" TO loadtag.t-qty.
              ELSE*/
              APPLY "entry" TO tt-tag.loc.

             RETURN NO-APPLY.
          END.
      END.
  end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
       {brsleave.i} 
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


ON "leave" OF tt-tag.tag-no IN BROWSE {&browse-name} DO:
   IF LASTKEY = -1 /*OR fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""  */
      OR LASTKEY = 27
      OR  BROWSE {&browse-name}:NUM-SELECTED-ROWS <= 0  THEN RETURN.

   RUN valid-tag# NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
/*
   FIND FIRST bf-tag WHERE bf-tag.tag-no = tt-tag.tag-no:SCREEN-VALUE IN BROWSE {&browse-name} 
                       AND RECID(bf-tag) <> RECID(tt-tag)   NO-LOCK NO-ERROR.
   IF AVAIL bf-tag THEN DO:
        /*
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
       */
       
       RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number.","",1,"OK", OUTPUT v-msgreturn).
       RETURN NO-APPLY.
    END.
*/
   
    FIND CURRENT tt-tag.
    ASSIGN BROWSE {&browse-name} tt-tag.tag-no tt-tag.i-no tt-tag.job-no
           tt-tag.job-no2 tt-tag.loc tt-tag.loc-bin
           tt-tag.case-bundle tt-tag.pallet-count tt-tag.qty-case tt-tag.tot-cases 
           tt-tag.qty tt-tag.partial.
    FIND CURRENT tt-tag NO-LOCK.
    RETURN.
    
END.

/* ***************************  Main Block  *************************** */

FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company 
                      AND sys-ctrl.NAME = "AUTOPOST"
                      NO-LOCK NO-ERROR.
lv-fgfile = AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFile".

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'r-no':U THEN DO:
       &Scope KEY-PHRASE fg-rctd.r-no eq INTEGER(key-value)
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* r-no */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF v-FIRST THEN DO:
     FOR EACH tt-tag:
         DELETE tt-tag.
     END.
     CREATE tt-tag.
  END.

  v-first = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE can-exit B-table-Win 
PROCEDURE can-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-can-exit AS LOG NO-UNDO.

   /*
   IF AVAIL loadtag AND loadtag.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
      RUN dispatch ('delete-record').
   END.
   */
   op-can-exit = /*IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN YES ELSE NO.*/
                 YES.
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR v-cost LIKE loadtag.std-cost NO-UNDO.


  DO TRANSACTION:
    {sys/inc/autopost.i}
    lv-fgfile = sys-ctrl.char-fld = "FGFile".
  END.

  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no EQ tt-tag.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      no-lock no-error.
  /*loadtag.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.*/
    /*  
  IF adm-new-record THEN DO:
    find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.
    assign
     loadtag.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count)
     loadtag.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = if itemfg.pur-man then itemfg.pur-uom
                                                                                else itemfg.prod-uom
     loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
     loadtag.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = "".
       
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.loc     eq itemfg.def-loc
          and fg-bin.loc-bin eq itemfg.def-loc-bin
          and fg-bin.i-no    eq ""
        no-lock no-error.
    if avail fg-bin then 
      assign
       loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = itemfg.def-loc
       loadtag.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.def-loc-bin
       loadtag.std-cost:SCREEN-VALUE = IF loadtag.po-no:SCREEN-VALUE = "" and
                                                  loadtag.job-no:SCREEN-VALUE = "" 
                                               THEN string(itemfg.last-cost) 
                                               ELSE loadtag.std-cost:SCREEN-VALUE
       loadtag.qty-case:SCREEN-VALUE = /*IF loadtag.po-no:SCREEN-VALUE = "" and
                                                  loadtag.job-no:SCREEN-VALUE = "" 
                                                THEN   STRING(itemfg.case-count)
                                                ELSE loadtag.qty-case:SCREEN-VALUE
                                                */
                                                STRING(itemfg.case-count)
       loadtag.cost-uom:SCREEN-VALUE = itemfg.prod-uom.

    RELEASE fg-bin.

    if autopost eq "ShipTo" then do:
      find first cust
          where cust.company eq cocode
            and cust.cust-no eq itemfg.cust-no
          no-lock no-error.
      if avail cust then do:              
        find first shipto
            where shipto.company eq cocode
              and shipto.cust-no eq cust.cust-no 
            no-lock no-error.
        if avail shipto then do:
          find first fg-bin
              where fg-bin.company eq cocode
                and fg-bin.loc     eq shipto.loc
                and fg-bin.loc-bin eq shipto.loc-bin
                and fg-bin.i-no    eq ""
              no-lock no-error.
          if avail fg-bin then 
            assign
            loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
            loadtag.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
        end.
      end. /*if avail cust*/                                
    end. /*if system default is shipto*/
   
    ELSE
    IF lv-fgfile THEN DO:
      find first fg-bin where fg-bin.company eq cocode
                and fg-bin.i-no    eq loadtag.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                and fg-bin.job-no  eq loadtag.job-no:SCREEN-VALUE
                and ((loadtag.job-no:SCREEN-VALUE ne "" and
                    fg-bin.job-no2 eq int(loadtag.job-no2:SCREEN-VALUE) ) or
                    (loadtag.job-no:SCREEN-VALUE eq ""))
                /*and fg-bin.qty     le 0*/
                no-lock no-error.
     if avail fg-bin then 
        ASSIGN loadtag.loc:SCREEN-VALUE     = fg-bin.loc
               loadtag.loc-bin:SCREEN-VALUE = fg-bin.loc-bin.
    END.

    /*if bin and warehouse are blank, goto cust "X" shipto file*/
    if loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}     eq "" OR 
      loadtag.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} eq "" then do:
      find first cust
          where cust.company eq cocode
            and cust.active  eq "X"
          no-lock no-error.
                                
      if avail cust then do:
        find first shipto
            where shipto.company eq cocode
              and shipto.cust-no eq cust.cust-no  
            no-lock no-error.
           
        if avail shipto then do:
          find first fg-bin
              where fg-bin.company eq cocode
                and fg-bin.loc     eq shipto.loc
                and fg-bin.loc-bin eq shipto.loc-bin
                and fg-bin.i-no    eq ""
              no-lock no-error.
          if avail fg-bin then
            assign
             loadtag.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
             loadtag.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
        end.                                  
      end.
    end.

    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    eq loadtag.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          and job-hdr.job-no  eq loadtag.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          and job-hdr.job-no2 eq int(loadtag.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK no-error.
       
    IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ loadtag.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ int(loadtag.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ loadtag.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    END.

    if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      loadtag.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = string(job-hdr.std-tot-cost).
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      loadtag.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(reftable.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(loadtag.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq loadtag.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, loadtag.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        loadtag.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost).
      END.
     
      else
      if avail itemfg then
        assign
         loadtag.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom
         loadtag.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.total-std-cost).
    end.
  END.
  
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix B-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def input parameter ip-first-disp as log no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR v-rec-qty AS INT NO-UNDO.


/*
  if not avail fg-rctd then return.  /* no records */
   
  cocode = fg-rctd.company.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" then do: /* for row-display */
  find itemfg  where itemfg.company eq cocode                           /* no screen-value used */
                     and itemfg.i-no  eq fg-rctd.i-no /*:screen-value in browse {&browse-name}*/
                     use-index i-no no-lock no-error.

  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name})
                       and po-ordl.i-no  = fg-rctd.i-no:screen-value
                       and po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       and po-ordl.item-type = no
                       no-lock no-error.

  if not avail po-ordl AND fg-rctd.po-no:SCREEN-VALUE <> "" then return.
  
  lv-out-qty = fg-rctd.t-qty . /* fg-rctd.qty-case. ??? */
  /* convert cost pr-uom*/
  run rm/convcuom.p(fg-rctd.cost-uom, IF AVAIL po-ordl THEN po-ordl.pr-qty-uom /*po-ordl.cons-uom*/  ELSE "EA",
                    0,0,0,0,fg-rctd.std-cost, output lv-out-cost).
end. /* avail fg-rctd */
/* ======================================================================= */
else
if avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE <> "" then do: /* in update mode - use screen-value */
  find itemfg  where itemfg.company eq cocode
                and itemfg.i-no  eq fg-rctd.i-no:screen-value in browse {&browse-name}
                      use-index i-no no-lock no-error.
  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) 
                       and po-ordl.i-no  = fg-rctd.i-no:screen-value
                       and po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       and po-ordl.item-type = no
                       no-lock no-error.
  
  
  
  IF AVAIL po-ordl THEN DO:
    v-rec-qty = po-ordl.t-rec-qty + int(fg-rctd.t-qty:SCREEN-VALUE).
    IF po-ordl.pr-qty-uom <> "EA" THEN
       run sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                                     v-rec-qty, output v-rec-qty).
    if v-rec-qty gt (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
       AND NOT lv-overrun-checked
    then do:
       message "The PO Qty + overrun has been exceeded. "
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
       /*APPLY "entry" TO fg-rctd.cases.
       RETURN ERROR.  */
    end.
  END.
  ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no:screen-value
                       and job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       no-lock no-error.
       IF AVAIL job-hdr THEN DO: 
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + (IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                               IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0 / 100))).
      
          END.
          IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX ERROR.
             /*RETURN ERROR.*/
          END.
          lv-overrun-checked = YES.
       END.
  END.
  ELSE if not avail po-ordl AND fg-rctd.po-no:SCREEN-VALUE <> "" then return. 

  lv-out-qty = decimal(fg-rctd.t-qty:screen-value in browse {&browse-name})  . 

  /* convert cost */
  if avail po-ordl then assign v-len = po-ordl.s-len
                               v-wid = po-ordl.s-wid.
  else assign v-len = 0
              v-wid = 0.

  run rm/convcuom.p( fg-rctd.cost-uom:screen-value in browse {&browse-name},
                     IF AVAIL po-ordl THEN /* po-ordl.cons-uom */ po-ordl.pr-qty-uom ELSE "EA" ,
                             0,v-len,v-wid,0,
                             fg-rctd.std-cost:screen-value in browse {&browse-name}, output lv-out-cost).
END.

DO WITH FRAME {&frame-name}:
  fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-qty * lv-out-cost).
END.

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-tmp-qty AS cha NO-UNDO.
  DEF VAR ls-tmp-uom AS cha NO-UNDO.
  DEF VAR ls-tmp-cst AS cha NO-UNDO.
  DEF BUFFER bf-ltag FOR loadtag.

  /* Code placed here will execute PRIOR to standard behavior. */
/*
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ls-tmp-qty = fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-cst = fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.
*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*
  ASSIGN
   fg-rctd.t-qty    = DEC(ls-tmp-qty)
   fg-rctd.pur-uom  = ls-tmp-uom
   fg-rctd.cost-uom = ls-tmp-uom
   fg-rctd.ext-cost = DEC(ls-tmp-cst).

  RUN fg/comprcpt.p (ROWID(fg-rctd)).  
  
*/

 

  FOR EACH bf-tag:
      FIND FIRST loadtag WHERE loadtag.company = g_company
                           AND loadtag.item-type = NO
                           AND loadtag.is-case-tag
                           AND loadtag.tag-no = bf-tag.tag-no NO-ERROR.
      IF AVAIL loadtag THEN DO:
         FIND FIRST shifts WHERE shifts.company = g_company AND
                         shifts.START_time >= TIME AND
                         shifts.END_time <= TIME NO-LOCK NO-ERROR.
         IF AVAIL shifts AND shifts.shift = loadtag.shift THEN
            ASSIGN loadtag.shift = IF AVAIL shifts THEN shifts.shift ELSE ""
                   loadtag.tag-date = TODAY
                   loadtag.tag-time = TIME
                   loadtag.tot-cases = loadtag.tot-cases + 1.
         ELSE IF AVAIL shifts THEN DO:
            FIND FIRST loadtag WHERE loadtag.company = g_company
                           AND loadtag.item-type = NO
                           AND loadtag.is-case-tag
                           AND loadtag.tag-no = bf-tag.tag-no + "-" + shifts.shift NO-ERROR.
            IF AVAIL loadtag THEN DO:
               ASSIGN loadtag.shift = IF AVAIL shifts THEN shifts.shift ELSE ""
                      loadtag.tag-date = TODAY
                      loadtag.tag-time = TIME
                      loadtag.tot-cases = loadtag.tot-cases + 1.
            END.
            ELSE DO:
               CREATE bf-ltag.
               BUFFER-COPY bf-tag TO bf-ltag.
               ASSIGN bf-ltag.company = g_company
                      bf-ltag.is-case-tag = YES
                      bf-ltag.item-type = NO
                      bf-ltag.tag-date = TODAY
                      bf-ltag.tag-time = TIME
                      bf-ltag.tot-cases = 1
                      bf-ltag.tag-no = bf-tag.tag-no + "-" + shifts.shift .
            END.
         END.
         ELSE DO:
               ASSIGN loadtag.shift = IF AVAIL shifts THEN shifts.shift ELSE ""
                      loadtag.tag-date = TODAY
                      loadtag.tag-time = TIME
                      loadtag.tot-cases = loadtag.tot-cases + 1.
         END.
      END.  /* avail loadtag */
      IF bf-tag.tag-no = "" THEN DELETE bf-tag.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */

   ASSIGN BROWSE {&browse-name} tt-tag.tag-no tt-tag.i-no tt-tag.job-no
            tt-tag.job-no2 tt-tag.loc tt-tag.loc-bin
            tt-tag.case-bundle tt-tag.pallet-count tt-tag.qty-case tt-tag.tot-cases 
            tt-tag.qty tt-tag.partial.

   
   DISPLAY tt-tag.tag-no tt-tag.i-no tt-tag.job-no
           tt-tag.job-no2 tt-tag.loc tt-tag.loc-bin
           tt-tag.case-bundle tt-tag.pallet-count tt-tag.qty-case tt-tag.tot-cases 
           tt-tag.qty tt-tag.partial WITH BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL tt-tag AND tt-tag.tag-no = "" THEN DO:      
     DELETE tt-tag.
  END.

 lv-overrun-checked = NO.
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
  DEF BUFFER b-fg-rctd FOR fg-rctd.


   /* Code placed here will execute PRIOR to standard behavior. */
   IF NOT adm-new-record THEN DO:
     {custom/askdelss.i}
   END.

   FOR EACH fg-rcpts
       WHERE fg-rcpts.company EQ cocode
         AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"):

     FOR EACH b-fg-rctd
         WHERE b-fg-rctd.company EQ cocode
           AND b-fg-rctd.r-no    EQ fg-rcpts.r-no:
       DELETE b-fg-rctd.
     END.
   END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    apply "entry" to tt-tag.tag-no in browse {&browse-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  RUN valid-tag# NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  /*
  RUN valid-po-no (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF lv-prev-job2 <> tt-tag.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} AND
     NOT lv-new-job-ran THEN RUN NEW-job-no.


  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN error.
*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .


  /* Code placed here will execute AFTER standard behavior.    */
  lv-overrun-checked = NO.
/*
  RUN repo-query (ROWID(loadtag)).
*/
  ASSIGN lv-new-job-ran = NO
         lv-prev-job2 = "".


  RUN scan-next.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no B-table-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DO WITH FRAME {&FRAME-NAME}:
    lv-closed-checked = NO.
    lv-new-job-ran = YES.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN DO:
        ASSIGN
         fg-rctd.job-no:SCREEN-VALUE   = job-hdr.job-no
         fg-rctd.job-no2:SCREEN-VALUE  = STRING(job-hdr.job-no2)
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE
         fg-rctd.i-no:SCREEN-VALUE     = job-hdr.i-no
         fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost).

        RUN get-def-values.
      END.
    END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-record B-table-Win 
PROCEDURE post-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* DEF VAR v-done AS INT NO-UNDO.
  RUN custom/d-msg.w ("Warning","","Are you ready to post FG Receipts?","",2,"YES,NO", OUTPUT v-done).
  IF v-done >= 2 THEN RETURN.
  
  RUN addon/fg/fgpstall.w (?,"R"). 
  
  RUN dispatch ('open-query').
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  RUN dispatch ('open-query').

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ('row-changed').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scan-next B-table-Win 
PROCEDURE scan-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN auto-add IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "r-no" "fg-rctd" "r-no"}
  {src/adm/template/sndkycas.i "company" "fg-rctd" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "tt-tag"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*
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
            and xfg-rctd.tag     begins string(int(fg-rctd.po-no:screen-value in browse {&browse-name}),"999999")
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
    find first fg-rctdh"where fg-rctdh.company eq rm-rcth.company
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

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no B-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*
 DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      IF fgrecpt                                                AND
         fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
         fg-rctd.rita-code NE "E"                                  THEN DO:
    /*    MESSAGE "You must enter a Job or a PO..." VIEW-AS ALERT-BOX ERROR. */
        RUN custom/d-msg.w ("Error","","You must enter a Job or a PO...","",1,"OK", OUTPUT v-msgreturn).         
        APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    ELSE DO:
      IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "".
        /*MESSAGE "You may only enter a Job or a PO, Job No will be erased..."
            VIEW-AS ALERT-BOX ERROR. */
        RUN custom/d-msg.w ("Error","","You may only enter a Job or a PO. Job# will be erased...","",1,"OK", OUTPUT v-msgreturn).         
        fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        RETURN.
      END.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
        /*MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.*/
        RUN custom/d-msg.w ("Error","","Invalid Job#.  Try Help...","",1,"OK", OUTPUT v-msgreturn).         
        APPLY "entry" TO fg-rctd.job-no.
        RETURN ERROR.
      END.
    END.
  END.
 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 B-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*
 DEF VAR lv-ans AS LOG NO-UNDO.
  DEF VAR lv-err AS LOG INIT NO NO-UNDO.

  DO WITH FRAME {&frame-name}:
    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
          NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN
      FIND FIRST job
          WHERE job.company EQ job-hdr.company
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
          NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr OR NOT AVAIL job THEN DO:
        MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
        lv-err = YES.        
      END.

      IF NOT lv-err AND NOT lv-closed-checked AND
         INDEX("CZ",job.stat) GT 0            THEN DO:
        ASSIGN
         lv-ans            = NO
         lv-closed-checked = YES.
        MESSAGE "Job is CLOSED, would you like to reopen?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
        CASE lv-ans:
           WHEN YES THEN RUN jc/jc-reopn.p (ROWID(job)).
           WHEN NO  THEN.
           OTHERWISE lv-err = YES.
         END CASE.
      END.
    END.

    IF lv-err THEN DO:
      lv-closed-checked = NO.
      APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no B-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEF INPUT PARAM ip-type AS INT NO-UNDO.

  DO WITH FRAME {&frame-name}:
    IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
       /* MESSAGE "You may only enter a Job or a PO, PO will be erased..." VIEW-AS ALERT-BOX ERROR.*/
        RUN custom/d-msg.w ("Error","","You may only enter a Job or a PO. PO# will be erased...","",1,"OK", OUTPUT v-msgreturn).         
        fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        RETURN.
      END.

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ fg-rctd.company
            AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.ITEM-type EQ NO
            AND (po-ordl.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                 fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        IF ip-type NE 0 THEN DO:
        /*  MESSAGE "Invalid PO#, try help..." VIEW-AS ALERT-BOX ERROR. */
            RUN custom/d-msg.w ("Error","","Invalid PO#.  Try Help...","",1,"OK", OUTPUT v-msgreturn).         
            APPLY "entry" TO fg-rctd.po-no IN BROWSE {&browse-name}.
        END.
        RETURN ERROR.
      END.
    END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-record B-table-Win 
PROCEDURE valid-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*
 DEF VAR li-max-qty AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

 
  FIND itemfg WHERE itemfg.company = fg-rctd.company
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no .
            RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND first itemfg {sys/look/itemfgrlW.i}
                       and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       no-lock no-error.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.

  IF itemfg.isaset                                                        AND
     (itemfg.alloc EQ NO                OR
      (itemfg.alloc EQ YES       AND
       fgrecpt-char EQ "AUTOPOST" AND
       TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "")) THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING((INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                 INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),"->>>,>>>,>>9.99")
     li-max-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
            
    RUN fg/checkset.w (ROWID(itemfg),
                       ROWID(fg-rctd),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       INPUT-OUTPUT li-max-qty).

    IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      ll = NO.
      MESSAGE "Create receipt with maximum quantity available?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

      IF ll THEN  
        ASSIGN
         fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(li-max-qty)
         fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}  = 
              STRING(TRUNC((li-max-qty - DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})) /
                           DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0))
         fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = 
              STRING(li-max-qty - (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                                   DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).

      IF NOT ll OR li-max-qty EQ 0 THEN DO:
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.
  
  FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc-bin.
          RETURN ERROR.
  END.
  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> ""
  THEN DO:
  
    FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                            bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.tag.
       RETURN ERROR.
    END.
    ELSE DO:
        find first xfg-rdtlh
               where xfg-rdtlh.company   eq g_company
                 and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                 and xfg-rdtlh.qty       gt 0
                 and xfg-rdtlh.rita-code ne "S"
               use-index tag no-lock no-error.
           if avail xfg-rdtlh THEN  DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO fg-rctd.tag.
               RETURN error.
           END.
    END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag# B-table-Win 
PROCEDURE valid-tag# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 FIND FIRST loadtag WHERE loadtag.company = g_company
      AND loadtag.item-type = NO
      AND loadtag.is-case-tag
      AND loadtag.tag-no = tt-tag.tag-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
 IF NOT AVAIL loadtag OR  tt-tag.tag-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
   
    /*MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.*/
     RUN custom/d-msg.w ("Error","","","Invalid Tag#. Try help or Scan valid tag#...",1,"OK", OUTPUT v-out).
     

    APPLY "entry" TO tt-tag.tag-no IN BROWSE {&browse-name}.
    RETURN ERROR.
 END.
 ELSE DO:
      
       ASSIGN tt-tag.i-no:SCREEN-VALUE = loadtag.i-no
              tt-tag.job-no:SCREEN-VALUE = loadtag.job-no
              tt-tag.job-no2:SCREEN-VALUE = STRING(loadtag.job-no2)
              tt-tag.loc:SCREEN-VALUE = loadtag.loc
              tt-tag.loc-bin:SCREEN-VALUE = loadtag.loc-bin
              tt-tag.case-bundle:SCREEN-VALUE = string(loadtag.case-bundle) 
              tt-tag.pallet-count:SCREEN-VALUE = string(loadtag.pallet-count) 
              tt-tag.qty-case:SCREEN-VALUE = string(loadtag.qty-case) 
              tt-tag.tot-cases:SCREEN-VALUE = string(loadtag.tot-cases) 
              tt-tag.qty:SCREEN-VALUE = string(loadtag.qty)
              tt-tag.partial:SCREEN-VALUE = string(loadtag.partial)
             .
                  
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*
 DEF VAR li-max-qty AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

 
  FIND itemfg WHERE itemfg.company = fg-rctd.company
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
    /*    MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX. */
        RUN custom/d-msg.w ("Error","","Invalid Item#.  Try Help...","",1,"OK", OUTPUT v-msgreturn).
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        /*MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO: */
        RUN custom/d-msg.w ("Error","","F/G  Item is not on file.  Would you like to add it? ","",2,"Yes,No", OUTPUT v-msgreturn).
        IF v-msgreturn >= 2 THEN DO:
           APPLY "entry" TO fg-rctd.i-no .
           RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND first itemfg {sys/look/itemfgrlW.i}
                       and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       no-lock no-error.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.

  IF itemfg.isaset                                                        AND
     (itemfg.alloc EQ NO                OR
      (itemfg.alloc EQ YES       AND
       fgrecpt-char EQ "AUTOPOST" AND
       TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "")) THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING((INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                 INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),"->>>,>>>,>>9.99")
     li-max-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
            
    RUN fg/checkset.w (ROWID(itemfg),
                       ROWID(fg-rctd),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       INPUT-OUTPUT li-max-qty).

    IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      ll = NO.
      /*MESSAGE "Create receipt with maximum quantity available?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. */
      RUN custom/d-msg.w ("Question","","Create receipt with maximum quantity available? ","",2,"Yes,No", OUTPUT v-msgreturn).
      ll = v-msgreturn = 1.

      IF ll THEN  
        ASSIGN
         fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(li-max-qty)
         fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}  = 
              STRING(TRUNC((li-max-qty - DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})) /
                           DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0))
         fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = 
              STRING(li-max-qty - (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                                   DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).

      IF NOT ll OR li-max-qty EQ 0 THEN DO:
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.
  
  FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
      /*    MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR. */
          RUN custom/d-msg.w ("Error","","Invalid Warehouse. Try Help...","",1,"OK", OUTPUT v-msgreturn).
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          /*MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.*/
          RUN custom/d-msg.w ("Error","","Invalid Bin#.  Try Help...","",1,"OK", OUTPUT v-msgreturn).
          APPLY "entry" TO fg-rctd.loc-bin.
          RETURN ERROR.
  END.
  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> ""
  THEN DO:
  
    FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                            bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       /*MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number."  view-as alert-box error*/
       RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number...","",1,"OK", OUTPUT v-msgreturn).         
       APPLY "entry" TO fg-rctd.tag.
       RETURN ERROR.
    END.
    ELSE DO:
        find first xfg-rdtlh
               where xfg-rdtlh.company   eq g_company
                 and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                 and xfg-rdtlh.qty       gt 0
                 and xfg-rdtlh.rita-code ne "S"
               use-index tag no-lock no-error.
           if avail xfg-rdtlh THEN  DO:
       /*        MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR. */
               RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number...","",1,"OK", OUTPUT v-msgreturn).         
               APPLY "entry" TO fg-rctd.tag.
               RETURN error.
           END.
    END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

