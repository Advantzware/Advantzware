&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-r-no LIKE fg-rcpth.r-no NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rctd reftable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.i-no fg-rctd.i-name ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.t-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK, ~
      EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND ~
reftable.company  EQ fg-rctd.company AND ~
reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK, ~
      EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND ~
reftable.company  EQ fg-rctd.company AND ~
reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd reftable
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table reftable


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd, 
      reftable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rctd.i-no COLUMN-LABEL "Item" FORMAT "X(15)":U
      fg-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
      fg-rctd.cases-unit COLUMN-LABEL "Unit!per Pallet" FORMAT ">>9":U
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
      fg-rctd.t-qty COLUMN-LABEL "Total!Qty" FORMAT "->>>,>>>,>>9.99":U
  ENABLE
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.tag
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.cases-unit
      fg-rctd.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.fg-rctd,ASI.reftable WHERE ASI.fg-rctd ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", OUTER"
     _Where[1]         = "fg-rctd.r-no ge lv-frst-rno and
(fg-rctd.rita-code eq ""R"" or fg-rctd.rita-code eq ""E"")"
     _JoinCode[2]      = "reftable.reftable EQ ""fg-rctd.user-id"" AND
reftable.company  EQ fg-rctd.company AND
reftable.loc      EQ STRING(fg-rctd.r-no,""9999999999"")"
     _FldNameList[1]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" "Item" "X(15)" "character" ? ? ? ? ? ? no "FG Item Number" no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Name/Desc" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Unit!per Pallet" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
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
   def var ll-tag# as log no-undo.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     ll-help-run = yes.
     case focus:name:
        when "po-no" then do:
             run windows/l-pofg.w (fg-rctd.company,fg-rctd.po-no:screen-value in browse {&browse-name}, output char-val).
             if char-val <> "" then do:
                assign fg-rctd.po-no:screen-value in browse {&browse-name} = entry(1,char-val)
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
                  assign /*-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                         fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom
                         fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cost)
                         .
               end.

               find first itemfg where itemfg.company = fg-rctd.company and
                                        itemfg.i-no = entry(2,char-val)
                                        no-lock no-error.
               if avail itemfg then do:                         
                  assign fg-rctd.loc:screen-value in browse {&browse-name} =  itemfg.def-loc
                         fg-rctd.loc-bin:screen-value in browse {&browse-name} =  itemfg.def-loc-bin
                         fg-rctd.qty-case:screen-value in browse {&browse-name} = string(itemfg.case-count)
                       /*  fg-rctd.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                                            else itemfg.prod-uom  */                        
                         .
               end.
  /*
               run tag-method (output ll-tag#).
               if ll-tag# and fg-rctd.po-no:screen-value in browse {&browse-name} <> ""
               then do:
                   run tag-sequence.
               end.
    */
               fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
             end.  /* char-val <> "" */
             return no-apply.   
       end.
       when "i-no" then do:
             IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
                RUN windows/l-poitem.w (fg-rctd.company,fg-rctd.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
                if char-val <> "" then do :
                   assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.i-name:screen-value = entry(2,char-val)
                       fg-rctd.job-no:screen-value = entry(3,char-val)
                       fg-rctd.job-no2:screen-value = entry(4,char-val)
                       .
                end.
             END.
             ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
                  RUN windows/l-jobit1.w (fg-rctd.company,fg-rctd.job-no:SCREEN-VALUE,fg-rctd.job-no2:screen-value, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
                  IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                  IF rec-val <> ? THEN DO:
                     FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.
                     IF AVAIL tt-job-hdr THEN 
                         ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(tt-job-hdr.std-mat-cost +
                                                              tt-job-hdr.std-lab-cost +
                                                              tt-job-hdr.std-fix-cost +
                                                              tt-job-hdr.std-var-cost)
                               .

                  END.

             END.
             ELSE DO:
                  RUN windows/l-itemf2.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                  IF rec-val <> ? THEN DO:
                     FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                     ASSIGN fg-rctd.i-no:SCREEN-VALUE  = itemfg.i-no
                       fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                       fg-rctd.std-cost:SCREEN-VALUE = string(itemfg.avg-cost)
                       fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

                  END.
             END.
             return no-apply.   
       end.
       when "job-no" /*or when "job-no2" */ then do:
             run windows/l-jobno.w (fg-rctd.company, focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       */ 
                       fg-rctd.job-no:screen-value = entry(1,char-val)
                       fg-rctd.job-no2:screen-value = entry(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)
                          .
             end.
             FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

             return no-apply.   
       end.  
       when "job-no2" then do:
             run windows/l-jobno2.w (fg-rctd.company, fg-rctd.job-no:screen-value,focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       fg-rctd.job-no:screen-value = entry(1,char-val) */
                       fg-rctd.job-no2:screen-value = entry(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)
                   .
             end.
             FIND itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
             return no-apply.   
       end.  
       when "loc" then do:
             run windows/l-loc.w (fg-rctd.company,focus:screen-value, output char-val).
             if char-val <> "" then do :
                assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                       .

             end.
             return no-apply.   
       end.
       when "loc-bin" then do:
             run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc:screen-value, fg-rctd.loc-bin:screen-value,output char-val).
             if char-val <> "" then do :
                assign focus:screen-value  = entry(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             end.
             return no-apply.   
       end.
     end case.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

    ASSIGN
     lv-prev-job2       = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
     ll-qty-case-ent    = NO
     lv-overrun-checked = NO 
     lv-closed-checked  = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
    {est/brsleave.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startSearch.
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


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY = -1 THEN RETURN.

  IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
     fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE ""      THEN DO:
    RUN valid-po-no (0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "FG does not exist on PO..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.

  find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                 no-lock no-error.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
        RETURN NO-apply.
     END.
     ELSE DO:
       MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF NOT ll-ans THEN DO:
           APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
           RETURN NO-APPLY.           
       END.
       ELSE DO:
           RUN fg/d-crtitm.w (SELF:SCREEN-VALUE) .
           find first itemfg {sys/look/itemfgrlW.i}
                     and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     no-lock no-error.
           IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                                        .
       END.
     END.
  END.

  /*IF SELF:MODIFIED THEN*/ RUN get-def-values.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF SELF:MODIFIED THEN DO:
       FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
    IF LASTKEY = -1 THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                           AND fg-bin.i-no = ""
                           AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                           USE-INDEX co-ino NO-LOCK NO-ERROR.
       IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  RUN new-tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  ll-qty-case-ent = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
cocode = g_company.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR ls-tmp-qty AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ls-tmp-qty = fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty    = DEC(ls-tmp-qty)
   fg-rctd.ext-cost = fg-rctd.std-cost * fg-rctd.t-qty /
                      (IF fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-fg-rctd FOR fg-rctd.

  DEF VAR ll AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reset-cursor.

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
  IF validRUN get-r-no.

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
  RUN valid-tag (fg-rctd.tag:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty B-table-Win 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
               INT(fg-rctd.qty-case:SCREEN-VALUE) +
               INT(fg-rctd.partial:SCREEN-VALUE)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tag B-table-Win 
PROCEDURE new-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST loadtag NO-LOCK
        WHERE loadtag.company   EQ cocode
          AND loadtag.item-type EQ NO
          AND loadtag.tag-no    EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-ERROR.
    IF AVAIL loadtag THEN
      fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = loadtag.i-no. 
  END.

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


  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-cursor B-table-Win 
PROCEDURE reset-cursor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-right' TO {&BROWSE-NAME}.
    END.
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

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
  {src/adm/template/snd-list.i "fg-rctd"}
  {src/adm/template/snd-list.i "reftable"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-bin-info B-table-Win 
PROCEDURE show-bin-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN get-def-values.
    ASSIGN
     li = TRUNC(INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0)
     fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(li)
     fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) -
               (li * DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      IF lv-msg EQ ""                                           AND
         (CAN-FIND(FIRST b-fg-rctd
                   WHERE b-fg-rctd.company EQ cocode
                     AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE
                     AND RECID(b-fg-rctd)  NE RECID(fg-rctd)) OR
          CAN-FIND(FIRST b-fg-rdtlh
                   WHERE b-fg-rdtlh.company   EQ cocode
                     AND b-fg-rdtlh.tag       EQ ip-focus:SCREEN-VALUE
                     AND b-fg-rdtlh.qty       GT 0
                     AND b-fg-rdtlh.rita-code NE "S"))          THEN
        lv-msg = "Tag# has already been used, please re-enter".

      IF lv-msg EQ ""                                                   AND
         fgrecpt-int EQ 1                                               AND
         NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company   EQ cocode
                        AND loadtag.item-type EQ NO
                        AND loadtag.tag-no    EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = "Invalid Tag#, try help or scan valid tag#".

      IF lv-msg NE "" THEN DO:
        MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
    END.
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

  IF INT(fg-rctd.cases-unit:SCREEN-VALUE) < 1 THEN DO:  /* task# 06200520*/
    MESSAGE "Unit/Pallet must be greater than or equal to 1." VIEW-AS ALERT-BOX.
    APPLY "entry" TO fg-rctd.cases-unit .
    RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

