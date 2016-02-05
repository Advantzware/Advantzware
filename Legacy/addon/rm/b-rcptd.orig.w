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
{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i NEW SHARED } 

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
DEF VAR lv-rowid AS ROWID NO-UNDO.
def var ll-qty-valid as LOG no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR lv-po-len LIKE po-ordl.s-len FORM ">>,>>9.9999" NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR ll-tag-meth AS LOG NO-UNDO.

DEF SHARED VAR g-sharpshooter AS LOG NO-UNDO.


DEF BUFFER bf-tmp FOR rm-rctd.  /* for tag validation */
DEF BUFFER xrm-rdtlh FOR rm-rdtlh. /* for tag validation */

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
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.tag rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.rct-date rm-rctd.po-no rm-rctd.s-num rm-rctd.i-no ~
rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty rm-rctd.pur-uom ~
rm-rctd.cost rm-rctd.cost-uom calc-ext-cost() @ ext-cost ~
display-dimension('W') @ lv-po-wid display-dimension('L') @ lv-po-len 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.job-no rm-rctd.job-no2 rm-rctd.rct-date rm-rctd.po-no rm-rctd.s-num ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty ~
rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char )  FORWARD.

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
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.rct-date FORMAT "99/99/9999":U
      rm-rctd.po-no FORMAT "x(6)":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->,>>>,>>9.9<<":U
            WIDTH 20
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
  ENABLE
      rm-rctd.tag
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.rct-date
      rm-rctd.po-no
      rm-rctd.s-num
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.qty
      rm-rctd.pur-uom
      rm-rctd.cost
      rm-rctd.cost-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
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
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
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
         HEIGHT             = 18.43
         WIDTH              = 154.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

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
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = gcompany and
rm-rctd.rita-code = ""R"""
     _FldNameList[1]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > asi.rm-rctd.job-no
"rm-rctd.job-no" "Job" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > asi.rm-rctd.job-no2
"rm-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > asi.rm-rctd.po-no
"rm-rctd.po-no" ? "x(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > asi.rm-rctd.s-num
"rm-rctd.s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > asi.rm-rctd.i-no
"rm-rctd.i-no" "Item" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > asi.rm-rctd.i-name
"rm-rctd.i-name" "Name/Desc" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" "->,>>>,>>9.9<<" "decimal" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" ""
     _FldNameList[12]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[13]   > asi.rm-rctd.cost
"rm-rctd.cost" "Cost" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > asi.rm-rctd.cost-uom
"rm-rctd.cost-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[15]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[17]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
ON CURSOR-DOWN OF Browser-Table IN FRAME F-Main
DO:  
  RUN get-matrix(YES).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
 DEF VAR help-recid AS RECID NO-UNDO.

 ll-help-run = yes.

 case focus:name :
      when "po-no" then do:
           run windows/l-poordl.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                     rm-rctd.i-no:screen-value in browse {&browse-name} = entry(2,char-val)
                     rm-rctd.i-name:screen-value in browse {&browse-name} = entry(3,char-val)
                     rm-rctd.job-no:screen-value in browse {&browse-name} = entry(4,char-val)
                     rm-rctd.job-no2:screen-value in browse {&browse-name} = entry(5,char-val)
                     .
             find po-ordl where po-ordl.company = rm-rctd.company and
                                po-ordl.po-no = integer(entry(1,char-val)) and
                                po-ordl.line = integer(entry(6,char-val))
                                no-lock no-error.
             if avail po-ordl then RUN update-from-po-line.

             else do:
                find first item where item.company = rm-rctd.company and
                                      item.i-no = entry(2,char-val)
                                      no-lock no-error.
                assign rm-rctd.pur-uom:screen-value in browse {&browse-name} = item.cons-uom
                       rm-rctd.cost-uom:screen-value in browse {&browse-name} = item.cons-uom                       
/*                       r
m-rctd.s-num:screen-value in browse {&browse-name} = string(item.s-num)*/
                       .                      
             end.
             if not avail item then find first item where item.company = rm-rctd.company and
                                                          item.i-no = entry(2,char-val)
                                      no-lock no-error.
             
             assign rm-rctd.loc:screen-value in browse {&browse-name} =  item.loc
                    rm-rctd.loc-bin:screen-value in browse {&browse-name} =  item.loc-bin
                    .
             if rm-rctd.loc-bin:screen-value in browse {&browse-name} eq "" then do:
                  find first sys-ctrl where sys-ctrl.company eq rm-rctd.company
                                          and sys-ctrl.name    eq "AUTOISSU"
                                no-lock no-error.
                  if not avail sys-ctrl then do:
                        create sys-ctrl.
                        assign sys-ctrl.company = rm-rctd.company
                                   sys-ctrl.name    = "AUTOISSU"
                                   sys-ctrl.descrip = "Automatically Issue RM Receipts to asi"
                                   sys-ctrl.log-fld = yes.
                        message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                           update sys-ctrl.char-fld.
                  end.
                  assign rm-rctd.loc-bin:screen-value in browse {&browse-name} = sys-ctrl.char-fld.
             end.
             run tag-method (output ll-tag#).
             if ll-tag# and rm-rctd.po-no:screen-value in browse {&browse-name} <> ""
             then do:
                 run tag-sequence.
             end.
             ext-cost = 0.
             disp ext-cost with browse {&browse-name}.
           end.  /* char-val <> "" */
           return no-apply.   
     end.
     when "i-no" then do:
         IF rm-rctd.po-no:SCREEN-VALUE = "" THEN DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
            run windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val,OUTPUT help-recid).
            if char-val <> "" then do :
               ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                      rm-rctd.i-name:SCREEN-VALUE = ENTRY(2,char-val).
               RUN display-item(help-recid).
            END.
         END.
         ELSE DO:
           run windows/l-poitem.w (rm-rctd.company,rm-rctd.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                     rm-rctd.i-name:screen-value = entry(2,char-val)
                     rm-rctd.job-no:screen-value = entry(3,char-val)
                     rm-rctd.job-no2:screen-value = entry(4,char-val)
                     .
              FIND FIRST po-ordl WHERE ROWID(po-ordl) EQ TO-ROWID(ENTRY(5,char-val)) NO-LOCK NO-ERROR.
              IF AVAIL po-ordl THEN RUN update-from-po-line.
           end.
         END.
         return no-apply.   
     end.
     when "s-num" then do:
         IF rm-rctd.po-no:SCREEN-VALUE NE "" THEN DO:                
           RUN windows/l-poitem.w (rm-rctd.company,
                                   rm-rctd.po-no:SCREEN-VALUE ,
                                   "",
                                   output char-val).
           IF char-val NE "" then DO:
              assign rm-rctd.i-no:screen-value = entry(1,char-val)
                     rm-rctd.i-name:screen-value = entry(2,char-val)
                     rm-rctd.job-no:screen-value = entry(3,char-val)
                     rm-rctd.job-no2:screen-value = entry(4,char-val)
                     .
           
              FIND FIRST po-ordl WHERE ROWID(po-ordl) EQ TO-ROWID(ENTRY(5,char-val)) NO-LOCK NO-ERROR.
              IF AVAIL po-ordl THEN RUN update-from-po-line.
           end.
         END.
         return no-apply.   
     end.
     when "job-no" or when "job-no2" then do:
           run windows/l-pojob.w (rm-rctd.company,rm-rctd.po-no:screen-value,rm-rctd.i-no:screen-value, output char-val).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val)
                     .
             
           end.
           return no-apply.   
     end.  
     when "loc" then do:
           run rm/l-loc.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                     /*rm-rctd.loc-bin:screen-value in browse {&browse-name} = entry(2,char-val) */
                     .
             
           end.
           return no-apply.   
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val)
                     rm-rctd.loc:screen-value = entry(2,char-val)
                     rm-rctd.qty:screen-value = entry(3,char-val)
                     /*rm-rctd.tag:screen-value = entry(4,char-val)*/
                     .
             
           end.
           return no-apply.   
     end.
     WHEN "tag" THEN DO:
           run windows/l-ldtag.w (g_company,yes,focus:screen-value,output char-val,OUTPUT HELP-recid).
           if char-val <> "" then do :
              FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
              /*  ===*/
              FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                            bf-tmp.tag = SELF:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(rm-rctd)
                        NO-LOCK NO-ERROR.
              IF AVAIL bf-tmp THEN DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
              END.
              ELSE DO:
                  
                 find first xrm-rdtlh where xrm-rdtlh.company   eq g_company
                      and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                      and xrm-rdtlh.qty       gt 0
                      and xrm-rdtlh.rita-code ne "S" 
                      use-index tag no-lock no-error.
                 if avail xrm-rdtlh THEN  DO:
                    MESSAGE "This Tag Number Has Already Been Used." skip
                            "Please Enter A Unique Tag Number." 
                            VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                 END.
                 
              END.
              {addon/loadtags/disptagr.i "RMItem" FOCUS:SCREEN-VALUE}
              RETURN NO-APPLY.
           END.
       END.


   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
anywhere
DO:
       apply "tab" to self.
       return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  ASSIGN
   ll-help-run  = NO
   ll-qty-valid = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
 /*  {src/adm/template/brsleave.i}  */
   
    if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:  
      return no-apply.
   end.

   {est/brsleave.i}  /* same as src but update will be same as add record*/

   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. 
  
   
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


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON END-ERROR OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
   /*
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
   RUN do-cancel IN WIDGET-HANDLE(char-hdl).
   RUN check-modified ('clear').

   RETURN NO-APPLY.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY = -1 OR LASTKEY = 27 THEN RETURN.
  MESSAGE 'here' LASTKEY VIEW-AS ALERT-BOX.
  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  {addon/loadtags/disptagr.i "RMItem" SELF:SCREEN-VALUE}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  /*IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
    APPLY "leave" TO {&self-name}.
    RETURN NO-APPLY.
  END.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
    APPLY "leave" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
       
      RUN display-po-job.
      IF lv-rowid = ? THEN  RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
    APPLY "leave" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-s-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  FIND ITEM
      WHERE item.company EQ rm-rctd.company
        AND item.i-no    BEGINS rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.             
  IF AVAIL ITEM THEN RUN display-item (RECID(ITEM)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY = -1 THEN RETURN.
  DO WITH FRAME {&frame-name}:
    IF {&self-name}:MODIFIED IN BROWSE {&browse-name} AND NOT ll-qty-valid THEN DO:
      {rm/chkporun.i}
      ll-qty-valid = YES.
    END.
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN get-matrix (FALSE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (FALSE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
  run get-matrix (false).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (FALSE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

{sys/inc/tag#.i}
ll-tag-meth = v-tag#.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.


FIND ITEM WHERE RECID(ITEM) EQ ip-recid NO-LOCK NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  IF AVAIL ITEM THEN DO:
    ASSIGN
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = ITEM.i-name
     rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = ITEM.loc
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.loc-bin.

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      {rm/avgcost.i}

      ASSIGN
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.cons-uom 
       rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}    =
           IF INT(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
             IF v-avgcost THEN STRING(ITEM.avg-cost)
             ELSE STRING(ITEM.last-cost)
           ELSE rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.cons-uom. 
    END.
  END.

  IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ "" OR
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.active  EQ "X" 
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
        ASSIGN   
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-info B-table-Win 
PROCEDURE display-po-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
  DEF VAR lv-i-no LIKE rm-rctd.i-no NO-UNDO.
  DEF VAR lv-locode like locode.
  DEF VAR li-tag-seq as int.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

  
  FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-i-no   = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}

     rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}     = po-ordl.i-no
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = po-ordl.i-name
     rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = po-ordl.job-no
     rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(po-ordl.job-no2)    
     rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.s-num)
     lv-cost                                                = po-ordl.cons-cost.

    IF po-ordl.cons-uom NE "" THEN
      ASSIGN
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.cons-uom 
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.cons-uom.

    IF po-ordl.pr-uom EQ "L" THEN lv-cost = po-ordl.t-cost / po-ordl.cons-qty.

    ASSIGN
     /*rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(po-ordl.ord-qty)*/
     rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(lv-cost).
                    
    IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-i-no THEN DO:
      FIND FIRST ITEM
          WHERE item.company EQ rm-rctd.company
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.             
      IF AVAIL ITEM THEN RUN display-item (RECID(ITEM)).
    END.

    IF ll-tag-meth AND adm-new-record THEN DO:
      ASSIGN
       li-tag-seq = 0
       lv-locode  = "".

      DO WHILE TRUE:
        FIND FIRST b-rm-rctd
            WHERE b-rm-rctd.company eq cocode
              AND b-rm-rctd.loc     gt lv-locode
            NO-LOCK NO-ERROR.

        IF AVAIL b-rm-rctd THEN DO:
          lv-locode = b-rm-rctd.loc.

          FOR EACH b-rm-rctd
              WHERE b-rm-rctd.company EQ cocode
                AND b-rm-rctd.loc     EQ lv-locode
                AND b-rm-rctd.tag     BEGINS STRING(po-ordl.po-no,"999999")
              USE-INDEX tag NO-LOCK
              BY b-rm-rctd.tag DESC:

            IF INT(SUBSTR(b-rm-rctd.tag,7,2)) GT li-tag-seq THEN
              li-tag-seq = INT(SUBSTR(b-rm-rctd.tag,7,2)).
            LEAVE.
          END.
        END.

        ELSE LEAVE.
      END.

      lv-locode = "".

      IF li-tag-seq EQ 0 THEN DO WHILE TRUE:
        FIND FIRST rm-rdtlh
            WHERE rm-rdtlh.company EQ cocode
              AND rm-rdtlh.loc     GT lv-locode
            NO-LOCK NO-ERROR.

        IF AVAIL rm-rdtlh THEN DO:
          lv-locode = rm-rdtlh.loc.

          FOR EACH rm-rdtlh
              WHERE rm-rdtlh.company EQ cocode
                AND rm-rdtlh.loc     EQ lv-locode
                AND rm-rdtlh.tag     BEGINS STRING(po-ordl.po-no,"999999")
              USE-INDEX tag NO-LOCK
              BY rm-rdtlh.tag DESC:

            IF INT(SUBSTR(rm-rdtlh.tag,7,2)) GT li-tag-seq THEN
              li-tag-seq = INT(SUBSTR(rm-rdtlh.tag,7,2)).
            LEAVE.
          END.
        END.

        ELSE LEAVE.
      END.

      ASSIGN
       li-tag-seq   = li-tag-seq + 1
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} =
           STRING(po-ordl.po-no,"999999") + STRING(li-tag-seq,"99").
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-job B-table-Win 
PROCEDURE display-po-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li-po-cnt AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    lv-rowid = ?.
    IF adm-adding-record THEN DO:
       FOR EACH  po-ordl WHERE po-ordl.company   EQ rm-rctd.company
                         AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        /* AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})       
                        AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} */
                         AND po-ordl.item-type EQ YES NO-LOCK.
          li-po-cnt = li-po-cnt + 1.
          IF li-po-cnt >= 2 THEN LEAVE.
       END.
       IF li-po-cnt >= 2 THEN RUN rm/d-selpo.w (rm-rctd.company,
                                             INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             "",
                                             0,
                                             "",
                                             OUTPUT lv-rowid
                                             ).
    END.
    ELSE DO:
        FOR EACH  po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})       
          AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} 
          AND po-ordl.item-type EQ YES NO-LOCK.
          li-po-cnt = li-po-cnt + 1.
          IF li-po-cnt >= 2 THEN LEAVE.
       END.
       IF li-po-cnt >= 2 THEN RUN rm/d-selpo.w (rm-rctd.company,
                                             INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                             OUTPUT lv-rowid
                                             ).
    END.
   
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-exact-po B-table-Win 
PROCEDURE find-exact-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.item-type EQ YES
        NO-LOCK NO-ERROR.
    lv-rowid = IF AVAIL po-ordl THEN ROWID(po-ordl) ELSE ?.
  END.

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
  def var lv-out-qty LIKE rm-rctd.qty no-undo.
  def var lv-out-cost LIKE rm-rctd.cost no-undo.
  DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.

  
  

if ip-first-disp  and avail rm-rctd and rm-rctd.i-no <> "" then do: /* for row-display */
  find item  where item.company eq cocode                           /* no screen-value used */
                     and item.i-no  eq rm-rctd.i-no /*:screen-value in browse {&browse-name}*/
                     use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.      
  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rctd.i-no
                       and po-ordl.job-no = rm-rctd.job-no
                       and po-ordl.job-no2 = rm-rctd.job-no2
                       and po-ordl.item-type = yes 
                       and po-ordl.s-num = rm-rctd.s-num
                           no-lock no-error.
  /*if not avail po-ordl then return.  */

  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0
             lv-qty-uom = po-ordl.cons-uom
             lv-cost-uom = po-ordl.cons-uom.
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
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
  end.
  
  /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
 /* run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).
  
  /* convert cost pr-uom*/
  run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                    v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
  */
     /*SYS/REF/ */
  run custom/convquom.p(gcompany,rm-rctd.pur-uom,
                    lv-qty-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).

    /* convert cost pr-uom*/
        /*sys/ref/convcuom.p*/
  IF rm-rctd.cost-uom EQ "L" THEN
    lv-out-cost = rm-rctd.cost / lv-out-qty.
  ELSE
    run custom/convcuom.p(gcompany,rm-rctd.cost-uom, lv-cost-uom,                    
                          v-bwt, v-len, v-wid, v-dep,
                          rm-rctd.cost, output lv-out-cost).

   ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
  
  /*disp ext-cost with browse {&browse-name}. it's displayed automatically */
 /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
  */

end. /* ip-first */
/* ======================================================================= */
else if avail rm-rctd and rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" then do: /* in update mode - use screen-value */
  find item  where item.company eq cocode
                and item.i-no  eq rm-rctd.i-no:screen-value in browse {&browse-name}
                      use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.    
  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no:screen-value in browse {&browse-name})
                       and po-ordl.i-no  = rm-rctd.i-no:screen-value
                       and po-ordl.job-no = (rm-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(rm-rctd.job-no2:screen-value)
                       and po-ordl.item-type = yes
                       and po-ordl.s-num = integer(rm-rctd.s-num:screen-value)
                           no-lock no-error.
 /*
  if not avail po-ordl then return.
 */   
  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0
             lv-qty-uom = po-ordl.cons-uom
             lv-cost-uom = po-ordl.cons-uom.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:screen-value
                         and job.job-no2 eq integer(rm-rctd.job-no2:screen-value)
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no:screen-value
                                  and job-mat.frm     eq integer(rm-rctd.s-num:screen-value)
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          .
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = item.cons-uom
               lv-cost-uom = ITEM.cons-uom .
  end.
  
  /* convert qty */
  run custom/convquom.p(gcompany,
                        rm-rctd.pur-uom:screen-value in browse {&browse-name} ,
                        lv-qty-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty:screen-value in browse {&browse-name},
                         output lv-out-qty).
  
  /* convert cost */
  IF rm-rctd.cost-uom:screen-value in browse {&browse-name} EQ "L" THEN
    lv-out-cost = DEC(rm-rctd.cost:screen-value in browse {&browse-name}) / lv-out-qty.
  ELSE
    run custom/convcuom.p(gcompany,
                          rm-rctd.cost-uom:screen-value in browse {&browse-name},
                          lv-cost-uom,
                          v-bwt, v-len, v-wid, v-dep,
                          rm-rctd.cost:screen-value in browse {&browse-name}, output lv-out-cost).

  ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
  ASSIGN rm-rctd.cost:SCREEN-VALUE = STRING(lv-out-cost)
         rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
         rm-rctd.qty:SCREEN-VALUE = STRING(lv-out-qty)
         rm-rctd.pur-uom:SCREEN-VALUE = lv-qty-uom.
  disp ext-cost with browse {&browse-name}.

end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li-nxt-r-no AS INT INIT 1 NO-UNDO.
 DEF BUFFER bf-rctd FOR rm-rctd.


  /* Code placed here will execute PRIOR to standard behavior. */
 FOR EACH bf-rctd NO-LOCK BY bf-rctd.r-no DESCENDING:
     li-nxt-r-no = bf-rctd.r-no.
     LEAVE.
 END.
 FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
 IF AVAIL rm-rcpth AND rm-rcpth.r-no GT li-nxt-r-no THEN li-nxt-r-no = rm-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rm-rctd.company = gcompany
         rm-rctd.loc = gloc
         rm-rctd.r-no    = li-nxt-r-no + 1
         rm-rctd.rita-code = "R"
         rm-rctd.s-num  = 0
         rm-rctd.s-num:screen-value in browse {&browse-name} = "0" 
         rm-rctd.rct-date = today
         .
  find first sys-ctrl where sys-ctrl.company eq gcompany
                        and sys-ctrl.name    eq "AUTOISSU"
                        no-lock no-error.
  if not avail sys-ctrl then do:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "AUTOISSU"
                 sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs"
                 sys-ctrl.log-fld = yes.
      message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                 update sys-ctrl.char-fld.
  end.
  rm-rctd.loc-bin = sys-ctrl.char-fld.
  disp rm-rctd.rct-date rm-rctd.loc-bin with browse {&browse-name}.  
  
/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and rm-rctd:po-no:screen*/
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
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

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
  def var li as int no-undo.
  def var hd-next as widget-handle no-undo.


  RETURN.
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
  /*
  DO WITH FRAME {&FRAME-NAME}:
    {&BROWSE-NAME}:READ-ONLY = NO.

    APPLY "entry" TO rm-rctd.rct-date IN BROWSE {&browse-name}.

    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  DO li = 1 TO 2:
    RUN valid-uom (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

 /* IF g-sharpshooter THEN */ RUN scan-next.

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
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO RECID ip-recid NO-ERROR.
    RUN dispatch ("row-changed").
  END.
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
  {src/adm/template/snd-list.i "rm-rctd"}

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
  def buffer xrm-rctd for rm-rctd.
  
  assign v-tag-seq = 0
         v-locode  = "".

  do while true:
    find first xrm-rctd
        where xrm-rctd.company eq rm-rctd.company
          and xrm-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xrm-rctd then do:
      v-locode = xrm-rctd.loc.

      for each xrm-rctd where xrm-rctd.company eq rm-rctd.company
            and xrm-rctd.loc     eq v-locode
            and xrm-rctd.tag     begins string(int(rm-rctd.po-no:screen-value in browse {&browse-name}),"999999")
            use-index tag no-lock
            by xrm-rctd.tag desc:

           if int(substr(xrm-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xrm-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first rm-rctdh where rm-rctdh.company eq rm-rcth.company
          and rm-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail rm-rctdh then do:
      v-locode = rm-rctdh.loc.

      for each rm-rctdh
          where rm-rctdh.company eq cocode
            and rm-rctdh.loc     eq v-locode
            and rm-rctdh.tag     begins string(int(rm-rctd.po-no),"999999")
          use-index tag no-lock
          by rm-rctdh.tag desc:

        if int(substr(rm-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(rm-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign  v-tag-seq   = v-tag-seq + 1
         /* rm-rctd.tag:screen-value in browse {&browse-name}
          = string(int(rm-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
           */
        .        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-from-po-line B-table-Win 
PROCEDURE update-from-po-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.s-num)
     rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(po-ordl.ord-qty)
     rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(po-ordl.cons-cost)  /* po-ordl.cost*/
     rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.s-num)
     lv-po-wid:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(po-ordl.s-wid)
     lv-po-len:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(po-ordl.s-len).

    IF po-ordl.cons-uom NE "" THEN
      ASSIGN
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.cons-uom 
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.cons-uom.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE DO:
      FIND FIRST ITEM
          WHERE ITEM.company EQ gcompany
            AND ITEM.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND ITEM.i-code  EQ "R"
          NO-LOCK NO-ERROR.
      IF NOT AVAIL ITEM THEN ERROR-STATUS:ERROR = YES.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.


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
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
      ASSIGN
       rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = ""
       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}   = "".

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

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
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES. 
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no2 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

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
  
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.po-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty B-table-Win 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ERROR-STATUS:ERROR = NO.

    IF dec(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ERROR-STATUS:ERROR = YES.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Receipt qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num B-table-Win 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN ERROR-STATUS:ERROR = YES.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.s-num IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

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
  DEF BUFFER b-rm-rctd FOR rm-rctd.

  DO WITH FRAME {&FRAME-NAME}:
    /* Check for a unique entry of a tag number when one is entered */
    IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE  "" THEN DO:
      RELEASE b-rm-rctd.
      RELEASE rm-rcpth.
      
      FIND FIRST b-rm-rctd
          WHERE b-rm-rctd.company   EQ rm-rctd.company
            AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
            AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
            AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
          USE-INDEX tag NO-LOCK NO-ERROR.
      IF NOT AVAIL b-rm-rctd THEN
      FIND FIRST rm-rdtlh
          WHERE rm-rdtlh.company EQ rm-rctd.company
            AND rm-rdtlh.loc     EQ rm-rctd.loc
            AND rm-rdtlh.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          USE-INDEX tag no-lock NO-ERROR.
            
      IF AVAIL rm-rdtlh THEN
      FIND FIRST rm-rcpth WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no NO-LOCK NO-ERROR.
      IF AVAIL b-rm-rctd OR
         (AVAIL rm-rcpth AND (rm-rcpth.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                              DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0)) THEN DO:
        MESSAGE "This Tag Number Has Already Been Used.  Please Enter A Unique Tag Number"
                VIEW-AS ALERT-BOX.
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.

      /* Check for a unique entry, 1 entry per tag per bin */
      FIND FIRST b-rm-rctd
          WHERE b-rm-rctd.r-no    EQ rm-rctd.r-no
            AND b-rm-rctd.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
            AND b-rm-rctd.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
            AND b-rm-rctd.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
            AND ROWID(b-rm-rctd)  NE ROWID(rm-rctd)
          NO-LOCK NO-ERROR.
      IF AVAIL b-rm-rctd THEN DO:
        MESSAGE "Transaction has already been entered with same Tag & Bin".
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = IF ip-int EQ 1 THEN rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}
                            ELSE rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

    IF AVAIL item AND item.mat-type EQ "M" AND ip-int EQ 2 THEN
      lv-uom-list = lv-uom-list + ",L".

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO rm-rctd.pur-uom IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO rm-rctd.cost-uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

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
  
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR ld-dim AS DEC NO-UNDO.
  DEF VAR v-wid-num AS DEC NO-UNDO.
  DEF VAR v-len-num AS DEC NO-UNDO.
  DEF BUFFER b-jm FOR job-mat.

  IF AVAIL rm-rctd THEN DO:
    FIND FIRST ITEM
        WHERE ITEM.company EQ cocode
          AND ITEM.i-no    EQ rm-rctd.i-no
          AND ITEM.i-code  EQ "R"
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
      ASSIGN
       v-wid-num = IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid
       v-len-num = ITEM.s-len.

    IF rm-rctd.po-no <> "" THEN DO:
      find first po-ordl where po-ordl.company   eq cocode
                           and po-ordl.po-no     eq int(rm-rctd.po-no)
                           and po-ordl.i-no      eq rm-rctd.i-no
                           and po-ordl.job-no    eq rm-rctd.job-no
                           and po-ordl.job-no2   eq rm-rctd.job-no2
                           and po-ordl.item-type eq yes
                           and po-ordl.s-num     eq rm-rctd.s-num
          no-lock no-error.
      if avail po-ordl then
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
      else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
      end.
    END.
      
    IF ip-dim = "W" THEN ld-dim = v-wid-num.
    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
  END.
  
  RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

