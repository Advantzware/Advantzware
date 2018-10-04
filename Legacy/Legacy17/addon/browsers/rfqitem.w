&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: addon/browsers/rfqitem.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\addon\browsers\rfqitem.i
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{custom/gcompany.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def new shared temp-table formule field formule as dec extent 12.
def var lv-log as log no-undo.
def var char-val as cha no-undo.
def buffer bf-rfqitem for rfqitem.
def new shared buffer xritem  for rfqitem.  /* for k-len k-wid calc in u2estic.p */
def var li-seq as int no-undo.
def var lv-recid as recid no-undo.
def var lv-copy-record as log no-undo.
def var lv-old-rfqitem-id as recid no-undo.
def var lv-rfqitem-copied-from-est as log no-undo.
def var lv-copy-qty as int extent 10 no-undo.
def var lv-part-no-prev as cha no-undo.
def var lv-stock-no-prev as cha no-undo.
def var lv-copy-pr as dec extent 10 no-undo.
def var lv-copy-uom as cha extent 10 no-undo.
def var lv-copy-date as date extent 10 no-undo.
def var ll-is-corr-style as log no-undo.
def var k_frac as dec init 6.25 NO-UNDO.
def var ls-prev-val as cha no-undo.   /* for leave triggers like modified */

{cec/descalc.i "new"}

{sys/inc/f16to32.i}

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
&Scoped-define EXTERNAL-TABLES rfq
&Scoped-define FIRST-EXTERNAL-TABLE rfq


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfq.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfqitem

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rfqitem.seq rfqitem.qty[1] ~
rfqitem.stock-no rfqitem.i-name rfqitem.part-no rfqitem.style ~
rfqitem.procat rfqitem.i-col rfqitem.i-coat ~
to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len rfqitem.len ~
to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len ~
to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid rfqitem.wid ~
to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid ~
to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep rfqitem.dep ~
to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep rfqitem.board ~
rfqitem.cal rfqitem.qty[99] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table rfqitem.qty[1] ~
rfqitem.stock-no rfqitem.i-name rfqitem.part-no rfqitem.style ~
rfqitem.procat rfqitem.i-col rfqitem.i-coat rfqitem.len rfqitem.wid ~
rfqitem.dep rfqitem.board rfqitem.cal 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table rfqitem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table rfqitem
&Scoped-define QUERY-STRING-br_table FOR EACH rfqitem OF rfq WHERE ~{&KEY-PHRASE} ~
      AND rfqitem.seq < 999 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rfqitem OF rfq WHERE ~{&KEY-PHRASE} ~
      AND rfqitem.seq < 999 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rfqitem


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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD To-Corrware-Dim B-table-Win 
FUNCTION To-Corrware-Dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      rfqitem.seq COLUMN-LABEL "#" FORMAT ">9":U COLUMN-FONT 0
      rfqitem.qty[1] COLUMN-LABEL "Qty" FORMAT "->>,>>>,>>9":U
            COLUMN-FONT 0
      rfqitem.stock-no COLUMN-LABEL "F.G. Item#" FORMAT "x(15)":U
            COLUMN-FONT 0
      rfqitem.i-name FORMAT "x(30)":U COLUMN-FONT 0
      rfqitem.part-no FORMAT "x(15)":U COLUMN-FONT 0
      rfqitem.style COLUMN-LABEL "Style" FORMAT "x(6)":U COLUMN-FONT 0
      rfqitem.procat FORMAT "x(5)":U COLUMN-FONT 0
      rfqitem.i-col FORMAT ">9":U COLUMN-FONT 0
      rfqitem.i-coat FORMAT ">9":U COLUMN-FONT 0
      to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len
      rfqitem.len FORMAT ">9.99999":U COLUMN-FONT 0
      to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len
      to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid
      rfqitem.wid FORMAT ">9.99999":U
      to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid
      to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep
      rfqitem.dep FORMAT ">9.99999":U COLUMN-FONT 0
      to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep
      rfqitem.board FORMAT "x(10)":U COLUMN-FONT 0
      rfqitem.cal FORMAT "9.99999":U COLUMN-FONT 0
      rfqitem.qty[99] COLUMN-LABEL "Qty/Set" FORMAT "->,>>>,>>9":U
  ENABLE
      rfqitem.qty[1]
      rfqitem.stock-no
      rfqitem.i-name
      rfqitem.part-no
      rfqitem.style
      rfqitem.procat
      rfqitem.i-col
      rfqitem.i-coat
      rfqitem.len
      rfqitem.wid
      rfqitem.dep
      rfqitem.board
      rfqitem.cal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142 BY 13.81
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: rfq.rfq
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
         HEIGHT             = 18
         WIDTH              = 142.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

{Advantzware/WinKit/dataGridProc.i}

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
     _TblList          = "rfq.rfqitem OF rfq.rfq"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rfq.rfqitem.seq < 999"
     _FldNameList[1]   > rfq.rfqitem.seq
"rfqitem.seq" "#" ">9" "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > rfq.rfqitem.qty[1]
"rfqitem.qty[1]" "Qty" "->>,>>>,>>9" "integer" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > rfq.rfqitem.stock-no
"rfqitem.stock-no" "F.G. Item#" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > rfq.rfqitem.i-name
"rfqitem.i-name" ? "x(30)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > rfq.rfqitem.part-no
"rfqitem.part-no" ? ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > rfq.rfqitem.style
"rfqitem.style" "Style" "x(6)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > rfq.rfqitem.procat
"rfqitem.procat" ? ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > rfq.rfqitem.i-col
"rfqitem.i-col" ? ? "integer" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > rfq.rfqitem.i-coat
"rfqitem.i-coat" ? ? "integer" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > rfq.rfqitem.len
"rfqitem.len" ? ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.len) @ rfqitem.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > rfq.rfqitem.wid
"rfqitem.wid" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.wid) @ rfqitem.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[17]   > rfq.rfqitem.dep
"rfqitem.dep" ? ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[18]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style,rfqitem.dep) @ rfqitem.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   > rfq.rfqitem.board
"rfqitem.board" ? ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[20]   > rfq.rfqitem.cal
"rfqitem.cal" ? ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[21]   > rfq.rfqitem.qty[99]
"rfqitem.qty[99]" "Qty/Set" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).

    RUN new-state in phandle ('update-begin':U).
  /*  assign add-active = no. */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
     def var ls-cur-val as cha no-undo.
     def var lv-eb-recid as recid no-undo.

     case focus:name :
     when "qty" then do:

          /*lv-recid = recid(rfqitem). will get from other place*/
          lv-recid = if avail rfqitem then recid(rfqitem) else lv-recid .

          if lv-copy-record then do: 
             /*lv-recid = lv-old-rfqitem-id.*/  /* source record for copy */
             run windows/rfqqtyd2.w (lv-old-rfqitem-id, rfqitem.qty[1]:screen-value in browse {&browse-name}, output char-val).                  
             if char-val <> "?" 
                then assign rfqitem.qty[1]:screen-value = entry(1,char-val)
                            lv-copy-qty[2] = integer(entry(2,char-val))
                            lv-copy-qty[3] = integer(entry(3,char-val))
                            lv-copy-qty[4] = integer(entry(4,char-val))
                            lv-copy-qty[5] = integer(entry(5,char-val))
                            lv-copy-qty[6] = integer(entry(6,char-val))
                            lv-copy-qty[7] = integer(entry(7,char-val))
                            lv-copy-qty[8] = integer(entry(8,char-val))
                            lv-copy-qty[9] = integer(entry(9,char-val))
                            lv-copy-qty[10] = integer(entry(10,char-val))
                            lv-copy-pr[1] = decimal(entry(11,char-val))
                            lv-copy-pr[2] = decimal(entry(12,char-val))
                            lv-copy-pr[3] = decimal(entry(13,char-val))
                            lv-copy-pr[4] = decimal(entry(14,char-val))
                            lv-copy-pr[5] = decimal(entry(15,char-val))
                            lv-copy-pr[6] = decimal(entry(16,char-val))
                            lv-copy-pr[7] = decimal(entry(17,char-val))
                            lv-copy-pr[8] = decimal(entry(18,char-val))
                            lv-copy-pr[9] = decimal(entry(19,char-val))
                            lv-copy-pr[10] = decimal(entry(20,char-val))
                            lv-copy-uom[1] = entry(21,char-val)
                            lv-copy-uom[2] = entry(22,char-val)
                            lv-copy-uom[3] = entry(23,char-val)
                            lv-copy-uom[4] = entry(24,char-val)
                            lv-copy-uom[5] = entry(25,char-val)
                            lv-copy-uom[6] = entry(26,char-val)
                            lv-copy-uom[7] = entry(27,char-val)
                            lv-copy-uom[8] = entry(28,char-val)
                            lv-copy-uom[9] = entry(29,char-val)
                            lv-copy-uom[10] = entry(30,char-val)
                            lv-copy-date[1] = date(entry(31,char-val))
                            lv-copy-date[2] = date(entry(32,char-val))
                            lv-copy-date[3] = date(entry(33,char-val))
                            lv-copy-date[4] = date(entry(34,char-val))
                            lv-copy-date[5] = date(entry(35,char-val))
                            lv-copy-date[6] = date(entry(36,char-val))
                            lv-copy-date[7] = date(entry(37,char-val))
                            lv-copy-date[8] = date(entry(38,char-val))
                            lv-copy-date[9] = date(entry(39,char-val))
                            lv-copy-date[10] = date(entry(40,char-val))
                            .
             return no-apply.
          end.
          run windows/rfqqtyd.w (lv-recid, rfqitem.qty[1]:screen-value in browse {&browse-name}, output char-val).     
          if rfqitem.qty[1]:screen-value <> char-val and char-val <> "?" then
             assign rfqitem.qty[1]:screen-value = char-val.

     end.
     when "part-no" then do: 
           run rfq/l-ebrfqP.w (rfq.company, rfq.loc, focus:screen-value, output lv-eb-recid) .
           if lv-eb-recid = ?  then return no-apply.
           find eb where recid(eb) = lv-eb-recid no-lock no-error.
           find ef of eb no-lock no-error.
           run copy-from-est.
           run copy-from-est2.
           lv-part-no-prev = eb.part-no.
           return no-apply. 
      end.
      when "stock-no" then do:
           /*
           run rfq/l-ebrfq.w (rfq.company, rfq.loc,rfq.cust-no,focus:screen-value, output lv-eb-recid) .
           */
           run rfq/l-ebrfqs.w (rfq.company, rfq.loc,focus:screen-value, output lv-eb-recid) .
           if lv-eb-recid = ?  then return.
           find eb where recid(eb) = lv-eb-recid no-lock no-error.
           find ef of eb no-lock no-error.
           run copy-from-est.
           run copy-from-est2.       
           return no-apply.
      end.
      when "style" then do:
        /*   for each style no-lock:
               lv-log = sl-style-list:add-last(style.style) in frame {&frame-name}.
           end.

           assign sl-style-list:columns = browse br_table:columns + 43
                  sl-style-list:row = browse br_table:row + 1
                  sl-style-list:sensitive  = true
                  sl-style-list:visible  = true
                  sl-style-list:screen-value in frame {&frame-name} = rfqitem.style.
                  .     
         */
           ls-cur-val = focus:screen-value.
           run windows/l-style.w (rfq.company,ls-cur-val, output char-val).
           if char-val <> "" then do:
              rfqitem.style:screen-value in browse {&browse-name} =  entry(1,char-val).
              find style where style.company = rfq.company and
                               style.style = rfqitem.style:screen-value in browse {&browse-name}
                         no-lock no-error.            
              if avail style then 
                 assign rfqitem.board:screen-value in browse {&browse-name} = style.material[1].

           end.  
           return no-apply.
      end.
      when "procat" then do:
         /*  for each fgcat no-lock:
              lv-log = sl-cat-list:add-last(fgcat.procat) in frame {&frame-name}.
           end.

           assign sl-cat-list:columns = browse br_table:columns + 52
                  sl-cat-list:row = browse br_table:row + 1
                  sl-cat-list:sensitive  = true
                  sl-cat-list:visible  = true
                  sl-cat-list:screen-value in frame {&frame-name} = rfqitem.procat.
                  .
           */
           ls-cur-val = focus:screen-value.
           run windows/l-fgcat.w (rfq.company,ls-cur-val,output char-val).
           if char-val <> "" then
              rfqitem.procat:screen-value in browse {&browse-name} =  entry(1,char-val).

           return no-apply.

       end.
       when "Board" then do:
           def var lv-ind like style.industry no-undo.
           ls-cur-val = focus:screen-value.
           find style where style.company = rfq.company and
                            style.style = rfqitem.style:screen-value in browse {&browse-name}
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (rfq.company,lv-ind,ls-cur-val,output char-val).
           else run windows/l-board.w (rfq.company,lv-ind, ls-cur-val,output char-val).
           if char-val <> "" then 
              assign rfqitem.board:screen-value in browse {&browse-name} = entry(1,char-val)
                     rfqitem.cal:screen-value in browse {&browse-name} = entry(2,char-val)
                     .
           return no-apply.   
       end.

  end case.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON return OF br_table IN FRAME F-Main
anywhere
DO:
    apply "tab" to self.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
    find style where style.company = rfq.company and
                      style.style = rfqitem.style
                      no-lock no-error.

   if avail style and style.industry = "2" then   ll-is-corr-style = yes.
   else ll-is-corr-style = no.
   IF ll-is-corr-style then
   DO:
      IF v-cecscrn-char NE "Decimal" THEN
         assign rfqitem.len:format in browse {&browse-name} = ">>>>9.99"
                rfqitem.wid:format in browse {&browse-name} = ">>>>9.99"
                rfqitem.dep:format in browse {&browse-name} = ">>>>9.99".
      ELSE
         assign rfqitem.len:format in browse {&browse-name} = ">>>>9.999999"
                rfqitem.wid:format in browse {&browse-name} = ">>>>9.999999"
                rfqitem.dep:format in browse {&browse-name} = ">>>>9.999999".
   END.
   else assign rfqitem.len:format in browse {&browse-name} = ">9.99999"
               rfqitem.wid:format in browse {&browse-name} = ">9.99999"
               rfqitem.dep:format in browse {&browse-name} = ">9.99999".
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
    /*{src/adm/template/brsleave.i}*/
   if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:

      return no-apply.
   end.

    {rfq/brsleave.i}  /* same as src but update will be same as add record*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  find style where style.company = rfqitem.company and
                      style.style = rfqitem.style
                      no-lock no-error.

   if avail style and style.industry = "2" then      ll-is-corr-style = yes.
   else ll-is-corr-style = no.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.qty[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.qty[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.qty[1] IN BROWSE br_table /* Qty */
DO:
   IF LASTKEY <> -1 THEN DO:
      IF dec(rfqitem.qty[1]:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN DO:
         MESSAGE "Quantity must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.stock-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rfqitem.stock-no IN BROWSE br_table /* F.G. Item# */
DO:
   lv-stock-no-prev = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.stock-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.stock-no IN BROWSE br_table /* F.G. Item# */
DO:
  def var lv-eb-recid as recid no-undo.
        /* 1. get values from itemfg if itemfg.est-no <> ""
           2. else get estimate info window */
  if adm-new-record AND rfqitem.part-no:screen-value in browse {&browse-name} = "" 
     and self:screen-value <> ""
  then do:
  /*=====================
     def var ll-choice as log  no-undo.
   message "Copy From FG(Yes), RFQ(No) or Cancel ?" view-as alert-box question 
           button yes-no-cancel 
                update ll-choice .        
   case ll-choice:
             when true then do:  /* copy from FG */
                  find first itemfg where itemfg.company = rfq.company and
                                          itemfg.i-no = rfqitem.stock-no:screen-value in browse {&browse-name}
                                no-lock no-error.
              if avail itemfg then do :
                  assign rfqitem.len:screen-value in browse {&browse-name} = string(itemfg.l-score[50])
                         rfqitem.wid:screen-value in browse {&browse-name} = string(itemfg.w-score[50])
                         rfqitem.dep:screen-value in browse {&browse-name} = string(itemfg.d-score[50])
                         rfqitem.procat:screen-value in browse {&browse-name} = itemfg.procat
                         rfqitem.style:screen-value in browse {&browse-name} = itemfg.style
                         rfqitem.part-no:screen-value in browse {&browse-name} = itemfg.i-no                
                         rfqitem.i-name:screen-value in browse {&browse-name} = itemfg.i-name

                         .
                  find style where style.company = rfq.company and
                                   style.style = rfqitem.style:screen-value
                                   no-lock no-error.
                  if avail style then assign rfqitem.board:screen-value = style.material[1].                 
              end.

             end.
             when false then do: /* copy from RFQ */
                  run windows/l-rfq.q (rfq.company, focus:screen-value, output char-val).
                  if char-val <> "" then do:
                     focus:screen-value in frame {&frame-name} = entry(1,char-val).
                     find bf-rfqitem where bf-rfqitem.rfq-no = integer(entry(2,char-val)) and
                                    bf-rfqitem.seq = integer(entry(3,char-val))
                                    no-lock no-error.
                     /* copy info */                                  
                     if avail bf-rfqitem then do:
                        assign rfqitem.len:screen-value in browse {&browse-name} = string(bf-rfqitem.len)
                               rfqitem.wid:screen-value in browse {&browse-name} = string(bf-rfqitem.wid)
                               rfqitem.dep:screen-value in browse {&browse-name} = string(bf-rfqitem.dep)
                               rfqitem.procat:screen-value in browse {&browse-name} = bf-rfqitem.procat
                               rfqitem.style:screen-value in browse {&browse-name} = bf-rfqitem.style
                               rfqitem.part-no:screen-value in browse {&browse-name} = bf-rfqitem.part-no
                               rfqitem.i-name:screen-value in browse {&browse-name} = bf-rfqitem.i-name                               
                               .
                        find style where style.company = rfq.company and
                                         style.style = rfqitem.style:screen-value
                                   no-lock no-error.
                        if avail style then assign rfqitem.board:screen-value = style.material[1].
                     end. 
                  end.
                  return no-apply.
             end.
             when ? then do:  /* cancel */
                  return .
             end.
    end case.
    ===============================  */
    /* copy from estimate */
    find first itemfg where itemfg.company = rfq.company and
                            itemfg.i-no = rfqitem.stock-no:screen-value in browse {&browse-name}
                      no-lock no-error.
    if avail itemfg and itemfg.est-no <> "" then do:
       find est where est.company  = itemfg.company and
                      est.loc = itemfg.loc and
                      est.est-no = itemfg.est-no
                      no-lock no-error.
       if avail est then do:
          find first eb where eb.company = itemfg.company
                          AND eb.est-no = est.est-no and
                              eb.stock-no = itemfg.i-no
                              no-lock no-error.
          if avail eb then do:
             find first ef WHERE ef.company = eb.company and
                                 ef.est-no = eb.est-no and
                                 ef.form-no = eb.form-no no-lock no-error.
             assign rfqitem.len:screen-value in browse {&browse-name} = string(eb.len)
                               rfqitem.wid:screen-value in browse {&browse-name} = string(eb.wid)
                               rfqitem.dep:screen-value in browse {&browse-name} = string(eb.dep)
                               rfqitem.procat:screen-value in browse {&browse-name} = eb.procat
                               rfqitem.style:screen-value in browse {&browse-name} = eb.style
                               rfqitem.part-no:screen-value in browse {&browse-name} = eb.part-no
                               rfqitem.i-name:screen-value in browse {&browse-name} = eb.part-dscr1                               
                               rfqitem.cal:screen-value in browse {&browse-name} = if avail ef then string(ef.cal)  else ""
                               rfqitem.board:screen-value in browse {&browse-name} = if avail ef then ef.board else ""
                               rfqitem.i-col:screen-value in browse {&browse-name} = string(eb.i-col)                               
                               rfqitem.i-coat:screen-value in browse {&browse-name} = string(eb.i-coat)                               
                               .
             find style where style.company = rfqitem.company and
                              style.style = eb.style
                              no-lock no-error.
             IF adm-new-record THEN do:
             IF AVAIL style AND style.material[2] EQ ""  THEN do:
                  ASSIGN
                      rfqitem.i-col:screen-value in browse {&browse-name} = string(0) 
                      rfqitem.i-pass = 0    .
              END.
              ELSE rfqitem.i-col:screen-value in browse {&browse-name} = string(1) .

              IF AVAIL style AND style.material[6] EQ "" THEN do:
                  rfqitem.i-coat:screen-value in browse {&browse-name} = "0" .
              END.
              ELSE rfqitem.i-coat:screen-value in browse {&browse-name} = "1" .

             END.
             if avail style and style.industry = "2" then      ll-is-corr-style = yes.
             else ll-is-corr-style = no.
             IF ll-is-corr-style THEN do:
                 ASSIGN rfqitem.len:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.len))
                        rfqitem.wid:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.wid))
                        rfqitem.dep:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.dep))
                        .
             END.
             run  copy-from-est.    

           end.
       end.    
    end.  /* avail itemfg */
    else do:  /* get it from estimate */                             
      /*  when use windows/l-ebrfq.w help ==================
           run windows/l-ebrfq.w (rfq.company, rfq.loc, rfq.cust-no, self:screen-value, output char-val). 
           if char-val <> "" then do:
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
              find est where est.company  = rfq.company and
                             est.loc = rfq.loc and
                             est.est-no = (entry(2,char-val))
                      no-lock no-error.
              if avail est then do:

                 find first eb where eb.e-num = est.e-num 
                                 and eb.stock-no = focus:screen-value
                                 no-lock no-error.
                 if avail eb then do:
                    find ef of eb no-lock no-error.
                    assign rfqitem.len:screen-value in browse {&browse-name} = string(eb.len)
                               rfqitem.wid:screen-value in browse {&browse-name} = string(eb.wid)
                               rfqitem.dep:screen-value in browse {&browse-name} = string(eb.dep)
                               rfqitem.procat:screen-value in browse {&browse-name} = eb.procat
                               rfqitem.style:screen-value in browse {&browse-name} = eb.style
                               rfqitem.part-no:screen-value in browse {&browse-name} = eb.part-no
                               rfqitem.i-name:screen-value in browse {&browse-name} = eb.part-dscr1                               
                               rfqitem.cal:screen-value in browse {&browse-name} = if avail ef then string(ef.cal) else ""
                               rfqitem.board:screen-value in browse {&browse-name} = if avail ef then ef.board else ""
                               rfqitem.i-col:screen-value in browse {&browse-name} = string(eb.i-col)                               
                               rfqitem.i-coat:screen-value in browse {&browse-name} = string(eb.i-coat)
                               .
                     run  copy-from-est.                  
                 end.  /* avail eb */
             end.     /* avail est */
           end.       /* char-val <> "" */
           ==================*/
           run rfq/l-ebrfq.w (rfq.company, rfq.loc,rfq.cust-no,self:screen-value, output lv-eb-recid) .
           if lv-eb-recid = ?  then return no-apply.
           find eb where recid(eb) = lv-eb-recid no-lock no-error.
           find ef of eb no-lock no-error.
           run copy-from-est.
           run copy-from-est2.

    end.            /* else */  
  end. /* adm-new-record */ 

  else if not adm-new-record and lastkey <> -1 and rfqitem.stock-no:screen-value <> ""
  then do:  /* update existing records */

    find first itemfg where itemfg.company = rfq.company and
                            itemfg.i-no = rfqitem.stock-no:screen-value in browse {&browse-name}
                      no-lock no-error.

    if not avail itemfg then do:
       message "Invalid FG Item#. Try Help.".
       return no-apply.
    end.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-name br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.i-name IN BROWSE br_table /* Item Name */
DO:
   IF LASTKEY <> -1 THEN DO:
      IF self:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Item Name must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.part-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rfqitem.part-no IN BROWSE br_table /* Cust Part# */
DO:
      lv-part-no-prev = rfqitem.part-no:screen-value in browse {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.part-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.part-no IN BROWSE br_table /* Cust Part# */
DO :
   def var lv-eb-recid as recid no-undo.
   IF LASTKEY <> -1 AND
      self:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Customer Part# must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
   END.
      /* lv-part-no-prev ; assigned from entry and help */  
   if lv-part-no-prev <> self:screen-value in browse {&browse-name} and
      lastkey <> -1
   then do:


      /* find estimate rec first */
      find first eb where eb.company = rfq.company and
                           eb.loc = rfq.loc and
                           eb.part-no = self:screen-value
                           use-index part no-lock no-error.  
       if avail eb then do:
          find next eb where eb.company = rfq.company and
                           eb.loc = rfq.loc and
                           eb.part-no = self:screen-value
                           use-index part no-lock no-error.  
          if avail eb then do:  /* multi estimates */
             run rfq/l-ebrfqP.w (rfq.company, rfq.loc,self:screen-value, output lv-eb-recid) .
             if lv-eb-recid = ?  then return no-apply.
             find eb where recid(eb) = lv-eb-recid no-lock no-error.
             find ef of eb no-lock no-error.
             run copy-from-est.
             run copy-from-est2.
            /* return no-apply. */
          end.
          else do:  /* only one estimate exists for the part# */
             find first eb where eb.company = rfq.company and
                              eb.loc = rfq.loc and
                              eb.part-no = self:screen-value
                              use-index part no-lock no-error.  
             find first ef of eb no-lock no-error.
             run copy-from-est.
             run copy-from-est2.
             /*return no-apply.          */
          end.
       end.
       else do:  /* no estimate record exist */
          find first itemfg where itemfg.company = rfq.company and
                               itemfg.part-no = rfqitem.part-no:screen-value in browse {&browse-name}
                               no-lock no-error.
          find style where style.company = rfq.company and
                        style.style = itemfg.style
                        no-lock no-error.
          if avail itemfg then
             assign rfqitem.i-name:screen-value in browse {&browse-name} = itemfg.i-name
                 rfqitem.style:screen-value in browse {&browse-name} = itemfg.style
                 rfqitem.stock-no:screen-value in browse {&browse-name} = itemfg.i-no
                 rfqitem.procat:screen-value in browse {&browse-name} = itemfg.procat
                 rfqitem.board:screen-value in browse {&browse-name} = if avail style then style.material[1]
                                                                       else rfqitem.board
                 rfqitem.len:screen-value = string(itemfg.l-score[50])
                 rfqitem.wid:screen-value = string(itemfg.w-score[50])
                 rfqitem.dep:screen-value = string(itemfg.d-score[50])
                 .


        end.
    end.  /* modified */    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.style br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rfqitem.style IN BROWSE br_table /* Style */
DO :
   /* assign cb-part-list:hidden in frame {&frame-name} = true
           cb-part-list:sensitive in frame {&frame-name} = false
           cb-cat-list:hidden in frame {&frame-name} = true
           cb-cat-list:sensitive in frame {&frame-name} = false.

    for each style no-lock:
        lv-log = cb-style-list:add-last(style.style) in frame {&frame-name}.
    end.

    assign cb-style-list:columns = browse br_table:columns + 43
           cb-style-list:row = browse br_table:row + 1
           cb-style-list:sensitive  = true
           cb-style-list:visible  = true
           cb-style-list:screen-value in frame {&frame-name} = rfqitem.style.
           .

    apply "entry" to cb-style-list.  
    return no-apply.
    */     

    ls-prev-val = self:screen-value.
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.style br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.style IN BROWSE br_table /* Style */
DO:
   IF LASTKEY <> -1 THEN DO:
      IF self:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Style must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
      END.
      IF NOT CAN-FIND(style where style.company = rfq.company and
                      style.style = rfqitem.style:screen-value in browse {&browse-name}) THEN DO:
         MESSAGE "Invalid Style." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.

   END.

   self:screen-value = caps(self:screen-value).
   find style where style.company = rfq.company and
                    style.style = rfqitem.style:screen-value in browse {&browse-name}
                    no-lock no-error.   

   IF adm-new-record THEN DO:
          IF style.material[2] EQ ""  THEN do:
              ASSIGN
                  rfqitem.i-col:screen-value in browse {&browse-name} = "0" 
                  rfqitem.i-pass = 0     .
          END.
          ELSE rfqitem.i-col:screen-value in browse {&browse-name} = "1"  .

          IF style.material[6] EQ "" THEN do:
              rfqitem.i-coat:screen-value in browse {&browse-name} = "0" .
          END.
          ELSE rfqitem.i-coat:screen-value in browse {&browse-name} = "1" .
   END.

   if avail style and style.industry = "2" then   ll-is-corr-style = yes.
   else ll-is-corr-style = no.
   IF ll-is-corr-style then
   DO:
      IF v-cecscrn-char NE "Decimal" THEN
         assign rfqitem.len:format in browse {&browse-name} = ">>>>9.99"
                rfqitem.wid:format in browse {&browse-name} = ">>>>9.99"
                rfqitem.dep:format in browse {&browse-name} = ">>>>9.99".
      ELSE
         assign rfqitem.len:format in browse {&browse-name} = ">>>>9.999999"
                rfqitem.wid:format in browse {&browse-name} = ">>>>9.999999"
                rfqitem.dep:format in browse {&browse-name} = ">>>>9.999999".
   END.
   else assign rfqitem.len:format in browse {&browse-name} = ">9.99999"
               rfqitem.wid:format in browse {&browse-name} = ">9.99999"
               rfqitem.dep:format in browse {&browse-name} = ">9.99999".

   if lv-rfqitem-copied-from-est then return.  /* ??? */

/*   if rfqitem.style:modified in browse {&browse-name} then do: not work */
   if ls-prev-val <> self:screen-value then do:
      if avail style then 
        assign rfqitem.board:screen-value in browse {&browse-name} = style.material[1].
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.procat br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rfqitem.procat IN BROWSE br_table /* Category */
DO:
    /*  assign cb-style-list:hidden in frame {&frame-name} = true
           cb-style-list:sensitive in frame {&frame-name} = false
           cb-part-list:hidden in frame {&frame-name} = true
           cb-part-list:sensitive in frame {&frame-name} = false.

    for each fgcat no-lock:
        lv-log = cb-cat-list:add-last(fgcat.procat) in frame {&frame-name}.
    end.

    assign cb-cat-list:columns = browse br_table:columns + 52
           cb-cat-list:row = browse br_table:row + 1
           cb-cat-list:sensitive  = true
           cb-cat-list:visible  = true
           cb-cat-list:screen-value in frame {&frame-name} = rfqitem.procat.
           .

    apply "entry" to cb-cat-list.  
    return no-apply.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.procat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.procat IN BROWSE br_table /* Category */
DO:
   IF LASTKEY <> -1 THEN DO:
      IF self:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Product Category must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
      END.
      IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = rfq.company
                      AND fgcat.PROCat = rfqitem.procat:SCREEN-VALUE IN BROWSE {&browse-name} )
      THEN DO:
        MESSAGE "Invalid Product Category. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
      END.
   END.

    self:screen-value = caps(self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.procat br_table _BROWSE-COLUMN B-table-Win
ON PAGE-DOWN OF rfqitem.procat IN BROWSE br_table /* Category */
DO:
   find first fgcat where fgcat.company = rfq.company and
                          fgcat.procat > self:screen-value in browse {&browse-name}
                          no-lock no-error.
   if avail fgcat then self:screen-value in browse {&browse-name} = fgcat.procat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.len br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.len IN BROWSE br_table /* Length */
DO: 
   IF LASTKEY <> -1 AND DEC(SELF:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   if lastkey <> -1 and 
      ll-is-corr-style and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.wid br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.wid IN BROWSE br_table /* Width */
DO:
   IF LASTKEY <> -1 AND DEC(SELF:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   if lastkey <> -1 and 
      ll-is-corr-style and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.dep br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.dep IN BROWSE br_table /* Depth */
DO:
      if lastkey <> -1 and 
        ll-is-corr-style and 
        decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
      then do:
         message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
         return no-apply.
      end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.board br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rfqitem.board IN BROWSE br_table /* Board */
DO:
   IF LASTKEY <> -1 THEN DO:
      IF self:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Board must be entered." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-apply.
      END.
   END.

    find item where item.company = rfq.company
                and item.i-no = rfqitem.board:screen-value in browse {&browse-name}
                    no-lock no-error.
    if lastkey <> -1 and rfqitem.board:screen-value <> "" and
       (not avail item or
       (avail item and lookup(item.mat-type,"B,P,1,2,3,4") <= 0 )
       )
    then do:
         message "Invalid Board. Try Help."    view-as alert-box error.
         return no-apply.

    end.     
    if avail item then 
       assign rfqitem.cal:screen-value in browse {&browse-name} = string(item.cal)
              .               

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "rfq"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfq"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-rfqitem B-table-Win 
PROCEDURE assign-rfqitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-dim-fit as log no-undo.
  DEF VAR i AS INT NO-UNDO.

  if lv-rfqitem-copied-from-est then do:
     lv-rfqitem-copied-from-est = no.
     return.
  end. 

  find style where style.company = rfq.company and
                         style.style = rfqitem.style no-lock no-error.   
  if avail style and style.industry = "2" then  do:
           find item where item.company = rfq.company and
                           item.i-no = rfqitem.board
                           no-lock no-error.                  
           if avail item then assign rfqitem.test = item.reg-no
                                     rfqitem.flute = item.flute
                                     rfqitem.brd-dscr = item.i-name
                                     .
  end.   

  IF adm-new-record then do:
        if not avail style then find style where style.company = rfq.company and
                                     style.style = rfqitem.style no-lock no-error.   
        if avail style and style.industry = "2" then  do:
           find item where item.company = rfq.company and
                           item.i-no = rfqitem.board
                           no-lock no-error.                  
           if avail item then assign rfqitem.test = item.reg-no
                                     rfqitem.flute = item.flute
                                     rfqitem.k-len = style.dim-dkl
                                     rfqitem.k-wid = style.dim-dkw
                                     .
           /* = score allowance k-wid, k-len ==== */ 
           find xritem where recid(xritem) = recid(rfqitem).

           {rfq/u2estc.i rfqitem.gluelap 1}
           {rfq/u2estc.i rfqitem.k-wid 2}

           find first item where item.company = rfqitem.company and
                                 index("GTS",item.mat-type) > 0 and
                                 item.i-no eq rfqitem.adhesive
                  no-lock no-error.
           if avail item and rfqitem.adhesive ne "NO JOINT" then do:
                 if item.mat-type eq "G" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 3}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 4}
                    end.
                 end.
                 else if item.mat-type eq "S" then do:
                    if /*input tab-inout eq "I" */  rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 5}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 6}
                    end.
                 end.
                 else if item.mat-type eq "T" then do:
                    {rfq/u2estc.i rfqitem.k-len 7}
                 end.
           end.
           else do:
                 {rfq/u2estc.i rfqitem.k-len 7}
           end.
           if rfqitem.len eq rfqitem.wid then do:
                 {rfq/u2estc.i rfqitem.k-wid 2 dim-fit}
           end.
           else do:
                 {rfq/u2estc.i rfqitem.k-wid 2}
           end.   
           /* ======== end of k-wid, k-len value =======*/
        end.   /* avail style */  
        IF rfqitem.stock-no <> "" THEN DO:
           FIND FIRST itemfg NO-LOCK WHERE itemfg.company = rfq.company
                                       AND itemfg.i-no = rfqitem.stock-no NO-ERROR.
           IF AVAIL itemfg AND itemfg.est-no <> "" THEN do:
              FIND FIRST eb NO-LOCK WHERE eb.company = itemfg.company
                                      AND eb.est-no = itemfg.est-no
                                      AND eb.stock-no = rfqitem.stock-no NO-ERROR.
              IF AVAIL eb THEN DO:
                 ASSIGN rfqitem.i-coat = eb.i-coat
                        rfqitem.i-coldscr = eb.i-coldscr
                        rfqitem.i-col = eb.i-col
                        rfqitem.i-pass = eb.i-pass.
                 IF eb.est-type > 4 THEN DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps[i]
                           rfqitem.i-code[i] = eb.i-code[i]
                           rfqitem.i-dscr[i] = eb.i-dscr[i]
                           rfqitem.i-%[i] = eb.i-%[i].
                 END.
                 else DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps2[i]
                           rfqitem.i-code[i] = eb.i-code2[i]
                           rfqitem.i-dscr[i] = eb.i-dscr2[i]
                           rfqitem.i-%[i] = eb.i-%2[i].
                 END.
                 FIND FIRST ef NO-LOCK WHERE ef.company = eb.company
                                         AND ef.est-no = eb.est-no
                                         AND ef.form-no = eb.form-no NO-ERROR.
                 IF AVAIL ef THEN DO i = 1 TO 4:
                    ASSIGN rfqitem.leaf[i] = ef.leaf[i]
                           rfqitem.leaf-dscr[i] = ef.leaf-dscr[i].
                 END.
              END.

              IF AVAIL style AND style.material[2] EQ ""  THEN do:
                  ASSIGN
                      rfqitem.i-col:screen-value in browse {&browse-name} = "0" 
                      rfqitem.i-pass = 0     .
              END.
              ELSE rfqitem.i-col:screen-value in browse {&browse-name} = "1" .
              IF AVAIL style AND style.material[6] EQ "" THEN do:
                  rfqitem.i-coat:screen-value in browse {&browse-name} = "0" .
              END.
              ELSE rfqitem.i-coat:screen-value in browse {&browse-name} = "1" .


           END.
        END.
  end.  /* adm-new */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size B-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* calc blank W,L SqIn */

   def var lv-panels as log no-undo.
   DEF VAR i AS INT NO-UNDO.
   def var j as int no-undo.
   def var v-score-char like v-lscore-c extent 100 NO-UNDO.


   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.

   lv-panels = sys-ctrl.log-fld.

   if not avail rfq then find first rfq where rfq.company = rfqitem.company and
                                        rfq.rfq-no = rfqitem.rfq-no
                                        no-lock.        
   find style where style.company = rfq.company and
                    style.style = rfqitem.style
                    no-lock no-error.
   if avail style and style.industry = "2" then do:

      if style.type <> "F" then run calc-blank-size2. 

      run rfq/u2kinc1c.p (recid(rfq), recid(rfqitem)).
      run rfq/u2kinc2c.p (recid(rfq), recid(rfqitem)).
      find bf-rfqitem of rfqitem exclusive-lock.    
      find first formule no-lock.
      assign bf-rfqitem.t-wid = (formule.formule[1])
          bf-rfqitem.t-len = (formule.formule[2])
          bf-rfqitem.t-sqin = (formule.formule[7] * formule.formule[8]).

      if not lv-panels or style.type = "F" then 
         assign bf-rfqitem.k-wid-array2[1] = bf-rfqitem.t-wid
                bf-rfqitem.k-len-array2[1] = bf-rfqitem.t-len.
      else do:
         run rfq/descalc2.p (recid(bf-rfqitem)).

         DO i = 1 TO EXTENT(bf-rfqitem.k-wid-scr-type2):
           ASSIGN
            bf-rfqitem.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            bf-rfqitem.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" THEN
            assign bf-rfqitem.k-wid-array2[1] = bf-rfqitem.t-wid
                   bf-rfqitem.k-len-array2[1] = bf-rfqitem.t-len.
         else do:
           i = 0.
           for each w-box-design-line:

             ASSIGN
                i = i + 1
                bf-rfqitem.k-wid-array2[i] = w-box-design-line.wscore-d.
             {sys/inc/k16bb.i bf-rfqitem.k-wid-array2[i]}



           end.

           assign v-score-char    = ""
                  j               = 1.
           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt /*12*/ EXTENT(bf-rfqitem.k-len-array2) then leave.
           end.
           do i = 1 to EXTENT(bf-rfqitem.k-len-array2):
              bf-rfqitem.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i bf-rfqitem.k-len-array2[i]}.
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */

   end.
   else if avail style then do:  /* folding */
      run rfq/u2kinc1f.p (recid(rfq), recid(rfqitem)).
      run rfq/u2kinc2f.p (recid(rfq), recid(rfqitem)).
      find first formule no-lock.
      find bf-rfqitem of rfqitem exclusive-lock.    
      assign bf-rfqitem.t-wid = (formule.formule[1])
          bf-rfqitem.t-len = (formule.formule[2])
          bf-rfqitem.t-sqin = (formule.formule[7] * formule.formule[8]).
   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 B-table-Win 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   find xritem where recid(xritem) = recid(rfqitem) no-lock.

   {rfq/u2estc.i rfqitem.gluelap 1}
   {rfq/u2estc.i rfqitem.k-wid 2}
   find first item where item.company = rfq.company
                    and item.i-no eq rfqitem.adhesive
                  no-lock no-error.
   if avail item then do:
            if item.mat-type eq "G" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 3}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 5}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    {rfq/u2estc.i rfqitem.k-len 7}
            end.
    end.
    else do:
                 {rfq/u2estc.i rfqitem.k-len 7}
    end.

    if rfqitem.len eq rfqitem.wid
    then do:
                 {rfq/u2estc.i rfqitem.k-wid 2 dim-fit}
    end.
    else do:
                 {rfq/u2estc.i rfqitem.k-wid 2}
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pass B-table-Win 
PROCEDURE calc-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* *********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.

      find first style where style.company = rfqitem.company and
                 style.style = rfqitem.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = rfqitem.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = rfqitem.company  and
                 alt-item.mat-type = "V"     and
                 alt-item.i-no     = style.material[6]
                 no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first rfq-ctrl where rfq-ctrl.company = rfqitem.company and
                                   rfq-ctrl.loc = rfqitem.loc
                                   no-lock no-error.
         if k = 0 then k = rfq-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = rfqitem.company and
                       item.i-no = rfq-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = rfqitem.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = rfq-ctrl.def-coat
                                      no-lock no-error.
      end.

      ASSIGN
      save_id = recid(item)
      save_id2 = IF AVAIL ALT-ITEM THEN recid(alt-item) ELSE ?
      j = (integer(rfqitem.i-col:screen-value in browse {&browse-name})
          + integer(rfqitem.i-coat:screen-value in browse {&browse-name})  ) 
      counter = 1
      choice = true.
      {sys/inc/roundup.i j}

      find bf-rfqitem of rfqitem exclusive-lock.    
      if choice then do i = 1 to 10:
         if i le integer(rfqitem.i-col:screen-value) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
              assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = item.i-no
                     bf-rfqitem.i-dscr[i] = item.est-dscr
                     bf-rfqitem.i-%[i]    = k.
         end.
         else if (i > integer(rfqitem.i-col)) and
                 (i <= (integer(rfqitem.i-col) + 
                       integer(rfqitem.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = IF AVAIL ALT-ITEM THEN alt-item.i-no ELSE ""
                     bf-rfqitem.i-dscr[i] = IF AVAIL ALT-ITEM THEN alt-item.est-dscr ELSE ""
                     bf-rfqitem.i-%[i]    = 100.
         end.
         else if (i >  (rfqitem.i-col + rfqitem.i-coat) )
         then do:
            assign bf-rfqitem.i-ps[i]   = 0  
                     bf-rfqitem.i-code[i] = ""
                     bf-rfqitem.i-dscr[i] = "" 
                     bf-rfqitem.i-%[i]    = 0.  

         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (rfqitem.i-pass) then counter = rfqitem.i-pass.
      end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est B-table-Win 
PROCEDURE copy-from-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-tmp-recid as recid no-undo.
  DEF VAR i AS INT NO-UNDO.
  /*def input parameter ip-est-no like est.est-no no-undo.

  find est where est.company = rfq.company and
                 est.loc = rfq.loc and
                 est.est-no = ip-est-no
                 no-lock no-error.   
  find first ef where ef.e-num = est.e-num no-lock no-error
  find first eb of ef no-lock no-error

  if avail eb then  do:
   */

  lv-rfqitem-copied-from-est = yes.
  lv-tmp-recid = if recid(rfqitem) <> ? then recid(rfqitem) else lv-recid.
  find bf-rfqitem where recid(bf-rfqitem) = lv-tmp-recid .  /* lv-recid <= add */
  assign bf-rfqitem.adder[1] = ef.adder[1]                                        
         bf-rfqitem.adder[2] = ef.adder[2]
         bf-rfqitem.adder[3] = ef.adder[3]
         bf-rfqitem.adder[4] = ef.adder[4]
         bf-rfqitem.adder[5] = ef.adder[5]
         bf-rfqitem.adder[6] = ef.adder[6]
         bf-rfqitem.adder[7] = ef.adder[7]
         bf-rfqitem.adder[8] = ef.adder[8]
         bf-rfqitem.adder[9] = ef.adder[9]
         bf-rfqitem.adder[10] = ef.adder[10]
         bf-rfqitem.adder[11] = ef.adder[11]
         bf-rfqitem.adder[12] = ef.adder[12]
         bf-rfqitem.leaf[1]  = ef.leaf[1]   
         bf-rfqitem.leaf[2]  = ef.leaf[2]   
         bf-rfqitem.leaf[3]  = ef.leaf[3]   
         bf-rfqitem.leaf[4]  = ef.leaf[4]   
         bf-rfqitem.leaf-dscr[1]  = ef.leaf-dscr[1] 
         bf-rfqitem.leaf-dscr[2]  = ef.leaf-dscr[2] 
         bf-rfqitem.leaf-dscr[3]  = ef.leaf-dscr[3] 
         bf-rfqitem.leaf-dscr[4]  = ef.leaf-dscr[4] 
         bf-rfqitem.leaf-l[1]  = ef.leaf-l[1]   
         bf-rfqitem.leaf-l[2]  = ef.leaf-l[2]   
         bf-rfqitem.leaf-l[3]  = ef.leaf-l[3]   
         bf-rfqitem.leaf-l[4]  = ef.leaf-l[4]   
         bf-rfqitem.leaf-w[1]  = ef.leaf-w[1]   
         bf-rfqitem.leaf-w[2]  = ef.leaf-w[2]   
         bf-rfqitem.leaf-w[3]  = ef.leaf-w[3]   
         bf-rfqitem.leaf-w[4]  = ef.leaf-w[4]   
         bf-rfqitem.brd-dscr   = ef.brd-dscr
         bf-rfqitem.cost-msh   = ef.cost-msh 
         bf-rfqitem.cost-uom     = ef.cost-uom 
         bf-rfqitem.die-in       = ef.die-in 
         bf-rfqitem.flute   = ef.flute    
         bf-rfqitem.fr-msh   = ef.fr-msh  
         bf-rfqitem.fr-uom    = ef.fr-uom  
         bf-rfqitem.lam-code =  ef.lam-code 
         bf-rfqitem.m-code   = ef.m-code    
         bf-rfqitem.m-dscr  = ef.m-dscr      
         bf-rfqitem.medium  = ef.medium     
         bf-rfqitem.test     =    ef.test 
         .

  assign  bf-rfqitem.adhesive = eb.adhesive 
          bf-rfqitem.cad-no   = eb.cad-no        
          bf-rfqitem.carr-dscr = eb.carr-dscr 
          bf-rfqitem.carrier = eb.carrier      
          bf-rfqitem.cas-cnt = eb.cas-cnt         
          bf-rfqitem.cas-cost = eb.cas-cost 
          bf-rfqitem.cas-dep  = eb.cas-dep
          bf-rfqitem.cas-len   = eb.cas-len  
          bf-rfqitem.cas-no   = eb.cas-no    
          bf-rfqitem.cas-pal  = eb.cas-pal 
          bf-rfqitem.cas-wid  = eb.cas-wid 
          bf-rfqitem.cas-wt   = eb.cas-wt 
          bf-rfqitem.die-in   = eb.die-in
          bf-rfqitem.die-no   = eb.die-no 
          bf-rfqitem.dust     = eb.dust 
          bf-rfqitem.flute    = eb.flute 
          bf-rfqitem.fpanel   = eb.fpanel
         .

    assign 
        bf-rfqitem.gluelap = eb.gluelap                                       
        bf-rfqitem.i-%[1] = eb.i-%[1]    
        bf-rfqitem.i-%[2] = eb.i-%[2]    
        bf-rfqitem.i-%[3] = eb.i-%[3]    
        bf-rfqitem.i-%[4] = eb.i-%[4]    
        bf-rfqitem.i-%[5] = eb.i-%[5]    
        bf-rfqitem.i-%[6] = eb.i-%[6]    
        bf-rfqitem.i-%[7] = eb.i-%[7]    
        bf-rfqitem.i-%[8] = eb.i-%[8]    
        bf-rfqitem.i-%[9] = eb.i-%[9]    
        bf-rfqitem.i-%[10] = eb.i-%[10]    
        /*bf-rfqitem.i-coat = eb.i-coat   */
        bf-rfqitem.i-code[1] = eb.i-code[1] 
        bf-rfqitem.i-code[2] = eb.i-code[2] 
        bf-rfqitem.i-code[3] = eb.i-code[3] 
        bf-rfqitem.i-code[4] = eb.i-code[4] 
        bf-rfqitem.i-code[5] = eb.i-code[5] 
        bf-rfqitem.i-code[6] = eb.i-code[6] 
        bf-rfqitem.i-code[7] = eb.i-code[7] 
        bf-rfqitem.i-code[8] = eb.i-code[8] 
        bf-rfqitem.i-code[9] = eb.i-code[9] 
        bf-rfqitem.i-code[10] = eb.i-code[10] 
      /*  bf-rfqitem.i-col = eb.i-col   */
        bf-rfqitem.i-coldscr = eb.i-coldscr 
        bf-rfqitem.i-dscr[1] = eb.i-dscr[1] 
        bf-rfqitem.i-dscr[2] = eb.i-dscr[2] 
        bf-rfqitem.i-dscr[3] = eb.i-dscr[3] 
        bf-rfqitem.i-dscr[4] = eb.i-dscr[4] 
        bf-rfqitem.i-dscr[5] = eb.i-dscr[5] 
        bf-rfqitem.i-dscr[6] = eb.i-dscr[6] 
        bf-rfqitem.i-dscr[7] = eb.i-dscr[7] 
        bf-rfqitem.i-dscr[8] = eb.i-dscr[8] 
        bf-rfqitem.i-dscr[9] = eb.i-dscr[9] 
        bf-rfqitem.i-dscr[10] = eb.i-dscr[10] 
        bf-rfqitem.i-pass = eb.i-pass   
        bf-rfqitem.i-ps[1] = eb.i-ps[1]  
        bf-rfqitem.i-ps[2] = eb.i-ps[2]  
        bf-rfqitem.i-ps[3] = eb.i-ps[3]  
        bf-rfqitem.i-ps[4] = eb.i-ps[4]  
        bf-rfqitem.i-ps[5] = eb.i-ps[5]  
        bf-rfqitem.i-ps[6] = eb.i-ps[6]  
        bf-rfqitem.i-ps[7] = eb.i-ps[7]  
        bf-rfqitem.i-ps[8] = eb.i-ps[8]  
        bf-rfqitem.i-ps[9] = eb.i-ps[9]  
        bf-rfqitem.i-ps[10] = eb.i-ps[10]  
        .
  assign      
        bf-rfqitem.k-len = eb.k-len     
        bf-rfqitem.k-wid = eb.k-wid   
        bf-rfqitem.len  = eb.len      
        bf-rfqitem.lin-in = eb.len     
        bf-rfqitem.lock = eb.lock
        bf-rfqitem.part-dscr1 = eb.part-dscr1 
        bf-rfqitem.part-dscr2 = eb.part-dscr2
        bf-rfqitem.part-no = eb.part-no 
        bf-rfqitem.plate-no = eb.plate-no
     /*   bf-rfqitem.procat   = eb.procat */
/*        rfq.ship-addr[1] = eb.ship-addr[1]
        rfq.ship-addr[2] = eb.ship-addr[2]
        rfq.ship-city    = eb.ship-city 
         bf-rfqitem.ship-id = eb.ship-id   
        eb.ship-name     = rfq.ship-name                                 
        eb.ship-state    = rfq.ship-state                
        eb.ship-zip      = rfq.ship-zip                    
        eb.sman          = rfq.sman               
*/  
        bf-rfqitem.spc-no   = eb.spc-no  
        bf-rfqitem.tr-cas  = eb.tr-cas 
        bf-rfqitem.tr-cnt  = eb.tr-cnt 
        bf-rfqitem.tr-cost = eb.tr-cost 
        bf-rfqitem.tr-dep  = eb.tr-dep
        bf-rfqitem.tr-len  = eb.tr-len
        bf-rfqitem.tr-no   = eb.tr-no
        bf-rfqitem.tr-wid  = eb.tr-wid 
        bf-rfqitem.tuck    = eb.tuck 
        bf-rfqitem.upc-no  = eb.upc-no
        bf-rfqitem.weight-m = eb.weight-m
        bf-rfqitem.t-wid    = eb.t-wid 
        bf-rfqitem.t-len    = eb.t-len
        bf-rfqitem.t-sqin = eb.t-sqin              
        bf-rfqitem.est-no = eb.est-no
        bf-rfqitem.form-no = eb.form-no
        bf-rfqitem.blank-no = eb.blank-no
        .   
/*==========
ef.lsh-dep      = bf-rfqitem.lsh-dep
ef.lsh-len      = bf-rfqitem.lsh-len                             
ef.lsh-lock     = bf-rfqitem.lsh-lock                 
ef.lsh-wid      = bf-rfqitem.lsh-wid                
ef.nsh-dep    = bf-rfqitem.nsh-dep
ef.nsh-len    = bf-rfqitem.nsh-len                   
ef.nsh-wid    = bf-rfqitem.nsh-wid                   
ef.op-lock    = bf-rfqitem.op-lock                   

eb.bl-qty                                         
eb.i-coat-p  = bf-rfqitem.i-coat-p                    
eb.cust-seq        
eb.cust-%                       
eb.dest-code  
eb.disc-rate = bf-rfqitem.disc-rate                                     
eb.est-int   = 
eb.fr-out-c  = bf-rfqitem.fr-out-c                       
eb.fr-out-m  = bf-rfqitem.fr-out-m                   
eb.i-code2   = bf-rfqitem.i-code2             
eb.i-%2      = bf-rfqitem.i-%2                        
eb.i-dscr2   = bf-rfqitem.i-dscr   
eb.i-ps2     = bf-rfqitem.i-ps2                                     
eb.k-len-array  = bf-rfqitem.k-len-array
eb.k-wid-array  = bf-rfqitem.k-wid-array                    
eb.n-ply     = bf-rfqitem.n-ply
eb.num-dep   = bf-rfqitem.num-dep                      
eb.num-len   = bf-rfqitem.num-len                    
eb.num-up    = bf-rfqitem.num-up                    
eb.num-wid   = bf-rfqitem.num-wid                     
eb.ship-no       = bf-rfqitem.ship-no                                 
eb.stack-code    = bf-rfqitem.stack-code                                 
eb.stacks        = bf-rfqitem.stacks                                 
eb.sty-lock      = bf-rfqitem.sty-lock
eb.t-dep         = bf-rfqitem.t-dep                               
eb.t-len         = bf-rfqitem.t-len                   
eb.t-sqin        = bf-rfqitem.t-sqin                  
eb.t-wid         = bf-rfqitem.t-wid                 
eb.t-win         = bf-rfqitem.t-win                  
eb.tab-in        = bf-rfqitem.tab-in                  
eb.yld-qty       = bf-rfqitem.yld-qty                    
eb.yrprice       = bf-rfqitem.yrprice        
*/

   IF eb.est-type < 5 THEN DO i = 1 TO 10:
      ASSIGN bf-rfqitem.i-ps[i] = eb.i-ps2[i]
             bf-rfqitem.i-code[i] = eb.i-code2[i]
             bf-rfqitem.i-dscr[i] = eb.i-dscr2[i]
             bf-rfqitem.i-%[i] = eb.i-%2[i].
   END.
   IF eb.est-type = 4 OR eb.est-type = 8 THEN DO:
      i = 1.
      FOR EACH est-flm NO-LOCK WHERE est-flm.company = eb.company
                                 AND est-flm.est-no = eb.est-no
                                 by est-flm.snum 
                                 by est-flm.bnum
                                 BY est-flm.line.
          IF i > 10 THEN LEAVE.
          ASSIGN bf-rfqitem.leaf[i] = est-flm.i-no
                 bf-rfqitem.leaf-dscr[i] = est-flm.dscr
                 bf-rfqitem.leaf-l[i]  = est-flm.len   
                 bf-rfqitem.leaf-w[i]  = est-flm.wid 
                 i = i + 1.

      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est2 B-table-Win 
PROCEDURE copy-from-est2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       assign rfqitem.len:screen-value in browse {&browse-name} = string(eb.len)
              rfqitem.wid:screen-value in browse {&browse-name} = string(eb.wid)
              rfqitem.dep:screen-value in browse {&browse-name} = string(eb.dep)
              rfqitem.procat:screen-value in browse {&browse-name} = eb.procat
              rfqitem.style:screen-value in browse {&browse-name} = eb.style
              rfqitem.stock-no:screen-value in browse {&browse-name} = eb.stock-no
              rfqitem.i-name:screen-value in browse {&browse-name} = eb.part-dscr1
              rfqitem.part-no:screen-value in browse {&browse-name} = eb.part-no
              rfqitem.board:screen-value in browse {&browse-name} = ef.board
              rfqitem.cal:screen-value in browse {&browse-name} = string(ef.cal)
              rfqitem.i-col:screen-value in browse {&browse-name} = string(eb.i-col)
              rfqitem.i-coat:screen-value in browse {&browse-name} = string(eb.i-coat)
              .

        find style where style.company = rfqitem.company and
                              style.style = eb.style
                              no-lock no-error.

        IF AVAIL style AND style.material[2] EQ ""  THEN do:
            ASSIGN
                rfqitem.i-col:screen-value in browse {&browse-name} = "0" 
                rfqitem.i-pass = 0     .
        END.
        ELSE rfqitem.i-col:screen-value in browse {&browse-name} = "1" .
        IF AVAIL style AND style.material[6] EQ "" THEN do:
            rfqitem.i-coat:screen-value in browse {&browse-name} = "0" .
        END.
        ELSE rfqitem.i-coat:screen-value in browse {&browse-name} = "1" .

        if avail style and style.industry = "2" then      ll-is-corr-style = yes.
        else ll-is-corr-style = no.
        IF ll-is-corr-style THEN do:
        ASSIGN rfqitem.len:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.len))
                        rfqitem.wid:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.wid))
                        rfqitem.dep:SCREEN-VALUE = string(to-corrware-dim(ll-is-corr-style,eb.dep))
                        .
        END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-cwfw B-table-Win 
PROCEDURE disp-cwfw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find style where style.company = rfqitem.company and
                      style.style = rfqitem.style
                      no-lock no-error.
  if avail style and style.industry = "2" then do:

     IF v-cecscrn-char NE "Decimal" THEN
        assign rfqitem.len:format in browse {&browse-name} = ">>9.99"
               rfqitem.wid:format  = ">>9.99"
               rfqitem.dep:format  = ">>9.99".
     ELSE
        assign rfqitem.len:format in browse {&browse-name} = ">>9.99"
               rfqitem.wid:format  = ">>9.999999"
               rfqitem.dep:format  = ">>9.999999".

     ASSIGN
        rfqitem.len:screen-value = string({sys/inc/k16.i rfqitem.len } )
        rfqitem.wid:screen-value = string({sys/inc/k16.i rfqitem.wid } )
        rfqitem.dep:screen-value = string({sys/inc/k16.i rfqitem.dep } ).
  end.
  else 
     assign rfqitem.len:format  = ">9.99999"
            rfqitem.wid:format  = ">9.99999"
            rfqitem.dep:format  = ">9.99999".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

 /* run next-seq-no.
  rfqitem.seq:screen-value in browse {&browse-name} = string(return-value).
      ---> assing-record triggered
   */  

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.  */  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF lv-rfqitem-copied-from-est AND ll-is-corr-style THEN DO:
      {sys/inc/k16bb.i rfqitem.wid  } 
      {sys/inc/k16bb.i rfqitem.len  } 
      {sys/inc/k16bb.i rfqitem.dep  } 
  END.

  if not lv-copy-record and not lv-rfqitem-copied-from-est then do:

     if ll-is-corr-style then do:
        {sys/inc/k16bb.i rfqitem.wid  } 
        {sys/inc/k16bb.i rfqitem.len  } 
        {sys/inc/k16bb.i rfqitem.dep  } 
     end.

     if rfqitem.stock-no:screen-value in browse {&browse-name} = "" then do:
        find cust where cust.company = rfq.company and cust.cust-no = rfq.cust-no
                        no-lock no-error.
        find first rfq-ctrl no-lock no-error.
        /*if rfqitem.cas-no = "" then*/ 
        rfqitem.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else rfq-ctrl.def-case.
        /*if rfqitem.tr-no = "" then */ 
        rfqitem.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else rfq-ctrl.def-pal.      
     end.
     /* get default values from rm table */
     find item where item.company = rfqitem.company and
                     item.i-no = rfqitem.cas-no
              no-lock no-error.
     if avail item then assign /*rfqitem.cas-cost:Screen-value = */
                              rfqitem.cas-cnt = (item.box-case)
                              rfqitem.cas-len = (item.case-l)
                              rfqitem.cas-wid = (item.case-w)
                              rfqitem.cas-dep = (item.case-d)
                              rfqitem.cas-pal = (item.case-pall)
                              rfqitem.cas-wt = (item.avg-w)         
                              .
     find item where item.company = rfqitem.company and
                     item.i-no = rfqitem.tr-no
              no-lock no-error.
     if avail item then assign /*rfqitem.cas-cost:Screen-value = */
                              rfqitem.tr-len = (item.case-l)
                              rfqitem.tr-wid = (item.case-w)
                              rfqitem.tr-dep = (item.case-d)
                              .
     find first shipto where shipto.company = rfq.company and
                             shipto.cust-no = rfq.cust-no
                             no-lock no-error.
     if avail shipto then assign rfqitem.ship-id = shipto.ship-id
                                 rfqitem.carrier = shipto.carrier.

     find style where style.company = rfq.company and
                      style.style = rfqitem.style:screen-value
                      no-lock no-error.
     if avail style then assign rfqitem.adhesive = style.material[7]
                                rfqitem.gluelap = style.dim-gl
                                rfqitem.k-wid = style.dim-dkw
                                rfqitem.fpanel = style.dim-pan5
                                rfqitem.lock = style.dim-fit
                                rfqitem.tuck = style.dim-tk.                 
     if rfqitem.gluelap <> 0 then rfqitem.lin-in = rfqitem.dep.
     IF adm-adding-record AND rfqitem.i-col = 0 THEN rfqitem.i-pass = 0.
     run assign-rfqitem.   
     run calc-pass.
     run calc-blank-size.
  end.
  else if lv-copy-record then do:     
       rfqitem.est-no = "".
       if ll-is-corr-style then do:
          {sys/inc/k16bb.i rfqitem.wid  } 
          {sys/inc/k16bb.i rfqitem.len  } 
          {sys/inc/k16bb.i rfqitem.dep  } 
       end.
       find bf-rfqitem where recid(bf-rfqitem) = recid(rfqitem).
       assign bf-rfqitem.qty[2] = lv-copy-qty[2]
              bf-rfqitem.qty[3] = lv-copy-qty[3]
              bf-rfqitem.qty[4] = lv-copy-qty[4]
              bf-rfqitem.qty[5] = lv-copy-qty[5]
              bf-rfqitem.qty[6] = lv-copy-qty[6]
              bf-rfqitem.qty[7] = lv-copy-qty[7]
              bf-rfqitem.qty[8] = lv-copy-qty[8]
              bf-rfqitem.qty[9] = lv-copy-qty[9]
              bf-rfqitem.qty[10] = lv-copy-qty[10]
              bf-rfqitem.qty-price[1] = lv-copy-pr[1]
              bf-rfqitem.qty-price[2] = lv-copy-pr[2]
              bf-rfqitem.qty-price[3] = lv-copy-pr[3]
              bf-rfqitem.qty-price[4] = lv-copy-pr[4]
              bf-rfqitem.qty-price[5] = lv-copy-pr[5]
              bf-rfqitem.qty-price[6] = lv-copy-pr[6]
              bf-rfqitem.qty-price[7] = lv-copy-pr[7]
              bf-rfqitem.qty-price[8] = lv-copy-pr[8]
              bf-rfqitem.qty-price[9] = lv-copy-pr[9]
              bf-rfqitem.qty-price[10] = lv-copy-pr[10]
              bf-rfqitem.qty-uom[1] = lv-copy-uom[1]
              bf-rfqitem.qty-uom[2] = lv-copy-uom[2]
              bf-rfqitem.qty-uom[3] = lv-copy-uom[3]
              bf-rfqitem.qty-uom[4] = lv-copy-uom[4]
              bf-rfqitem.qty-uom[5] = lv-copy-uom[5]
              bf-rfqitem.qty-uom[6] = lv-copy-uom[6]
              bf-rfqitem.qty-uom[7] = lv-copy-uom[7]
              bf-rfqitem.qty-uom[8] = lv-copy-uom[8]
              bf-rfqitem.qty-uom[9] = lv-copy-uom[9]
              bf-rfqitem.qty-uom[10] = lv-copy-uom[10]
              bf-rfqitem.qty-date[1] = lv-copy-date[1]  
              bf-rfqitem.qty-date[2] = lv-copy-date[2]  
              bf-rfqitem.qty-date[3] = lv-copy-date[3]  
              bf-rfqitem.qty-date[4] = lv-copy-date[4]  
              bf-rfqitem.qty-date[5] = lv-copy-date[5]  
              bf-rfqitem.qty-date[6] = lv-copy-date[6]  
              bf-rfqitem.qty-date[7] = lv-copy-date[7]  
              bf-rfqitem.qty-date[8] = lv-copy-date[8]  
              bf-rfqitem.qty-date[9] = lv-copy-date[9]  
              bf-rfqitem.qty-date[10] = lv-copy-date[10]  
              .  
  end.
/*  
  find style where style.company = rfqitem.company and
                      style.style = rfqitem.style
                      no-lock no-error.
  if avail style and style.industry = "2" then   
  /*     ll-is-corr-style = yes.
      else ll-is-corr-style = no.  
  */  
  do:  
     {sys/inc/k16bb.i rfqitem.wid  } 
     {sys/inc/k16bb.i rfqitem.len  } 
     {sys/inc/k16bb.i rfqitem.dep  } 
  end.   
*/

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
  RUN set-panel(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  lv-copy-record = yes. 
  lv-old-rfqitem-id = recid(rfqitem).

  ASSIGN lv-copy-qty[2] = rfqitem.qty[2]
         lv-copy-qty[3] = rfqitem.qty[3]
         lv-copy-qty[4] = rfqitem.qty[4]
         lv-copy-qty[5] = rfqitem.qty[5]
         lv-copy-qty[6] = rfqitem.qty[6]
         lv-copy-qty[7] = rfqitem.qty[7]
         lv-copy-qty[8] = rfqitem.qty[8]
         lv-copy-qty[9] = rfqitem.qty[9]
         lv-copy-qty[10] = rfqitem.qty[10]
         lv-copy-pr[1] = rfqitem.qty-price[1] 
         lv-copy-pr[2] = rfqitem.qty-price[2] 
         lv-copy-pr[3] = rfqitem.qty-price[3] 
         lv-copy-pr[4] = rfqitem.qty-price[4] 
         lv-copy-pr[5] = rfqitem.qty-price[5]  
         lv-copy-pr[6] = rfqitem.qty-price[6] 
         lv-copy-pr[7] = rfqitem.qty-price[7] 
         lv-copy-pr[8] = rfqitem.qty-price[8] 
         lv-copy-pr[9] = rfqitem.qty-price[9] 
         lv-copy-pr[10] = rfqitem.qty-price[10] 
         lv-copy-uom[1] = rfqitem.qty-uom[1] 
         lv-copy-uom[2] = rfqitem.qty-uom[2] 
         lv-copy-uom[3] = rfqitem.qty-uom[3] 
         lv-copy-uom[4] = rfqitem.qty-uom[4] 
         lv-copy-uom[5] = rfqitem.qty-uom[5] 
         lv-copy-uom[6] = rfqitem.qty-uom[6] 
         lv-copy-uom[7] = rfqitem.qty-uom[7] 
         lv-copy-uom[8] = rfqitem.qty-uom[8] 
         lv-copy-uom[9] = rfqitem.qty-uom[9] 
         lv-copy-uom[10] = rfqitem.qty-uom[10] 
         lv-copy-date[1] = rfqitem.qty-date[1] 
         lv-copy-date[2] = rfqitem.qty-date[2] 
         lv-copy-date[3] = rfqitem.qty-date[3] 
         lv-copy-date[4] = rfqitem.qty-date[4] 
         lv-copy-date[5] = rfqitem.qty-date[5] 
         lv-copy-date[6] = rfqitem.qty-date[6] 
         lv-copy-date[7] = rfqitem.qty-date[7] 
         lv-copy-date[8] = rfqitem.qty-date[8] 
         lv-copy-date[9] = rfqitem.qty-date[9] 
         lv-copy-date[10] = rfqitem.qty-date[10] 
         .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 /* run next-seq-no.
    display integer(return-value) @ rfqitem.seq with browse {&browse-name}.

   rfqitem.seq:screen-value in browse {&browse-name} = return-value.
*/

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

  /*if not lv-copy-record then */
  run next-seq-no.
  assign rfqitem.rfq-no = rfq.rfq-no
            rfqitem.company = rfq.company
            rfqitem.loc = rfq.loc
            rfqitem.seq = /*if not lv-copy-record then integer(return-value)
                          else integer(rfqitem.seq:screen-value in browse {&browse-name})
                          */
                          integer(return-value)
            .
  find first shipto where shipto.company  = rfq.company
                         and shipto.cust-no  = rfq.cust-no
                             no-lock no-error.
  assign rfqitem.ship-id = if avail shipto then shipto.ship-id else ""
         rfqitem.carrier = if avail shipto then shipto.carrier else "".

  if not lv-copy-record then do:
     FIND FIRST rfq-ctrl WHERE rfq-ctrl.company = rfq.company NO-LOCK NO-ERROR.

     assign rfqitem.qty[1] = 0  /* display problem when add new record */
            /*rfqitem.i-col = IF AVAIL rfq-ctrl AND rfq-ctrl.def-ink <> "" THEN 1 ELSE 0
            rfqitem.i-coat = IF AVAIL rfq-ctrl AND rfq-ctrl.def-coat <> "" THEN 1 ELSE 0*/
            rfqitem.qty-date = today
            rfqitem.qty-uom = "M"
            rfqitem.qty[99] = 1 .
     display rfqitem.qty[1] with browse {&browse-name}.
   end. 
   else do:
        find bf-rfqitem where recid(bf-rfqitem) = lv-old-rfqitem-id no-lock.
        buffer-copy  bf-rfqitem except bf-rfqitem.seq to rfqitem.      
   end.
   lv-recid = recid(rfqitem).  /* to resolve error "no rfqitem record avail" */

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
  if not avail rfqitem then find rfqitem where recid(rfqitem) = lv-recid no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run reset-seqno.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail rfqitem then return.

  RUN set-panel(0).
  apply "entry" to rfqitem.qty in browse {&browse-name}.
  return no-apply.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide B-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
 /*  rfq/brwleave.i  save automatically 
  /* check mode where in update */
  DEFINE VARIABLE vlChangePages AS LOGICAL NO-UNDO.  
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.
*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

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
  define variable cNewRecord as character no-undo.
  define variable cMode      as character no-undo.
  def var source-str as cha no-undo.    

  RUN get-attribute ( input 'ADM-NEW-RECORD':U ).
  cNewRecord = return-value.  
  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail rfqitem then find rfqitem where recid(rfqitem) = lv-recid no-error.
     /* There is a bug when row created from last row - no rfqitem records avail 
        => refind table using variable (lv-recid)   
     */

  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* validate stock-no */
  if not adm-new-record then do:  /* update existint records */
    find first itemfg where itemfg.company = rfq.company and
                            itemfg.i-no = rfqitem.stock-no:screen-value in browse {&browse-name}
                      no-lock no-error.

    if not avail itemfg then do:
       find first eb where eb.company = rfq.company and
                           eb.loc = rfq.loc and
                           eb.stock-no = rfqitem.stock-no:screen-value in browse {&browse-name}
                           no-lock no-error.
       if not avail eb then do:                    
          message "Invalid FG Item#. Try Help.".
          apply "entry" to rfqitem.stock-no in browse {&browse-name}.
          return no-apply.
       end.
    end.  
  end.
  if rfqitem.board:screen-value <> "" and
     not can-find(first item where item.company = rfq.company
                and item.i-no = rfqitem.board:screen-value in browse {&browse-name}
                and  lookup(item.mat-type,"B,P,1,2,3,4") > 0 )
  then do:
         message "Invalid Board. Try Help."    view-as alert-box error.
         apply "entry" to rfqitem.board.
         return no-apply.
  end.     
  if  ll-is-corr-style and
      decimal(rfqitem.len:screen-value) - trunc(decimal(rfqitem.len:screen-value),0) >= 0.16 
  then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to rfqitem.len.    
      return no-apply.
  end.
  if  ll-is-corr-style and
      decimal(rfqitem.wid:screen-value) - trunc(decimal(rfqitem.wid:screen-value),0) >= 0.16 
  then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to rfqitem.wid.    
      return no-apply.
  end.
  if  ll-is-corr-style and
      decimal(rfqitem.dep:screen-value) - trunc(decimal(rfqitem.dep:screen-value),0) >= 0.16 
  then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to rfqitem.dep.    
      return no-apply.
  end.



    /* ============================*/

  /* Dispatch standard ADM method.                             */  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .  

  /* Code placed here will execute AFTER standard behavior.    */

  /* --- refresh record source browser -----   */
  if cNewRecord = 'yes' then cMode = 'newRecord':U.
  else cMode = 'Update':U.

  if cNewRecord = 'yes' then do:
     RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'RECORD-SOURCE':U, OUTPUT source-str).

     run refreshRow in widget-handle(source-str) ( input cMode, rowid(rfq) ).
   end.

  /* =========== not working 
   RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'RECORD-SOURCE':U, OUTPUT source-str).
   run dispatch in widget-handle(source-str) ("open-query"). /* tell to parent */
/*   run get-rowid in widget-handle(source-str) (output lv-hd-rowid).*/
   run reposition-query in widget-handle(source-str) (this-procedure).
   run dispatch in widget-handle(source-str) ("row-changed"). /* tell to parent */
======================== */

    if lv-copy-record then    lv-copy-record = no.


    run notify ('row-available'). /*tell updated info to children window */


    assign lv-rfqitem-copied-from-est = no
           lv-copy-record = no.

  RUN set-panel(1).

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

  run dispatch ('row-available').  /* to display corrware dimension -
                                      will run function to-corrware-dim */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE next-seq-no B-table-Win 
PROCEDURE next-seq-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


   find last bf-rfqitem where bf-rfqitem.rfq-no = rfq.rfq-no no-lock no-error.
   if not avail bf-rfqitem then li-seq = 0.
   else li-seq = bf-rfqitem.seq.

   li-seq = li-seq + 1.
   /*rfqitem.seq:screen-value in browse {&browse-name} = string(li-seq).    
   */   
   return string(li-seq). 
  /*
   find bf-rfqitem where recid(bf-rfqitem) = recid(rfqitem) no-error.
   if not avail bf-rfqitem then create bf-rfqitem.
      assign bf-rfqitem.rfq-no = rfq.rfq-no
          bf-rfqitem.company = rfq.company
          bf-rfqitem.loc = rfq.loc
          bf-rfqitem.seq = li-seq   
          .         
  message "in add " rfq.company rfq.rfq-no skip
           bf-rfqitem.rfq-no ", " bf-rfqitem.seq ", " bf-rfqitem.company
    view-as alert-box.   
  */

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-seqno B-table-Win 
PROCEDURE reset-seqno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var li-diff as int no-undo.

   li-diff = 1.   
   for each bf-rfqitem of rfq by bf-rfqitem.rfq-no by bf-rfqitem.seq:
       bf-rfqitem.seq = bf-rfqitem.seq - bf-rfqitem.seq + li-diff.
       li-diff = li-diff + 1.
   end.

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
  {src/adm/template/snd-list.i "rfq"}
  {src/adm/template/snd-list.i "rfqitem"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-panel B-table-Win 
PROCEDURE set-panel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-switch AS INT NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'set-source':U,OUTPUT char-hdl).
  IF ip-switch EQ 0 THEN RUN disable-all IN WIDGET-HANDLE(char-hdl).
  ELSE RUN enable-all IN WIDGET-HANDLE(char-hdl).

  RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'transfer-source':U,OUTPUT char-hdl).
  IF ip-switch EQ 0 THEN RUN disable-all IN WIDGET-HANDLE(char-hdl).
  ELSE RUN enable-all IN WIDGET-HANDLE(char-hdl).


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-set B-table-Win 
PROCEDURE update-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-ritem FOR rfqitem.
  FIND FIRST bf-ritem OF rfq WHERE bf-ritem.seq = 999 NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-ritem THEN DO:
     CREATE bf-ritem.
     BUFFER-COPY rfqitem EXCEPT rfqitem.seq TO bf-ritem.
     bf-ritem.seq = 999.     
  END.
  RUN rfq/d-rfqset.w  (RECID(bf-ritem),6).

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
  IF dec(rfqitem.qty[1]:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN DO:
     MESSAGE "Quantity must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.qty[1] IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF rfqitem.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     MESSAGE "Item Name must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.i-name IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF rfqitem.part-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     MESSAGE "Customer Part# must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.part-no IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF rfqitem.style:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     MESSAGE "Style must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.style IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF rfqitem.procat:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     MESSAGE "Product Category must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.procat IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF rfqitem.board:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     MESSAGE "Board must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.board IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF dec(rfqitem.len:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN DO:
     MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.len IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  IF dec(rfqitem.wid:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN DO:
     MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.wid IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.

  /*
  find first itemfg where itemfg.company = rfq.company and
                          itemfg.part-no = rfqitem.part-no:screen-value in browse {&browse-name}
                          no-lock no-error.
  IF NOT AVAIL itemfg THEN DO:
     MESSAGE "Invalid Customer Part#." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.part-no IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
  */
  FIND FIRST style WHERE style.company = rfq.company
                     AND style.style = rfqitem.style:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL style THEN DO:
     MESSAGE "Invalid Style. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfqitem.style.
     RETURN ERROR.
  END.
  FIND FIRST fgcat WHERE fgcat.company = rfq.company
                      AND fgcat.PROCat = rfqitem.procat:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL fgcat THEN DO:
      MESSAGE "Invalid Product Category. " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rfqitem.procat.
      RETURN ERROR.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION To-Corrware-Dim B-table-Win 
FUNCTION To-Corrware-Dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as DEC DECIMALS 6 no-undo.

  if ip-is-corr-style AND v-cecscrn-char NE "Decimal" THEN
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.

  RETURN out-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

