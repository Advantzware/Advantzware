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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
&scoped-define fld-name-1 rm-rctd.tag
&scoped-define SORTBY-1 BY {&fld-name-1}
&global-define IAMWHAT LOOKUP

DEF VAR ll-help-run AS LOG NO-UNDO.

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rm-rctd.tag rm-rctd.loc ~
rm-rctd.loc-bin rm-rctd.job-no rm-rctd.job-no2 rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.i-no rm-rctd.i-name rm-rctd.cost rm-rctd.cost-uom ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table rm-rctd.loc rm-rctd.loc-bin 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table rm-rctd
&Scoped-define QUERY-STRING-br_table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = g_company and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = g_company and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS browse-order 

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
r-no|y|y|ASI.rm-rctd.r-no
company||y|ASI.rm-rctd.company
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
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tag#", 1
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      rm-rctd.loc COLUMN-LABEL "Whs" FORMAT "x(13)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.job-no FORMAT "x(6)":U
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.rct-date FORMAT "99/99/9999":U
      rm-rctd.po-no FORMAT "x(9)":U
      rm-rctd.i-no FORMAT "x(10)":U
      rm-rctd.i-name FORMAT "x(30)":U
      rm-rctd.cost COLUMN-LABEL "Cost/UOM" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      rm-rctd.qty COLUMN-LABEL "Total!Qty" FORMAT "->>>>>>9.9<<<<<":U
      rm-rctd.pur-uom FORMAT "x(3)":U
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U
  ENABLE
      rm-rctd.loc
      rm-rctd.loc-bin
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 15.48
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     browse-order AT ROW 15.05 COL 17 HELP
          "Select Browser Sort Order" NO-LABEL
     lv-search AT ROW 15.05 COL 84 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 15.05 COL 127 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 15.29 COL 1
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
         HEIGHT             = 15.71
         WIDTH              = 144.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN lv-search IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-search:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RECT-4:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = g_company and
rm-rctd.rita-code = ""R"""
     _FldNameList[1]   > ASI.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.rm-rctd.loc
"loc" "Whs" "x(13)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ASI.rm-rctd.job-no
     _FldNameList[5]   = ASI.rm-rctd.job-no2
     _FldNameList[6]   = ASI.rm-rctd.rct-date
     _FldNameList[7]   = ASI.rm-rctd.po-no
     _FldNameList[8]   = ASI.rm-rctd.i-no
     _FldNameList[9]   = ASI.rm-rctd.i-name
     _FldNameList[10]   > ASI.rm-rctd.cost
"cost" "Cost/UOM" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.rm-rctd.cost-uom
"cost-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.rm-rctd.qty
"qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = ASI.rm-rctd.pur-uom
     _FldNameList[14]   > ASI.rm-rctd.user-id
"user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEF BUFFER bf-tmp FOR rm-rctd.
   DEF BUFFER xfg-rdtlh FOR fg-rdtlh.

   ll-help-run = yes.
   case focus:name :
        when "po-no" then do:
             run windows/l-pofg.w (rm-rctd.company,focus:screen-value, output char-val).
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
               if avail po-ordl then do:
                  assign /*-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                         rm-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom /* pr-uom */
                         rm-rctd.cost:screen-value in browse {&browse-name} = string(po-ordl.cost)  /* po-ordl.cost*/
                         .
               end.

               find first item where item.company = rm-rctd.company and
                                        item.i-no = entry(2,char-val)
                                        no-lock no-error.
               if avail item then do:                         
                  assign rm-rctd.loc:screen-value in browse {&browse-name} =  item.loc
                         rm-rctd.loc-bin:screen-value in browse {&browse-name} =  item.loc-bin
                       /*  rm-rctd.cost-uom = if item.pur-man = item.pur-uom
                                            else item.prod-uom  */                        
                         .
               end.          
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
  /*
               run tag-method (output ll-tag#).
               if ll-tag# and rm-rctd.po-no:screen-value in browse {&browse-name} <> ""
               then do:
                   run tag-sequence.
               end.
    */
             end.  /* char-val <> "" */
             return no-apply.   
       end.
       /*
       when "i-no" then do:
             IF rm-rctd.po-no:SCREEN-VALUE <> "" THEN DO:
                RUN windows/l-poitem.w (rm-rctd.company,rm-rctd.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
                if char-val <> "" then do :
                   assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       rm-rctd.i-name:screen-value = entry(2,char-val)
                       rm-rctd.job-no:screen-value = entry(3,char-val)
                       rm-rctd.job-no2:screen-value = entry(4,char-val)
                       .
                end.
             END.
             ELSE IF rm-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
                  RUN windows/l-jobit1.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE,rm-rctd.job-no2:screen-value, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
                  IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                  IF rec-val <> ? THEN DO:
                     FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.
                     IF AVAIL tt-job-hdr THEN 
                         ASSIGN rm-rctd.cost:SCREEN-VALUE = string(tt-job-hdr.std-mat-cost +
                                                              tt-job-hdr.std-lab-cost +
                                                              tt-job-hdr.std-fix-cost +
                                                              tt-job-hdr.std-var-cost)
                               .

                  END.

             END.
             ELSE DO:
                  RUN windows/l-itemf2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                  IF rec-val <> ? THEN DO:
                     FIND item WHERE RECID(item) = rec-val NO-LOCK.
                     ASSIGN rm-rctd.i-no:SCREEN-VALUE  = item.i-no
                       rm-rctd.i-name:SCREEN-VALUE = item.i-name
                       rm-rctd.loc:SCREEN-VALUE = item.loc
                       rm-rctd.loc-bin:SCREEN-VALUE = item.loc-bin
                       rm-rctd.cost:SCREEN-VALUE = string(item.avg-cost)
                       rm-rctd.cost-uom:SCREEN-VALUE = item.prod-uom  .

                  END.
             END.
             return no-apply.   
       end.
       */
       when "job-no" /*or when "job-no2" */ then do:
             run windows/l-jobno.w (rm-rctd.company, focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       */ 
                       rm-rctd.job-no:screen-value = entry(1,char-val)
                       rm-rctd.job-no2:screen-value = entry(2,char-val)
                       rm-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN rm-rctd.loc:SCREEN-VALUE = job-hdr.loc
                          rm-rctd.cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost)
                          .
             end.
             FIND FIRST item WHERE item.company = g_company
                           AND item.i-no = rm-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL item THEN
                 ASSIGN rm-rctd.i-name:SCREEN-VALUE = item.i-name
                        rm-rctd.loc-bin:SCREEN-VALUE = item.loc-bin
                        /*
                        rm-rctd.cost-uom:SCREEN-VALUE = item.pr-uom
                        */.

             return no-apply.   
       end.  
       when "job-no2" then do:
             run windows/l-jobno2.w (rm-rctd.company, rm-rctd.job-no:screen-value,focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       rm-rctd.job-no:screen-value = entry(1,char-val) */
                       rm-rctd.job-no2:screen-value = entry(2,char-val)
                       rm-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN rm-rctd.loc:SCREEN-VALUE = job-hdr.loc
                          rm-rctd.cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost)
                   .
             end.
             FIND item WHERE item.company = g_company
                           AND item.i-no = rm-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL item THEN
                 ASSIGN rm-rctd.i-name:SCREEN-VALUE = item.i-name
                        rm-rctd.loc-bin:SCREEN-VALUE = item.loc-bin
                        /*
                        rm-rctd.cost-uom:SCREEN-VALUE = item.prod-uom
                        */.
             return no-apply.   
       end.  
       when "loc" then do:
             run windows/l-loc.w (rm-rctd.company,focus:screen-value, output char-val).
             if char-val <> "" then do :
                assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                       .

             end.
             return no-apply.   
       end.
/*        when "loc-bin" then do:                                                                                               */
/*              run windows/l-fgbin.w (rm-rctd.company,rm-rctd.loc:screen-value, rm-rctd.loc-bin:screen-value,output char-val). */
/*              if char-val <> "" then do :                                                                                     */
/*                 assign focus:screen-value  = entry(1,char-val)                                                               */
/*                        /*rm-rctd.loc:screen-value = entry(2,char-val)                                                        */
/*                         rm-rctd.tag:screen-value = entry(4,char-val)*/                                                       */
/*                        .                                                                                                     */
/*                                                                                                                              */
/*              end.                                                                                                            */
/*              return no-apply.                                                                                                */
/*        end.                                                                                                                  */


       when "loc-bin" then do:
           run rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val)
                       rm-rctd.loc:screen-value = entry(2,char-val)
                     /*rm-rctd.qty:screen-value = entry(3,char-val)
                   rm-rctd.tag:screen-value = entry(4,char-val)*/
                     .
             
           end.
           return no-apply.   
       end.
       WHEN "tag" THEN DO:
           run windows/l-ldtag.w (g_company,no,focus:screen-value,output char-val,OUTPUT rec-val).
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
                 find first xfg-rdtlh where xfg-rdtlh.company   eq g_company
                      and xfg-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      and xfg-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
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
              {addon/loadtags/disptagr.i "FGItem" FOCUS:SCREEN-VALUE}
              IF rm-rctd.loc:SCREEN-VALUE = "" THEN RUN get-def-values.
/*
              lv-prev-job2 = rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no2 = rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
*/  
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
  /* {src/adm/template/brsleave.i} */
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


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE br_table /* Whs */
DO:
    IF LASTKEY = -1 THEN RETURN.

    DEF VAR v-locbin AS cha NO-UNDO.
    IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:

          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 rm-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).

          FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
          IF NOT AVAIL rm-bin THEN DO:
             MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO rm-rctd.loc .
             RETURN NO-APPLY.
          END.

          APPLY "leave" TO SELF.
          APPLY "tab" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
          /*APPLY "row-leave" TO BROWSE {&browse-name}.*/

          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
        FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE br_table /* Bin */
DO:
  IF LASTKEY = -1 THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search B-table-Win
ON LEAVE OF lv-search IN FRAME F-Main /* Auto Find */
or return of lv-search
DO:
        DEF VAR char-hdl AS cha NO-UNDO.

        assign browse-order
               lv-search.
        &scoped-define IAMWHAT Search
        &scoped-define where-statement begins lv-search
        case browse-order:
            {srtord2.i 1}
            /*{srtord2.i 2}  */
        end.     


        IF AVAIL rm-rctd OR NUM-RESULTS ("{&browse-name}") > 0 THEN DO:
           RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
           RUN run-update IN WIDGET-HANDLE(char-hdl).
        END.
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
       &Scope KEY-PHRASE rm-rctd.r-no eq INTEGER(key-value)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE can-exit B-table-Win 
PROCEDURE can-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-can-exit AS LOG NO-UNDO.

   op-can-exit = /*IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN YES ELSE NO.*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-search B-table-Win 
PROCEDURE do-search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-search AS cha NO-UNDO.

   DEF VAR char-hdl AS cha NO-UNDO.

   lv-search = ip-search.

        &scoped-define IAMWHAT Search
        &scoped-define where-statement begins lv-search
        /*
        case browse-order:
            {srtord2.i 1}
            /*{srtord2.i 2}  */
        end.     
        */


  &scoped-define where-statement begins lv-search
  &scoped-define key-phrase ({&fld-name-1}) {&Where-statement}

  {&open-query-{&browse-name}}  
  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? AND lv-search <> "" THEN DO:
      FIND FIRST loadtag WHERE loadtag.company = g_company
                           AND loadtag.item-type = YES
                           AND loadtag.misc-char[1] = lv-search
                         NO-LOCK NO-ERROR.
      IF AVAIL loadtag THEN DO:
          lv-search = loadtag.tag-no.
          {&open-query-{&browse-name}}
      END.
  END.

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? AND lv-search <> "" THEN
  DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  end.    
  IF NUM-RESULTS ("{&browse-name}") = 1 THEN DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
     RUN run-update IN WIDGET-HANDLE(char-hdl).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win
PROCEDURE local-assign-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN 
      rm-rctd.enteredBy = USERID("asi")
      rm-rctd.enteredDT = DATETIME(TODAY, MTIME)
      . 


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
  APPLY "entry" TO rm-rctd.loc IN BROWSE {&browse-name}.

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
  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN scan-next.
  
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
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"srch-source",OUTPUT char-hdl).
  RUN apply-entry IN WIDGET-HANDLE(char-hdl).

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
  {src/adm/template/sndkycas.i "r-no" "rm-rctd" "r-no"}
  {src/adm/template/sndkycas.i "company" "rm-rctd" "company"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST rm-bin WHERE rm-bin.company = g_company
                      AND rm-bin.i-no = ""
                      AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
  IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc-bin.
          RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

