&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\d-invitm.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def input param ip-recid as recid no-undo.
def input param ip-r-no like inv-head.r-no no-undo.
def input param ip-type as cha no-undo .   /* add,update,view */

DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR cp-rowid AS ROWID NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared} 
assign cocode = g_company
       locode = g_loc.
define new shared var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */
define new shared var v-i-item like oe-ordl.i-no no-undo. /* INPUT ITEM */
define new shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */
define new shared var price-ent as log NO-UNDO.
define new shared var fil_id as recid no-undo.
DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.
DEF VAR lv-item-recid AS RECID NO-UNDO.
DEF VAR lv-cancel-update AS LOG NO-UNDO.
def VAR lv-uom-list as cha init "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" no-undo.
DEF VAR v-msg AS CHAR NO-UNDO.
DEF VAR v-print-head LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR glInvQtyChanged AS LOG NO-UNDO.
DEF NEW SHARED BUFFER xinv-line FOR inv-line.
DEF NEW SHARED BUFFER xinv-head FOR inv-head.

DEF TEMP-TABLE w-inv-line NO-UNDO LIKE inv-line.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPRINT"
    no-lock no-error.
if avail sys-ctrl then
  ASSIGN
   v-print-head = sys-ctrl.log-fld
   v-print-fmt  = sys-ctrl.char-fld.

DEF VAR fg-uom-list AS cha NO-UNDO.
RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).
{oe/oe-sysct1.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inv-line

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame inv-line.ord-no inv-line.po-no ~
inv-line.job-no inv-line.job-no2 inv-line.est-no inv-line.i-no ~
inv-line.part-no inv-line.i-name inv-line.qty inv-line.part-dscr1 ~
inv-line.ship-qty inv-line.part-dscr2 inv-line.inv-qty inv-line.price ~
inv-line.pr-uom inv-line.sman[1] inv-line.sname[1] inv-line.comm-amt[1] ~
inv-line.cost inv-line.cas-cnt inv-line.sman[2] inv-line.sname[2] ~
inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] inv-line.sname[3] ~
inv-line.comm-amt[3] inv-line.tax inv-line.t-price 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame inv-line.ord-no ~
inv-line.po-no inv-line.job-no inv-line.job-no2 inv-line.est-no ~
inv-line.i-no inv-line.part-no inv-line.i-name inv-line.qty ~
inv-line.part-dscr1 inv-line.ship-qty inv-line.part-dscr2 inv-line.inv-qty ~
inv-line.price inv-line.sman[1] inv-line.comm-amt[1] inv-line.cost ~
inv-line.sman[2] inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] ~
inv-line.comm-amt[3] inv-line.tax 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame inv-line
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame inv-line
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH inv-line SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH inv-line SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame inv-line
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame inv-line


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-line.ord-no inv-line.po-no ~
inv-line.job-no inv-line.job-no2 inv-line.est-no inv-line.i-no ~
inv-line.part-no inv-line.i-name inv-line.qty inv-line.part-dscr1 ~
inv-line.ship-qty inv-line.part-dscr2 inv-line.inv-qty inv-line.price ~
inv-line.sman[1] inv-line.comm-amt[1] inv-line.cost inv-line.sman[2] ~
inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] inv-line.comm-amt[3] ~
inv-line.tax 
&Scoped-define ENABLED-TABLES inv-line
&Scoped-define FIRST-ENABLED-TABLE inv-line
&Scoped-Define ENABLED-OBJECTS RECT-39 RECT-40 btn_ok btn_done btn_cancel 
&Scoped-Define DISPLAYED-FIELDS inv-line.ord-no inv-line.po-no ~
inv-line.job-no inv-line.job-no2 inv-line.est-no inv-line.i-no ~
inv-line.part-no inv-line.i-name inv-line.qty inv-line.part-dscr1 ~
inv-line.ship-qty inv-line.part-dscr2 inv-line.inv-qty inv-line.price ~
inv-line.pr-uom inv-line.sman[1] inv-line.sname[1] inv-line.comm-amt[1] ~
inv-line.cost inv-line.cas-cnt inv-line.sman[2] inv-line.sname[2] ~
inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] inv-line.sname[3] ~
inv-line.comm-amt[3] inv-line.tax inv-line.t-price 
&Scoped-define DISPLAYED-TABLES inv-line
&Scoped-define FIRST-DISPLAYED-TABLE inv-line
&Scoped-Define DISPLAYED-OBJECTS lv-bolno fi_comm-lbl

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTaxable Dialog-Frame
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
  (ipcCompany AS CHARACTER,
   ipcCust AS CHARACTER,
   ipcShipto AS CHARACTER,
   ipcFGItemID AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_done AUTO-GO 
     LABEL "&Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-bolno AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bol#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 4.52.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 139 BY 12.14.

DEFINE VARIABLE fi_comm-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Comm $" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .71
     FGCOLOR 9  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      inv-line SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     inv-line.ord-no AT ROW 1.48 COL 10 COLON-ALIGNED FORMAT ">>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     lv-bolno AT ROW 1.48 COL 34 COLON-ALIGNED
     inv-line.po-no AT ROW 1.48 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     inv-line.job-no AT ROW 1.48 COL 95 COLON-ALIGNED
          LABEL "Job#"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     inv-line.job-no2 AT ROW 1.48 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     inv-line.est-no AT ROW 1.48 COL 122 COLON-ALIGNED
          LABEL "Est#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     inv-line.i-no AT ROW 3.14 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     inv-line.part-no AT ROW 3.14 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     inv-line.i-name AT ROW 4.1 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.qty AT ROW 4.57 COL 25 COLON-ALIGNED
          LABEL "Qty Order" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     inv-line.part-dscr1 AT ROW 5.05 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.ship-qty AT ROW 5.52 COL 25 COLON-ALIGNED
          LABEL "Qty Ship" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     inv-line.part-dscr2 AT ROW 6 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.inv-qty AT ROW 6.48 COL 25 COLON-ALIGNED
          LABEL "Qty Invoice" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     inv-line.price AT ROW 8.62 COL 25 COLON-ALIGNED FORMAT "->>,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.pr-uom AT ROW 8.62 COL 59 COLON-ALIGNED
          LABEL "UOM"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     inv-line.sman[1] AT ROW 8.86 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     inv-line.sname[1] AT ROW 8.86 COL 88 NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     inv-line.comm-amt[1] AT ROW 8.86 COL 115 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.cost AT ROW 9.57 COL 25 COLON-ALIGNED
          LABEL "Cost"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.cas-cnt AT ROW 9.57 COL 59 COLON-ALIGNED
          LABEL "Case" FORMAT ">>>,>>>"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     inv-line.sman[2] AT ROW 9.81 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     inv-line.sname[2] AT ROW 9.81 COL 88 NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     inv-line.comm-amt[2] AT ROW 9.86 COL 115 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.disc AT ROW 10.52 COL 25 COLON-ALIGNED FORMAT ">>9.99%"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.sman[3] AT ROW 10.76 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
     inv-line.sname[3] AT ROW 10.76 COL 88 NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     inv-line.comm-amt[3] AT ROW 10.86 COL 115 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.tax AT ROW 11 COL 54
          LABEL "Taxable"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY 1
     inv-line.t-price AT ROW 11.48 COL 25 COLON-ALIGNED
          LABEL "Ext. Price"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     btn_ok AT ROW 14.33 COL 27
     btn_done AT ROW 14.33 COL 63
     btn_cancel AT ROW 14.33 COL 100
     "Code" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 8.14 COL 81
          FGCOLOR 9 
     /*"Comm $" VIEW-AS TEXT
          SIZE 11 BY .62*/ fi_comm-lbl NO-LABEL AT ROW 8.14 COL 117
          FGCOLOR 9 
     "Sales Rep Name" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 8.14 COL 88 WIDGET-ID 8
          FGCOLOR 9 
     "SalesRep" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 7.43 COL 82
          FGCOLOR 9 
     "/M" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.57 COL 47
     RECT-39 AT ROW 7.67 COL 79
     RECT-40 AT ROW 1 COL 1
     SPACE(0.59) SKIP(3.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Invoice Item Detail".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN inv-line.cas-cnt IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[1] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[2] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[3] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.cost IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.disc IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.est-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-line.inv-qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-line.job-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lv-bolno IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-line.ord-no IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.pr-uom IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN inv-line.price IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inv-line.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-line.ship-qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inv-line.sname[1] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN inv-line.sname[2] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN inv-line.sname[3] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN inv-line.t-price IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX inv-line.tax IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_comm-lbl IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.inv-line"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Invoice Item Detail */
DO:
     def var char-val as cha no-undo.
     def var look-recid as recid no-undo. 
    
     FIND FIRST inv-head WHERE inv-head.company = g_company 
                           AND inv-head.r-no = inv-line.r-no NO-LOCK NO-ERROR.
                   
     case focus:name :
          when "est-no" then do:
               run windows/l-estcst.w (g_company,g_loc,inv-head.cust-no,0,focus:screen-value, output char-val).
               if char-val <> "" then do:
                  run display-est-detail (char-val).
               end.                
          end.   
          when "i-no" then do:
               run windows/l-itemfa.w (g_company, inv-head.cust-no, focus:screen-value, output char-val, output look-recid).
               if entry(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO:
                  focus:screen-value = entry(1,char-val).
                  APPLY "value-changed" TO inv-line.i-no. 
               end.                           
          end.
          when "part-no" then do:
               run windows/l-cstprt.w (g_company, inv-head.cust-no, focus:screen-value, inv-line.i-no:screen-value, output char-val, output look-recid).
               if char-val <> "" then do:
                  assign inv-line.part-no:screen-value = entry(1,char-val)
                         inv-line.part-dscr1:screen-value = entry(2,char-val)
                         inv-line.part-dscr2:screen-value = entry(3,char-val).
                   IF inv-line.i-no:SCREEN-VALUE = "" OR inv-line.i-no:SCREEN-VALUE = "0" 
                      THEN inv-line.i-no:SCREEN-VALUE = ENTRY(4,char-val).
                      apply "entry" to inv-line.part-no.
               end.             
          end.
          when "s-man" then do:
             run windows/l-sman.w (g_company, output char-val).
             if char-val <> "" then do:
                case focus:index:
                     when 1 then assign inv-line.sman[1]:screen-value = entry(1,char-val)
                                        inv-line.sname[1]:screen-value = entry(2,char-val) 
                                        inv-line.comm-amt[1]:screen-value = entry(3,char-val)
                                        .
                     when 2 then assign inv-line.sman[2]:screen-value = entry(1,char-val)
                                        inv-line.sname[2]:screen-value = entry(2,char-val)
                                        inv-line.comm-amt[2]:screen-value = entry(3,char-val)
                                        .
                     when 3 then assign inv-line.sman[3]:screen-value = entry(1,char-val)
                                        inv-line.sname[3]:screen-value = entry(2,char-val) 
                                        inv-line.comm-amt[3]:screen-value = entry(3,char-val)
                                        .
                end.
             end.
         end.  
          /*
          when "price" then do:       /* oe/history2.p */              
               run windows/l-report.w (g_company,oe-ord.cust-no,inv-line.i-no:screen-value, output char-val).
               if char-val <> "" then focus:screen-value = entry(1,char-val).         
          end.*/
          when "pr-uom" then do:
               run windows/l-stduom.w (g_company,lv-uom-list,inv-line.pr-uom:screen-value, output char-val).
               if char-val <> "" then focus:screen-value = entry(1,char-val).         
          end.
          when "po-no" then do:
               run windows/l-ponopo.w (g_company,yes,focus:screen-value, output char-val).
               if char-val <> "" then assign focus:screen-value = entry(1,char-val)
                                             .
          end.

     end case.
     return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Item Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cancel Dialog-Frame
ON CHOOSE OF btn_cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    if lv-item-recid <> ? then do:
       find inv-line where recid(inv-line) = lv-item-recid  no-error.
       if avail inv-line then DELETE inv-line.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok Dialog-Frame
ON CHOOSE OF btn_ok IN FRAME Dialog-Frame /* Save */
DO:
  DEF VAR v-uom LIKE fg-bin.pur-uom NO-UNDO.
  DEF VAR lv-cost LIKE inv-line.t-cost NO-UNDO.
  DEF VAR ll-canceled AS LOG NO-UNDO.
  DEF VAR lv-hld-po-no LIKE inv-line.po-no NO-UNDO.
  DEF VAR xInvQtyPrev LIKE inv-line.inv-qty NO-UNDO.

  DEF BUFFER bf-oe-ordl FOR oe-ordl.


  DISABLE TRIGGERS FOR LOAD OF inv-line.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FIND CURRENT inv-line.

  EMPTY TEMP-TABLE w-inv-line.

  CREATE w-inv-line.
  BUFFER-COPY inv-line TO w-inv-line.

  upd-blok: DO TRANSACTION.
    lv-hld-po-no = inv-line.po-no.
    xInvQtyPrev = inv-line.inv-qty.
    DO WITH FRAME {&frame-name}:
      ASSIGN {&FIELDS-IN-QUERY-{&frame-name}}.
    END.

    IF inv-line.ord-no EQ 0 THEN DO:
      /*FIND FIRST oe-bolh
          WHERE oe-bolh.b-no EQ inv-line.b-no
            AND oe-bolh.printed
            AND oe-bolh.posted
          NO-ERROR.

      IF AVAIL oe-bolh THEN DO:
        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:
          DELETE oe-boll.
        END.
        DELETE oe-bolh.
      END. */

      RUN oe/d-invbol.w (ROWID(inv-line), OUTPUT ll-canceled).

      IF ll-canceled THEN DO WITH FRAME {&FRAME-NAME}:
        IF TRUE THEN UNDO upd-blok.
        BUFFER-COPY w-inv-line TO inv-line.
        APPLY "choose" TO btn_cancel.
      END.

      IF inv-line.cost EQ ? THEN inv-line.cost = 0.
      /* wfk - 06031202 - If they haven't canceled, recalc cost */
      IF TRUE /*inv-line.cost EQ 0 THEN */ THEN DO:
        inv-line.t-cost = 0.

        FOR EACH oe-boll
            WHERE oe-boll.company EQ inv-line.company
              AND oe-boll.b-no    EQ inv-line.b-no
              AND oe-boll.i-no    EQ inv-line.i-no
              AND oe-boll.line    EQ inv-line.line
            NO-LOCK,

            FIRST fg-bin
            WHERE fg-bin.company EQ oe-boll.company
              AND fg-bin.i-no    EQ oe-boll.i-no
              AND fg-bin.job-no  EQ oe-boll.job-no
              AND fg-bin.job-no2 EQ oe-boll.job-no2
              AND fg-bin.loc     EQ oe-boll.loc
              AND fg-bin.loc-bin EQ oe-boll.loc-bin
              AND fg-bin.tag     EQ oe-boll.tag
            NO-LOCK,
        
            FIRST itemfg
            WHERE itemfg.company EQ fg-bin.company
              AND itemfg.i-no    EQ fg-bin.i-no
            NO-LOCK:

          v-uom = fg-bin.pur-uom.

          IF v-uom EQ "" THEN v-uom = itemfg.prod-uom.
    
          IF v-uom EQ "M" THEN
            lv-cost = fg-bin.std-tot-cost.
          ELSE 
            RUN sys/ref/convcuom.p(v-uom, "M", 0, 0, 0, 0,
                                   fg-bin.std-tot-cost, OUTPUT lv-cost).

          inv-line.t-cost = inv-line.t-cost + (lv-cost * (oe-boll.qty / 1000)).
        END.

        ASSIGN
         inv-line.t-cost = inv-line.t-cost / inv-line.ship-qty * inv-line.inv-qty
         inv-line.cost   = inv-line.t-cost / (inv-line.inv-qty / 1000).

        IF inv-line.cost EQ ? THEN inv-line.cost = 0.
      END.
    END.

    ELSE DO: 
        /* Change Customer PO on BOLs & releases if changed on invoice */
        IF inv-line.po-no NE lv-hld-po-no THEN
            RUN oe/invlinpo.p (BUFFER inv-line, lv-hld-po-no).
        /*update invoice qty on orderline*/
        IF glInvQtyChanged THEN DO:
            FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ inv-line.company
                AND bf-oe-ordl.ord-no EQ inv-line.ord-no
                AND bf-oe-ordl.i-no EQ inv-line.i-no
                AND bf-oe-ordl.LINE EQ inv-line.LINE
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bf-oe-ordl THEN 
                bf-oe-ordl.inv-qty = bf-oe-ordl.inv-qty - xInvQtyPrev + INT(inv-line.inv-qty:SCREEN-VALUE).
            FIND CURRENT bf-oe-ordl NO-LOCK NO-ERROR.
        END.
    END.
  END.

  SESSION:SET-WAIT-STATE("general").

  RUN oe/oe-invup.p (ROWID(inv-head), INPUT NO).

  SESSION:SET-WAIT-STATE("").

  APPLY "go" TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.disc Dialog-Frame
ON VALUE-CHANGED OF inv-line.disc IN FRAME Dialog-Frame /* Discount */
DO:
  {oe/ordltot.i inv-line inv-qty}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.i-no Dialog-Frame
ON LEAVE OF inv-line.i-no IN FRAME Dialog-Frame /* Item# */
DO :

  IF LASTKEY NE -1 THEN DO WITH FRAME Dialog-Frame:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    DEF VAR li AS INT NO-UNDO.

    FIND itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ inv-line.i-no:SCREEN-VALUE IN FRAME Dialog-Frame
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      ASSIGN
       inv-line.i-no:SCREEN-VALUE        = CAPS(itemfg.i-no)
       inv-line.i-name:SCREEN-VALUE     = itemfg.i-name
       inv-line.pr-uom:SCREEN-VALUE     = itemfg.sell-uom
       inv-line.part-dscr1:SCREEN-VALUE = itemfg.part-dscr1
       inv-line.part-dscr2:SCREEN-VALUE = itemfg.part-dscr2
       inv-line.part-no:SCREEN-VALUE    = itemfg.part-no.
  
      RUN enable-cost.
  
      ASSIGN
       cp-part-no = ""
       cp-rowid   = ROWID(itemfg).
      RUN custom/getcpart.p (cocode, inv-head.cust-no,
                             INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
      IF cp-part-no NE "" THEN inv-line.part-no:SCREEN-VALUE = cp-part-no.
      IF itemfg.CLASS EQ "*" OR itemfg.exempt-disc THEN inv-line.disc:SCREEN-VALUE = "0".
    
      inv-line.price:SCREEN-VALUE = STRING(itemfg.sell-price).
  
      IF itemfg.i-code EQ "S"          AND
         inv-line.cost:SENSITIVE EQ NO THEN
        inv-line.cost:SCREEN-VALUE = STRING(itemfg.total-std-cost).
      
      inv-line.tax:SCREEN-VALUE = STRING(fGetTaxable(inv-head.company, inv-head.cust-no, inv-head.sold-no, itemfg.i-no),"Y/N").
      APPLY "value-changed" TO inv-line.ship-qty.

      RUN itemfg-sman.
/*       DO li = 1 TO LENGTH(TRIM(itemfg.i-no:SCREEN-VALUE)): */
/*         APPLY "cursor-right" TO itemfg.i-no.               */
/*       END.                                                  */
/*      RUN display-item. */
    END.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.i-no Dialog-Frame
ON VALUE-CHANGED OF inv-line.i-no IN FRAME Dialog-Frame /* Item# */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll-tax LIKE inv-line.tax NO-UNDO.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.inv-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.inv-qty Dialog-Frame
ON LEAVE OF inv-line.inv-qty IN FRAME Dialog-Frame /* Qty Invoice */
DO:
  IF LASTKEY NE - 1 THEN DO:
    IF {&self-name}:MODIFIED THEN DO:
      IF DEC(inv-line.ord-no:SCREEN-VALUE) NE 0 THEN DO:
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ inv-line.company
              AND oe-ord.ord-no  EQ inv-line.ord-no
            NO-LOCK NO-ERROR.
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ inv-line.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no
              AND oe-ordl.i-no    EQ inv-line.i-no
            NO-LOCK NO-ERROR.
        IF (oe-ordl.t-inv-qty + DEC(inv-line.inv-qty:SCREEN-VALUE)) GT
           (inv-line.qty + ROUND((inv-line.qty * oe-ordl.over-pct) / 100 ,2)) THEN
          MESSAGE "Total quantity invoiced will exceed the over run quantity..."
                  VIEW-AS ALERT-BOX WARNING.
      END.

      IF DEC(inv-line.inv-qty:SCREEN-VALUE) NE
         DEC(inv-line.ship-qty:SCREEN-VALUE) THEN DO:
        MESSAGE "Invoice quantity is not the same as ship quantity, "
                "do you wish to continue?"
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE v-sure AS LOG.
        IF NOT v-sure THEN RETURN NO-APPLY.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.inv-qty Dialog-Frame
ON VALUE-CHANGED OF inv-line.inv-qty IN FRAME Dialog-Frame /* Qty Invoice */
DO:
  {oe/ordltot.i inv-line inv-qty}
  glInvQtyChanged = YES. /*if there is an order, the new ship qty 
                    should be applied to ordl - 05281303*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.part-no Dialog-Frame
ON LEAVE OF inv-line.part-no IN FRAME Dialog-Frame /* Cust Part # */
DO :
  
  IF LASTKEY NE -1 THEN DO WITH FRAME Dialog-Frame:
   IF inv-line.part-no:SCREEN-VALUE <> "" THEN do:
    DEF VAR li AS INT NO-UNDO.
    
      find first itemfg where itemfg.company = g_company 
                          and itemfg.part-no = inv-line.part-no:screen-value
                          and itemfg.i-no = inv-line.i-no:screen-value
                          and itemfg.cust-no = inv-line.cust-no
                          no-lock no-error.
     
      IF NOT AVAIL itemfg THEN
      find first itemfg where itemfg.company = g_company 
                          and itemfg.part-no = inv-line.part-no:screen-value
                          and itemfg.cust-no = inv-line.cust-no
                          no-lock no-error.
      if not avail itemfg then 
         find first itemfg where itemfg.company = g_company 
                          and itemfg.part-no = inv-line.part-no:screen-value
                          no-lock no-error.
      IF AVAIL itemfg THEN DO:
      ASSIGN
       inv-line.i-no:SCREEN-VALUE        = CAPS(itemfg.i-no)
       inv-line.i-name:SCREEN-VALUE     = itemfg.i-name
       inv-line.pr-uom:SCREEN-VALUE     = itemfg.sell-uom
       inv-line.part-dscr1:SCREEN-VALUE = itemfg.part-dscr1
       inv-line.part-dscr2:SCREEN-VALUE = itemfg.part-dscr2
       inv-line.part-no:SCREEN-VALUE    = itemfg.part-no.
  
      RUN enable-cost.
      
      inv-line.price:SCREEN-VALUE = STRING(itemfg.sell-price).
  
      IF itemfg.i-code EQ "S"          AND
         inv-line.cost:SENSITIVE EQ NO THEN
        inv-line.cost:SCREEN-VALUE = STRING(itemfg.total-std-cost).
  
      inv-line.tax:SCREEN-VALUE = STRING(fGetTaxable(inv-head.company, inv-head.cust-no, inv-head.sold-no, itemfg.i-no),"Y/N").
      
      APPLY "value-changed" TO inv-line.ship-qty.
/*       DO li = 1 TO LENGTH(TRIM(itemfg.i-no:SCREEN-VALUE)): */
/*         APPLY "cursor-right" TO itemfg.i-no.               */
/*       END.                                                  */
/*      RUN display-item. */
        END.
   END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.pr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.pr-uom Dialog-Frame
ON VALUE-CHANGED OF inv-line.pr-uom IN FRAME Dialog-Frame /* UOM */
DO:
  {oe/ordltot.i inv-line inv-qty}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.price Dialog-Frame
ON VALUE-CHANGED OF inv-line.price IN FRAME Dialog-Frame /* Price */
DO:
  {oe/ordltot.i inv-line inv-qty}
  price-ent = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.qty Dialog-Frame
ON LEAVE OF inv-line.qty IN FRAME Dialog-Frame /* Qty Order */
DO:
   {oe/ordltot.i inv-line qty}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-line.ship-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.ship-qty Dialog-Frame
ON LEAVE OF inv-line.ship-qty IN FRAME Dialog-Frame /* Qty Ship */
DO:
  IF LASTKEY NE - 1 THEN DO:
    IF {&self-name}:MODIFIED THEN DO:
      IF DEC(inv-line.ord-no:SCREEN-VALUE) NE 0 THEN DO:
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ inv-line.company
              AND oe-ord.ord-no  EQ inv-line.ord-no
            NO-LOCK NO-ERROR.
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ inv-line.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no
              AND oe-ordl.i-no    EQ inv-line.i-no
            NO-LOCK NO-ERROR.
        IF (oe-ordl.t-ship-qty + DEC(inv-line.ship-qty:SCREEN-VALUE)) GT
           (inv-line.qty + ROUND((inv-line.qty * oe-ordl.over-pct) / 100 ,2)) THEN
          MESSAGE "Total quantity shipped will exceed the over run quantity..."
                  VIEW-AS ALERT-BOX WARNING.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-line.ship-qty Dialog-Frame
ON VALUE-CHANGED OF inv-line.ship-qty IN FRAME Dialog-Frame /* Qty Ship */
DO:
  IF inv-line.ord-no EQ 0 THEN DO:
    ASSIGN
     inv-line.qty:SCREEN-VALUE     = {&self-name}:SCREEN-VALUE
     inv-line.inv-qty:SCREEN-VALUE = {&self-name}:SCREEN-VALUE
     inv-line.price.

    IF NOT price-ent THEN DO:
      ASSIGN
       fil_id   = RECID(inv-line)
       save_id  = fil_id
       v-i-item = inv-line.i-no:SCREEN-VALUE
       v-i-qty  = INT({&self-name}:SCREEN-VALUE).

      RUN oe/oe-ipric.p.

      FIND xinv-line WHERE ROWID(xinv-line) EQ ROWID(inv-line) NO-LOCK NO-ERROR.

      IF AVAIL xinv-line THEN 
        ASSIGN inv-line.price:SCREEN-VALUE = STRING(xinv-line.price)
               inv-line.pr-uom:SCREEN-VALUE = STRING(xinv-line.pr-uom).
      
    END.

    {oe/ordltot.i inv-line inv-qty}

  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i}
session:data-entry-return = yes.       
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  if ip-type = "add" then do:
     run create-item .
     find inv-line where recid(inv-line) = lv-item-recid no-error.
  end.
  else find inv-line where recid(inv-line) = ip-recid no-error.
  find inv-head of inv-line no-lock.
  FIND xinv-head OF inv-line NO-LOCK.

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&FIELDS-IN-QUERY-{&FRAME-NAME}}.

    RUN display-item.
    
    RUN oe/oe-sysct.p.
    
    IF ip-type EQ "view" THEN
      ASSIGN
       btn_done:HIDDEN    = NO
       btn_done:SENSITIVE = YES
       btn_ok:HIDDEN      = YES
       btn_cancel:HIDDEN  = YES.

    ELSE DO:
      ENABLE inv-line.po-no inv-line.disc.

      IF inv-head.tax-gr NE "" THEN ENABLE inv-line.tax.

      IF inv-line.sman[1] NE "" THEN ENABLE inv-line.comm-amt[1].
      IF inv-line.sman[2] NE "" THEN ENABLE inv-line.comm-amt[2].
      IF inv-line.sman[3] NE "" THEN ENABLE inv-line.comm-amt[3].

      IF inv-line.ord-no EQ 0 THEN DO:
        ENABLE inv-line.ship-qty
               inv-line.part-dscr1
               inv-line.part-dscr2.

        IF ip-type EQ "add" THEN
          ENABLE inv-line.i-no
                 inv-line.part-no
                 inv-line.i-name.
      END.

      IF (inv-line.stat EQ "I" OR inv-line.stat EQ "B") THEN DO:
        ENABLE inv-line.price.
        IF inv-line.ord-no NE 0 THEN ENABLE inv-line.inv-qty.
      END.

      IF ip-type EQ "add" THEN DO:
        ENABLE inv-line.ship-qty
               inv-line.price.
        IF inv-line.ord-no NE 0 THEN ENABLE inv-line.inv-qty.
      END.
      btn_done:HIDDEN = YES.

      RUN enable-cost.
    END.
  END.
      IF NOT v-oecomm-log  THEN RUN hide-comm (YES).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-line AS INT NO-UNDO.

FIND inv-head WHERE inv-head.company = g_company 
               AND inv-head.r-no = ip-r-no NO-LOCK NO-ERROR.
v-line = 0.
for each inv-line of inv-head no-lock by inv-line.line:
  v-line = inv-line.line.
end.
v-line = v-line + 1.

create inv-line.
lv-item-recid = RECID(inv-line).
assign inv-line.r-no       = inv-head.r-no
       inv-line.line       = v-line
       inv-line.company    = inv-head.company
       inv-line.i-no       = ""
       inv-line.cust-no    = inv-head.cust-no
       inv-line.ord-date   = inv-head.inv-date
       .
find first cust of inv-line no-lock.

assign inv-line.sman[1] = caps(cust.sman)
       inv-line.disc    = cust.disc
       inv-line.tax     = cust.sort eq "Y" and inv-head.tax-gr ne "".

inv-line.tax = fGetTaxable(inv-head.company, inv-head.cust-no, inv-head.sold-no, inv-line.i-no).

find first sman where sman.company eq cust.company
             and sman.sman    eq cust.sman
    no-lock no-error.

if avail sman then do:
  inv-line.sname[1] = sman.sname.

  find first sman-mtx
      where sman-mtx.company eq cocode
        and sman-mtx.sman    eq sman.sman
        and sman-mtx.custype eq cust.type
      no-lock no-error.

  if avail sman-mtx then
    inv-line.s-comm[1] = if avail sman-mtx then sman-mtx.type-comm
                                           else sman.scomm.
end.

else do:
  find first custype where custype.custype eq cust.type no-lock no-error.

  if avail custype then
    inv-line.s-comm[1] = custype.commrate.
  else do:
    find ce-ctrl {sys/look/ce-ctrlW.i} no-lock.
    inv-line.s-comm[1] = ce-ctrl.comm-mrkup.
  end.
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  lv-bolno = "".
  

  IF AVAILABLE inv-line THEN DO:

     DISPLAY inv-line.ord-no inv-line.po-no inv-line.job-no inv-line.job-no2 
          inv-line.est-no inv-line.i-no inv-line.part-no inv-line.i-name 
          inv-line.qty inv-line.part-dscr1 inv-line.ship-qty inv-line.part-dscr2 
          inv-line.inv-qty inv-line.price inv-line.pr-uom inv-line.sman[1] 
          inv-line.sname[1] inv-line.comm-amt[1] inv-line.cost inv-line.cas-cnt inv-line.sman[2] 
          inv-line.sname[2] inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] inv-line.sname[3] 
          inv-line.comm-amt[3] inv-line.t-price inv-line.tax fi_comm-lbl
          WITH FRAME Dialog-Frame.

     FIND FIRST oe-bolh WHERE oe-bolh.company = inv-line.company AND
                              oe-bolh.b-no = inv-line.b-no NO-LOCK NO-ERROR.
     IF AVAIL oe-bolh THEN lv-bolno = string(oe-bolh.bol-no,">>>>>>>9").
     DISPLAY lv-bolno WITH FRAME {&FRAME-NAME}.

  END.
  enable btn_ok btn_cancel with frame {&frame-name}.    
  VIEW FRAME {&frame-name}.
  apply "entry" to frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-cost Dialog-Frame 
PROCEDURE enable-cost PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ inv-line.i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF inv-line.ord-no EQ 0 AND AVAIL itemfg AND itemfg.pur-man AND INT(itemfg.est-no) EQ 0 THEN
      ENABLE inv-line.cost.
    ELSE
      DISABLE inv-line.cost.
  END.

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
  DISPLAY lv-bolno 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE inv-line THEN 
    DISPLAY inv-line.ord-no inv-line.po-no inv-line.job-no inv-line.job-no2 
          inv-line.est-no inv-line.i-no inv-line.part-no inv-line.i-name 
          inv-line.qty inv-line.part-dscr1 inv-line.ship-qty inv-line.part-dscr2 
          inv-line.inv-qty inv-line.price inv-line.pr-uom inv-line.sman[1] 
          inv-line.sname[1] inv-line.comm-amt[1] inv-line.cost inv-line.cas-cnt 
          inv-line.sman[2] inv-line.sname[2] inv-line.comm-amt[2] inv-line.disc 
          inv-line.sman[3] inv-line.sname[3] inv-line.comm-amt[3] inv-line.tax 
          inv-line.t-price 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-39 RECT-40 inv-line.ord-no inv-line.po-no inv-line.job-no 
         inv-line.job-no2 inv-line.est-no inv-line.i-no inv-line.part-no 
         inv-line.i-name inv-line.qty inv-line.part-dscr1 inv-line.ship-qty 
         inv-line.part-dscr2 inv-line.inv-qty inv-line.price inv-line.sman[1] 
         inv-line.comm-amt[1] inv-line.cost inv-line.sman[2] 
         inv-line.comm-amt[2] inv-line.disc inv-line.sman[3] 
         inv-line.comm-amt[3] inv-line.tax btn_ok btn_done btn_cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemfg-sman Dialog-Frame 
PROCEDURE itemfg-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cNewRep AS CHARACTER   NO-UNDO.
  DEF BUFFER bf-inv-head FOR inv-head.
  DEF BUFFER bf-oe-ord FOR oe-ord.

  IF NOT AVAIL inv-head THEN
    FIND inv-head NO-LOCK WHERE inv-head.company EQ cocode
      AND inv-head.inv-no  EQ inv-line.inv-no
    NO-ERROR.

  /* if order is from an estimate, use the esitmate sales rep # */
  IF inv-line.est-no GT "" THEN
    RETURN.

  DO WITH FRAME {&FRAME-NAME}:
    
   
    FIND FIRST itemfg
        WHERE itemfg.company = g_company
          AND itemfg.i-no = inv-line.i-no:screen-value
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN
      RETURN.

    RUN fg/fgSlsRep.p (INPUT cocode,
                   INPUT inv-head.cust-no,
                   INPUT inv-line.part-no:screen-value,
                   INPUT itemfg.i-no,
                   OUTPUT cNewRep).
    FIND cust WHERE cust.company EQ cocode
      AND cust.cust-no EQ inv-head.cust-no
      NO-LOCK NO-ERROR.
    IF cNewRep GT "" AND cNewRep NE inv-line.sman[1]:SCREEN-VALUE THEN DO:

     inv-line.sman[1]:SCREEN-VALUE = cNewRep. 
          find first sman where sman.company eq inv-head.company
                   and sman.sman    eq cNewRep
          no-lock no-error.
      
      if avail sman AND AVAIL cust then do:
        inv-line.sname[1] = sman.sname.
      
        find first sman-mtx
            where sman-mtx.company eq cocode
              and sman-mtx.sman    eq sman.sman
              and sman-mtx.custype eq cust.type
            no-lock no-error.
      
        if avail sman-mtx then
          inv-line.s-comm[1] = if avail sman-mtx then sman-mtx.type-comm
                                                 else sman.scomm.
      END.
    END.
      
    IF cNewRep GT "" AND cNewRep NE inv-head.sman[1] THEN DO:

      /* Update the header with the new sales rep */
      FIND bf-inv-head WHERE ROWID(bf-inv-head) EQ ROWID(inv-head)
         EXCLUSIVE-LOCK NO-ERROR.
     
      IF AVAIL bf-inv-head THEN DO:
        bf-inv-head.sman[1] = cNewRep.
      END. /* fi avail header */
      RELEASE bf-inv-head.
    END. /* if new rep found */
  END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-total Dialog-Frame 
PROCEDURE update-total :
/*------------------------------------------------------------------------------
  Purpose:      oe/oe-invup.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER xinv-head FOR inv-head.
  DEF BUFFER xinv-line FOR inv-line.
  def var v-line               like inv-line.line.
  def var hold_invoice_id      as   recid no-undo.
  def var v-cred-lim           as   log no-undo.
  def var v-tax                as   DEC NO-UNDO.
  DEF VAR v-basis LIKE sman.commbasis NO-UNDO.


  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ inv-head.company NO-LOCK NO-ERROR.

  v-cred-lim = inv-head.bol-no eq 0 and inv-head.terms ne "CASH" and
               not program-name(2) begins "oe/oe-bolp3.".

  find first cust where cust.company = inv-head.company
                    and cust.cust-no = inv-head.cust-no.
  find xinv-head of inv-line.   
  if (inv-head.bol-no ne 0                       and
      not program-name(2) begins "oe/oe-invde."  and
      not program-name(2) begins "oe/oe-bolp3.") or
      v-cred-lim                                 THEN cust.ord-bal = cust.ord-bal - xinv-head.t-inv-rev.

  ASSIGN xinv-head.t-inv-tax  = 0
         xinv-head.t-inv-cost = 0
         xinv-head.t-comm     = 0
         xinv-head.t-inv-rev  = 0
         v-line               = 0.


  /* Find the tax record. */
  FIND FIRST stax WHERE 
        stax.company = cocode AND
        stax.tax-group = xinv-head.tax-gr NO-LOCK NO-ERROR.

  for each xinv-line of xinv-head:
    find first itemfg
        {sys/look/itemfgrlW.i}
          and itemfg.i-no eq xinv-line.i-no
        no-lock no-error.

    assign
     save_id = recid(xinv-line)
     v-i-item = xinv-line.i-no
     v-i-qty = xinv-line.ship-qty.

    if xinv-line.pr-uom begins "L" AND xinv-line.pr-uom NE "LB" then
      xinv-line.t-price = xinv-line.price *
                         if xinv-line.inv-qty lt 0 then -1 else 1.

    else
    if xinv-line.pr-uom eq "CS" and avail itemfg then 
      xinv-line.t-price = xinv-line.inv-qty /
                         (if xinv-line.cas-cnt ne 0 then
                            xinv-line.cas-cnt
                          else
                          if avail itemfg and itemfg.case-count ne 0 then
                            itemfg.case-count else 1) *
                         xinv-line.price.
    
    ELSE IF LOOKUP(xinv-line.pr-uom,fg-uom-list) GT 0 THEN
        xinv-line.t-price = xinv-line.inv-qty * xinv-line.price.
    else do:
      find first uom
          where uom.uom  eq xinv-line.pr-uom
            and uom.mult ne 0
          no-lock no-error.
      xinv-line.t-price = xinv-line.inv-qty /
                         (if avail uom then uom.mult else 1000) *
                         xinv-line.price.
    end.
   
    IF xinv-line.disc NE 0 THEN
      xinv-line.t-price = 
          IF v-print-fmt EQ "Dayton" THEN 
            (xinv-line.t-price - ROUND(xinv-line.t-price * xinv-line.disc / 100,2))
          ELSE
            ROUND(xinv-line.t-price * (1 - (xinv-line.disc / 100)),2).

    /* Calculate tax for header record. */
    if xinv-line.tax then do:

        RUN ar/calctax2.p (xinv-head.tax-gr,NO,xinv-line.t-price,xinv-head.company,xinv-line.i-no,OUTPUT v-tax).
/*       /* Determine which tax program to run (standard or varied tax calculation). */        */
/*       /* If the itemfg varied tax flag is set... */                                         */
/*       IF AVAIL itemfg AND itemfg.spare-char-2 = "YES" AND                                   */
/*           /* And the dollar limit is setup in tax code 5... */                              */
/*           AVAIL stax AND stax.tax-code1[5] = "" AND stax.tax-dscr1[5] = "Dollar Limit" AND  */
/*                stax.tax-rate1[5] > 0 AND                                                    */
/*           /* and the invoice price exceeds the dollar limit... */                           */
/*           xinv-line.t-price > stax.tax-rate1[5] THEN                                        */
/*           /* then run the varied tax rate calculation program. */                           */
/*           RUN ar/calcvtax.p (xinv-head.tax-gr, no,xinv-line.t-price, output v-tax).         */
/*       /* Else run the standard tax rate calculation program. */                             */
/*       ELSE                                                                                  */
/*           RUN ar/calctax.p (xinv-head.tax-gr, no,xinv-line.t-price, output v-tax).          */
    
      /* Accumulate tax to invoice header record. */
      ASSIGN xinv-head.t-inv-tax = (xinv-head.t-inv-tax + v-tax).
    END.
      
    assign
     xinv-head.t-inv-cost  = xinv-head.t-inv-cost +
                             round((xinv-line.cost *
                                   (xinv-line.inv-qty / 1000)),2)
     xinv-head.t-comm      = xinv-head.t-comm +
                             xinv-line.comm-amt[1] +
                             xinv-line.comm-amt[2] +
                             xinv-line.comm-amt[3]
     xinv-head.t-inv-rev   = xinv-head.t-inv-rev + xinv-line.t-price
     v-line                = v-line + 1.
  end.

  FOR EACH inv-misc of xinv-head where inv-misc.bill eq "Y" NO-LOCK:
    IF inv-misc.tax THEN DO:

        RUN ar/calctax2.p (inv-head.tax-gr,NO,inv-misc.amt,inv-head.company,inv-misc.inv-i-no,OUTPUT v-tax).
/*         /* Find itemfg. */                                                                    */
/*         FIND FIRST itemfg NO-LOCK WHERE                                                       */
/*             itemfg.company = inv-misc.company AND                                             */
/*             itemfg.i-no = inv-misc.inv-i-no NO-ERROR.                                         */
/*         /* Determine which tax program to run (standard or varied tax calculation). */        */
/*         /* If the itemfg varied tax flag is set... */                                         */
/*         IF AVAIL itemfg AND itemfg.spare-char-2 = "YES" AND                                   */
/*             /* And the dollar limit is setup in tax code 5... */                              */
/*             AVAIL stax AND stax.tax-code1[5] = "" AND stax.tax-dscr1[5] = "Dollar Limit" AND  */
/*                   stax.tax-rate1[5] > 0 AND                                                   */
/*             /* and the invoice price exceeds the dollar limit... */                           */
/*             inv-misc.amt > stax.tax-rate1[5] THEN                                             */
/*             /* then run the varied tax rate calculation program. */                           */
/*             RUN ar/calcvtax.p (inv-head.tax-gr, no,inv-misc.amt, output v-tax).               */
/*                                                                                               */
/*         /* Else run the standard tax rate calculation program. */                             */
/*         ELSE                                                                                  */
/*             RUN ar/calctax.p (xinv-head.tax-gr, no,inv-misc.amt, output v-tax).               */

       ASSIGN xinv-head.t-inv-tax = xinv-head.t-inv-tax + v-tax.
    END. /* IF inv-misc.tax */

    ASSIGN xinv-head.t-inv-rev = xinv-head.t-inv-rev + inv-misc.amt.

    IF oe-ctrl.prep-comm THEN DO:
      FIND FIRST prep
          WHERE prep.company EQ inv-misc.company
            AND prep.code    EQ inv-misc.charge
          NO-LOCK NO-ERROR.
      DO k = 1 TO 3:
        IF inv-misc.s-man[k] NE "" THEN DO:
              RUN custom/combasis.p (xinv-head.company, inv-misc.s-man[k],
                                 (IF AVAIL cust THEN cust.type ELSE ""),
                                 (IF AVAIL prep THEN prep.fgcat ELSE ""), 0,
                                 (IF AVAIL cust THEN cust.cust-no ELSE ""),
                                 OUTPUT v-basis).

          ASSIGN xinv-head.t-comm = xinv-head.t-comm +
                   ROUND((inv-misc.amt - IF v-basis EQ "G" THEN inv-misc.cost ELSE 0) *
                         (inv-misc.s-pct[k] / 100) * (inv-misc.s-comm[k] / 100),2).
        END.
      END.
    END.
  END. /* FOR EACH inv-misc */
 
  if xinv-head.f-bill then do:
     run ar/calctax.p (xinv-head.tax-gr, yes,
                    xinv-head.t-inv-freight, output v-tax).
     ASSIGN xinv-head.t-inv-tax = xinv-head.t-inv-tax + v-tax
            xinv-head.t-inv-rev = xinv-head.t-inv-rev + xinv-head.t-inv-freight.
  end.
  xinv-head.t-inv-rev = xinv-head.t-inv-rev + xinv-head.t-inv-tax.
  
  /*if v-cred-lim and xinv-head.stat ne "H" then do:
     if cust.cr-hold-invdays gt 0 then do:
        run oe/creditid.p (input recid(cust), output hold_invoice_id).
        if hold_invoice_id ne ? then do:
           message "Customer has exceeded invoice age limit. Invoice will be put on HOLD."
                   VIEW-AS ALERT-BOX WARNING .
           xinv-head.stat = "H".
        end.
     end.
     ELSE if cust.acc-bal + cust.ord-bal + xinv-head.t-inv-rev gt cust.cr-lim then do:    
          message "Customer has exceeded credit limit. Invoice will be put on HOLD."
                   VIEW-AS ALERT-BOX WARNING .
          xinv-head.stat = "H".
     end.
     ELSE if cust.ord-bal + xinv-head.t-inv-rev gt cust.ord-lim then do:
          message "WARNING: Customer has exceeded order limit. Invoice will be put on HOLD."
                  VIEW-AS ALERT-BOX WARNING.
          xinv-head.stat = "H".
     end.
  end.
    
  if (inv-head.bol-no ne 0                       and
      not program-name(2) begins "oe/oe-invde."  and
      not program-name(2) begins "oe/oe-bolp3.") or
      v-cred-lim                                 THEN  cust.ord-bal = cust.ord-bal + xinv-head.t-inv-rev.*/

  IF v-cred-lim AND xinv-head.stat NE "H" THEN
    RUN oe/creditck.p (ROWID(xinv-head), NO).

  FIND CURRENT inv-line.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Dialog-Frame 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-new-i-no LIKE inv-line.i-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF inv-line.i-no:SCREEN-VALUE EQ "" THEN v-msg = "Item may not be spaces".

    IF v-msg EQ "" AND ip-type EQ "add" THEN DO:
        
      FIND FIRST xinv-line
          WHERE xinv-line.r-no   EQ inv-line.r-no
            AND xinv-line.b-no   EQ inv-line.b-no
            AND xinv-line.i-no   EQ inv-line.i-no:SCREEN-VALUE
            AND xinv-line.ord-no EQ INT(inv-line.ord-no:SCREEN-VALUE)
            AND xinv-line.line   EQ inv-line.line
            AND xinv-line.po-no  EQ inv-line.po-no
            AND ROWID(xinv-line) NE ROWID(inv-line)
          NO-LOCK NO-ERROR.
     
      IF AVAIL xinv-line THEN v-msg = "Item has already been entered in this invoice".
    END.

    IF v-msg EQ "" THEN DO:
      FIND FIRST itemfg
          {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ inv-line.i-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL itemfg THEN DO:
        RUN custom/getobitm.p (cocode, inv-line.i-no:SCREEN-VALUE, YES,
                               OUTPUT lv-new-i-no).
        IF lv-new-i-no EQ "" THEN v-msg = "Invalid entry, try help".
        ELSE
        IF lv-new-i-no EQ FILL("?",30) THEN v-msg = lv-new-i-no.
        ELSE DO:
          inv-line.i-no:SCREEN-VALUE = lv-new-i-no.
          APPLY "value-changed" TO inv-line.i-no.
        END.
      END.
    END.

    IF v-msg NE "" THEN DO:
      IF v-msg NE lv-new-i-no THEN MESSAGE v-msg + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO inv-line.i-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-comm V-table-Win 
PROCEDURE hide-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-hidden AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        fi_comm-lbl:HIDDEN IN FRAME {&FRAME-NAME}  = ip-hidden 
        inv-line.comm-amt[1]:HIDDEN IN FRAME {&FRAME-NAME}  = ip-hidden
        inv-line.comm-amt[2]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden
        inv-line.comm-amt[3]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTaxable Dialog-Frame
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
  ( ipcCompany AS CHARACTER, ipcCust AS CHARACTER , ipcShipto AS CHARACTER, ipcFGItemID AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Gets the Taxable flag based on inputs
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

RUN system\TaxProcs.p (ipcCompany, ipcCust, ipcShipto, ipcFGItemID, OUTPUT lTaxable).  
RETURN lTaxable.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


