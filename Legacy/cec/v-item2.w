&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec\v-item2.w

  Description: from VIEWER.W - Template for SmartViewer Objects

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
{methods/defines/hndlset.i}
{custom/gcompany.i}
{custom/gloc.i}

DEF VAR lv-override AS LOG NO-UNDO.
DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.
def var uom-list-con as cha init ["M,EA,L,CS,C"] no-undo.
def NEW SHARED var cocode as   char  format "x(3)"  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item.inv-by-cust item.loc item.loc-bin ~
item.alloc item.stocked item.pur-man item.cons-uom item.pur-uom ~
item.cc-code item.pic-code item.pur-rcode item.ord-policy item.vend-no ~
item.vend-item item.vend2-no item.vend2-item item.lead-days item.ord-level ~
item.ord-min item.ord-max 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS btnOnHand btnOnOrder btnCommitted RECT-21 ~
RECT-28 RECT-29 RECT-30 RECT-31 RECT-32 btnAvailable 
&Scoped-Define DISPLAYED-FIELDS item.inv-by-cust item.i-no item.i-name ~
item.loc item.loc-bin item.alloc item.stocked item.pur-man item.cons-uom ~
item.pur-uom item.cc-code item.pic-code item.pur-rcode item.ord-policy ~
item.vend-no item.vend-item item.vend2-no item.vend2-item item.lead-days ~
item.ord-level item.ord-min item.ord-max item.i-dscr item.beg-bal ~
item.beg-date item.last-count item.last-date item.last-cost item.avg-cost ~
item.q-onh item.q-ono item.q-comm item.q-back item.q-avail 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS lv-rolls-on-hand roll-label loc-dscr ~
lv-rolls-on-order lv-rolls-comm lv-rolls-back lv-rolls-avail 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define List-3 item.beg-bal item.beg-date item.last-count ~
item.last-date item.last-cost item.avg-cost item.q-onh item.q-ono ~
item.q-comm item.q-back item.q-avail 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvailable 
     LABEL "Available" 
     SIZE 18 BY 1.

DEFINE BUTTON btnCommitted 
     LABEL "Committed" 
     SIZE 18 BY 1.

DEFINE BUTTON btnOnHand 
     LABEL "On Hand" 
     SIZE 18 BY 1.

DEFINE BUTTON btnOnOrder 
     LABEL "On Order" 
     SIZE 18 BY 1.

DEFINE VARIABLE loc-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rolls-avail AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rolls-back AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rolls-comm AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rolls-on-hand AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rolls-on-order AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE roll-label AS CHARACTER FORMAT "X(256)":U INITIAL "Rolls" 
      VIEW-AS TEXT 
     SIZE 5.6 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 3.62.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 2.62.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 4.52.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 3.33.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 3.33.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 3.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     item.inv-by-cust AT ROW 4.81 COL 32 WIDGET-ID 2
          LABEL "Zero On Hand"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY 1 TOOLTIP "When Checked, O/H quantity will be set to Zero"
     lv-rolls-on-hand AT ROW 17.05 COL 18 COLON-ALIGNED NO-LABEL
     roll-label AT ROW 17.05 COL 13.6 NO-LABEL
     btnOnHand AT ROW 15.05 COL 20
     item.i-no AT ROW 1.48 COL 15 COLON-ALIGNED
          LABEL "Item#" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     item.i-name AT ROW 1.48 COL 32.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     item.loc AT ROW 1.48 COL 88 COLON-ALIGNED
          LABEL "Warehouse" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     item.loc-bin AT ROW 2.43 COL 88 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     item.alloc AT ROW 3.86 COL 14
          LABEL "Auto Allocate?"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1
     item.stocked AT ROW 4.81 COL 14
          LABEL "Stocked?"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY 1
     item.pur-man AT ROW 5.76 COL 14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Purchased", yes,
"Manufactured", no
          SIZE 39 BY .95
     item.cons-uom AT ROW 3.86 COL 78 COLON-ALIGNED
          LABEL "Consumption UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     item.pur-uom AT ROW 5.29 COL 78 COLON-ALIGNED
          LABEL "Qty Purchased UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     item.cc-code AT ROW 3.86 COL 113.4 COLON-ALIGNED
          LABEL "Cycle Code"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     item.pic-code AT ROW 4.81 COL 113.4 COLON-ALIGNED
          LABEL "Prod. Code"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     item.pur-rcode AT ROW 5.76 COL 113.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     item.ord-policy AT ROW 7.43 COL 42 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Reorder Point", yes,
"Lot Controlled", no
          SIZE 45 BY 1.19
     item.vend-no AT ROW 8.62 COL 32 COLON-ALIGNED
          LABEL "Vendor 1"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     item.vend-item AT ROW 8.62 COL 60 COLON-ALIGNED
          LABEL "Item#"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     item.vend2-no AT ROW 9.57 COL 32 COLON-ALIGNED
          LABEL "Vendor 2"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     item.vend2-item AT ROW 9.57 COL 60 COLON-ALIGNED
          LABEL "Item#"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     item.lead-days AT ROW 10.52 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     item.ord-level AT ROW 8.38 COL 104.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     item.ord-min AT ROW 9.33 COL 104.4 COLON-ALIGNED
          LABEL "Minimum"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     item.ord-max AT ROW 10.29 COL 104.4 COLON-ALIGNED
          LABEL "Maximum"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     item.i-dscr AT ROW 2.43 COL 15 COLON-ALIGNED
          LABEL "Item Desc"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     item.beg-bal AT ROW 12.19 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     item.beg-date AT ROW 13.14 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     item.last-count AT ROW 12.19 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     item.last-date AT ROW 13.14 COL 69 COLON-ALIGNED
          LABEL "Count Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     loc-dscr AT ROW 1.48 COL 99 COLON-ALIGNED NO-LABEL
     item.last-cost AT ROW 12.19 COL 107.8 COLON-ALIGNED
          LABEL "Last Cost"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     item.avg-cost AT ROW 13.14 COL 107.8 COLON-ALIGNED
          LABEL "Average Cost"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     item.q-onh AT ROW 16 COL 18 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     item.q-ono AT ROW 16 COL 38 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     item.q-comm AT ROW 16 COL 58 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     item.q-back AT ROW 16 COL 78 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     item.q-avail AT ROW 16 COL 98 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     btnOnOrder AT ROW 15.05 COL 40
     btnCommitted AT ROW 15.05 COL 60
     lv-rolls-on-order AT ROW 17.05 COL 37.8 COLON-ALIGNED NO-LABEL
     lv-rolls-comm AT ROW 17.05 COL 57.8 COLON-ALIGNED NO-LABEL
     lv-rolls-back AT ROW 17.05 COL 77.8 COLON-ALIGNED NO-LABEL
     lv-rolls-avail AT ROW 17.05 COL 98 COLON-ALIGNED NO-LABEL
     btnAvailable AT ROW 15.05 COL 100 WIDGET-ID 4
     "Reorder Policy:" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 7.43 COL 22
          FGCOLOR 9 
     "Qty" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 16 COL 14
          FGCOLOR 9 
     "Backordered" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 15.05 COL 80
     RECT-21 AT ROW 14.81 COL 13
     RECT-28 AT ROW 11.95 COL 13
     RECT-29 AT ROW 7.19 COL 13
     RECT-30 AT ROW 3.62 COL 91
     RECT-31 AT ROW 3.62 COL 13
     RECT-32 AT ROW 3.62 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.item
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.62
         WIDTH              = 142.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX item.alloc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.avg-cost IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN item.beg-bal IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN item.beg-date IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN item.cc-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.cons-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN item.i-dscr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN item.i-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item.i-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR TOGGLE-BOX item.inv-by-cust IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.last-cost IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN item.last-count IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN item.last-date IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN item.loc IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN item.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-rolls-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-rolls-back IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-rolls-comm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-rolls-on-hand IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-rolls-on-order IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item.ord-max IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.ord-min IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.pic-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.pur-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN item.q-avail IN FRAME F-Main
   NO-ENABLE 3 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN item.q-back IN FRAME F-Main
   NO-ENABLE 3 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN item.q-comm IN FRAME F-Main
   NO-ENABLE 3 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN item.q-onh IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN item.q-ono IN FRAME F-Main
   NO-ENABLE 3 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN roll-label IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       roll-label:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX item.stocked IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.vend-item IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.vend-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.vend2-item IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.vend2-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var char-val as cha no-undo.


    case focus:name :
        when "cons-uom"  then do:
             run sys/ref/uom-rm2.p  (item.mat-type, output uom-list-con).
             run windows/l-stduom.w (gcompany,uom-list-con, item.cons-uom:SCREEN-VALUE in frame {&frame-name}, output char-val).
         /*    run windows/l-uom.w (focus:screen-value, output char-val).     display all Uom */
             if char-val <> "" then 
                assign item.cons-uom:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when  "pur-uom" then do:
             run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
             run windows/l-stduom.w (gcompany,uom-list, ITEM.pur-uom:SCREEN-VALUE in frame {&frame-name}, output char-val).
         /*    run windows/l-uom.w (focus:screen-value, output char-val).     display all Uom */
             if char-val <> "" then 
                assign ITEM.pur-uom:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "vend-no" then do:
             run windows/l-vendno.w (gcompany, "", ITEM.vend-no:SCREEN-VALUE in frame {&frame-name}, output char-val).
             if char-val <> "" then 
                assign ITEM.vend-no:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "vend2-no" then do:
             run windows/l-vendno.w (gcompany, "", ITEM.vend2-no:SCREEN-VALUE in frame {&frame-name}, output char-val).
             if char-val <> "" then 
                assign ITEM.vend2-no:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "loc" then do:
             run windows/l-loc.w (gcompany,ITEM.loc:SCREEN-VALUE in frame {&frame-name}, output char-val).
             if char-val <> "" then 
                assign ITEM.loc:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "loc-bin" then do:

             run windows/l-locbin.w (gcompany,item.loc:SCREEN-VALUE in frame {&frame-name},ITEM.loc-bin:SCREEN-VALUE in frame {&frame-name}, output char-val).
             if char-val <> "" then 
                assign ITEM.loc-bin:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.

    end.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvailable V-table-Win
ON CHOOSE OF btnAvailable IN FRAME F-Main /* Available */
DO:
  RUN rm/w-rminq.w ('RM Balance Inquiry','rminq/b-rmcoinq.w',ROWID(item)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCommitted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCommitted V-table-Win
ON CHOOSE OF btnCommitted IN FRAME F-Main /* Committed */
DO:
  RUN rm/w-rminq.w ('RM Committed Inquiry','rminq/b-rmainq.w',ROWID(item)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnHand
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnHand V-table-Win
ON CHOOSE OF btnOnHand IN FRAME F-Main /* On Hand */
DO:
  RUN rm/w-rminq.w ('RM On Hand Inquiry','browsers/rm-ibin.w',ROWID(item)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnOrder V-table-Win
ON CHOOSE OF btnOnOrder IN FRAME F-Main /* On Order */
DO:
  RUN rm/w-rminq.w ('RM On Order Inquiry','browsers/item-pos.w',ROWID(item)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.cons-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.cons-uom V-table-Win
ON LEAVE OF item.cons-uom IN FRAME F-Main /* Consumption UOM */
DO:
    run sys/ref/uom-rm2.p  (item.mat-type, output uom-list-con).       
    if lastkey <> -1 and self:screen-value <> "" and
      /* not can-find(uom where uom.uom = self:screen-value)  */
      LOOKUP(SELF:SCREEN-VALUE,uom-list-con) <= 0
    then do:
    {&methods/lValidateError.i YES}
       message "Invalid UOM. Try Help." view-as alert-box error.
       return no-apply.
    {&methods/lValidateError.i NO}
    end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.loc V-table-Win
ON LEAVE OF item.loc IN FRAME F-Main /* Warehouse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.loc-bin V-table-Win
ON LEAVE OF item.loc-bin IN FRAME F-Main /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.ord-policy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.ord-policy V-table-Win
ON return OF item.ord-policy IN FRAME F-Main /* Reorder Policy Code */
DO:
    apply "tab" to self.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.pur-man V-table-Win
ON return OF item.pur-man IN FRAME F-Main /* Purchased or Manf */
DO:
  apply "tab" to self.
    return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.pur-uom V-table-Win
ON LEAVE OF item.pur-uom IN FRAME F-Main /* Qty Purchased UOM */
DO:
    run sys/ref/uom-rm.p  (item.mat-type, output uom-list).       
    if lastkey <> -1 and self:screen-value <> "" and
      /* not can-find(uom where uom.uom = self:screen-value)  */
       LOOKUP(SELF:SCREEN-VALUE,uom-list) <= 0
    then do:
    {&methods/lValidateError.i YES}
       message "Invalid UOM. Try Help." view-as alert-box error.
       return no-apply.
    {&methods/lValidateError.i NO}
    end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{sys/inc/f3help.i}
{custom/getloc.i}
ASSIGN
session:data-entry-return = YES
cocode = gcompany.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-bin V-table-Win 
PROCEDURE display-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first rm-bin  where rm-bin.company eq gcompany
        and rm-bin.i-no    eq item.i-no use-index i-no
      no-lock no-error.
  if avail rm-bin then do:
    run rm/rm-ibin.p.
  end. /* avail rm-bin */
  else do:
   message " There are no available BIN records. " view-as alert-box error.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-override THEN  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&list-3}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

    lv-override = NO.
    DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.         /*Task# 10221304*/ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  def var out-hdl as cha no-undo.
  DEF VAR lv-rolls-hide-var AS LOG INIT YES NO-UNDO.
  DEF VAR lv-cons-uom AS CHAR NO-UNDO.
  DEF VAR lv-basis-w LIKE ITEM.basis-w NO-UNDO.
  DEF VAR lv-r-wid LIKE ITEM.r-wid NO-UNDO.
  DEF VAR lv-s-len LIKE ITEM.s-len NO-UNDO.
  DEF VAR lv-s-wid LIKE ITEM.s-wid NO-UNDO.
  DEF VAR lv-s-dep LIKE ITEM.s-dep NO-UNDO.
  DEF VAR lv-q-onh LIKE ITEM.q-onh NO-UNDO.
  DEF VAR lv-q-ono LIKE ITEM.q-ono NO-UNDO.
  DEF VAR lv-q-comm LIKE ITEM.q-comm NO-UNDO.
  DEF VAR lv-q-back LIKE ITEM.q-back NO-UNDO.
  DEF VAR lv-q-avail LIKE ITEM.q-back NO-UNDO.
  DEF VAR v-roll-multp AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR lv-rm-bin-qty LIKE rm-bin.qty NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     IF AVAIL ITEM AND ITEM.mat-type EQ "P" THEN
     DO:
        ASSIGN
          lv-rolls-hide-var = NO
          lv-cons-uom = ITEM.cons-uom
          lv-basis-w  = ITEM.basis-w
          lv-r-wid    = ITEM.r-wid
          lv-s-len    = ITEM.s-len
          lv-s-wid    = ITEM.s-wid
          lv-s-dep    = ITEM.s-dep.

        IF lv-r-wid > 0 THEN
           ASSIGN
             lv-q-ono    = ITEM.q-ono
             lv-q-comm   = ITEM.q-comm
             lv-q-back   = ITEM.q-back.
     END.

     ASSIGN
        roll-label:HIDDEN = lv-rolls-hide-var
        lv-rolls-on-hand:HIDDEN = lv-rolls-hide-var
        lv-rolls-on-order:HIDDEN = lv-rolls-hide-var
        lv-rolls-comm:HIDDEN = lv-rolls-hide-var
        lv-rolls-back:HIDDEN = lv-rolls-hide-var
        lv-rolls-avail:HIDDEN = lv-rolls-hide-var.
  END.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* local-view - no item avail on first view, so change page from here in case */ 
  if avail item and item.i-code = "E" then do:
     run get-link-handle in adm-broker-hdl (this-procedure, "container-source", output char-hdl).  
     RUN get-attribute IN widget-handle(char-hdl) ('Current-Page':U).
     if int(return-value) = 5 then do:     

        run select-page in widget-handle(char-hdl) (6).
        return no-apply.
     end.   
  end.   

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF NOT lv-rolls-hide-var THEN
  DO:
     IF lv-s-len EQ 0 THEN
     DO:
        FIND FIRST uom WHERE
             uom.uom = "ROLL"
             NO-LOCK NO-ERROR.

        IF AVAIL uom THEN
        DO:
           v-roll-multp = uom.mult.
           RELEASE uom.
        END.
     END.

     IF lv-r-wid > 0 THEN
     DO:
        IF AVAIL ITEM THEN
           FOR EACH rm-bin FIELDS(tag qty) WHERE
               rm-bin.company = item.company AND
               rm-bin.i-no = item.i-no AND
               rm-bin.i-no NE " "
               NO-LOCK:

               IF rm-bin.tag NE "" THEN
                  lv-q-onh = lv-q-onh + 1.
               ELSE
               DO:
                  IF lv-cons-uom NE "LF" THEN
                     RUN sys/ref/convquom.p(lv-cons-uom, "LF", lv-basis-w,
                                            12,
                                            lv-r-wid,
                                            lv-s-dep,                    
                                            rm-bin.qty, OUTPUT lv-rm-bin-qty).
                  ELSE
                     lv-rm-bin-qty = rm-bin.qty.

                  IF lv-s-len NE 0 THEN
                  DO:
                     lv-rm-bin-qty = lv-rm-bin-qty / lv-s-len.
                     {sys/inc/roundup.i lv-rm-bin-qty}
                     lv-q-onh = lv-q-onh + lv-rm-bin-qty.
                  END.
                  ELSE IF v-roll-multp NE 0 THEN
                  DO:
                     lv-rm-bin-qty = lv-rm-bin-qty / v-roll-multp.
                     {sys/inc/roundup.i lv-rm-bin-qty}
                     lv-q-onh = lv-q-onh + lv-rm-bin-qty.
                  END.
               END.
           END.

        IF lv-cons-uom NE "LF" THEN
        DO:
           RUN sys/ref/convquom.p(lv-cons-uom, "LF", lv-basis-w,
                                  12,
                                  lv-r-wid,
                                  lv-s-dep,                    
                                  lv-q-ono, OUTPUT lv-q-ono).

           RUN sys/ref/convquom.p(lv-cons-uom, "LF", lv-basis-w,
                                  12,
                                  lv-r-wid,
                                  lv-s-dep,                    
                                  lv-q-comm, OUTPUT lv-q-comm).

           RUN sys/ref/convquom.p(lv-cons-uom, "LF", lv-basis-w,
                                  12,
                                  lv-r-wid,
                                  lv-s-dep,                    
                                  lv-q-back, OUTPUT lv-q-back).
        END.
     END.

     IF lv-s-len NE 0 THEN
     DO:
        ASSIGN
          lv-q-ono = lv-q-ono / lv-s-len
          lv-q-comm = lv-q-comm / lv-s-len
          lv-q-back = lv-q-back / lv-s-len.

        {sys/inc/roundup.i lv-q-ono}
        {sys/inc/roundup.i lv-q-comm}
        {sys/inc/roundup.i lv-q-back}

     END.
     ELSE
     DO:
        IF v-roll-multp NE 0 THEN
        DO:
           ASSIGN
            lv-q-ono = lv-q-ono / v-roll-multp
            lv-q-comm = lv-q-comm / v-roll-multp
            lv-q-back = lv-q-back / v-roll-multp.

          {sys/inc/roundup.i lv-q-ono}
          {sys/inc/roundup.i lv-q-comm}
          {sys/inc/roundup.i lv-q-back}
        END.
        ELSE
           ASSIGN
             lv-q-ono = 0
             lv-q-comm = 0
             lv-q-back = 0.
     END.

     ASSIGN
        lv-rolls-on-hand:SCREEN-VALUE = STRING(lv-q-onh)
        lv-rolls-on-order:SCREEN-VALUE = STRING(lv-q-ono)
        lv-rolls-comm:SCREEN-VALUE = STRING(lv-q-comm)
        lv-rolls-back:SCREEN-VALUE = STRING(lv-q-back)
        lv-rolls-avail:SCREEN-VALUE = STRING(lv-q-onh + lv-q-ono - lv-q-comm).
  END.

  IF AVAIL ITEM THEN
     ASSIGN
        ITEM.q-onh:SCREEN-VALUE = STRING(TRUNCATE(ITEM.q-onh,2))
        ITEM.q-ono:SCREEN-VALUE = STRING(TRUNCATE(ITEM.q-ono,2))
        ITEM.q-comm:SCREEN-VALUE = STRING(TRUNCATE(ITEM.q-comm,2))
        ITEM.q-back:SCREEN-VALUE = STRING(TRUNCATE(ITEM.q-back,2))
        ITEM.q-avail:SCREEN-VALUE = STRING(TRUNCATE(ITEM.q-avail,2)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-override THEN DO:
     ENABLE {&list-3} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE do with frame {&frame-name} :

     if item.cons-uom = "" or ( 
          item.q-onh = 0 and item.q-ono = 0 and item.q-comm = 0 and
          item.q-back = 0 )
     then do: end. 
     else disable item.cons-uom.
     if (item.pur-uom = "" or (item.last-cost = 0 and item.avg-cost = 0 ) )
     then do:      end.
     else disable item.pur-uom.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  do with frame {&frame-name} :
    run sys/ref/uom-rm.p  (item.mat-type, output uom-list).   
    run sys/ref/uom-rm2.p  (item.mat-type, output uom-list-con).
    if item.cons-uom:screen-value <> "" and
      /* not can-find(uom where uom.uom = self:screen-value)  */
       LOOKUP(ITEM.cons-uom:SCREEN-VALUE,uom-list-con) <= 0
    then do:
    {&methods/lValidateError.i YES}
       message "Invalid UOM. Try Help." view-as alert-box error.
       APPLY "entry" TO ITEM.cons-uom.
       return no-apply.
    {&methods/lValidateError.i NO}
    end.

    if item.pur-uom:screen-value <> "" and
      /* not can-find(uom where uom.uom = self:screen-value)  */
       LOOKUP(ITEM.pur-uom:SCREEN-VALUE,uom-list) <= 0
    then do:
    {&methods/lValidateError.i YES}
       message "Invalid UOM. Try Help." view-as alert-box error.
       APPLY "entry" TO ITEM.pur-uom.
       return no-apply.
    {&methods/lValidateError.i NO}
    end.
  end.  /* do with frame  */

  IF item.q-onh:SENSITIVE AND
     ITEM.mat-type EQ "P" AND ITEM.r-wid > 0 THEN
     RUN update-rolls-proc.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-override = NO.
  DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view V-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  DEF BUFFER b-item FOR ITEM.

  /* Code placed here will execute PRIOR to standard behavior. */
 /* === not for corrugeted item   
  /* first view does not have item record info - page changed from local-display-fields */
  if avail item and item.i-code = "E" then do:
     run get-link-handle in adm-broker-hdl (this-procedure, "container-source", output char-hdl).
     run select-page in widget-handle(char-hdl) (6).

     return no-apply .
  end.   
*/  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE override V-table-Win 
PROCEDURE override :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
        DEF VAR lResult AS LOG NO-UNDO.
        RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
        RUN epCanAccess IN hPgmSecurity ("cec/v-item2.w", "", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.

IF lResult THEN
      ll-secure = YES .

  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).

  IF ll-secure THEN DO:
    lv-override = YES.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN run-update IN WIDGET-HANDLE(char-hdl) .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-qty V-table-Win 
PROCEDURE recalc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN rm/d-rmrqty.w (ROWID(item), YES).

  FIND CURRENT item NO-LOCK.
  RUN dispatch IN THIS-PROCEDURE ("display-fields").

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "bin-target", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-rolls-proc V-table-Win 
PROCEDURE update-rolls-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-q-onh LIKE ITEM.q-onh NO-UNDO.
   DEF VAR lv-q-ono LIKE ITEM.q-ono NO-UNDO.
   DEF VAR lv-q-comm LIKE ITEM.q-comm NO-UNDO.
   DEF VAR lv-q-back LIKE ITEM.q-back NO-UNDO.
   DEF VAR lv-q-avail LIKE ITEM.q-avail NO-UNDO.
   DEF VAR v-roll-multp AS DEC DECIMALS 4 NO-UNDO.

   /*not updating on hand roll quantity because it looks at rm-bin
     for its initial rolls calculation*/

   DO WITH FRAME {&FRAME-NAME}:

      IF item.cons-uom NE "LF" THEN
      DO:
          RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                12,
                                item.r-wid,
                                item.s-dep,                    
                                DEC(item.q-ono:SCREEN-VALUE), OUTPUT lv-q-ono).

          RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                 12,
                                 item.r-wid,
                                 item.s-dep,                    
                                 DEC(item.q-comm:SCREEN-VALUE), OUTPUT lv-q-comm).

          RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                 12,
                                 item.r-wid,
                                 item.s-dep,                    
                                 DEC(item.q-back:SCREEN-VALUE), OUTPUT lv-q-back).
      END.
      ELSE
        ASSIGN
           lv-q-ono = DEC(ITEM.q-ono:SCREEN-VALUE)
           lv-q-comm = DEC(ITEM.q-comm:SCREEN-VALUE)
           lv-q-back = DEC(ITEM.q-back:SCREEN-VALUE).

      IF item.s-len NE 0 THEN
      DO:
         ASSIGN
           lv-q-ono = lv-q-ono / item.s-len
           lv-q-comm = lv-q-comm / item.s-len
           lv-q-back = lv-q-back / item.s-len.

         {sys/inc/roundup.i lv-q-ono}
         {sys/inc/roundup.i lv-q-comm}
         {sys/inc/roundup.i lv-q-back}
      END.
      ELSE
      DO:
         FIND FIRST uom WHERE
              uom.uom = "ROLL"
              NO-LOCK NO-ERROR.

         IF AVAIL uom THEN
         DO:
            v-roll-multp = uom.mult.
            RELEASE uom.
         END.

         IF v-roll-multp NE 0 THEN
         DO:
            ASSIGN
             lv-q-ono = lv-q-ono / v-roll-multp
             lv-q-comm = lv-q-comm / v-roll-multp
             lv-q-back = lv-q-back / v-roll-multp.

           {sys/inc/roundup.i lv-q-ono}
           {sys/inc/roundup.i lv-q-comm}
           {sys/inc/roundup.i lv-q-back}
         END.
         ELSE
            ASSIGN
              lv-q-ono = 0
              lv-q-comm = 0
              lv-q-back = 0.
      END.

      ASSIGN
         lv-rolls-on-order:SCREEN-VALUE = STRING(lv-q-ono)
         lv-rolls-comm:SCREEN-VALUE = STRING(lv-q-comm)
         lv-rolls-back:SCREEN-VALUE = STRING(lv-q-back)
         lv-rolls-avail:SCREEN-VALUE = STRING(DEC(lv-rolls-on-hand:SCREEN-VALUE) +
                                              lv-q-ono - lv-q-comm).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF item.loc:SCREEN-VALUE NE ""                                 AND
       NOT CAN-FIND(FIRST rm-bin
                    WHERE rm-bin.company EQ item.company
                      AND rm-bin.i-no    EQ ""
                      AND rm-bin.loc     EQ item.loc:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(item.loc:SCREEN-VALUE) + 
              " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item.loc.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin V-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF item.loc-bin:SCREEN-VALUE NE ""                                 AND
       NOT CAN-FIND(FIRST rm-bin
                    WHERE rm-bin.company EQ item.company
                      AND rm-bin.i-no    EQ ""
                      AND rm-bin.loc     EQ item.loc:SCREEN-VALUE
                      AND rm-bin.loc-bin EQ item.loc-bin:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(item.loc-bin:SCREEN-VALUE) + 
              " is invalid for warehouse: " +
              TRIM(item.loc:SCREEN-VALUE) +
              ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item.loc-bin.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

