&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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
{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i NEW SHARED}

def var old-part-no#    like itemfg.part-no no-undo.
def var old-i-name      like itemfg.i-name no-undo.
def var old-part-dscr1  like itemfg.part-dscr1 no-undo.
def var old-die-no      like itemfg.die-no no-undo.
def var old-plate-no    like itemfg.plate-no no-undo.
def var old-cad-no      like itemfg.cad-no no-undo.
def var old-spc-no      like itemfg.spc-no no-undo.
def var old-upc-no      like itemfg.upc-no no-undo.
def var old-procat      like itemfg.procat no-undo.
def var uom-list as cha init "C,CS,EA,L,M" no-undo.

&SCOPED DEFINE itemfg-maint itemfg-maint

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.i-no itemfg.part-no itemfg.i-name ~
itemfg.part-dscr1 itemfg.part-dscr2 itemfg.part-dscr3 itemfg.est-no ~
itemfg.style itemfg.style-desc itemfg.die-no itemfg.plate-no itemfg.cad-no ~
itemfg.spc-no itemfg.upc-no itemfg.isaset itemfg.pur-man itemfg.i-code ~
itemfg.cust-no itemfg.cust-name itemfg.sell-price itemfg.sell-uom ~
itemfg.prod-uom itemfg.procat itemfg.procat-desc itemfg.def-loc ~
itemfg.def-loc-bin itemfg.case-count itemfg.weight-100 itemfg.prod-notes ~
itemfg.class itemfg.cc-code itemfg.prod-code itemfg.std-mat-cost ~
itemfg.std-lab-cost itemfg.std-var-cost itemfg.std-fix-cost ~
itemfg.total-std-cost itemfg.avg-cost itemfg.last-cost 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS rd_status RECT-10 RECT-6 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-FIELDS itemfg.i-no itemfg.part-no itemfg.i-name ~
itemfg.part-dscr1 itemfg.part-dscr2 itemfg.part-dscr3 itemfg.est-no ~
itemfg.style itemfg.style-desc itemfg.die-no itemfg.plate-no itemfg.cad-no ~
itemfg.spc-no itemfg.upc-no itemfg.isaset itemfg.pur-man itemfg.i-code ~
itemfg.cust-no itemfg.cust-name itemfg.sell-price itemfg.sell-uom ~
itemfg.prod-uom itemfg.procat itemfg.procat-desc itemfg.def-loc ~
itemfg.def-loc-bin itemfg.case-count itemfg.weight-100 itemfg.prod-notes ~
itemfg.class itemfg.cc-code itemfg.prod-code itemfg.std-mat-cost ~
itemfg.std-lab-cost itemfg.std-var-cost itemfg.std-fix-cost ~
itemfg.total-std-cost itemfg.avg-cost itemfg.last-cost 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-Define DISPLAYED-OBJECTS rd_status tb_taxable fi_curr-code 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS tb_taxable 
&Scoped-define DISPLAY-FIELD tb_taxable 

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
DEFINE VARIABLE fi_curr-code AS CHARACTER FORMAT "X(3)" 
     LABEL "Currency" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE rd_status AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Active", "A",
"InActive", "I"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 4.52.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 16.91.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 9.29.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 84 BY 7.62.

DEFINE VARIABLE tb_taxable AS LOGICAL INITIAL no 
     LABEL "Taxable?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rd_status AT ROW 4.33 COL 115 NO-LABEL
     tb_taxable AT ROW 2.91 COL 127
     itemfg.i-no AT ROW 1.95 COL 16 COLON-ALIGNED
          LABEL "FG Item #"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     itemfg.part-no AT ROW 2.91 COL 16 COLON-ALIGNED
          LABEL "Cust Part #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     itemfg.i-name AT ROW 3.86 COL 16 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.part-dscr1 AT ROW 4.81 COL 16 COLON-ALIGNED
          LABEL "Desc 1"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.part-dscr2 AT ROW 5.76 COL 16 COLON-ALIGNED
          LABEL "Desc 2"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.part-dscr3 AT ROW 6.71 COL 16 COLON-ALIGNED
          LABEL "Desc 3"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.est-no AT ROW 9.1 COL 16 COLON-ALIGNED
          LABEL "Estimate#"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     itemfg.style AT ROW 10.05 COL 16 COLON-ALIGNED
          LABEL "Style"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     itemfg.style-desc AT ROW 11 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.die-no AT ROW 12.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     itemfg.plate-no AT ROW 13.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     itemfg.cad-no AT ROW 14.33 COL 16 COLON-ALIGNED
          LABEL "CAD#"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1.05
     itemfg.spc-no AT ROW 15.29 COL 16 COLON-ALIGNED
          LABEL "SPC/QC #"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     itemfg.upc-no AT ROW 16.24 COL 16 COLON-ALIGNED
          LABEL "UPC #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     itemfg.isaset AT ROW 1.71 COL 47
          LABEL "Set Header?"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY 1.19
     itemfg.pur-man AT ROW 1.71 COL 67 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Purchased", yes,
"Manufactured", no
          SIZE 39 BY 1.19
     itemfg.i-code AT ROW 1.71 COL 106 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Stock Box", "S":U,
"Custom Box", "C":U
          SIZE 35 BY 1.19
     itemfg.cust-no AT ROW 2.91 COL 65 COLON-ALIGNED
          LABEL "Cust#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg.cust-name AT ROW 2.91 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.sell-price AT ROW 6 COL 77 COLON-ALIGNED
          LABEL "Sell Price" FORMAT ">,>>>,>>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     itemfg.sell-uom AT ROW 6 COL 105 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     itemfg.prod-uom AT ROW 16.48 COL 123 COLON-ALIGNED
          LABEL "Cost UOM"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     itemfg.procat AT ROW 6.95 COL 77 COLON-ALIGNED
          LABEL "FG Category" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     itemfg.procat-desc AT ROW 6.95 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY .95
     itemfg.def-loc AT ROW 7.91 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     itemfg.def-loc-bin AT ROW 8.86 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     itemfg.case-count AT ROW 9.81 COL 77 COLON-ALIGNED
          LABEL "Count" FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     itemfg.weight-100 AT ROW 10.76 COL 77 COLON-ALIGNED
          LABEL "Wt per 100"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     itemfg.prod-notes AT ROW 11.71 COL 77 COLON-ALIGNED
          LABEL "Packing Note"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     itemfg.class AT ROW 8.14 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .95
     itemfg.cc-code AT ROW 9.1 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .95
     itemfg.prod-code AT ROW 10.05 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     itemfg.std-mat-cost AT ROW 13.62 COL 87 COLON-ALIGNED
          LABEL "Std Mat'l Cost"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     itemfg.std-lab-cost AT ROW 14.57 COL 87 COLON-ALIGNED
          LABEL "Std Labor Cost"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     itemfg.std-var-cost AT ROW 15.52 COL 87 COLON-ALIGNED
          LABEL "Std Var OH Cost"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     itemfg.std-fix-cost AT ROW 16.48 COL 87 COLON-ALIGNED
          LABEL "Std Fix OH Cost"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     itemfg.total-std-cost AT ROW 13.62 COL 123 COLON-ALIGNED
          LABEL "Total Std Cost"
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
     itemfg.avg-cost AT ROW 14.57 COL 123 COLON-ALIGNED
          LABEL "Average Cost" FORMAT ">>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     itemfg.last-cost AT ROW 15.52 COL 123 COLON-ALIGNED
          LABEL "Last Cost"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     fi_curr-code AT ROW 11.71 COL 131 COLON-ALIGNED
     RECT-10 AT ROW 13.38 COL 61
     RECT-6 AT ROW 1.24 COL 1
     RECT-8 AT ROW 8.62 COL 2
     RECT-9 AT ROW 5.52 COL 61
     "Status:" VIEW-AS TEXT
          SIZE 9 BY .95 AT ROW 4.33 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg
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
         HEIGHT             = 17.86
         WIDTH              = 144.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.avg-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.cad-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.case-count IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.cust-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.est-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_curr-code IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX itemfg.isaset IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.last-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-dscr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-dscr3 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.procat IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.prod-notes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.prod-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.sell-price IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.sell-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.spc-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.std-fix-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.std-lab-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.std-mat-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.std-var-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.style IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_taxable IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN itemfg.total-std-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.upc-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.weight-100 IN FRAME F-Main
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
   DEF VAR lv-prep-type AS cha NO-UNDO.

   case focus:name :
        when "sell-uom" then do:
             /*run sys/ref/uom-rm.p  (item.mat-type, output uom-list). */
             run windows/l-stduom.w (gcompany,uom-list, focus:screen-value, output char-val).
         /*    run windows/l-uom.w (focus:screen-value, output char-val).     display all Uom */
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "def-loc" then do:
             run windows/l-loc.w (gcompany,focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        WHEN "cust-no" THEN DO:
             RUN windows/l-cust.w (gcompany, FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
               FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
               APPLY "value-changed" TO FOCUS.
             END.
        END.
        when "def-loc-bin" then do:
             run windows/l-fgbin.w (gcompany,itemfg.def-loc:screen-value,focus:screen-value, output char-val).
      /*       run windows/l-locbin.w (gcompany,itemfg.def-loc:screen-value,focus:screen-value, output char-val).  
      */  
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        WHEN "est-no" THEN DO:
             RUN windows/l-fgest.w (gcompany, itemfg.cust-no:screen-value, FOCUS:SCREEN-VALUE, OUTPUT char-val).
             FIND FIRST eb WHERE RECID(eb) EQ INT(char-val) NO-LOCK NO-ERROR.
             IF AVAIL eb AND TRIM(FOCUS:SCREEN-VALUE) NE TRIM(eb.est-no) THEN DO:
               FOCUS:SCREEN-VALUE = eb.est-no.
               APPLY "value-changed" TO FOCUS.
             END. 
        END.
        when "procat" then do:
             run windows/l-fgcat.w (gcompany,focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       itemfg.procat-desc:screen-value = entry(2,char-val)
                       .
        end.
        when "style" then do:
             run windows/l-style.w (gcompany,focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       itemfg.style-desc:screen-value = entry(2,char-val)
                       .
        end.
        when "plate-no" or when "die-no" then do:
           /*run windows/l-matpr.w  (gcompany,focus:screen-value, output char-val).  */
           lv-prep-type = IF FOCUS:NAME = "Plate-no" THEN "P" ELSE "F,P,R".
           RUN windows/l-diepl.w (gcompany,lv-prep-type,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
       end.
    end.    


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.case-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count V-table-Win
ON LEAVE OF itemfg.case-count IN FRAME F-Main /* Count */
DO:
    if lastkey <> -1 and 
       (itemfg.i-code:screen-value = "S" and
         int(itemfg.case-count:screen-value) < 1 ) 
    then do:
         message "Case count can not less than ONE !!! " view-as alert-box error.
         return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.cust-no V-table-Win
ON LEAVE OF itemfg.cust-no IN FRAME F-Main /* Cust# */
do:
  if lastkey ne -1 then do:
    if itemfg.i-code:screen-value eq "C" and self:screen-value eq "" then do:
      message "Must enter a valid Customer#" view-as alert-box error.
      return no-apply.
    end.
    
    find first cust
        where cust.company      eq gcompany
          and cust.cust-no      eq self:screen-value
          and self:screen-value ne ""
        no-lock no-error.
    itemfg.cust-name:screen-value = if avail cust then cust.name else "". 
  end.
    
  if itemfg.def-loc:screen-value eq ""     and
     itemfg.def-loc-bin:screen-value eq "" then do:
     
    find first cust
        where cust.company eq gcompany
          and cust.active  eq "X"
        no-lock no-error.
    if avail cust then do:
      find first shipto of cust no-lock no-error.
      if avail shipto then
        assign
         itemfg.def-loc:screen-value = caps(shipto.loc)
         itemfg.def-loc-bin:screen-value = caps(shipto.loc-bin).
    end.
  end.               
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.cust-no V-table-Win
ON VALUE-CHANGED OF itemfg.cust-no IN FRAME F-Main /* Cust# */
DO:
  FIND cust
      WHERE cust.company EQ gcompany
        AND cust.cust-no BEGINS {&self-name}:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL cust THEN
    ASSIGN
     {&self-name}:SCREEN-VALUE     = cust.cust-no
     itemfg.cust-name:SCREEN-VALUE = cust.name
     fi_curr-code:SCREEN-VALUE     = cust.curr-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.def-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.def-loc V-table-Win
ON LEAVE OF itemfg.def-loc IN FRAME F-Main /* Warehouse */
DO:
    if lastkey <> -1 and itemfg.def-loc:screen-value <> "" and
    not can-find(first loc where loc.company = gcompany and loc.loc = itemfg.def-loc:screen-value)
    then do:
         message "Invalid Warehouse. Try Help." view-as alert-box error.
         return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.def-loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.def-loc-bin V-table-Win
ON LEAVE OF itemfg.def-loc-bin IN FRAME F-Main /* Bin */
DO:
    if lastkey <> -1 and itemfg.def-loc-bin:screen-value <> "" and
       not can-find(first fg-bin where fg-bin.company = gcompany and fg-bin.loc = itemfg.def-loc:screen-value and
                          fg-bin.loc-bin = itemfg.def-loc-bin:screen-value)
    then do:
         message "Invalid Warehouse Bin. Try Help." view-as alert-box error.
         return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.est-no V-table-Win
ON LEAVE OF itemfg.est-no IN FRAME F-Main /* Estimate # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.est-no V-table-Win
ON VALUE-CHANGED OF itemfg.est-no IN FRAME F-Main /* Estimate # */
DO:
  DEF VAR lv-cust-no LIKE eb.cust-no NO-UNDO.


  lv-cust-no = itemfg.cust-no:SCREEN-VALUE.

  FIND eb
      WHERE eb.company EQ gcompany
        AND eb.est-no  BEGINS STRING(INT({&self-name}:SCREEN-VALUE),">>>>>")
        AND eb.part-no EQ itemfg.part-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL eb THEN
    ASSIGN
     itemfg.cad-no:SCREEN-VALUE   = CAPS(eb.cad-no)
     itemfg.spc-no:SCREEN-VALUE   = CAPS(eb.spc-no)
     itemfg.upc-no:SCREEN-VALUE   = CAPS(eb.upc-no)
     itemfg.plate-no:SCREEN-VALUE = CAPS(eb.plate-no)
     itemfg.die-no:SCREEN-VALUE   = CAPS(eb.die-no)
     itemfg.cust-no:SCREEN-VALUE  = CAPS(eb.cust-no)
     itemfg.part-no:SCREEN-VALUE  = CAPS(eb.part-no).

  IF itemfg.cust-no:SCREEN-VALUE NE lv-cust-no THEN
    APPLY "value-changed" TO itemfg.cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.i-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-code V-table-Win
ON return OF itemfg.i-code IN FRAME F-Main /* Item Code */
DO:
  apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no V-table-Win
ON ENTRY OF itemfg.i-no IN FRAME F-Main /* FG Item # */
DO:
  RUN enable-itemfg-field.
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no V-table-Win
ON LEAVE OF itemfg.i-no IN FRAME F-Main /* FG Item # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.isaset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.isaset V-table-Win
ON return OF itemfg.isaset IN FRAME F-Main /* Set Header? */
DO:
   apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.procat V-table-Win
ON LEAVE OF itemfg.procat IN FRAME F-Main /* FG Category */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first fgcat where fgcat.company = gcompany and
                                      fgcat.procat = self:screen-value)
    then do:
         message "Invalid Product Category. Try Help." view-as alert-box error.
         return no-apply.
    end.
    
    find first fgcat where fgcat.company = gcompany and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
   itemfg.procat-desc:screen-value = if avail fgcat then fgcat.dscr else "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.prod-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.prod-uom V-table-Win
ON LEAVE OF itemfg.prod-uom IN FRAME F-Main /* Cost UOM */
DO:
    if lastkey <> -1 and 
       ( (itemfg.i-code:screen-value = "S" and
          can-do("EA,M",itemfg.prod-uom:screen-value )) or
         (itemfg.i-code:screen-value = "C" and can-do("M",itemfg.prod-uom:screen-value) )
        )
    then do:  end.
    else if lastkey <>  -1 then do:
         message "Enter M for Box Products, Enter EA or M for Non Box Products."
                   view-as alert-box error.
         return no-apply.
    end.     
    def var cocode as cha no-undo.
    cocode = gcompany.
    for each fg-bin  where fg-bin.company eq gcompany
                       and fg-bin.i-no    eq itemfg.i-no:screen-value
                       and fg-bin.pur-uom ne input itemfg.prod-uom:
        
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.avg-cost,
                                     output fg-bin.avg-cost).
        
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.last-cost,
                                     output fg-bin.last-cost).
        
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.std-mat-cost,
                                     output fg-bin.std-mat-cost).
                                     
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.std-lab-cost,
                                     output fg-bin.std-lab-cost).
                                     
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.std-var-cost,
                                     output fg-bin.std-var-cost).
                                     
        run sys/ref/convcuom.p (fg-bin.pur-uom, input itemfg.prod-uom,
                                0, 0, 0, 0, fg-bin.std-fix-cost,
                                     output fg-bin.std-fix-cost).
                                
        assign
         fg-bin.std-tot-cost = fg-bin.std-mat-cost +
                               fg-bin.std-lab-cost +
                               fg-bin.std-var-cost +
                               fg-bin.std-fix-cost
         fg-bin.pur-uom      = input itemfg.prod-uom.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-man V-table-Win
ON return OF itemfg.pur-man IN FRAME F-Main /* Purchased or Manf */
DO:
   apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-man V-table-Win
ON VALUE-CHANGED OF itemfg.pur-man IN FRAME F-Main /* Purchased or Manf */
DO:
    if itemfg.pur-man:screen-value = "no" then itemfg.prod-uom:screen-value = "M".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.sell-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.sell-uom V-table-Win
ON LEAVE OF itemfg.sell-uom IN FRAME F-Main /* UOM */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first uom where uom.uom = self:screen-value and
                                    lookup(uom.uom, uom-list) > 0 )
    then do:
         message "Invalid Unit of Measure. Try help." view-as alert-box error.
         return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-fix-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-fix-cost V-table-Win
ON LEAVE OF itemfg.std-fix-cost IN FRAME F-Main /* Std Fix OH Cost */
DO:
  run calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-lab-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-lab-cost V-table-Win
ON LEAVE OF itemfg.std-lab-cost IN FRAME F-Main /* Std Labor Cost */
DO:
  run calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-mat-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-mat-cost V-table-Win
ON LEAVE OF itemfg.std-mat-cost IN FRAME F-Main /* Std Mat'l Cost */
DO:
  run calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-var-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-var-cost V-table-Win
ON LEAVE OF itemfg.std-var-cost IN FRAME F-Main /* Std Var OH Cost */
DO:
  run calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.style V-table-Win
ON LEAVE OF itemfg.style IN FRAME F-Main /* Style */
DO:  
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first style where style.company = gcompany and
                                      style.style = self:screen-value)
    then do:
         message "Invalid Style. Try Help." view-as alert-box error.
         return no-apply.
    end.
    
    find first style where style.company = gcompany and
                                      style.style = self:screen-value
                                      no-lock no-error.
    itemfg.style-desc:screen-value = if avail style then style.dscr else "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

session:data-entry-return = yes.

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-std-cost V-table-Win 
PROCEDURE calc-std-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  do with frame {&frame-name}:
    itemfg.total-std-cost:screen-value =
       string(dec(itemfg.std-mat-cost:screen-value) +
              dec(itemfg.std-lab-cost:screen-value) +
              dec(itemfg.std-var-cost:screen-value) +
              dec(itemfg.std-fix-cost:screen-value)).
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation V-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-itemfg-field V-table-Win 
PROCEDURE enable-itemfg-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&frame-name}:
    ENABLE ALL.

    DISABLE itemfg.cust-name
            itemfg.procat-desc
            itemfg.style-desc.

    IF NOT adm-new-record THEN DISABLE itemfg.i-no.
    ELSE
    IF adm-adding-record THEN tb_taxable:SCREEN-VALUE = "No".
    
    IF AVAIL itemfg THEN DO:
      IF itemfg.q-onh NE 0 THEN
        DISABLE itemfg.std-mat-cost
                itemfg.std-lab-cost
                itemfg.std-var-cost
                itemfg.std-fix-cost
                itemfg.total-std-cost
                itemfg.avg-cost
                itemfg.last-cost.
              
      IF (itemfg.i-code:SCREEN-VALUE NE "S" OR itemfg.q-onh NE 0) AND itemfg.prod-uom NE "" THEN
        DISABLE itemfg.prod-uom.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation V-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-taxable      AS LOG NO-UNDO.
  def var lv-custype      like oe-prmtx.custype no-undo.
  def var lv-cust-no      like oe-prmtx.cust-no no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   old-part-no# = itemfg.part-no
   old-part-dscr1 = itemfg.part-dscr1
   old-i-name = itemfg.i-name
   old-die-no = itemfg.die-no
   old-plate-no = itemfg.plate-no
   old-cad-no = itemfg.cad-no
   old-spc-no = itemfg.spc-no
   old-upc-no = itemfg.upc-no
   old-procat = itemfg.procat.

  DO WITH FRAME {&FRAME-NAME}:
    ll-taxable = tb_taxable:SCREEN-VALUE EQ "yes".
  END.
         
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  itemfg.taxable = ll-taxable.

  if old-part-no#   ne itemfg.part-no or
     old-i-name     ne itemfg.i-name or 
     old-part-dscr1 ne itemfg.part-dscr1 or
     old-die-no     ne itemfg.die-no or 
     old-plate-no   ne itemfg.plate-no or
     old-cad-no     ne itemfg.cad-no or
     old-spc-no     ne itemfg.spc-no or
     old-upc-no     ne itemfg.upc-no
  then do:
       run update-order.
  end.

  IF old-procat NE "" AND itemfg.procat ne old-procat and itemfg.i-code eq "S" then do:
    find first oe-prmtx
        where oe-prmtx.company eq cocode
        no-lock no-error.
        
    do while avail oe-prmtx:
      lv-custype = oe-prmtx.custype.
          
      do while avail oe-prmtx:
        lv-cust-no = oe-prmtx.cust-no.
        
        for each oe-prmtx
            where oe-prmtx.company eq cocode
              and oe-prmtx.custype eq lv-custype
              and oe-prmtx.cust-no eq lv-cust-no
              and oe-prmtx.procat  eq old-procat
              and oe-prmtx.i-no    eq itemfg.i-no:
            
          oe-prmtx.procat = itemfg.procat.    
        end.
           
        find first oe-prmtx
            where oe-prmtx.company eq cocode
              and oe-prmtx.custype eq lv-custype
              and oe-prmtx.cust-no lt lv-cust-no
            no-lock no-error.
      end.
          
      find first oe-prmtx
          where oe-prmtx.company eq cocode
            and oe-prmtx.custype lt lv-custype
          no-lock no-error.
    end.  
  end.


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
  DO WITH FRAME {&frame-name}:
    DISABLE ALL.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find first oe-ctrl where oe-ctrl.company = gcompany no-lock no-error.
  
  assign itemfg.company = gcompany
         itemfg.loc = gloc
         itemfg.sell-uom = "M"
/*       itemfg.alloc = 0 ??? logical */
         itemfg.i-code = if avail oe-ctrl and oe-ctrl.i-code then "S" else "C".
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL itemfg AND itemfg.cust-no NE "" THEN
  FIND FIRST cust
      WHERE cust.company EQ gcompany
        AND cust.cust-no EQ itemfg.cust-no
      NO-LOCK NO-ERROR.
  fi_curr-code = IF AVAIL cust THEN cust.curr-code ELSE "".

  IF AVAIL itemfg AND itemfg.i-no NE "" THEN
  FIND FIRST reftable
      WHERE reftable.reftable EQ "FGTAXABLE"
        AND reftable.company  EQ gcompany
        AND reftable.loc      EQ ""
        AND reftable.code     EQ itemfg.i-no
      NO-LOCK NO-ERROR.
  tb_taxable = AVAIL itemfg AND itemfg.taxable.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages as log.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  do with frame {&frame-name}:
    if itemfg.style:screen-value <> "" and
       not can-find(first style where style.company = gcompany and
                                      style.style = itemfg.style:screen-value)
    then do:
         message "Invalid Style. Try Help." view-as alert-box error.
         apply "entry" to itemfg.style.
         return no-apply.
    end.
    if  (itemfg.i-code:screen-value eq "C" and
         itemfg.cust-no:screen-value eq "" ) 
    then do:
         message "Invalid Customer Number" view-as alert-box error.
         apply "entry" to itemfg.cust-no.
         return no-apply.
    end.
    if itemfg.procat:screen-value <> "" and
       not can-find(first fgcat where fgcat.company = gcompany and
                                      fgcat.procat = itemfg.procat:screen-value)
    then do:
         message "Invalid Product Category. Try Help." view-as alert-box error.
         apply "entry" to itemfg.procat.
         return no-apply.
    end.
    if itemfg.def-loc:screen-value <> "" and
       not can-find(first loc where loc.company = gcompany and loc.loc = itemfg.def-loc:screen-value)
    then do:
         message "Invalid Warehouse. Try Help." view-as alert-box error.
         apply "entry" to itemfg.def-loc.
         return no-apply.
    end.
    
    if itemfg.def-loc-bin:screen-value <> "" and
       not can-find(first fg-bin where fg-bin.company = gcompany and fg-bin.loc = itemfg.def-loc:screen-value and
                          fg-bin.loc-bin = itemfg.def-loc-bin:screen-value)
    then do:
         message "Invalid Warehouse Bin. Try Help." view-as alert-box error.
         apply "entry" to itemfg.def-loc-bin.
         return no-apply.
    end.
  end.
  
  run calc-std-cost.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&frame-name}:
    DISABLE ALL.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-cost V-table-Win 
PROCEDURE recalc-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  
  
  run fg/d-recost.w (rowid(itemfg)).
  
  run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
  
  run repo-query in widget-handle(char-hdl) (rowid(itemfg)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query V-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-rowid as rowid no-undo.
  
  FIND CURRENT itemfg NO-LOCK NO-ERROR.
  RUN dispatch ('display-fields').

  
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
  {src/adm/template/snd-list.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-order V-table-Win 
PROCEDURE update-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var yn#             as   log format "Yes/No" no-undo.
   def buffer b-eb for eb.
  
   message "Update information for order ?"  VIEW-AS ALERT-BOX BUTTON YES-NO update yn#.

   if yn# then do:
      if old-part-no# ne itemfg.part-no then
      for each oe-ordl where oe-ordl.company eq itemfg.company
                         and oe-ordl.i-no    eq itemfg.i-no
                         and oe-ordl.part-no eq old-part-no# 
                         exclusive-lock:
           oe-ordl.part-no = itemfg.part-no.
      end.

      for each oe-ordl where oe-ordl.company eq itemfg.company
                         and oe-ordl.i-no            eq itemfg.i-no
                         exclusive-lock:
            assign
                oe-ordl.i-name       = itemfg.i-name
                oe-ordl.part-no      = itemfg.part-no
                oe-ordl.part-dscr1 = itemfg.part-dscr1
                oe-ordl.part-dscr2 = itemfg.part-dscr2.
      end.    
   end.
   yn# = NO.

   message "Update information for estimate?"  VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE yn#.
  
   if yn# then do:
      for each eb where eb.company eq itemfg.company
                    and eb.cust-no eq itemfg.cust-no
                    and eb.est-no  eq itemfg.est-no
                    and eb.part-no eq old-part-no#
                    exclusive-lock:
      
          find first b-eb where b-eb.est-no   eq eb.est-no
                            and b-eb.form-no eq eb.form-no
                            and b-eb.part-no eq itemfg.part-no
                            and recid(b-eb)  ne recid(eb)
                            no-lock no-error.
          if not avail b-eb then eb.part-no = itemfg.part-no.  
          assign eb.part-dscr1 = itemfg.i-name
                 eb.part-dscr2 = itemfg.part-dscr1
                 eb.die-no     = itemfg.die-no 
                 eb.plate-no   = itemfg.plate-no
                 eb.cad-no     = itemfg.cad-no
                 eb.spc-no     = itemfg.spc-no
                 eb.upc-no     = itemfg.upc-no.
      end.
    
      if avail b-eb then do:
        itemfg.part-no = old-part-no#.
        message "ERROR: Customer Part# already exists on form,"
                "please update estimate."
                view-as alert-box error.
      end.
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no V-table-Win 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&frame-name}:
    IF INT(itemfg.est-no:SCREEN-VALUE) GT 0 AND
       NOT CAN-FIND(FIRST eb WHERE eb.company   EQ gcompany
                               AND eb.est-no    EQ STRING(INT(itemfg.est-no:SCREEN-VALUE),">>>>>")
                               AND (eb.part-no  EQ itemfg.part-no:SCREEN-VALUE OR
                                    eb.stock-no EQ itemfg.i-no:SCREEN-VALUE))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.est-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF itemfg.i-no:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "FG Item# may not be spaces..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.i-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

