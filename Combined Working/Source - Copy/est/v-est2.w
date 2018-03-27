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

def var ll-auto-calc-selected as log no-undo.

def new shared buffer xef for ef.
def new shared buffer xeb for eb.

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
&Scoped-define EXTERNAL-TABLES ef eb
&Scoped-define FIRST-EXTERNAL-TABLE ef


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ef, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ~
ef.xgrain ef.board ef.brd-dscr ef.cost-msh ef.nc ef.fr-msh ef.cal ef.weight ~
ef.roll ef.roll-wid eb.num-wid ef.trim-w ef.trim-l eb.num-len eb.num-up ~
eb.die-in ef.nsh-wid ef.nsh-len ef.n-out ef.n-cuts ef.gsh-wid ef.gsh-len ~
ef.leaf[1] ef.leaf-dscr[1] ef.leaf-snum[1] ef.leaf-bnum[1] ef.leaf-w[1] ~
ef.leaf-l[1] ef.leaf-dscr[2] ef.leaf-snum[2] ef.leaf-bnum[2] ef.leaf-w[2] ~
ef.leaf-l[2] ef.leaf[2] ef.leaf-l[3] ef.leaf[3] ef.leaf-dscr[3] ~
ef.leaf-snum[3] ef.leaf-bnum[3] ef.leaf-w[3] ef.leaf[4] ef.leaf-dscr[4] ~
ef.leaf-snum[4] ef.leaf-bnum[4] ef.leaf-l[4] ef.leaf-w[4] 
&Scoped-define ENABLED-TABLES ef eb
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-define SECOND-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-30 
&Scoped-Define DISPLAYED-FIELDS ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ~
ef.xgrain ef.board ef.brd-dscr ef.cost-uom ef.cost-msh ef.nc ef.fr-uom ~
ef.fr-msh ef.i-code ef.cal ef.weight ef.roll ef.roll-wid eb.t-sqin eb.t-wid ~
eb.t-len eb.num-wid ef.trim-w ef.trim-l eb.num-len eb.num-up eb.die-in ~
ef.nsh-wid ef.nsh-len ef.n-out ef.n-cuts ef.gsh-wid ef.gsh-len ef.leaf[1] ~
ef.leaf-dscr[1] ef.leaf-snum[1] ef.leaf-bnum[1] ef.leaf-w[1] ef.leaf-l[1] ~
ef.leaf-dscr[2] ef.leaf-snum[2] ef.leaf-bnum[2] ef.leaf-w[2] ef.leaf-l[2] ~
ef.leaf[2] ef.leaf-l[3] ef.leaf[3] ef.leaf-dscr[3] ef.leaf-snum[3] ~
ef.leaf-bnum[3] ef.leaf-w[3] ef.leaf[4] ef.leaf-dscr[4] ef.leaf-snum[4] ~
ef.leaf-bnum[4] ef.leaf-l[4] ef.leaf-w[4] 
&Scoped-define DISPLAYED-TABLES ef eb
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-define SECOND-DISPLAYED-TABLE eb


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 142 BY 15.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ef.m-code AT ROW 1.48 COL 13 COLON-ALIGNED
          LABEL "Machine"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ef.m-dscr AT ROW 1.48 COL 29 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
     ef.lsh-wid AT ROW 1.48 COL 77 COLON-ALIGNED
          LABEL "Front-Back"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.lsh-len AT ROW 1.48 COL 103 COLON-ALIGNED
          LABEL "Side-Side"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.xgrain AT ROW 1.48 COL 136 COLON-ALIGNED
          LABEL "Xgrain"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.board AT ROW 2.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.brd-dscr AT ROW 2.91 COL 27.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.cost-uom AT ROW 2.91 COL 65.6 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.cost-msh AT ROW 2.91 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     ef.nc AT ROW 2.91 COL 89.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     ef.fr-uom AT ROW 2.91 COL 107 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ef.fr-msh AT ROW 2.91 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.i-code AT ROW 4.1 COL 63.6 COLON-ALIGNED
          LABEL "Real"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.cal AT ROW 4.1 COL 81.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     ef.weight AT ROW 4.1 COL 107 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.roll AT ROW 5.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.roll-wid AT ROW 5.29 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-sqin AT ROW 6.24 COL 116 COLON-ALIGNED
          LABEL "Blank Sq.Ft."
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-wid AT ROW 6.48 COL 16 COLON-ALIGNED FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-len AT ROW 6.48 COL 28 COLON-ALIGNED NO-LABEL FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.num-wid AT ROW 7.19 COL 64 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.trim-w AT ROW 7.43 COL 16 COLON-ALIGNED
          LABEL "Die Size"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ef.trim-l AT ROW 7.43 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.num-len AT ROW 7.43 COL 76 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.num-up AT ROW 7.43 COL 91 COLON-ALIGNED
          LABEL "Tot Out"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.die-in AT ROW 7.43 COL 116 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.nsh-wid AT ROW 8.38 COL 16 COLON-ALIGNED
          LABEL "Press"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.nsh-len AT ROW 8.38 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.n-out AT ROW 8.38 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     ef.n-cuts AT ROW 8.38 COL 76 COLON-ALIGNED
          LABEL "Cuts"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.gsh-wid AT ROW 9.33 COL 16 COLON-ALIGNED
          LABEL "Gross Sheet"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.gsh-len AT ROW 9.33 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.leaf[1] AT ROW 11.95 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[1] AT ROW 11.95 COL 28.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[1] AT ROW 11.95 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.leaf-bnum[1] AT ROW 11.95 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.leaf-w[1] AT ROW 11.95 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf-l[1] AT ROW 11.95 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf-dscr[2] AT ROW 12.91 COL 28.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[2] AT ROW 12.91 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.leaf-bnum[2] AT ROW 12.91 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.leaf-w[2] AT ROW 12.91 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf-l[2] AT ROW 12.91 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf[2] AT ROW 12.95 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-l[3] AT ROW 13.86 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf[3] AT ROW 13.95 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[3] AT ROW 13.95 COL 28.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[3] AT ROW 13.95 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.leaf-bnum[3] AT ROW 13.95 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.leaf-w[3] AT ROW 13.95 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf[4] AT ROW 14.95 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[4] AT ROW 14.95 COL 28.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[4] AT ROW 14.95 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.leaf-bnum[4] AT ROW 14.95 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.leaf-l[4] AT ROW 14.95 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.leaf-w[4] AT ROW 15.05 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-30 AT ROW 1 COL 1
     "Leaf/Film" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.24 COL 17
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.24 COL 81
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.24 COL 71
     "Cost/" VIEW-AS TEXT
          SIZE 6.4 BY .95 AT ROW 2.91 COL 59
     "Freight/" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.14 COL 106.6 RIGHT-ALIGNED
     "W #Up" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 6.24 COL 64
     "Description" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 11.24 COL 34
     "L #Up" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 6.24 COL 75
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.57 COL 19
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.57 COL 32
     "S /  B" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.24 COL 59.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ef,ASI.eb
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
         HEIGHT             = 21.62
         WIDTH              = 144.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ef.cost-uom IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR FILL-IN ef.fr-uom IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.gsh-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.i-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.lsh-len IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.lsh-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.m-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.m-dscr IN FRAME F-Main
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN ef.n-cuts IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.nsh-len IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.nsh-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.num-len IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.num-up IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.num-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.roll-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.t-len IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       eb.t-len:PRIVATE-DATA IN FRAME F-Main     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.t-sqin IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.t-wid IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       eb.t-wid:PRIVATE-DATA IN FRAME F-Main     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.trim-l IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.trim-w IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.xgrain IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TEXT-LITERAL "Freight/"
          SIZE 10 BY .62 AT ROW 3.14 COL 106.6 RIGHT-ALIGNED            */

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
   def var lv-ind like style.industry no-undo.
   def var char-val as cha no-undo.
   def var lv-rowid as rowid no-undo.


   case focus:label :
     when "Board" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (eb.company,lv-ind,focus:screen-value,output char-val).
           else run windows/l-board1.w (eb.company,lv-ind,focus:screen-value, output lv-rowid).
           FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN DO:
              assign ef.board:screen-value in frame {&frame-name}    = item.i-no
                     ef.cal:screen-value in frame {&frame-name}      = string(item.cal)
                     ef.brd-dscr:screen-value in frame {&frame-name} = item.i-name.  
             /*find item where item.company = eb.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign ef.gsh-wid:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        ef.gsh-len:screen-value = string(item.s-len) 
                                        ef.test:screen-value = item.reg-no
                                        ef.flute:screen-value = item.flute
                                        .
              */                        
           end.
           return no-apply.   
     end.
     when "leaf" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,lv-ind,"F",focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     .
              case focus:index :
                   when 1 then do:    
                        assign ef.leaf-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val). 
                        find item where item.company = eb.company and item.i-no = entry(1,char-val)
                              no-lock no-error.
                        if avail item then assign ef.leaf-w[1]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                                  ef.leaf-l[1]:screen-value = string(item.s-len) 
                                                  .
                   end.  /* 1 */                  
                   when 2 then do:    
                        assign ef.leaf-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val). 
                        find item where item.company = eb.company and item.i-no = entry(1,char-val)
                              no-lock no-error.
                        if avail item then assign ef.leaf-w[2]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                                  ef.leaf-l[2]:screen-value = string(item.s-len) 
                                                  .
                   end.  /* 2 */                  
                   when 3 then do:    
                        assign ef.leaf-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val). 
                        find item where item.company = eb.company and item.i-no = entry(1,char-val)
                              no-lock no-error.
                        if avail item then assign ef.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                                  ef.leaf-l[3]:screen-value = string(item.s-len) 
                                                  .
                   end.  /* 3 */                  
                   when 4 then do:    
                        assign ef.leaf-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val). 
                        find item where item.company = eb.company and item.i-no = entry(1,char-val)
                              no-lock no-error.
                        if avail item then assign ef.leaf-w[4]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                                  ef.leaf-l[4]:screen-value = string(item.s-len) 
                                                  .
                   end.  /* 4 */                  

              end case.                          
           end.  /* char-val */
           return no-apply.   
     end.  /* leaf */

  end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-len V-table-Win
ON LEAVE OF eb.t-len IN FRAME F-Main /* Blank Length */
DO:
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
    {&methods/lValidateError.i YES}
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-wid V-table-Win
ON LEAVE OF eb.t-wid IN FRAME F-Main /* Blank Width */
DO:
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
    {&methods/lValidateError.i YES}
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
    {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "ef"}
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ef"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-calc V-table-Win 
PROCEDURE auto-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ll-auto-calc-selected = yes.
   run dispatch ('enable-fields').
   disable ef.m-code ef.lsh-len ef.lsh-wid ef.m-dscr
           ef.xgrain ef.board ef.flute
           ef.gsh-wid ef.gsh-len ef.gsh-dep

           with frame {&frame-name}.

/*===========  corrware
 ll-auto-calc-selected = yes.

   find first item where item.company = gcompany and
                         item.i-no = ef.board
                         no-lock no-error.

   if not lv-is-foam or item.i-code = "E" then do:
      find first mach where mach.company = gcompany and
                            mach.loc = eb.loc and
                            mach.m-code = ef.m-code
                            use-index m-code no-lock no-error.
        if avail mach and mach.dept[1] eq "RC" then
           assign ef.nsh-wid:screen-value in frame {&frame-name} = string(ef.nsh-wid - (2 * mach.min-triml))
                  ef.nsh-len:screen-value = string(ef.nsh-len - (2 * mach.min-trimw)).   
        assign ef.n-out:screen-value   = string(trunc(ef.lsh-len / ef.nsh-wid,0))
               ef.n-out-l:screen-value = string(trunc(ef.lsh-wid / ef.nsh-len,0))
               ef.n-out-d:screen-value = string("1").
   end.

   assign ef.n-out:screen-value   = string("0")
          ef.n-out-l:screen-value = string("0")
          ef.n-out-d:screen-value = string("0")
          ef.gsh-len:screen-value = string("0")
          ef.gsh-wid:screen-value = string("0")
          ef.gsh-dep:screen-value = string("0")
          ef.nsh-len:screen-value = string("0")
          ef.nsh-wid:screen-value = string("0")
          ef.nsh-dep:screen-value = string("0")
          ef.trim-w:screen-value  = string("0")
          ef.trim-l:screen-value  = string("0")
          ef.trim-d:screen-value  = string("0")
          eb.num-len:screen-value = string("0")
          eb.num-wid:screen-value = string("0")
          eb.num-dep:screen-value = string("0").


   run dispatch ('enable-fields').
   disable ef.gsh-wid ef.gsh-len ef.gsh-dep
           ef.nsh-wid ef.nsh-len ef.nsh-dep
           ef.trim-w ef.trim-l ef.trim-d
           with frame {&frame-name}.
   enable ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ef.xgrain with frame {&frame-name}.
   apply "entry" to ef.m-code in frame {&frame-name} .

======*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if ll-auto-calc-selected then do:
     ll-auto-calc-selected = no.
     /*     find xest where recid(xest) = recid(est).  */
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     run ce/calc-dim.p .
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     assign ef.lsh-len:screen-value in frame {&frame-name} = string(xef.lsh-len )
              ef.lsh-wid:screen-value = string(xef.lsh-wid )
              ef.gsh-len:screen-value = string(xef.gsh-len )
              ef.gsh-wid:screen-value = string(xef.gsh-wid )
              ef.nsh-len:screen-value = string(xef.nsh-len )
              ef.nsh-wid:screen-value = string(xef.nsh-wid)
              ef.trim-l:screen-value = string(xef.trim-l )
              ef.trim-w:screen-value = string(xef.trim-w )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              .

     disable ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ef.xgrain with frame {&frame-name}.
  end.

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
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}

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

