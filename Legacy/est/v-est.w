&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p on 03.28.2017 @ 10:44:16 am */

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
def var li-new-estnum like ce-ctrl.e-num no-undo.
def var ll-new-record as log no-undo.
def var ll-is-copy-record as log no-undo.
def var char-val as cha no-undo.
def var ll-auto-calc-selected as log no-undo.
def buffer bf-est for est.
def buffer bf-eb for eb.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.
def new shared var formule as de extent 12 .
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.
DEF BUFFER recalc-mr FOR reftable.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Fold

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est eb est-qty ef
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb, est-qty, ef.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.cust-no eb.ship-id eb.ship-name ~
eb.ship-addr[1] eb.ship-addr[2] eb.ship-city eb.ship-state eb.ship-zip ~
eb.sman eb.comm eb.part-no eb.plate-no eb.stock-no eb.die-no eb.part-dscr1 ~
eb.cad-no eb.part-dscr2 eb.upc-no eb.procat eb.spc-no ef.board ef.brd-dscr ~
eb.i-col eb.i-pass eb.i-coat eb.i-coat-p eb.style eb.len eb.wid eb.dep ~
eb.dust eb.lock eb.fpanel eb.k-len eb.k-wid eb.adhesive eb.tuck eb.gluelap ~
eb.lin-in eb.t-len eb.t-wid eb.t-sqin 
&Scoped-define ENABLED-TABLES eb ef
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-define SECOND-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-18 
&Scoped-Define DISPLAYED-FIELDS est.est-no est.est-date est.mod-date ~
est.ord-no est.ord-date eb.cust-no eb.ship-id eb.ship-name eb.ship-addr[1] ~
eb.ship-addr[2] eb.ship-city eb.ship-state eb.ship-zip est-qty.eqty eb.sman ~
eb.comm eb.part-no eb.plate-no eb.stock-no eb.die-no eb.part-dscr1 ~
eb.cad-no eb.part-dscr2 eb.upc-no eb.procat eb.spc-no ef.board ef.brd-dscr ~
eb.i-col eb.i-pass eb.i-coat eb.i-coat-p eb.style eb.len eb.wid eb.dep ~
eb.dust eb.lock eb.fpanel eb.k-len eb.k-wid eb.adhesive eb.tuck eb.gluelap ~
eb.lin-in eb.t-len eb.t-wid eb.t-sqin 
&Scoped-define DISPLAYED-TABLES est eb est-qty ef
&Scoped-define FIRST-DISPLAYED-TABLE est
&Scoped-define SECOND-DISPLAYED-TABLE eb
&Scoped-define THIRD-DISPLAYED-TABLE est-qty
&Scoped-define FOURTH-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS sman_sname procat_desc style_dscr 

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
DEFINE VARIABLE procat_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 8.1.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 16.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Fold
     est.est-no AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     est.est-date AT ROW 1.24 COL 43 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     est.mod-date AT ROW 1.24 COL 76 COLON-ALIGNED
          LABEL "Mod"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     est.ord-no AT ROW 1.24 COL 110 COLON-ALIGNED
          LABEL "Last Order#"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     est.ord-date AT ROW 1.24 COL 128 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     eb.cust-no AT ROW 2.19 COL 19 COLON-ALIGNED
          LABEL "Bill To Code"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.ship-id AT ROW 2.19 COL 47 COLON-ALIGNED
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.ship-name AT ROW 3.14 COL 19 COLON-ALIGNED
          LABEL "Company"
          VIEW-AS FILL-IN 
          SIZE 39.4 BY 1
     eb.ship-addr[1] AT ROW 4.1 COL 19 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 39.4 BY 1
     eb.ship-addr[2] AT ROW 5.05 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39.4 BY 1
     eb.ship-city AT ROW 6 COL 19 COLON-ALIGNED
          LABEL "City/State/Zip"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.ship-state AT ROW 6 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     eb.ship-zip AT ROW 6 COL 46 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     est-qty.eqty AT ROW 2.19 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.sman AT ROW 2.19 COL 98 COLON-ALIGNED
          LABEL "Slsmn"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     sman_sname AT ROW 2.19 COL 103 COLON-ALIGNED NO-LABEL
     eb.comm AT ROW 2.19 COL 134.8 COLON-ALIGNED
          LABEL "%"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.part-no AT ROW 3.38 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.plate-no AT ROW 3.38 COL 124 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.stock-no AT ROW 4.57 COL 76 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.die-no AT ROW 4.57 COL 124 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.part-dscr1 AT ROW 5.52 COL 76 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.cad-no AT ROW 5.76 COL 145 RIGHT-ALIGNED
          LABEL "CAD#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.part-dscr2 AT ROW 6.48 COL 76 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     eb.upc-no AT ROW 6.95 COL 124 COLON-ALIGNED
          LABEL "UPC#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.procat AT ROW 7.43 COL 19 COLON-ALIGNED
          LABEL "FG Category"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     procat_desc AT ROW 7.43 COL 27 COLON-ALIGNED NO-LABEL
     eb.spc-no AT ROW 8.14 COL 124 COLON-ALIGNED
          LABEL "QC #"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Fold
     ef.board AT ROW 9.81 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ef.brd-dscr AT ROW 9.81 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     eb.i-col AT ROW 9.81 COL 72 COLON-ALIGNED
          LABEL "Colors"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-pass AT ROW 9.81 COL 88 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 9.81 COL 109 COLON-ALIGNED
          LABEL "Coatings"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat-p AT ROW 9.81 COL 126 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.style AT ROW 11.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     style_dscr AT ROW 11.24 COL 27 COLON-ALIGNED NO-LABEL
     eb.len AT ROW 12.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.wid AT ROW 12.43 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dep AT ROW 12.43 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dust AT ROW 13.38 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lock AT ROW 13.38 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.fpanel AT ROW 13.38 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-len AT ROW 14.33 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-wid AT ROW 14.33 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.adhesive AT ROW 14.33 COL 91 COLON-ALIGNED
          LABEL "Glue Code"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     eb.tuck AT ROW 15.29 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.gluelap AT ROW 15.29 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lin-in AT ROW 15.29 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     eb.t-len AT ROW 16.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-wid AT ROW 16.24 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-sqin AT ROW 16.24 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     RECT-19 AT ROW 1 COL 1
     RECT-18 AT ROW 9.57 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.eb,ASI.est-qty,ASI.ef
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
         HEIGHT             = 17
         WIDTH              = 151.
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
/* SETTINGS FOR FRAME Fold
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME Fold:SCROLLABLE       = FALSE
       FRAME Fold:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.adhesive IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.brd-dscr IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.cad-no IN FRAME Fold
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN eb.comm IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.cust-no IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est-qty.eqty IN FRAME Fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN est.est-date IN FRAME Fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.est-no IN FRAME Fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat-p IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-col IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-pass IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.mod-date IN FRAME Fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.ord-date IN FRAME Fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.ord-no IN FRAME Fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.procat IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN procat_desc IN FRAME Fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.ship-addr[1] IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.ship-city IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.ship-id IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.ship-name IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.sman IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME Fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.spc-no IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME Fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME Fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.upc-no IN FRAME Fold
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Fold
/* Query rebuild information for FRAME Fold
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Fold */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fold V-table-Win
ON HELP OF FRAME Fold
DO:
   def var lv-handle as widget-handle no-undo.
   case focus:name :
        when "sman" then do:
             run windows/l-sman.w (gcompany, output char-val).
             if char-val <> "" then 
                assign focus:screen-value = entry(1,char-val)
                       sman_sname:screen-value = entry(2,char-val)
                       eb.comm:screen-value = entry(3,char-val).
             return no-apply.          
        end.
        when "req-date" or when "due-date" then do:
             /*{methods/calendar.i}  run on self's help trigger*/

        end.
        otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
                 assign eb.ship-name:screen-value = cust.name
                     eb.ship-addr[1]:screen-value = cust.addr[1]
                     eb.ship-addr[2]:screen-value = cust.addr[2]
                     eb.ship-city:screen-value =    cust.city
                     eb.ship-state:screen-value =   cust.state
                     eb.ship-zip:screen-value =     cust.zip
                     eb.sman:screen-value in frame {&frame-name} = if avail cust then cust.sman else ""
            /*         eb.dest-code:screen-value in frame {&frame-name} = if cust.fob-code = "Dest" then "D"
                                                                        else if cust.fob-code = "orig" then "O"
                                                                        else ""
                     eb.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "B" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else ""
              */
                     .                

                 find sman where sman.company = gcompany and
                              sman.sman = eb.sman:screen-value
                              no-lock no-error.
                 assign sman_sname:screen-value = if avail sman then sman.sname else ""
                     eb.comm:screen-value = if avail sman then string(sman.scomm) else "0"
                     .
              end.  /* cust-no */
           end.   /* g_lookup-var <> "" */

        end.   
   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
assign cocode = gcompany
       locode = gloc.
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
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "eb"}
  {src/adm/template/row-list.i "est-qty"}
  {src/adm/template/row-list.i "ef"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "eb"}
  {src/adm/template/row-find.i "est-qty"}
  {src/adm/template/row-find.i "ef"}

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
   disable eb.t-wid eb.t-len eb.t-sqin
           with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size V-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* calc blank W,L SqIn */

   find xest where recid(xest) = recid(est) no-lock.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.

   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
      run est/u2kinc1.p .
      run est/u2kinc2.p .

      find bf-eb of eb exclusive-lock.    
      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          .
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-childrecord V-table-Win 
PROCEDURE create-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def buffer bb for eb.

  create ef.
  assign
   ef.est-type  = 1
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-len   = ce-ctrl.ls-length
   ef.lsh-wid   = ce-ctrl.ls-width.

  for each bb where bb.e-num = 0 or bb.e-num = est.e-num:
      message "BB crt1 : " bb.est-no bb.e-num view-as alert-box.
  end.



  create eb.
  assign  eb.est-type = 1
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = integer(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0.

  /* ???? bugs : 2 records are created  , delete one ========== */
  for each bb where bb.e-num = 0 :
      delete bb.
  end.
  /*========*/
  find first item where item.company = gcompany
                    and item.mat-type = "C"  /* Case/Bundle */
                    and item.i-no eq eb.cas-no
      no-lock no-error.
  if avail item then do:
     find first e-item where e-item.company eq item.company
                         and e-item.loc     eq item.loc
                         and e-item.i-no    eq item.i-no
        no-lock no-error.
     find first itemfg  where itemfg.company eq gcompany
                          and itemfg.i-no    eq eb.stock-no
        no-lock no-error.
     if avail e-item then
        assign  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = if avail itemfg then itemfg.case-count else e-item.box-case
                .
     if eb.cas-len eq 0 then eb.cas-len = item.case-l.
     if eb.cas-wid eq 0 then eb.cas-wid = item.case-w.
     if eb.cas-dep eq 0 then eb.cas-dep = item.case-d.
     if eb.cas-wt  eq 0 then eb.cas-wt  = item.avg-w.
     if eb.cas-pal eq 0 then eb.cas-pal = item.case-pall.
     if eb.cas-cnt eq 0 then eb.cas-cnt =
              if avail itemfg then itemfg.case-count else item.box-case.
  end.  /* avail item */

    RUN est/BuildDefaultPreps.p(BUFFER est,
                              BUFFER ef,
                              INPUT 1,
                              INPUT 1).
/*   i = 1.                                                                                */
/*                                                                                         */
/*                                                                                         */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:           */
/*       create est-prep.                                                                  */
/*       assign est-prep.e-num  = est.e-num                                                */
/*              est-prep.est-no = est.est-no                                               */
/*              est-prep.line   = i                                                        */
/*              est-prep.s-num  = 1                                                        */
/*              est-prep.b-num  = 1                                                        */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in      */
/*                                else if prep.mat-type eq "b" and  avail ef               */
/*                                then ef.adh-sqin                                         */
/*                         else 1  /* mat-type eq "m" */                                   */
/*             est-prep.code   = prep.code                                                 */
/*             est-prep.dscr   = prep.dscr                                                 */
/*             est-prep.cost   = prep.cost                                                 */
/*             est-prep.ml     = prep.ml                                                   */
/*             est-prep.simon  = prep.simon                                                */
/*             est-prep.mkup   = prep.mkup                                                 */
/*             est-prep.amtz   = prep.amtz                                                 */
/*             est-prep.mat-type = prep.mat-type.                                          */
/*             if lookup(est-prep.mat-type, "p,f") gt 0 then                               */
/*                run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty). */
/*             i = i + 1.                                                                  */
/*   end.                                                                                  */

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
  HIDE FRAME Fold.
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
  find bf-est where recid(bf-est) = recid(est) exclusive-lock.
  assign bf-est.mod-date = today
         est.mod-date:screen-value in frame {&frame-name} = string(today)
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-enum like est.e-num no-undo.
  def var cocode as cha no-undo.
  def buffer bf-est for est.
  def buffer bb for eb.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */
  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           no-lock.
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = yes.

  assign est.est-type = 1
         est.company = gcompany
         est.loc = gloc
         est.e-num = li-enum + 1
         est.est-no = string(li-new-estnum,">>>>>>>>")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         .
   display est.est-no est.est-date with frame {&frame-name}.

   assign cocode = gcompany
         .      

   {sys/ref/est-add.i est}     

   run create-childrecord.  /* create ef,eb,est-prep */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if ll-auto-calc-selected then do:
     run calc-blank-size.
     find bf-eb of eb no-lock.
     assign eb.t-len = bf-eb.t-len   /* decimal(eb.t-len:screen-value in frame {&frame-name})*/
            eb.t-wid = bf-eb.t-wid   /*decimal(eb.t-wid:screen-value in frame {&frame-name})*/
            eb.t-sqin = eb.t-len * eb.t-wid .
     ll-auto-calc-selected = no.
  end.

  run dispatch ('display-fields').  /* refresh 2nd & all children pages */

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "ef"}

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

