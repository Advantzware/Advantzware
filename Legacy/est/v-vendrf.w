&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: est\v-vendrf.w

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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
&scoped-define proc-enable  ENABLE-detail

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/VAR.i NEW SHARED}

{est/printquo.i new}

def NEW SHARED buffer xquo for quote-vendor.

def var list-name as cha no-undo.
def var tmp-dir as cha no-undo.
def var init-dir as cha no-undo.
DEF VAR lv-ship-no LIKE shipto.ship-no NO-UNDO.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-part-no LIKE quote-vendor-item.part-no NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

DEF TEMP-TABLE w-qqty NO-UNDO FIELD w-rowid AS ROWID.

{sa/sa-sls01.i}  /* report */

{fg/d-invprc.i NEW}

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

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
&Scoped-define EXTERNAL-TABLES quote-vendor
&Scoped-define FIRST-EXTERNAL-TABLE quote-vendor


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quote-vendor.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS quote-vendor.quo-date quote-vendor.est-no ~
quote-vendor.del-date quote-vendor.contact quote-vendor.carrier ~
quote-vendor.del-zone 
&Scoped-define ENABLED-TABLES quote-vendor
&Scoped-define FIRST-ENABLED-TABLE quote-vendor
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS quote-vendor.q-no quote-vendor.quo-date ~
quote-vendor.est-no quote-vendor.del-date quote-vendor.contact ~
quote-vendor.carrier quote-vendor.del-zone 
&Scoped-define DISPLAYED-TABLES quote-vendor
&Scoped-define FIRST-DISPLAYED-TABLE quote-vendor
&Scoped-Define DISPLAYED-OBJECTS ls-status carrier_desc zon_desc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define List-5 quote-vendor.carrier 

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
DEFINE VARIABLE carrier_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE ls-status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order Status" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE zon_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 10.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     quote-vendor.q-no AT ROW 1.24 COL 11 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     quote-vendor.quo-date AT ROW 1.24 COL 41.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     quote-vendor.est-no AT ROW 1.24 COL 70.8 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-status AT ROW 1.24 COL 116 COLON-ALIGNED
     quote-vendor.del-date AT ROW 2.19 COL 41.2 COLON-ALIGNED
          LABEL "Delivery Date"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     quote-vendor.contact AT ROW 2.19 COL 70.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59.2 BY 1
     quote-vendor.carrier AT ROW 3.86 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     carrier_desc AT ROW 3.86 COL 52 COLON-ALIGNED NO-LABEL
     quote-vendor.del-zone AT ROW 4.81 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     zon_desc AT ROW 4.81 COL 52 COLON-ALIGNED NO-LABEL
     RECT-5 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.quote-vendor
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
         HEIGHT             = 17.19
         WIDTH              = 134.6.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN quote-vendor.carrier IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN carrier_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN quote-vendor.del-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN quote-vendor.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ls-status IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN quote-vendor.q-no IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN zon_desc IN FRAME F-Main
   NO-ENABLE                                                            */
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
   def var lv-handle as handle no-undo.   
   DEF VAR rec-val AS RECID NO-UNDO.

   case focus:name :
        /*when "cust-no" then do:
              run windows/l-cust.w (gcompany, quote-vendor.cust-no:SCREEN-VALUE, output char-val).
              if char-val <> "" AND entry(1,char-val) NE quote-vendor.cust-no:SCREEN-VALUE then do:
                 quote-vendor.cust-no:SCREEN-VALUE = entry(1,char-val).
                 RUN new-cust-no.                                             
              end.
              return no-apply.
        end.
        when "ship-id" then do:           
              run windows/l-shipt2.w (gcompany,gloc, quote-vendor.cust-no:SCREEN-VALUE, quote-vendor.ship-id:SCREEN-VALUE, output char-val, output rec-val).
              find shipto where recid(shipto) = rec-val no-lock NO-ERROR.
              if AVAIL shipto AND shipto.ship-id NE quote-vendor.ship-id:SCREEN-VALUE then do:
                 quote-vendor.ship-id:SCREEN-VALUE = shipto.ship-id.
                 RUN new-ship-id.
              end.
              return no-apply.
        END.
        when "sold-id" then do:           
              run windows/l-soldto.w (gcompany, quote-vendor.cust-no:screen-value, quote-vendor.sold-id:SCREEN-VALUE, output char-val).
              if char-val <> "" AND entry(2,char-val) NE quote-vendor.sold-id:SCREEN-VALUE then do:
                 quote-vendor.sold-id:SCREEN-VALUE = entry(2,char-val).
                 RUN new-sold-id.
              end.
              return no-apply.
        END.
       WHEN "sman" THEN DO:
           RUN windows/l-sman.w (gcompany,OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN quote-vendor.sman:SCREEN-VALUE = ENTRY(1,char-val)
                                         sman_desc:SCREEN-VALUE = ENTRY(2,char-val).
       END.
       */
       when "del-zone" then do:
           run windows/l-delzon.w 
              (gcompany,gloc, quote-vendor.carrier:screen-value, quote-vendor.del-zone:SCREEN-VALUE, output char-val).
           if char-val <> "" then 
              assign quote-vendor.del-zone:SCREEN-VALUE = entry(1,char-val).
           return no-apply.  
       end.                  
       otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.
             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
        
           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.
       end.  /* otherwise */

         
   end case.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor.carrier V-table-Win
ON LEAVE OF quote-vendor.carrier IN FRAME F-Main /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
    carrier_desc:SCREEN-VALUE = ''.
    IF quote-vendor.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
    FIND FIRST carrier NO-LOCK WHERE carrier.company EQ gcompany
                                 AND carrier.loc EQ gloc
                                 AND carrier.carrier EQ quote-vendor.carrier:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE carrier THEN DO:
      MESSAGE 'Invalid Carrier Code. Try Help.' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    carrier_desc:SCREEN-VALUE = carrier.dscr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor.del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor.del-zone V-table-Win
ON LEAVE OF quote-vendor.del-zone IN FRAME F-Main /* Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    zon_desc:SCREEN-VALUE = ''.
    IF quote-vendor.del-zone:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
    FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ gcompany
                                  AND carr-mtx.loc EQ gloc
                                  AND carr-mtx.carrier EQ quote-vendor.carrier:SCREEN-VALUE
                                  AND carr-mtx.del-zone EQ quote-vendor.del-zone:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE carr-mtx THEN DO:
      MESSAGE 'Invalid Delivey Zone. Try Help.' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    zon_desc:SCREEN-VALUE = carr-mtx.del-dscr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
ASSIGN cocode = gcompany
       locode = gloc.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlusButton V-table-Win 
PROCEDURE addPlusButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "quote-vendor"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quote-vendor"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-line-items V-table-Win 
PROCEDURE create-line-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN fg/d-invprc.w (quote-vendor.cust-no).

  FOR EACH tt-inv WHERE tt-inv.selekt,
      FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company EQ ar-invl.company
        AND itemfg.i-no    EQ ar-invl.i-no
      NO-LOCK:
  
    FIND LAST quote-vendor-item
        WHERE quote-vendor-item.company EQ quote-vendor.company
          AND quote-vendor-item.loc     EQ quote-vendor.loc
          AND quote-vendor-item.q-no    EQ quote-vendor.q-no
        USE-INDEX q-line NO-LOCK NO-ERROR.
    li = (IF AVAIL quote-vendor-item THEN quote-vendor-item.line else 0) + 1.

    ASSIGN
     lv-part-no = ""
     lv-rowid   = ROWID(itemfg).
    RUN custom/getcpart.p (cocode, quote-vendor.cust-no,
                           INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).

    CREATE quote-vendor-item.
    ASSIGN
     quote-vendor-item.company    = quote-vendor.company
     quote-vendor-item.loc        = quote-vendor.loc
     quote-vendor-item.q-no       = quote-vendor.q-no
     quote-vendor-item.line       = li
     quote-vendor-item.upd-date   = TODAY
     quote-vendor-item.upd-user   = USERID("nosweat")
     quote-vendor-item.part-no    = IF lv-part-no EQ "" THEN itemfg.part-no
                                               ELSE lv-part-no
     quote-vendor-item.part-dscr1 = itemfg.i-name
     quote-vendor-item.style      = itemfg.style
     quote-vendor-item.price      = ar-invl.unit-pr
     quote-vendor-item.qty        = ar-invl.inv-qty
     quote-vendor-item.uom        = ar-invl.pr-qty-uom
     quote-vendor-item.size       = STRING(itemfg.l-score[50]) + " x " +
                           STRING(itemfg.w-score[50]) + " x " +
                           STRING(itemfg.d-score[50])
     /*RCO400*/
     quote-vendor-item.i-no = itemfg.i-no.

    CREATE quote-vendor-qty.
    ASSIGN
     quote-vendor-qty.company    = quote-vendor-item.company
     quote-vendor-qty.loc        = quote-vendor-item.loc
     quote-vendor-qty.q-no       = quote-vendor-item.q-no
     quote-vendor-qty.line       = quote-vendor-item.line
     quote-vendor-qty.qty        = quote-vendor-item.qty
     quote-vendor-qty.price      = quote-vendor-item.price
     quote-vendor-qty.uom        = quote-vendor-item.uom
     quote-vendor-qty.quote-date = TODAY
     quote-vendor-qty.quote-user = USERID("nosweat").

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-mat-cost,
                           OUTPUT quote-vendor-qty.mat-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-lab-cost,
                           OUTPUT quote-vendor-qty.lab-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-fix-cost,
                           OUTPUT quote-vendor-qty.fo-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-var-cost,
                           OUTPUT quote-vendor-qty.vo-cost).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-detail V-table-Win 
PROCEDURE enable-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
 IF quote-vendor.est-no <> "" AND NOT adm-new-record THEN  /* update with est#*/
      DISABLE /*quote-vendor.cust-no quote-vendor.billto[1] quote-vendor.billto[2]
              quote-vendor.billto[3] quote-vendor.billto[4]
              quote-vendor.ship-id quote-vendor.shipto[1 FOR 4]*/
              {&list-5}
       WITH FRAME {&FRAME-NAME}.

 ELSE IF quote-vendor.est-no <> "" AND adm-new-record AND NOT adm-adding-record THEN  /* copy with est#*/
      DISABLE /*quote-vendor.cust-no quote-vendor.billto[1] quote-vendor.billto[2]
              quote-vendor.billto[3] quote-vendor.billto[4]
              quote-vendor.ship-id quote-vendor.shipto[1 FOR 4]*/
              {&list-5}
              quote-vendor.est-no quote-vendor.rfq quote-vendor.sman quote-vendor.terms
              quote-vendor.carrier quote-vendor.del-zone
       WITH FRAME {&FRAME-NAME}.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ship-id LIKE quote-vendor.ship-id NO-UNDO.
  DEF VAR lv-sman LIKE quote-vendor.sman NO-UNDO.
  DEF BUFFER bf-eb FOR eb.
  DEFINE BUFFER bquote-vendor-item FOR quote-vendor-item.
  DEFINE BUFFER bquote-vendor-qty FOR quote-vendor-qty.
  DEF BUFFER bf-quote-vendor FOR quote-vendor.
  DEF BUFFER bf-quote-vendor-item FOR quote-vendor-item.
  DEF BUFFER bf-quote-vendor-qty FOR quote-vendor-qty.
  DEF BUFFER bf-quotechg FOR quotechg.
  DEF VAR v-prev-q-no AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-ship-id = quote-vendor.ship-id
         lv-sman = quote-vendor.sman
         v-prev-q-no = quote-vendor.q-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  quote-vendor.est-no = FILL(" ",8 - LENGTH(TRIM(quote-vendor.est-no))) + TRIM(quote-vendor.est-no).

  IF quote-vendor.est-no <> "" THEN DO:
     IF /*lv-ship-no <> 0 AND*/ lv-ship-id <> quote-vendor.ship-id AND
        NOT CAN-FIND(FIRST bf-eb
                     WHERE bf-eb.company  EQ quote-vendor.company
                        AND bf-eb.est-no  EQ quote-vendor.est-no 
                        AND bf-eb.cust-no EQ quote-vendor.cust-no
                        AND bf-eb.ship-id NE lv-ship-id)
     THEN DO:
       FOR each bf-eb WHERE bf-eb.company = quote-vendor.company
                        AND bf-eb.est-no = quote-vendor.est-no 
                        AND bf-eb.cust-no = quote-vendor.cust-no:
         IF bf-eb.ship-id = lv-ship-id THEN DO:
            FIND FIRST shipto  WHERE shipto.company = quote-vendor.company
                                 AND shipto.cust-no = quote-vendor.cust-no
                                 AND shipto.ship-id = quote-vendor.ship-id 
                      NO-LOCK NO-ERROR.
            IF AVAIL shipto THEN ASSIGN bf-eb.ship-no = shipto.ship-no
                                        bf-eb.ship-id = quote-vendor.ship-id
                                        bf-eb.ship-name = quote-vendor.shipto[1]
                                        bf-eb.ship-addr[1] = quote-vendor.shipto[2]
                                        bf-eb.ship-addr[2] = quote-vendor.shipto[3]
                                        bf-eb.ship-city = shipto.ship-city
                                        bf-eb.ship-state = shipto.ship-state
                                        bf-eb.ship-zip = shipto.ship-zip
                                        .
         END.
       END.
     END.
     IF lv-sman <> quote-vendor.sman
     THEN FOR each bf-eb WHERE bf-eb.company = quote-vendor.company
                           AND bf-eb.est-no = quote-vendor.est-no :
          bf-eb.sman = quote-vendor.sman.        
     END.       
  END.  /* est-no <> "" */
  
  /* update rfqitem qty - start */

  IF quote-vendor.rfq NE '' THEN DO:
    FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
    IF AVAILABLE module AND module.is-used THEN DO:
      IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
        IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
        CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
        IF CONNECTED('rfq') THEN DO:
          FOR EACH bquote-vendor-item OF quote-vendor NO-LOCK,
              EACH bquote-vendor-qty NO-LOCK WHERE bquote-vendor-qty.company EQ bquote-vendor-item.company
                                       AND bquote-vendor-qty.loc EQ bquote-vendor-item.loc
                                       AND bquote-vendor-qty.q-no EQ bquote-vendor-item.q-no
                                       AND bquote-vendor-qty.line EQ bquote-vendor-item.line:
            RUN custom/rfq-qty.p (quote-vendor.company,quote-vendor.loc,quote-vendor.est-no,
                                  quote-vendor.rfq,bquote-vendor-item.part-no,bquote-vendor-qty.qty,
                                  bquote-vendor-qty.price,bquote-vendor-qty.uom,TODAY,bquote-vendor-qty.rels).
          END. /* each quote-vendor-item */
          DISCONNECT rfq.
        END. /* if connected */
      END. /* expire-date */
    END. /* avail module */
  END. /* if rfq */
  /* update rfqitem qty - end */

  /* copy */
  IF adm-new-record AND NOT adm-adding-record THEN DO:
     find first bf-quote-vendor use-index q-no where bf-quote-vendor.company = gcompany 
                             AND bf-quote-vendor.loc = gloc
                             AND bf-quote-vendor.q-no = v-prev-q-no no-lock no-error.
     ASSIGN quote-vendor.comment[1] = bf-quote-vendor.comment[1]
            quote-vendor.comment[2] = bf-quote-vendor.comment[2]
            quote-vendor.comment[3] = bf-quote-vendor.comment[3]
            quote-vendor.comment[4] = bf-quote-vendor.comment[4]
            quote-vendor.comment[5] = bf-quote-vendor.comment[5].

     FOR EACH bf-quote-vendor-item OF bf-quote-vendor NO-LOCK :
         CREATE quote-vendor-item.
         BUFFER-COPY bf-quote-vendor-item EXCEPT bf-quote-vendor-item.q-no TO quote-vendor-item.
         ASSIGN quote-vendor-item.q-no = quote-vendor.q-no.

         FOR EACH bf-quote-vendor-qty WHERE bf-quote-vendor-qty.company = bf-quote-vendor-item.company
                                AND bf-quote-vendor-qty.loc = bf-quote-vendor-item.loc
                                AND bf-quote-vendor-qty.q-no = bf-quote-vendor-item.q-no
                                AND bf-quote-vendor-qty.line = bf-quote-vendor-item.line NO-LOCK:
             CREATE quote-vendor-qty.
             BUFFER-COPY bf-quote-vendor-qty EXCEPT bf-quote-vendor-qty.q-no TO quote-vendor-qty.
             ASSIGN quote-vendor-qty.q-no = quote-vendor.q-no.

             FOR EACH bf-quotechg WHERE bf-quotechg.company eq bf-quote-vendor-qty.company
                                    AND bf-quotechg.loc eq bf-quote-vendor-qty.loc
                                    AND bf-quotechg.q-no eq bf-quote-vendor-qty.q-no
                                    AND ((bf-quotechg.line eq bf-quote-vendor-qty.line AND bf-quotechg.qty eq bf-quote-vendor-qty.qty) OR
                                        (bf-quotechg.LINE eq 0                 AND bf-quotechg.qty eq 0               )) NO-LOCK:
                 CREATE quotechg.
                 BUFFER-COPY bf-quotechg EXCEPT bf-quotechg.q-no TO quotechg.
                 ASSIGN quotechg.q-no = quote-vendor.q-no.
             END.   
         END.
     END.
  END. /* copy*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-qno as int no-undo.
  def buffer bf-hd for quote-vendor.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  /*find last quote-vendor use-index q-no where quote-vendor.company = gcompany and
                                   quote-vendor.loc = gloc no-lock no-error.
  li-next-qno = if avail quote-vendor then quote-vendor.q-no + 1 else 1.  */          
                       
  find first bf-hd use-index q-no where bf-hd.company = gcompany and
                                        bf-hd.loc = gloc no-lock no-error.          
                                        
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign /*quote-vendor.company = gcompany
         quote-vendor.loc = gloc
         quote-vendor.q-no = li-next-qno*/
         quote-vendor.quo-date = today.

  if avail bf-hd then assign quote-vendor.comment[1] = bf-hd.comment[1]
                             quote-vendor.comment[2] = bf-hd.comment[2]
                             quote-vendor.comment[3] = bf-hd.comment[3]
                             quote-vendor.comment[4] = bf-hd.comment[4]
                             quote-vendor.comment[5] = bf-hd.comment[5]
                             .          
  
  IF adm-new-record AND NOT adm-adding-record THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&ENABLED-FIELDS}.
  END.
  display quote-vendor.q-no with frame {&frame-name}.
  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ship-id LIKE quote-vendor.ship-id NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail quote-vendor then return.
  /*if quote-vendor.sman <> "" then do:
     find first sman where sman.sman = quote-vendor.sman:screen-value in frame {&frame-name}
          no-lock no-error.
     if avail sman then sman_desc:screen-value = sman.sname.

  end.
  if quote-vendor.terms <> "" then do:
     find first terms where terms.t-code = quote-vendor.terms:screen-value no-lock no-error.
     if avail terms then term_desc:screen-value = terms.dscr.
  end.
  */
  if quote-vendor.carrier <> "" then do:
     find first carrier where carrier.company = gcompany and 
                                     carrier.loc = gloc and
                                     carrier.carrier = quote-vendor.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                                     no-lock no-error.
     if avail carrier then carrier_desc:screen-value = carrier.dscr.
  end.
  if quote-vendor.del-zone <> "" then do:
     find first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = gloc and
                                     carr-mtx.carrier = quote-vendor.carrier:screen-value and
                                     carr-mtx.del-zone = quote-vendor.del-zone:screen-value
                                     no-lock no-error.
     if avail carr-mtx then zon_desc:screen-value = carr-mtx.del-dscr.
  end.
  if quote-vendor.sts = "O" then ls-status = "Ordered".
  else ls-status = "Quote".

  lv-ship-id = quote-vendor.ship-id.
  IF lv-ship-id = "" THEN DO:  /* due to eariler bug */
     FIND FIRST shipto WHERE shipto.company = quote-vendor.company
                         AND shipto.cust-no = quote-vendor.cust-no
                         AND shipto.ship-id = quote-vendor.ship-id NO-LOCK NO-ERROR.
     IF AVAIL shipto THEN lv-ship-id = shipto.ship-id.
  END.
  display ls-status lv-ship-id @ quote-vendor.ship-id with frame {&frame-name}.

  /*IF quote-vendor.sold-id:SCREEN-VALUE EQ "" THEN
    ASSIGN
     quote-vendor.sold-id:SCREEN-VALUE   = quote-vendor.cust-no
     quote-vendor.soldto[1]:SCREEN-VALUE = quote-vendor.billto[1]:SCREEN-VALUE
     quote-vendor.soldto[2]:SCREEN-VALUE = quote-vendor.billto[2]:SCREEN-VALUE
     quote-vendor.soldto[3]:SCREEN-VALUE = quote-vendor.billto[3]:SCREEN-VALUE
     quote-vendor.soldto[4]:SCREEN-VALUE = quote-vendor.billto[4]:SCREEN-VALUE.
   */  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE newRecord AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
   newRecord = adm-new-record.
  
  /* validation ===*/
   RUN valid-cust-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-ship-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-sold-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   DO WITH FRAME {&FRAME-NAME}:
     /*sman_desc:SCREEN-VALUE = ''.
     IF quote-vendor.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE '' THEN DO:
       FIND FIRST sman NO-LOCK WHERE sman.sman EQ quote-vendor.sman:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE sman THEN DO:
         MESSAGE 'Invalid Sales Rep. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quote-vendor.sman.
         RETURN NO-APPLY.
       END.
       sman_desc:SCREEN-VALUE = sman.sname.
     END.
     
     term_desc:SCREEN-VALUE = ''.
     IF quote-vendor.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE '' THEN DO:
       FIND FIRST terms NO-LOCK WHERE terms.t-code EQ quote-vendor.terms:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
         MESSAGE 'Invalid Terms. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quote-vendor.terms.
         RETURN NO-APPLY.
       END.
       term_desc:SCREEN-VALUE = terms.dscr.
     END.
     */
     
     carrier_desc:SCREEN-VALUE = ''.
     IF quote-vendor.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN DO:
       FIND FIRST carrier NO-LOCK WHERE carrier.company EQ gcompany
                                    AND carrier.loc EQ gloc
                                    AND carrier.carrier EQ quote-vendor.carrier:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
         MESSAGE 'Invalid Carrier Code. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quote-vendor.carrier.
         RETURN NO-APPLY.
       END.
       carrier_desc:SCREEN-VALUE = carrier.dscr.
     END.

     zon_desc:SCREEN-VALUE = ''.
     IF quote-vendor.del-zone:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN DO:
       FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ gcompany
                                     AND carr-mtx.loc EQ gloc
                                     AND carr-mtx.carrier EQ quote-vendor.carrier:SCREEN-VALUE
                                     AND carr-mtx.del-zone EQ quote-vendor.del-zone:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE carr-mtx THEN DO:
         MESSAGE 'Invalid Delivey Zone. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quote-vendor.del-zone.
         RETURN NO-APPLY.
       END.
       zon_desc:SCREEN-VALUE = carr-mtx.del-dscr.
     END.
   END.
  /* end of validation ==== */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT CAN-FIND(FIRST quote-vendor-item OF quote-vendor) THEN RUN create-line-items.
  
  IF newRecord THEN DO:
    {methods/run_link.i "RECORD-SOURCE" "resetQuery" (quote-vendor.q-no)}
    newRecord = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no V-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    /*FIND cust
        WHERE cust.company EQ gcompany
          AND cust.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
          AND cust.cust-no NE "TEMP"
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      ASSIGN
       quote-vendor.billto[1]:SCREEN-VALUE = cust.name
       quote-vendor.billto[2]:SCREEN-VALUE = cust.addr[1]
       quote-vendor.billto[3]:SCREEN-VALUE = cust.addr[2]
       quote-vendor.billto[4]:SCREEN-VALUE = cust.city + ", " +
                                        cust.state + "  " +
                                        cust.zip
       quote-vendor.sman:SCREEN-VALUE      = cust.sman
       quote-vendor.terms:SCREEN-VALUE     = cust.terms
       quote-vendor.contact:SCREEN-VALUE   = cust.contact
       quote-vendor.del-zone:SCREEN-VALUE  = cust.del-zone
       quote-vendor.carrier:SCREEN-VALUE   = cust.carrier.

      FIND FIRST shipto
          WHERE shipto.company EQ gcompany
            AND shipto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
            AND shipto.ship-id EQ quote-vendor.ship-id:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN
      FOR EACH shipto
          WHERE shipto.company EQ gcompany
            AND shipto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
            AND shipto.ship-id NE ""
          NO-LOCK:
        quote-vendor.ship-id:SCREEN-VALUE = shipto.ship-id.
        RUN new-ship-id.
        LEAVE.
      END.

      FIND FIRST soldto
          WHERE soldto.company EQ gcompany
            AND soldto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
            AND soldto.sold-id EQ quote-vendor.sold-id:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL soldto THEN
      FOR EACH soldto
          WHERE soldto.company EQ gcompany
            AND soldto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
            AND soldto.sold-id NE ""
          NO-LOCK:
        quote-vendor.sold-id:SCREEN-VALUE = soldto.sold-id.
        RUN new-sold-id.
        LEAVE.
      END.
    END.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-ship-id V-table-Win 
PROCEDURE new-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    /*FIND shipto
        WHERE shipto.company EQ gcompany
          AND shipto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
          AND shipto.ship-id EQ quote-vendor.ship-id:SCREEN-VALUE
          AND shipto.ship-id NE "TEMP" 
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       lv-ship-no                     = shipto.ship-no
       quote-vendor.shipto[1]:SCREEN-VALUE = shipto.ship-name
       quote-vendor.shipto[2]:SCREEN-VALUE = shipto.ship-addr[1]
       quote-vendor.shipto[3]:SCREEN-VALUE = shipto.ship-addr[2]
       quote-vendor.shipto[4]:SCREEN-VALUE = TRIM(shipto.ship-city) + ", " +
                                        TRIM(shipto.ship-state) + "  " +
                                        TRIM(shipto.ship-zip).
  */                                        
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sold-id V-table-Win 
PROCEDURE new-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    /*FIND soldto
        WHERE soldto.company EQ gcompany
          AND soldto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
          AND soldto.sold-id EQ quote-vendor.sold-id:SCREEN-VALUE
          AND soldto.sold-id NE "TEMP"
          AND soldto.sold-id NE "" 
        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN
       quote-vendor.soldto[1]:SCREEN-VALUE = soldto.sold-name
       quote-vendor.soldto[2]:SCREEN-VALUE = soldto.sold-addr[1]
       quote-vendor.soldto[3]:SCREEN-VALUE = soldto.sold-addr[2]
       quote-vendor.soldto[4]:SCREEN-VALUE = TRIM(soldto.sold-city) + ", " +
                                        TRIM(soldto.sold-state) + "  " +
                                        TRIM(soldto.sold-zip).
   */                                        
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file V-table-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-quote V-table-Win 
PROCEDURE print-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF quote-vendor.sman = "" THEN DO:
     MESSAGE "Invalid Sales Rep!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF quote-vendor.carrier = ""  THEN DO:
     MESSAGE "Invalid Carrier!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF quote-vendor.terms = "" THEN DO:
     MESSAGE "Invalid Terms!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  
  RUN est/r-quoprt.w (ROWID(quote-vendor)) .
  {methods/run_link.i "CONTAINER-SOURCE" "moveToTop"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetQuery V-table-Win 
PROCEDURE resetQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "RECORD-SOURCE" "resetQuery" (quote-vendor.q-no)}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report V-table-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  output to 
  */
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
  {src/adm/template/snd-list.i "quote-vendor"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    /*quote-vendor.cust-no:SCREEN-VALUE = CAPS(quote-vendor.cust-no:SCREEN-VALUE).

    IF quote-vendor.cust-no:SCREEN-VALUE NE "TEMP" THEN DO:
      FIND FIRST cust
          WHERE cust.company EQ gcompany
            AND cust.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE 
          NO-LOCK NO-ERROR.
      IF NOT AVAIL cust THEN DO:
        MESSAGE "Invalid Bill To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quote-vendor.cust-no.
        RETURN ERROR.
      END.

      ASSIGN
       quote-vendor.billto[1]:screen-value = cust.name
       quote-vendor.billto[2]:screen-value = cust.addr[1]
       quote-vendor.billto[3]:screen-value = cust.addr[2]
       quote-vendor.billto[4]:screen-value = cust.city + ", " +
                                        cust.state + "  " +
                                        cust.zip.
    END.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id V-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    /*quote-vendor.ship-id:SCREEN-VALUE = CAPS(quote-vendor.ship-id:SCREEN-VALUE).

    IF quote-vendor.ship-id:SCREEN-VALUE NE "TEMP" THEN DO:
      IF NOT CAN-FIND(FIRST shipto
                      WHERE shipto.company EQ gcompany
                        AND shipto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
                        AND shipto.ship-id EQ quote-vendor.ship-id:SCREEN-VALUE)
      THEN DO:
        MESSAGE "Invalid Ship To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quote-vendor.ship-id.
        RETURN ERROR.
      END.

      RUN new-ship-id.
    END.
    */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sold-id V-table-Win 
PROCEDURE valid-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    /*quote-vendor.sold-id:SCREEN-VALUE = CAPS(quote-vendor.sold-id:SCREEN-VALUE).

    IF quote-vendor.sold-id:SCREEN-VALUE NE "TEMP" AND
       quote-vendor.sold-id:SCREEN-VALUE NE ""     THEN DO:
      IF NOT CAN-FIND(FIRST soldto
                      WHERE soldto.company EQ gcompany
                        AND soldto.cust-no EQ quote-vendor.cust-no:SCREEN-VALUE
                        AND soldto.sold-id EQ quote-vendor.sold-id:SCREEN-VALUE)
      THEN DO:
        MESSAGE "Invalid Sold To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quote-vendor.sold-id.
        RETURN ERROR.
      END.

      RUN new-sold-id.
    END.
    */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

