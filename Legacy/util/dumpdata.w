&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: util\dumpdata.w
  
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

DEF VAR v-dummy AS LOG NO-UNDO.
DEF VAR v-heading AS cha NO-UNDO.
DEF VAR v-delimiter AS cha FORM "x" NO-UNDO.
DEF VAR v-quote AS cha INIT '"' NO-UNDO.

v-delimiter = ",". /* "," or "~t"*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS table-list btn_select-all ~
table-selected-list btn_close-all-2 btn_add btn_remove btn_selected-all ~
btn_close-all Btn_Cancel btn_ok v-dumpfile 
&Scoped-Define DISPLAYED-OBJECTS table-list table-selected-list v-dumpfile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_add 
     LABEL "Add >>" 
     SIZE 17 BY .91
     FONT 6.

DEFINE BUTTON Btn_Cancel DEFAULT 
     LABEL "&Cancel" 
     SIZE 16 BY .95
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn_close-all 
     LABEL "Clear All >>" 
     SIZE 17 BY .95
     FONT 6.

DEFINE BUTTON btn_close-all-2 
     LABEL "<< Clear All" 
     SIZE 17 BY .95
     FONT 6.

DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "OK" 
     SIZE 16 BY .95
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn_remove 
     LABEL "<< Remove" 
     SIZE 17 BY .95
     FONT 6.

DEFINE BUTTON btn_select-all 
     LABEL "<< Select  All" 
     SIZE 17 BY .95
     FONT 6.

DEFINE BUTTON btn_selected-all 
     LABEL "Select  All >>" 
     SIZE 17 BY .95
     FONT 6.

DEFINE VARIABLE v-dumpfile AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\" 
     LABEL "Folder to Export Tables" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE table-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 43 BY 10 NO-UNDO.

DEFINE VARIABLE table-selected-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 43 BY 10 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     table-list AT ROW 2.67 COL 3 HELP
          "Jobs to be closed" NO-LABEL
     btn_select-all AT ROW 2.67 COL 50
     table-selected-list AT ROW 2.67 COL 73 NO-LABEL
     btn_close-all-2 AT ROW 3.62 COL 50
     btn_add AT ROW 5.29 COL 50
     btn_remove AT ROW 6.24 COL 50
     btn_selected-all AT ROW 7.91 COL 50
     btn_close-all AT ROW 8.86 COL 50
     Btn_Cancel AT ROW 10.29 COL 50 HELP
          "Use this function to CANCEL field selecition"
     btn_ok AT ROW 11.48 COL 50
     v-dumpfile AT ROW 13.62 COL 31 COLON-ALIGNED
     "Available Table" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 1.71 COL 12
          FONT 6
     "Table Selected" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 1.71 COL 73
          FONT 6
     SPACE(22.99) SKIP(12.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Machines"
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Export Machines */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "choose" TO btn_cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_add D-Dialog
ON CHOOSE OF btn_add IN FRAME D-Dialog /* Add >> */
DO:
    RUN add-list.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  APPLY "go" TO FRAME {&frame-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close-all D-Dialog
ON CHOOSE OF btn_close-all IN FRAME D-Dialog /* Clear All >> */
DO:
  RUN clear-list (table-selected-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close-all-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close-all-2 D-Dialog
ON CHOOSE OF btn_close-all-2 IN FRAME D-Dialog /* << Clear All */
DO:
  RUN clear-list (table-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR v-fin-qty AS INT NO-UNDO.
  DEF VAR v AS INT NO-UNDO.

  ASSIGN v-dumpfile .
  IF SUBSTRING(v-dumpfile,LENGTH(v-dumpfile),1) <> "/"
       AND SUBSTRING(v-dumpfile,LENGTH(v-dumpfile),1) <> "\"
  THEN v-dumpfile = v-dumpfile + "\".
  
  IF NOT v-process THEN
    MESSAGE "Are you sure you want to dump all the selected tables to " v-dumpfile " ?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN DO WITH FRAME {&FRAME-NAME}:
    SESSION:SET-WAIT-STATE("general").
    RUN dump-data.
    MESSAGE "Export is completed." VIEW-AS ALERT-BOX INFORMATION.

    SESSION:SET-WAIT-STATE("").
        
    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_remove D-Dialog
ON CHOOSE OF btn_remove IN FRAME D-Dialog /* << Remove */
DO:
  RUN remove-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_select-all D-Dialog
ON CHOOSE OF btn_select-all IN FRAME D-Dialog /* << Select  All */
DO:
  RUN SELECT-ALL (table-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_selected-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_selected-all D-Dialog
ON CHOOSE OF btn_selected-all IN FRAME D-Dialog /* Select  All >> */
DO:
  RUN SELECT-ALL (table-selected-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME table-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL table-list D-Dialog
ON DEFAULT-ACTION OF table-list IN FRAME D-Dialog
DO:
  APPLY "choose" TO btn_add.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME table-selected-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL table-selected-list D-Dialog
ON DEFAULT-ACTION OF table-selected-list IN FRAME D-Dialog
DO:
  APPLY "choose" TO btn_remove.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-list D-Dialog 
PROCEDURE add-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i AS INT NO-UNDO.
 DEF VAR v-list AS cha NO-UNDO.
 DEF VAR lv-num-items AS INT NO-UNDO.
 DEF VAR lv-item-list AS cha NO-UNDO.
 DEF VAR lv-screen-value AS cha NO-UNDO.

 DO i = 1 TO table-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF table-list:IS-SELECTED(i) AND
      /*(NOT CAN-DO(table-selected-list:LIST-ITEMS,table-list:ENTRY(i)) */
       (LOOKUP(ENTRY(i,table-list:list-items),table-selected-list:LIST-ITEMS) <= 0
       OR
       table-selected-list:NUM-ITEMS = 0 )
    THEN DO:
       v-dummy = table-selected-list:ADD-LAST(table-list:ENTRY(i)).
    END.
  END.
  lv-num-items = table-list:NUM-ITEMS IN FRAME {&FRAME-NAME}.
  lv-item-list = table-list:LIST-ITEMS.
  lv-screen-value = table-list:SCREEN-VALUE.
  DO i = 1 TO lv-num-items:
     IF lookup(ENTRY(i,lv-item-list),lv-screen-value) > 0 THEN
        /*IF table-list:IS-SELECTED(i)*/   table-list:DELETE(ENTRY(i,lv-item-list)).     
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-list D-Dialog 
PROCEDURE clear-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-handle AS WIDGET-HANDLE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
 /*
  DO i = 1 TO ip-handle:num-items:
      IF ip-handle:IS-SELECTED(i) THEN
          v-dummy = ip-handle:DESELECT-SELECTED-ROW(i) NO-ERROR.
  END.
 */ 
  ip-handle:SCREEN-VALUE = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-cust D-Dialog 
PROCEDURE dump-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-active AS cha NO-UNDO.
  DEF VAR v-inv-meth AS cha NO-UNDO.
  DEF VAR v-frtpay AS cha NO-UNDO.
  DEF VAR v-jdedw AS cha NO-UNDO.
  DEF VAR v-man-tax AS LOG NO-UNDO.

  v-heading = "Company,Customer#,Name,Status,Address,Address2,City,State,Zip,Email,Type,Contact,SalesRep,Phone," +
              "Fax,Terms,Cr Acct#,Credit Rating,Credit Limit,Order Limit,Discount%,Currency,Grace Days,$," +
              "Credit Hold,Finance Charges,Auto Reprice,EDI,Factored,Invoice Per,Taxable,Tax Code,Tax Resale#," +
              "Exp Date,Freight Payment,FOB,Partial Ship,Location,Carrier,Delivery Zone,Territory," +
              "Underrun%,Overrun%,Mark-Up,Whse Days,Pallet,Case/Bundle,No. Load tags"
              .
  

  OUTPUT TO VALUE (v-dumpfile + "Customer Master.csv").
  PUT UNFORMATTED v-heading SKIP.

  FOR EACH cust NO-LOCK.
     case cust.active:
         when "A" then v-active = "(A)ctive".
         when "I" then v-active = "(I)nactive".
         when "X" then v-active = "(X) Inhouse".
         when "S" then v-active = "(S)tatement".     
         when "E" then v-active = "(E)-Service".     
    end case.
    CASE cust.frt-pay:
         WHEN "B":U THEN v-frtpay = "Bill".
         WHEN "C":U THEN v-frtpay = "Collect".
         WHEN "P":U THEN v-frtpay = "Prepaid" .
         WHEN "T":U THEN v-frtpay = "3rd Party".
    END CASE.
    

    v-inv-meth = IF cust.inv-meth THEN "PO"
                 ELSE IF NOT cust.inv-meth  THEN "BOL"
                 ELSE "Group by Date".

     PUT UNFORMATTED
         '"' cust.company '"' v-delimiter
         '"' cust.cust-no '"' v-delimiter
         '"' cust.NAME   '"' v-delimiter
         '"' v-active '"' v-delimiter
         '"' cust.addr[1]   '"' v-delimiter
         '"' cust.addr[2]   '"' v-delimiter
         '"' cust.city   '"' v-delimiter
         '"' cust.state   '"' v-delimiter
         '"' cust.zip   '"' v-delimiter
         '"' cust.email   '"' v-delimiter
         '"' cust.TYPE   '"' v-delimiter
         '"' cust.contact   '"' v-delimiter
         '"' cust.sman   '"' v-delimiter
         '"' cust.area-code + phone   '"' v-delimiter
         '"' cust.fax   '"' v-delimiter
         '"' cust.terms   '"' v-delimiter
         '"' cust.cr-use   '"' v-delimiter
         '"' cust.cr-rating   '"' v-delimiter
         '"' cust.cr-lim   '"' v-delimiter
         '"' cust.ord-lim   '"' v-delimiter
         '"' cust.disc   '"' v-delimiter
         '"' cust.curr-code   '"' v-delimiter
         '"' cust.cr-hold-invdays   '"' v-delimiter
         '"' cust.cr-hold-invdue   '"' v-delimiter 
         '"' cust.cust-level   '"' v-delimiter
         '"' cust.cr-hold   '"' v-delimiter
         '"' cust.fin-chg   '"' v-delimiter
         '"' cust.auto-reprice   '"' v-delimiter
         '"' cust.an-edi-cust  '"' v-delimiter
         '"' cust.factored   '"' v-delimiter
         '"' v-inv-meth '"' v-delimiter
         '"' cust.SORT = "Y"   '"' v-delimiter
         '"' cust.tax-gr   '"' v-delimiter
         '"' cust.tax-id   '"' v-delimiter
         '"' cust.date-field[2]   '"' v-delimiter
         '"' v-frtpay   '"' v-delimiter
         '"' cust.fob-code   '"' v-delimiter
         '"' cust.ship-part   '"' v-delimiter
         '"' cust.loc   '"' v-delimiter
         '"' cust.carrier   '"' v-delimiter
         '"' cust.del-zone   '"' v-delimiter
         '"' cust.terr   '"' v-delimiter

         '"' cust.under-pct   '"' v-delimiter
         '"' cust.over-pct   '"' v-delimiter
         '"' cust.markup   '"' v-delimiter
         '"' cust.ship-days   '"' v-delimiter
         '"' cust.pallet   '"' v-delimiter
         '"' cust.case-bundle   '"' v-delimiter
         '"' cust.int-field[1]   '"' 
/*         '"' cust.   '"' v-delimiter
         '"' cust.   '"' v-delimiter
         '"' cust.   '"' v-delimiter
         '"' cust.   '"' v-delimiter
*/
         SKIP.
  END.
  OUTPUT CLOSE.

  v-heading = "Company,Customer#,Shipto id,Name,Address,Address2,City,State,Zip,Contact,Phone,Fax,JD Edw#," +
                     "Tax Code,Mandatory Tax?,Broker?,Billable?,Dock#,Dock Hours,Warehouse,Bin,Carrier,Zone,Pallet," +
                     "Notes1,Notes2,Notes3,notes4".

  OUTPUT TO VALUE (v-dumpfile + "Shipto.csv").
  PUT UNFORMATTED v-heading SKIP.

  FOR EACH shipto NO-LOCK:
      FIND FIRST reftable WHERE reftable.reftable EQ "JDEDWARDCUST#" 
                      AND reftable.company  EQ shipto.company       
                      AND reftable.loc      EQ ""              
                      AND reftable.code     EQ shipto.cust-no  
                      AND reftable.code2    EQ shipto.ship-id
                      NO-LOCK NO-ERROR. 
      v-jdedw = IF AVAIL reftable THEN reftable.dscr ELSE "".

      FIND FIRST reftable WHERE reftable.reftable EQ "shipto.mandatory-tax" 
                      AND reftable.company  EQ shipto.company         
                      AND reftable.loc      EQ ""                     
                      AND reftable.code     EQ shipto.cust-no         
                      AND reftable.code2    EQ shipto.ship-id
                      NO-LOCK NO-ERROR.
      v-man-tax = IF AVAIL reftable THEN reftable.val[1] EQ 1 ELSE NO.

      PUT UNFORMATTED
          v-quote shipto.company v-quote v-delimiter
          v-quote shipto.cust-no v-quote v-delimiter          
          v-quote shipto.ship-id v-quote v-delimiter
          v-quote shipto.ship-name v-quote v-delimiter
          v-quote shipto.ship-addr[1] v-quote v-delimiter
          v-quote shipto.ship-addr[2] v-quote v-delimiter          
          v-quote shipto.ship-city v-quote v-delimiter
          v-quote shipto.ship-state v-quote v-delimiter
          v-quote shipto.ship-zip v-quote v-delimiter  
          v-quote shipto.contact v-quote v-delimiter
          v-quote shipto.area-code + shipto.phone v-quote v-delimiter
          v-quote shipto.fax v-quote v-delimiter          
          v-quote v-jdedw v-quote v-delimiter          
          v-quote shipto.tax-code v-quote v-delimiter
          v-quote v-man-tax v-quote v-delimiter          
          v-quote shipto.broker v-quote v-delimiter
          v-quote shipto.bill v-quote v-delimiter  
          v-quote shipto.dock-loc v-quote v-delimiter
          v-quote shipto.dock-hour v-quote v-delimiter
          v-quote shipto.loc v-quote v-delimiter          
          v-quote shipto.loc-bin v-quote v-delimiter
          v-quote shipto.carrier v-quote v-delimiter
          v-quote shipto.dest-code v-quote v-delimiter          
          v-quote shipto.pallet v-quote v-delimiter
          v-quote shipto.notes[1] v-quote v-delimiter          
          v-quote shipto.notes[2] v-quote v-delimiter          
          v-quote shipto.notes[3] v-quote v-delimiter          
          v-quote shipto.notes[4] v-quote v-delimiter          
          SKIP.
  END.
  OUTPUT CLOSE.

  v-heading = "Company,Customer#,Soldto id,Name,Address,Address2,City,State,Zip".

  OUTPUT TO VALUE (v-dumpfile + "Soldto.csv").
  PUT UNFORMATTED v-heading SKIP.

  FOR EACH soldto NO-LOCK:
      PUT UNFORMATTED
          v-quote sold.company v-quote v-delimiter
          v-quote sold.cust-no v-quote v-delimiter
          v-quote sold.sold-id v-quote v-delimiter
          v-quote sold.sold-name v-quote v-delimiter
          v-quote sold.sold-addr[1] v-quote v-delimiter
          v-quote sold.sold-addr[2] v-quote v-delimiter
          v-quote sold.sold-city v-quote v-delimiter
          v-quote sold.sold-city v-quote v-delimiter
          v-quote sold.sold-state v-quote v-delimiter
          v-quote sold.sold-zip v-quote 
          SKIP.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-data D-Dialog 
PROCEDURE dump-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  /* "Customer Master,Order Header,Order Line,FG Master,Invoice Header,Invoice Line".*/

  DO i = 1 TO table-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
     CASE ENTRY(i,table-selected-list:LIST-ITEMS):
       /* for whole records exporting
         WHEN "Customer Master" THEN RUN dump-data2 ("cust").
         WHEN "Order Header" THEN RUN dump-data2 ("oe-ord").
         WHEN "Order Line" THEN RUN dump-data2 ("oe-ordl").
         WHEN "FG Master" THEN RUN dump-data2 ("itemfg").
         WHEN "Invoice Header" THEN RUN dump-data2 ("ar-inv").
         WHEN "Invoice Line" THEN RUN dump-data2 ("ar-invl").
       */  
         WHEN "Customer Master" THEN RUN dump-cust.
         WHEN "Order Header" THEN RUN dump-orderH.
         WHEN "Order Line" THEN RUN dump-orderL.
         WHEN "FG Master" THEN RUN dump-itemfg.
         WHEN "Invoice Header" THEN RUN dump-invoiceH.
         WHEN "Invoice Line" THEN RUN dump-invoiceL.
     END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-data2 D-Dialog 
PROCEDURE dump-data2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-table AS cha .
  DEF VAR v-first AS LOG INIT YES NO-UNDO.
  DEF VAR v-heading AS cha NO-UNDO.

  DEF VAR v-delimiter AS cha FORM "x" NO-UNDO.
  v-delimiter = "~t". /* "," or "~t"*/
  
  CASE ip-table:
      when "cust" THEN DO: {util/dumpdata.i "cust" "Customer Master.csv" }.  END.
      when "oe-ord" THEN DO: {util/dumpdata.i "oe-ord" "Order Header.csv" }. END.
      when "oe-ordl" THEN DO: {util/dumpdata.i "oe-ordl" "Order Line.csv"}. END.
      when "itemfg" THEN DO: {util/dumpdata.i "itemfg" "FG Master.csv"}. END.
      when "ar-inv" THEN DO: {util/dumpdata.i "ar-inv" "Invoice Header.csv"}. END.
      when "ar-invl" THEN DO: {util/dumpdata.i "ar-invl" "Invoice Line.csv"}. END.
  END CASE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-invoiceH D-Dialog 
PROCEDURE dump-invoiceH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  v-heading = "Company,Invoice#,Customer#,Customer Name,Ship To,PO#,Invoice Date,Due Date,Tax Code,Terms Code,Terms Description," +
              "Discount%,Disc Days,Carrier,Currency Code,Exchange Rate,Invoice Amount,Freight Amount,Tax Amount," +
              "Discount Amount,Amount Paid,Balance Due"
             .

  OUTPUT TO VALUE (v-dumpfile + "Invoice Header.csv").
  PUT UNFORMATTED v-heading SKIP.
  FOR EACH ar-inv NO-LOCK.
      PUT UNFORMATTED
          v-quote ar-inv.company v-quote v-delimiter
          v-quote ar-inv.inv-no v-quote v-delimiter
          v-quote ar-inv.cust-no v-quote v-delimiter
          v-quote ar-inv.cust-name v-quote v-delimiter
          v-quote ar-inv.ship-id v-quote v-delimiter
          v-quote ar-inv.po-no v-quote v-delimiter
          v-quote ar-inv.inv-date v-quote v-delimiter
          v-quote ar-inv.due-date v-quote v-delimiter
          v-quote ar-inv.tax-code v-quote v-delimiter
          v-quote ar-inv.terms v-quote v-delimiter
          v-quote ar-inv.terms-d v-quote v-delimiter          
          v-quote ar-inv.disc-% v-quote v-delimiter
          v-quote ar-inv.disc-days v-quote v-delimiter
          v-quote ar-inv.carrier v-quote v-delimiter
          v-quote ar-inv.curr-code[1] v-quote v-delimiter
          v-quote ar-inv.ex-rate v-quote v-delimiter
          v-quote ar-inv.gross v-quote v-delimiter          
          v-quote ar-inv.freight v-quote v-delimiter
          v-quote ar-inv.tax-amt v-quote v-delimiter
          v-quote ar-inv.disc-taken v-quote v-delimiter
          v-quote ar-inv.paid v-quote v-delimiter
          v-quote ar-inv.due v-quote v-delimiter
          SKIP.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-invoiceL D-Dialog 
PROCEDURE dump-invoiceL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  v-heading = "Invoice#,Line#,Account Number,Item#,Item Name,Item Description,Quantity,Cons. UOM,SqFt," +
              "Price,UOM,Amount,Amount MSF,SalesRep 1,% of Sales 1,Comm% 1,SalesRep 2,% of Sales 2,Comm% 2,SalesRep 3,% of Sales 3,Comm% 3," +
              "Miscellaneous,Billable"
             .

  OUTPUT TO VALUE (v-dumpfile + "Invoice Line.csv").
  PUT UNFORMATTED v-heading SKIP.
  FOR EACH ar-invl NO-LOCK.
      PUT UNFORMATTED
          v-quote ar-invl.inv-no v-quote v-delimiter
          v-quote ar-invl.LINE v-quote v-delimiter
          v-quote ar-invl.actnum v-quote v-delimiter
          v-quote ar-invl.i-no v-quote v-delimiter
          v-quote ar-invl.i-name v-quote v-delimiter
          v-quote ar-invl.i-dscr v-quote v-delimiter
          v-quote ar-invl.inv-qty v-quote v-delimiter
          v-quote ar-invl.cons-uom v-quote v-delimiter
          v-quote ar-invl.sf-sht v-quote v-delimiter
          v-quote ar-invl.unit-pr v-quote v-delimiter
          v-quote ar-invl.pr-qty-uom v-quote v-delimiter
          v-quote ar-invl.amt v-quote v-delimiter
          v-quote ar-invl.amt-msf v-quote v-delimiter
          v-quote ar-invl.sman[1] v-quote v-delimiter
          v-quote ar-invl.s-pct[1] v-quote v-delimiter
          v-quote ar-invl.s-comm[1] v-quote v-delimiter
          v-quote ar-invl.sman[2] v-quote v-delimiter
          v-quote ar-invl.s-pct[2] v-quote v-delimiter
          v-quote ar-invl.s-comm[2] v-quote v-delimiter
          v-quote ar-invl.sman[3] v-quote v-delimiter
          v-quote ar-invl.s-pct[3] v-quote v-delimiter
          v-quote ar-invl.s-comm[3] v-quote v-delimiter
          v-quote ar-invl.misc v-quote v-delimiter
          v-quote ar-invl.billable v-quote v-delimiter
          SKIP
          .

  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-itemfg D-Dialog 
PROCEDURE dump-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-pur-man AS cha NO-UNDO.
  DEF VAR v-stock-custom AS cha NO-UNDO.
  DEF VAR v-ship-meth AS cha NO-UNDO.
  DEF VAR v-status AS cha NO-UNDO.
  DEF VAR v-exempt-disc AS LOG NO-UNDO.

  v-heading = "Company,Item#,Cust Part#,Item Name,Desc1,Desc2,Desc3,Set Header,Purchased/Manufactured,Stock Item/Custom Box," +
              "Cust#,Taxable,Shipping Method,Status," +
              "Estimate#,Style,Die#,Plate#,CAD#,SPC/QC#,UPC#,Sell Price,UOM,Currency,FG Category,Warehouse,Bin," +
              "Count,Wt per 100,Freight Class,Inventory Class,Cycle Count Code,Production Code,Packing Note," +
              "Exempt from Customer Discount?,Std Mat'l Cost,Std Labor Cost,Std Var OH Cost,Std Fix OH Cost," +
              "Total Std Cost,Average Cost,Last Cost,Cost UOM"
              .

  OUTPUT TO VALUE (v-dumpfile + "FG Master.csv").
  PUT UNFORMATTED v-heading SKIP.
  FOR EACH itemfg NO-LOCK:
      v-pur-man = IF itemfg.pur-man THEN "Purchased"
                  ELSE "Manufactured".
      v-stock-custom = IF itemfg.i-code = "S" THEN "Stock Item"
                       ELSE IF itemfg.i-code = "C" THEN "Custom Box"
                       ELSE "".
      v-ship-meth = IF itemfg.ship-meth THEN "Case"
                    ELSE "Pallet".

/*       FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"           */
/*                    AND reftable.company  EQ itemfg.company                */
/*                    AND reftable.loc      EQ ""                            */
/*                    AND reftable.code     EQ itemfg.i-no NO-LOCK NO-ERROR. */
/*       v-status = IF AVAILABLE reftable THEN reftable.code2 ELSE "A".      */
      v-status = itemfg.stat.


        v-exempt-disc = itemfg.exempt-disc.


      PUT UNFORMATTED
          v-quote itemfg.company v-quote v-delimiter
          v-quote itemfg.i-no v-quote v-delimiter
          v-quote itemfg.part-no v-quote v-delimiter
          v-quote itemfg.i-name v-quote v-delimiter
          v-quote itemfg.part-dscr1 v-quote v-delimiter
          v-quote itemfg.part-dscr2 v-quote v-delimiter
          v-quote itemfg.part-dscr3 v-quote v-delimiter
          v-quote itemfg.isaset v-quote v-delimiter
          v-quote v-pur-man v-quote v-delimiter
          v-quote v-stock-custom v-quote v-delimiter
          v-quote itemfg.cust-no v-quote v-delimiter
          /*v-quote itemfg.cust-name v-quote v-delimiter*/
          v-quote itemfg.taxable v-quote v-delimiter
          v-quote v-ship-meth v-quote v-delimiter
          v-quote v-status v-quote v-delimiter

          v-quote itemfg.est-no v-quote v-delimiter
          v-quote itemfg.style v-quote v-delimiter
          v-quote itemfg.die-no v-quote v-delimiter
          v-quote itemfg.plate-no v-quote v-delimiter
          v-quote itemfg.cad-no v-quote v-delimiter
          v-quote itemfg.spc-no v-quote v-delimiter
          v-quote itemfg.upc-no v-quote v-delimiter
          v-quote itemfg.sell-price v-quote v-delimiter
          v-quote itemfg.sell-uom v-quote v-delimiter
          v-quote itemfg.curr-code[1] v-quote v-delimiter
          v-quote itemfg.procat v-quote v-delimiter
          v-quote itemfg.def-loc v-quote v-delimiter
          v-quote itemfg.def-loc-bin v-quote v-delimiter

          v-quote itemfg.case-COUNT v-quote v-delimiter
          v-quote itemfg.weight-100 v-quote v-delimiter
          v-quote itemfg.frt-class v-quote v-delimiter
          v-quote itemfg.class v-quote v-delimiter
          v-quote itemfg.cc-code v-quote v-delimiter
          v-quote itemfg.prod-code v-quote v-delimiter
          v-quote itemfg.prod-notes v-quote v-delimiter
          v-quote v-exempt-disc v-quote v-delimiter
          v-quote itemfg.std-mat-cost v-quote v-delimiter
          v-quote itemfg.std-lab-cost v-quote v-delimiter
          v-quote itemfg.std-var-cost v-quote v-delimiter
          v-quote itemfg.std-fix-cost v-quote v-delimiter
          v-quote itemfg.total-std-cost v-quote v-delimiter
          v-quote itemfg.avg-cost v-quote v-delimiter
          v-quote itemfg.last-cost v-quote v-delimiter
          v-quote itemfg.prod-uom v-quote 
          /*v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
          v-quote itemfg. v-quote v-delimiter
           */
          SKIP
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-orderH D-Dialog 
PROCEDURE dump-orderH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  v-heading = "Company,Order#,Estimate#,Job#,Last User,Status,Bill To,Bill To Name,Bill To Address1,Bill To Address2,Bill To City,Bill To State, Bill To Zip," +
              "Sold To,Sold To Name,Sold To Address1,Sold To Address2,Sold To City,Sold To State,Sold To Zip," +
              "Order Date,Due Date,Last Ship Date,Prod Date,Cust PO#,Contact,Previous Order,Overrun%,Underrun%,Pay Terms,Terms Description,Tax Code," +
              "SalesRep1,SalesRep Name1,% of Sales1,Comm%1,SalesRep2,SalesRep Name2,% of Sales2,Comm%2,SalesRep3,SalesRep Name3,% of Sales3,Comm%3," +
              "Freight Charge,Carrier,FOB,Payment Type,Expiration Date,Account#,Ref#"
              .

  OUTPUT TO VALUE (v-dumpfile + "Order Header.csv").
  PUT UNFORMATTED v-heading SKIP.
  FOR EACH oe-ord NO-LOCK.
      PUT  UNFORMATTED 
           v-quote oe-ord.company v-quote v-delimiter
           v-quote oe-ord.ord-no v-quote v-delimiter
           v-quote oe-ord.est-no v-quote v-delimiter
           v-quote oe-ord.job-no + "-" + STRING(oe-ord.job-no2,"99") v-quote v-delimiter
           v-quote oe-ord.user-id v-quote v-delimiter
           v-quote oe-ord.stat v-quote v-delimiter
           v-quote oe-ord.cust-no v-quote v-delimiter
           v-quote oe-ord.cust-name v-quote v-delimiter
           v-quote oe-ord.addr[1] v-quote v-delimiter 
           v-quote oe-ord.addr[2] v-quote v-delimiter
           v-quote oe-ord.city v-quote v-delimiter
           v-quote oe-ord.state v-quote v-delimiter
           v-quote oe-ord.zip v-quote v-delimiter 
           v-quote oe-ord.sold-id v-quote v-delimiter
           v-quote oe-ord.sold-name v-quote v-delimiter
           v-quote oe-ord.sold-addr[1] v-quote v-delimiter
           v-quote oe-ord.sold-addr[2] v-quote v-delimiter
           v-quote oe-ord.sold-city v-quote v-delimiter
           v-quote oe-ord.sold-state v-quote v-delimiter
           v-quote oe-ord.sold-zip v-quote v-delimiter
           v-quote oe-ord.ord-date v-quote v-delimiter
           v-quote oe-ord.due-code + " " + string(oe-ord.due-date,"99/99/9999") v-quote v-delimiter
           v-quote oe-ord.last-date v-quote v-delimiter
           v-quote oe-ord.prod-date v-quote v-delimiter
           v-quote oe-ord.po-no v-quote v-delimiter
           v-quote oe-ord.contact v-quote v-delimiter
           v-quote oe-ord.pord-no v-quote v-delimiter
           v-quote oe-ord.over-pct v-quote v-delimiter
           v-quote oe-ord.under-pct v-quote v-delimiter
           v-quote oe-ord.terms v-quote v-delimiter
           v-quote oe-ord.terms-d v-quote v-delimiter
           v-quote oe-ord.tax-gr v-quote v-delimiter
           v-quote oe-ord.sman[1] v-quote v-delimiter
           v-quote oe-ord.sname[1] v-quote v-delimiter
           v-quote oe-ord.s-pct[1] v-quote v-delimiter
           v-quote oe-ord.s-comm[1] v-quote v-delimiter
           v-quote oe-ord.sman[2] v-quote v-delimiter
           v-quote oe-ord.sname[2] v-quote v-delimiter
           v-quote oe-ord.s-pct[2] v-quote v-delimiter
           v-quote oe-ord.s-comm[2] v-quote v-delimiter
           v-quote oe-ord.sman[3] v-quote v-delimiter
           v-quote oe-ord.sname[3] v-quote v-delimiter
           v-quote oe-ord.s-pct[3] v-quote v-delimiter
           v-quote oe-ord.s-comm[3] v-quote v-delimiter
           v-quote oe-ord.frt-pay v-quote v-delimiter
           v-quote oe-ord.carrier v-quote v-delimiter
           v-quote oe-ord.fob-code v-quote v-delimiter
           v-quote oe-ord.cc-type v-quote v-delimiter
           v-quote oe-ord.cc-expiration v-quote v-delimiter
           v-quote oe-ord.cc-num v-quote v-delimiter
           v-quote oe-ord.cc-auth v-quote v-delimiter
           SKIP.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-orderL D-Dialog 
PROCEDURE dump-orderL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-heading = "Company,Order#,Line#,Estimate#,Job#,Quantity,FG Item#,Cust Part#,Item Name,Description1,Description2," +
             "Price,UOM,Tax,Discount,Qty/Unit,Total Price,Partial,Cost/M,Units/Pallet,Cust PO#,Ln#,Board PO#," +
             "Board Vendor,Priority,Due Date,Priority,Promise Date," +
             "SalesRep 1,% of Sales 1,Comm% 1,SalesRep 2,% of Sales 2,Comm% 2,SalesRep 3,% of Sales 3,Comm% 3," +
             "Type,Overrun%,Underrun%,"
             .

  OUTPUT TO VALUE (v-dumpfile + "Order Line.csv").
  PUT UNFORMATTED v-heading SKIP.
  FOR EACH oe-ordl NO-LOCK.
      PUT UNFORMATTED
          v-quote oe-ordl.company v-quote v-delimiter
          v-quote oe-ordl.ord-no v-quote v-delimiter
          v-quote oe-ordl.LINE v-quote v-delimiter
          v-quote oe-ordl.est-no v-quote v-delimiter
          v-quote oe-ordl.job-no + "-" string(oe-ordl.job-no2,"99") v-quote v-delimiter
          v-quote oe-ordl.qty v-quote v-delimiter
          v-quote oe-ordl.i-no v-quote v-delimiter
          v-quote oe-ordl.part-no v-quote v-delimiter
          v-quote oe-ordl.i-name v-quote v-delimiter
          v-quote oe-ordl.part-dscr1 v-quote v-delimiter
          v-quote oe-ordl.part-dscr2 v-quote v-delimiter
          v-quote oe-ordl.price v-quote v-delimiter
          v-quote oe-ordl.pr-uom v-quote v-delimiter
          v-quote oe-ordl.tax v-quote v-delimiter
          v-quote oe-ordl.disc v-quote v-delimiter
          v-quote oe-ordl.cas-cnt v-quote v-delimiter
          v-quote oe-ordl.t-price v-quote v-delimiter
          v-quote oe-ordl.partial v-quote v-delimiter
          v-quote oe-ordl.cost v-quote v-delimiter
          v-quote oe-ordl.cases-unit v-quote v-delimiter
          v-quote oe-ordl.po-no v-quote v-delimiter
          v-quote oe-ordl.e-num v-quote v-delimiter
          v-quote oe-ordl.po-no-po v-quote v-delimiter
          v-quote oe-ordl.vend-no v-quote v-delimiter
          v-quote oe-ordl.req-code v-quote v-delimiter
          v-quote oe-ordl.req-date v-quote v-delimiter
          v-quote oe-ordl.prom-code v-quote v-delimiter
          v-quote oe-ordl.prom-date v-quote v-delimiter
          v-quote oe-ordl.s-man[1] v-quote v-delimiter
          v-quote oe-ordl.s-pct[1] v-quote v-delimiter
          v-quote oe-ordl.s-comm[1] v-quote v-delimiter
          v-quote oe-ordl.s-man[2] v-quote v-delimiter
          v-quote oe-ordl.s-pct[2] v-quote v-delimiter
          v-quote oe-ordl.s-comm[2] v-quote v-delimiter
          v-quote oe-ordl.s-man[3] v-quote v-delimiter
          v-quote oe-ordl.s-pct[3] v-quote v-delimiter
          v-quote oe-ordl.s-comm[3] v-quote v-delimiter
          v-quote oe-ordl.type-code v-quote v-delimiter
          v-quote oe-ordl.over-pct v-quote v-delimiter
          v-quote oe-ordl.under-pct v-quote v-delimiter
          SKIP.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-buttons D-Dialog 
PROCEDURE enable-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&frame-name}:
/*    DISABLE btn_close btn_open btn_close-all btn_open-all.

    IF close-list:NUM-ITEMS GT 0 THEN ENABLE btn_open  btn_open-all.
    IF open-list:NUM-ITEMS  GT 0 THEN ENABLE btn_close btn_close-all.
    */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY table-list table-selected-list v-dumpfile 
      WITH FRAME D-Dialog.
  ENABLE table-list btn_select-all table-selected-list btn_close-all-2 btn_add 
         btn_remove btn_selected-all btn_close-all Btn_Cancel btn_ok v-dumpfile 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-screen D-Dialog 
PROCEDURE init-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  RUN load-list.

  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY table-list table-selected-list.
  END.

  RUN enable-buttons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-list D-Dialog 
PROCEDURE load-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-list AS CHAR NO-UNDO.
  
  v-list = "Customer Master,Order Header,Order Line,FG Master,Invoice Header,Invoice Line".
  table-list:LIST-ITEMS IN FRAME {&FRAME-NAME} = v-list.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN init-screen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remove-list D-Dialog 
PROCEDURE remove-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR i AS INT NO-UNDO.
 DEF VAR v-list AS cha NO-UNDO.
 DEF VAR lv-num-items AS INT NO-UNDO.
 DEF VAR lv-item-list AS cha NO-UNDO.
 DEF VAR lv-screen-value AS cha NO-UNDO.

 DO i = 1 TO table-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF table-selected-list:IS-SELECTED(i)
       THEN v-dummy = table-list:ADD-LAST(table-selected-list:ENTRY(i)).
  END.
  lv-num-items = table-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}.
  lv-item-list = table-selected-list:LIST-ITEMS.
  lv-screen-value = table-selected-list:SCREEN-VALUE.
  DO i = 1 TO lv-num-items:
     IF lookup(ENTRY(i,lv-item-list),lv-screen-value) > 0 
        THEN table-selected-list:DELETE(ENTRY(i,lv-item-list)).     
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-all D-Dialog 
PROCEDURE select-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-handle AS WIDGET-HANDLE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

/*  DO i = 1 TO ip-handle:num-items:
     ip-handle:SELECTED NO-ERROR.
  END.
*/
  ip-handle:SCREEN-VALUE = ip-handle:LIST-ITEMS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

