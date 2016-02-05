&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

{custom/globdefs.i}
{sys/inc/VAR.i "new shared"}
def var v-n-bol like oe-bolh.bol-no no-undo.
def var lv-ship-no like oe-bolh.ship-no no-undo.
ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE oe-bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-bolh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-bolh.bol-date oe-bolh.carrier ~
oe-bolh.trailer oe-bolh.frt-pay oe-bolh.ship-id oe-bolh.freight oe-bolh.cwt ~
oe-bolh.tot-wt oe-bolh.tot-pallets 
&Scoped-define ENABLED-TABLES oe-bolh
&Scoped-define FIRST-ENABLED-TABLE oe-bolh
&Scoped-define DISPLAYED-TABLES oe-bolh
&Scoped-define FIRST-DISPLAYED-TABLE oe-bolh
&Scoped-Define ENABLED-OBJECTS RECT-2 
&Scoped-Define DISPLAYED-FIELDS oe-bolh.bol-no oe-bolh.bol-date ~
oe-bolh.tot-qty oe-bolh.release# oe-bolh.carrier oe-bolh.trailer ~
oe-bolh.frt-pay oe-bolh.cust-no oe-bolh.ship-id oe-bolh.freight oe-bolh.cwt ~
oe-bolh.tot-wt oe-bolh.tot-pallets 
&Scoped-Define DISPLAYED-OBJECTS cust_name ship_name cust_addr1 ship_addr1 ~
cust_addr2 ship_addr2 cust_city cust_state cust_zip ship_city ship_state ~
ship_zip 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS oe-bolh.bol-no 

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
DEFINE VARIABLE cust_addr1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_addr2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_city AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE cust_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cust_zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE ship_addr1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_addr2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_city AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE ship_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ship_zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 143.6 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-bolh.bol-no AT ROW 1.24 COL 14 COLON-ALIGNED
          LABEL "BOL#" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-bolh.bol-date AT ROW 1.24 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.tot-qty AT ROW 1.24 COL 119 COLON-ALIGNED HELP
          "Total BOL Quantity"
          LABEL "Total BOL Qty"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          FGCOLOR 9 
     oe-bolh.release# AT ROW 2.43 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-bolh.carrier AT ROW 2.43 COL 65 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.trailer AT ROW 2.43 COL 119 COLON-ALIGNED
          LABEL "Trailer#"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.frt-pay AT ROW 3.86 COL 126 COLON-ALIGNED
          LABEL "Frt Payment"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-bolh.cust-no AT ROW 4.33 COL 14 COLON-ALIGNED HELP
          "Enter customer number."
          LABEL "Customer#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.ship-id AT ROW 4.33 COL 65 COLON-ALIGNED
          LABEL "Ship To#"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.freight AT ROW 4.81 COL 126 COLON-ALIGNED
          LABEL "Freight Cost"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     cust_name AT ROW 5.29 COL 14 COLON-ALIGNED NO-LABEL
     ship_name AT ROW 5.29 COL 65 COLON-ALIGNED NO-LABEL
     cust_addr1 AT ROW 6.24 COL 14 COLON-ALIGNED NO-LABEL
     oe-bolh.cwt AT ROW 6.24 COL 126 COLON-ALIGNED
          LABEL "Rate/100 Wt"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ship_addr1 AT ROW 6.29 COL 65 COLON-ALIGNED NO-LABEL
     cust_addr2 AT ROW 7.19 COL 14 COLON-ALIGNED NO-LABEL
     oe-bolh.tot-wt AT ROW 7.19 COL 126 COLON-ALIGNED
          LABEL "Total Weight"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     ship_addr2 AT ROW 7.29 COL 65 COLON-ALIGNED NO-LABEL
     cust_city AT ROW 8.14 COL 14 COLON-ALIGNED NO-LABEL
     cust_state AT ROW 8.14 COL 34 COLON-ALIGNED NO-LABEL
     cust_zip AT ROW 8.14 COL 38 COLON-ALIGNED NO-LABEL
     ship_city AT ROW 8.14 COL 65 COLON-ALIGNED NO-LABEL
     ship_state AT ROW 8.14 COL 85 COLON-ALIGNED NO-LABEL
     ship_zip AT ROW 8.14 COL 89 COLON-ALIGNED NO-LABEL
     oe-bolh.tot-pallets AT ROW 8.14 COL 126 COLON-ALIGNED
          LABEL "Total Pallets"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RECT-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-bolh
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
         HEIGHT             = 15
         WIDTH              = 143.6.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-bolh.bol-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN oe-bolh.carrier IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-bolh.cust-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN cust_addr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_addr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.cwt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.release# IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.ship-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ship_addr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_addr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.tot-pallets IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.tot-qty IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN oe-bolh.tot-wt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.trailer IN FRAME F-Main
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
    def var look-recid as recid no-undo. 
    case focus:name :
         when "cust-no" then do:
              run windows/l-custact.w (g_company, focus:screen-value in frame {&frame-name}, output char-val, output look-recid).
              if look-recid <> ? then do:
                 run display-cust-detail (look-recid).
              end.       
         end.  
         when "ship-id" then do:
              run windows/l-shipt2.w (g_company,g_loc, oe-bolh.cust-no:screen-value in frame {&frame-name}, focus:screen-value, output char-val, output look-recid).
              if look-recid <> ? then do:
                 run display-shipto-detail (look-recid).
              end.       
          end.  
          when "carrier" then do:
              run windows/l-carrie.w (g_company,g_loc, focus:screen-value, output char-val).
              if char-val <> "" then do:
                 focus:screen-value = entry(1,char-val).
              end.       
          end.  


    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}  /* asi field contents help */
SESSION:DATA-ENTRY-RETURN = YES.

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
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-bolh V-table-Win 
PROCEDURE delete-bolh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-do-bol AS LOG NO-UNDO.
for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    
    if oe-bolh.posted then oe-boll.deleted = yes.
    else delete oe-boll.

end.

DISABLE TRIGGERS FOR LOAD OF oe-relh.

{oe/bollrell.i}


find first oe-relh where oe-relh.company eq cocode
                     and oe-relh.release# eq oe-bolh.release#
                     exclusive-lock no-error.
if avail oe-relh THEN if v-do-bol then delete oe-relh.
                      else oe-relh.posted = no.


if oe-bolh.posted then do:
    oe-bolh.deleted = yes.
    RETURN ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-cust-detail V-table-Win 
PROCEDURE display-cust-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-recid as recid no-undo.
  find cust where recid(cust) = ip-recid no-lock no-error.
  if avail cust then do with frame {&frame-name} :

       assign oe-bolh.cust-no:screen-value   = cust.cust-no
              cust_name:screen-value = cust.name
              cust_addr1:screen-value   = cust.addr[1]
              cust_addr2:screen-value   = cust.addr[2]
              cust_city:screen-value      = cust.city
              cust_state:screen-value     = cust.state
              cust_zip:screen-value       = cust.zip.

  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-shipto-detail V-table-Win 
PROCEDURE display-shipto-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-recid as recid no-undo.
  find shipto where recid(ship) = ip-recid no-lock no-error.
  if avail shipto then do with frame {&frame-name} :
     assign oe-bolh.ship-id:screen-value   = shipto.ship-id
            ship_name:screen-value = shipto.ship-name
            ship_addr1:screen-value   = shipto.ship-addr[1]
            ship_addr2:screen-value   = shipto.ship-addr[2]
            ship_city:screen-value      = shipto.ship-city
            ship_state:screen-value     = shipto.ship-state
            ship_zip:screen-value       = shipto.ship-zip
            lv-ship-no = shipto.ship-no.
            
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-bol as int no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  find last oe-bolh use-index b-no no-lock no-error.
  li-next-bol = if avail oe-bolh then oe-bolh.b-no + 1 else 1.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find first oe-ctrl where oe-ctrl.company eq g_company exclusive.
  do while true:
     assign v-n-bol       = oe-ctrl.n-bol
            oe-ctrl.n-bol = v-n-bol + 1.
   
     if oe-ctrl.n-bol gt 999999 then oe-ctrl.n-bol = 1.  
     find first oe-bolh  where oe-bolh.company eq g_company
                         and oe-bolh.bol-no  eq v-n-bol
                use-index bol-no no-lock no-error.
     if not avail oe-bolh then leave.
  end.

  assign oe-bolh.company = g_company
         oe-bolh.loc = g_loc
         oe-bolh.b-no = li-next-bol
         oe-bolh.bol-date = today
         oe-bolh.bol-no = v-n-bol 
         .
         
  display oe-bolh.bol-no bol-date with frame {&frame-name}.       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}
  RUN delete-bolh NO-ERROR.

  /* Dispatch standard ADM method.                             */
  IF NOT error-status:ERROR THEN
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if avail oe-bolh then do:
      find cust where cust.company = oe-bolh.company and
                      cust.cust-no = oe-bolh.cust-no no-lock no-error.
      if avail cust then assign cust_name = cust.name
                                cust_addr1 = cust.addr[1]
                                cust_addr2 = cust.addr[2]
                                cust_city = cust.city
                                cust_state = cust.state
                                cust_zip = cust.zip.
      find first shipto where shipto.company = oe-relh.company
                          and shipto.cust-no = oe-relh.cust-no
                          and shipto.ship-id = oe-relh.ship-id
                          no-lock no-error.
      if avail shipto then assign ship_name = shipto.ship-name
                                  ship_addr1 = shipto.ship-addr[1]
                                  ship_addr2 = shipto.ship-addr[2]
                                  ship_city = shipto.ship-city
                                  ship_state = shipto.ship-state
                                  ship_zip   = shipto.ship-zip.                                                                               
   end.
   display cust_name cust_addr1 cust_addr2 cust_city cust_state cust_zip
           ship_name ship_addr1 ship_addr2 ship_city ship_state ship_zip
           with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 find first sys-ctrl where sys-ctrl.company eq g_company
                        and sys-ctrl.name    eq "BOLFMT"
                        no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     assign sys-ctrl.company  = g_company
            sys-ctrl.name     = "BOLFMT"
            sys-ctrl.descrip  = "Bill of Lading Format"
            sys-ctrl.char-fld = "ASI".
     message "System control record not found. Update BOL Print Format"
     update sys-ctrl.char-fld.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-update V-table-Win 
PROCEDURE release-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-bolh for oe-bolh .
  
  
  find bf-bolh where recid(bf-bolh) = recid(oe-bolh).

  if bf-bolh.trailer EQ "HOLD" then
    assign
     bf-bolh.trailer = ""
     bf-bolh.printed = yes.

  else
  if bf-bolh.trailer EQ "" then
    assign
     bf-bolh.trailer = "HOLD"
     bf-bolh.printed = no.

  DO WITH FRAME {&FRAME-NAME}:
    oe-bolh.trailer:SCREEN-VALUE = bf-bolh.trailer.
  END.

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
  {src/adm/template/snd-list.i "oe-bolh"}

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

