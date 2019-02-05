&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: po\vi-poord.w

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
ASSIGN cocode = g_company
       locode = g_loc.

DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
def NEW shared var factor# as decimal no-undo.
def NEW shared var v-default-gl-log as log no-undo.
def NEW shared var v-default-gl-cha as cha no-undo.
def NEW shared var v-po-qty as log initial true no-undo.
def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.
DEF SHARED VAR lNewOrd AS LOG NO-UNDO.
DEF VAR lv-copy-from-po-num AS INT NO-UNDO.

RUN po/po-sysct.p .  /* for vars factor#.... need for d-poordl.w  */

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
&Scoped-define EXTERNAL-TABLES po-ord
&Scoped-define FIRST-EXTERNAL-TABLE po-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS po-ord.po-no po-ord.po-date po-ord.loc ~
po-ord.type 
&Scoped-define ENABLED-TABLES po-ord
&Scoped-define FIRST-ENABLED-TABLE po-ord
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS po-ord.po-no po-ord.po-date po-ord.loc ~
po-ord.type 
&Scoped-define DISPLAYED-TABLES po-ord
&Scoped-define FIRST-DISPLAYED-TABLE po-ord
&Scoped-Define DISPLAYED-OBJECTS v-tot-msf 

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
company||y|ASI.po-ord.company
Carrier||y|ASI.po-ord.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE v-tot-msf AS DECIMAL FORMAT ">>>>,>>9.999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     po-ord.po-no AT ROW 1.48 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FGCOLOR 0 
     po-ord.po-date AT ROW 1.48 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     po-ord.loc AT ROW 1.48 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     po-ord.type AT ROW 1.48 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     v-tot-msf AT ROW 1.48 COL 95 COLON-ALIGNED NO-LABEL
     po-ord.stat AT ROW 1.48 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     RECT-1 AT ROW 1 COL 1
     "MSF" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.71 COL 116
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.po-ord
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
         HEIGHT             = 17.14
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

/* SETTINGS FOR FILL-IN po-ord.stat IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       po-ord.stat:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN v-tot-msf IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "po-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ord"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-msf V-table-Win 
PROCEDURE display-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* po/podisdet.i */

  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-tot-msf LIKE v-tot-msf NO-UNDO.
  DEF VAR v-ord-qty AS dec NO-UNDO.
  def var lv-uom-list as cha init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" no-undo.
  DEF VAR pr-uom-list AS cha NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
  DEF VAR cons-uom-list AS CHA NO-UNDO INIT "M,LF,EA,LB,TON".
 

    {ce/msfcalc.i}
  v-tot-msf = 0.

  FOR EACH po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no   EQ po-ord.po-no
      NO-LOCK:

  FIND FIRST ITEM WHERE ITEM.company = cocode AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
  ASSIGN
    v-basis-w = IF AVAIL ITEM THEN ITEM.basis-w ELSE v-basis-w
    v-dep = IF AVAIL ITEM THEN ITEM.s-dep ELSE v-dep
   
    v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-ord-qty = po-ordl.ord-qty
    {po/calc10.i v-len}
    {po/calc10.i v-wid}.

  IF po-ordl.pr-qty-uom = "EA" THEN
     lv-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * po-ordl.ord-qty) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * po-ordl.ord-qty) / 1000).
  else do:
                  /*convert whatever the UOM is into "EACH" first*/
      
                    lv-tot-msf = 0.
                  if po-ordl.pr-qty-uom ne "EA" then do:
                        lv-tot-msf = 0.
                        run sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                               "EA",
                                               v-basis-w,
                                               v-len,
                                               v-wid,
                                               v-dep,
                                               v-ord-qty,
                                               output v-out-qty).

                        /*now convert from "EACH" into MSF*/   
                        lv-tot-msf = if v-corr
                          then
                            ((v-len * v-wid * .007 * v-out-qty) / 1000)
                          else
                            ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                       IF po-ordl.pr-qty-uom EQ "ROLL" THEN
                         lv-tot-msf = lv-tot-msf * (12 / v-len).
                  end. 
  end.
/*
  lv-cons-qty = v-ord-qty.
  IF po-ordl.cons-uom NE po-ordl.pr-qty-uom THEN
    RUN sys/ref/convquom.p(INPUT (po-ordl.pr-qty-uom),
                           INPUT (po-ordl.cons-uom),
                           v-basis-w, v-len, v-wid, v-dep,
                           lv-cons-qty,
                           OUTPUT lv-cons-qty).     
  

  lv-cons-cost = po-ordl.cost.

  IF po-ordl.cons-uom NE  po-ordl.pr-uom THEN
    IF  po-ordl.pr-uom EQ "L" THEN lv-cons-cost = lv-cons-cost / lv-cons-qty.
    ELSE
      RUN sys/ref/convcuom.p(INPUT (po-ordl.pr-uom),
                             INPUT (po-ordl.cons-uom),
                             v-basis-w, v-len, v-wid, v-dep,
                             lv-cons-cost,
                             OUTPUT lv-cons-cost).
  po-ordl.cons-cost:SCREEN-VALUE = STRING(lv-cons-cost).
  */
      v-tot-msf = v-tot-msf + round(lv-tot-msf,3).
  END. /* each po-ordl */
  DISP v-tot-msf WITH FRAME {&FRAME-NAME}.

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
  RUN display-msf.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "po-ord" "company"}
  {src/adm/template/sndkycas.i "Carrier" "po-ord" "Carrier"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "po-ord"}

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

