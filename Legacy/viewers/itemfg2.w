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

&SCOPED-DEFINE itemfg2-maint itemfg2-maint
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

def var uom-list as cha init "C,CS,EA,L,M" no-undo.
DEF VAR v-prior-i-no AS CHAR NO-UNDO.
DEF VAR v-whseadded AS LOG NO-UNDO.
DEFINE VARIABLE h_w-inqord AS HANDLE      NO-UNDO.
&Scoped-define List-buttons btn_onh btn_ono btn_all
&Scoped-define List-nonreord itemfg.i-no itemfg.i-name itemfg.i-dscr itemfg.vend-no ~
     itemfg.vend-item itemfg.vend2-no itemfg.vend2-item itemfg.ord-policy ~
     itemfg.stocked itemfg.pur-man itemfg.isaset itemfg.alloc itemfg.pur-uom ~
     itemfg.beg-date

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.pur-uom itemfg.beg-date itemfg.vend-no itemfg.vend-item ~
itemfg.vend2-no itemfg.vend2-item itemfg.ord-policy itemfg.stocked ~
itemfg.pur-man itemfg.isaset itemfg.alloc 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define DISPLAYED-FIELDS itemfg.i-no itemfg.i-name itemfg.pur-uom itemfg.beg-date ~
itemfg.vend-no itemfg.vend-item itemfg.vend2-no itemfg.vend2-item ~
itemfg.ord-policy itemfg.stocked itemfg.pur-man itemfg.isaset itemfg.alloc 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg


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
DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 144 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg.i-no AT ROW 1.44 COL 14 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 27 BY .62
     itemfg.i-name AT ROW 1.44 COL 41 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 38 BY .62
     itemfg.pur-uom AT ROW 1.24 COL 97 COLON-ALIGNED 
          LABEL "Purchase UOM"
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
     itemfg.beg-date AT ROW 1.24 COL 124 COLON-ALIGNED 
          LABEL "Beg Bal Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     itemfg.vend-no AT ROW 2.91 COL 14 COLON-ALIGNED
          LABEL "Vendor 1"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     itemfg.vend-item AT ROW 2.91 COL 40 COLON-ALIGNED
          LABEL "Item No"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 
     itemfg.vend2-no AT ROW 4.1 COL 14 COLON-ALIGNED
          LABEL "Vendor 2"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     itemfg.vend2-item AT ROW 4.1 COL 40 COLON-ALIGNED
          LABEL "Item No"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 
     itemfg.ord-policy AT ROW 5.52 COL 23 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Reorder Point", yes,
"Lot Controlled", no
          SIZE 45 BY 1.1
     itemfg.stocked AT ROW 2.67 COL 68
          LABEL "Stocked?"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .95
     itemfg.pur-man AT ROW 2.67 COL 106 HELP
          "" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Purchased", yes,
"Manufactured", no
          SIZE 37 BY .95
     itemfg.isaset AT ROW 4.57 COL 68
          LABEL "Set Header?"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .95
     itemfg.alloc AT ROW 3.86 COL 106 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Assembled", no,
"Unassembled", yes,
"Assembled w/Part Receipts", ?
          SIZE 36 BY 2.62
     "Set Allocation" VIEW-AS TEXT
          SIZE 17 BY .95 AT ROW 4.57 COL 88
          FGCOLOR 9 
     "Reorder Policy" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 5.76 COL 4
          FGCOLOR 9 
     "Item Is" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 2.67 COL 95
          FGCOLOR 9 
     RECT-22 AT ROW 1 COL 1
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
         HEIGHT             = 5.71
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.i-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME F-Main
   EXP-LABEL                                                             */
/* SETTINGS FOR FILL-IN itemfg.pur-uom IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.beg-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX itemfg.isaset IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET itemfg.pur-man IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX itemfg.stocked IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.vend-item IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.vend-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.vend2-item IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.vend2-no IN FRAME F-Main
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


  case focus:name:
    when 'vend-no' or when 'vend2-no' then do:
      APPLY 'entry' TO FOCUS.
      run windows/l-vendno.w (gcompany, "", focus:screen-value, output char-val).
      if char-val <> "" then focus:screen-value = entry(1,char-val).

    end.
    when "pur-uom" then do:
      run windows/l-stduom.w (gcompany,uom-list, focus:screen-value, output char-val).
      if char-val <> "" then focus:screen-value = caps(entry(1,char-val)).
    end.
  end case.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.isaset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.isaset V-table-Win
ON VALUE-CHANGED OF itemfg.isaset IN FRAME F-Main /* Set Header? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      RUN SetPurMan(itemfg.isaset:SCREEN-VALUE = "Y").

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.ord-policy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.ord-policy V-table-Win
ON return OF itemfg.ord-policy IN FRAME F-Main /* Reorder Policy Code */
DO:
     apply "tab" to self.
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


&Scoped-define SELF-NAME itemfg.stocked
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.stocked V-table-Win
ON return OF itemfg.stocked IN FRAME F-Main /* Stocked? */
DO:
     apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.vend-no V-table-Win
ON LEAVE OF itemfg.vend-no IN FRAME F-Main /* Vendor 1 */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and itemfg.vend-no:screen-value <> "" and
       not can-find(first vend where vend.vend-no = itemfg.vend-no:screen-value)
    then do:
         message "Invalid Vendor. Try Help." view-as alert-box error .
         return no-apply.
    end.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.vend2-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.vend2-no V-table-Win
ON LEAVE OF itemfg.vend2-no IN FRAME F-Main /* Vendor 2 */
DO:
      {&methods/lValidateError.i YES}
      if lastkey <> -1 and itemfg.vend2-no:screen-value <> "" and
       not can-find(first vend where vend.vend-no = itemfg.vend2-no:screen-value)
    then do:
         message "Invalid Vendor. Try Help." view-as alert-box error .
         return no-apply.
    end.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME itemfg.vend2-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-uom V-table-Win
ON LEAVE OF itemfg.pur-uom IN FRAME F-Main /* Purchased UOM */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN pValidatePurUOM NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END. /* if lastkey */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{custom/getloc.i}

assign
 cocode = gcompany
 locode = gloc.

RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).

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
    DEF BUFFER bf-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

 /*Task# 04121312*/
 FIND FIRST fg-set WHERE fg-set.company = itemfg.company 
     AND fg-set.set-no = itemfg.i-no NO-LOCK NO-ERROR.

 IF AVAIL itemfg AND AVAIL fg-set THEN
   FOR EACH eb NO-LOCK      
       WHERE eb.company EQ itemfg.company
         AND eb.cust-no EQ itemfg.cust-no
         AND eb.stock-no EQ itemfg.i-no:

     FIND bf-eb WHERE ROWID(bf-eb) EQ ROWID(eb) EXCLUSIVE NO-WAIT NO-ERROR.
     IF AVAIL bf-eb THEN DO:
       ASSIGN bf-eb.pur-man = itemfg.pur-man.
     END.
   END. /* each eb */


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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&list-5}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.
  DEF VAR v-return AS LOG.
  DEF VAR v-alloc-save AS DEC NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  def var v-q-alloc like itemfg.q-alloc NO-UNDO.
  def var v-q-back  like itemfg.q-back NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL itemfg THEN DO:
    {sys/inc/oereordr.i}
    
    DO WITH TRANSACTION:
      FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL b-itemfg THEN
      b-itemfg.q-avail = b-itemfg.q-onh +
                         (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE b-itemfg.q-ono) -
                         b-itemfg.q-alloc.
      RELEASE b-itemfg.
      FIND CURRENT itemfg NO-LOCK.
    END.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit V-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_w-inqord) THEN
    DELETE OBJECT h_w-inqord.
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
   {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  do with frame {&frame-name}:
    if itemfg.vend-no:screen-value in frame {&frame-name} <> "" and
       not can-find(first vend where vend.vend-no = itemfg.vend-no:screen-value)
    then do:
         message "Invalid Vendor. Try Help." view-as alert-box error .
         apply 'entry' to itemfg.vend-no.
         return no-apply.
    end.
    if itemfg.vend2-no:screen-value <> "" and
       not can-find(first vend where vend.vend-no = itemfg.vend2-no:screen-value)
    then do:
         message "Invalid Vendor. Try Help." view-as alert-box error .
         apply "entry" to itemfg.vend2-no.
         return no-apply.
    end.
  end.   /* with frame */
  {&methods/lValidateError.i NO}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&list-5}.
  END.

  {methods/run_link.i "RECORD-TARGET" "local-open-query"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidatePurUOM V-table-Win 
PROCEDURE pValidatePurUOM :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        itemfg.pur-uom:SCREEN-VALUE = CAPS(itemfg.pur-uom:SCREEN-VALUE).
        IF NOT CAN-FIND(FIRST uom
                        WHERE uom.uom EQ itemfg.pur-uom:SCREEN-VALUE  
                          AND CAN-DO(uom-list, uom.uom)) THEN DO:
            MESSAGE
                TRIM(itemfg.pur-uom:LABEL) + " is invalid, try help..."       
            VIEW-AS ALERT-BOX ERROR.                                          
            RETURN ERROR.                                                         
        END. /* if not can-find */                                                                    
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

