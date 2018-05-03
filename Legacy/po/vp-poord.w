&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: po\vp-poord.w

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

def var char-hdl as cha no-undo.
/* === vars for d-poordl.w ====*/
{methods/prgsecdt.i}

{sys/inc/VAR.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.
def NEW shared var factor# as decimal no-undo.
def NEW shared var v-default-gl-log as log no-undo.
def NEW shared var v-default-gl-cha as cha no-undo.
def NEW shared var v-po-qty as log initial true no-undo.
def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.

DEF TEMP-TABLE tt-po-ordl NO-UNDO LIKE po-ordl.

{windows/l-jobmt1.i NEW}
{oe/tt-item-qty-price.i}

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
&Scoped-define EXTERNAL-TABLES po-ord po-ordl
&Scoped-define FIRST-EXTERNAL-TABLE po-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ord, po-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-ord Btn-View Btn-Save Btn-Add Btn-copy ~
Btn-Delete btn-scores btn-recost 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE BUTTON Btn-Add 
     LABEL "&Add" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-copy 
     LABEL "&Copy" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-Delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON btn-recost 
     LABEL "&Recost Board" 
     SIZE 15 BY 1.29.

DEFINE BUTTON Btn-Save 
     LABEL "&Update" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON btn-scores 
     LABEL "Sc&ores" 
     SIZE 15 BY 1.29.

DEFINE BUTTON Btn-View 
     LABEL "&View" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-ord 
     LABEL "&Order#" 
     SIZE 15 BY 1.29
     FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn-ord AT ROW 1 COL 1
     Btn-View AT ROW 1 COL 16
     Btn-Save AT ROW 1 COL 31
     Btn-Add AT ROW 1 COL 46
     Btn-copy AT ROW 1 COL 61
     Btn-Delete AT ROW 1 COL 76
     btn-scores AT ROW 1 COL 91
     btn-recost AT ROW 1 COL 105 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.po-ord,ASI.po-ordl
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
         HEIGHT             = 7.05
         WIDTH              = 140.4.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Btn-Add:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-copy:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-Delete:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       btn-recost:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-Save:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       btn-scores:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-View:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".
ASSIGN 
       Btn-ord:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

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

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add V-table-Win
ON CHOOSE OF Btn-Add IN FRAME F-Main /* Add */
DO:
  DEF BUFFER b-po-ordl FOR po-ordl.

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li AS INT INIT 0 EXTENT 2 NO-UNDO.

  IF AVAIL po-ord THEN
  DO:
     FOR EACH b-po-ordl WHERE
         b-po-ordl.company EQ po-ord.company AND
         b-po-ordl.po-no EQ po-ord.po-no AND
         b-po-ordl.line NE 0
         NO-LOCK BY b-po-ordl.line:
       li[1] = li[1] + 1.
     END.
    
     DO WHILE TRUE:
       RUN po/d-poordl.w (?, po-ord.po-no, "add").
       FIND FIRST w-po-ordl NO-ERROR.
       IF AVAIL w-po-ordl THEN DELETE w-po-ordl.
       IF NOT CAN-FIND(FIRST w-po-ordl) THEN LEAVE.
     END.
    
     FOR EACH b-po-ordl WHERE
         b-po-ordl.company EQ po-ord.company AND
         b-po-ordl.po-no EQ po-ord.po-no AND
         b-po-ordl.line NE 0
         NO-LOCK BY b-po-ordl.line:
       ASSIGN
        li[2]    = li[2] + 1
        lv-rowid = ROWID(b-po-ordl).
     END.
    
     IF li[2] GT 0 AND (li[1] NE li[2] OR li[2] EQ 1) THEN DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
       RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
       RUN reopen-po-ord-query.
    
       RUN set-rec-key IN WIDGET-HANDLE(char-hdl).
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-copy V-table-Win
ON CHOOSE OF Btn-copy IN FRAME F-Main /* Copy */
DO:
   DEF BUFFER b-po-ordl FOR po-ordl.

   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF VAR li AS INT INIT 0 EXTENT 2 NO-UNDO.

   IF AVAIL po-ord THEN
   DO:
      FOR EACH b-po-ordl WHERE
          b-po-ordl.company EQ po-ord.company AND
          b-po-ordl.po-no EQ po-ord.po-no AND
          b-po-ordl.line NE 0
          NO-LOCK BY b-po-ordl.line:
        li[1] = li[1] + 1.
      END.
     
      find last b-po-ordl WHERE
           b-po-ordl.company EQ po-ord.company AND
           b-po-ordl.po-no EQ po-ord.po-no
           no-lock no-error.

      z = if avail b-po-ordl then b-po-ordl.line + 1 else 1.
      
      CREATE b-po-ordl.
      BUFFER-COPY po-ordl EXCEPT rec_key line rel-qty t-rel-qty t-inv-qty deleted t-rec-qty opened TO b-po-ordl.
      b-po-ordl.LINE = z.
     
      RUN po/d-poordl.w (RECID(b-po-ordl), po-ord.po-no, "Copy").
     
      FOR EACH b-po-ordl WHERE
          b-po-ordl.company EQ po-ord.company AND
          b-po-ordl.po-no EQ po-ord.po-no AND
          b-po-ordl.line NE 0
          NO-LOCK BY b-po-ordl.line:
        ASSIGN
         li[2]    = li[2] + 1
         lv-rowid = ROWID(b-po-ordl).
      END.
     
      IF li[2] GT 0 AND (li[1] NE li[2] OR li[2] EQ 1) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
        RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
     
        RUN reopen-po-ord-query.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete V-table-Win
ON CHOOSE OF Btn-Delete IN FRAME F-Main /* Delete */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run delete_item in widget-handle(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-recost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-recost V-table-Win
ON CHOOSE OF btn-recost IN FRAME F-Main /* Recost Board */
DO:
    IF AVAIL po-ord AND AVAIL po-ordl THEN DO:
        RUN po\RecostBoardPO.p(INPUT ROWID(po-ord),
                              INPUT YES).
        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
        RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(po-ordl)).
/*         RUN reopen-po-ord-query. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save V-table-Win
ON CHOOSE OF Btn-Save IN FRAME F-Main /* Update */
DO:
    DEF VAR ll AS LOG NO-UNDO.

    IF AVAIL po-ord AND AVAIL po-ordl THEN
    DO:
      IF po-ordl.stat NE "c" THEN do:   /*10231304*/
       EMPTY TEMP-TABLE tt-po-ordl.
      
       CREATE tt-po-ordl.
       BUFFER-COPY po-ordl TO tt-po-ordl.
      
       run po/d-poordl.w (recid(po-ordl), po-ord.po-no, "update") . 
      
       BUFFER-COMPARE tt-po-ordl TO po-ordl SAVE RESULT IN ll.
      
       IF NOT ll              AND
          po-ordl.stat NE "U" AND
          po-ordl.stat NE "C" AND
          po-ord.printed      AND
          po-ord.opened       THEN DO:
         FIND CURRENT po-ordl.
         po-ordl.stat = "U".
         FIND CURRENT po-ordl NO-LOCK.
       END.
      
       run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
       run reopen-query in widget-handle(char-hdl) (rowid(po-ordl)).
      
       RUN reopen-po-ord-query.
      END.
      ELSE DO:                              /*10231304*/
          IF po-ordl.stat EQ "c" THEN
              MESSAGE "Line Item is Closed, Update Not Allowed until Item is Reopened."
              VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-scores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-scores V-table-Win
ON CHOOSE OF btn-scores IN FRAME F-Main /* Scores */
DO:
  IF AVAIL po-ordl THEN
     run po/d-scores.w (ROWID(po-ordl)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
DO:
   IF AVAIL po-ord THEN
      run po/d-poordl.w (recid(po-ordl), po-ord.po-no, "view"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-ord V-table-Win
ON CHOOSE OF Btn-ord IN FRAME F-Main /* View */
DO:
   IF AVAIL po-ordl THEN
   FIND FIRST oe-ord WHERE oe-ord.company  = cocode
       AND oe-ord.ord-no = po-ordl.ord-no  NO-LOCK NO-ERROR.

   IF AVAIL oe-ord THEN
   RUN oe/w-inqord.w(ROWID(oe-ord),YES) .
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
  {src/adm/template/row-list.i "po-ord"}
  {src/adm/template/row-list.i "po-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ord"}
  {src/adm/template/row-find.i "po-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browser-dbclicked V-table-Win 
PROCEDURE browser-dbclicked :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   APPLY "choose" TO btn-view IN FRAME {&FRAME-NAME}.
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
  RUN po/po-sysct.p .  /* for vars factor#.... need for d-poordl.w  */

  DO WITH FRAME {&FRAME-NAME}: 
    IF NOT v-can-create THEN ASSIGN btn-add:SENSITIVE = NO.                                                          
    IF NOT v-can-update THEN btn-save:SENSITIVE = NO.
    IF NOT v-can-delete THEN btn-delete:SENSITIVE = NO.
    IF v-can-create AND Btn-Save:LABEL EQ "&Save" THEN btn-save:SENSITIVE = yes.
    /*IF NOT v-can-run THEN btn-save:SENSITIVE = NO. */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH w-po-ordl:
    DELETE w-po-ordl.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-po-ord-query V-table-Win 
PROCEDURE reopen-po-ord-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR no-undo.
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  IF AVAIL po-ordl THEN DO:
    lv-rowid = ROWID(po-ordl).
      
    run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
    run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).

    run reopen-query1 in widget-handle(char-hdl) (lv-rowid).

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
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
  {src/adm/template/snd-list.i "po-ord"}
  {src/adm/template/snd-list.i "po-ordl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-item V-table-Win 
PROCEDURE update-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run oe/d-oeitem.w (recid(oe-ordl), oe-ordl.ord-no,INPUT TABLE tt-item-qty-price,
                     "Update").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

