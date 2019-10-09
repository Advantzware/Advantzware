&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\v-oeqtys.w

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
{sys/inc/var.i "new shared"}
assign cocode = g_company
       locode = g_loc.

DEF VAR ls-onh AS CHAR NO-UNDO.
DEF VAR ls-ono AS CHAR NO-UNDO.
DEF VAR ls-all AS CHAR NO-UNDO.
DEF VAR ls-bor AS CHAR NO-UNDO.
DEF VAR ls-ava AS CHAR NO-UNDO.
DEF VAR ls-reo AS CHAR NO-UNDO.

DEF VAR li-onh AS INT NO-UNDO.
DEF VAR li-ono AS INT NO-UNDO.
DEF VAR li-all AS INT NO-UNDO.
DEF VAR li-bor AS INT NO-UNDO.
DEF VAR li-ava AS INT NO-UNDO.
DEF VAR li-reo AS INT NO-UNDO.

{sys/inc/oereordr.i}

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
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-38 RECT-40 btn_onh btn_ono btn_all ~
btn_bor btn_ava 
&Scoped-Define DISPLAYED-OBJECTS btn_reo 

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
DEFINE BUTTON btn_all 
     LABEL "Allocated" 
     SIZE 44 BY 1.19.

DEFINE BUTTON btn_ava 
     LABEL "Available" 
     SIZE 44 BY 1.19.

DEFINE BUTTON btn_bor 
     LABEL "Backordered" 
     SIZE 44 BY 1.19.

DEFINE BUTTON btn_onh 
     LABEL "On Hand" 
     SIZE 44 BY 1.19.

DEFINE BUTTON btn_ono 
     LABEL "On Order" 
     SIZE 44 BY 1.19.

DEFINE VARIABLE btn_reo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 43.6 BY 1.1 NO-UNDO.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152 BY 2.86.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn_onh AT ROW 1.24 COL 20
     btn_ono AT ROW 1.24 COL 64
     btn_all AT ROW 1.24 COL 108
     btn_bor AT ROW 2.43 COL 20
     btn_ava AT ROW 2.43 COL 64
     btn_reo AT ROW 2.48 COL 106 COLON-ALIGNED NO-LABEL
     "FG Item Totals" VIEW-AS TEXT
          SIZE 18 BY 2.38 AT ROW 1.24 COL 2
          FGCOLOR 9 
     RECT-38 AT ROW 1 COL 1
     RECT-40 AT ROW 2.43 COL 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl
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
         HEIGHT             = 12.67
         WIDTH              = 152.6.
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

/* SETTINGS FOR FILL-IN btn_reo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn_reo:PRIVATE-DATA IN FRAME F-Main     = 
                "Reorder".

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

&Scoped-define SELF-NAME btn_all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_all V-table-Win
ON CHOOSE OF btn_all IN FRAME F-Main /* Allocated */
DO:
  IF li-all NE 0 THEN
  RUN oe/w-inqord.w (ROWID(itemfg), YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_onh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_onh V-table-Win
ON CHOOSE OF btn_onh IN FRAME F-Main /* On Hand */
DO:
  IF li-onh NE 0 THEN
  RUN fg/w-inqonh.w (ROWID(itemfg), NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ono
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ono V-table-Win
ON CHOOSE OF btn_ono IN FRAME F-Main /* On Order */
DO:
  IF li-ono NE 0 THEN DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
          AND job-hdr.opened  EQ YES
          AND CAN-FIND(FIRST job WHERE job.company EQ job-hdr.company
                                   AND job.job     EQ job-hdr.job
                                   AND job.job-no  EQ job-hdr.job-no
                                   AND job.job-no2 EQ job-hdr.job-no2)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN
    RUN jc/w-inqjob.w (ROWID(itemfg), YES).
    ELSE DO:
        FIND FIRST fg-set WHERE fg-set.company EQ itemfg.company
                            AND fg-set.part-no EQ itemfg.i-no
                          NO-LOCK NO-ERROR.
        IF AVAIL fg-set THEN
        RUN jc/w-inqjbc.w (ROWID(itemfg), YES).
    END.
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ itemfg.company
          AND po-ordl.i-no      EQ itemfg.i-no
          AND po-ordl.item-type EQ NO
          AND po-ordl.opened    EQ YES
          AND CAN-FIND(FIRST po-ord WHERE po-ord.company EQ po-ordl.company
                                      AND po-ord.po-no   EQ po-ordl.po-no)
        NO-LOCK NO-ERROR.
    IF AVAIL po-ordl THEN
    RUN po/w-inqpo.w (ROWID(itemfg), YES).
  END.
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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-fgqtys V-table-Win 
PROCEDURE calc-fgqtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-use-rel as log no-undo.
  

  find first itemfg where itemfg.company = oe-ordl.company and
                          itemfg.i-no = oe-ordl.i-no
                          no-lock no-error.                          
  if not avail itemfg then do:
     assign li-onh = 0
              li-ono = 0
              li-all = 0
              li-bor = 0
              li-ava = 0
              li-reo = 0.
     return error.
  end.
 
  IF oereordr-log OR oereordr-log EQ ? THEN
      RUN oe/oereordr.p (BUFFER itemfg, INPUT oereordr-log, OUTPUT li-all).
  ELSE li-all = itemfg.q-alloc. 

  if avail itemfg then
    assign
     li-onh = itemfg.q-onh
     li-ono = itemfg.q-ono
     /*li-all = itemfg.q-alloc */
     li-bor = itemfg.q-back
     li-ava = itemfg.q-onh +
              (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
              li-all
     li-reo = itemfg.ord-level.

  else assign li-onh = 0
              li-ono = 0
              li-all = 0
              li-bor = 0
              li-ava = 0
              li-reo = 0.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-qtys V-table-Win 
PROCEDURE get-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-onh AS INT NO-UNDO.
  DEF OUTPUT PARAM op-ono AS INT NO-UNDO.
  DEF OUTPUT PARAM op-all AS INT NO-UNDO.
  DEF OUTPUT PARAM op-bor AS INT NO-UNDO.
  DEF OUTPUT PARAM op-ava AS INT NO-UNDO.
  DEF OUTPUT PARAM op-reo AS INT NO-UNDO.


  ASSIGN
   op-onh = li-onh
   op-ono = li-ono
   op-all = li-all
   op-bor = li-bor
   op-ava = li-ava
   op-reo = li-reo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    IF ls-onh EQ "" THEN
      ASSIGN
       ls-onh = btn_onh:LABEL
       ls-ono = btn_ono:LABEL
       ls-all = btn_all:LABEL
       ls-bor = btn_bor:LABEL
       ls-ava = btn_ava:LABEL
       ls-reo = btn_reo:PRIVATE-DATA.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    RUN calc-fgqtys NO-ERROR.

    ASSIGN
     btn_onh:LABEL        = TRIM(ls-onh) + ": " + TRIM(STRING(li-onh,"->>,>>>,>>9"))
     btn_ono:LABEL        = TRIM(ls-ono) + ": " + TRIM(STRING(li-ono,"->>,>>>,>>9"))
     btn_all:LABEL        = TRIM(ls-all) + ": " + TRIM(STRING(li-all,"->>>,>>>,>>9"))     /*Task# 08191306*/      
     btn_bor:LABEL        = TRIM(ls-bor) + ": " + TRIM(STRING(li-bor,"->>,>>>,>>9"))
     btn_ava:LABEL        = TRIM(ls-ava) + ": " + TRIM(STRING(li-ava,"->>>,>>>,>>9"))     /*Task# 08191306*/
     btn_reo:SCREEN-VALUE = TRIM(ls-reo) + ": " + TRIM(STRING(li-reo,"->>,>>>,>>9"))
     {sys/inc/ctrtext.i btn_reo:SCREEN-VALUE btn_reo:WIDTH}
     btn_reo:BGCOLOR      = IF (li-onh + li-ono - li-all) LE li-reo AND li-reo GT 0
                            THEN 12 ELSE ?.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oe-qtys-target",OUTPUT char-hdl).
    DO li = 1 TO NUM-ENTRIES(char-hdl):
      IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
        RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ("display-fields").
    END.
    
  END.

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
  {src/adm/template/snd-list.i "oe-ordl"}

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

