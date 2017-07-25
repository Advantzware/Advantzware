&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ap\v-apinv.w

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
&SCOPED-DEFINE create-more methods/viewers/create/ap-inv
{custom/globdefs.i}
DEF BUFFER bf-inv FOR ap-inv.

DEF VAR lv-prev-exrate AS DEC NO-UNDO.
DEF VAR lv-got-exrate AS LOG NO-UNDO.
DEF VAR ll-first AS LOG NO-UNDO.
DEF VAR lv-cancel AS LOG NO-UNDO.
DEF VAR ll-date-warning AS LOG NO-UNDO.
DEF VAR ll-recur AS LOG NO-UNDO.
DEF VAR v-delete-choice AS LOG NO-UNDO.

&SCOPED-DEFINE enable-proc proc-enable
{sys/inc/VAR.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.
{sys/inc/ap-gl#.i}
{sys/inc/apsecure.i}
DO WITH TRANSACTION:
   {sys/inc/apautocheck.i}
END.

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
&Scoped-define EXTERNAL-TABLES ap-inv
&Scoped-define FIRST-EXTERNAL-TABLE ap-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-inv.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ap-inv.vend-no ap-inv.inv-no ap-inv.inv-date ~
ap-inv.due-date ap-inv.tax-gr ap-inv.disc-% ap-inv.disc-days ap-inv.tax-amt 
&Scoped-define ENABLED-TABLES ap-inv
&Scoped-define FIRST-ENABLED-TABLE ap-inv
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 
&Scoped-Define DISPLAYED-FIELDS ap-inv.vend-no ap-inv.inv-no ~
ap-inv.inv-date ap-inv.due-date ap-inv.tax-gr ap-inv.disc-% ~
ap-inv.disc-days ap-inv.stat ap-inv.tax-amt ap-inv.net ap-inv.paid ~
ap-inv.freight ap-inv.due ap-inv.user-id 
&Scoped-define DISPLAYED-TABLES ap-inv
&Scoped-define FIRST-DISPLAYED-TABLE ap-inv
&Scoped-Define DISPLAYED-OBJECTS vend_name cb_freq scr-manual-check-no ~
tg_overwrite-tax 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS scr-manual-check-no 
&Scoped-define ADM-ASSIGN-FIELDS cb_freq scr-manual-check-no 

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
DEFINE BUTTON btn-exrate 
     LABEL "ExRate" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb_freq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frequency" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","Daily","Weekly","Monthly","Annually","Bi-weekly","Bi-monthly","Bi-annually","Intermittently" 
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE scr-manual-check-no AS INTEGER FORMAT "999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE vend_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 5.95.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 5.48.

DEFINE VARIABLE tg_overwrite-tax AS LOGICAL INITIAL no 
     LABEL "Overwrite Tax?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ap-inv.vend-no AT ROW 1.24 COL 17 COLON-ALIGNED
          LABEL "Vendor#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     vend_name AT ROW 1.24 COL 37 COLON-ALIGNED NO-LABEL
     ap-inv.inv-no AT ROW 2.19 COL 17 COLON-ALIGNED
          LABEL "Invoice#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.inv-date AT ROW 3.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.due-date AT ROW 4.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     btn-exrate AT ROW 5.52 COL 2 NO-TAB-STOP
     ap-inv.tax-gr AT ROW 2.43 COL 51.2 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     ap-inv.disc-% AT ROW 3.43 COL 51.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ap-inv.disc-days AT ROW 4.43 COL 51.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     cb_freq AT ROW 5.52 COL 28.8 COLON-ALIGNED HELP
          "Please enter how often this journal entry will be applied"
     scr-manual-check-no AT ROW 5.52 COL 61.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     ap-inv.stat AT ROW 2.43 COL 71 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ap-inv.tax-amt AT ROW 1.48 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ap-inv.net AT ROW 2.43 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.paid AT ROW 3.43 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.freight AT ROW 4.43 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.due AT ROW 5.43 COL 98 COLON-ALIGNED
          LABEL "Balance Due"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-inv.user-id AT ROW 1.24 COL 129 COLON-ALIGNED
          LABEL "User"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     tg_overwrite-tax AT ROW 2.67 COL 122
     "Manual Check#" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 4.76 COL 63 WIDGET-ID 8
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 1.24 COL 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ap-inv
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

/* SETTINGS FOR BUTTON btn-exrate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb_freq IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ap-inv.due IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ap-inv.freight IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-inv.inv-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ap-inv.net IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-inv.paid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-manual-check-no IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ap-inv.stat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-inv.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tg_overwrite-tax IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-inv.user-id IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ap-inv.vend-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend_name IN FRAME F-Main
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
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "vend-no" THEN DO:
      RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE ""                                      AND
         TRIM(FOCUS:SCREEN-VALUE) NE TRIM(ENTRY(1,char-val)) THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-vend-no.
      END.
    END.  
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exrate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exrate V-table-Win
ON CHOOSE OF btn-exrate IN FRAME F-Main /* ExRate */
DO:
    DO:
        RUN windows/d-exrate.w PERSISTENT SET hProgram  ("ap-inv", RECID(ap-inv)).
        RUN dispatch IN hProgram ("initialize").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-inv.disc-days
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.disc-days V-table-Win
ON LEAVE OF ap-inv.disc-days IN FRAME F-Main /* Days */
DO:
  /* Task# 19219593 
  IF LASTKEY  <> -1  THEN DO:
      APPLY "entry" TO ap-inv.vend-no.
      RETURN NO-APPLY.
   END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-inv.inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.inv-date V-table-Win
ON LEAVE OF ap-inv.inv-date IN FRAME F-Main /* Invoice Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.inv-date V-table-Win
ON VALUE-CHANGED OF ap-inv.inv-date IN FRAME F-Main /* Invoice Date */
DO:
  RUN new-inv-date.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-inv.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.inv-no V-table-Win
ON LEAVE OF ap-inv.inv-no IN FRAME F-Main /* Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-inv.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.tax-gr V-table-Win
ON LEAVE OF ap-inv.tax-gr IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-gr (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-inv.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.vend-no V-table-Win
ON ENTRY OF ap-inv.vend-no IN FRAME F-Main /* Vendor# */
DO:
  IF adm-adding-record AND ll-first AND ap-inv.vend-no:SCREEN-VALUE EQ "" THEN DO:
    FIND LAST bf-inv
      WHERE bf-inv.company EQ g_company
        AND bf-inv.vend-no NE ""
      USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAIL bf-inv THEN ap-inv.vend-no:SCREEN-VALUE = bf-inv.vend-no.

    DEF VAR char-hdl AS cha NO-UNDO.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"clear-target",OUTPUT char-hdl).
    RUN clear-lines IN WIDGET-HANDLE(char-hdl).

    ap-inv.inv-date:SCREEN-VALUE = STRING(TODAY).

    RUN new-vend-no.
  END.

  ll-first = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.vend-no V-table-Win
ON LEAVE OF ap-inv.vend-no IN FRAME F-Main /* Vendor# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-vend-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-inv.vend-no V-table-Win
ON VALUE-CHANGED OF ap-inv.vend-no IN FRAME F-Main /* Vendor# */
DO:
  RUN new-vend-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME       


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_overwrite-tax V-table-Win
ON CHOOSE  OF tg_overwrite-tax IN FRAME F-Main /* Vendor# */
DO:
 APPLY "tab" TO tg_overwrite-tax .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
/*{custom/resizmn.i}  */
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-inv V-table-Win 
PROCEDURE add-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN dispatch ('add-record').

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
  {src/adm/template/row-list.i "ap-inv"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-inv"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-choice V-table-Win 
PROCEDURE delete-choice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-delete-choice AS LOG NO-UNDO.

   op-delete-choice = v-delete-choice.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE cb_freq btn-exrate tg_overwrite-tax scr-manual-check-no.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-open-po V-table-Win 
PROCEDURE find-open-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM ip-recid AS RECID.
DEF INPUT PARAM ip-po-no LIKE po-ord.po-no.
DEF OUTPUT PARAM op-recid AS RECID .

DEF VAR v-qty AS DEC NO-UNDO.


find vend where recid(vend) eq ip-recid no-lock no-error.

if avail vend then 
for each po-ord
    {po/look/pobyven.i}
      and po-ord.opened eq yes
    use-index opened no-lock,

    each po-ordl
    {po/look/pobyven1.i}
    use-index po-no no-lock:

  RUN po/rec-inv.p (ROWID(po-ordl), OUTPUT v-qty).

  if v-qty gt 0 then do:
    /*  create report.
    assign
     report.term-id = v-term
     report.key-01  = string(po-ord.po-no,">>>>>>")
     report.rec-id  = recid(po-ord).

    if po-ord.po-no ge ip-po-no and v-po ne 0 then
      assign
       fil_id = recid(report)
       v-po   = 0.

    if program-name(2) begins "ap/ap-inv." then leave blok. else leave.  */

    op-recid = RECID(po-ord).
    leave.
  end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-ap V-table-Win 
PROCEDURE hold-ap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-ap-inv FOR ap-inv.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

      IF ap-inv.inv-date:SENSITIVE IN FRAME {&FRAME-NAME} THEN do:
         MESSAGE "You can not change status middle of modification. "
            VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      IF AVAIL ap-inv THEN DO:
         message "Are you sure you wish to " +
            trim(string(ap-inv.stat eq "H","release/hold")) + " this Vendor Invoice?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO update choice AS LOG.
         if choice then do:  
            find bf-ap-inv where recid(bf-ap-inv) eq recid(ap-inv) no-error.
            bf-ap-inv.stat = if bf-ap-inv.stat eq "H" then "R" else "H".    
         END.

         FIND CURRENT ap-inv NO-LOCK NO-ERROR.
         IF AVAIL ap-inv THEN DISP ap-inv.stat WITH FRAME {&FRAME-NAME}.
      END.
   END.
   {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-in-update V-table-Win 
PROCEDURE is-in-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-is-enable AS LOG .
  op-is-enable = ap-inv.vend-no:SENSITIVE IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tax-amt AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF tg_overwrite-tax:CHECKED IN FRAME {&FRAME-NAME} THEN
     lv-tax-amt = DEC(ap-inv.tax-amt:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ap-inv.freq = cb_freq.

  IF scr-manual-check-no:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ap-inv.receiver-no = STRING(scr-manual-check-no).

  FIND FIRST vend
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ ap-inv.vend-no
      NO-LOCK NO-ERROR.
  ap-inv.terms = vend.terms.

  lv-got-exrate = NO.

  IF adm-new-record THEN DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN set-attribute-list IN WIDGET-HANDLE(char-hdl) ("NEW-AP=YES").
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
    RUN update-header IN WIDGET-HANDLE(ENTRY(1,char-hdl)) (ROWID(ap-inv), 
                                                           tg_overwrite-tax:CHECKED IN FRAME {&FRAME-NAME}).

  IF tg_overwrite-tax:CHECKED THEN
     ap-inv.tax-amt = lv-tax-amt.
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
  lv-cancel = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-cancel = NO.
  adm-adding-record = NO .

  FIND CURRENT ap-inv NO-ERROR.
  IF AVAIL ap-inv THEN ap-inv.ex-rate = lv-prev-exrate.
  FIND CURRENT ap-inv NO-LOCK NO-ERROR.

  RUN disable-fields.

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
  FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = g_company AND
                          currency.c-code = company.curr-code
                          NO-LOCK NO-ERROR.

  ASSIGN
   ap-inv.company      = cocode
   ap-inv.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
   ap-inv.ex-rate      = IF AVAIL currency THEN currency.ex-rate ELSE 0
   ap-inv.recur        = ll-recur.

  IF ap-inv.recur THEN
    ASSIGN ap-inv.inv-no = STRING(ap-inv.i-no,"9999999999")
           ap-inv.inv-no:screen-value IN FRAME {&FRAME-NAME} = STRING(ap-inv.i-no,"9999999999").

  IF adm-adding-record THEN
  DO WITH FRAME {&FRAME-NAME}:
     cb_freq:SCREEN-VALUE = "".
  END. 

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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN
  DO:
     cb_freq = IF AVAIL ap-inv THEN ap-inv.freq ELSE "".

     IF scr-manual-check-no:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        scr-manual-check-no = IF AVAIL ap-inv THEN INT(ap-inv.receiver-no) ELSE 0.
  END.

  /* Dispatch standard ADM method.                             */
  IF NOT adm-adding-record OR lv-cancel THEN
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST vend
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ap-inv.vend-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL vend THEN vend_name:SCREEN-VALUE = vend.name.

    DISPLAY UNLESS-HIDDEN scr-manual-check-no.
  END.

  IF AVAIL ap-inv AND NOT lv-got-exrate THEN
    ASSIGN
     lv-prev-exrate = ap-inv.ex-rate
     lv-got-exrate  = YES.

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
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"recurring-source",OUTPUT char-hdl).

  ll-recur = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-recur THEN 
      ASSIGN
       ap-inv.inv-no:VISIBLE   = NO
       ap-inv.inv-date:VISIBLE = NO
       ap-inv.due-date:VISIBLE = NO
       cb_freq:HIDDEN          = NO.
    ELSE
      ASSIGN
       cb_freq:SENSITIVE = NO
       cb_freq:HIDDEN    = YES.

    IF apautocheck-log THEN
       scr-manual-check-no:HIDDEN = NO.
    ELSE
       ASSIGN
          scr-manual-check-no:HIDDEN = YES
          scr-manual-check-no:SENSITIVE = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-po-recid AS RECID NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.

  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN scr-manual-check-no.

    RUN valid-vend-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-inv-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-tax-gr (ap-inv.tax-gr:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /*RUN find-open-po (RECID(vend),0,OUTPUT lv-po-recid).
  FIND po-ord WHERE RECID(po-ord) EQ lv-po-recid NO-LOCK NO-ERROR.
  IF AVAIL po-ord THEN MESSAGE "Vendor has purchase orders not fully invoiced" VIEW-AS ALERT-BOX INFORMATION .*/

  RUN disable-fields.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN DO:

    RUN reopen-browser.

    ASSIGN
     adm-new-record    = NO
     adm-adding-record = NO.

    DEF VAR char-hdl AS cha NO-UNDO.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
    RUN auto-line-add IN WIDGET-HANDLE(char-hdl).
  END.

  RUN dispatch ("display-fields").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-inv-date V-table-Win 
PROCEDURE new-inv-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date AS DATE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ll-date-warning = NO.

    FIND FIRST vend
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ap-inv.vend-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    lv-date = DATE(ap-inv.inv-date:SCREEN-VALUE) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND AVAIL vend THEN DO:
      FIND FIRST terms NO-LOCK
          WHERE terms.company EQ vend.company
            AND terms.t-code  EQ vend.terms
          NO-ERROR.

      ap-inv.due-date:SCREEN-VALUE  = IF AVAIL terms THEN STRING(terms.net-day + lv-date)
                                      ELSE ap-inv.inv-date:SCREEN-VALUE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend-no V-table-Win 
PROCEDURE new-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST vend
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ap-inv.vend-no:SCREEN-VALUE
          AND vend.active  EQ "A"
        NO-LOCK NO-ERROR.
    IF AVAIL vend THEN DO:
      FIND FIRST terms WHERE terms.t-code EQ vend.terms NO-LOCK NO-ERROR.

      ASSIGN
       vend_name:SCREEN-VALUE        = vend.name
       ap-inv.disc-%:SCREEN-VALUE    = STRING(vend.disc-%)
       ap-inv.disc-days:SCREEN-VALUE = STRING(vend.disc-days)
       ap-inv.tax-gr:SCREEN-VALUE    = vend.tax-gr.

      RUN new-inv-date.

      IF AVAIL terms THEN
        ASSIGN
         ap-inv.disc-%:SCREEN-VALUE    = STRING(terms.disc-rate)
         ap-inv.disc-days:SCREEN-VALUE = STRING(terms.disc-days).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR v-msg AS CHAR NO-UNDO.

  v-msg = "".

  IF NOT adm-new-record THEN DO:
    IF v-msg EQ "" AND ap-inv.posted THEN
      v-msg = "This invoice has been posted, no changes are allowed!".

    IF v-msg EQ "" AND apsecure-log AND ap-inv.user-id NE USERID("nosweat") THEN
      v-msg = "This invoice may only be updated by UserID: " +
              TRIM(ap-inv.user-id) + "...".
  END.

  IF v-msg NE "" THEN DO:
    MESSAGE v-msg VIEW-AS ALERT-BOX ERROR.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
    RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
  END.

  ENABLE btn-exrate tg_overwrite-tax WITH FRAME {&FRAME-NAME}.

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-recur THEN ENABLE cb_freq.
    IF apautocheck-log THEN ENABLE scr-manual-check-no.
  END.

  ASSIGN
   ll-first        = YES
   ll-date-warning = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-browser V-table-Win 
PROCEDURE reopen-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    IF NOT ll-recur THEN RUN record-added IN WIDGET-HANDLE(char-hdl).
    RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(ap-inv)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query V-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL ap-inv THEN DO:
    /*RUN reopen-browser.*/
    RUN dispatch ('display-fields').
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
  {src/adm/template/snd-list.i "ap-inv"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-date V-table-Win 
PROCEDURE valid-inv-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF NOT ll-recur THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF DATE(ap-inv.inv-date:SCREEN-VALUE) GT TODAY THEN DO:
      IF NOT ll-date-warning THEN
        MESSAGE "Invoice date greater than todays date, continue?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-date-warning.

      IF NOT ll-date-warning THEN DO:
        APPLY "entry" TO ap-inv.inv-date.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-no V-table-Win 
PROCEDURE valid-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF NOT ll-recur THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF ap-inv.inv-no:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Invoice# must be entered..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO ap-inv.inv-no.
      RETURN ERROR.
    END.

    RUN valid-vend-inv NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-gr V-table-Win 
PROCEDURE valid-tax-gr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF ip-focus:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ ip-focus:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-inv V-table-Win 
PROCEDURE valid-vend-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-inv
          WHERE bf-inv.company EQ cocode
            AND bf-inv.vend-no EQ ap-inv.vend-no:SCREEN-VALUE
            AND bf-inv.inv-no  EQ ap-inv.inv-no:SCREEN-VALUE
            AND ROWID(bf-inv)  NE ROWID(ap-inv)
          NO-LOCK NO-ERROR.
    IF AVAIL bf-inv THEN DO:
      ll = FOCUS:NAME EQ "vend-no".
      MESSAGE "Vendor#/Invoice# already entered,"
              "re-enter Vendor#?  (NO to re-enter Invoice#)"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
      IF ll THEN
        APPLY "entry" TO ap-inv.vend-no.
      ELSE
        APPLY "entry" TO ap-inv.inv-no.
      RETURN ERROR.
    END.
  END.


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no V-table-Win 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST vend
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ ap-inv.vend-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAIL vend                                                      OR
       (vend.active NE "A" AND
        (ap-inv.vend-no NE ap-inv.vend-no:SCREEN-VALUE OR adm-new-record)) THEN DO:
      IF AVAIL vend THEN
        MESSAGE TRIM(ap-inv.vend-no:LABEL) + " not active, try help..."
            VIEW-AS ALERT-BOX ERROR.
      ELSE 
        MESSAGE "Invalid " + TRIM(ap-inv.vend-no:LABEL) + ", try help..."
            VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    IF ap-inv.inv-no:SCREEN-VALUE NE "" THEN DO:
      RUN valid-vend-inv NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

