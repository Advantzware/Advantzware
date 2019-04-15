&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ap\b-reconc.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE yellowColumnsName b-reconc
{methods/defines/winReSize.i}


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

{ap/reconcil.i NEW}

&SCOPED-DEFINE BRWSDEFS reconcile

/* gdm - */
DEF VAR v-can-update AS LOG NO-UNDO.
DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO INIT TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES reconcile

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-number tt-date tt-amt tt-bank tt-vend tt-name tt-cleared   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-cleared   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table reconcile
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table reconcile


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fi_sortBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortBy AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      reconcile SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-number             FORMAT "x(13)"      LABEL "Check/Journal#" LABEL-BGCOLOR 14
      tt-date                                   LABEL "Trans Date" LABEL-BGCOLOR 14
      tt-amt                                    LABEL "Amount" LABEL-BGCOLOR 14
      tt-bank                                   LABEL "Bank" LABEL-BGCOLOR 14
      tt-vend                                   LABEL "Vendor#" LABEL-BGCOLOR 14
      tt-name                                   LABEL "Name" LABEL-BGCOLOR 14
      tt-cleared                                LABEL "Cleared?" LABEL-BGCOLOR 14
   ENABLE tt-cleared
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 17.62
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 18.86 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 18.86 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 18.86 COL 98 HELP
          "CLEAR AUTO FIND Value"
     fi_sortBy AT ROW 18.86 COL 115 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     "Sort:" VIEW-AS TEXT
          SIZE 5 BY .95 AT ROW 18.86 COL 112 WIDGET-ID 14
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 18.86 COL 2
     RECT-4 AT ROW 18.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 20.24
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Query            is OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i}*/
   {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cleared
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cleared Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF tt-cleared IN BROWSE Browser-Table /* PO Number */
DO:
  DEF BUFFER b-ap-pay FOR ap-pay.
  DEF VAR lv-bank LIKE ar-cash.bank-code NO-UNDO.

  /*IF tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes" THEN
    tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "No".
  ELSE
    tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".*/

  IF AVAIL reconcile THEN DO:
    FIND CURRENT reconcile NO-LOCK NO-ERROR.
    tt-cleared = tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Yes".

    IF tt-type EQ 1 THEN DO /*TRANSACTION*/ :
      RELEASE b-ap-pay.

      FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.

      IF AVAIL ap-pay THEN
        IF ap-pay.d-no NE 0                                              AND
           NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN DO:
           FIND FIRST b-ap-pay
               WHERE b-ap-pay.company   EQ ap-pay.company
                 AND b-ap-pay.check-act EQ ap-pay.check-act
                 AND b-ap-pay.check-no  EQ ap-pay.d-no
               EXCLUSIVE-LOCK NO-ERROR.
        END.

        ELSE FIND b-ap-pay WHERE ROWID(b-ap-pay) EQ ROWID(ap-pay) EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL b-ap-pay THEN DO:
         b-ap-pay.cleared = tt-cleared.

         FOR EACH ap-pay
             WHERE ap-pay.company EQ b-ap-pay.company
               AND ap-pay.d-no    EQ b-ap-pay.check-no
               AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
             EXCLUSIVE-LOCK
             USE-INDEX d-no:
             ap-pay.cleared = b-ap-pay.cleared.
         END.
      END.
    END. /*tt-type eq 1*/

    ELSE
    IF tt-type EQ 2 THEN DO:
      FIND ar-cash WHERE ROWID(ar-cash) EQ tt-rowid NO-LOCK NO-ERROR.
      IF AVAIL ar-cash THEN DO:
        lv-bank = ar-cash.bank-code.
        RELEASE ar-cash.
        FOR EACH tt-cash
            WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10))
            USE-INDEX tt-trnum,
            FIRST ar-cash EXCLUSIVE-LOCK
            WHERE ROWID(ar-cash)     EQ tt-cash.row-id
              AND ar-cash.reconciled EQ NO
              AND ar-cash.posted     EQ YES
              AND ar-cash.memo       EQ NO
              AND ar-cash.bank-code  EQ lv-bank,
            FIRST bank NO-LOCK
            WHERE bank.company   EQ ar-cash.company
              AND bank.bank-code EQ ar-cash.bank-code
            USE-INDEX bank
            /*TRANSACTION*/ :
          ar-cash.cleared = tt-cleared.
        END.
      END.
    END.

    ELSE
    IF tt-type EQ 3 THEN DO /*TRANSACTION*/ :
      FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL gl-jrn THEN gl-jrn.cleared = tt-cleared.
    END.

    ELSE
    IF tt-type EQ 4 THEN
    FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
        FIRST ar-mcash-ref
        WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
          AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
          AND ar-mcash-ref.company  EQ "ar-mcash"
          USE-INDEX rec_key
        /*TRANSACTION*/ EXCLUSIVE-LOCK:
      ar-mcash-ref.val[2] = INT(tt-cleared).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&SCOPED-DEFINE cellColumnDat b-reconc
  {methods/browsers/setCellColumns.i}
  {custom/yellowColumns.i}

DEF BUFFER b-ap-pay FOR ap-pay.

/*     Task 12111502 */
ON MOUSE-SELECT-CLICK OF tt-cleared IN BROWSE Browser-Table /* Reconciled */
DO:
  DEF VAR lv-bank LIKE ar-cash.bank-code NO-UNDO.


  IF tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes" THEN
    tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "No".
  ELSE
    tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".

  IF AVAIL reconcile THEN DO:
    FIND CURRENT reconcile NO-LOCK NO-ERROR.
    tt-cleared = tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Yes".

    IF tt-type EQ 1 THEN DO /*TRANSACTION*/ :
      RELEASE b-ap-pay.

      FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.

      IF AVAIL ap-pay THEN
        IF ap-pay.d-no NE 0                                              AND
           NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN DO:
           FIND FIRST b-ap-pay
               WHERE b-ap-pay.company   EQ ap-pay.company
                 AND b-ap-pay.check-act EQ ap-pay.check-act
                 AND b-ap-pay.check-no  EQ ap-pay.d-no
               EXCLUSIVE-LOCK NO-ERROR.
        END.

        ELSE FIND b-ap-pay WHERE ROWID(b-ap-pay) EQ ROWID(ap-pay) EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL b-ap-pay THEN DO:
         b-ap-pay.cleared = tt-cleared.

         FOR EACH ap-pay
             WHERE ap-pay.company EQ b-ap-pay.company
               AND ap-pay.d-no    EQ b-ap-pay.check-no
               AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
             EXCLUSIVE-LOCK
             USE-INDEX d-no:
             ap-pay.cleared = b-ap-pay.cleared.
         END.
      END.
    END. /*tt-type eq 1*/

    ELSE
    IF tt-type EQ 2 THEN DO:
      FIND ar-cash WHERE ROWID(ar-cash) EQ tt-rowid NO-LOCK NO-ERROR.
      IF AVAIL ar-cash THEN DO:
        lv-bank = ar-cash.bank-code.
        RELEASE ar-cash.
        FOR EACH tt-cash
            WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10))
            USE-INDEX tt-trnum,
            FIRST ar-cash EXCLUSIVE-LOCK
            WHERE ROWID(ar-cash)     EQ tt-cash.row-id
              AND ar-cash.reconciled EQ NO
              AND ar-cash.posted     EQ YES
              AND ar-cash.memo       EQ NO
              AND ar-cash.bank-code  EQ lv-bank,
            FIRST bank NO-LOCK
            WHERE bank.company   EQ ar-cash.company
              AND bank.bank-code EQ ar-cash.bank-code
            USE-INDEX bank
            /*TRANSACTION*/ :
          ar-cash.cleared = tt-cleared.
        END.
      END.
    END.

    ELSE
    IF tt-type EQ 3 THEN DO /*TRANSACTION*/ :
      FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL gl-jrn THEN gl-jrn.cleared = tt-cleared.
    END.

    ELSE
    IF tt-type EQ 4 THEN
    FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
        FIRST ar-mcash-ref
        WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
          AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
          AND ar-mcash-ref.company  EQ "ar-mcash"
          USE-INDEX rec_key
        /*TRANSACTION*/ EXCLUSIVE-LOCK:
      ar-mcash-ref.val[2] = INT(tt-cleared).
    END.
  END.

  RETURN NO-APPLY.
END.

ASSIGN
 cocode = g_company
 locode = g_loc.

RUN ap/reconcil.p.

/* gdm - */
RUN get-security.


ON 'ENTRY':U OF reconcile.tt-cleared
DO:
   IF LASTKEY NE -1 AND NOT v-can-update THEN
   DO:       
      APPLY "TAB" TO BROWSE {&browse-name}.
      RETURN NO-APPLY .
   END.
END.
/* gdm - */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-called-setCellColumns = NO THEN DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      {&BROWSE-NAME}:COLUMN-MOVABLE = v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move .
       fi_sortBy = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY fi_sortBy. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-security B-table-Win 
PROCEDURE get-security :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE v-dirname LIKE b-prgrms.DIR_group NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

ASSIGN
   v-prgmname = "w-reconl."
   v-dirname  = "ap".

FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname AND
                    b-prgrms.DIR_group = v-dirname NO-LOCK NO-ERROR.

IF NOT AVAIL b-prgrms THEN 
  FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.

IF AVAILABLE b-prgrms THEN DO:    

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
    IF NOT CAN-DO(TRIM(b-prgrms.can_run),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_create),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_delete),ENTRY(num-groups,g_groups)) THEN
    NEXT.

    
    IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups))
          THEN v-can-update = YES.
    

    group-ok = yes.
    /*LEAVE. */
  END.
  IF NOT CAN-DO(TRIM(b-prgrms.can_run),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_create),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_delete),USERID("ASI")) AND NOT group-ok THEN
  DO:
    MESSAGE "Program :" PROGRAM-NAME(1) SKIP "Title :" b-prgrms.prgtitle SKIP(1)
        "Access to this Program Denied - Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.

    access-close = YES.  /* used later in methods/template/windows.i - local-initialize procedure */

  END.
  ELSE DO:
      IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI"))
            THEN v-can-update = YES.      
  END.
END. 
ELSE
DO: 
  MESSAGE "Program :" PROGRAM-NAME(1) SKIP(1)
      "Program Master Record Does Not Exist - Contact Systems Manager" 
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "reconcile"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

