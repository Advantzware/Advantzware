&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: p-estop.w

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
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i}

/*{methods/defines/hndldefs.i}
{methods/prgsecdt.i}*/

{sys/inc/VAR.i "new shared"}
ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE TEMP-TABLE tt-est-op NO-UNDO LIKE est-op.
DEFINE VARIABLE llOEPrcChg-sec AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-close AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-list  AS CHARACTER NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "p-estop.",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT llOEPrcChg-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */

     

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
&Scoped-define EXTERNAL-TABLES est est-op est-qty
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est-qty, est-op.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-View Btn-add Btn-Add-std Btn-Imp ~
Btn-Over btn-build btn-del btn-copy 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS RECT-1 

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
DEFINE BUTTON Btn-add 
    LABEL "&Add" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-Add-std 
    LABEL "&Add Stds" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON btn-build 
    LABEL "&Build" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON btn-copy 
    LABEL "&Copy" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON btn-del 
    LABEL "&Delete" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-Imp 
    LABEL "&Import" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-Over 
    LABEL "&Override" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-View 
    LABEL "&View" 
    SIZE 13 BY .91
    FONT 4.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 14 BY 7.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    Btn-View AT ROW 1.19 COL 1.6
    Btn-add AT ROW 2.05 COL 1.6
    Btn-Add-std AT ROW 2.95 COL 1.6
    Btn-Imp AT ROW 3.81 COL 1.6
    Btn-Over AT ROW 4.67 COL 1.6
    btn-build AT ROW 5.57 COL 1.6 WIDGET-ID 2
    btn-del AT ROW 6.43 COL 1.6 WIDGET-ID 4
    btn-copy AT ROW 7.29 COL 1.6 WIDGET-ID 6
    RECT-1 AT ROW 1.05 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.est-op
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.48
         WIDTH              = 50.4.
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
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

ASSIGN 
    Btn-add:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-Add-std:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    btn-build:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    btn-copy:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    btn-del:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-Imp:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-Over:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-View:PRIVATE-DATA IN FRAME F-Main = "panel-image".

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME Btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-add V-table-Win
ON CHOOSE OF Btn-add IN FRAME F-Main /* Add */
    DO:
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
        DEFINE BUFFER bf-est-op FOR est-op .
        IF AVAILABLE est AND AVAILABLE est-qty THEN 
        DO:
       
            IF est.est-type LT 5 THEN
                RUN ce/d-estop.w (?,RECID(est),RECID(est-qty), "Add",OUTPUT lv-rowid).
            ELSE
                RUN est/d-estop.w (?,RECID(est),RECID(est-qty), "Add",OUTPUT lv-rowid).
     
            FIND FIRST bf-est-op NO-LOCK
                WHERE bf-est-op.company EQ est.company
                AND bf-est-op.est-no EQ est.est-no
                AND ROWID(bf-est-op) EQ lv-rowid
                NO-ERROR .
         
            IF AVAILABLE bf-est-op THEN 
            DO:
          
                RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
       
                RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Add-std
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add-std V-table-Win
ON CHOOSE OF Btn-Add-std IN FRAME F-Main /* Add Stds */
    DO:
        DEFINE BUFFER bf-est-op FOR est-op.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE iL       AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.
 
        IF AVAILABLE est AND AVAILABLE est-qty THEN 
        DO:
       
            IF est.est-type LT 5 THEN
                RUN ce/d-estop.w (?,RECID(est),RECID(est-qty), "Addstd",OUTPUT lv-rowid).
            ELSE
                RUN est/d-estop.w (?,RECID(est),RECID(est-qty), "Addstd",OUTPUT lv-rowid).
     
            FIND FIRST bf-est-op NO-LOCK
                WHERE bf-est-op.company EQ est.company
                AND bf-est-op.est-no EQ est.est-no
                AND ROWID(bf-est-op) EQ lv-rowid
                NO-ERROR .
         
            IF AVAILABLE bf-est-op THEN 
            DO:
          
                RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
       
                RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-build
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-build V-table-Win
ON CHOOSE OF btn-build IN FRAME F-Main /* Build */
    DO:
        DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-target", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
            RUN build-route IN WIDGET-HANDLE(char-hdl). 
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy V-table-Win
ON CHOOSE OF btn-copy IN FRAME F-Main /* Copy */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
        DEFINE BUFFER bf-est-op FOR est-op.
        DEFINE BUFFER xop       FOR est-op.

        IF est.est-type LT 5 THEN 
        DO:

            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
            RUN op-copy IN WIDGET-HANDLE(char-hdl).
        END.
        ELSE 
        DO:
            IF AVAILABLE est-op AND AVAILABLE est THEN
            DO:

                i = 1.
                FOR EACH xop
                    WHERE xop.company EQ est.company
                    AND xop.est-no  EQ est.est-no
                    AND xop.line    LT 500
                    NO-LOCK
                    BY xop.line DESCENDING:
                    i = xop.line + 1.
                    LEAVE.
                END.

                CREATE bf-est-op.
                BUFFER-COPY est-op EXCEPT rec_key line TO bf-est-op.
                ASSIGN 
                    bf-est-op.LINE = i .
           
                RUN est/d-estop.w (RECID(bf-est-op), RECID(est),RECID(est-qty), "Copy",OUTPUT lv-rowid).

                FIND FIRST bf-est-op NO-LOCK
                    WHERE bf-est-op.company EQ est.company
                    AND ROWID(bf-est-op) EQ lv-rowid
                    NO-ERROR .

                IF AVAILABLE bf-est-op THEN 
                DO:
                    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
               
                    RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
                END.
            END.

        END.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-del V-table-Win
ON CHOOSE OF btn-del IN FRAME F-Main /* Delete */
    DO:
        IF AVAILABLE est-op THEN 
        DO: 
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            RUN local-delete-record IN WIDGET-HANDLE(char-hdl).
        END.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Imp V-table-Win
ON CHOOSE OF Btn-Imp IN FRAME F-Main /* Import */
    DO:
        DEFINE BUFFER bf-est-prep FOR est-prep.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE iL       AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.


        IF AVAILABLE est AND AVAILABLE est-op THEN
        DO:
     
            EMPTY TEMP-TABLE tt-est-op.

            IF est.est-type LT 5 THEN
                RUN ce/d-estop.w (RECID(est-op),RECID(est),RECID(est-qty), "import", OUTPUT lv-rowid) .
            ELSE
                RUN est/d-estop.w (RECID(est-op),RECID(est),RECID(est-qty), "import", OUTPUT lv-rowid) . 
       
       
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      
        END.

    /*
     run get-link-handle in adm-broker-hdl (this-procedure,"record-source",output char-hdl).
      run set-import-stds in widget-handle(char-hdl) ("update", yes).*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Over V-table-Win
ON CHOOSE OF Btn-Over IN FRAME F-Main /* Override */
    DO:
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
        IF AVAILABLE est AND AVAILABLE est-op THEN
        DO:
     
            EMPTY TEMP-TABLE tt-est-op.
      
            CREATE tt-est-op.
            BUFFER-COPY est-op TO tt-est-op.

            IF est.est-type LT 5 THEN
                RUN ce/d-estop.w (RECID(est-op),RECID(est),RECID(est-qty), "update", OUTPUT lv-rowid) .
            ELSE
                RUN est/d-estop.w (RECID(est-op),RECID(est),RECID(est-qty), "update", OUTPUT lv-rowid) . 
       
            BUFFER-COMPARE tt-est-op TO est-op SAVE RESULT IN ll.
       
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        IF AVAILABLE est-op THEN 
        DO:
            IF est.est-type LT 5 THEN
                RUN ce/d-estop.w (RECID(est-op), RECID(est), RECID(est-qty), "view",OUTPUT lv-rowid). 
            ELSE
                RUN est/d-estop.w (RECID(est-op), RECID(est), RECID(est-qty), "view",OUTPUT lv-rowid). 
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-line V-table-Win 
PROCEDURE add-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    APPLY "choose" TO btn-add IN FRAME {&FRAME-NAME}.
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
    {src/adm/template/row-list.i "est"}
    {src/adm/template/row-list.i "est-op"}
    {src/adm/template/row-list.i "est-qty"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "est"}
    {src/adm/template/row-find.i "est-op"}
    {src/adm/template/row-find.i "est-qty"}

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
    APPLY "choose" TO Btn-Over IN FRAME {&FRAME-NAME}.
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
/*RUN po/po-sysct.p .  /* for vars factor#.... need for d-poordl.w  */*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE l-can-update AS LOGICAL NO-UNDO.
    DEFINE VARIABLE l-can-create AS LOGICAL NO-UNDO.
    DEFINE VARIABLE l-can-delete AS LOGICAL NO-UNDO.
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .
  
    DO WITH FRAME {&FRAME-NAME}: 
        IF NOT AVAILABLE est-op  THEN 
        DO:
            ASSIGN 
                Btn-View:SENSITIVE = NO.                                                          
            Btn-Imp:SENSITIVE = NO.
            Btn-Over:SENSITIVE = NO.
            btn-del:SENSITIVE = NO.
            btn-copy:SENSITIVE = NO.
        END.
        ELSE 
        DO:
            ASSIGN 
                Btn-View:SENSITIVE = YES.                                                          
            Btn-Imp:SENSITIVE = YES.
            Btn-Over:SENSITIVE = YES.
            btn-del:SENSITIVE = YES.
            btn-copy:SENSITIVE = YES.
        END.
        ASSIGN
            l-can-update = IF SUBSTRING(v-access-list, 2, 1) EQ "1" THEN TRUE ELSE FALSE
            l-can-create = IF SUBSTRING(v-access-list, 3, 1) EQ "1" THEN TRUE ELSE FALSE
            l-can-delete = IF SUBSTRING(v-access-list, 4, 1) EQ "1" THEN TRUE ELSE FALSE.


        IF NOT l-can-create THEN ASSIGN btn-add:SENSITIVE     = NO
                btn-add-std:SENSITIVE = NO
                btn-copy:SENSITIVE    = NO.

        IF NOT l-can-update THEN ASSIGN Btn-Over:SENSITIVE  = NO
                Btn-Imp:SENSITIVE   = NO
                btn-build:SENSITIVE = NO.
        IF NOT l-can-delete THEN btn-del:SENSITIVE = NO.
   
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
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.


/*IF AVAIL oe-rell THEN DO:
  lv-rowid = ROWID(oe-rell).
      
  run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
  run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).

  run reopen-query1 in widget-handle(char-hdl) (lv-rowid).

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
END.*/

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
    {src/adm/template/snd-list.i "est-op"}
    {src/adm/template/snd-list.i "est-qty"}

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
    DEFINE INPUT PARAMETER cP-state      AS CHARACTER NO-UNDO.

    CASE cP-state:
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
/*run oe/d-oeitem.w (recid(oe-ordl), oe-ordl.ord-no,INPUT TABLE tt-item-qty-price,
                   "Update").*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


