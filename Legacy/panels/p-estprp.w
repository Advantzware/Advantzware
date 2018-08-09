&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: panel\p-estprp.w

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

DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
/* === vars for d-poordl.w ====*/
/*{methods/prgsecdt.i}*/
{methods/defines/globdefs.i}

{sys/inc/VAR.i "new shared"}
ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

{est/d-selblk.i NEW}

DEFINE TEMP-TABLE tt-est-prep NO-UNDO LIKE est-prep.
DEFINE VARIABLE llOEPrcChg-sec AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-close AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-list  AS CHARACTER NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "p-estprp.",
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
&Scoped-define EXTERNAL-TABLES est est-prep
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, est-prep.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-View Btn-Save Btn-Add Btn-copy ~
Btn-Delete btn-stds 

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
DEFINE BUTTON Btn-Add 
    LABEL "&Add" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-copy 
    LABEL "&Copy" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-Delete 
    LABEL "&Delete" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-Save 
    LABEL "&Update" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON btn-stds 
    LABEL "&Job Stds" 
    SIZE 13 BY .91
    FONT 4.

DEFINE BUTTON Btn-View 
    LABEL "&View" 
    SIZE 13 BY .91
    FONT 4.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 14 BY 5.7.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    Btn-View AT ROW 1.14 COL 1.6
    Btn-Save AT ROW 2.05 COL 1.6
    Btn-Add AT ROW 2.95 COL 1.6
    Btn-copy AT ROW 3.86 COL 1.6
    Btn-Delete AT ROW 4.76 COL 1.6
    btn-stds AT ROW 5.67 COL 1.6 WIDGET-ID 2
    RECT-1 AT ROW 1.05 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.est-prep
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
         HEIGHT             = 7.33
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
    Btn-Add:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-copy:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-Delete:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    Btn-Save:PRIVATE-DATA IN FRAME F-Main = "panel-image".

ASSIGN 
    btn-stds:PRIVATE-DATA IN FRAME F-Main = "panel-image".

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

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add V-table-Win
ON CHOOSE OF Btn-Add IN FRAME F-Main /* Add */
    DO:
        DEFINE BUFFER bf-est-prep FOR est-prep.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE iL       AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.

        IF AVAILABLE est THEN
        DO:
            FOR EACH bf-est-prep NO-LOCK 
                WHERE bf-est-prep.company EQ est.company
                AND bf-est-prep.est-no EQ est.est-no 
                BY bf-est-prep.s-num BY bf-est-prep.b-num BY bf-est-prep.CODE:
                iL[1] = iL[1] + 1.
            END.

            RUN est/d-estprp.w (?, RECID(est), "add",OUTPUT lv-rowid).
     
            FOR EACH bf-est-prep NO-LOCK
                WHERE bf-est-prep.company EQ est.company
                AND bf-est-prep.est-no EQ est.est-no
                BY bf-est-prep.s-num BY bf-est-prep.b-num BY bf-est-prep.CODE:
                ASSIGN
                    iL[2] = iL[2] + 1.
            /*lv-rowid = ROWID(bf-oe-rell)*/ 
            END.

            IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN 
            DO:
                RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
                /*RUN local-open-query IN WIDGET-HANDLE(char-hdl).
                RUN reopen-query IN WIDGET-HANDLE(char-hdl)("add") .*/
                RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-copy V-table-Win
ON CHOOSE OF Btn-copy IN FRAME F-Main /* Copy */
    DO:
        DEFINE BUFFER bf-est-prep FOR est-prep.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE iL       AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.
        DEFINE VARIABLE op-prep  AS LOG     NO-UNDO .
        DEFINE VARIABLE lv-b-num LIKE est-prep.b-num NO-UNDO.
        DEFINE BUFFER b-est-prep FOR est-prep.
        DEFINE VARIABLE li-line AS INTEGER NO-UNDO.

        /* Code placed here will execute PRIOR to standard behavior. */
        /*{custom/checkuse.i}*/

        IF AVAILABLE est-prep THEN
        DO:
    
            ASSIGN
                lv-rowid = ROWID(est-prep)
                lv-b-num = INT(est-prep.b-num).

            RELEASE eb.

            IF NOT CAN-FIND(eb WHERE eb.company EQ est.company
                AND eb.est-no  EQ est.est-no
                AND eb.form-no NE 0) THEN
                FIND FIRST eb
                    WHERE eb.company   EQ est.company
                    AND eb.est-no    EQ est.est-no
                    AND eb.form-no   EQ INT(est-prep.s-num)
                    AND (eb.blank-no EQ lv-b-num OR lv-b-num EQ 0)
                    NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN 
            DO:
                RUN est/d-selblkp.w (ROWID(eb), "Copy " + TRIM("Preparation"),OUTPUT op-prep).
      
      
                FOR EACH tt-select WHERE tt-selected,
                    FIRST eb WHERE ROWID(eb) EQ tt-rowid
                    BREAK BY eb.form-no
                    BY eb.blank-no:

                    /* IF FIRST-OF(eb.form-no) OR lv-b-num NE 0 THEN DO:*/ /* task 4231527 */
                    FIND FIRST b-est-prep
                        WHERE b-est-prep.company EQ est-prep.company
                        AND b-est-prep.est-no  EQ est-prep.est-no
                        AND b-est-prep.code    EQ est-prep.code
                        AND b-est-prep.s-num   EQ eb.form-no
                        AND b-est-prep.b-num   EQ eb.blank-no
                        NO-ERROR.

                    IF AVAILABLE b-est-prep AND op-prep THEN
                        ASSIGN
                            b-est-prep.qty         = est-prep.qty
                            b-est-prep.dscr        = est-prep.dscr
                            b-est-prep.cost        = est-prep.cost
                            b-est-prep.ml          = est-prep.ml
                            b-est-prep.simon       = est-prep.simon
                            b-est-prep.mkup        = est-prep.mkup
                            b-est-prep.amtz        = est-prep.amtz 
                            b-est-prep.spare-dec-1 = est-prep.spare-dec-1 .

                    ELSE 
                    DO:
                        FIND LAST b-est-prep NO-LOCK
                            WHERE b-est-prep.company EQ est-prep.company
                            AND b-est-prep.est-no  EQ est-prep.est-no
                            AND b-est-prep.eqty    EQ est-prep.eqty
                            USE-INDEX est-qty NO-ERROR.
                        li-line = IF AVAILABLE b-est-prep THEN b-est-prep.LINE + 1 ELSE 1.

                        CREATE b-est-prep.

                        BUFFER-COPY est-prep EXCEPT rec_key TO b-est-prep
                            ASSIGN
                            b-est-prep.line  = li-line
                            b-est-prep.s-num = eb.form-no
                            b-est-prep.b-num = eb.blank-no.
                    END.

                    lv-rowid = ROWID(b-est-prep).
                /*END.*/
                END.

                /*RUN repo-query (lv-rowid).*/
                RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
                RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
            END.

            ELSE 
            DO:


                FOR EACH bf-est-prep NO-LOCK 
                    WHERE bf-est-prep.company EQ est.company
                    AND bf-est-prep.est-no EQ est.est-no 
                    BY bf-est-prep.s-num BY bf-est-prep.b-num BY bf-est-prep.CODE:
                    iL[1] = iL[1] + 1.
                END.
      
                CREATE bf-est-prep.
                BUFFER-COPY est-prep EXCEPT rec_key line TO bf-est-prep.
                /*bf-oe-rell.LINE = z.*/
     
                RUN est/d-estprp.w (RECID(bf-est-prep), RECID(est), "Copy",OUTPUT lv-rowid).
     
                FOR EACH bf-est-prep NO-LOCK 
                    WHERE bf-est-prep.company EQ est.company
                    AND bf-est-prep.est-no EQ est.est-no 
                    BY bf-est-prep.s-num BY bf-est-prep.b-num BY bf-est-prep.CODE:
                    ASSIGN
                        iL[2] = iL[2] + 1.
                END.
     
                IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN 
                DO:
          
                    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
                    RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      
                END.
            END.

        END.  /* avail est-prep */ 


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete V-table-Win
ON CHOOSE OF Btn-Delete IN FRAME F-Main /* Delete */
    DO:
        IF AVAILABLE est-prep THEN 
        DO: 
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            RUN local-delete-record IN WIDGET-HANDLE(char-hdl).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save V-table-Win
ON CHOOSE OF Btn-Save IN FRAME F-Main /* Update */
    DO:
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
        IF AVAILABLE est AND AVAILABLE est-prep THEN
        DO:
     
            EMPTY TEMP-TABLE tt-est-prep.
      
            CREATE tt-est-prep.
            BUFFER-COPY est-prep TO tt-est-prep.
      
            RUN est/d-estprp.w (RECID(est-prep),RECID(est), "update", OUTPUT lv-rowid) . 
       
            BUFFER-COMPARE tt-est-prep TO est-prep SAVE RESULT IN ll.
       
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-stds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-stds V-table-Win
ON CHOOSE OF btn-stds IN FRAME F-Main /* Job Stds */
    DO:
        DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-target", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
            RUN run-job-stds IN WIDGET-HANDLE(char-hdl). 
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        IF AVAILABLE est-prep THEN
            RUN est/d-estprp.w (RECID(est-prep), RECID(est), "view",OUTPUT lv-rowid). 
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
    {src/adm/template/row-list.i "est-prep"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "est"}
    {src/adm/template/row-find.i "est-prep"}

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
    APPLY "choose" TO Btn-Save IN FRAME {&FRAME-NAME}.
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
        IF NOT AVAILABLE est-prep THEN 
        DO:
            ASSIGN 
                Btn-View:SENSITIVE = NO.                                                          
            Btn-Save:SENSITIVE = NO.
        
            Btn-copy:SENSITIVE = NO.
            Btn-Delete:SENSITIVE = NO.
        
        END.
        ELSE 
        DO:
            ASSIGN 
                Btn-View:SENSITIVE = YES.                                                          
            Btn-Save:SENSITIVE = YES.
            Btn-copy:SENSITIVE = YES.
            Btn-Delete:SENSITIVE = YES.
       
        END.

        ASSIGN
            l-can-update = IF SUBSTRING(v-access-list, 2, 1) EQ "1" THEN TRUE ELSE FALSE
            l-can-create = IF SUBSTRING(v-access-list, 3, 1) EQ "1" THEN TRUE ELSE FALSE
            l-can-delete = IF SUBSTRING(v-access-list, 4, 1) EQ "1" THEN TRUE ELSE FALSE.


        IF NOT l-can-create THEN ASSIGN btn-add:SENSITIVE  = NO
                btn-copy:SENSITIVE = NO.

        IF NOT l-can-update THEN ASSIGN Btn-Save:SENSITIVE = NO
                btn-stds:SENSITIVE = NO.
                                   
        IF NOT l-can-delete THEN Btn-Delete:SENSITIVE = NO.
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
    {src/adm/template/snd-list.i "est-prep"}

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


