&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\d-multbl.w
  
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
DEF INPUT-OUTPUT PARAMETER io-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER op-fb-changed AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared}
ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED TEMP-TABLE multbl NO-UNDO
    FIELD company AS CHARACTER
    FIELD loc AS CHARACTER
    FIELD est-no like est.est-no
    FIELD board like ef.board
    FIELD brd-dscr like ef.brd-dscr
    FIELD form-no like eb.form-no
    FIELD blank-no like eb.blank-no
    FIELD eb-recid as recid
    .



DEF BUFFER xeb-form-ef FOR ef.
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-eb FOR eb.

DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE where-multbl WHERE multbl.company  EQ est.company      ~
                              AND multbl.loc      EQ est.loc          ~
                              AND multbl.est-no     EQ est.est-no

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-multbl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     SPACE(112.20) SKIP(20.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Items for Estimate #".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   External Tables: ASI.est
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
ON GO OF FRAME D-Dialog /* Items for Estimate # */
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR ll-fb-changed AS LOG NO-UNDO.

  FOR EACH multbl NO-LOCK {&where-multbl},
      FIRST eb NO-LOCK 
      WHERE RECID(eb)    EQ multbl.eb-recid   
        AND (eb.form-no  NE multbl.form-no OR
             eb.blank-no NE multbl.blank-no):
    ll-fb-changed = YES.
    LEAVE.
  END.


  op-fb-changed = ll-fb-changed.

  IF ll-fb-changed THEN DO:
    RUN new-forms.

    FOR EACH multbl {&where-multbl},
        FIRST eb
        WHERE RECID(eb)    EQ multbl.eb-recid 
          AND  eb.blank-no LT 999
          AND (eb.form-no  NE multbl.form-no OR
               eb.blank-no NE multbl.blank-no):

      IF eb.form-no NE multbl.form-no THEN DO:
        {sys/inc/xeb-form.i "eb." "0" "multbl.form-no" "0"}
      END.

      multbl.blank-no = (multbl.blank-no * 1000) +
                      (1 * (IF multbl.blank-no LT eb.blank-no THEN -1 ELSE 1)).

      {sys/inc/xeb-form.i "eb." "eb.blank-no" "multbl.form-no" "multbl.blank-no * 1000"}

      ASSIGN
       eb.form-no  = multbl.form-no
       eb.blank-no = multbl.blank-no * 1000.

      RUN update-ef-board-proc.
    END.




    RUN del-ref-records.

    RUN est/resetf&b.p (ROWID(est), NO).

    li = 0.
    FOR EACH est-op
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
          AND est-op.line    LT 500
        BY est-op.qty
        BY est-op.s-num
        BY est-op.b-num
        BY est-op.d-seq
        BY est-op.op-pass
        BY est-op.rec_key:
      
      {sys/inc/outstrPL.i est-op SHARE}
      ASSIGN
       li          = li + 1
       est-op.line = li.
     
      multbl.loc = STRING(est-op.line,"9999999999"). 
    END.
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-target", OUTPUT char-hdl).

  RUN get-eb-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT io-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Items for Estimate # */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "go":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
FIND eb WHERE ROWID(eb) EQ io-rowid NO-LOCK NO-ERROR.
IF AVAIL eb THEN FIND FIRST est OF eb NO-LOCK.

IF AVAIL est THEN DO:
  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + TRIM(est.est-no).

  RELEASE eb.

  RUN del-ref-records.

  FOR EACH ef FIELDS(est-no form-no board brd-dscr) NO-LOCK
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no,
      EACH eb FIELDS(form-no blank-no) NO-LOCK
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no:
    CREATE multbl.
    ASSIGN
     multbl.company  = est.company
     multbl.loc      = est.loc
     multbl.est-no     = est.est-no
     multbl.board    = ef.board
     multbl.brd-dscr     = ef.brd-dscr
     multbl.form-no   = eb.form-no
     multbl.blank-no   = eb.blank-no
     multbl.eb-recid   = RECID(eb).
    RELEASE multbl.
  END.

  {src/adm/template/dialogmn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-multbl.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-multbl ).
       RUN set-position IN h_b-multbl ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b-multbl ( 16.91 , 112.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 18.62 , 40.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 1.76 , 31.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-multbl. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'TableIO':U , h_b-multbl ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_b-multbl ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan ,
             h_b-multbl , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-ref-records D-Dialog 
PROCEDURE del-ref-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH multbl {&where-multbl}:
    DELETE multbl.
  END.


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
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb-rowid D-Dialog 
PROCEDURE get-eb-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.

  
  op-rowid = io-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-forms D-Dialog 
PROCEDURE new-forms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-multbl FOR multbl.

  DEF VAR li AS INT NO-UNDO.

  
FOR EACH multbl {&where-multbl},
      FIRST eb
      WHERE RECID(eb) EQ multbl.eb-recid
        AND NOT CAN-FIND(FIRST ef
                         WHERE ef.company EQ eb.company
                           AND ef.est-no  EQ eb.est-no
                           AND ef.form-no EQ multbl.form-no),
      FIRST ef
      WHERE ef.company EQ eb.company
        AND ef.est-no  EQ eb.est-no
        AND ef.form-no EQ eb.form-no
      BREAK BY multbl.form-no
            BY multbl.blank-no:

    IF FIRST-OF(multbl.form-no) THEN                                               
      IF CAN-FIND(FIRST b-multbl                                                    
                  WHERE b-multbl.company  EQ multbl.company                          
                    AND b-multbl.loc      EQ multbl.loc                              
                    AND b-multbl.est-no   EQ multbl.est-no                             
                    AND b-multbl.form-no  EQ ef.form-no                            
                    AND b-multbl.eb-recid   NE multbl.eb-recid) THEN DO:
        CREATE b-ef.
        BUFFER-COPY ef EXCEPT rec_key TO b-ef
        ASSIGN
         b-ef.form-no   = multbl.form-no
         b-ef.blank-qty = 1.

        {sys/inc/xeb-form.i "eb." "0" "multbl.form-no" "0"}
      END.

      ELSE DO:
        /*ASSIGN
         multbl.val[1] = (multbl.val[1] * 1000) +
                         (1 * (IF multbl.val[1] LT eb.form-no THEN -1 ELSE 1))
         multbl.val[2] = (multbl.val[2] * 1000) +
                         (1 * (IF multbl.val[2] LT eb.blank-no THEN -1 ELSE 1)).*/

        {sys/inc/xeb-form.i "eb." "0" "multbl.form-no" "0"}

        /*{sys/inc/xeb-form.i "eb." "eb.blank-no" "multbl.val[1]" "multbl.val[2]"}

        ASSIGN
         eb.form-no  = multbl.val[1]
         eb.blank-no = multbl.val[2]*/ 
         ef.form-no  = multbl.form-no.
      END.
  END.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "est"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ef-board-proc D-Dialog 
PROCEDURE update-ef-board-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISABLE TRIGGERS FOR LOAD OF ef.

   FIND FIRST ef WHERE
        ef.company EQ cocode AND
        ef.est-no EQ eb.est-no AND
        ef.eqty EQ eb.eqty AND
        ef.form-no EQ eb.form-no.

   IF AVAIL ef THEN
   DO:
      ASSIGN
         ef.board = multbl.board
         ef.brd-dscr = multbl.brd-dscr.
      RELEASE ef.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

