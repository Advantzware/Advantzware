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

  File: ar/v-mcash-.w

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

{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

&scoped-define enable-ar-mcash enable-ar-mcash

DEF TEMP-TABLE tt-mcash LIKE ar-mcash.

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
&Scoped-define EXTERNAL-TABLES ar-mcash
&Scoped-define FIRST-EXTERNAL-TABLE ar-mcash


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-mcash.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-mcash.payer ar-mcash.dscr ~
ar-mcash.check-date ar-mcash.bank-code ar-mcash.curr-code[1] 
&Scoped-define ENABLED-TABLES ar-mcash
&Scoped-define FIRST-ENABLED-TABLE ar-mcash
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 
&Scoped-Define DISPLAYED-FIELDS ar-mcash.payer ar-mcash.m-no ar-mcash.dscr ~
ar-mcash.posted ar-mcash.check-date ar-mcash.bank-code ~
ar-mcash.curr-code[1] ar-mcash.ex-rate 
&Scoped-define DISPLAYED-TABLES ar-mcash
&Scoped-define FIRST-DISPLAYED-TABLE ar-mcash
&Scoped-Define DISPLAYED-OBJECTS fl_checkno 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS fl_checkno 
&Scoped-define DISPLAY-FIELD fl_checkno 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.ar-mcash.company
m-no|y|y|ASI.ar-mcash.m-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company,m-no",
     Keys-Supplied = "company,m-no"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fl_checkno AS INTEGER FORMAT "9999999999":U INITIAL 0 
     LABEL "Check No" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 5.71.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ar-mcash.payer AT ROW 1.48 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     fl_checkno AT ROW 1.48 COL 73 COLON-ALIGNED WIDGET-ID 4
     ar-mcash.m-no AT ROW 1.71 COL 114.6 COLON-ALIGNED
          LABEL "Record#"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-mcash.dscr AT ROW 2.67 COL 19 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     ar-mcash.posted AT ROW 2.91 COL 114.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ar-mcash.check-date AT ROW 3.86 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-mcash.bank-code AT ROW 3.86 COL 68 COLON-ALIGNED
          LABEL "Bank Code"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ar-mcash.curr-code[1] AT ROW 5.05 COL 30 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-mcash.ex-rate AT ROW 5.05 COL 68 COLON-ALIGNED
          LABEL "Exchange Rate"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 1.24 COL 101.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ar-mcash
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ar-mcash.bank-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-mcash.curr-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-mcash.dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-mcash.ex-rate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fl_checkno IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN ar-mcash.m-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ar-mcash.posted IN FRAME F-Main
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

&Scoped-define SELF-NAME ar-mcash.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-mcash.bank-code V-table-Win
ON LEAVE OF ar-mcash.bank-code IN FRAME F-Main /* Bank Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bank-code (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-mcash.curr-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-mcash.curr-code[1] V-table-Win
ON LEAVE OF ar-mcash.curr-code[1] IN FRAME F-Main /* Currency Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-curr-code (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-mcash.curr-code[1] V-table-Win
ON VALUE-CHANGED OF ar-mcash.curr-code[1] IN FRAME F-Main /* Currency Code */
DO:
  RUN new-curr-code (FOCUS) NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ar-mcash
           &WHERE = "WHERE ar-mcash.company eq key-value"
       }
    WHEN 'm-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ar-mcash
           &WHERE = "WHERE ar-mcash.m-no eq INTEGER(key-value)"
       }
  END CASE.

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
  {src/adm/template/row-list.i "ar-mcash"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-mcash"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-ar-mcash V-table-Win 
PROCEDURE enable-ar-mcash :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF adm-new-record THEN
        ENABLE fl_checkno.
  END.

  IF AVAIL ar-mcash AND ar-mcash.posted AND NOT adm-new-record THEN DO:
     RUN dispatch ('disable-fields').
     RETURN "adm-error".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-check-no V-table-Win 
PROCEDURE get-check-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-check-no AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      op-check-no = fl_checkno:SCREEN-VALUE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* gdm - */
  IF STRING(fl_checkno) NE "" THEN DO:
    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "AR-MCASH"       
        AND reftable.company  = ar-mcash.company
        AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
        AND reftable.code     = ar-mcash.rec_key NO-ERROR.
    IF NOT AVAIL reftable THEN DO:

      CREATE reftable.
      ASSIGN
       reftable.reftable = "AR-MCASH"
       reftable.company  = ar-mcash.company
       reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
       reftable.code     = ar-mcash.rec_key
       reftable.code2 = STRING(fl_checkno,"9999999999").
    END.                                                

    RELEASE reftable.
  END.

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
  /* gdm - */
   fl_checkno:SENSITIVE IN FRAME {&FRAME-NAME} = NO .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-mcash FOR ar-mcash.
  DEF VAR li-next-mno AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-mcash USE-INDEX m-no NO-LOCK NO-ERROR.
  li-next-mno = IF AVAIL bf-mcash THEN bf-mcash.m-no + 1 ELSE 1.

  /* gdm - */
  ASSIGN fl_checkno = 0
         fl_checkno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
  IF AVAIL ar-ctrl THEN DO:
     FIND FIRST bank WHERE bank.company = g_company AND
                           bank.actnum = ar-ctrl.cash-act NO-LOCK NO-ERROR.
     IF NOT AVAIL bank THEN FIND FIRST bank WHERE bank.company = g_company NO-LOCK NO-ERROR.
     IF AVAIL bank THEN ASSIGN ar-mcash.bank-code = bank.bank-code.
  END.
  FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = g_company AND
                            currency.c-code = company.curr-code
                            NO-LOCK NO-ERROR.
  ASSIGN ar-mcash.company = g_company
         ar-mcash.actnum = ar-ctrl.sales
         ar-mcash.m-no = li-next-mno
         ar-mcash.check-date = TODAY
         ar-mcash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
         ar-mcash.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
         .
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
  FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable = "AR-MCASH"       
      AND reftable.company  = ar-mcash.company
      AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
      AND reftable.code     = ar-mcash.rec_key NO-ERROR.
  IF AVAIL reftable THEN
    ASSIGN fl_checkno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                    STRING(reftable.code2,"9999999999") .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR ll-new AS LOG NO-UNDO.
  DEF VAR old-payer AS CHAR NO-UNDO.
  DEF VAR old-desc AS CHAR NO-UNDO.
  DEF VAR old-date AS DATE NO-UNDO.
  DEF VAR old-bank-code AS CHAR NO-UNDO.
  DEF VAR old-curr-code AS CHAR NO-UNDO.
  DEF VAR old-posted AS LOG NO-UNDO.
  DEF VAR old-check-no AS CHAR NO-UNDO.

  DEF BUFFER bf-ar-mcash FOR ar-mcash.
  DEF BUFFER b-reftable FOR reftable. 

  /* Code placed here will execute PRIOR to standard behavior. */
  /* === validations ====*/
  DO WITH FRAME {&FRAME-NAME}: 
    RUN valid-bank-code (ar-mcash.bank-code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-curr-code (ar-mcash.curr-code[1]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN
       old-payer = ar-mcash.payer
       old-desc  = ar-mcash.dscr
       old-date  = ar-mcash.check-date
       old-bank-code = ar-mcash.bank-code
       old-curr-code = ar-mcash.curr-code[1]
       old-posted    = ar-mcash.posted.

    FIND FIRST b-reftable WHERE
         b-reftable.reftable = "AR-MCASH" AND
         b-reftable.company  = ar-mcash.company AND
         b-reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
         b-reftable.code     = ar-mcash.rec_key
         NO-LOCK NO-ERROR.

    IF AVAIL b-reftable THEN
       old-check-no = b-reftable.code2.
  END.

  ll-new = adm-new-record.    

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"line-item-target", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN add-first-line IN WIDGET-HANDLE(char-hdl) (ROWID(ar-mcash)).

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-line-target", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN auto-line-upd IN WIDGET-HANDLE(char-hdl).
  END.
  ELSE 
     FOR EACH bf-ar-mcash WHERE
         bf-ar-mcash.company      EQ cocode AND
         bf-ar-mcash.posted       EQ old-posted AND
         bf-ar-mcash.payer        EQ old-payer AND
         bf-ar-mcash.check-date   EQ old-date AND
         bf-ar-mcash.bank-code    EQ old-bank-code AND
         bf-ar-mcash.curr-code[1] EQ old-curr-code AND
         bf-ar-mcash.m-no         NE ar-mcash.m-no
         EXCLUSIVE-LOCK:

         ASSIGN
           bf-ar-mcash.payer        = ar-mcash.payer:SCREEN-VALUE
           bf-ar-mcash.dscr         = ar-mcash.dscr:SCREEN-VALUE
           bf-ar-mcash.check-date   = DATE(ar-mcash.check-date:SCREEN-VALUE)
           bf-ar-mcash.bank-code    = ar-mcash.bank-code:SCREEN-VALUE
           bf-ar-mcash.curr-code[1] = ar-mcash.curr-code[1]:SCREEN-VALUE.

         FIND FIRST b-reftable WHERE
              b-reftable.reftable = "AR-MCASH" AND
              b-reftable.company  = bf-ar-mcash.company AND
              b-reftable.loc      = STRING(bf-ar-mcash.m-no,">>>>>>9") AND
              b-reftable.code     = bf-ar-mcash.rec_key AND
              b-reftable.code2    = fl_checkno:SCREEN-VALUE
              EXCLUSIVE-LOCK NO-ERROR.

         IF NOT AVAIL b-reftable THEN
         DO:
            CREATE b-reftable.
            ASSIGN
               b-reftable.reftable = "AR-MCASH"
               b-reftable.company  = bf-ar-mcash.company
               b-reftable.loc      = STRING(bf-ar-mcash.m-no,">>>>>>9")
               b-reftable.code     = bf-ar-mcash.rec_key.
         END.

         b-reftable.code2 = fl_checkno:SCREEN-VALUE.
         RELEASE b-reftable.
     END.

  IF ll-new EQ NO THEN
  DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN repos-to-new IN WIDGET-HANDLE(char-hdl)(old-posted,
                                                   old-payer,
                                                   old-date, 
                                                   old-bank-code, 
                                                   old-curr-code,
                                                   old-check-no,
                                                   ar-mcash.posted,
                                                   ar-mcash.payer,
                                                   ar-mcash.check-date, 
                                                   ar-mcash.bank-code, 
                                                   ar-mcash.curr-code[1],
                                                   fl_checkno:SCREEN-VALUE).
  END.  
  /* gdm - */
  fl_checkno:SENSITIVE IN FRAME {&FRAME-NAME} = NO .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-curr-code V-table-Win 
PROCEDURE new-curr-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST currency NO-LOCK
        WHERE currency.company EQ cocode 
          AND currency.c-code  EQ ip-focus:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL currency THEN
      ar-mcash.ex-rate:SCREEN-VALUE = STRING(currency.ex-rate).
  END.

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
  {src/adm/template/sndkycas.i "company" "ar-mcash" "company"}
  {src/adm/template/sndkycas.i "m-no" "ar-mcash" "m-no"}

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
  {src/adm/template/snd-list.i "ar-mcash"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt V-table-Win 
PROCEDURE update-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

   IF AVAIL ar-mcash AND
      VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN repos-to-new IN WIDGET-HANDLE(char-hdl)(ar-mcash.posted,
                                                  ar-mcash.payer,
                                                  ar-mcash.check-date, 
                                                  ar-mcash.bank-code, 
                                                  ar-mcash.curr-code[1],
                                                  fl_checkno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                                  ar-mcash.posted,
                                                  ar-mcash.payer,
                                                  ar-mcash.check-date, 
                                                  ar-mcash.bank-code, 
                                                  ar-mcash.curr-code[1],
                                                  fl_checkno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt-line V-table-Win 
PROCEDURE update-tt-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-posted AS LOG NO-UNDO.
   DEFINE INPUT PARAMETER ip-payer AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-check-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER ip-bank-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-curr-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-check-no AS CHAR NO-UNDO.

   DEF BUFFER b-ar-mcash FOR ar-mcash.
   DEF BUFFER b3-reftable FOR reftable.

   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
   DO:
      FOR EACH b-ar-mcash WHERE
           b-ar-mcash.company      EQ cocode AND
           b-ar-mcash.posted       EQ ip-posted AND
           b-ar-mcash.payer        EQ ip-payer AND
           b-ar-mcash.check-date   EQ ip-check-date AND
           b-ar-mcash.bank-code    EQ ip-bank-code AND
           b-ar-mcash.curr-code[1] EQ ip-curr-code
           NO-LOCK,
           FIRST b3-reftable fields(code2) WHERE
                 b3-reftable.reftable = "AR-MCASH" AND
                 b3-reftable.company  = b-ar-mcash.company and
                 b3-reftable.loc      = STRING(b-ar-mcash.m-no,">>>>>>9") AND
                 b3-reftable.code     = b-ar-mcash.rec_key AND
                 b3-reftable.code2    = ip-check-no
                 NO-LOCK:

         RUN repos-to-new IN WIDGET-HANDLE(char-hdl)(b-ar-mcash.posted,
                                                     b-ar-mcash.payer,
                                                     b-ar-mcash.check-date, 
                                                     b-ar-mcash.bank-code, 
                                                     b-ar-mcash.curr-code[1],
                                                     b3-reftable.code2,
                                                     b-ar-mcash.posted,
                                                     b-ar-mcash.payer,
                                                     b-ar-mcash.check-date, 
                                                     b-ar-mcash.bank-code, 
                                                     b-ar-mcash.curr-code[1],
                                                     b3-reftable.code2).
         LEAVE.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bank-code V-table-Win 
PROCEDURE valid-bank-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST bank
                    WHERE bank.company   EQ cocode
                      AND bank.bank-code EQ ip-focus:SCREEN-VALUE)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-curr-code V-table-Win 
PROCEDURE valid-curr-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST currency
                    WHERE currency.company EQ cocode 
                      AND currency.c-code  EQ ip-focus:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN NO-APPLY.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

