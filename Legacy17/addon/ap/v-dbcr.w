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
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED }

ASSIGN cocode = g_company
       locode = g_loc.

DEF BUFFER xap-pay FOR ap-pay.
DEF BUFFER bf-payl FOR ap-payl.
&SCOPED-DEFINE enable-appay proc-enable

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
&Scoped-define EXTERNAL-TABLES ap-pay vend
&Scoped-define FIRST-EXTERNAL-TABLE ap-pay


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-pay, vend.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ap-pay.check-date 
&Scoped-define ENABLED-TABLES ap-pay
&Scoped-define FIRST-ENABLED-TABLE ap-pay
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS ap-pay.vend-no ap-pay.check-no ~
ap-pay.check-date 
&Scoped-define DISPLAYED-TABLES ap-pay
&Scoped-define FIRST-DISPLAYED-TABLE ap-pay
&Scoped-Define DISPLAYED-OBJECTS vend_name ld-amt 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS ap-pay.vend-no 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
c-no|y|y|ASI.ap-pay.c-no
check-no||y|ASI.ap-pay.check-no
company||y|ASI.ap-pay.company
d-no||y|ASI.ap-pay.d-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "c-no",
     Keys-Supplied = "c-no,check-no,company,d-no"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE ld-amt AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE vend_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 81 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ap-pay.vend-no AT ROW 2.43 COL 19 COLON-ALIGNED
          LABEL "Vendor#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     vend_name AT ROW 2.43 COL 39 COLON-ALIGNED HELP
          "Enter Vendor name." NO-LABEL
     ap-pay.check-no AT ROW 3.43 COL 19 COLON-ALIGNED
          LABEL "Memo Number" FORMAT "9999999999"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ld-amt AT ROW 4.33 COL 54 COLON-ALIGNED
     ap-pay.check-date AT ROW 4.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ap-pay,ASI.vend
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

/* SETTINGS FOR FILL-IN ap-pay.check-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ld-amt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-pay.vend-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
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


&Scoped-define SELF-NAME ap-pay.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-pay.vend-no W-Win
ON HELP OF ap-pay.vend-no IN FRAME F-Main /* Vendor# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    APPLY "entry" TO {&self-name}.
    RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN ap-pay.vend-no:SCREEN-VALUE = ENTRY(1,char-val)
                                vend_name:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ap-pay.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-pay.vend-no V-table-Win
ON LEAVE OF ap-pay.vend-no IN FRAME F-Main /* Vendor# */
DO:
   IF LASTKEY = -1 THEN RETURN.

   FIND FIRST vend WHERE vend.company = g_company
                        AND vend.vend-no = ap-pay.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                        NO-LOCK NO-ERROR.
   IF NOT AVAIL vend THEN DO:
      MESSAGE "Vendor does not exist. Do you want to add it?" VIEW-AS ALERT-BOX
          QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
      IF ll-ans THEN do:
          RUN create-vend NO-ERROR.
          IF ERROR-STATU:ERROR THEN RETURN NO-APPLY.
      END.
      ELSE DO:
            MESSAGE "Invalid Vendor. Try Help." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ap-pay.vend-no.
            RETURN NO-APPLY.
      END.
   END.
   vend_name = IF AVAIL vend THEN vend.NAME ELSE "".
   
   DISP vend_name WITH FRAME {&FRAME-NAME}.

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
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    WHEN 'c-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ap-pay
           &WHERE = "WHERE ap-pay.c-no eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "ap-pay"}
  {src/adm/template/row-list.i "vend"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-pay"}
  {src/adm/template/row-find.i "vend"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-vend V-table-Win 
PROCEDURE create-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-saved AS LOG NO-UNDO.
  CREATE vend.
  ASSIGN vend.company = g_company
         vend.vend-no = ap-pay.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  RUN ap/d-crtvnd.w (RECID(vend), OUTPUT ll-saved).
  IF NOT ll-saved THEN do:
      DELETE vend.
      RETURN ERROR.
  END.


  /*
  RUN init-object IN THIS-PROCEDURE (
        INPUT  'ap/d-crtvnd.w':U ,
        INPUT  FRAME F-Main:HANDLE ,
        INPUT  'Layout = ':U ,
        OUTPUT h_d-crtvnd ).
  /*RUN set-position IN h_b-dbcr ( 4.81 , 3.00 ) NO-ERROR.
  RUN set-size IN h_b-dbcr ( 19.52 , 145.00 ) NO-ERROR.
  */
    /* Links to SmartNavBrowser h_b-dbcr. */
  RUN add-link IN adm-broker-hdl ( this-procedure , 'record':U , h_b-dbcr ).
  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR xchk LIKE ap-pay.check-no NO-UNDO.
  DEF VAR X AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  for each xap-pay use-index c-no no-lock 
           where xap-pay.memo by xap-pay.check-no descending:
    xchk = xap-pay.check-no.
    leave.
  end.
  x = 0.
  for each xap-pay use-index c-no no-lock by c-no descending:
    x = xap-pay.c-no.
    leave.
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/* ---------------------------------------------------- ap/ap-pay.a 10/97 fwk */
/* Add Mode for AP CR/DB Memo module                                          */
/* -------------------------------------------------------------------------- */

  

   ap-pay.check-no = xchk + 1.
   if ap-pay.check-no < 90000001 then ap-pay.check-no = 90000001.

   assign ap-pay.memo       = yes
          ap-pay.c-no       = x + 1
          ap-pay.company    = cocode
          ap-pay.check-date = today
      .

  RUN dispatch ('row-changed'). /* refresh line browser */
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
  IF AVAIL ap-pay THEN do:
      FIND FIRST vend WHERE vend.company = ap-pay.company
                        AND vend.vend-no = ap-pay.vend-no
                        NO-LOCK NO-ERROR.
      vend_name = IF AVAIL vend THEN vend.NAME ELSE "".
  END.
  DISP vend_name WITH FRAME {&FRAME-NAME}.

  ld-amt = 0.
  FOR EACH bf-payl OF ap-pay NO-LOCK:
    ld-amt = ld-amt - bf-payl.amt-paid + bf-payl.amt-disc.
  END.
 
  DISPLAY ld-amt WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF var ll-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST vend WHERE vend.company = ap-pay.company
                        AND vend.vend-no = ap-pay.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     MESSAGE "Vendor does not exist. Do you want to add it?" VIEW-AS ALERT-BOX
          QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
     IF ll-ans THEN do:
          RUN create-vend NO-ERROR.
          IF ERROR-STATU:ERROR THEN DO:
              APPLY "entry" TO ap-pay.vend-no.
              RETURN NO-APPLY.
          END.
     END.
     ELSE do:
         MESSAGE "Invalid Vendor. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ap-pay.vend-no.
         RETURN NO-APPLY.
     END.
  END.
  vend_name = IF AVAIL vend THEN vend.NAME ELSE "". 
  DISP vend_name WITH FRAME {&FRAME-NAME}.
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-new-record THEN DO:
      DEF VAR char-hdl AS cha NO-UNDO.
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
      RUN auto-line-add IN WIDGET-HANDLE(char-hdl).

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
 IF NOT adm-new-record AND ap-pay.posted THEN do:
       
      DEF VAR char-hdl AS cha NO-UNDO.
      MESSAGE "This Memo has been posted. No changes are allowed!"           
           VIEW-AS ALERT-BOX ERROR.
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
      RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
      
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
  FIND CURRENT ap-pay NO-LOCK NO-ERROR.
  IF AVAIL ap-pay THEN RUN dispatch ('display-fields').

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
  {src/adm/template/sndkycas.i "c-no" "ap-pay" "c-no"}
  {src/adm/template/sndkycas.i "check-no" "ap-pay" "check-no"}
  {src/adm/template/sndkycas.i "company" "ap-pay" "company"}
  {src/adm/template/sndkycas.i "d-no" "ap-pay" "d-no"}

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
  {src/adm/template/snd-list.i "ap-pay"}
  {src/adm/template/snd-list.i "vend"}

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

