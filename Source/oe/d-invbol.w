&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-cancel AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF NEW SHARED VAR out-recid AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES inv-line oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE inv-line


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-line, oe-bolh.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-invbol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-oerel AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     SPACE(121.25) SKIP(9.34)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bins For Invoice Line".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   External Tables: ASI.inv-line,ASI.oe-bolh
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
                                                                        */
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
ON GO OF FRAME D-Dialog /* Bins For Invoice Line */
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR li-ship-qty LIKE inv-line.ship-qty NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-target", OUTPUT char-hdl).

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.i-no    EQ inv-line.i-no
        AND oe-boll.line    EQ inv-line.line
      NO-LOCK:

    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(oe-boll)).

    RUN validate-all IN WIDGET-HANDLE(char-hdl) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    li-ship-qty = li-ship-qty + oe-boll.qty.
  END.

  IF li-ship-qty NE inv-line.ship-qty THEN DO:
    ll = NO.
    MESSAGE "Total bin quantity must equal invoice line ship quantity..." SKIP
            "Return to invoice line?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
    IF ll THEN DO:
      RUN purge-release.
      FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
        DELETE oe-boll.
      END.
      DELETE oe-bolh.
    END.

    ELSE RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Bins For Invoice Line */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "go":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
DEF VAR li AS INT NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.
DEF VAR ll-none AS LOG NO-UNDO.
DEF VAR li-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR v-nxt-r-no AS INT NO-UNDO.

FIND inv-line WHERE ROWID(inv-line) EQ ip-rowid NO-ERROR.
IF NOT AVAIL inv-line THEN RETURN.

DO TRANSACTION:

  IF inv-line.b-no NE 0 THEN
     FIND FIRST oe-bolh WHERE
          oe-bolh.company EQ cocode AND
          oe-bolh.b-no EQ inv-line.b-no
          NO-LOCK NO-ERROR.
/*   10051225 */
/*   FIND LAST oe-relh USE-INDEX r-no NO-LOCK NO-ERROR. */
/*   li = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.    */
  RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT v-nxt-r-no).

  RUN oe/release#.p (cocode, OUTPUT li2).
  
  CREATE oe-relh.
  ASSIGN
   oe-relh.company  = cocode
   oe-relh.r-no     = v-nxt-r-no
   oe-relh.release# = li2
   oe-relh.posted   = YES
   oe-relh.user-id  = USERID("nosweat")
   oe-relh.upd-time = TIME
   oe-relh.upd-date = TODAY
   out-recid = RECID(oe-relh).

  RUN oe/fifoloop.p (ROWID(inv-line), NO, OUTPUT ll-none).
  
  IF NOT ll-none THEN
     FOR EACH oe-rell FIELDS(qty)
         WHERE oe-rell.company EQ oe-relh.company
           AND oe-rell.r-no    EQ oe-relh.r-no
         USE-INDEX r-no NO-LOCK:
         li-qty = li-qty + oe-rell.qty.
     END.
END.

IF li-qty GE inv-line.ship-qty THEN DO TRANSACTION:
  FIND CURRENT inv-line.

  IF NOT AVAIL oe-bolh THEN
  DO:
     FIND LAST oe-bolh USE-INDEX b-no NO-LOCK NO-ERROR.
     li = IF AVAIL oe-bolh THEN oe-bolh.b-no ELSE 0.
    
     CREATE oe-bolh.
     ASSIGN
      oe-bolh.company  = cocode
      oe-bolh.loc      = locode
      oe-bolh.b-no     = li + 1
      oe-bolh.release# = oe-relh.release#
      inv-line.b-no    = oe-bolh.b-no
      oe-bolh.printed  = YES
      oe-bolh.posted   = YES
      oe-bolh.deleted  = YES
      oe-bolh.bol-date = TODAY.
  
     FOR EACH oe-rell
         WHERE oe-rell.company EQ oe-relh.company
           AND oe-rell.r-no    EQ oe-relh.r-no
         NO-LOCK:
       CREATE oe-boll.
       BUFFER-COPY oe-rell TO oe-boll
       ASSIGN
        oe-boll.bol-no = oe-bolh.bol-no
        oe-boll.ord-no = oe-bolh.ord-no
        oe-boll.b-no   = oe-bolh.b-no
        oe-boll.r-no   = oe-bolh.r-no.
     END.
  END.

  FIND CURRENT inv-line NO-LOCK.
END.

ELSE DO:
  MESSAGE "Insufficient qty in FG bins, decrease ship qty or receive FG..."
      VIEW-AS ALERT-BOX ERROR.
  op-cancel = YES.
END.

RUN purge-release.

IF NOT op-cancel THEN DO:
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
             INPUT  'oe/b-invbol.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-invbol ).
       RUN set-position IN h_b-invbol ( 1.24 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-invbol ( 6.67 , 120.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-oerel.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-oerel ).
       RUN set-position IN h_p-oerel ( 8.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-oerel ( 1.91 , 120.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-invbol. */
       RUN add-link IN adm-broker-hdl ( h_p-oerel , 'TableIO':U , h_b-invbol ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_b-invbol ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-oerel ,
             h_b-invbol , 'AFTER':U ).
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
  {src/adm/template/row-list.i "inv-line"}
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-line"}
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purge-release D-Dialog 
PROCEDURE purge-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no:
    DELETE oe-rell.
  END.

  DELETE oe-relh.

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
  {src/adm/template/snd-list.i "inv-line"}
  {src/adm/template/snd-list.i "oe-bolh"}

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

