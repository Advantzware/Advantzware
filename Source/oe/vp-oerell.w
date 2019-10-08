&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&SCOPED-DEFINE WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\vp-oerell.w

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
ASSIGN cocode = g_company.
ASSIGN locode = g_loc.

DEFINE TEMP-TABLE tt-oe-rell NO-UNDO LIKE oe-rell.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE SmartViewer
&SCOPED-DEFINE DB-AWARE no

&SCOPED-DEFINE ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&SCOPED-DEFINE FRAME-NAME F-Main

/* External Tables                                                      */
&SCOPED-DEFINE EXTERNAL-TABLES  oe-relh oe-rell oe-ordl
&SCOPED-DEFINE FIRST-EXTERNAL-TABLE oe-relh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-relh, oe-rell, oe-ordl .
/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS Btn-View Btn-Save Btn-Add Btn-copy ~
Btn-Delete btn-selbin 

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

DEFINE BUTTON btn-selbin 
     LABEL "Select &bins/tags" 
     SIZE 20 BY 1.29.

DEFINE BUTTON Btn-Save 
     LABEL "&Update" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON Btn-View 
     LABEL "&View" 
     SIZE 15 BY 1.29
     FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn-View AT ROW 1 COLUMN 1
     Btn-Save AT ROW 1 COLUMN 16
     Btn-Add AT ROW 1 COLUMN 31
     Btn-copy AT ROW 1 COLUMN 46
     Btn-Delete AT ROW 1 COLUMN 61
     btn-selbin AT ROW 1 COLUMN 76 WIDGET-ID 2
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
   NOT-VISIBLE Size-to-Fit                                              */
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
       Btn-Save:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       btn-selbin:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-View:PRIVATE-DATA IN FRAME F-Main     = 
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

&SCOPED-DEFINE SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add V-table-Win
ON CHOOSE OF Btn-Add IN FRAME F-Main /* Add */
DO:
  DEFINE BUFFER bf-oe-rell FOR oe-rell.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE iL AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.

  IF AVAILABLE oe-relh THEN
  DO:
     FOR EACH bf-oe-rell NO-LOCK 
         WHERE bf-oe-rell.company EQ oe-relh.company
           AND bf-oe-rell.r-no EQ oe-relh.r-no 
           AND bf-oe-rell.line NE 0
           BY bf-oe-rell.line:
         iL[1] = iL[1] + 1.
     END.

     RUN oe/d-oerell.w (?, RECID(oe-relh), "add",OUTPUT lv-rowid).
     
     FOR EACH bf-oe-rell NO-LOCK
         WHERE bf-oe-rell.company EQ oe-relh.company
           AND bf-oe-rell.r-no EQ oe-relh.r-no
           AND bf-oe-rell.line NE 0
           BY bf-oe-rell.line:
         ASSIGN
             iL[2]    = iL[2] + 1.
         /*lv-rowid = ROWID(bf-oe-rell)*/ 
     END.

     IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
       RUN local-open-query IN WIDGET-HANDLE(char-hdl).
       RUN reopen-query IN WIDGET-HANDLE(char-hdl)("add") .
       RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-line C-WIn 
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


&SCOPED-DEFINE SELF-NAME Btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-copy V-table-Win
ON CHOOSE OF Btn-copy IN FRAME F-Main /* Copy */
DO:
   DEFINE BUFFER bf-oe-rell FOR oe-rell.
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
   DEFINE VARIABLE iL AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.

   IF AVAILABLE oe-relh THEN
   DO:
      FOR EACH bf-oe-rell NO-LOCK
          WHERE bf-oe-rell.company EQ oe-relh.company
            AND bf-oe-rell.r-no EQ oe-relh.r-no
            AND bf-oe-rell.line NE 0
            BY bf-oe-rell.line:
          iL[1] = iL[1] + 1.
      END.
     
      FIND LAST bf-oe-rell NO-LOCK
           WHERE bf-oe-rell.company EQ oe-relh.company
             AND bf-oe-rell.r-no EQ oe-relh.r-no NO-ERROR.

      z = IF AVAILABLE bf-oe-rell THEN bf-oe-rell.line + 1 ELSE 1.
      
      CREATE bf-oe-rell.
      BUFFER-COPY oe-rell EXCEPT rec_key line TO bf-oe-rell.
      /*bf-oe-rell.LINE = z.*/
     
      RUN oe/d-oerell.w (RECID(bf-oe-rell), RECID(oe-relh), "Copy",OUTPUT lv-rowid).
     
      FOR EACH bf-oe-rell NO-LOCK
          WHERE bf-oe-rell.company EQ oe-relh.company
            AND bf-oe-rell.r-no EQ oe-relh.r-no
            AND bf-oe-rell.line NE 0
            BY bf-oe-rell.line:
          ASSIGN
              iL[2]    = iL[2] + 1.
          /*lv-rowid = ROWID(bf-oe-rell)*/ 
      END.
     
      IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN DO:
          
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
       RUN local-open-query IN WIDGET-HANDLE(char-hdl).
       RUN reopen-query IN WIDGET-HANDLE(char-hdl)("copy") .
       RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete V-table-Win
ON CHOOSE OF Btn-Delete IN FRAME F-Main /* Delete */
DO:
    IF AVAIL oe-rell THEN DO: 
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
      RUN delete_item IN WIDGET-HANDLE(char-hdl).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-selbin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-selbin V-table-Win
ON CHOOSE OF btn-selbin IN FRAME F-Main /* Recost Board */
DO:
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-target", OUTPUT char-hdl).
    RUN select-bintags IN WIDGET-HANDLE(char-hdl) .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save V-table-Win
ON CHOOSE OF Btn-Save IN FRAME F-Main /* Update */
DO:
    DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    IF AVAILABLE oe-relh AND AVAILABLE oe-rell THEN
    DO:
      IF oe-relh.stat NE "c" THEN DO:   
       EMPTY TEMP-TABLE tt-oe-rell.
      
       CREATE tt-oe-rell.
       BUFFER-COPY oe-rell TO tt-oe-rell.
      
       RUN oe/d-oerell.w (RECID(oe-rell),RECID(oe-relh), "update", OUTPUT lv-rowid) . 
       
       BUFFER-COMPARE tt-oe-rell TO oe-rell SAVE RESULT IN ll.
       
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
       RUN reopen-query IN WIDGET-HANDLE(char-hdl)("update") .
       RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
      
       /*RUN reopen-po-ord-query.*/
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
DO:
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  IF AVAILABLE oe-rell THEN
      RUN oe/d-oerell.w (RECID(oe-rell), RECID(oe-relh), "view",OUTPUT lv-rowid). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 

/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN          
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
  {src/adm/template/row-list.i "oe-relh"}
  {src/adm/template/row-list.i "oe-rell"}
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-relh"}
  {src/adm/template/row-find.i "oe-rell"}
  {src/adm/template/row-list.i "oe-ordl"}

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  DO WITH FRAME {&FRAME-NAME}: 
    IF AVAIL oe-relh AND oe-relh.posted EQ YES THEN DO:
        ASSIGN Btn-View:SENSITIVE = NO.                                                          
        Btn-Save:SENSITIVE = NO.
        Btn-Add:SENSITIVE = NO.
        Btn-copy:SENSITIVE = NO.
        Btn-Delete:SENSITIVE = NO.
        btn-selbin:SENSITIVE = NO.
    END.
    ELSE DO:
        ASSIGN Btn-View:SENSITIVE = YES.                                                          
        Btn-Save:SENSITIVE = YES.
        Btn-Add:SENSITIVE = YES.
        Btn-copy:SENSITIVE = YES.
        Btn-Delete:SENSITIVE = YES.
        btn-selbin:SENSITIVE = YES.
    END.
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
  {src/adm/template/snd-list.i "oe-relh"}
  {src/adm/template/snd-list.i "oe-rell"}
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

