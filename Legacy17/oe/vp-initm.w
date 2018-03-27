&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\vp-initm.w

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

def var char-hdl as cha no-undo.
DEF VAR ll-canceled AS LOG NO-UNDO.

{methods/prgsecdt.i}

{oe/tt-item-qty-price.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartPanel
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES inv-head inv-line
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head, inv-line.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-View Btn-Save Btn-Add Btn-Delete 

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

DEFINE BUTTON Btn-Delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON btn-his 
     LABEL "&History" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn-misc 
     LABEL "&Misc" 
     SIZE 15 BY 1.29.

DEFINE BUTTON Btn-Save 
     LABEL "&Update" 
     SIZE 15 BY 1.29
     FONT 4.

DEFINE BUTTON btn-ship 
     LABEL "S&hipped" 
     SIZE 15 BY 1.29.

DEFINE BUTTON Btn-View 
     LABEL "&View" 
     SIZE 15 BY 1.29
     FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn-View AT ROW 1 COL 1
     Btn-Save AT ROW 1 COL 16
     Btn-Add AT ROW 1 COL 31
     Btn-Delete AT ROW 1 COL 46
     btn-misc AT ROW 1 COL 61
     btn-ship AT ROW 1 COL 76
     btn-his AT ROW 1 COL 91
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartPanel
   External Tables: ASI.inv-head,ASI.inv-line
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
         HEIGHT             = 7.29
         WIDTH              = 140.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/winkit-panel.i}
{src/adm/method/panel.i}

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

ASSIGN 
       Btn-Add:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-Delete:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

/* SETTINGS FOR BUTTON btn-his IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-his:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

/* SETTINGS FOR BUTTON btn-misc IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-misc:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

ASSIGN 
       Btn-Save:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

/* SETTINGS FOR BUTTON btn-ship IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-ship:PRIVATE-DATA IN FRAME F-Main     = 
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

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add V-table-Win
ON CHOOSE OF Btn-Add IN FRAME F-Main /* Add */
DO:

   IF inv-head.bol-no <> 0 THEN DO:
      MESSAGE "Can't add item for Invoices created via BOL." VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   run oe/d-invitm.w (?, inv-head.r-no,"ADD").  
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run reopen-query in widget-handle(char-hdl) (?).
  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete V-table-Win
ON CHOOSE OF Btn-Delete IN FRAME F-Main /* Delete */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run delete-item in widget-handle(char-hdl).
  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-his
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-his V-table-Win
ON CHOOSE OF btn-his IN FRAME F-Main /* History */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"oeitem-target", output char-hdl).
   run select-his in widget-handle(char-hdl).

  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-misc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-misc V-table-Win
ON CHOOSE OF btn-misc IN FRAME F-Main /* Misc */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
   run select-price in widget-handle(char-hdl).

  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save V-table-Win
ON CHOOSE OF Btn-Save IN FRAME F-Main /* Update */
DO:
    run oe/d-invitm.w (recid(inv-line), inv-line.r-no,"Update").
    run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
    run reopen-query in widget-handle(char-hdl) (recid(inv-line)).
  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ship V-table-Win
ON CHOOSE OF btn-ship IN FRAME F-Main /* Shipped */
DO:
   run get-link-handle in adm-broker-hdl(this-procedure,"oeitem-target", output char-hdl).
   run select-stat in widget-handle(char-hdl).

  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-View V-table-Win
ON CHOOSE OF Btn-View IN FRAME F-Main /* View */
DO:
   run oe/d-invitm.w (recid(inv-line), inv-line.r-no,"view").

  {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _admPanels.p */
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

  {methods/setButton.i Btn-Add "Add"} /* added by script _panelImages.p */
  {methods/setButton.i Btn-Delete "Delete"} /* added by script _panelImages.p */
  {methods/setButton.i btn-his "History"} /* added by script _panelImages.p */
  {methods/setButton.i btn-misc "Misc"} /* added by script _panelImages.p */
  {methods/setButton.i Btn-Save "Update"} /* added by script _panelImages.p */
  {methods/setButton.i btn-ship "Shipped"} /* added by script _panelImages.p */
  {methods/setButton.i Btn-View "View"} /* added by script _panelImages.p */

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
  {src/adm/template/row-list.i "inv-head"}
  {src/adm/template/row-list.i "inv-line"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}
  {src/adm/template/row-find.i "inv-line"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add V-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "choose" TO btn-add IN FRAME {&FRAME-NAME}.

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
 /* Insert pre-dispatch code here. */ 
  IF access-close THEN  do:  /* YSK  not leave window on after closed */
     APPLY 'CLOSE' TO THIS-PROCEDURE.
     RETURN.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

   DO WITH FRAME {&FRAME-NAME}:
    IF NOT v-can-create THEN btn-add:SENSITIVE = NO.
    IF NOT v-can-update THEN btn-save:SENSITIVE = NO.
    IF NOT v-can-delete THEN btn-delete:SENSITIVE = NO.
    IF NOT v-can-run THEN btn-view:SENSITIVE = NO. 

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
  {src/adm/template/snd-list.i "inv-head"}
  {src/adm/template/snd-list.i "inv-line"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-item V-table-Win 
PROCEDURE update-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-rowid-list AS CHAR NO-UNDO. /* used to pull records from history */
  run oe/d-oeitem.w (recid(oe-ordl), oe-ordl.ord-no,"Update",
                     INPUT TABLE tt-item-qty-price, 
                     OUTPUT v-rowid-list,
                     OUTPUT ll-canceled).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

