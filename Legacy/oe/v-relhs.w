&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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
DEFINE VARIABLE glShipNotesExpanded AS LOGICAL NO-UNDO.
DEFINE VARIABLE opcParsedText AS CHARACTER NO-UNDO EXTENT 100.
DEFINE VARIABLE opiFilledArraySize AS INTEGER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES oe-relh
&Scoped-define FIRST-EXTERNAL-TABLE oe-relh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-relh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-relh.ship-i[1] oe-relh.ship-i[2] ~
oe-relh.ship-i[3] oe-relh.ship-i[4] 
&Scoped-define ENABLED-TABLES oe-relh
&Scoped-define FIRST-ENABLED-TABLE oe-relh
&Scoped-Define ENABLED-OBJECTS RECT-41 ship_note 
&Scoped-Define DISPLAYED-FIELDS oe-relh.ship-i[1] oe-relh.ship-i[2] ~
oe-relh.ship-i[3] oe-relh.ship-i[4] 
&Scoped-define DISPLAYED-TABLES oe-relh
&Scoped-define FIRST-DISPLAYED-TABLE oe-relh
&Scoped-Define DISPLAYED-OBJECTS ship_note 

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
DEFINE VARIABLE ship_note AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 104 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 5.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ship_note AT ROW 1.48 COL 5 NO-LABEL
     oe-relh.ship-i[1] AT ROW 1.48 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     oe-relh.ship-i[2] AT ROW 2.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     oe-relh.ship-i[3] AT ROW 3.38 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     oe-relh.ship-i[4] AT ROW 4.33 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 104 BY 1
     RECT-41 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-relh
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
         HEIGHT             = 6.86
         WIDTH              = 130.2.
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
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-relh.ship-i[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-relh.ship-i[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-relh.ship-i[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-relh.ship-i[4] IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME ship_note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship_note V-table-Win
ON LEAVE OF ship_note IN FRAME F-Main
DO:
  Define Variable hNotesProcs as Handle NO-UNDO. 
  RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  
  RUN ConvertToArray IN hNotesProcs (INPUT ship_note:SCREEN-VALUE, 
              INPUT 60,
              OUTPUT opcParsedText,
              OUTPUT opiFilledArraySize).  
  IF opiFilledArraySize GT 4 THEN DO:
    MESSAGE "Autoparsed lines exceed 4 lines of text. Only first 4 lines will be used." view-as alert-box error.       
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SESSION:DATA-ENTRY-RETURN = YES.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
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

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-relh"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetShipToExpanded V-table-Win
PROCEDURE pSetShipToExpanded:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .

RUN sys/ref/nk1look.p (ipcCompany, "ShipNotesExpanded", "L" /* Logical */, NO /* check by cust */, 
          INPUT NO /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
      OUTPUT cRtnChar, OUTPUT lRecFound).
  glShipNotesExpanded = LOGICAL(cRtnChar) NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_notes V-table-Win 
PROCEDURE enable_notes :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF glShipNotesExpanded EQ YES THEN DO:
      ASSIGN 
         oe-relh.ship-i[1]:HIDDEN IN FRAME F-Main           = TRUE
         oe-relh.ship-i[2]:HIDDEN IN FRAME F-Main           = TRUE
         oe-relh.ship-i[3]:HIDDEN IN FRAME F-Main           = TRUE
         oe-relh.ship-i[4]:HIDDEN IN FRAME F-Main           = TRUE
         .      
  END.
  ELSE DO:
      ASSIGN 
         ship_note:HIDDEN IN FRAME F-Main           = TRUE
         .      
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win
PROCEDURE local-cancel-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ship_note WITH FRAME {&FRAME-NAME}.


END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  Define Variable hNotesProcs as Handle NO-UNDO.
  IF AVAILABLE oe-relh THEN DO:
    RUN pSetShipToExpanded(oe-relh.company).
  END.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN enable_notes.
  ASSIGN ship_note = "".
  IF glShipNotesExpanded THEN DO:
      DISABLE ship_note WITH FRAME {&FRAME-NAME}.
      IF AVAILABLE oe-relh THEN DO:
          RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.
          RUN GetNoteOfType IN hNotesProcs (oe-relh.rec_key, "ES", OUTPUT ship_note).
          DELETE OBJECT hNotesProcs.
      END. /*IF AVAILABLE oe-rel THEN DO:*/
      DISPLAY ship_note WITH FRAME {&FRAME-NAME}.    
  END.  /*IF glShipNotesExpanded THEN DO:*/        
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win
PROCEDURE local-enable-fields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF glShipNotesExpanded THEN DO:
      ENABLE ship_note WITH FRAME {&FRAME-NAME}.         
  END.


END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win
PROCEDURE local-update-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  Define Variable hNotesProcs as Handle NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF glShipNotesExpanded EQ YES THEN DO:
        ASSIGN ship_note = ship_note:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        
        RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  
        RUN ConvertToArray IN hNotesProcs (INPUT ship_note, 
              INPUT 60,
              OUTPUT opcParsedText,
              OUTPUT opiFilledArraySize). 
         
        FIND CURRENT oe-relh EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE oe-relh THEN DO:
            ASSIGN
               oe-relh.ship-i[1] =  opcParsedText[1]
               oe-relh.ship-i[2] =  opcParsedText[2]
               oe-relh.ship-i[3] =  opcParsedText[3]
               oe-relh.ship-i[4] =  opcParsedText[4]
               .               
            RUN UpdateNoteOfType IN hNotesProcs (oe-relh.rec_key,
                                                 "ES", /*extended ShipNote*/
                                                 ship_note).
            DELETE OBJECT hNotesProcs.        
        END.
        FIND CURRENT oe-relh NO-LOCK NO-ERROR.
        DISABLE ship_note WITH FRAME {&FRAME-NAME}.
  END. /* IF glShipNotesExpanded EQ YES THEN DO: */
   

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME