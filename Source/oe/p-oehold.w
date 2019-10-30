&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe/p-oehold.w

  Description: Links to order viewer to place order on hold status.

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


DEFINE VARIABLE hViewer  AS HANDLE NO-UNDO.
DEFINE VARIABLE char-hdl AS CHAR NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lARAutoReleaseCreditHold AS LOGICAL NO-UNDO .
DEFINE VARIABLE cCustStatCheck AS CHARACTER NO-UNDO .
/* DEFINE VARIABLE char-val AS CHAR NO-UNDO INIT "".  */
/*                                                    */
/* DEFINE BUFFER b-oe-ord FOR oe-ord.                 */
/* DEFINE BUFFER b-oe-ordl FOR oe-ordl.               */
{oe/ordholdstat.i}

{methods/prgsecdt.i}

  {sys/inc/var.i "new shared" }

    ASSIGN
    cocode = g_company
    locode = g_loc.

{sys/inc/oecredit.i}



RUN sys/ref/nk1look.p (INPUT g_company, "ARAutoReleaseCreditHold", "L" /* Logical */, NO /* check by cust */, 
                          INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                          OUTPUT cRtnChar, OUTPUT lRecFound).
   IF lRecFound THEN
       lARAutoReleaseCreditHold = LOGICAL(cRtnChar) NO-ERROR.

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord

/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btnHold 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validHandle V-table-Win 
FUNCTION validHandle RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnHold 
     LABEL "Hold" 
     SIZE 9 BY 1.29 TOOLTIP "Change Order Hold Status".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnHold AT ROW 1.29 COL 2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         WIDTH              = 66.
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

ASSIGN 
       btnHold:PRIVATE-DATA IN FRAME F-Main     = 
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

&Scoped-define SELF-NAME btnHold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHold V-table-Win
ON CHOOSE OF btnHold IN FRAME F-Main /* Hold */
DO:
    RUN Process-Hold-Status.
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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-all V-table-Win 
PROCEDURE disable-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-all V-table-Win 
PROCEDURE enable-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE ALL.
  END.

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
 IF NOT v-can-update THEN ASSIGN btnHold:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                                  .
 IF NOT v-can-run THEN DISABLE ALL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-Hold-Status V-table-Win 
PROCEDURE Process-Hold-Status :
/*------------------------------------------------------------------------------
  Purpose:     Processes the hold status of the order.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE viCompany LIKE oe-ord.company NO-UNDO.
   DEFINE VARIABLE viOrdNum LIKE oe-ord.ord-no NO-UNDO.
   DEFINE BUFFER bff-oe-ord FOR oe-ord.

   IF NOT validHandle() THEN RETURN.


   /* Get the order number from the viewer. */
   RUN Get-current-order IN hViewer (OUTPUT viCompany, OUTPUT viOrdNum) NO-ERROR.

   RUN os-Process-Hold-Status (INPUT viCompany, INPUT viOrdNum).

   /* Find the order record. */
    FIND FIRST bff-oe-ord NO-LOCK WHERE
        bff-oe-ord.company EQ viCompany AND
        bff-oe-ord.ord-no  EQ viOrdNum NO-ERROR .
    IF AVAIL bff-oe-ord THEN
        FIND FIRST cust NO-LOCK
          WHERE cust.company EQ bff-oe-ord.company
          AND cust.cust-no EQ bff-oe-ord.cust-no NO-ERROR .
    
    IF lARAutoReleaseCreditHold AND AVAIL cust AND cust.active NE "X" AND bff-oe-ord.type NE "T"
        AND bff-oe-ord.stat NE "H" THEN DO:
          FIND CURRENT cust NO-LOCK NO-ERROR.
          cCustStatCheck = cust.cust-no .
          RUN check-status .
    END.

   /* Refresh the viewer to get updated information. */
     IF VALID-HANDLE(hViewer) THEN
      RUN dispatch IN hViewer  ('row-available':U).


   RETURN.


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

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT v-can-update THEN ASSIGN btnHold:SENSITIVE IN FRAME {&FRAME-NAME} = NO .
      ELSE ENABLE btnHold.
    IF AVAIL oe-ord THEN
      btnHold:LABEL = IF oe-ord.stat EQ "H" THEN "Re&lease" ELSE "&Hold".
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-status V-table-Win 
 PROCEDURE check-status :
 /*------------------------------------------------------------------------------
   Purpose:     
   Parameters:  <none>
   Notes:       
 ------------------------------------------------------------------------------*/
 
 DEFINE VARIABLE ld-ord-bal      LIKE cust.ord-bal NO-UNDO.
 DEFINE VARIABLE lRelHold AS LOGICAL NO-UNDO .
 FOR EACH cust EXCLUSIVE-LOCK
     WHERE cust.company EQ g_company
       AND LOOKUP(cust.cust-no,cCustStatCheck) NE 0
       AND cust.cust-no NE "" :
            
     FIND FIRST  terms NO-LOCK
         WHERE terms.company = cust.company
           AND terms.t-code  = cust.terms NO-ERROR.
     
     IF cust.cr-hold THEN do:
         ld-ord-bal      = cust.ord-bal.

         IF oecredit-cha EQ "" THEN
             RUN ar/updcust1.p (YES, BUFFER cust, OUTPUT ld-ord-bal).

          lRelHold = NO.
         IF ld-ord-bal + cust.acc-bal LT cust.cr-lim 
             AND ld-ord-bal LT cust.ord-lim THEN
             ASSIGN lRelHold = YES . 
         

              IF lRelHold AND lARAutoReleaseCreditHold THEN  DO:  
                  ASSIGN cust.cr-hold = NO .
                  
                  FOR EACH oe-ord EXCLUSIVE-LOCK
                     WHERE oe-ord.company             EQ g_company
                       AND oe-ord.cust-no             EQ cust.cust-no
                       AND oe-ord.stat EQ "H" :
                     ASSIGN
                         oe-ord.stat = "A" .
                  END.
              END. /* lRelHold*/

     END. /* cust.cr-hold */
 END. /* for cust */
 
 END PROCEDURE.
 
 /* _UIB-CODE-BLOCK-END */
 &ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validHandle V-table-Win 
FUNCTION validHandle RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   IF VALID-HANDLE(hViewer) THEN RETURN YES.

   
   /* Get the handle to the linked viewer. */
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"status-source", OUTPUT char-hdl).

   /* If not found, then abort. */
   IF NOT VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RETURN FALSE.

   /* Save a permanent handle to the viewer object. */
   ASSIGN hViewer = WIDGET-HANDLE(char-hdl).


   IF VALID-HANDLE(hViewer) THEN RETURN YES.
   ELSE
       RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

