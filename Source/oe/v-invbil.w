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

/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO. 

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
&Scoped-define EXTERNAL-TABLES inv-head
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS  inv-head.spare-char-5 
&Scoped-define ENABLED-TABLES inv-head
&Scoped-define FIRST-ENABLED-TABLE inv-head
&Scoped-Define ENABLED-OBJECTS RECT-42 RECT-43 cBillNotes1 cBillNotes2 ~
cBillNotes3 cBillNotes4 
&Scoped-Define DISPLAYED-FIELDS inv-head.spare-char-5 
&Scoped-define DISPLAYED-TABLES inv-head
&Scoped-define FIRST-DISPLAYED-TABLE inv-head
&Scoped-Define DISPLAYED-OBJECTS cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4


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
DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 5.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 2.19.
     
 
DEFINE VARIABLE cBillNotes1 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 .
     
DEFINE VARIABLE cBillNotes2 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 .
     
DEFINE VARIABLE cBillNotes3 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 .
     
DEFINE VARIABLE cBillNotes4 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 .     


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main     
     cBillNotes1 AT ROW 1.95 COL 5 COLON-ALIGNED NO-LABEL     
     cBillNotes2 AT ROW 2.91 COL 5 COLON-ALIGNED NO-LABEL
     cBillNotes3 AT ROW 3.95 COL 5 COLON-ALIGNED NO-LABEL
     cBillNotes4 AT ROW 4.95 COL 5 COLON-ALIGNED NO-LABEL
     inv-head.spare-char-5 AT ROW 7.14 COL 7.6 NO-LABEL WIDGET-ID 4 FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 99 BY 1
     "Invoice Comments" VIEW-AS TEXT
          SIZE 18.8 BY .62 AT ROW 6.05 COL 3 WIDGET-ID 8
     "Bill Instructions" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.24 COL 3
     RECT-42 AT ROW 1.48 COL 1
     RECT-43 AT ROW 6.43 COL 1 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.inv-head
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
         HEIGHT             = 9.24
         WIDTH              = 122.4.
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

/* SETTINGS FOR FILL-IN inv-head.spare-char-5 IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME



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
  {src/adm/template/row-list.i "inv-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-inv-head FOR inv-head.

  /* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST cust NO-LOCK 
         WHERE cust.company EQ inv-head.company
         AND cust.cust-no EQ inv-head.cust-no NO-ERROR .
         
   IF AVAIL inv-head THEN DO:  
       FIND CURRENT inv-head EXCLUSIVE-LOCK NO-ERROR .
       ASSIGN 
           inv-head.bill-i[1] = cBillNotes1
           inv-head.bill-i[2] = cBillNotes2
           inv-head.bill-i[3] = cBillNotes3
           inv-head.bill-i[4] = cBillNotes4.    
       FIND CURRENT inv-head NO-LOCK NO-ERROR .    
    END.        
         
  IF AVAIL cust AND cust.inv-meth EQ ? THEN do:        
     FIND FIRST bf-inv-head  EXCLUSIVE-LOCK
         WHERE bf-inv-head.company EQ inv-head.company
         AND bf-inv-head.bol-no EQ inv-head.bol-no
         AND bf-inv-head.cust-no EQ inv-head.cust-no
         AND bf-inv-head.multi-invoice NO-ERROR.
         IF AVAIL bf-inv-head THEN
         DO:   
           ASSIGN 
           bf-inv-head.bill-i[1] = cBillNotes1
           bf-inv-head.bill-i[2] = cBillNotes2
           bf-inv-head.bill-i[3] = cBillNotes3
           bf-inv-head.bill-i[4] = cBillNotes4.             
         END.  
         RELEASE bf-inv-head .
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
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE  cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
 END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win
PROCEDURE local-disable-fields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER bf-inv-head FOR inv-head.
  /* Code placed here will execute PRIOR to standard behavior. */
  
    /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE  cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
    FIND FIRST cust NO-LOCK 
         WHERE cust.company EQ inv-head.company
         AND cust.cust-no EQ inv-head.cust-no NO-ERROR .
         
    IF AVAIL inv-head AND AVAIL cust AND cust.inv-meth EQ ? THEN do:        
     FIND FIRST bf-inv-head  NO-LOCK
         WHERE bf-inv-head.company EQ inv-head.company
         AND bf-inv-head.bol-no EQ inv-head.bol-no
         AND bf-inv-head.cust-no EQ inv-head.cust-no
         AND bf-inv-head.multi-invoice NO-ERROR.
         IF AVAIL bf-inv-head THEN
         DO:
           ASSIGN 
           cBillNotes1 = bf-inv-head.bill-i[1]
           cBillNotes2 = bf-inv-head.bill-i[2]
           cBillNotes3 = bf-inv-head.bill-i[3]
           cBillNotes4 = bf-inv-head.bill-i[4] .             
         END.                
    END.
    ELSE IF AVAIL inv-head THEN DO:  
       ASSIGN 
           cBillNotes1 = inv-head.bill-i[1]
           cBillNotes2 = inv-head.bill-i[2]
           cBillNotes3 = inv-head.bill-i[3]
           cBillNotes4 = inv-head.bill-i[4] .      
    END.
    DISPLAY cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
  END.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lUpdNotes AS LOGICAL NO-UNDO.
  DEFINE BUFFER bf-head FOR inv-head .
  
  lUpdNotes = YES.
  FIND FIRST cust WHERE
            cust.company EQ inv-head.company AND
            cust.cust-no EQ inv-head.cust-no
            NO-LOCK NO-ERROR.
            
  IF AVAIL cust AND cust.inv-meth EQ ? AND AVAIL inv-head AND inv-head.inv-no EQ 0 THEN
  DO:
     MESSAGE "You are editing bill notes on an invoice for a customer that invoices per user selection."
     "Editing these bill notes will change bill notes for all unprinted invoices for this customer.  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE lUpdNotes.  
  END.
  IF NOT lUpdNotes THEN RETURN .  
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN  cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF lUpdNotes AND AVAIL cust AND cust.inv-meth EQ ? 
     AND AVAIL inv-head AND inv-head.inv-no EQ 0 THEN
  DO:
     FOR EACH bf-head WHERE
         bf-head.company EQ inv-head.company AND
         bf-head.cust-no EQ inv-head.cust-no AND
         ROWID(bf-head) NE ROWID(inv-head) AND
         bf-head.inv-no EQ 0:           
          ASSIGN 
           bf-head.bill-i[1] = inv-head.bill-i[1]
           bf-head.bill-i[1] = inv-head.bill-i[2]
           bf-head.bill-i[1] = inv-head.bill-i[3]
           bf-head.bill-i[1] = inv-head.bill-i[4] .              
     END.  
  END.

 DO WITH FRAME {&FRAME-NAME}:
    DISABLE  cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
 END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */  
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE  cBillNotes1 cBillNotes2 cBillNotes3 cBillNotes4.
 END. 
 
 {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
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

