&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewerid/<table>.w

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES inv-line
&Scoped-define FIRST-EXTERNAL-TABLE inv-line


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-line.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-line.ord-no inv-line.po-no ~
inv-line.job-no inv-line.job-no2 inv-line.est-no inv-line.i-no ~
inv-line.part-no inv-line.qty inv-line.i-name inv-line.ship-qty ~
inv-line.i-dscr inv-line.inv-qty inv-line.part-dscr1 inv-line.price ~
inv-line.cons-uom inv-line.sman[1] inv-line.comm-amt[1] inv-line.cost ~
inv-line.cas-cnt inv-line.sman[2] inv-line.comm-amt[2] inv-line.disc ~
inv-line.sman[3] inv-line.comm-amt[3] inv-line.t-cost inv-line.tax 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}ord-no ~{&FP2}ord-no ~{&FP3}~
 ~{&FP1}po-no ~{&FP2}po-no ~{&FP3}~
 ~{&FP1}job-no ~{&FP2}job-no ~{&FP3}~
 ~{&FP1}job-no2 ~{&FP2}job-no2 ~{&FP3}~
 ~{&FP1}est-no ~{&FP2}est-no ~{&FP3}~
 ~{&FP1}i-no ~{&FP2}i-no ~{&FP3}~
 ~{&FP1}part-no ~{&FP2}part-no ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}i-name ~{&FP2}i-name ~{&FP3}~
 ~{&FP1}ship-qty ~{&FP2}ship-qty ~{&FP3}~
 ~{&FP1}i-dscr ~{&FP2}i-dscr ~{&FP3}~
 ~{&FP1}inv-qty ~{&FP2}inv-qty ~{&FP3}~
 ~{&FP1}part-dscr1 ~{&FP2}part-dscr1 ~{&FP3}~
 ~{&FP1}price ~{&FP2}price ~{&FP3}~
 ~{&FP1}cons-uom ~{&FP2}cons-uom ~{&FP3}~
 ~{&FP1}sman[1] ~{&FP2}sman[1] ~{&FP3}~
 ~{&FP1}comm-amt[1] ~{&FP2}comm-amt[1] ~{&FP3}~
 ~{&FP1}cost ~{&FP2}cost ~{&FP3}~
 ~{&FP1}cas-cnt ~{&FP2}cas-cnt ~{&FP3}~
 ~{&FP1}sman[2] ~{&FP2}sman[2] ~{&FP3}~
 ~{&FP1}comm-amt[2] ~{&FP2}comm-amt[2] ~{&FP3}~
 ~{&FP1}disc ~{&FP2}disc ~{&FP3}~
 ~{&FP1}sman[3] ~{&FP2}sman[3] ~{&FP3}~
 ~{&FP1}comm-amt[3] ~{&FP2}comm-amt[3] ~{&FP3}~
 ~{&FP1}t-cost ~{&FP2}t-cost ~{&FP3}~
 ~{&FP1}tax ~{&FP2}tax ~{&FP3}
&Scoped-define ENABLED-TABLES inv-line
&Scoped-define FIRST-ENABLED-TABLE inv-line
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS inv-line.ord-no inv-line.po-no ~
inv-line.job-no inv-line.job-no2 inv-line.est-no inv-line.i-no ~
inv-line.part-no inv-line.qty inv-line.i-name inv-line.ship-qty ~
inv-line.i-dscr inv-line.inv-qty inv-line.part-dscr1 inv-line.price ~
inv-line.cons-uom inv-line.sman[1] inv-line.comm-amt[1] inv-line.cost ~
inv-line.cas-cnt inv-line.sman[2] inv-line.comm-amt[2] inv-line.disc ~
inv-line.sman[3] inv-line.comm-amt[3] inv-line.t-cost inv-line.tax 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     inv-line.ord-no AT ROW 10.29 COL 20 COLON-ALIGNED
          LABEL "Order"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     inv-line.po-no AT ROW 10.29 COL 42 COLON-ALIGNED
          LABEL "Cust PO"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.job-no AT ROW 10.29 COL 85 COLON-ALIGNED
          LABEL "Job"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     inv-line.job-no2 AT ROW 10.29 COL 96.4
          LABEL "-"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     inv-line.est-no AT ROW 10.29 COL 115 COLON-ALIGNED
          LABEL "Est."
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     inv-line.i-no AT ROW 11.24 COL 20 COLON-ALIGNED
          LABEL "Item"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.part-no AT ROW 11.24 COL 85 COLON-ALIGNED
          LABEL "Cust Part #"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.qty AT ROW 12.19 COL 20 COLON-ALIGNED
          LABEL "Qty Order"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     inv-line.i-name AT ROW 12.19 COL 85 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.ship-qty AT ROW 13.14 COL 20 COLON-ALIGNED
          LABEL "Qty Ship"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     inv-line.i-dscr AT ROW 13.14 COL 85 COLON-ALIGNED
          LABEL "Item Dscr"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.inv-qty AT ROW 14.1 COL 20 COLON-ALIGNED
          LABEL "Qty Invoice"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     inv-line.part-dscr1 AT ROW 14.1 COL 85 COLON-ALIGNED
          LABEL "Dscr"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     inv-line.price AT ROW 15.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     inv-line.cons-uom AT ROW 15.05 COL 48 COLON-ALIGNED
          LABEL "UOM"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     inv-line.sman[1] AT ROW 15.05 COL 85 COLON-ALIGNED
          LABEL "SLMN"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     inv-line.comm-amt[1] AT ROW 15.05 COL 104 COLON-ALIGNED
          LABEL "Comm $"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.cost AT ROW 16 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-line.cas-cnt AT ROW 16 COL 48 COLON-ALIGNED
          LABEL "Case"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     inv-line.sman[2] AT ROW 16 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     inv-line.comm-amt[2] AT ROW 16 COL 104 COLON-ALIGNED
          LABEL "Comm $"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.disc AT ROW 16.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     inv-line.sman[3] AT ROW 16.95 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     inv-line.comm-amt[3] AT ROW 16.95 COL 104 COLON-ALIGNED
          LABEL "Comm $"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     inv-line.t-cost AT ROW 17.91 COL 20 COLON-ALIGNED
          LABEL "Ext. Price"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     inv-line.tax AT ROW 17.91 COL 85 COLON-ALIGNED
          LABEL "Tax"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-1 AT ROW 9.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.inv-line
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 19.76
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN inv-line.cas-cnt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.comm-amt[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.cons-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.est-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.i-dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.inv-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.job-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.job-no2 IN FRAME F-Main
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN inv-line.ord-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.part-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.ship-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.sman[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.t-cost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-line.tax IN FRAME F-Main
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

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartViewerCues" V-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartViewer,uib,49270
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-line"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
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


