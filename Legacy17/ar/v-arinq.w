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
&Scoped-define EXTERNAL-TABLES ar-inv ar-invl
&Scoped-define FIRST-EXTERNAL-TABLE ar-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-inv, ar-invl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-inv.cust-no ar-inv.cust-name ar-inv.gross ~
ar-inv.ship-id ar-inv.freight ar-inv.inv-no ar-invl.cost ar-invl.dscr[1] ~
ar-inv.tax-amt ar-inv.tax-code ar-inv.terms ar-inv.terms-d ar-inv.inv-date ~
ar-inv.disc-% ar-inv.disc-taken ar-inv.due-date ar-inv.disc-days ~
ar-inv.paid ar-invl.ord-no ar-inv.carrier ar-inv.po-no ar-inv.due 
&Scoped-define ENABLED-TABLES ar-inv ar-invl
&Scoped-define FIRST-ENABLED-TABLE ar-inv
&Scoped-define SECOND-ENABLED-TABLE ar-invl
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS ar-inv.cust-no ar-inv.cust-name ~
ar-inv.gross ar-inv.ship-id ar-inv.freight ar-inv.inv-no ar-invl.cost ~
ar-invl.dscr[1] ar-inv.tax-amt ar-inv.tax-code ar-inv.terms ar-inv.terms-d ~
ar-inv.inv-date ar-inv.disc-% ar-inv.disc-taken ar-inv.due-date ~
ar-inv.disc-days ar-inv.paid ar-invl.ord-no ar-inv.carrier ar-inv.po-no ~
ar-inv.due 
&Scoped-define DISPLAYED-TABLES ar-inv ar-invl
&Scoped-define FIRST-DISPLAYED-TABLE ar-inv
&Scoped-define SECOND-DISPLAYED-TABLE ar-invl


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
x-no|y|y|ASI.ar-inv.x-no
check-no||y|ASI.ar-inv.check-no
company||y|ASI.ar-inv.company
Carrier||y|ASI.ar-inv.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "x-no",
     Keys-Supplied = "x-no,check-no,company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ar-inv.cust-no AT ROW 1.24 COL 19 COLON-ALIGNED
          LABEL "Cust#"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.cust-name AT ROW 1.24 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 57 BY 1
     ar-inv.gross AT ROW 1.24 COL 119 COLON-ALIGNED
          LABEL "Invoice Amt"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     ar-inv.ship-id AT ROW 2.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-inv.freight AT ROW 2.19 COL 119 COLON-ALIGNED
          LABEL "Freight"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ar-inv.inv-no AT ROW 3.14 COL 19 COLON-ALIGNED
          LABEL "Invoice#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-invl.cost AT ROW 3.14 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-invl.dscr[1] AT ROW 3.14 COL 99.6 COLON-ALIGNED HELP
          "" NO-LABEL FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     ar-inv.tax-amt AT ROW 3.14 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ar-inv.tax-code AT ROW 4.1 COL 19 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     ar-inv.terms AT ROW 5.05 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ar-inv.terms-d AT ROW 5.05 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 65 BY 1
     ar-inv.inv-date AT ROW 6 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.disc-% AT ROW 6 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.disc-taken AT ROW 6 COL 119 COLON-ALIGNED
          LABEL "Discount"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     ar-inv.due-date AT ROW 6.95 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.disc-days AT ROW 6.95 COL 65 COLON-ALIGNED
          LABEL "Disc Days"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ar-inv.paid AT ROW 6.95 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     ar-invl.ord-no AT ROW 7.91 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.carrier AT ROW 7.91 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     ar-inv.po-no AT ROW 7.91 COL 77 COLON-ALIGNED
          LABEL "PO#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-inv.due AT ROW 7.91 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ar-inv,asi.ar-invl
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
         HEIGHT             = 8.57
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

/* SETTINGS FOR FILL-IN ar-inv.cust-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.disc-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.disc-taken IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.dscr[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ar-inv.freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.gross IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.inv-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.tax-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.terms-d IN FRAME F-Main
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
    WHEN 'x-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ar-inv
           &WHERE = "WHERE ar-inv.x-no eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "ar-inv"}
  {src/adm/template/row-list.i "ar-invl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-inv"}
  {src/adm/template/row-find.i "ar-invl"}

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
  IF AVAIL ar-inv THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT ar-inv.f-bill THEN ar-inv.freight:SCREEN-VALUE = "".
  END.

  DO WITH FRAME {&FRAME-NAME}:
     IF avail ar-invl and not ar-invl.dscr[1]:hidden and ar-invl.dscr[1] EQ "" THEN
        ar-invl.dscr[1]:SCREEN-VALUE = "M".
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
  ASSIGN
     ar-invl.cost:HIDDEN IN FRAME {&FRAME-NAME} = NOT
        CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ ar-invl.company
                              AND sys-ctrl.name EQ 'OECOMM'
                              AND sys-ctrl.log-fld EQ YES)
     ar-invl.dscr[1]:HIDDEN IN FRAME {&FRAME-NAME} = ar-invl.cost:HIDDEN IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/sndkycas.i "x-no" "ar-inv" "x-no"}
  {src/adm/template/sndkycas.i "check-no" "ar-inv" "check-no"}
  {src/adm/template/sndkycas.i "company" "ar-inv" "company"}
  {src/adm/template/sndkycas.i "Carrier" "ar-inv" "Carrier"}

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
  {src/adm/template/snd-list.i "ar-inv"}
  {src/adm/template/snd-list.i "ar-invl"}

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

