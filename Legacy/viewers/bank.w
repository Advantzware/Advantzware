&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/bank.w

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
{custom/gcompany.i}
{custom/gloc.i}

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR char-val AS CHAR NO-UNDO.
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES bank
&Scoped-define FIRST-EXTERNAL-TABLE bank


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR bank.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bank.bank-code bank.bank-name bank.addr[1] ~
bank.addr[2] bank.city bank.state bank.zip bank.phone bank.contact ~
bank.bk-act bank.actnum bank.last-chk bank.spare-int-1 bank.bal ~
bank.pay-type  bank.o-chk bank.RTN bank.dep-tr bank.ODFI bank.serv ~
bank.ACHID bank.curr-code[1] bank.SwiftBIC  bank.Bank-ID 
&Scoped-define ENABLED-TABLES bank
&Scoped-define FIRST-ENABLED-TABLE bank
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-7 
&Scoped-Define DISPLAYED-FIELDS bank.bank-code bank.bank-name bank.addr[1] ~
bank.addr[2] bank.city bank.state bank.zip bank.phone bank.contact ~
bank.bk-act bank.actnum bank.last-chk bank.spare-int-1 bank.bal ~
bank.pay-type bank.o-chk bank.RTN bank.dep-tr bank.ODFI bank.serv ~
bank.ACHID bank.curr-code[1] bank.SwiftBIC  bank.Bank-ID
&Scoped-define DISPLAYED-TABLES bank
&Scoped-define FIRST-DISPLAYED-TABLE bank
&Scoped-Define DISPLAYED-OBJECTS account_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS bank.bank-code 
&Scoped-define DISPLAY-FIELD bank.actnum 

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
DEFINE VARIABLE account_dscr AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 6.6.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 2.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bank.bank-code AT ROW 1.71 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          FONT 4
     bank.bank-name AT ROW 1.71 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          FONT 4
     bank.addr[1] AT ROW 2.67 COL 44 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          FONT 4
     bank.addr[2] AT ROW 3.62 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          FONT 4
     bank.city AT ROW 4.57 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          FONT 4
     bank.state AT ROW 4.57 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          FONT 4
     bank.zip AT ROW 4.57 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          FONT 4
     bank.phone AT ROW 5.52 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          FONT 4
     bank.contact AT ROW 6.48 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          FONT 4
     bank.bk-act AT ROW 8.1 COL 44 COLON-ALIGNED FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FONT 4
     bank.actnum AT ROW 9.05 COL 44 COLON-ALIGNED
          LABEL "G/L Account #"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          FONT 4
     account_dscr AT ROW 9.05 COL 76 COLON-ALIGNED NO-LABEL
     bank.last-chk AT ROW 10.76 COL 44 COLON-ALIGNED
          LABEL "Last Check # Used"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
     bank.spare-int-1 AT ROW 10.76 COL 86 COLON-ALIGNED WIDGET-ID 2
          LABEL "ACH Check #" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
     bank.bal AT ROW 11.71 COL 44 COLON-ALIGNED
          LABEL "Account Balance"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4                 
    bank.pay-type AT ROW 11.71 COL 86 COLON-ALIGNED
          LABEL "Pay Type" FORMAT "x(25)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "PC-Payment Card","CC-Credit Card","DC-Debit Card","DD-Direct Deposit","BD-Direct Business Debit", "WT-Wire Transfer","EP-Electronic Payment","ET-Electronic Transfer","VC-Virtual Currency","BC-Bitcoin" 
          DROP-DOWN-LIST
          SIZE 32 BY 1 TOOLTIP "PC-Payment Card,CC-Credit Card,DC-Debit Card,DD-Direct Deposit,BD-Direct Business Debit,WT-Wire Transfer,EP-Electronic Payment,ET-Electronic Transfer,VC-Virtual Currency,BC-Bitcoin"
          BGCOLOR 15 
     
     bank.o-chk AT ROW 12.95 COL 44 COLON-ALIGNED
          LABEL "Outstanding Balance"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
    bank.RTN AT ROW 12.95 COL 86 COLON-ALIGNED
          LABEL "Routing#" FORMAT ">>>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
    
     bank.dep-tr AT ROW 13.91 COL 44 COLON-ALIGNED
          LABEL "Deposits in Transit"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
    bank.ODFI AT ROW 13.91 COL 86 COLON-ALIGNED
          LABEL "ODFI"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY 1
          FONT 4
     bank.serv AT ROW 14.86 COL 44 COLON-ALIGNED
          LABEL "Service charge"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
    bank.ACHID AT ROW 14.86 COL 86 COLON-ALIGNED
          LABEL "ACH Payment" FORMAT ">>>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
     bank.curr-code[1] AT ROW 15.95 COL 44 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
    bank.SwiftBIC AT ROW 15.95 COL 86 COLON-ALIGNED
          LABEL "Swift Code" FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
     bank.Bank-ID AT ROW 16.95 COL 86 COLON-ALIGNED
          LABEL "Bank ID" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
          FONT 4
     RECT-1 AT ROW 1.24 COL 3
     RECT-5 AT ROW 7.86 COL 3
     RECT-7 AT ROW 10.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.bank
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
         HEIGHT             = 17.86
         WIDTH              = 121.4.
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

/* SETTINGS FOR FILL-IN account_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN bank.actnum IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN bank.addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.bal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.bank-code IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN bank.Bank-ID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN bank.bk-act IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN bank.curr-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.dep-tr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.last-chk IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.o-chk IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.serv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.spare-int-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bank.SwiftBIC IN FRAME F-Main
   EXP-LABEL  EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bank.pay-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.RTN IN FRAME F-Main
   EXP-LABEL  EXP-FORMAT                                                */
/* SETTINGS FOR FILL-IN bank.ODFI IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank.ACHID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

&Scoped-define SELF-NAME bank.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank.actnum V-table-Win
ON HELP OF bank.actnum IN FRAME F-Main /* G/L Account # */
DO:
  lw-focus = FOCUS.

  RUN windows/l-acct.w (cocode,"",lw-focus:SCREEN-VALUE,OUTPUT char-val).

  IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
    lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-actnum (lw-focus).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank.actnum V-table-Win
ON LEAVE OF bank.actnum IN FRAME F-Main /* G/L Account # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank.actnum V-table-Win
ON VALUE-CHANGED OF bank.actnum IN FRAME F-Main /* G/L Account # */
DO:
  RUN new-actnum (FOCUS).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
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
  {src/adm/template/row-list.i "bank"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "bank"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/bank.i}

  RUN display-paytype.

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
 

  assign
   bank.pay-type   = substr(bank.pay-type,1,2) .
  

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
  
  RUN display-paytype.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR ll-ans AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-actnum (bank.actnum:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-actnum V-table-Win 
PROCEDURE new-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND account
        WHERE account.company EQ bank.company
          AND account.actnum  BEGINS ip-focus:SCREEN-VALUE
/*           AND account.type    EQ "A" */
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN account_dscr:SCREEN-VALUE = account.dscr.
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
  {src/adm/template/snd-list.i "bank"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum V-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF BUFFER b-bank FOR bank.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR lv-types AS CHAR INIT "ACELRT" NO-UNDO.
  DEF VAR lv-type-dscr AS CHAR INIT "Asset,Capital,Expense,Liability,Revenue,Total" NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" AND
       NOT CAN-FIND(FIRST account
                    WHERE account.company EQ cocode
                      AND account.actnum  EQ ip-focus:SCREEN-VALUE
                      /*AND account.type    EQ "A"*/) THEN
      lv-msg = "Invalid" +
/*             " " + TRIM(ENTRY(INDEX(lv-types,"A"),lv-type-dscr)) + */
               " account#, try help".

    IF lv-msg EQ "" AND
       CAN-FIND(FIRST b-bank
                WHERE b-bank.company EQ cocode
                  AND b-bank.actnum  EQ ip-focus:SCREEN-VALUE
                  AND ROWID(b-bank)  NE ROWID(bank)) THEN
      lv-msg = "Another Bank already uses this Account#, please try again".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-paytype V-table-Win 
PROCEDURE display-paytype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    if not avail bank then return.

    case bank.pay-type:
      when "PC" then bank.pay-type:screen-value in frame {&frame-name} = "PC-Payment Card".
      when "CC" then bank.pay-type:screen-value in frame {&frame-name} = "CC-Credit Card".
      when "DC" then bank.pay-type:screen-value in frame {&frame-name} = "DC-Debit Card".
      when "DD" then bank.pay-type:screen-value in frame {&frame-name} = "DD-Direct Deposit".     
      when "BD" then bank.pay-type:screen-value in frame {&frame-name} = "BD-Direct Business Debit".  
      when "WT" then bank.pay-type:screen-value in frame {&frame-name} = "WT-Wire Transfer".
      when "EP" then bank.pay-type:screen-value in frame {&frame-name} = "EP-Electronic Payment".
      when "ET" then bank.pay-type:screen-value in frame {&frame-name} = "ET-Electronic Transfer".
      when "VC" then bank.pay-type:screen-value in frame {&frame-name} = "VC-Virtual Currency".     
      when "BC" then bank.pay-type:screen-value in frame {&frame-name} = "BC-Bitcoin".  
    end case.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
