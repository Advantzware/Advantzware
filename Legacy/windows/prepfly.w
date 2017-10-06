&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: windows\prepfly.w
  
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
DEF INPUT-OUTPUT PARAM io-code1 LIKE prep.code NO-UNDO.
DEF INPUT        PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT        PARAM ip-type AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared}
ASSIGN
 cocode = g_company
 locode = g_loc.

DO TRANSACTION:
   {sys/inc/addprep.i}
   {sys/inc/prepdiegl.i}
   {sys/inc/prepdiebin.i}
   {sys/inc/prepplgl.i}
   {sys/inc/prepplbin.i}
END.

DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR ll-valid AS LOG INIT NO NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR lcNk1Value AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.
DEF VAR prepMaster-log AS LOG NO-UNDO.
DEFINE VARIABLE prepMaster-chr AS CHARACTER   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "prepMaster", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT lcNk1Value, OUTPUT llRecFound).
IF llRecFound THEN
prepMaster-log = LOGICAL(lcNk1Value) NO-ERROR.
RUN sys/ref/nk1look.p (INPUT cocode, "prepMaster", "C" /* Char */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT lcNk1Value, OUTPUT llRecFound).
IF llRecFound THEN
prepMaster-chr = lcNk1Value NO-ERROR.

{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES prep
&Scoped-define FIRST-EXTERNAL-TABLE prep


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prep.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDefaultPrep D-Dialog 
FUNCTION getDefaultPrep RETURNS CHARACTER
  (ipcMatType AS CHAR  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_prep AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     Btn_OK AT ROW 19.33 COL 37
     Btn_Cancel AT ROW 19.33 COL 78
     SPACE(38.99) SKIP(0.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Add New Prep Code"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   External Tables: asi.prep
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
   FRAME-NAME                                                           */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Add New Prep Code */
DO:
  APPLY "choose" TO Btn_Cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-target':U,OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("cancel-record").
    io-code1 = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-target':U,OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("update-record").
    IF NOT ll-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FIND FIRST prep
    WHERE prep.company EQ cocode
    NO-LOCK NO-ERROR.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:
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
             INPUT  'viewers/prep.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_prep ).
       RUN set-position IN h_prep ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 131.00 ) */

       /* Links to SmartViewer h_prep. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_prep ).
       RUN add-link IN adm-broker-hdl ( h_prep , 'prepfly':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_prep ,
             Btn_OK:HANDLE , 'BEFORE':U ).
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
  {src/adm/template/row-list.i "prep"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prep"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Create D-Dialog 
PROCEDURE Allow-Create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.

  op-flag = YES.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Delete D-Dialog 
PROCEDURE Allow-Delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.

  op-flag = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Update D-Dialog 
PROCEDURE Allow-Update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.

  op-flag = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation D-Dialog 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation D-Dialog 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  ENABLE Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-defaults D-Dialog 
PROCEDURE get-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid1 AS ROWID NO-UNDO.
  DEFINE VARIABLE cDefaultFromPrep AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMatTypeSearch AS CHARACTER NO-UNDO.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-prep FOR prep.
  DEF BUFFER bf-def-prep FOR prep.

  DEF VAR ll-corr AS LOG NO-UNDO.

  FIND prep WHERE ROWID(prep) EQ ip-rowid1 NO-ERROR.

  IF AVAIL prep THEN DO:
    ASSIGN
     prep.code        = io-code1
     prep.mat-type    = IF ip-type EQ 1 THEN "D" ELSE "P"
     prep.cust-no     = eb.cust-no
     prep.box-style   = eb.style
     prep.last-est-no = eb.est-no
     ll-corr   = AVAIL prep                   AND
                  TRIM(prep.last-est-no) NE "" AND
                  CAN-FIND(FIRST est
                            WHERE est.company  EQ prep.company
                              AND est.est-no   EQ prep.last-est-no
                              AND est.est-type GT 4)
     prep.carton-w    = eb.wid
     prep.carton-l    = eb.len
     prep.carton-d    = eb.dep.

    IF ll-corr THEN
       ASSIGN
          prep.carton-w = {sys/inc/k16.i prep.carton-w}
          prep.carton-l = {sys/inc/k16.i prep.carton-l}
          prep.carton-d = {sys/inc/k16.i prep.carton-d}.

    FOR EACH b-eb FIELDS(num-up)
        WHERE b-eb.company EQ ef.company
          AND b-eb.est-no  EQ ef.est-no
          AND b-eb.form-no EQ ef.form-no
        NO-LOCK:
      prep.number-up = prep.number-up + b-eb.num-up.
    END.
   
    IF prep.cust-no NE "" THEN
       FOR EACH cust FIELDS(cust-no NAME)
           WHERE cust.company EQ prep.company
             AND cust.cust-no  EQ prep.cust-no
           NO-LOCK:
         prep.cust-name = cust.name. 
         ASSIGN
             prep.owner[1]   = cust.cust-no
             prep.owner-%[1] = 100.
         LEAVE.
       END.

    /*FOR EACH cust FIELDS(cust-no rec_key)
        WHERE cust.company EQ prep.company
          AND cust.active  EQ "X"
        NO-LOCK
        BY cust.rec_key:
      ASSIGN
       prep.owner[1]   = cust.cust-no
       prep.owner-%[1] = 100.
      LEAVE.
    END.*/ /* ticket 23653 */
    
    RUN  est/calcMatType.p (INPUT io-code1, OUTPUT cMatTypeSearch).
    
    cDefaultFromPrep = "".
    IF prepMaster-log THEN
      cDefaultFromPrep = getDefaultPrep(cMatTypeSearch).   
    IF cDefaultFromPrep GT "" THEN DO:
        FIND bf-def-prep WHERE bf-def-prep.company EQ prep.company
          AND bf-def-prep.code EQ cDefaultFromPrep
          NO-LOCK NO-ERROR.
        IF NOT AVAIL bf-def-prep THEN 
         cDefaultFromPrep = "".
    END. 
      

    FOR EACH b-prep 
        WHERE b-prep.company  EQ prep.company
          AND (b-prep.mat-type EQ prep.mat-type OR 
                cDefaultFromPrep GT "")
          AND (IF cDefaultFromPrep GT "" THEN 
                b-prep.code     EQ cDefaultFromPrep
              ELSE 
                TRUE)
        NO-LOCK
        BY b-prep.rec_key:
      ASSIGN
       prep.cost      = b-prep.cost
       prep.ml        = b-prep.ml
       prep.amtz      = b-prep.amtz
       prep.cost-type = b-prep.cost-type
       prep.fgcat     = b-prep.fgcat
       prep.loc       = b-prep.loc
       prep.simon     = b-prep.simon
       prep.mkup      = b-prep.mkup
       /*prep.owner[1]     = b-prep.owner[1]
       prep.owner-%[1]   = b-prep.owner-%[1]
       prep.owner[2]     = b-prep.owner[2]
       prep.owner-%[2]   = b-prep.owner-%[2]*/ /* ticket 23653 */
       .  
       
      IF cDefaultFromPrep GT "" THEN DO:
          ASSIGN prep.actnum = b-prep.actnum.
                 prep.loc-bin = b-prep.loc-bin.
      END.
      ELSE DO:
          IF prep.mat-type EQ "D" AND prepdiegl-log THEN
             prep.actnum = prepdiegl-chr.
          ELSE IF prep.mat-type EQ "P" AND prepplgl-log THEN
             prep.actnum = prepplgl-chr.
          ELSE
             prep.actnum = b-prep.actnum.
    
          IF prep.mat-type EQ "D" AND prepdiebin-log THEN
             prep.loc-bin = prepdiebin-chr.
          ELSE IF prep.mat-type EQ "P" AND prepplbin-log THEN
             prep.loc-bin = prepplbin-chr.
          ELSE
             prep.loc-bin = b-prep.loc-bin.
      END.
      LEAVE.
    END.

    IF addprep-int EQ 1 THEN
    DO:
       IF prep.mat-type EQ "P" THEN
          prep.dscr = "Printing Plate".
       ELSE
          IF LOOKUP(prep.mat-type,"D,R,F") GT 0 THEN
              prep.dscr = "Cutting Die".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-target':U,OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN prepfly IN WIDGET-HANDLE(char-hdl).

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
  {src/adm/template/snd-list.i "prep"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-code D-Dialog 
PROCEDURE set-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM io-code2 LIKE io-code1 NO-UNDO.

  ASSIGN
   io-code1 = io-code2
   ll-valid = YES.

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


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDefaultPrep D-Dialog 
FUNCTION getDefaultPrep RETURNS CHARACTER
  (ipcMatType AS CHAR  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE result AS CHARACTER NO-UNDO.
                DEF VAR i AS INT NO-UNDO.
                DEF VAR j AS INT NO-UNDO.
                DEF VAR cListEntry AS CHAR NO-UNDO.
                DEF VAR cExtractedType AS CHAR NO-UNDO.
                DEF VAR cExtractedPrepCode AS CHAR NO-UNDO.
                DO i = 1 TO NUM-ENTRIES(prepMaster-chr):
                    cListEntry = ENTRY(i, prepMaster-chr).
                    j = INDEX(cListEntry, "=").
                    IF j GT 0 THEN 
                      ASSIGN cExtractedType = TRIM(SUBSTRING(cListEntry, 1, j - 1))
                             cExtractedPrepCode = TRIM(SUBSTRING(cListEntry, j + 1)). 
                    IF cExtractedType = ipcMatType THEN DO:
                        RESULT = cExtractedPrepCode.
                        LEAVE.
                    END.
        END.
                RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


