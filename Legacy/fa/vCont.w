&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"fa/sdocont.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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


&Scoped-define FRAME-NAME F-Main
{pt/probase1.i}
{dev/method/mxpbrokr.i}


        find  login where terminal-no eq terminalid
                     and user-id eq entry(1,userid("ptdb":U),"@") and system eq "fa" NO-LOCK NO-ERROR.
        IF AVAIL login THEN find  entity of  login NO-LOCK NO-ERROR.
        IF AVAIL entity THEN find fa-control of  entity NO-LOCK NO-ERROR.
        IF AVAIL fa-control THEN DO:
            v-fa-entity =  login.entity-code.
            logincontrol = fa-control.fa-entity.
            if not logged then do:
                display "You are logged off!  Cannot execute" functdesc
                   with {&UGUI-SCR} frame std_1 no-label.
                undo, leave.
            end.
        systemname = entity-name.
        END.


DEF VAR add-new AS LOG NO-UNDO.
/* CURRENT-WINDOW:TITLE = "fa-control Maintenance". */

{pt/setinit use-file 'fa-control'}
define variable do-copy   as   logical.
define new shared variable fa-control-rowid as rowid.
DEF VAR cfa-entity LIKE fa-control.fa-entity.
DEF BUFFER fa-control-d FOR fa-control.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "fa/sdocont.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.FA-entity RowObject.Yr ~
RowObject.Prd RowObject.Number-prd RowObject.calc-mode ~
RowObject.aqui-service RowObject.tag-update RowObject.fa-gl-disp ~
RowObject.fa-gl-clear RowObject.Currency-cod 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.FA-entity RowObject.Entity-name ~
RowObject.Yr RowObject.gl-installed RowObject.Prd RowObject.Job-no ~
RowObject.Number-prd RowObject.last-ad RowObject.calc-mode ~
RowObject.last-prd RowObject.aqui-service RowObject.tag-update ~
RowObject.fa-gl-disp RowObject.fa-gl-clear RowObject.Currency-cod 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS g1-desc g2-desc 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE g1-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE g2-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.FA-entity AT ROW 1 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     RowObject.Entity-name AT ROW 1 COL 41 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 41.57 BY 1
     RowObject.Yr AT ROW 2.25 COL 19 COLON-ALIGNED
          LABEL "Current Year"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.gl-installed AT ROW 2.25 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.Prd AT ROW 3.5 COL 19 COLON-ALIGNED
          LABEL "Current Period"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
     RowObject.Job-no AT ROW 3.5 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     RowObject.Number-prd AT ROW 4.75 COL 19 COLON-ALIGNED
          LABEL "Nbr Periods"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
     RowObject.last-ad AT ROW 4.75 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
     RowObject.calc-mode AT ROW 6 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.last-prd AT ROW 6 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
     RowObject.aqui-service AT ROW 7.25 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     RowObject.tag-update AT ROW 8.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.fa-gl-disp AT ROW 9.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     g1-desc AT ROW 9.75 COL 33 COLON-ALIGNED NO-LABEL
     RowObject.fa-gl-clear AT ROW 11 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     g2-desc AT ROW 11 COL 33 COLON-ALIGNED NO-LABEL
     RowObject.Currency-cod AT ROW 12.25 COL 19 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 112.43 BY 16.33
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "fa/sdocont.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {fa/sdocont.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 16.33
         WIDTH              = 112.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Currency-cod IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Entity-name IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN g1-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN g2-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.gl-installed IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.gl-installed:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Job-no IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Job-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.last-ad IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.last-ad:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.last-prd IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.last-prd:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Number-prd IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Prd IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Yr IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Currency-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Currency-cod vTableWin
ON ENTRY OF RowObject.Currency-cod IN FRAME F-Main /* Currency Code */
DO:
    {pt/setinit br-file 'currency'}. 
    {pt/setinit br-field 'currency-cod'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Currency-cod vTableWin
ON LEAVE OF RowObject.Currency-cod IN FRAME F-Main /* Currency Code */
DO:
    IF LASTKEY <> 306 THEN DO: 
        FIND currency WHERE
            currency.currency-cod = rowobject.currency-cod:{&SV} NO-LOCK NO-ERROR.
        IF NOT AVAIL currency THEN DO:
            MESSAGE "Invalid currency"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.currency-cod.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.FA-entity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.FA-entity vTableWin
ON ENTRY OF RowObject.FA-entity IN FRAME F-Main /* FA Entity */
DO:
    {pt/setinit br-file 'entity'}. 
    {pt/setinit br-field 'entity-code'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.FA-entity vTableWin
ON LEAVE OF RowObject.FA-entity IN FRAME F-Main /* FA Entity */
DO:
    IF LASTKEY <> -1 AND LASTKEY <> 306 THEN DO:
        FIND  entity where 
             entity.entity-code = rowobject.fa-entity:{&SV} no-lock no-error.
        IF NOT AVAIL  entity THEN DO:
            MESSAGE "Entity not on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.fa-entity.
            RETURN NO-APPLY.
        END.
    
        FIND fa-control-d where 
            fa-control-d.fa-entity = rowobject.fa-entity:{&SV} NO-LOCK no-error.
        IF AVAIL fa-control-d THEN DO:
            MESSAGE "FA Control record is already on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.fa-entity.
            RETURN NO-APPLY.
        END.
        ASSIGN rowobject.entity-name:{&SV} =  entity.name.
     END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.fa-gl-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.fa-gl-clear vTableWin
ON ENTRY OF RowObject.fa-gl-clear IN FRAME F-Main /* FA Clearing */
DO:
    {pt/setinit br-file 'gl-account'}. 
    {pt/setinit br-field 'gl-code'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.fa-gl-clear vTableWin
ON LEAVE OF RowObject.fa-gl-clear IN FRAME F-Main /* FA Clearing */
DO:
    IF LASTKEY <> 306 THEN DO:
        FIND gl-account WHERE 
            gl-account.gl-code = rowobject.fa-gl-clear:{&SV} no-lock no-error.
        IF NOT available gl-account THEN do:
            MESSAGE "Account not on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO rowobject.fa-gl-clear.
            RETURN NO-APPLY.
        END.
        ASSIGN g2-desc:{&SV} = gl-account.DESCRIPTION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.fa-gl-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.fa-gl-disp vTableWin
ON ENTRY OF RowObject.fa-gl-disp IN FRAME F-Main /* FA Disposal */
DO:
    {pt/setinit br-file 'gl-account'}. 
    {pt/setinit br-field 'gl-code'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.fa-gl-disp vTableWin
ON LEAVE OF RowObject.fa-gl-disp IN FRAME F-Main /* FA Disposal */
DO:
    IF LASTKEY <> 306 THEN DO:
        FIND gl-account WHERE 
            gl-account.gl-code = rowobject.fa-gl-disp:{&SV} no-lock no-error.
        if NOT available gl-account THEN do:
            MESSAGE "Account not on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO rowobject.fa-gl-disp.
            RETURN NO-APPLY.
        END.
        ASSIGN g1-desc:{&SV} = gl-account.DESCRIPTION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Yr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Yr vTableWin
ON LEAVE OF RowObject.Yr IN FRAME F-Main /* Current Year */
DO:
    IF INT(rowobject.yr:{&SV}) < {val\centbeg} OR 
        INT(rowobject.yr:{&SV}) > {val\centend} THEN DO:
        MESSAGE "Invalid year"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO rowobject.yr.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ENABLE rowobject.fa-entity 
         rowobject.currency-cod WITH FRAME {&FRAME-NAME}.

  RUN SUPER.
  ASSIGN g1-desc:{&SV} =  ""
     g2-desc:{&SV} =  "".

  add-new = YES.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  add-new = NO.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ENABLE rowobject.fa-entity
         rowobject.currency-cod WITH FRAME {&FRAME-NAME}.

  RUN SUPER.

  add-new = YES.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  do-delete = no.
  message "Are you sure you want to delete this record?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      update do-delete.
  if not do-delete then  return no-apply .
  deleteok = yes.


  FIND fa-control WHERE
    fa-control.fa-entity = rowobject.fa-entity:{&SV} NO-LOCK NO-ERROR.

  ASSIGN fa-control-rowid = rowid(fa-control).

 
        /* TEST IF RECORD IS REFERENCED ELSEWERE */

  run value("pt/hypdel.p":U) (input "fa-control":U,
                              input rowid(fa-control),
                              input no,
                              input-output deleteok,
                              input yes).

  if deleteok then do:
      run messageOnly IN hMessage ("fa0016":U,  string(fa-control.fa-entity) +  "~001":U ).
            /* Delete UDF value */
      run value("pt/deluval.p":U) ("fa-control":U).
      RUN SUPER.
  END.
  view frame {&frame-name}.

 /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord vTableWin 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cfa-entity LIKE fa-control.fa-entity.
DEF BUFFER z_numgen FOR  z_numgen.

    /* Code placed here will execute PRIOR to standard behavior. */
    IF add-new THEN do:
        ENABLE rowobject.fa-entity
               rowobject.currency-cod WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DO:
        DISABLE rowobject.fa-entity
                rowobject.currency-cod WITH FRAME {&FRAME-NAME}.
    END.

    RUN SUPER.
    ASSIGN cfa-entity = rowobject.fa-entity:{&SV}.

    {pt/setinit fa-entity cfa-entity}.
   ASSIGN   wkeylist = 
        rowobject.fa-entity:{&SV} + "," 
      wfilename = "fa-control":U.


    FIND fa-control WHERE
        fa-control.fa-entity = rowobject.fa-entity:{&SV} NO-LOCK NO-ERROR.

    ASSIGN fa-control-rowid = rowid(fa-control).

    FIND gl-account WHERE 
         gl-account.gl-code = rowobject.fa-gl-disp:{&SV} no-lock no-error.
    ASSIGN g1-desc:{&SV} =  IF AVAIL gl-account THEN  gl-account.DESCRIPTION ELSE ?.
 
    FIND gl-account WHERE 
         gl-account.gl-code = rowobject.fa-gl-clear:{&SV} no-lock no-error.
    ASSIGN g2-desc:{&SV} =  IF AVAIL gl-account THEN  gl-account.DESCRIPTION ELSE ?.


    do for z_numgen transaction:
        {fa/contedt3}.   /* GET NEXT SEQUENCE FOR AUDIT */
    END.

  /* Code placed here will execute PRIOR to standard behavior. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /*
  ASSIGN wmemo-function = IF PROGRAM-NAME(2) BEGINS "pt/ddialog.w" 
      THEN "Inquiry" ELSE "Maintenance":U.
*/


  RUN SUPER.
  DISABLE rowobject.fa-entity 
          rowobject.currency-cod WITH FRAME {&FRAME-NAME}.

  

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord vTableWin 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO PRINTER.
    VIEW FRAME {&FRAME-NAME}.

    OUTPUT CLOSE.
    message "The help window has now been listed to the system printer".
    hide message.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER z_audit FOR  z_audit.
  /* Code placed here will execute PRIOR to standard behavior. */
  FIND  entity where 
         entity.entity-code = rowobject.fa-entity:{&SV} no-lock no-error.
  IF NOT AVAIL  entity THEN DO:
      MESSAGE "Entity not on file"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO rowobject.fa-entity.
      RETURN NO-APPLY.
  END.
  IF INT(rowobject.yr:{&SV}) < {val\centbeg} OR 
      INT(rowobject.yr:{&SV}) > {val\centend} THEN DO:
      MESSAGE "Invalid year"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO rowobject.yr.
      RETURN NO-APPLY.
  END.
  FIND gl-account WHERE 
      gl-account.gl-code = rowobject.fa-gl-disp:{&SV} no-lock no-error.
  if NOT available gl-account THEN do:
      MESSAGE "Account not on file"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO rowobject.fa-gl-disp.
      RETURN NO-APPLY.
  END.
  FIND gl-account WHERE 
       gl-account.gl-code = rowobject.fa-gl-clear:{&SV} no-lock no-error.
  IF NOT available gl-account THEN do:
      MESSAGE "Account not on file"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO rowobject.fa-gl-clear.
      RETURN NO-APPLY.
  END.
  FIND currency WHERE
      currency.currency-cod = rowobject.currency-cod:{&SV} NO-LOCK NO-ERROR.
  IF NOT AVAIL currency THEN DO:
      MESSAGE "Invalid currency"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO rowobject.currency-cod.
      RETURN NO-APPLY.
  END.

  FIND fa-control WHERE
       fa-control.fa-entity = rowobject.fa-entity:{&SV} NO-LOCK NO-ERROR.


  IF add-new THEN DO: 
      ENABLE rowobject.fa-entity 
             rowobject.currency-cod WITH FRAME {&FRAME-NAME}.
  END.
  ELSE do:
      DISABLE rowobject.fa-entity 
              rowobject.currency-cod WITH FRAME {&FRAME-NAME}.
  END.

  if NOT add-new  AND AVAIL uom then do:
      if wdo-audit then do for z_audit TRANSACTION:
          {fa/contedt4}.
      END.

      waction = "CHANGE:NEW":U.
      waction1 = "Modifying an existing G/L Mast record".
   END.     /* OF NOT NEW RECORD */
   else waction1 =  "Creating new G/L Mast record.".
   RUN SUPER.

   FIND fa-control WHERE
        fa-control.fa-entity = rowobject.fa-entity:{&SV} NO-LOCK NO-ERROR.

    /* Update UDF value */
      if search("udf/fa-contr.p":U)<>? or search("udf/fa-contr.r":U)<>?
         then run value("udf/fa-contr.p":U) ("update":U).
      if wdo-audit then do for  z_audit TRANSACTION:
          {fa/contedt4 "+ 0.5"}.   /* CREATE AUDIT "NEW" */
          
      end. /* OF AUDIT CREATION */
    add-new = NO.
  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

