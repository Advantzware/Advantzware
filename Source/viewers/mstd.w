&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/mstd.w

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
&Scoped-define EXTERNAL-TABLES mstd
&Scoped-define FIRST-EXTERNAL-TABLE mstd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mstd.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mstd.m-code mstd.style mstd.mr-x mstd.mr-y ~
mstd.rs-x mstd.rs-y mstd.sp-x mstd.sp-y 
&Scoped-define ENABLED-TABLES mstd
&Scoped-define FIRST-ENABLED-TABLE mstd
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS mstd.m-code mstd.dept mstd.style mstd.mr-x ~
mstd.mx-dscr[1] mstd.mr-y mstd.mx-dscr[2] mstd.rs-x mstd.mx-dscr[3] ~
mstd.rs-y mstd.mx-dscr[4] mstd.sp-x mstd.sp-y 
&Scoped-define DISPLAYED-TABLES mstd
&Scoped-define FIRST-DISPLAYED-TABLE mstd
&Scoped-Define DISPLAYED-OBJECTS mcode_dscr dept_dscr style_dscr sp-dscr1 ~
sp-dscr2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS mstd.m-code mstd.dept 

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
DEFINE VARIABLE dept_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE mcode_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sp-dscr1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE sp-dscr2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 8.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mstd.m-code AT ROW 1.48 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.2 BY 1
     mcode_dscr AT ROW 1.48 COL 28 COLON-ALIGNED NO-LABEL
     mstd.dept AT ROW 2.43 COL 16 COLON-ALIGNED
          LABEL "Dept"
          VIEW-AS FILL-IN 
          SIZE 12.2 BY 1
     dept_dscr AT ROW 2.43 COL 28 COLON-ALIGNED NO-LABEL
     mstd.style AT ROW 3.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.2 BY 1
     style_dscr AT ROW 3.38 COL 28 COLON-ALIGNED NO-LABEL
     mstd.mr-x AT ROW 6 COL 17 COLON-ALIGNED
          LABEL "Make Ready" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mstd.mx-dscr[1] AT ROW 6 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     mstd.mr-y AT ROW 6 COL 57 COLON-ALIGNED NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mstd.mx-dscr[2] AT ROW 6 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     mstd.rs-x AT ROW 6.95 COL 17 COLON-ALIGNED
          LABEL "Run Speed" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mstd.mx-dscr[3] AT ROW 6.95 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     mstd.rs-y AT ROW 6.95 COL 57 COLON-ALIGNED NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mstd.mx-dscr[4] AT ROW 6.95 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     mstd.sp-x AT ROW 7.91 COL 17 COLON-ALIGNED
          LABEL "Run Spoil" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     sp-dscr1 AT ROW 7.91 COL 22 COLON-ALIGNED NO-LABEL
     mstd.sp-y AT ROW 7.91 COL 57 COLON-ALIGNED NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     sp-dscr2 AT ROW 7.91 COL 62 COLON-ALIGNED NO-LABEL
     "Standard" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 4.57 COL 9
     "Y  Axis  (Rows)" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 5.29 COL 65
     "X  Axis  (Columns)" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 21
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 4.81 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.mstd
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
         HEIGHT             = 17.14
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

/* SETTINGS FOR FILL-IN mstd.dept IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN dept_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.m-code IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN mcode_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.mr-x IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mstd.mr-y IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mstd.mx-dscr[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.mx-dscr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.mx-dscr[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.mx-dscr[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.rs-x IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mstd.rs-y IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN sp-dscr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sp-dscr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mstd.sp-x IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mstd.sp-y IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var ls-cur-val as cha no-undo.
   def var char-val as cha no-undo.
   def var ls-industry as cha no-undo.
   DEF VAR lv-focus AS WIDGET NO-UNDO.


   lv-focus = FOCUS.

   case focus:name :
      when "m-code" then do:
           ls-cur-val = lv-focus:screen-value.
           run windows/l-mach.w (gcompany, gloc, ls-cur-val, output char-val).
           if char-val <> "" then do:
             lv-focus:screen-value = entry(1,char-val).
             APPLY "value-changed" TO lv-focus.  
           END.
      end.
      when "style" then do:
           ls-cur-val = lv-focus:screen-value.
           ls-industry = "".
           run windows/l-style.w (gcompany, ls-cur-val, output char-val).
           if char-val <> "" then do:
              assign lv-focus:screen-value =  entry(1,char-val)
                     style_dscr:screen-value = entry(2,char-val)
                       .
           end. 
      end.
      when "mr-x" or when "mr-y" or when "rs-x" or when "rs-y" or when "sp-x"
           or when "sp-y"
      then do:
           ls-cur-val = lv-focus:screen-value.
           run windows/l-stdcd.w (gcompany, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lv-focus:screen-value =  entry(1,char-val).
              case lv-focus:name:
                  when "mr-x" then mstd.mx-dscr[1]:screen-value = entry(2,char-val).
                  when "mr-y" then mstd.mx-dscr[2]:screen-value = entry(2,char-val).
                  when "rs-x" then mstd.mx-dscr[3]:screen-value = entry(2,char-val).
                  when "rs-y" then mstd.mx-dscr[4]:screen-value = entry(2,char-val).
                  when "sp-x" then sp-dscr1:screen-value = entry(2,char-val).
                  when "sp-y" then sp-dscr2:screen-value = entry(2,char-val).                  
              end case.
           end.  
      end.
  end case.

  APPLY "entry" TO lv-focus.
 return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.m-code V-table-Win
ON ENTRY OF mstd.m-code IN FRAME F-Main /* Code */
DO:
  IF NOT adm-new-record OR adm-adding-record THEN DO:
    APPLY "leave" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.m-code V-table-Win
ON LEAVE OF mstd.m-code IN FRAME F-Main /* Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.m-code V-table-Win
ON VALUE-CHANGED OF mstd.m-code IN FRAME F-Main /* Code */
DO:
  DEF BUFFER bf-mach FOR mach.

  FIND bf-mach
      WHERE bf-mach.company EQ gcompany
        AND bf-mach.loc     EQ gloc
        AND bf-mach.m-code  BEGINS mstd.m-code:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL bf-mach THEN
    ASSIGN
     mstd.m-code:SCREEN-VALUE = bf-mach.m-code
     mstd.dept:SCREEN-VALUE   = bf-mach.dept[1]
     mcode_dscr:SCREEN-VALUE  = bf-mach.m-dscr.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.mr-x
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.mr-x V-table-Win
ON LEAVE OF mstd.mr-x IN FRAME F-Main /* Make Ready */
DO:
     {&methods/lValidateError.i YES}
      find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code  and lastkey <> -1 and self:screen-value <> "00" 
      then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      mstd.mx-dscr[1]:screen-value = if avail std-code then std-code.dscr else "".
      {&methods/lValidateError.i NO}  
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.mr-y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.mr-y V-table-Win
ON LEAVE OF mstd.mr-y IN FRAME F-Main /* Y Axis */
DO:
    {&methods/lValidateError.i YES}
        find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code and lastkey <> -1 and self:screen-value <> "00" 
 then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      mstd.mx-dscr[2]:screen-value = if avail std-code then std-code.dscr else "".  
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.rs-x
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.rs-x V-table-Win
ON LEAVE OF mstd.rs-x IN FRAME F-Main /* Run Speed */
DO:
      {&methods/lValidateError.i YES}
      find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code and lastkey <> -1 and self:screen-value <> "00" 
then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      mstd.mx-dscr[3]:screen-value = if avail std-code then std-code.dscr else "".

      RUN check-for-special-codes.  
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.rs-y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.rs-y V-table-Win
ON LEAVE OF mstd.rs-y IN FRAME F-Main /* Y Axis */
DO:
     {&methods/lValidateError.i YES}
        find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code and lastkey <> -1 and self:screen-value <> "00" 
then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      mstd.mx-dscr[4]:screen-value = if avail std-code then std-code.dscr else "".  

      RUN check-for-special-codes. 
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.sp-x
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.sp-x V-table-Win
ON LEAVE OF mstd.sp-x IN FRAME F-Main /* Run Spoil */
DO:
     {&methods/lValidateError.i YES}
      find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code and lastkey <> -1 and self:screen-value <> "00" 
then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      sp-dscr1:screen-value = if avail std-code then std-code.dscr else "".  
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.sp-y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.sp-y V-table-Win
ON LEAVE OF mstd.sp-y IN FRAME F-Main /* Y axis */
DO:
   {&methods/lValidateError.i YES}
   find first std-code where std-code.code = self:screen-value no-lock no-error.
      if not avail std-code and lastkey <> -1 and self:screen-value <> "00" 
then do:
         message "Invalid code. Try help." view-as alert-box error.
         return no-apply.
      end.
      sp-dscr2:screen-value = if avail std-code then std-code.dscr else "".  
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mstd.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.style V-table-Win
ON LEAVE OF mstd.style IN FRAME F-Main /* Style Code */
DO:
  {&methods/lValidateError.i YES}
  IF LASTKEY NE -1 AND {&self-name}:SCREEN-VALUE NE "" THEN DO:
    IF NOT CAN-FIND(FIRST style WHERE style.company EQ gcompany
                                  AND style.style   EQ {&self-name}:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
  END.  
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mstd.style V-table-Win
ON VALUE-CHANGED OF mstd.style IN FRAME F-Main /* Style Code */
DO:
  FIND style
      WHERE style.company EQ gcompany
        AND style.style   EQ {&self-name}:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL style THEN
    ASSIGN
     mstd.style:SCREEN-VALUE = style.style
     style_dscr:SCREEN-VALUE = style.dscr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
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
  {src/adm/template/row-list.i "mstd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mstd"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-special-codes V-table-Win 
PROCEDURE check-for-special-codes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF mstd.rs-x:SCREEN-VALUE EQ "98" THEN
      ASSIGN
       mstd.rs-y:SCREEN-VALUE       = mstd.rs-x:SCREEN-VALUE
       mstd.mx-dscr[4]:SCREEN-VALUE = mstd.mx-dscr[3]:SCREEN-VALUE.
    ELSE
    IF mstd.rs-y:SCREEN-VALUE EQ "98" THEN
      ASSIGN
       mstd.rs-x:SCREEN-VALUE       = mstd.rs-y:SCREEN-VALUE
       mstd.mx-dscr[3]:SCREEN-VALUE = mstd.mx-dscr[4]:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-matrix V-table-Win 
PROCEDURE copy-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param ip-m-code like mstd.m-code no-undo.
def input param ip-dept like mstd.dept no-undo.
def input param ip-style like mstd.style no-undo.

def buffer bf-mmtx for mmtx.
def buffer bf-mmtx2 for mmtx2.
def buffer bf-mmty for mmty.
DEF BUFFER bf-mstd FOR mstd.

DEF VAR i AS INT NO-UNDO.
def var li-next-mmtx-no as INT NO-UNDO.

for each mmtx
    where mmtx.company eq mstd.company
      and mmtx.loc     eq mstd.loc
      and mmtx.m-code  eq ip-m-code
      and mmtx.dept    eq ip-dept
      and mmtx.style   eq ip-style
    no-lock:

  find last bf-mmtx use-index mmtx-no no-lock no-error.
  li-next-mmtx-no = (if avail bf-mmtx then bf-mmtx.mmtx-no else 0) + 1.

  create bf-mmtx.
  buffer-copy mmtx except mmtx.m-code mmtx.dept mmtx.style mmtx.mmtx-no to bf-mmtx
  assign 
   bf-mmtx.m-code  = mstd.m-code
   bf-mmtx.dept    = mstd.dept
   bf-mmtx.style   = mstd.style
   bf-mmtx.mmtx-no = li-next-mmtx-no.

  find first mmtx2 of mmtx no-lock no-error.

  if avail mmtx2 then do:
    create bf-mmtx2.
    buffer-copy mmtx2 except mmtx2.mmtx-no mmtx2.m-code to bf-mmtx2
    assign 
     bf-mmtx2.mmtx-no = li-next-mmtx-no
     bf-mmtx2.m-code  = mstd.m-code.
  end.
end.

for each mmty
    where mmty.company eq mstd.company
      and mmty.loc     eq mstd.loc
      and mmty.m-code  eq ip-m-code
      and mmty.dept    eq ip-dept
      and mmty.style   eq ip-style 
    no-lock:

  create bf-mmty.
  buffer-copy mmty except mmty.m-code mmty.dept mmty.style to bf-mmty
  assign 
   bf-mmty.m-code = mstd.m-code
   bf-mmty.dept   = mstd.dept
   bf-mmty.style  = mstd.style.
end.


FIND bf-mstd where bf-mstd.company eq mstd.company
               and bf-mstd.loc     eq mstd.loc
               and bf-mstd.m-code  eq ip-m-code
               and bf-mstd.dept    eq ip-dept
               and bf-mstd.style   eq ip-style NO-LOCK NO-ERROR.
IF AVAIL bf-mstd THEN DO:
   DO i = 1 TO 9:
      ASSIGN mstd.run-qty[i] = bf-mstd.run-qty[i]
             mstd.x-sheets[i] = bf-mstd.x-sheets[i]
             mstd.board-cal[i] = bf-mstd.board-cal[i]
             mstd.spd-reduc[i] = bf-mstd.spd-reduc[i].             
   END.
   DO i = 1 TO 15:
      ASSIGN mstd.board-depth[i] = bf-mstd.board-depth[i]
             mstd.depth-reduc[i] = bf-mstd.depth-reduc[i].
   END.


END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-matrix V-table-Win 
PROCEDURE create-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var tmpstore as cha no-undo.
  def var i as int no-undo.

  /* ====== create setup ================*/
  find first mmty of mstd no-lock no-error.
  if not avail mmty then do:
     create mmty.
     assign mmty.company = mstd.company
            mmty.loc = mstd.loc
            mmty.m-code = mstd.m-code
            mmty.dept = mstd.dept
            mmty.style = mstd.style
            .
     tmpstore = "".
     if mstd.mr-x <> 0 then do:
        find std-code where std-code.code = string(mr-x,"99") no-lock no-error.
        mmty.c-title = caps(std-code.dscr).
        do i = 1 to length(mmty.c-title):
           tmpstore = tmpstore + substring(mmty.c-title,i,1) + " ".
        end.
        mmty.c-title = tmpstore.
     end.
     if mr-y ne 0 then do:
        find std-code where std-code.code = string(mr-y,"99") no-lock no-error.
        mmty.r-title = caps(std-code.dscr).
        tmpstore = "".
        do i = 1 to 15:
           mmty.rtit[i] = substring(mmty.r-title,i,1).
        end.
     end.
  end.  /* not avail mmty */
    /* ====== create run ================*/
  find first mmtx of mstd where not mmtx.mr-run no-lock no-error.
  if not avail mmtx then do:
     tmpstore = "".
     for each mmtx by mmtx-no descending:
         i = mmtx-no.
         leave.
     end.
     create mmtx.
     assign  mmtx.mmtx-no = i + 1
             mmtx.company = mstd.company
             mmtx.loc     = mstd.loc
             mmtx.m-code  = mstd.m-code
             mmtx.dept    = mstd.dept
             mmtx.style   = mstd.style
             mmtx.mr-run  = no.    /* yes = spoil Mtx *** no = run mtx */

     if rs-x ne 0 then do:
        find std-code where std-code.code = string(rs-x,"99") no-lock no-error.
        mmtx.c-title = caps(std-code.dscr).
        do i = 1 to length(mmtx.c-title):
           tmpstore = tmpstore + substring(mmtx.c-title,i,1) + " ".
        end.
        mmtx.c-title = tmpstore.
     end.
     if rs-y ne 0 then do:
        find std-code where std-code.code = string(rs-y,"99") no-lock no-error.
        mmtx.r-title = caps(std-code.dscr).
        tmpstore = "".
        do i = 1 to 15:
           mmtx.rtit[i] = substring(mmtx.r-title,i,1).
        end.
     end.
  end.  /* not avail mmtx */
  /* ====== create spoilage ================*/
  if (mstd.sp-x ne 0 and mstd.sp-y ne 0) then
     find first mmtx of mstd where mmtx.mr-run = true no-lock no-error.
  if not avail mmtx then do:
     tmpstore = "".
     for each mmtx by mmtx.mmtx-no descending:
         i = mmtx.mmtx-no.
         leave.
     end.

     create mmtx.
     assign  mmtx.mmtx-no = i + 1
             mmtx.company = mstd.company
             mmtx.loc     = mstd.loc
             mmtx.m-code  = mstd.m-code
             mmtx.dept    = mstd.dept
             mmtx.style   = mstd.style
             mmtx.mr-run  = true.

     if sp-x ne 0 then do:
        find std-code where std-code.code = string(sp-x,"99") no-lock no-error.
        mmtx.c-title = caps(std-code.dscr).
        do i = 1 to length(mmtx.c-title):
           tmpstore = tmpstore + substring(mmtx.c-title,i,1) + " ".
        end.
        mmtx.c-title = tmpstore.
     end.
     if sp-y ne 0 then do:
        find std-code where std-code.code = string(sp-y,"99") no-lock no-error.
        mmtx.r-title = caps(std-code.dscr).
        tmpstore = "".
        do i = 1 to 15:
           mmtx.rtit[i] = substring(mmtx.r-title,i,1).
        end.
     end.
  end.   

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-mstd V-table-Win 
PROCEDURE enable-mstd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-adding-record THEN mstd.style:SCREEN-VALUE = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-cnt as int no-undo.
  def var ll-new-record as log no-undo.
  def var ls-old-m-code like mstd.m-code no-undo.
  def var ls-old-dept like mstd.dept no-undo.
  def var ls-old-style like mstd.style no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
   ll-new-record = adm-new-record
   ls-old-m-code = mstd.m-code
   ls-old-dept   = mstd.dept
   ls-old-style  = mstd.style.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  do with frame {&frame-name}:
     ASSIGN
      mstd.mx-dscr[1] = mstd.mx-dscr[1]:SCREEN-VALUE
      mstd.mx-dscr[2] = mstd.mx-dscr[2]:SCREEN-VALUE
      mstd.mx-dscr[3] = mstd.mx-dscr[3]:SCREEN-VALUE
      mstd.mx-dscr[4] = mstd.mx-dscr[4]:SCREEN-VALUE
      mstd.dept       = mstd.dept:SCREEN-VALUE.
  end.

  if ll-new-record then do:
     if adm-adding-record then run create-matrix.
     else run copy-matrix (ls-old-m-code, ls-old-dept, ls-old-style).
  end.
  else run update-matrix (ls-old-style). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-mach-recid as recid no-undo.
  def var char-hdl as cha no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (This-procedure,"Record-Source", output char-hdl).
  run get-recid in widget-handle(char-hdl)  (output lv-mach-recid).
  if not avail mach then find mach where recid(mach) = lv-mach-recid no-lock no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign mstd.company = mach.company
         mstd.loc = mach.loc
         .
  if adm-adding-record then do:  /* for add */
     assign mstd.m-code = mach.m-code
            mstd.dept = mach.dept[1]
            mstd.style = "            so we don't get error"   /* blank style already exists */
            .
     display mstd.m-code mstd.dept with frame {&frame-name}.
     find first dept where dept.company = mach.company
                    and dept.code = mstd.dept
                    no-lock no-error.

     assign mcode_dscr = mach.m-dscr
            dept_dscr =  if avail dept then dept.dscr else "".
     disp mcode_dscr dept_dscr with frame {&frame-name}.        

   end.  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-mach-recid as recid no-undo.
  def var char-hdl as cha  no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run get-link-handle in adm-broker-hdl(This-procedure, "Record-Source", output char-hdl).
  run get-recid in widget-handle(char-hdl) (output lv-mach-recid).
  find mach where recid(mach) = lv-mach-recid no-lock no-error.

  mcode_dscr:screen-value in frame {&frame-name} = mach.m-dscr.
  find first dept where /*dept.company = mach.company and */
                        dept.code = mstd.dept
                    no-lock no-error.
  dept_dscr:screen-value in frame {&frame-name} = if avail dept then dept.dscr else "".

  find first style where style.style = mstd.style no-lock no-error.
  style_dscr:screen-value = if avail style then style.dscr else "".

  find FIRST std-code where std-code.code = string(mstd.mr-x:screen-value in frame {&frame-name}, "99")
                no-lock no-error.
  mx-dscr[1]:screen-value = if avail std-code then std-code.dscr else "".
  find first std-code where std-code.code = mr-y:screen-value no-lock no-error.
  mx-dscr[2]:screen-value = if avail std-code then std-code.dscr else "".
  find first std-code where std-code.code = rs-x:screen-value no-lock no-error.
  mx-dscr[3]:screen-value = if avail std-code then std-code.dscr else "".
  find first std-code where std-code.code = rs-y:screen-value no-lock no-error.
  mx-dscr[4]:screen-value = if avail std-code then std-code.dscr else "".
  find first std-code where std-code.code = sp-x:screen-value no-lock no-error.
  sp-dscr1:screen-value = if avail std-code then std-code.dscr else "".
  find first std-code where std-code.code = sp-y:screen-value no-lock no-error.
  sp-dscr2:screen-value = if avail std-code then std-code.dscr else "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 def buffer bf-mstd for mstd.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-m-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  if can-find(first bf-mstd where bf-mstd.company = mstd.company and
                              bf-mstd.loc = mstd.loc and
                              bf-mstd.m-code = mstd.m-code and
                              bf-mstd.dept = mstd.dept and
                              bf-mstd.style = mstd.style:screen-value in frame {&frame-name} and
                              recid(bf-mstd) <> recid(mstd)
                 ) 
      then do:
           message "Machine Standard already exists with these parameters!" view-as alert-box error.
          /* run dispatch ('enable-fields').*/
           apply "entry" to mstd.style in frame {&frame-name}.
           return no-apply.            
   end.             
   IF mstd.mr-x:screen-value <> "00" AND
      NOT CAN-FIND(first std-code where
      std-code.code = mstd.mr-x:SCREEN-VALUE)  then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.mr-x in frame {&frame-name}.
      return no-apply.
   end.
   IF mstd.mr-y:screen-value <> "00" AND
      NOT CAN-FIND(first std-code where
      std-code.code = mstd.mr-y:screen-value) then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.mr-y in frame {&frame-name}.
      return no-apply.
   end.
   IF mstd.rs-x:screen-value <> "00" AND
      NOT CAN-FIND(first std-code WHERE
      std-code.code = mstd.rs-x:screen-value) then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.rs-x in frame {&frame-name}.
      return no-apply.
   end.
   IF mstd.rs-y:screen-value <> "00" AND
      NOT CAN-FIND(first std-code WHERE
      std-code.code = mstd.rs-y:SCREEN-VALUE) then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.rs-y in frame {&frame-name}.
      return no-apply.
   end.
   IF mstd.sp-x:screen-value <> "00" AND
      NOT CAN-FIND(first std-code WHERE
      std-code.code = mstd.sp-x:screen-value) then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.sp-x in frame {&frame-name}.
      return no-apply.
   end.
   IF mstd.sp-y:screen-value <> "00" AND
      NOT CAN-FIND(first std-code WHERE
      std-code.code = mstd.sp-y:screen-value) then do:
      message "Invalid code. Try help." view-as alert-box error.
      apply "entry" to mstd.sp-y in frame {&frame-name}.
      return no-apply.
   end.

  IF mstd.mr-x:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.mr-x in frame {&frame-name}.
     return no-apply.
  END.
  IF mstd.mr-y:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.mr-y in frame {&frame-name}.
     return no-apply.
  END.
  IF mstd.rs-x:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.rs-x in frame {&frame-name}.
     return no-apply.
  END.
  IF mstd.rs-y:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.rs-y in frame {&frame-name}.
     return no-apply.
  END.
  IF mstd.sp-x:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.sp-x in frame {&frame-name}.
     return no-apply.
  END.
  IF mstd.sp-y:SCREEN-VALUE = "30" AND INDEX("DC,GL",mstd.dept) EQ 0 THEN
  DO:
     message "Code 30 is only valid for department DC or GL." view-as alert-box error.
     apply "entry" to mstd.sp-y in frame {&frame-name}.
     return no-apply.
  END.
  {&methods/lValidateError.i NO}
  RUN check-for-special-codes.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run get-link-handle in adm-broker-hdl (This-procedure,"Record-Source", output char-hdl).
  run new-mach-copied in widget-handle(char-hdl) (ROWID(mstd)).

  ASSIGN
   adm-new-record    = NO
   adm-adding-record = NO.

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
  {src/adm/template/snd-list.i "mstd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-matrix V-table-Win 
PROCEDURE update-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param ip-style like mstd.style no-undo.
def var tmpstore as cha no-undo.
def var i as int no-undo.
def buffer bf-mmtx for mmtx.

tmpstore = "".
FOR EACH mmty where mmty.company = mstd.company and
                      mmty.loc = mstd.loc and
                      mmty.m-code = mstd.m-code and
                      mmty.dept = mstd.dept and
                      mmty.style = ip-style EXCLUSIVE-LOCK:
   if mstd.mr-x ne 0 then do:
      find first std-code where std-code.code = string(mstd.mr-x,"99") no-lock no-error.
      if mmty.c-title ne std-code.dscr then mmty.c-title = caps(std-code.dscr).
      do i = 1 to length(mmty.c-title):
         tmpstore = tmpstore + substring(mmty.c-title,i,1) + " ".
      end.
      ASSIGN
         mmty.c-title = tmpstore
         tmpstore = "" .
   end.
   if mstd.mr-y ne 0 then do:
      find first std-code where std-code.code = string(mstd.mr-y,"99") no-lock no-error.
      if mmty.r-title ne std-code.dscr then mmty.r-title = caps(std-code.dscr).
      do i = 1 to 15:
         mmty.rtit[i] = substring(mmty.r-title,i,1).
      end.
   end.
   mmty.style = mstd.style.
end.

tmpstore = "".
FOR EACH mmtx where mmtx.company = mstd.company and
                      mmtx.loc = mstd.loc and
                      mmtx.m-code = mstd.m-code and
                      mmtx.dept = mstd.dept and
                      mmtx.style = ip-style and
                      not mr-run  EXCLUSIVE-LOCK:
   if mstd.rs-x ne 0 then do:
      find first std-code where std-code.code = string(mstd.rs-x,"99") no-lock no-error.
      if mmtx.c-title ne std-code.dscr then mmtx.c-title = caps(std-code.dscr).
      do i = 1 to length(mmtx.c-title):
         tmpstore = tmpstore + substring(mmtx.c-title,i,1) + " ".
      end.
      ASSIGN
         mmtx.c-title = tmpstore 
         tmpstore = "" .
   end.
   if mstd.rs-y ne 0 then do:
      find first std-code where std-code.code = string(mstd.rs-y,"99") no-lock no-error.
      if mmtx.r-title ne std-code.dscr then mmtx.r-title = caps(std-code.dscr).
      do i = 1 to 15:
         mmtx.rtit[i] = substring(mmtx.r-title,i,1).
      end.
   end.
   mmtx.style = mstd.style.
end.

tmpstore = "".
FIND FIRST mmtx where mmtx.company = mstd.company and
                      mmtx.loc = mstd.loc and
                      mmtx.m-code = mstd.m-code and
                      mmtx.dept = mstd.dept and
                      mmtx.style = ip-style
                      and mr-run NO-ERROR.  /* spoil mtx */
IF AVAIL mmtx THEN DO:
 FOR EACH bf-mmtx where bf-mmtx.company = mstd.company and
                      bf-mmtx.loc = mstd.loc and
                      bf-mmtx.m-code = mstd.m-code and
                      bf-mmtx.dept = mstd.dept and
                      bf-mmtx.style = ip-style
                      and mr-run EXCLUSIVE-LOCK.  /* spoil mtx */
   if mstd.sp-x ne 0 then do:
      find first std-code where std-code.code = string(mstd.sp-x,"99") no-lock no-error.
      if bf-mmtx.c-title ne std-code.dscr then bf-mmtx.c-title = caps(std-code.dscr).
      do i = 1 to length(bf-mmtx.c-title):
         tmpstore = tmpstore + substring(bf-mmtx.c-title,i,1) + " ".
      end.
      ASSIGN
         bf-mmtx.c-title = tmpstore
         tmpstore = "" .
   end.
   if mstd.sp-y ne 0 then do:
      find first std-code where std-code.code = string(mstd.sp-y,"99") no-lock no-error.
      if bf-mmtx.r-title ne std-code.dscr then bf-mmtx.r-title = caps(std-code.dscr).
      do i = 1 to 15:
         bf-mmtx.rtit[i] = substring(bf-mmtx.r-title,i,1).
      end.
   end.
   bf-mmtx.style = mstd.style.
 end. /* end of for each bf-mmtx*/
END. /* avail mmtx*/
  ELSE if (mstd.sp-x ne 0 and mstd.sp-y ne 0) then do:
     tmpstore = "".
     for each mmtx by mmtx.mmtx-no descending:
         i = mmtx.mmtx-no.
         leave.
     end.

     create mmtx.
     assign  mmtx.mmtx-no = i + 1
             mmtx.company = mstd.company
             mmtx.loc     = mstd.loc
             mmtx.m-code  = mstd.m-code
             mmtx.dept    = mstd.dept
             mmtx.style   = mstd.style
             mmtx.mr-run  = true.

     if sp-x ne 0 then do:
        find std-code where std-code.code = string(sp-x,"99") no-lock no-error.
        mmtx.c-title = caps(std-code.dscr).
        do i = 1 to length(mmtx.c-title):
           tmpstore = tmpstore + substring(mmtx.c-title,i,1) + " ".
        end.
        mmtx.c-title = tmpstore.
     end.
     if sp-y ne 0 then do:
        find std-code where std-code.code = string(sp-y,"99") no-lock no-error.
        mmtx.r-title = caps(std-code.dscr).
        tmpstore = "".
        do i = 1 to 15:
           mmtx.rtit[i] = substring(mmtx.r-title,i,1).
        end.
     end.
end.   


def var char-hdl as cha no-undo.
/* reopen children query */
run get-link-handle in adm-broker-hdl (this-procedure,'st-open-y-target', output char-hdl).
if valid-handle(widget-handle(char-hdl)) then run dispatch in widget-handle(char-hdl) ('open-query').
run get-link-handle in adm-broker-hdl (this-procedure,'st-open-x-target', output char-hdl).
if valid-handle(widget-handle(char-hdl)) then run dispatch in widget-handle(char-hdl) ('open-query').
run get-link-handle in adm-broker-hdl (this-procedure,'st-open-xsp-target', output char-hdl).
if valid-handle(widget-handle(char-hdl)) then run dispatch in widget-handle(char-hdl) ('open-query') .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code V-table-Win 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-mach FOR mach.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST bf-mach WHERE bf-mach.company EQ gcompany
                                    AND bf-mach.loc     EQ gloc
                                    AND bf-mach.m-code  EQ mstd.m-code:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO mstd.m-code.
      RETURN ERROR.
    END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

