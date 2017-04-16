&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File:              editasi.w

  Description:       Dynamic Field Editor

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           02/16/2000
  MOdified:   YSK  03/16/2001   Not to have "," on current-widget:screen-value
                                for Decimal,Integer type field 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE m_file AS CHARACTER NO-UNDO.
DEFINE VARIABLE m_field AS CHARACTER NO-UNDO.
DEFINE VARIABLE primflds AS CHARACTER NO-UNDO.
DEFINE VARIABLE fld-frmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE fld-type AS CHARACTER NO-UNDO.
DEFINE VARIABLE fld-labels AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE y AS INTEGER NO-UNDO.
DEFINE VARIABLE dyn-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE label-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE table_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE data-private AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD field-name AS CHARACTER
  FIELD field-value AS CHARACTER
    INDEX ttbl IS PRIMARY UNIQUE
          field-name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Table Btn_Field Btn_Refresh Btn_First ~
Btn_Next Btn_Previous Btn_Last Btn_Get Btn_Apply Btn_Exit m_dbs m_fields ~
m_files where-phrase Btn_Clear Btn_Copy_Key RECT-20 RECT-21 RECT-22 RECT-23 
&Scoped-Define DISPLAYED-OBJECTS m_dbs m_fields m_files where-phrase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Apply 
     LABEL "&Apply" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Clear 
     LABEL "&Clear Phrase" 
     SIZE 14 BY .95.

DEFINE BUTTON Btn_Copy_Key 
     LABEL "Copy &Key Flds" 
     SIZE 14 BY .95.

DEFINE BUTTON Btn_Exit AUTO-END-KEY DEFAULT 
     LABEL "E&xit" 
     SIZE 10 BY 1
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Field 
     LABEL "Fi&eld" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_First 
     LABEL "&First" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Get 
     LABEL "&Get" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Last 
     LABEL "&Last" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Next 
     LABEL "&Next" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Previous 
     LABEL "&Previous" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Refresh 
     LABEL "&Refresh" 
     SIZE 10 BY 1.

DEFINE BUTTON Btn_Table DEFAULT 
     LABEL "&Table" 
     SIZE 11 BY 1
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE where-phrase AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 78 BY 3.29
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 1.43.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 1.43.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.43.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 1.43.

DEFINE VARIABLE m_dbs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 20 BY 4.76 NO-UNDO.

DEFINE VARIABLE m_fields AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 21 BY 16.67 NO-UNDO.

DEFINE VARIABLE m_files AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 20 BY 12.62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     Btn_Table AT ROW 1.24 COL 23 HELP
          "Use this function to ACCEPT selected field"
     Btn_Field AT ROW 1.24 COL 35
     Btn_Refresh AT ROW 1.24 COL 46
     Btn_First AT ROW 1.24 COL 58
     Btn_Next AT ROW 1.24 COL 69
     Btn_Previous AT ROW 1.24 COL 80
     Btn_Last AT ROW 1.24 COL 91
     Btn_Get AT ROW 1.24 COL 103
     Btn_Apply AT ROW 1.24 COL 114
     Btn_Exit AT ROW 1.24 COL 126 HELP
          "Use this function to CANCEL field selecition"
     m_dbs AT ROW 1.71 COL 1 HELP
          "Select Database Name" NO-LABEL
     m_fields AT ROW 3.14 COL 22 HELP
          "Select Field Name" NO-LABEL
     m_files AT ROW 7.19 COL 1 HELP
          "Select Table Name" NO-LABEL
     where-phrase AT ROW 16.48 COL 59 HELP
          "Enter Where Phrase" NO-LABEL
     Btn_Clear AT ROW 17.43 COL 44
     Btn_Copy_Key AT ROW 18.62 COL 44
     RECT-20 AT ROW 1 COL 22
     RECT-21 AT ROW 1 COL 57
     RECT-22 AT ROW 1 COL 102
     RECT-23 AT ROW 1 COL 125
     "Table Names" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 6.48 COL 2
          FONT 4
     "Field Names" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 2.43 COL 23
          FONT 4
     "Database Names" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1 COL 2
          FONT 4
     "Where Phrase:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 16.48 COL 44
     SPACE(78.00) SKIP(2.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "DB.Table.Field Editor"
         CANCEL-BUTTON Btn_Exit.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Apply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Apply DIALOG-1
ON CHOOSE OF Btn_Apply IN FRAME DIALOG-1 /* Apply */
DO:
  RUN Apply_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear DIALOG-1
ON CHOOSE OF Btn_Clear IN FRAME DIALOG-1 /* Clear Phrase */
DO:
  where-phrase:SCREEN-VALUE = ''.
  APPLY 'LEAVE' TO where-phrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Copy_Key
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Copy_Key DIALOG-1
ON CHOOSE OF Btn_Copy_Key IN FRAME DIALOG-1 /* Copy Key Flds */
DO:
  RUN Copy_Key_Fields.
  APPLY 'LEAVE' TO where-phrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Field DIALOG-1
ON CHOOSE OF Btn_Field IN FRAME DIALOG-1 /* Field */
DO:
  DO i = 1 TO m_fields:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF m_fields:IS-SELECTED(i) THEN
    DO:
      m_field = IF INDEX(m_fields:ENTRY(i),'[') = 0 THEN m_fields:ENTRY(i)
                ELSE SUBSTRING(m_fields:ENTRY(i),1,INDEX(m_fields:ENTRY(i),'[') - 1).
      RUN "editasi/fld_lbls.p" (m_files:SCREEN-VALUE,m_field,OUTPUT fld-labels).
      IF fld-labels = ? THEN
      fld-labels = m_fields:ENTRY(i).
      RUN "editasi/get_type.p" (m_files:SCREEN-VALUE,m_field,OUTPUT fld-type).
      RUN "editasi/get_frmt.p" (m_files:SCREEN-VALUE,m_field,OUTPUT fld-frmt).
      data-private = 'field'.
      RUN Create_Fields (m_fields:ENTRY(i),fld-type,fld-frmt,fld-labels).
      y = y + 20.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_First DIALOG-1
ON CHOOSE OF Btn_First IN FRAME DIALOG-1 /* First */
DO:
  RUN 'editasi/dbimage.p' ('FIRST',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  RUN Display_Key_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Get
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Get DIALOG-1
ON CHOOSE OF Btn_Get IN FRAME DIALOG-1 /* Get */
DO:
  RUN Get_Key_Value.
  RUN 'editasi/dbimage.p' ('GET',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  RUN Display_Key_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Last DIALOG-1
ON CHOOSE OF Btn_Last IN FRAME DIALOG-1 /* Last */
DO:
  RUN 'editasi/dbimage.p' ('LAST',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  RUN Display_Key_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Next DIALOG-1
ON CHOOSE OF Btn_Next IN FRAME DIALOG-1 /* Next */
DO:
  RUN 'editasi/dbimage.p' ('NEXT',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  RUN Display_Key_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Previous
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Previous DIALOG-1
ON CHOOSE OF Btn_Previous IN FRAME DIALOG-1 /* Previous */
DO:
  RUN 'editasi/dbimage.p' ('PREV',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  RUN Display_Key_Value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Refresh DIALOG-1
ON CHOOSE OF Btn_Refresh IN FRAME DIALOG-1 /* Refresh */
DO:
  DELETE WIDGET-POOL "dynlabels-widget" NO-ERROR.
  CREATE WIDGET-POOL "dynlabels-widget" PERSISTENT.
  DELETE WIDGET-POOL "dynamic-widget" NO-ERROR.
  CREATE WIDGET-POOL "dynamic-widget" PERSISTENT.
  where-phrase:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Table DIALOG-1
ON CHOOSE OF Btn_Table IN FRAME DIALOG-1 /* Table */
DO:
  APPLY 'CHOOSE' TO Btn_Refresh.
  ASSIGN
    x = 295
    y = 35
    m_file = m_dbs:SCREEN-VALUE + '.' + m_files:SCREEN-VALUE
    where-phrase:SCREEN-VALUE = ''
    ldummy = where-phrase:SAVE-FILE('editasi/dbimage.whr')
    key_value = ''
    table_rowid = ?
    data-private = 'save'.
  RUN 'editasi/primflds.p' (m_files:SCREEN-VALUE,OUTPUT primflds).
  RUN 'editasi/fld_lbls.p' (m_files:SCREEN-VALUE,primflds,OUTPUT fld-labels).
  IF fld-labels = ? THEN
  fld-labels = primflds.
  DO i = 1 TO NUM-ENTRIES(primflds):
    RUN 'editasi/get_type.p' (m_files:SCREEN-VALUE,ENTRY(i,primflds),OUTPUT fld-type).
    RUN 'editasi/get_frmt.p' (m_files:SCREEN-VALUE,ENTRY(i,primflds),OUTPUT fld-frmt).
    RUN Create_Fields
       (ENTRY(i,primflds),fld-type,fld-frmt,ENTRY(i,fld-labels)).
    y = y + 20.
  END.
  RUN 'editasi/mkimage.p' (m_files:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_dbs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_dbs DIALOG-1
ON VALUE-CHANGED OF m_dbs IN FRAME DIALOG-1
DO:
  RUN Get_Files.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,INDEX(m_field,".") + 1,
                  R-INDEX(m_field,".") - INDEX(m_field,".") - 1),
                  m_files:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_files:SCREEN-VALUE = ENTRY(i,m_files:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO m_files.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_fields DIALOG-1
ON DEFAULT-ACTION OF m_fields IN FRAME DIALOG-1
DO:
  APPLY "CHOOSE" TO Btn_Field.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_files
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_files DIALOG-1
ON DEFAULT-ACTION OF m_files IN FRAME DIALOG-1
DO:
  APPLY "CHOOSE" TO Btn_Table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_files DIALOG-1
ON VALUE-CHANGED OF m_files IN FRAME DIALOG-1
DO:
  RUN Get_Fields.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,R-INDEX(m_field,".") + 1),m_fields:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_fields:SCREEN-VALUE = ENTRY(i,m_fields:LIST-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME where-phrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL where-phrase DIALOG-1
ON LEAVE OF where-phrase IN FRAME DIALOG-1
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE '' AND
     INDEX({&SELF-NAME}:SCREEN-VALUE,'WHERE') = 0 THEN
  {&SELF-NAME}:SCREEN-VALUE = 'WHERE ' + {&SELF-NAME}:SCREEN-VALUE.
  ldummy = {&SELF-NAME}:SAVE-FILE('editasi/dbimage.whr').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Get_DBs.
  IF m_dbs:LIST-ITEMS IN FRAME {&FRAME-NAME} = ? THEN
  RUN Check_PF_File.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,1,INDEX(m_field,".") - 1),m_dbs:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_dbs:SCREEN-VALUE = ENTRY(i,m_dbs:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO m_dbs.
  WAIT-FOR GO OF FRAME {&FRAME-NAME} FOCUS m_dbs.
END.
RUN disable_UI.
/* QUIT. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_Value DIALOG-1 
PROCEDURE Apply_Value :
/*------------------------------------------------------------------------------
  Purpose:     Apply Field Values
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

  IF key_value = '' THEN
  RETURN.
  OUTPUT TO 'editasi/dbimage.i'.
  PUT UNFORMATTED 'ASSIGN' SKIP.
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'field' THEN
    DO:
      RUN "editasi/get_type.p" (m_files:SCREEN-VALUE,current-widget:NAME,OUTPUT fld-type).
      PUT UNFORMATTED '  '
        m_files:SCREEN-VALUE '.' current-widget:NAME ' = '.
      IF fld-type = 'STRING' THEN
      PUT UNFORMATTED '~"'.
      /* 03/16/01 YSK  not to include "," for deciaml,integer type */
      if fld-type = 'Decimal' or fld-type = 'Integer'
         then PUT UNFORMATTED decimal(current-widget:screen-value).
      else 
      /* =========== end of mods ================     YSK */
      PUT UNFORMATTED current-widget:SCREEN-VALUE.
      IF fld-type = 'STRING' THEN
      PUT UNFORMATTED '~"'.
      PUT UNFORMATTED SKIP.
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.
  PUT UNFORMATTED '  .' SKIP.
  OUTPUT CLOSE.
  IF SEARCH('editasi/dbimage.r') <> ? THEN OS-DELETE VALUE(SEARCH('editasi/dbimage.r')).

  IF where-phrase:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
  RUN 'editasi/dbimage.p' ('APPLY',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  ELSE
  DO:
    ldummy = no.
    MESSAGE 'Apply Value Changes based on Where Phrase to ALL Table Records?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF ldummy THEN
    RUN 'editasi/dbimage.p' ('APPLY-ALL',INPUT-OUTPUT key_value,INPUT-OUTPUT table_rowid).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check_PF_File DIALOG-1 
PROCEDURE Check_PF_File :
/*------------------------------------------------------------------------------
  Purpose:     Check to be sure -q startup parameter does not exist
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE x AS CHARACTER NO-UNDO.

  ldummy = no.
  MESSAGE 'Is this a Single User Environment?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE ldummy.
  IF ldummy THEN
  INPUT FROM 'asi.pf' NO-ECHO.
  ELSE
  INPUT FROM 'asinet.pf' NO-ECHO.
  ldummy = no.
  OUTPUT TO 'editasi.pf'.
  REPEAT:
    IMPORT UNFORMATTED x.
    IF INDEX(x,'-q') NE 0 OR INDEX(x,'-p') NE 0 THEN
    DO:
      ldummy = yes.
      NEXT.
    END.
    PUT UNFORMATTED x SKIP.
  END.
  OUTPUT CLOSE.
  INPUT CLOSE.
  IF ldummy THEN
  DO:
    MESSAGE 'DB.Table.Field Editor environment has been correctly configured.' SKIP
            'This process will auto quit, re-run this process to continue.'
        VIEW-AS ALERT-BOX.
    QUIT.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copy_Key_Fields DIALOG-1 
PROCEDURE Copy_Key_Fields :
/*------------------------------------------------------------------------------
  Purpose:     Display Key Values
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE text-fld AS CHARACTER INITIAL 'WHERE' NO-UNDO.
  DEFINE VARIABLE wphrase AS CHARACTER NO-UNDO.

  IF key_value = '' THEN
  RETURN.
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'save' THEN
    DO:
      RUN "editasi/get_type.p" (m_files:SCREEN-VALUE,current-widget:NAME,OUTPUT fld-type).
      wphrase = wphrase + text-fld + ' ' +
          m_files:SCREEN-VALUE + '.' + current-widget:NAME + ' = '.
      IF fld-type = 'STRING' THEN
      wphrase = wphrase + '~"'.
      wphrase = wphrase + current-widget:SCREEN-VALUE.
      IF fld-type = 'STRING' THEN
      wphrase = wphrase + '~"'.
      ASSIGN
        wphrase = wphrase + CHR(10)
        text-fld = '  AND'.
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.
  where-phrase:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wphrase.
  RUN Display_Field_Value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_Fields DIALOG-1 
PROCEDURE Create_Fields :
/*------------------------------------------------------------------------------
  Purpose:     Create Dymnamic Widget Fields and Text Labels
  Parameters:  field, data type, format and field label
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER field-name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER field-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER field-format AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER field-label AS CHARACTER NO-UNDO.

  DEFINE VARIABLE field-width AS DECIMAL NO-UNDO.

  CREATE TEXT label-widget IN WIDGET-POOL 'dynlabels-widget'
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      AUTO-RESIZE = YES
      Y = y
      FORMAT = 'X(' +
       (IF LENGTH(field-label) NE 0 THEN STRING(LENGTH(field-label) + 1)
        ELSE '1') + ')'
      SENSITIVE = YES
      SCREEN-VALUE = IF field-label = '' THEN '' ELSE field-label + ':'.
  ASSIGN
      label-widget:FONT = ?
      label-widget:X = IF x - label-widget:WIDTH-PIXELS - 1 LT 0 THEN 0
                       ELSE x - label-widget:WIDTH-PIXELS - 1
      label-widget:HEIGHT-CHARS = 1
      ldummy = label-widget:MOVE-TO-TOP()
      label-widget:HIDDEN = NO
      field-width = LENGTH(field-format) * 1.8.

  IF  field-type = 'STRING' AND INDEX(field-format,'(') NE 0 THEN
  field-width = INTEGER(REPLACE(REPLACE(field-format,')',''),'x(','')) * 1.8.
  IF field-width LT 3 THEN
  field-width = 4.
  IF field-width GT 75 THEN
  field-width = 75.

  CREATE FILL-IN dyn-widget IN WIDGET-POOL 'dynamic-widget'
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      NAME = field-name
      FONT = 4
      Y = y
      X = x
      PRIVATE-DATA = ''
      SENSITIVE = YES
      WIDTH-CHARS = field-width
      HEIGHT-CHARS = 1
      SIDE-LABEL-HANDLE = label-widget
      DATA-TYPE = IF field-type = 'STRING' THEN 'CHARACTER' ELSE field-type
      FORMAT = field-format
      PRIVATE-DATA = data-private
      BGCOLOR = IF data-private = 'save' THEN 4 ELSE ?
      FGCOLOR = IF data-private = 'save' THEN 15 ELSE ?.
  ASSIGN
      ldummy = dyn-widget:MOVE-TO-TOP()
      dyn-widget:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display_Field_Value DIALOG-1 
PROCEDURE Display_Field_Value :
/*------------------------------------------------------------------------------
  Purpose:     Display Field Values
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

  IF key_value = '' THEN
  DO:
    MESSAGE m_files:SCREEN-VALUE IN FRAME {&FRAME-NAME} ' record not available'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
  FOR EACH ttbl EXCLUSIVE-LOCK:
    DELETE ttbl.
  END.
  INPUT FROM 'editasi/table.d' NO-ECHO.
  REPEAT TRANSACTION:
    CREATE ttbl.
    IMPORT ttbl.field-name ttbl.field-value.
  END.
  INPUT CLOSE.
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'field' THEN
    DO:
      FIND ttbl WHERE ttbl.field-name = current-widget:NAME NO-LOCK NO-ERROR.
      IF AVAILABLE ttbl THEN
      current-widget:SCREEN-VALUE = ttbl.field-value.
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display_Key_Value DIALOG-1 
PROCEDURE Display_Key_Value :
/*------------------------------------------------------------------------------
  Purpose:     Display Key Values
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

  IF key_value = '' THEN
  DO:
    MESSAGE m_files:SCREEN-VALUE IN FRAME {&FRAME-NAME} ' record not available'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD
    i = 0.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'save' THEN
    ASSIGN
      i = i + 1
      current-widget:SCREEN-VALUE = ENTRY(i,key_value).
    current-widget = current-widget:NEXT-SIBLING.
  END.
  RUN Display_Field_Value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY m_dbs m_fields m_files where-phrase 
      WITH FRAME DIALOG-1.
  ENABLE Btn_Table Btn_Field Btn_Refresh Btn_First Btn_Next Btn_Previous 
         Btn_Last Btn_Get Btn_Apply Btn_Exit m_dbs m_fields m_files 
         where-phrase Btn_Clear Btn_Copy_Key RECT-20 RECT-21 RECT-22 RECT-23 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_DBs DIALOG-1 
PROCEDURE Get_DBs :
/* -----------------------------------------------------------
  Purpose: Populate m_dbs selection list with connected database names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_dbs AS CHARACTER NO-UNDO.
  
  m_get_dbs = SUBSTR(m_file,1,INDEX(m_file,".") - 1).
  RUN "editasi/db_list.p" (OUTPUT m_get_dbs).
  m_dbs:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_dbs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Fields DIALOG-1 
PROCEDURE Get_Fields :
/* -----------------------------------------------------------
  Purpose: Populate m_fields selection list with field names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_fields AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(m_dbs:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN "editasi/fld_list.p" (INPUT m_files:SCREEN-VALUE,OUTPUT m_get_fields).
  m_fields:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_fields.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Files DIALOG-1 
PROCEDURE Get_Files :
/* -----------------------------------------------------------
  Purpose: Populate m_files selection list with table names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_files AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(m_dbs:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN "editasi/filelist.p" (OUTPUT m_get_files).
  m_files:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_files.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Key_Value DIALOG-1 
PROCEDURE Get_Key_Value :
/*------------------------------------------------------------------------------
  Purpose:     Get Displayed Key Values
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

  key_value = ''.
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD
    i = 0.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'save' THEN
    key_value = key_value + ',' + current-widget:SCREEN-VALUE.
    current-widget = current-widget:NEXT-SIBLING.
  END.
  key_value = SUBSTRING(key_value,2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

