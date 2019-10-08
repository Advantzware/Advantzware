&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME f-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS f-Main 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  INPUT-OUTPUT Parameters:
      <none>

  Author: 

  Created: 
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

/* Local INPUT-OUTPUT PARAMETERiable Definitions ---                                       */
&SCOPED-DEFINE FRAME-NAME f-main
&SCOPED-DEFINE IN IN FRAME {&FRAME-NAME}
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}

DEF INPUT PARAMETER cFullFieldList AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER cFldDataType AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER cFldFormat AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER cFldInitVal AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER cFldColLabel AS CHAR NO-UNDO. 
DEF INPUT-OUTPUT PARAMETER cFldFormula AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiLabel cbDataType fiFormat fiInitVal ~
eFormula slAvail Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiLabel cbDataType fiFormat fiInitVal ~
eFormula slAvail 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cbDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Char","Date","Decimal","Integer","Logical" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE eFormula AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 47 BY 3.75 NO-UNDO.

DEFINE VARIABLE fiFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiInitVal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Initial Value" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Column Label" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE slAvail AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 46 BY 8.75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-Main
     fiLabel AT ROW 1.5 COL 17 COLON-ALIGNED
     cbDataType AT ROW 3 COL 17 COLON-ALIGNED
     fiFormat AT ROW 4.5 COL 17 COLON-ALIGNED
     fiInitVal AT ROW 6 COL 17 COLON-ALIGNED
     eFormula AT ROW 7.5 COL 19 NO-LABEL
     slAvail AT ROW 12.5 COL 19 NO-LABEL
     Btn_OK AT ROW 1.5 COL 51
     Btn_Cancel AT ROW 2.75 COL 51
     "Use Excel-style formatting (e.g. ~"=IF(<condition>,<val if true>,<val if false>)~"" VIEW-AS TEXT
          SIZE 63 BY .67 AT ROW 11.25 COL 3
     "(Dbl-click a field to add it to the formula.)" VIEW-AS TEXT
          SIZE 46 BY .67 AT ROW 21.25 COL 19
     "Formula:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 7.5 COL 11
     "Available Fields:" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 12.5 COL 4
     SPACE(50.71) SKIP(9.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add Calculated Field"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB f-Main 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX f-Main
   L-To-R,COLUMNS                                                       */
ASSIGN 
       FRAME f-Main:SCROLLABLE       = FALSE
       FRAME f-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX f-Main
/* Query rebuild information for DIALOG-BOX f-Main
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX f-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME f-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Main f-Main
ON WINDOW-CLOSE OF FRAME f-Main /* Add Calculated Field */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK f-Main
ON CHOOSE OF Btn_OK IN FRAME f-Main /* OK */
DO:
    ASSIGN
        cFldDataType = IF cbDataType:{&SV} = "Character" THEN "CHAR" ELSE cbDataType:{&SV}
        cFldFormat = fiFormat:{&SV}
        cFldInitVal = fiInitVal:{&SV}
        cFldColLabel = fiLabel:{&SV}
        cFldFormula = eFormula:{&SV}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDataType f-Main
ON VALUE-CHANGED OF cbDataType IN FRAME f-Main /* Data Type */
DO:
    CASE SELF:{&SV}:
        WHEN "Char" THEN DO:
            ASSIGN
                fiFormat:{&SV} = "x(8)"
                fiInitVal:{&SV} = ''.
        END.
        WHEN "Date" THEN DO:
            ASSIGN
                fiFormat:{&SV} = "99/99/99"
                fiInitVal:{&SV} = '?'.
        END.
        WHEN "Decimal" THEN DO:
            ASSIGN
                fiFormat:{&SV} = "->,>>>,>>9.99<<<<<"
                fiInitVal:{&SV} = '0.00'.
        END.
        WHEN "Integer" THEN DO:
            ASSIGN
                fiFormat:{&SV} = "->,>>>,>>>,>>9"
                fiInitVal:{&SV} = '0'.
        END.
        WHEN "Logical" THEN DO:
            ASSIGN
                fiFormat:{&SV} = "YES/NO"
                fiInitVal:{&SV} = 'NO'.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slAvail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slAvail f-Main
ON MOUSE-SELECT-DBLCLICK OF slAvail IN FRAME f-Main
OR RETURN OF slAvail
DO:
    ASSIGN
        eFormula:{&SV} = eFormula:{&SV} + SELF:{&SV}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK f-Main 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects f-Main  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI f-Main  _DEFAULT-DISABLE
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
  HIDE FRAME f-Main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI f-Main  _DEFAULT-ENABLE
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
  DISPLAY fiLabel cbDataType fiFormat fiInitVal eFormula slAvail 
      WITH FRAME f-Main.
  ENABLE fiLabel cbDataType fiFormat fiInitVal eFormula slAvail Btn_OK 
         Btn_Cancel 
      WITH FRAME f-Main.
  VIEW FRAME f-Main.
  {&OPEN-BROWSERS-IN-QUERY-f-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject f-Main 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        slAvail:LIST-ITEMS {&IN} = cFullFieldList.

  RUN SUPER.

    IF cFldDataType = ""
    OR cFldDataType = ? THEN RETURN.
    ELSE ASSIGN
        cbDataType:{&SV} = cFldDataType
        fiFormat:{&SV} = cFldFormat
        fiInitVal:{&SV} = cFldInitVal
        fiLabel:{&SV} = cFldColLabel
        eFormula:{&SV} = cFldFormula.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

