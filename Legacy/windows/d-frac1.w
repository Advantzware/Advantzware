&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-31 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS ls-frac-1 ls-frac-2 ls-frac-3 ls-frac-4 ~
ls-frac-5 ls-frac-6 ls-frac-7 ls-frac-8 ls-frac-9 ls-frac-10 ls-frac-11 ~
ls-frac-12 ls-frac-13 ls-frac-14 ls-frac-15 ls-frac-16 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartDialogCues" D-Dialog _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartDialog,uib,49267
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ls-frac-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE VARIABLE ls-frac-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE ls-frac-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 111 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117 BY 15.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     ls-frac-1 AT ROW 1.48 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-2 AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-3 AT ROW 3.38 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-4 AT ROW 4.33 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-5 AT ROW 5.29 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-6 AT ROW 6.24 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-7 AT ROW 7.19 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-8 AT ROW 8.14 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-9 AT ROW 9.1 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-10 AT ROW 10.05 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-11 AT ROW 11 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-12 AT ROW 11.71 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-13 AT ROW 12.67 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-14 AT ROW 13.62 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-15 AT ROW 14.57 COL 1 COLON-ALIGNED NO-LABEL
     ls-frac-16 AT ROW 15.52 COL 1 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 17.19 COL 49
     RECT-31 AT ROW 1 COL 1
     SPACE(3.99) SKIP(1.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 0
         TITLE "Fraction to Decimal Chart"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ls-frac-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-10 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-11 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-12 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-13 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-14 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-15 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-16 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-3 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-6 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-7 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-8 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-frac-9 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Fraction to Decimal Chart */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY ls-frac-1 ls-frac-2 ls-frac-3 ls-frac-4 ls-frac-5 ls-frac-6 ls-frac-7 
          ls-frac-8 ls-frac-9 ls-frac-10 ls-frac-11 ls-frac-12 ls-frac-13 
          ls-frac-14 ls-frac-15 ls-frac-16 
      WITH FRAME D-Dialog.
  ENABLE RECT-31 Btn_OK 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
/*  ed-fraction =
     " 1/64 = .015625  17/64 = .265625  33/64 = .515625  49/64 = .765625" + chr(13) +
     " 1/32 = .03125    9/32 = .28125   17/32 = .53125   25/32 = .78125" + chr(13) +
     " 3/64 = .046875  19/64 = .296875  35/64 = .546875  51/64 = .796875" + chr(13) +
     " 1/16 = .0625     5/16 = .3125     9/16 = .5625    13/16 = .8125" + chr(13) +
     " 5/64 = .078125  21/64 = .328125  37/64 = .578125  53/64 = .828125" + chr(13) +
     " 3/32 = .09375   11/32 = .34375   19/32 = .59375   27/32 = .84375" + chr(13) +
     " 7/64 = .109375  23/64 = .359375  39/64 = .609375  55/64 = .859375" + chr(13) +
     "  1/8 = .125       3/8 = .375       5/8  = .625      7/8 = .875" + chr(13) +
     " 9/64 = .140625  25/64 = .390625  41/64 = .640625  57/64 = .890625" + chr(13) +
     " 5/32 = .15625   13/32 = .40625   21/32 = .65625   29/32 = .90625" + chr(13) +
     "11/64 = .171875  27/64 = .421875  43/64 = .671875  59/64 = .921875"+ chr(13) +
     " 3/16 = .1875     7/16 = .4375    11/16 = .6875    15/16 = .9375" + chr(13) +
     "13/64 = .203125  29/64 = .453125  45/64 = .703125  61/64 = .953125" + chr(13) +
     " 7/32 = .21875   15/32 = .46875   23/32 = .71875   31/32 = .96875" + chr(13) +
     "15/64 = .234375  31/64 = .484375  47/64 = .734375  63/64 = .984375" + chr(13) +
     "  1/4 = .25        1/2  = .5        3/4  = .75         1 = 1" 
     .
*/
  assign ls-frac-1  = "   1/64 = .015625    17/64 = .265625    33/64 = .515625    49/64 = .765625"
         ls-frac-2  = "   1/32 = .03125      9/32 = .28125     17/32 = .53125     25/32 = .78125" 
         ls-frac-3  = "   3/64 = .046875    19/64 = .296875    35/64 = .546875    51/64 = .796875"
         ls-frac-4  = "   1/16 = .0625       5/16 = .3125       9/16 = .5625      13/16 = .8125" 
         ls-frac-5  = "   5/64 = .078125    21/64 = .328125    37/64 = .578125    53/64 = .828125"
         ls-frac-6  = "   3/32 = .09375     11/32 = .34375     19/32 = .59375     27/32 = .84375"
         ls-frac-7  = "   7/64 = .109375    23/64 = .359375    39/64 = .609375    55/64 = .859375"
         ls-frac-8  = "    1/8 = .125         3/8 = .375         5/8  = .625        7/8 = .875" 
         ls-frac-9  = "   9/64 = .140625    25/64 = .390625    41/64 = .640625    57/64 = .890625" 
         ls-frac-10 = "   5/32 = .15625     13/32 = .40625     21/32 = .65625     29/32 = .90625" 
         ls-frac-11 = "  11/64 = .171875    27/64 = .421875    43/64 = .671875    59/64 = .921875"
         ls-frac-12 = "   3/16 = .1875       7/16 = .4375      11/16 = .6875      15/16 = .9375" 
         ls-frac-13 = "  13/64 = .203125    29/64 = .453125    45/64 = .703125    61/64 = .953125"
         ls-frac-14 = "   7/32 = .21875     15/32 = .46875     23/32 = .71875     31/32 = .96875"
         ls-frac-15 = "  15/64 = .234375    31/64 = .484375    47/64 = .734375    63/64 = .984375"
         ls-frac-16 = "    1/4 = .25         1/2  = .5          3/4  = .75            1 = 1" 
         .


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


