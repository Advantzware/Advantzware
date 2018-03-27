&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : boardDefs.i 
    Purpose     : contains Scheduler Pro definitions

    Syntax      : {{&includes}/Pro/boardDefs.i}

    Description : 

    Author(s)   : Ron Stark
    Created     : 5.5.2004
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*
DEFINE VARIABLE proOptions AS INTEGER NO-UNDO.
DEFINE VARIABLE proOpts AS LOGICAL NO-UNDO EXTENT 10.
*/

/* job move vars */
DEFINE VARIABLE copy2Resource AS LOGICAL NO-UNDO.
DEFINE VARIABLE cursorX AS INTEGER NO-UNDO.
DEFINE VARIABLE cursorY AS INTEGER NO-UNDO.
DEFINE VARIABLE endX AS INTEGER NO-UNDO.
DEFINE VARIABLE endY AS INTEGER NO-UNDO.
DEFINE VARIABLE jobHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE jobMoving AS LOGICAL NO-UNDO.
DEFINE VARIABLE newResourceWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE resizeJob AS LOGICAL NO-UNDO.
DEFINE VARIABLE sortSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE startHeight AS INTEGER NO-UNDO.
DEFINE VARIABLE startWidth AS INTEGER NO-UNDO.
DEFINE VARIABLE startX AS INTEGER NO-UNDO.
DEFINE VARIABLE startY AS INTEGER NO-UNDO.
DEFINE VARIABLE useSequence AS LOGICAL NO-UNDO INITIAL YES.

/* resource vars */
DEFINE VARIABLE lightBulbWidget AS HANDLE NO-UNDO EXTENT 50.

/* job changes history table used by undo & redo functions */
DEFINE TEMP-TABLE ttblJob-do NO-UNDO
  FIELD order AS INTEGER
  FIELD ttblJobRowID AS ROWID
  FIELD resource AS CHARACTER EXTENT 2
  FIELD resourceSequence AS INTEGER EXTENT 2
  FIELD resourceDescription AS CHARACTER EXTENT 2
  FIELD startDate AS DATE EXTENT 2
  FIELD startTime AS INTEGER EXTENT 2
  FIELD endDate AS DATE EXTENT 2
  FIELD endTime AS INTEGER EXTENT 2
  FIELD jobLocked AS LOGICAL EXTENT 2
  FIELD copyOf AS LOGICAL
    INDEX ttblJob-do IS PRIMARY UNIQUE order
    INDEX reverse order DESCENDING.

PROCEDURE GetCursorPos EXTERNAL 'user32.dll':
  DEFINE INPUT-OUTPUT PARAMETER IRect AS MEMPTR.
END PROCEDURE.

PROCEDURE ScreenToClient EXTERNAL 'user32.dll':
  DEFINE INPUT PARAMETER hWnd AS LONG.
  DEFINE INPUT PARAMETER lpPoint AS MEMPTR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD proOptsMsg Include 
FUNCTION proOptsMsg RETURNS CHARACTER
  (ipOption AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 17.1
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION proOptsMsg Include 
FUNCTION proOptsMsg RETURNS CHARACTER
  (ipOption AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  display popup message when pro option not available
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE opts AS CHARACTER NO-UNDO INITIAL
    'Drop and Drag,Update/Create Senerios,Use of Downtime,Viewing Detail Window'.
  
  RETURN ENTRY(ipOption,opts) + ' Enabled Option is Only' + CHR(10) +
         'Available as an Enhanced Scheduler Pro Feature.'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

