&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sys/ref/getParms.p
    Purpose     : Build a parameter label list and a parameter value list for
                  a report.
    Syntax      :

    Description :

    Author(s)   : Brad Vigrass
    Created     : 5//14/2013
    Notes       : Field PRIVATE-DATA must include "parm".  Optionally, the 
                 PRIVATE-DATA could include a comma with the desired label
                 to be shown on the parameter output.
                 Ex: "parm,RadioSetLabel" 
  ----------------------------------------------------------------------*/
/*          This .p file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER opcParmValues AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcParmLabels AS CHARACTER NO-UNDO.

DEFINE VARIABLE vhGroup AS HANDLE NO-UNDO.
DEFINE VARIABLE vhField AS HANDLE NO-UNDO.
DEFINE VARIABLE vhFieldRadioLabel AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabel Procedure 
FUNCTION getLabel RETURNS CHARACTER
  ( opcPrivateData AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
  vhGroup = iphFrame:FIRST-CHILD.
  vhField = vhGroup:FIRST-CHILD.
  
  DO WHILE VALID-HANDLE(vhField):
    IF LOOKUP("parm",vhField:PRIVATE-DATA) > 0 THEN DO:
        opcParmValues = opcParmValues + vhField:SCREEN-VALUE + ",".
        IF INDEX(vhField:PRIVATE-DATA,",") = 0 THEN
            opcParmLabels = opcParmLabels + vhField:LABEL + ",".
        ELSE /*parameter label included in private data*/
            opcParmLabels = opcParmLabels + getLabel(vHField:PRIVATE-DATA) + ",".
    END. /*parm field*/
    vhField = vhField:NEXT-SIBLING.
  END. /*main do*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabel Procedure 
FUNCTION getLabel RETURNS CHARACTER
  ( opcPrivateData AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLabel AS CHAR NO-UNDO.
DEFINE VARIABLE iComma AS INTEGER     NO-UNDO.
  
  iComma = INDEX(opcPrivateData,",").
  cLabel = SUBSTRING(opcPrivateData, iComma + 1, LENGTH(opcPrivateData) - iComma ).

  RETURN cLabel.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

