&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : brwcustomdef.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwcustomdef.i}

    Description : Browser customization defination as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(exclude-brw-custom) EQ 0 &THEN
DEFINE VARIABLE hColumnRowColor  AS HANDLE    NO-UNDO.
DEFINE VARIABLE cColHandList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cTemp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE hTemp            AS HANDLE    NO-UNDO. 
DEFINE VARIABLE hCurrentColumn   AS HANDLE    NO-UNDO. 
DEFINE VARIABLE hPrevColumn      AS HANDLE    NO-UNDO. 
DEFINE VARIABLE lsortBy          AS LOGICAL   NO-UNDO INITIAL TRUE.
&GLOBAL-DEFINE exclude-brw-custom TRUE
&ENDIF
