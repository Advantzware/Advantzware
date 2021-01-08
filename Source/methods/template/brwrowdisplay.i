&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : brwrowdisply.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwrowdisply.i}

    Description : Browser customization as per new enhanced UI 
    
    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DO iCnt = 1 TO NUM-ENTRIES(cColHandList, ","):
    hColumnRowColor = HANDLE(ENTRY(iCnt,cColHandList,",")).
    IF VALID-HANDLE(hColumnRowColor) THEN
    hColumnRowColor:BGCOLOR = IF CURRENT-RESULT-ROW("{&BROWSE-NAME}") MOD 2 EQ 0 THEN 25
                              ELSE 26.   
END. 
