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

IF CURRENT-RESULT-ROW("{&BROWSE-NAME}") mod 2 = 0 then
  DO iCnt = 1 TO num-entries(colHandList, ","): 
     colHand = handle(entry(iCnt,colHandList,",")).
     colHand:BGCOLOR = 25. 
  END. 
  else
  DO iCnt = 1 TO num-entries(colHandList, ","):
     colHand = handle(entry(iCnt,colHandList,",")).
     colHand:BGCOLOR = 26.
  END.






