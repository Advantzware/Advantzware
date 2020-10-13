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

    on 'row-display' of {&brw-name} do:
        for each brwtemp :   
            brwHand = handle(brwtemp.brwname).
            colHandList = brwtemp.columlist.
            if brwHand:name <> "{&brw-name}" then next.
            iF CURRENT-RESULT-ROW(brwHand:name) mod 2 = 0 then
            do iCnt = 1 TO num-entries(colHandList, ","): 
                colHand = handle(entry(iCnt,colHandList,",")).
                if VALID-HANDLE(colHand) then
                colHand:BGCOLOR = 25.  
            end. 
            else
            do iCnt = 1 TO num-entries(colHandList, ","):
                colHand = handle(entry(iCnt,colHandList,",")).
                if VALID-HANDLE(colHand) then
                colHand:BGCOLOR = 26.
            end.
            colHand = ?.
            brwHand = ?.
        end.  
    end.






