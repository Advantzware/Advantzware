&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browsercustom.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwcustom.i}

    Description : Browser customization as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


do icnt = 1 to num-entries({&brw-list}, ","):
       
    brwHand = handle(entry(icnt, {&brw-list}, ",")).
    
    if VALID-HANDLE(brwHand) then
    assign
        brwHand:bgcolor            = 25 
        brwHand:fgcolor            = 0
        brwHand:separator-fgcolor  = 15
        brwHand:row-height-chars   = 0.84
        brwHand:font               = 22.

    colHand = brwHand:first-column.
    do while valid-handle(colHand).
        colHandList =  colHandList + ","  + string(colHand).
        colHand = colHand:next-column.
    end.
    colHandList = trim(colHandList, ",").
 
    create  brwtemp.
    assign
        brwtemp.brwname = entry(icnt, {&brw-list}, ",")
        brwtemp.columlist = colHandList.
        
    colHandList = "".
end.
    







