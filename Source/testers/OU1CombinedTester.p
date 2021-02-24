
/*------------------------------------------------------------------------
    File        : testers/OU1CombinedTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Rahul Rawat
    Created     : Mon Feb 15 23:28:43 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    DEFINE VARIABLE cScreenNo   AS CHARACTER NO-UNDO FORMAT "X(3)".
    DEFINE VARIABLE cScreenList AS CHARACTER NO-UNDO INIT "OU1,OW,OC,OU6,OQ1".
    
    REPEAT:
        MESSAGE "Enter the screen No:" UPDATE cScreenNo.
        
        IF LOOKUP(cScreenNo,cScreenList) EQ 0 THEN 
            MESSAGE "Invalid Screen No#.Please select from following options" cScreenList
            VIEW-AS ALERT-BOX ERROR.
            
        ELSE 
            RUN oe/w-order.w(
                INPUT cScreenNo
                ).
    END.        
        