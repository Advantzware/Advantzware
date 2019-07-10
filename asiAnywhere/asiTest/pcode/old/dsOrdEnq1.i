
/*------------------------------------------------------------------------
    File        : dsOrdEnq.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Enquiry Maintenance

    Author(s)   : Dilbagh Gill
    Created     : Sat August 25 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE oe-ord

BEFORE-TABLE beforeOrder
    FIELD order_image AS CHARACTER.

    
DEFINE DATASET dsOrder FOR ttOrder.

DEFINE QUERY q-OrderQuery FOR oe-ord.

DEFINE DATA-SOURCE src-Order FOR QUERY q-OrderQuery.

BUFFER ttOrder:ATTACH-DATA-SOURCE(DATA-SOURCE src-Order:HANDLE).

DEFINE VARIABLE v-Order AS HANDLE     NO-UNDO. 
v-Order = DATASET dsOrder:HANDLE.   
v-Order:GET-BUFFER-HANDLE("ttOrder"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-image", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-image:
    DEFINE INPUT PARAMETER DATASET FOR dsOrder.
    
    ASSIGN ttOrder.Order_image = "images/cat" + STRING(ttOrder.po-no, "99999") + ".jpg".
END PROCEDURE. 

