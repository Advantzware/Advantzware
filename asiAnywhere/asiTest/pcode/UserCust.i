
/*------------------------------------------------------------------------
    File        : UserCusti
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Users Customer Maintenance

    Author(s)   :Kuldeep
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserCust NO-UNDO 
    BEFORE-TABLE beforeUserCust
    FIELD user_id AS CHAR
    FIELD cust_no LIKE cust.cust-no
    FIELD customer_default AS LOGICAL
    field CustomerName as char.
  
DEFINE DATASET dsUserCust FOR ttUserCust.  
DEFINE VARIABLE q-UserCustQuery AS HANDLE.
DEFINE VARIABLE src-UserCust AS HANDLE.


CREATE QUERY q-UserCustQuery.
q-UserCustQuery:SET-BUFFERS(BUFFER users:HANDLE,BUFFER usercust:Handle).

CREATE DATA-SOURCE src-UserCust.
src-UserCust:QUERY = q-UserCustQuery.

BUFFER ttUserCust:ATTACH-DATA-SOURCE(src-UserCust).

DEFINE VARIABLE v-UserCust AS HANDLE     NO-UNDO. 
v-UserCust = DATASET dsUserCust:HANDLE.   
v-UserCust:GET-BUFFER-HANDLE("ttUserCust"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-lang", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-lang:
    DEFINE INPUT PARAMETER DATASET FOR dsUserCust.
    find first cust where cust.cust-no EQ ttUserCust.cust_no NO-LOCK .
       

    ASSIGN ttUserCust.CustomerName = cust.name.
    
END PROCEDURE. 

