
/*------------------------------------------------------------------------
    File        : OnOrder.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for OnOrder Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOnOrder NO-UNDO 
        FIELD job-no AS character
        FIELD job-no2 AS Integer 
        FIELD i-no AS CHARACTER   
        FIELD cust-no    AS CHARACTER
        FIELD est-no    AS CHARACTER
        FIELD ord-no AS Integer
        FIELD Startdate LIKE job.start-date 
        FIELD Enddate LIKE job.complete-date
        FIELD Prod Like fg-rdtlh.qty
        FIELD onhandqty       AS INTEGER
        FIELD Cust-part Like oe-ordl.part-no
        FIELD Ord-Qty  like oe-ordl.qty
        FIELD Ship-Qty like oe-ordl.ship-qty
        FIELD InvQty like oe-ordl.inv-qty
        FIELD WP as integer
        FIELD OU as integer
   .
        
                                    
    
DEFINE DATASET dsOnOrder FOR ttOnOrder .

DEFINE QUERY q-OnOrderQuery FOR ttOnOrder.

DEFINE DATA-SOURCE src-OnOrder  FOR QUERY q-OnOrderQuery.

BUFFER ttOnOrder :ATTACH-DATA-SOURCE(DATA-SOURCE src-OnOrder  :HANDLE).
