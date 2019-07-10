
/*------------------------------------------------------------------------
    File        MatInfo.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE mat NO-UNDO 
    field form-no like job-mat.frm 
    field blank-no like job-mat.blank-no
    field rm-i-no like job-mat.rm-i-no  
    field i-name like item.i-name  
    field q-onh like item.q-onh 
    field q-comm like item.q-comm
    field q-avail like item.q-avail
    field qty-all like po-all.qty-all
    field due-date like po-ordl.due-date
    .

  
DEFINE DATASET dsMatInfo FOR mat .

DEFINE QUERY q-MatInfoQuery FOR mat.

DEFINE DATA-SOURCE src-MatInfo  FOR QUERY q-MatInfoQuery.

BUFFER mat :ATTACH-DATA-SOURCE(DATA-SOURCE src-MatInfo  :HANDLE).
