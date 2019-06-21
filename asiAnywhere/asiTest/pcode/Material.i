
/*------------------------------------------------------------------------
    File        Material.i
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
   field updatable as log init no
   field qty-std as decimal format '>>>>>9.9'
   field qty-act as decimal format '>>>>>9.9'
   field qty-var as decimal format '->>>>>9.9'
   field cst-std as decimal format '>>>>>>9.99'
   field cst-act as decimal format '>>>>>>9.99'
   field cst-var as decimal format '->>>>>>9.99'
   field basis-w as dec
   field len as dec
   field wid as dec
   field cst-uom like job-mat.sc-uom
   INDEX mat form-no blank-no rm-i-no.





DEFINE DATASET dsMaterial FOR mat .

DEFINE QUERY q-MaterialQuery FOR mat.

DEFINE DATA-SOURCE src-Material  FOR QUERY q-MaterialQuery.

BUFFER mat :ATTACH-DATA-SOURCE(DATA-SOURCE src-Material  :HANDLE).
