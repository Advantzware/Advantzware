
/*------------------------------------------------------------------------
    File        MachQty.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE mch NO-UNDO
    field v-line like job-mch.line
   field v-form-no like mch-act.frm
   field v-blank-no like mch-act.blank-no
   field v-m-code like mch-act.m-code
   field v-i-no   like job-mch.i-no
   field v-dept like job-mch.dept
   field v-run-std as decimal format '>>>>>>9.99'
   field v-run-act as decimal format '>>>>>>9.99'
   field v-run-var as decimal format '->>>>>>9.99'
   field v-mr-std as decimal format '>>>>>>9.99'
   field v-mr-act as decimal format '>>>>>>9.99'
   field v-mr-var as decimal format '->>>>>>9.99'.

DEFINE TEMP-TABLE x-mch NO-UNDO 
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field dept like job-mch.dept
   field run-std as decimal format '>>>>>>9.99'
   field run-act as decimal format '>>>>>>9.99'
   field run-var as decimal format '->>>>>>9.99'
   field mr-std as decimal format '>>>>>>9.99'
   field mr-act as decimal format '>>>>>>9.99'
   field mr-var as decimal format '->>>>>>9.99'
   field std-hrs as dec
   field run-hrs as dec
   field act-qty as dec
   field wst-qty as dec
   field wst-prct as dec
   field est-speed as dec.


DEFINE DATASET dsMachQty FOR mch .

DEFINE QUERY q-MachQtyQuery FOR mch.

DEFINE DATA-SOURCE src-MachQty  FOR QUERY q-MachQtyQuery.

BUFFER mch :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachQty  :HANDLE).
