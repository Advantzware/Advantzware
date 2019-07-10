
/*------------------------------------------------------------------------
    File        MachWaste.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE Waste NO-UNDO
    field w-line like job-mch.line
   field w-form-no like mch-act.frm
   field w-blank-no like mch-act.blank-no
   field w-m-code like mch-act.m-code
   field w-i-no   like job-mch.i-no
   field w-dept like job-mch.dept
   field w-run-std as decimal format '>>>>>>9.99'
   field w-run-act as decimal format '>>>>>>9.99'
   field w-run-var as decimal format '->>>>>>9.99'
   field w-mr-std as decimal format '>>>>>>9.99'
   field w-mr-act as decimal format '>>>>>>9.99'
   field w-mr-var as decimal format '->>>>>>9.99'.

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


DEFINE DATASET dsMachWaste FOR Waste .

DEFINE QUERY q-MachWasteQuery FOR Waste.

DEFINE DATA-SOURCE src-MachWaste  FOR QUERY q-MachWasteQuery.

BUFFER Waste :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachWaste  :HANDLE).
