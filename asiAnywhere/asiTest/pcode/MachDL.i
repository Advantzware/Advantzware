
/*------------------------------------------------------------------------
    File        MachDL.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE MachDL NO-UNDO
    field d-line like job-mch.line
   field d-form-no like mch-act.frm
   field d-blank-no like mch-act.blank-no
   field d-m-code like mch-act.m-code
   field d-i-no   like job-mch.i-no
   field d-dept like job-mch.dept
   field d-run-std as decimal format '>>>>>>9.99'
   field d-run-act as decimal format '>>>>>>9.99'
   field d-run-var as decimal format '->>>>>>9.99'
   field d-mr-std as decimal format '>>>>>>9.99'
   field d-mr-act as decimal format '>>>>>>9.99'
   field d-mr-var as decimal format '->>>>>>9.99'.

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


DEFINE DATASET dsMachDL FOR MachDL .

DEFINE QUERY q-MachDLQuery FOR MachDL.

DEFINE DATA-SOURCE src-MachDL  FOR QUERY q-MachDLQuery.

BUFFER MachDL :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachDL  :HANDLE).
