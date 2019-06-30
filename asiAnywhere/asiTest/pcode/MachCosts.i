

/*------------------------------------------------------------------------
    File        MachCostsi
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE MachCosts NO-UNDO
    field c-line like job-mch.line
   field c-form-no like mch-act.frm
   field c-blank-no like mch-act.blank-no
   field c-m-code like mch-act.m-code
   field c-i-no   like job-mch.i-no
   field c-dept like job-mch.dept
   field c-run-std as decimal format '>>>>>>9.99'
   field c-run-act as decimal format '>>>>>>9.99'
   field c-run-var as decimal format '->>>>>>9.99'
   field c-mr-std as decimal format '>>>>>>9.99'
   field c-mr-act as decimal format '>>>>>>9.99'
   field c-mr-var as decimal format '->>>>>>9.99'.

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

DEFINE DATASET dsMachCosts FOR MachCosts .

DEFINE QUERY q-MachCostsQuery FOR MachCosts.

DEFINE DATA-SOURCE src-MachCosts  FOR QUERY q-MachCostsQuery.

BUFFER MachCosts :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachCosts  :HANDLE).
