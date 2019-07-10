
/*------------------------------------------------------------------------
    File        MachVarOH.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE VarOH NO-UNDO
    field V-line like job-mch.line
   field V-form-no like mch-act.frm
   field V-blank-no like mch-act.blank-no
   field V-m-code like mch-act.m-code
   field V-i-no   like job-mch.i-no
   field V-dept like job-mch.dept
   field V-run-std as decimal format '>>>>>>9.99'
   field V-run-act as decimal format '>>>>>>9.99'
   field V-run-var as decimal format '->>>>>>9.99'
   field V-mr-std as decimal format '>>>>>>9.99'
   field V-mr-act as decimal format '>>>>>>9.99'
   field V-mr-var as decimal format '->>>>>>9.99'.

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


DEFINE DATASET dsMachVarOH FOR VarOH .

DEFINE QUERY q-MachVarOHQuery FOR VarOH.

DEFINE DATA-SOURCE src-MachVarOH  FOR QUERY q-MachVarOHQuery.

BUFFER VarOH :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachVarOH  :HANDLE).
