
/*------------------------------------------------------------------------
    File        MachFixOH.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE FixOH NO-UNDO
    field f-line like job-mch.line
   field f-form-no like mch-act.frm
   field f-blank-no like mch-act.blank-no
   field f-m-code like mch-act.m-code
   field f-i-no   like job-mch.i-no
   field f-dept like job-mch.dept
   field f-run-std as decimal format '>>>>>>9.99'
   field f-run-act as decimal format '>>>>>>9.99'
   field f-run-var as decimal format '->>>>>>9.99'
   field f-mr-std as decimal format '>>>>>>9.99'
   field f-mr-act as decimal format '>>>>>>9.99'
   field f-mr-var as decimal format '->>>>>>9.99'.

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


DEFINE DATASET dsMachFixOH FOR FixOH .

DEFINE QUERY q-MachFixOHQuery FOR FixOH.

DEFINE DATA-SOURCE src-MachFixOH  FOR QUERY q-MachFixOHQuery.

BUFFER FixOH :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachFixOH  :HANDLE).
