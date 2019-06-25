
/*------------------------------------------------------------------------
    File        MachHr.i
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
   field v-run-hr like job-mch.run-hr
   field v-run-act like job-mch.run-hr
   field v-run-var as dec format '->>9.99'
   field v-mr-hr like job-mch.mr-hr
   field v-mr-act like job-mch.mr-hr
   field v-mr-var as dec format '->>9.99'.


DEFINE TEMP-TABLE x-mch NO-UNDO 
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field dept like job-mch.dept
   field run-hr like job-mch.run-hr
   field run-act like job-mch.run-hr
   field run-var as dec format '->>9.99'
   field mr-hr like job-mch.mr-hr
   field mr-act like job-mch.mr-hr
   field mr-var as dec format '->>9.99'
   field est-speed like job-mch.speed
   field act-qty as dec.



DEFINE DATASET dsMachHr FOR mch .

DEFINE QUERY q-MachHrQuery FOR mch.

DEFINE DATA-SOURCE src-MachHr  FOR QUERY q-MachHrQuery.

BUFFER mch :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachHr  :HANDLE).
