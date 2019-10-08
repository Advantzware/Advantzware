define TEMP-TABLE mch NO-UNDO
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   FIELD pass LIKE mch-act.pass
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field dept like job-mch.dept
   field run-std as decimal format '>>>>>>9.99'
   field run-act as decimal format '>>>>>>9.99'
   field run-var as decimal format '->>>>>>9.99'
   field mr-std as decimal format '>>>>>>9.99'
   field mr-act as decimal format '>>>>>>9.99'
   field mr-var as decimal format '->>>>>>9.99'.

define TEMP-TABLE x-mch NO-UNDO
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   FIELD pass LIKE mch-act.pass
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
   field std-mr-hrs as dec
   field run-hrs as dec
   field act-qty as dec
   field wst-qty as dec
   field wst-prct as dec
   field est-speed as dec.
