DEF TEMP-TABLE work-tmp NO-UNDO
   field job like job.job
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   FIELD sort-field AS CHAR
   field dept as char format 'xx'
   field m-code like mach.m-code
   field sch-m-code LIKE mach.m-code
   field pass like job-mch.pass
   field r-act-hrs as dec format '>>>>9.99'
   field m-act-hrs as dec format '>>>>9.99'
   field dt-chg-hrs as dec format '>>>>9.99'
   field dt-nochg-hrs as dec format '>>>>9.99'
   field qty as dec format '>>>>>>>>9'
   field msf as dec format '>>>>>>.999'
   FIELD qty-fg-rec AS DEC
   FIELD msf-fg-rec AS DEC
   FIELD qty-sheets AS DEC
   FIELD msf-sheets AS DEC
   FIELD qty-scrap-rec AS DEC
   FIELD msf-scrap-received AS DEC
   FIELD shift-sort AS CHAR
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD est-no AS INT
   FIELD job-hr-total AS DEC
   FIELD tot-mr-hours AS DEC
   FIELD i-no AS CHAR 
   FIELD cust-no AS CHAR 
   FIELD i-name AS CHARACTER  
   INDEX idx1 m-code shift-sort job job-no job-no2 frm blank-no
   INDEX work-tmp job frm blank-no dept m-code pass sort-field
   INDEX job job-no job-no2.

DEF TEMP-TABLE work-rep NO-UNDO
   FIELD sort-field AS CHAR
   FIELD dept as char format 'xx'
   FIELD m-code like mach.m-code
   FIELD sch-m-code LIKE mach.m-code
   FIELD no-jobs as int
   FIELD no-setups AS INT
   FIELD r-std-hrs as dec format '>>>>9.99'
   FIELD r-act-hrs as dec format '>>>>9.99'
   FIELD m-std-hrs as dec format '>>>>9.99'
   FIELD m-act-hrs as dec format '>>>>9.99'
   FIELD dt-chg-hrs as dec format '>>>>9.99'
   FIELD dt-nochg-hrs as dec format '>>>>9.99'
   FIELD qty as dec format '>>>>>>>>9'
   FIELD msf as dec format '>>>>>>.999'
   FIELD qty-fg-rec AS DEC
   FIELD msf-fg-rec AS DEC
   FIELD qty-scrap-rec AS DEC
   FIELD msf-scrap-received AS DEC
   FIELD perc-total-scrap AS DEC
   FIELD qty-sheets AS DEC
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD cust-no AS CHARACTER
   FIELD i-name AS CHARACTER
   FIELD i-no AS CHAR
   INDEX work-rep sort-field dept m-code.

 DEF TEMP-TABLE work-rep-copy NO-UNDO LIKE work-rep.

