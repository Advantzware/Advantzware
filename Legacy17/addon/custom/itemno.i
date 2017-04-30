/* itemno.i */

if avail po-ordl then
  assign s-i-no = po-ordl.i-no
         s-i-name = po-ordl.i-name
         s-job-no = po-ordl.job-no
         s-job-no2 = po-ordl.job-no2
         s-recid = recid(po-ordl).
