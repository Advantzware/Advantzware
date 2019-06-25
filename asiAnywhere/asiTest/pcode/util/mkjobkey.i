
{util/dljobkey.i}

create reftable.
assign
 reftable.reftable = "JOB-HDR01" + job.company
 reftable.company  = string(job.opened,"O/C")
 reftable.loc      = job-hdr.job-no
 reftable.code     = string(job-hdr.job-no2,"9999999999")
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR01" + job.company
 reftable.company  = "A"
 reftable.loc      = job-hdr.job-no
 reftable.code     = string(job-hdr.job-no2,"9999999999")
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR02" + job.company
 reftable.company  = string(job.opened,"O/C")
 reftable.loc      = string(job-hdr.ord-no ,">>>>>>")
 reftable.code2    = string(job-hdr.j-no,"9999999999").

RELEASE reftable.
   
create reftable.
assign
 reftable.reftable = "JOB-HDR02" + job.company
 reftable.company  = "A"
 reftable.loc      = string(job-hdr.ord-no ,">>>>>>")
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR03" + job.company
 reftable.company  = string(job.opened,"O/C")
 reftable.loc      = job-hdr.est-no
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR03" + job.company
 reftable.company  = "A"
 reftable.loc      = job-hdr.est-no
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR04" + job.company
 reftable.company  = string(job.opened,"O/C")
 reftable.loc      = job-hdr.i-no
 reftable.code2    = string(job-hdr.j-no,"9999999999").
   
RELEASE reftable.

create reftable.
assign
 reftable.reftable = "JOB-HDR04" + job.company
 reftable.company  = "A"
 reftable.loc      = job-hdr.i-no
 reftable.code2    = string(job-hdr.j-no,"9999999999").
 
RELEASE reftable.
