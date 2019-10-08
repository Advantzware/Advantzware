
for each reftable
    where reftable.reftable eq "JOB-HDR01" + job.company
      and reftable.code2    eq string(job-hdr.j-no,"9999999999")
    use-index code2:
    
  delete reftable.
end.

for each reftable
    where reftable.reftable eq "JOB-HDR02" + job.company
      and reftable.code2    eq string(job-hdr.j-no,"9999999999")
    use-index code2:
    
  delete reftable.
end.

for each reftable
    where reftable.reftable eq "JOB-HDR03" + job.company
      and reftable.code2    eq string(job-hdr.j-no,"9999999999")
    use-index code2:
    
  delete reftable.
end.

for each reftable
    where reftable.reftable eq "JOB-HDR04" + job.company
      and reftable.code2    eq string(job-hdr.j-no,"9999999999")
    use-index code2:
    
  delete reftable.
end.
