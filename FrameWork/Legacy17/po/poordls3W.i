
if avail job-mat then
find first job
    where job.company eq job-mat.company
      and job.job     eq job-mat.job
      and job.job-no  eq job-mat.job-no
      and job.job-no2 eq job-mat.job-no2
    no-lock no-error.
     
if avail job then
find first est
    where est.company eq job.company
      AND est.est-no  EQ job.est-no
    no-lock no-error.
     
if avail est then
find first ef
    where ef.company eq est.company
      AND ef.est-no  EQ est.est-no
      and ef.form-no eq job-mat.frm
    no-lock no-error.
    
if avail ef then
find first eb
    where eb.company eq ef.company
      AND eb.est-no  EQ ef.est-no
      and eb.form-no eq ef.form-no
    no-lock no-error.

if avail eb then
find first item
    {sys/look/itemgsW.i}
      and item.i-no eq eb.adhesive
    no-lock no-error.

if avail eb then
find first style
    where style.company eq eb.company
      and style.style   eq eb.style
      and LOOKUP(style.type,"D,R,P") NE 0
    no-lock no-error.
  
if avail style then
find first box-design-hdr
    where box-design-hdr.company   eq style.company
      and box-design-hdr.design-no eq style.design-no
      AND box-design-hdr.est-no = eb.est-no
    no-lock no-error.

IF AVAIL style AND NOT AVAIL box-design-hdr THEN
find first box-design-hdr
    where box-design-hdr.company   eq style.company
      and box-design-hdr.design-no eq style.design-no
    no-lock no-error.
