
for each {2}account
    where {2}account.company eq {1}
      and {2}account.actnum  ne gl-ctrl.contra:
        
  if "{2}" eq "" then
    status default "Processing account: " + trim({2}account.actnum).
  
  do i = 1 to company.num-per:
    assign
     {2}account.cyr[i] = {2}account.lyr[i]
     {2}account.lyr[i] = 0.
     
    find first period
        where period.company eq {1}
          and period.yr      eq fisc-yr - 1
          and period.pnum    eq i
          and period.pstat   eq no
        no-lock no-error.
        
    if avail period then
    for each glhist
        where glhist.company eq {1}
          and glhist.actnum  eq {2}account.actnum
          and glhist.tr-date ge period.pst
          and glhist.tr-date le period.pend
          and glhist.period  eq period.pnum
        no-lock:
       
      {2}account.lyr[i] = {2}account.lyr[i] + glhist.tr-amt.    
    end.
  end.
  
  {2}account.cyr-open = {2}account.lyr-open.
   
  if index("ALCT",{2}account.type) ne 0 then
  do i = 1 to company.num-per:
    {2}account.lyr-open = {2}account.lyr-open - {2}account.lyr[i].
  end.
  
  else {2}account.lyr-open = 0.
end.

find first {2}account
    where {2}account.company eq {1}
      and {2}account.actnum  eq gl-ctrl.contra.
      
if "{2}" eq "" then
  status default "Processing account: " + trim({2}account.actnum).
  
assign
 {2}account.cyr = 0
 {2}account.lyr = 0.
       
for each b-{2}acc
    where b-{2}acc.company eq {1}
      and b-{2}acc.actnum  ne gl-ctrl.contra
      and index("RE",b-{2}acc.type) ne 0
    no-lock:
      
  do i = 1 to company.num-per:
    assign
     {2}account.cyr[i] = {2}account.cyr[i] - b-{2}acc.cyr[i]
     {2}account.lyr[i] = {2}account.lyr[i] - b-{2}acc.lyr[i].
  end.
end.
  
find first b-{2}acc
    where b-{2}acc.company eq {1}
      and b-{2}acc.actnum  eq gl-ctrl.ret.
        
b-{2}acc.lyr-open = b-{2}acc.cyr-open.
 
do i = 1 to company.num-per:
  assign
   b-{2}acc.lyr[i]   = b-{2}acc.lyr[i] - {2}account.lyr[i]
   b-{2}acc.lyr-open = b-{2}acc.lyr-open - b-{2}acc.lyr[i].
end.

