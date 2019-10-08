
assign
 {1} = ""
 {2} = ""
 {3} = "LABB4P],LABEL,LABEL2,LABEL3,LABEL5,LABEL6,LABELB,LABXC,"  + /* A */
       "POTDEV,POTDV1,POTDV2,POTDV3,POTDV4,POTDV5,"               +
       "BOBST1,BOBST2,BOBST3,BOBST4,BOBSXC,MIEHLE,MIEHXC,"        + /* B */
       "COMPAC,COMPC1,COMPXC,"                                    + /* C */
       "5CLRXC,5COLOR,CUIR,CUIRXC,"                               + /* F */
       "GOPF1,GOPFXC,"                                            + /* G */
       "GLUER,GLUER2,GLUER3,JLGLU1,JLGLU2,JLGLUR,JLGLXC,POST,"    + /* J */
       "LANGDC,LANGS,LANGS1,LANGS2,LANGS3,LANGS4,LANGS5,LNGDC,"   + /* L */
       "MINI,MINIGL,"                                             + /* M */
       "ROTARY,"                                                  + /* R */
       "BOXMK3,BOXMKR,BXMKXC,RESTCK,SLITER,SLITR2,SLITR3,SLITXC," + /* S */
       "SLOTER,STITCH,THDIXC,THMPXC,THDIAG,THOMPS,TITAN,"         +
       "WARD-2,WARD-4,WRD-DC,WRD2XC,WRD4XC"                         /* W */
 {4} = "A,A,A,A,A,A,A,A,A,A,A,A,A,A,"   +
       "B,B,B,B,B,B,B,"                 +
       "C,C,C,"                         +
       "F,F,F,F,"                       +
       "G,G,"                           +
       "J,J,J,J,J,J,J,J,"               +
       "L,L,L,L,L,L,L,L,"               +
       "M,M,"                           +
       "R,"                             +
       "S,S,S,S,S,S,S,S,S,S,S,S,S,S,S," +
       "W,W,W,W,W".

for first job
    where job.company eq po-ordl.company
      and job.job-no  eq po-ordl.job-no
      and job.job-no2 eq po-ordl.job-no2
    no-lock,
    
    first job-mch
    where job-mch.company            eq job.company
      and job-mch.job                eq job.job
      and job-mch.job-no             eq job.job-no
      and job-mch.job-no2            eq job.job-no2
      and job-mch.frm                eq po-ordl.s-num
      and (job-mch.dept              ne "DM" and
           job-mch.dept              ne "PM")
      and can-do({3},job-mch.m-code)
    use-index line-idx no-lock:
    
  assign
   {1} = job-mch.m-code
   {2} = entry(lookup(job-mch.m-code,{3}),{4}).
end.
    
