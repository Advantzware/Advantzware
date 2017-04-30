/* -------------------------------------------------- sys/ref/mach.v 2/92 cd  */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

v-label1 = if mach.dept[1] eq "HS"
             then "Convert Production from Blanks to Sheets?"
             else "Use Lineal Feet in RUN Matrix?".
             
display v-label /*v-label1 */ .

head[4] = (if mach.p-type eq "B"                then "Blanks" else 
           if mach.therm and mach.p-type eq "R" then "Lin.Ft" else "Sheets") +
          "/Run:".

display
  mach.m-code
  mach.m-dscr
  mach.industry
  entry(int(mach.industry) + 1,",F,C") when mach.industry le "9" @ mach.industry
  mach.dept[1]
  mach.dept[2]
  mach.dept[3]
  mach.dept[4]
  mach.loc
  mach.d-seq
  mach.m-seq
  mach.therm
  head[1]
  mach.run-spoil
  mach.mr-waste
  mach.p-type
  mach.mr-rate
  mach.mr-varoh
  mach.mr-fixoh
  mach.mr-trate
  mach.run-rate
  mach.run-varoh
  mach.run-fixoh
  mach.run-trate
  mach.mrk-rate
  mach.lab-drate
  mach.mr-crusiz
  mach.run-crusiz
  mach.lab-rate[1 for 3]
  head[2] 
  mach.min-len
  mach.max-len
  mach.min-wid
  mach.max-wid
  mach.min-triml
  mach.min-trimw
  mach.min-cal
  mach.max-cal
  mach.min-pan-l
  mach.min-pan-w
  head[4] FORM "X(20)"
  mach.min-run
  mach.max-run
  mach.num-len when mach.num-len ne 0
  mach.num-wid when mach.num-wid ne 0.
   
if mach.dept[1] eq "PR" or mach.dept[2] eq "PR" or
   mach.dept[3] eq "PR" or mach.dept[4] eq "PR" then do:
   
  head[3] = "==========  Printing Press  ===========" .
  display
    head[3]
    mach.pr-type
    mach.washup
    mach.col-pass
    mach.max-color
    mach.coater
    mach.col-wastesh
    mach.ink-waste
    mach.col-wastelb
    mach.tan-mrp
    mach.tan-mrf.
end.

else
if mach.dept[1] eq "GL" or mach.dept[2] eq "GL" or
   mach.dept[3] eq "GL" or mach.dept[4] eq "GL" then do:
   
  head[3] = "==== M/R Additional Items - Tandem ====".
  display
    head[3]
    mach.tan-mrp
    mach.tan-mrf.
end.

else do:
  head[3] = "==========  Printing Press  ===========".
  display
    head[3]
    "" @ mach.pr-type
    "" @ mach.washup
    "" @ mach.col-pass
    "" @ mach.max-color
    "" @ mach.coater
    "" @ mach.col-wastesh
    "" @ mach.ink-waste
    "" @ mach.col-wastelb
    "" @ mach.tan-mrp
    "" @ mach.tan-mrf.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
