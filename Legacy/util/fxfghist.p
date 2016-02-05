/* util/fxfghist.p 
   r-no in fg-rcpth and fg-rdtlh should be unique */

for each fg-rcpth no-lock,
    each fg-rdtlh where fg-rdtlh.r-no = fg-rcpth.r-no no-lock
      break by fg-rcpth.r-no DESC BY fg-rcpth.i-no BY fg-rdtlh.tag:
      
      if  first-of(fg-rcpth.r-no) and last-of(fg-rcpth.r-no)
      then .
      else do:
          
          disp fg-rcpth.trans-date fg-rcpth.r-no fg-rcpth.i-no tag form "x(23)"
                fg-rcpth.rec_key fg-rdtlh.rec_key label "FG-Rdtlh_Reckey"
                with width 100 no-box stream-io .
         
      END.
END.
