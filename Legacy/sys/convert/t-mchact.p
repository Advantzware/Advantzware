def var i as int no-undo.
DEF STREAM st-out .
def temp-table tt-act like mch-act.

input from c:\temp\mch-act.dc2 no-echo.


repeat:
    create tt-act.
    set 
       tt-act.m-code                 
tt-act.qty                       form ">>>>>>>>>>9-"
tt-act.i-name                    
tt-act.i-no                      
tt-act.j-no                      
tt-act.job-no                    
tt-act.job-no2                   
tt-act.frm                       form ">>>>9"
tt-act.pass                      form ">>>>9"
tt-act.hours                     form ">>>>>>>>9.99-"
tt-act.speed                     form ">>>>>>9"
tt-act.blank-no                  form ">>>9"
tt-act.job                       form ">>>>>>>>>9"
tt-act.shift                     form ">>>9"
tt-act.crew                      form ">>99.9<<"
tt-act.complete                  
tt-act.notes                     form "x(55)"
tt-act.op-date                   
tt-act.company                   
tt-act.code                      
tt-act.waste                     form ">>>>>>>>9-"
tt-act.dept                      
tt-act.opn                       
tt-act.op-time                   form "->>>>>>,>>>,>>9"
tt-act.start                     form ">>>>>>>,>>9"
tt-act.stopp                     form "->>>>>,>>>,>>9"
tt-act.bc-m-code                 
tt-act.bc-job                    
tt-act.bc-i-no                   
tt-act.bc-code                   
tt-act.bc-crew-id                
tt-act.bc-emp-id                 
tt-act.n-out                     form "->>>>>,>>>,>>9"
tt-act.user-id                   
tt-act.total-rate                form ">>>>>>>>9.99<<<"
tt-act.factor                    form ">>>>>>>>9.99<<<"
tt-act.rate                      form ">>>>>>>>9.99<<<"
tt-act.emp-id                    
tt-act.crew-id                   
tt-act.rec_key                   .

                   
    find first mch-act use-index dly-idx where
        mch-act.company = tt-act.company
        and mch-act.m-code = tt-act.m-code
        and mch-act.op-date = tt-act.op-date
        and mch-act.shift = tt-act.shift
        and mch-act.job-no = tt-act.job-no
        and mch-act.job-no2 = tt-act.job-no2
        and mch-act.job = tt-act.job
        and mch-act.pass = tt-act.pass
        and mch-act.frm = tt-act.frm
        and mch-act.blank-no = tt-act.blank-no
        and mch-act.start = tt-act.start
        and mch-act.stopp = tt-act.stopp no-lock no-error.
        if not avail mch-act then do:
            create mch-act.
            buffer-copy tt-act to mch-act.
        end.

    i = i + 1.


        disp i tt-act.m-code tt-act.job-no tt-act.op-date tt-act.start with frame dis down.
        down with frame dis.
        pause 0.
        
end.
