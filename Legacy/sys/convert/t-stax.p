/* stax import program */
/*input from r:\tmp\stax.d no-echo.

repeat:
   create stax.
   update stax.tax-group form "x(15)"
          company
          tax-code
          tax-code1
          tax-dscr
          tax-dscr1
          tax-rate
          tax-rate1
          tax-acc
          tax-acc1
          tax-frt
          tax-frt1.
          
end.
*/

DEF TEMP-TABLE tt-stax FIELD rec-id AS RECID
                       FIELD company AS cha
                       FIELD tax-group AS cha
                       FIELD tax-dscr AS cha
                       FIELD accum-tax AS LOG.

for each stax:
    /*
  assign stax.accum-tax = if stax.company = "Yes" then yes else no
         stax.company = substring(stax.tax-group,1,10)
         stax.tax-group = substring(stax.tax-group,11,length(trim(stax.tax-group)) - 10)
         .
  find first stax-group where stax-group.company = stax.company and
                               stax-group.tax-group = stax.tax-group
                               no-lock no-error.
   if not avail stax-group then do:
      create stax-group.
      assign stax-group.company = stax.company
             stax-group.tax-group = stax.tax-group
             stax-group.tax-dscr = stax.tax-group.
             
   end.
    
   */
    CREATE tt-stax.
    ASSIGN tt-stax.accum-tax = stax.company EQ "yes"
           tt-stax.company = SUBSTRING(stax.tax-group,1,3)
           tt-stax.tax-group = SUBSTRING(stax.tax-group,11,3)           
           tt-stax.rec-id = RECID(stax).
end.
          
FOR EACH tt-stax:
    FIND stax WHERE recid(stax) = tt-stax.rec-id.
    ASSIGN stax.company = tt-stax.company
           stax.tax-group = tt-stax.tax-group
           stax.tax-dscr =  tt-stax.tax-group.
           stax.accum-tax = tt-stax.accum-tax.
END.
   
