 /* stax.i */
 
 if {1}:MODIFIED AND {1}:SCREEN-VALUE <> "" then do:
       find first stax where /*stax.tax-group = g_company 
                      and substr(stax.tax-group,1,10) = g_company
                      and substri(stax.tax-group,11, length(trim(stax.tax-group)) - 10)
                          = oe-ord.tax-gr:screen-value old */
                          stax.company = g_company and
                          stax.tax-group = {1}:screen-value
                          no-lock no-error.
       if not avail stax then do:
          message "Invalid Tax Group. Try help. " view-as alert-box error.
          APPLY "entry" TO {1}.
          return no-apply.
       end.
 end.   
