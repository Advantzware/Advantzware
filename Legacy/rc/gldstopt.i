def var distrib_summary as logical label "Print GL Summary?" no-undo.
def workfile wkdistrib /* no-undo 9810 CAH should UNDO */
field wk_jsc like gljsc.jsc
field wk_acct like glacct.acct
field wk_amount as decimal format "(>>>,>>>,>>>.99)".
{1}  /* 9712 CAH */
if login_group = "field" then distrib_summary = true.
else do:
update
    distrib_summary
        help "Enter Yes to print a summary by GL Distribution"
    with frame f-gldstopt center side-labels  title "GL DISTRIBUTION SUMMARY?"
    color value(c_pop).
end.    
if distrib_summary then for each wkdistrib: delete wkdistrib. end.
