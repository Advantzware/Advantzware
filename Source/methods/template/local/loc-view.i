/* loc-view.i   for local view */

&if defined(LOC-VIEW) &then
    &IF "{&LOC-VIEW}" eq "est-spec" &THEN
         run dispatch ('open-query').
    &endif  

&endif
