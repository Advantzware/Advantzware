
put "*** T O T A L S     in Cost/M ".

do v = {1} to vhld + 4:
  if qtty[v] ne 0 then 
    if qtty[v] gt 99999999 then put qtty[v] format ">>>>>>>>>9".
                           else put qtty[v] format ">>,>>>,>>9".
end.

put skip
    space(17)
    "# of Releases" format "x(13)".

do v = {1} to vhld + 4:
  if qtty[v] ne 0 then put rels[v]          format ">>,>>>,>>9".
end.

put skip
    fill("-",30) format "x(30)".

do v = {1} to vhld + 4:
  if qtty[v] ne 0 then  put unformatted fill("-",10) format "x(10)".
end.

put skip.

mclean-loop:
for each mclean:
   
  if mclean.rec-type eq ""            or
     (not mclean.descr begins "    ") then v-skip-pct = no.
      
  if v-skip-pct then next.

  do v = {1} to vhld + 4:
    if mclean.cost[v] ne 0 then leave.
    if v ge vhld + 4 then next mclean-loop.
  end.

  if mclean.rec-type   ne ""          and
     (not mclean.descr begins "    ") then do:
         
    v-skip-pct = yes.
    find first bmclean
        where bmclean.rec-type eq mclean.rec-type
          and bmclean.descr    begins "    "
        no-error.

    if avail bmclean then
    do v = {1} + 1 to vhld + 4:
      if bmclean.cost[v] ne bmclean.cost[v - 1] and
         bmclean.cost[v] ne 0                   then do:
        v-skip-pct = no.
        leave.
      end.
    end.
    if v-skip-pct and avail bmclean then
      put trim(mclean.descr) + " - " +
          trim(string(bmclean.cost[{1}],"->>9.99%")) format "x(30)".
    else
      put mclean.descr.
  end.

  else
    put mclean.descr.

  do v = {1} to vhld + 4:
    if mclean.cost[{1}]     ne 0 or
       mclean.cost[{1} + 1] ne 0 or
       mclean.cost[{1} + 2] ne 0 or
       mclean.cost[{1} + 3] ne 0 or
       mclean.cost[{1} + 4] ne 0 then
           
      if mclean.cost[v] eq 0 then
        put space(10).
      else
        put mclean.cost[v].
  end.

  put skip.
end.
      
