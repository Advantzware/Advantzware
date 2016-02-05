
if v-delete then
for each stack-flute
    where stack-flute.company eq cocode
      and stack-flute.code    ge fflute
      and stack-flute.code    le tflute:
  delete stack-flute.
end.

for each item
    where item.company  eq cocode
      and item.mat-type eq "B"
      and item.flute    ge fflute
      and item.flute    le tflute
      and item.flute    ne ""
      and item.reg-no   ge ftest
      and item.reg-no   le ttest
      and item.reg-no   ne ""
      and item.cal      ne 0
    no-lock use-index mat-type

    break by item.flute
          by item.reg-no:

  if first-of(item.reg-no) then do:
    find first reftable
        where reftable.reftable eq "FLUTE"
          and reftable.company  eq ""
          and reftable.code     eq item.flute
        no-lock no-error.

    if avail reftable then
    for each b-item
        where b-item.company  eq cocode
          and b-item.mat-type eq "D"
          and b-item.i-no     ge fpall
          and b-item.i-no     le tpall
        use-index mat-type:

      status default " Processing...    Flute: " + trim(item.flute) +
                                     "  Pallet: " + b-item.i-no.

      find first stack-flute
          where stack-flute.company eq cocode
            and stack-flute.loc     eq locode
            and stack-flute.code    eq reftable.code
            and stack-flute.pallet  eq b-item.i-no
          no-error.
      if not avail stack-flute then do:
        {sys/ref/stack-fl.a}
        stack-flute.pallet = b-item.i-no.
      end.

      do i = 1 to 15:
        if stack-flute.row-value[i] eq ""          or
           stack-flute.row-value[i] eq item.reg-no then do:

          stack-flute.row-value[i] = item.reg-no.

          do j = 1 to 10:
            if stack-flute.col-value[j]       ne "" and
               stack-flute.vals[(i * 10) + j] eq 0  then do:

              find first b-reftable
                  where b-reftable.reftable eq "STACK"
                    and b-reftable.company  eq ""
                    and b-reftable.code     eq stack-flute.col-value[j]
                  no-lock no-error.
              if avail b-reftable then
                assign
                 stack-flute.vals[(i * 10) + j] =
                                    (b-item.case-d - v-height) / (item.cal * 2)
                 stack-flute.vals[(i * 10) + j] =
                                    (stack-flute.vals[(i * 10) + j] -
                                     stack-flute.vals[(i * 10) + j] modulo 5) *
                                    b-reftable.val[1].
            end.
          end.

          leave.
        end.
      end.
    end.
  end.
end.
