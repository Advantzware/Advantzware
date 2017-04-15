def var v-des-no like box-design-hdr.design-no init 0.

do while true:
  find first box-design-hdr
      WHERE box-design-hdr.design-no gt v-des-no
      no-lock no-error.

  if not avail box-design-hdr then leave.

  v-des-no = box-design-hdr.design-no.

  for each box-design-hdr
      where box-design-hdr.design-no eq v-des-no
        and box-design-hdr.company   eq {1}.company
	    and box-design-hdr.est-no    eq {1}.est-no
	    and box-design-hdr.form-no   eq {1}.form-no
	    and box-design-hdr.blank-no  eq {1}.blank-no:

    for each box-design-line
	    where box-design-line.design-no eq box-design-hdr.design-no
          and box-design-line.company   eq box-design-hdr.company
	      and box-design-line.est-no    eq box-design-hdr.est-no
	      and box-design-line.form-no   eq box-design-hdr.form-no
	      and box-design-line.blank-no  eq box-design-hdr.blank-no:

      delete box-design-line.
    end.

    delete box-design-hdr.
  end.
end.
