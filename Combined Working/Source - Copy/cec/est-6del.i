/* -------------------------------------------------- cec/est-6del.i 08/97 JLF */
/* Cost Estimating - DELETE box designs                                       */
/* -------------------------------------------------------------------------- */

    for each box-design-hdr where box-design-hdr.design-no = 0 and
                              box-design-hdr.company = xeb.company 
                          and box-design-hdr.est-no = xeb.est-no
	  and box-design-hdr.form-no   eq xeb.form-no
	  and box-design-hdr.blank-no  eq xeb.blank-no:
      for each box-design-line of box-design-hdr:
	delete box-design-line.
      end.
      delete box-design-hdr.
    end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
