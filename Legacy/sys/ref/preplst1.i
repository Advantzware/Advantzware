/* --------------------------------------------- sys/ref/preplst1.i 07/97 JLF */
/* Prep List by Material Type                                                 */
/* -------------------------------------------------------------------------- */

  for each eb
	  where eb.company              eq cocode
	    and eb.loc                  eq locode
	    and eb.{1}-no               eq prep.code

	    and eb.cust-no              ge fcus
	    and eb.cust-no              le tcus

	    and eb.sman                 ge fsmn
	    and eb.sman                 le tsmn

	  use-index {1} no-lock,

	  EACH est
	  where est.company             eq cocode
	    and est.loc                 eq locode
	    and est.est-no              eq eb.est-no

	    and est.mod-date            ge fdat
	    and est.mod-date            le tdat

	  use-index est-no no-lock

	  by est.mod-date

	  with frame {1} no-box no-labels stream-io width 132.

	{sys/ref/preplist.i}
  end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */

