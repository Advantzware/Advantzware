/* ---------------------------------------------- pc/rep/openjobs.i 04/98 JLF */
/* Open Jobs By Due Date By Customer                                          */
/* -------------------------------------------------------------------------- */

	each job-hdr
	where job-hdr.company eq cocode
	  and job-hdr.job     eq job.job
	  and job-hdr.job-no  eq job.job-no
	  and job-hdr.job-no2 eq job.job-no2
	  and job-hdr.cust-no ge v-fcust[1]
	  and job-hdr.cust-no le v-fcust[2]
	no-lock:

  v-date = job-hdr.due-date.

  RELEASE oe-ord.

  IF job-hdr.ord-no NE 0 THEN
  FIND FIRST oe-ord NO-LOCK
	  WHERE oe-ord.company eq job-hdr.company
	    AND oe-ord.ord-no  eq job-hdr.ord-no
	  NO-ERROR.

  IF AVAIL oe-ord THEN DO:
    EMPTY TEMP-TABLE tt-rel.

    FOR EACH oe-rel
	    WHERE oe-rel.company EQ job-hdr.company
	      AND oe-rel.ord-no  EQ job-hdr.ord-no
	      AND oe-rel.link-no EQ 0
	      AND oe-rel.i-no    EQ job-hdr.i-no
	    USE-INDEX ord-item NO-LOCK:

      {oe/rel-stat.i lv-stat}

      CREATE tt-rel.
      tt-rel.rel-date = IF AVAIL oe-relh THEN oe-relh.rel-date
                                         ELSE oe-rel.rel-date.
    END.

    FOR EACH tt-rel BY tt-rel.rel-date:
      LEAVE.
    END.

    if avail tt-rel and tt-rel.rel-date ne ? then v-date = tt-rel.rel-date.

    else do:
	  find first oe-ordl
	      where oe-ordl.company eq job-hdr.company
	        and oe-ordl.ord-no  eq job-hdr.ord-no
	        and oe-ordl.i-no    eq job-hdr.i-no
	      no-lock no-error.
	  if avail oe-ordl and oe-ordl.req-date ne ? then
	    v-date = oe-ordl.req-date.

  	  else
	  if avail oe-ordl and oe-ordl.prom-date ne ? then
	    v-date = oe-ordl.prom-date.

	  else 
	  if avail oe-ord and oe-ord.due-date ne ? then v-date = oe-ord.due-date.
    end.
  END.

  if v-date lt v-fdate[1] or
	 v-date gt v-fdate[2] or
	 v-date eq ?          then next.

  find first cust of job-hdr no-lock no-error.
  create tt-report.
  assign
   tt-report.key-01  = string(year(v-date),"9999") +
	                   string(month(v-date),"99")  +
			           string(day(v-date),"99")
   tt-report.key-02  = if avail cust then cust.name else job-hdr.cust-no
   tt-report.rec-id  = recid(job-hdr).

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */

