/* cecrep/tt-artios.i*/
DEF {1} SHARED TEMP-TABLE tt-artios FIELD tt-job-no LIKE job-hdr.job-no
                                   FIELD tt-job-no2 LIKE job-hdr.job-no2
                                   FIELD tt-frm LIKE job-hdr.frm
                                   FIELD tt-blank LIKE job-hdr.blank-no
                                   FIELD tt-see-print AS LOG
                                   FIELD tt-see-dept AS LOG
                                   FIELD tt-see-plain AS LOG
                                   FIELD tt-black-only AS LOG.
