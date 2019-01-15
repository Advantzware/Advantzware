FOR EACH job-code

    WHERE job-code.dmiID EQ 0:

    job-code.dmiID = NEXT-VALUE(jobCodeDMIseq).

END. /* each job-code */