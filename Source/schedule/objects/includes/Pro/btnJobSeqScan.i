/* btnJobSeqScan.i - trigger for btnJobSeqScan in board.w */

RUN asiCommaList IN containerHandle ("Company",OUTPUT commaList).
RUN VALUE(findProgram('{&prompts}','','/jobSeqScan.w')) (THIS-PROCEDURE, commaList).
