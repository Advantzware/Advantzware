/* setSize.i - used in procedure setSize in board.w */

RUN buildBoardDowntime.
RUN jobDowntimeSpan.
RUN getJobNotes.
HIDE btnDetail btnPending btnPendingJobs
     scenario btnSave /*btnRemove*/ btnReset btnJobSeqScan btnDatePrompt
     /* btnRedo btnUndo */ NO-PAUSE.
