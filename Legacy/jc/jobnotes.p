/* when job's closed, create job's own note and assign new rec_key 
   Task# 07280514   */
DEF PARAM BUFFER io-job FOR job.

DEF BUFFER b-notes FOR notes.


DISABLE TRIGGERS FOR LOAD OF oe-ord.  
DISABLE TRIGGERS FOR LOAD OF oe-ordl.

DEF VAR v-old-job-rec_key AS CHAR NO-UNDO.
DEF VAR lDoCopyNote2ASI AS LOG NO-UNDO.

IF AVAIL io-job THEN
FIND FIRST est NO-LOCK
    WHERE est.company EQ io-job.company
      AND est.est-no  EQ io-job.est-no
      AND est.rec_key EQ io-job.rec_key
    NO-ERROR.
    
lDoCopyNote2ASI = NO.
FILE-INFO:FILE-NAME = ".".
IF FILE-INFO:FULL-PATHNAME MATCHES "*addon*" THEN lDoCopyNote2ASI = YES.
  
/*MESSAGE "debug: test: Addon?" FILE-INFO:FULL-PATHNAME SKIP
     lDoCopyNote2ASI
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
IF AVAIL est THEN DO:
  ASSIGN
     v-old-job-rec_key = io-job.rec_key
     io-job.rec_key = STRING(TODAY,"99999999") +
                      STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").

  CREATE rec_key.
  ASSIGN
   rec_key.rec_key    = io-job.rec_key
   rec_key.table_name = "JobNotes".

  FOR EACH notes NO-LOCK WHERE notes.rec_key EQ est.rec_key:
    CREATE b-notes.
    BUFFER-COPY notes TO b-notes
    ASSIGN
     b-notes.rec_key = io-job.rec_key.
  END.

  IF v-old-job-rec_key <> est.rec_key THEN
   FOR EACH notes NO-LOCK WHERE notes.rec_key EQ v-old-job-rec_key:
    CREATE b-notes.
    BUFFER-COPY notes TO b-notes
    ASSIGN
     b-notes.rec_key = io-job.rec_key.
  END.


  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ io-job.company
        AND oe-ordl.job-no  EQ io-job.job-no
        AND oe-ordl.job-no2 EQ io-job.job-no2,
      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no:
    ASSIGN
     oe-ordl.rec_key = io-job.rec_key
     oe-ord.rec_key  = io-job.rec_key.
  END.

  IF lDoCopyNote2ASI THEN DO:
     RUN touch/savenoteA.p (io-job.rec_key,STRING(io-job.job)).
    
  END.
  /*IF CAN-FIND(FIRST nosweat._file WHERE
                    nosweat._file._file-name = "mfgroup") THEN
     RUN custom/mfvalue-rec-key-update.p(INPUT v-old-job-rec_key,
                                         INPUT io-job.rec_key).*/
END.
