/* Quick & Dirty Speed Test */

DEF TEMP-TABLE tt-reftabTrack
  FIELD refTabRow AS ROWID.

DEF VAR i AS INT.
DEFINE VARIABLE a AS INT64 NO-UNDO.
DEF VAR cLocalDir AS CHAR INIT "c:\tmp\" VIEW-AS FILL-IN.
DEF VAR cRemoteDir AS CHAR INIT "N:\rcode\logs\" VIEW-AS FILL-IN.

DEF TEMP-TABLE ttResults 
  FIELD time-value AS INT64 COLUMN-LABEL "Miliseconds"
  FIELD descr AS CHAR FORMAT "x(30)" COLUMN-LABEL "Test Name"
  FIELD iseq AS INTEGER COLUMN-LABEL "Sequence"
  INDEX i1 iseq.

DEF VAR iResultSeq AS INT.


DEF STREAM sWholeFile.
DEF STREAM sSmallFile.

UPDATE cLocalDir FORMAT "x(50)" SKIP cRemoteDir FORMAT "x(50)" WITH SIDE-LABELS.
IF NOT SUBSTRING(cLocalDir, LENGTH(cLocalDir), 1) EQ "\" THEN 
  cLocalDir = cLocalDir + "\".
  
IF NOT SUBSTRING(cRemoteDir, LENGTH(cRemoteDir), 1) EQ "\" THEN 
    cRemoteDir = cRemoteDir + "\".


/* Db Write */
a = ETIME(YES).

DO  i = 1 TO 1000:

  CREATE reftable.
  ASSIGN reftable.reftable = "junkRef"
         reftable.company = '001'
         reftable.loc = "main"
         reftable.CODE = "a;sldj a;lsd fa;sldjkf a;sldf ;alsdjfl;asjd fl;asdf;lajsdf lajs df;ljasdf;ljasdl;fja;lsdjf ;lajsdf l;ajsdfl;kajsdfl;as;dlf as;ldfja;lsjdf ;lajs dfl;akjs df;ljasdf l;jas dl;fj asl;djf ;lasj df;lasj dfl;kaj sdfl;kja sdlf;j asldfj al;sj df;las df;lajsdf;ljas;ldfj ;alsjdf ;lasjdf;lajs df;jas ;dflja s;ljdf ;lasjkdf;lajsdf;ljak sdf;ljka sdl;fj a;lskjdfa;ljksdf ja;sdj;fas;jdf;alkjsdf;ljasdl;fjka;lsdfjl;asjdfl;jas;ldfja;lskjdf;ljasdfljk;a;ljsdf;alksdf;ljasl;dkjfa;lsjdfl;jasdjlfj;alsjdfjds"
         reftable.code2 = reftable.CODE + reftable.CODE + reftable.CODE.
  CREATE tt-refTabTrack.
  ASSIGN tt-refTabTrack.refTabRow = ROWID(reftable).

END.
RUN pCreateResultRec (INPUT "Db Write").
MESSAGE "DB Write" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* Local file write */
a = ETIME(YES).
OUTPUT STREAM sWholeFile TO value(cLocalDir + "reftable.d").
FOR EACH tt-refTabTrack i = 1 TO 1000:
  FIND reftable NO-LOCK
      WHERE ROWID(reftable) = tt-refTabTrack.refTabRow.
  OUTPUT STREAM sSmallFile TO VALUE(cLocalDir + "reftable" + STRING(i) + ".d").
  EXPORT STREAM sSmallFile reftable.
  OUTPUT STREAM sSmallFile CLOSE.
  EXPORT STREAM sWholeFile reftable.
END.
OUTPUT STREAM sWholeFile CLOSE.

RUN pCreateResultRec (INPUT "Local Write").
MESSAGE "local write" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Remove file copy */
a = ETIME(YES).

FOR EACH tt-refTabTrack i = 1 TO 1000:
  FIND reftable NO-LOCK
      WHERE ROWID(reftable) = tt-refTabTrack.refTabRow.

  OS-COPY VALUE(cLocalDir + "reftable" + STRING(i) + ".d") VALUE(cRemoteDir + "reftable" + STRING(i) + ".d").
  
END.
OUTPUT STREAM sWholeFile CLOSE.

RUN pCreateResultRec (INPUT "Network Copy").
MESSAGE "network copy" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.


/* Remove file Remove */
a = ETIME(YES).

FOR EACH tt-refTabTrack i = 1 TO 1000:
  FIND reftable NO-LOCK
      WHERE ROWID(reftable) = tt-refTabTrack.refTabRow.

  OS-DELETE VALUE(cRemoteDir + "reftable" + STRING(i) + ".d").
  
END.
OUTPUT STREAM sWholeFile CLOSE.

RUN pCreateResultRec (INPUT "Network File Del").
MESSAGE "network file del" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.


/* Remove Remove file local */
a = ETIME(YES).

FOR EACH tt-refTabTrack i = 1 TO 1000:
  FIND reftable NO-LOCK
      WHERE ROWID(reftable) = tt-refTabTrack.refTabRow.

  OS-DELETE VALUE(cLocalDir + "reftable" + STRING(i) + ".d") .
  
END.
OUTPUT STREAM sWholeFile CLOSE.

RUN pCreateResultRec (INPUT "Local File Del").
MESSAGE "local file del" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* Local Db Del */
a = ETIME(YES).

FOR EACH tt-refTabTrack:
  FIND reftable EXCLUSIVE-LOCK 
      WHERE ROWID(reftable) = tt-refTabTrack.refTabRow.
  DELETE reftable.
END.

RUN pCreateResultRec (INPUT "Db Recs Del").
MESSAGE "Db del" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Local Db Del */
a = ETIME(YES).


FOR EACH reftable NO-LOCK i = 1 TO 10000:

END.
RUN pCreateResultRec (INPUT "Db Reads").
MESSAGE "Db Reads" ETIME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

OUTPUT TO "CLIPBOARD".
FOR EACH ttResults:
  DISP ttResults WITH WIDTH 200 STREAM-IO.
END.
OUTPUT CLOSE.

HIDE ALL NO-PAUSE.

MESSAGE "Summary is in clipboard"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  
PROCEDURE pCreateResultRec:
  DEF INPUT PARAMETER ipcDesc AS CHAR. 
  iResultSeq = iResultSeq + 1.
  CREATE ttResults.
  ASSIGN time-value = ETIME
       descr = ipcDesc
       iSeq = iResultSeq.

END PROCEDURE.
