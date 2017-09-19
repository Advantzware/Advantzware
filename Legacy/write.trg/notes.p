
/*------------------------------------------------------------------------
    File        : notes.p
    Purpose     : 

    Syntax      :

    Description : Write trigger for the Notes table	

    Author(s)   : 
    Created     : Thu Sep 14 18:30:00 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/
&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME notes
TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.
DEFINE VARIABLE cPrograms AS CHARACTER NO-UNDO.

{methods/triggers/write.i}
cPrograms = 
      (IF PROGRAM-NAME(1) NE ? THEN PROGRAM-NAME(1) ELSE "") + "," 
    + (IF PROGRAM-NAME(2) NE ? THEN PROGRAM-NAME(2) ELSE "") + ","
    + (IF PROGRAM-NAME(3) NE ? THEN PROGRAM-NAME(3) ELSE "") + ","
    + (IF PROGRAM-NAME(4) NE ? THEN PROGRAM-NAME(4) ELSE "")
    .

ASSIGN notes.updateDate = TODAY
       notes.updateTime = TIME 
       notes.updateUser = USERID("asi")
       notes.updateProgram = cPrograms
       .
