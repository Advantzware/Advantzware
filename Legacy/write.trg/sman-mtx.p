&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME sman-mtx

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.

DO li = 1 TO EXTENT({&TABLENAME}.procat) - 1:
  IF {&TABLENAME}.procat[li]     EQ "" AND
     old-{&TABLENAME}.procat[li] NE "" THEN
  DO lj = li TO EXTENT({&TABLENAME}.procat) - 1:
    IF {&TABLENAME}.procat[lj + 1] NE "" THEN
      ASSIGN
        {&TABLENAME}.procat[lj]    = {&TABLENAME}.procat[lj + 1]
        {&TABLENAME}.dscr[lj]      = {&TABLENAME}.dscr[lj + 1]
        {&TABLENAME}.comm[lj]      = {&TABLENAME}.comm[lj + 1]
        {&TABLENAME}.PTD[lj]       = {&TABLENAME}.PTD[lj + 1]
        {&TABLENAME}.YTD[lj]       = {&TABLENAME}.YTD[lj + 1]
        {&TABLENAME}.LYR[lj]       = {&TABLENAME}.LYR[lj + 1]
        {&TABLENAME}.commbasis[lj] = {&TABLENAME}.commbasis[lj + 1]
        
        {&TABLENAME}.procat[lj + 1]    = ""
        {&TABLENAME}.dscr[lj + 1]      = ""
        {&TABLENAME}.comm[lj + 1]      = 0
        {&TABLENAME}.PTD[lj + 1]       = 0
        {&TABLENAME}.YTD[lj + 1]       = 0
        {&TABLENAME}.LYR[lj + 1]       = 0
        {&TABLENAME}.commbasis[lj + 1] = "".
  END.
END.

