&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME dept

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER bf-dept FOR dept.
DEF VAR i AS INT NO-UNDO.

i = dept.fc.
for each bf-dept WHERE bf-dept.fc     GE dept.fc
         and bf-dept.fc     lt 99
         and rowid(bf-dept) ne rowid(dept)
         by bf-dept.fc:

        assign
           i       = i + 1
           bf-dept.fc = i * -1  .
end.

for each bf-dept WHERE bf-dept.fc lt 0:
       bf-dept.fc = bf-dept.fc * -1.
end.

for each bf-dept no-lock,
       each mach where mach.dept[1] eq bf-dept.code:

     mach.d-seq = bf-dept.fc.
end.

   
 
