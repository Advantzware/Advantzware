def var cDfName as char no-undo.
assign 
    cDfName = "c:\asigui\databases\comp\schema\" + ldbname(1) + ".df".
run prodict/load_df.p (cDfName).
quit.