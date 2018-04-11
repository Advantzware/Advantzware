def var cDfFile as char.
assign
    cDfFile = session:parameter.
run prodict\load_df.p (input cDfFile).