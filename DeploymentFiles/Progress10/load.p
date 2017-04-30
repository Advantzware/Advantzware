 DEF INPUT PARAMETER ipcDeltaFile AS CHAR NO-UNDO.
 /* load.p */  
 output to stdErr.out.
RUN prodict/load_df.p (ipcDeltaFile). /* Make sure you also place the delta.df file in the database folder. */ 
 
/* end of load.p */ 
 
