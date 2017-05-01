/*---------------------------------------------------------------------------*/
/*  File:           load.p                                                   */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Applies a delta.df to a connected database               */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:  prodict/load.df                                      */
/*  External files:     WRITE stdErr.out                                     */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/
 
DEF INPUT PARAMETER ipcDeltaFile AS CHAR NO-UNDO.
 
OUTPUT TO stdErr.out.

/* Make sure you also place the delta.df file in the database folder. */ 
RUN prodict/load_df.p (ipcDeltaFile). 
 
