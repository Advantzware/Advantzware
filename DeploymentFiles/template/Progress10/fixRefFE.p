/*---------------------------------------------------------------------------*/
/*  File:           fixRefFE.p                                               */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    DB connection file for fixRef.p                          */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:  fixRef.p                                             */
/*  External files:     WRITE propath.log                                    */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/

OUTPUT TO propath.log.
    
ASSIGN 
    propath = SESSION:PARAM + "," + PROPATH.
PUT UNFORMATTED PROPATH SKIP.
CONNECT -pf VALUE(SESSION:PARAM + "\" + "asi.pf") .

RUN fixRef.p.
