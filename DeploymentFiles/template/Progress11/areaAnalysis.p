/*---------------------------------------------------------------------------*/
/*  File:           areaAnalysis.p                                           */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Procedure to list available storage areas and limits     */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:                                                       */
/*  External files:     WRITE <dbname>Areas.txt                              */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/

DEF VAR cDb AS CHAR.
DEF VAR iFree AS INT.

ASSIGN 
    cDb = SESSION:PARAM.

OUTPUT TO VALUE(cDb + "Areas.txt").
FOR EACH _AreaStatus:
    ASSIGN 
        iFree = _AreaStatus-TotBlocks - _AreaStatus-Hiwater .
    EXPORT DELIMITER "," 
        "Database" 
        "Area Name" 
        "Total Blocks" 
        "High Watermark" 
        "Free".
    EXPORT DELIMITER "," 
        cDb 
        _AreaStatus-AreaName
        _AreaStatus-TotBlocks  
        _AreaStatus-Hiwater 
        iFree.
END.
OUTPUT CLOSE.
