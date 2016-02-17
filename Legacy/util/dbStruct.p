
/*------------------------------------------------------------------------
    File        : dbStruct.p
    Purpose     : 

    Syntax      :

    Description : 
STEPS:

1.) Find the database blocksize needed

$  prorest dbname backupfile -vp

Example:

This is a full backup of <dir>\dbname.db. (6759)
This backup was taken [ddd mmm DD HH:MM:SS YYYY]. (6760)
The blocksize is 4096. (6994)
Partial verification successfully read backup volume. (6765)
Verify pass started. (3751)
Verified nnnn db blocks in 00:00:00
Backup for <dir>\dbname.db verified ok. (6758)

2a.)  Verify the size needed from the probkup volumes:
$  prorest dbname backupfile -list

Example:
Area Name: <Area_Name>
       Size: 2130958090, Records/Block: 32, Area Number: 9, Cluster Size: 1

2b.)  From the output calculate the space needed per Storage area for the restore:

Restore_Size = "Size_from_prorest" / RPB * databaseblocksize
266,369,761 =  2130958090 / 32 * 4
Restore_Size = 254.03 GB

The (sum) of the extents in the .st file for this Storage Area, will therefore need at least 254.03 GB for the restore.

Alternatively, the prostrct -list output can be parsed with 4GL/ABL to provide a psudo .st file ready for editing. An ABL code example is provided below.
    Author(s)   : 
    Created     : Sun Feb 14 14:44:53 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH-CHARS = 152.
DEF VAR wFile        AS CHAR NO-UNDO FORMAT "x(35)" INIT "c:\tmp\bklist.txt".
DEF VAR wDbBlockSize AS INT  NO-UNDO INIT 4.
DEF VAR wAreaName    AS CHAR NO-UNDO FORMAT "x(30)".
DEF VAR wC           AS CHAR NO-UNDO FORMAT "x(100)".

DEF TEMP-TABLE ttAreaInfo
    FIELD AreaNbr  AS INT  FORMAT "zzzz9"
    FIELD AreaName AS CHAR FORMAT "x(30)"
    FIELD AreaBPC  AS INT  FORMAT "zz9" 
    FIELD AreaRPB  AS INT  FORMAT "zz9" 
    FIELD AreaSize AS DEC  FORMAT "zzz,zzz,zzz,zz9"
    FIELD AreaKB   AS DEC  FORMAT "zzz,zzz,zzz,zz9"
    INDEX AreaNr IS PRIMARY UNIQUE AreaNbr
    INDEX AreaSize                 AreaSize.
  

FOR EACH _filelist:
    DISPLAY
        _FileList-BlkSize /* INTEGER Block size of the file */
        _FileList-Extend /* INTEGER Amount of most recent extend in blocks */
        _FileList-LogicalSz /* INTEGER Logical file size, in blocks */
        _FileList-Name /* CHARACTER Name of the file */
        _FileList-Openmode /* CHARACTER Displays the mode in which the file is */
        _FileList-Size /* INTEGER Size of the file */ WITH FRAME fFileList 20 DOWN  WIDTH 150.
END.

PAUSE.
HIDE FRAME fFileList.


MESSAGE "Run to a file: prorest dbname backupfile -list > filename"
    VIEW-AS ALERT-BOX. 

/* Update wDbBlockSize wFile view-as editor size 80 by 1. */
/* Update wDbBlockSize wFile. */
HIDE ALL NO-PAUSE.

MESSAGE "Output file for -list (x to exit)" UPDATE wFile.
IF wFile EQ "X" THEN 
    QUIT.
  
INPUT FROM VALUE(wFile) NO-ECHO.
SET ^.
REPEAT:
    IMPORT UNFORMATTED wC.

    IF wC = "" THEN
    NullStill: DO:
        wAreaName = "".
        NEXT.
    END.

    IF wC BEGINS "Area" THEN
        ASSIGN wAreaName = TRIM(ENTRY(2, wC, ":")).
    ELSE IF wC BEGINS "       Size" THEN
        DO:
            CREATE ttAreaInfo.
            ASSIGN
                ttAreaInfo.AreaName = wAreaName
                ttAreaInfo.AreaNbr  = Int(TRIM(ENTRY(2, ENTRY(3,wC,","), ":")))
                ttAreaInfo.AreaBPC  = Int(TRIM(ENTRY(2, ENTRY(4,wC,","), ":")))
                ttAreaInfo.AreaRPB  = Int(TRIM(ENTRY(2, ENTRY(2,wC,","), ":")))
                ttAreaInfo.AreaSize = Int(TRIM(ENTRY(2, ENTRY(1,wC,","), ":"))).
            ttAreaInfo.AreaKB = ttAreaInfo.AreaSize / ttAreaInfo.AreaRPB * wDbBlockSize.
        END.
END.
INPUT Close.

OUTPUT TO "c:\tmp\bkupst.txt".

PUT UNFORM "# ProRest -list Area Info in " + wFile SKIP.
PUT UNFORM "b . " SKIP.

FOR EACH ttAreaInfo:
    PUT UNFORM 'd "' +
        ttAreaInfo.AreaName +
        '":' +
        STRING(ttAreaInfo.AreaNbr) +
        ',' +
        STRING(ttAreaInfo.AreaRPB) +
        ';' + STRING(ttAreaInfo.AreaBPC) +
        ' . f ' + STRING(ttAreaInfo.AreaKB)
        SKIP.
END.
OUTPUT CLOSE.