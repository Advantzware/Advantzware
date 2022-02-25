
/*------------------------------------------------------------------------
    File        : ttCalcLayoutSize.i
    Purpose     : include file to hold tt definition for layout size

    Syntax      : 

    Description : it will be included in est/CalcLayoutSize.p and other caller programs

    Author(s)   : sakshi.singh
    Created     : Fri Dec 10 03:25:24 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLayoutSize NO-UNDO
    FIELD dLayoutSheetLength    AS DECIMAL  // xef.lsh-len- Side to Side Layout Length
    FIELD dLayoutSheetWidth     AS DECIMAL  // xef.lsh-wid- Front to Back Layout Width
    FIELD dNetSheetLength       AS DECIMAL  // xef.nsh-len
    FIELD dNetSheetWidth        AS DECIMAL  // xef.nsh-wid
    FIELD dNetSheetDepth        AS DECIMAL  // xef.nsh-dep
    FIELD dGrossSheetLength     AS DECIMAL  // xef.gsh-len 
    FIELD dGrossSheetWidth      AS DECIMAL  // xef.gsh-wid
    FIELD dGrossSheetDepth      AS DECIMAL  // xef.gsh-dep
    FIELD dDieSizeLength        AS DECIMAL  // xef.trim-l
    FIELD dDieSizeWidth         AS DECIMAL  // xef.trim-w
    FIELD dDieSizeDepth         AS DECIMAL  // xef.trim-d 
    FIELD dRollWidth            AS DECIMAL  // xef.roll-wid
    FIELD dDieInchesRequired    AS DECIMAL  // xef.die-in - Total die inches required for the layout
    FIELD cBoardItemCode        AS CHARACTER  // xef.i-code
    FIELD cBoardItemBasisWeight AS DECIMAL  // xef.weight
    FIELD dBoardItemCaliper     AS DECIMAL  // xef.cal
    FIELD IsRollMaterial        AS LOGICAL  // xef.roll - Is this a Roll material? 
    FIELD iNumOutWidth          AS DECIMAL  // xef.n-out - Number Out
    FIELD iNumOutLength         AS DECIMAL  // xef.n-out-l
    FIELD iNumOutDepth          AS DECIMAL  //  xef.n-out-d
    FIELD iNumberCuts           AS DECIMAL  // xef.n-cuts
    FIELD iBlankNumUp           AS DECIMAL  // xeb.num-up 
    FIELD iBlankNumOnWidth      AS DECIMAL  // xeb.num-wid
    FIELD iBlankNumOnLength     AS DECIMAL  // xeb.num-len
    FIELD iBlankNumOnDepth      AS DECIMAL  // xeb.num-dep
    FIELD lRecalcFullLayout     AS LOGICAL  // In case of error, recalculate and assign full layout calc
    .
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
