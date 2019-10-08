
/*------------------------------------------------------------------------
    File        : calcTotalBrdQty.p
    Purpose     : 

    Syntax      :

    Description : Calculate all board qty matching the given board

    Author(s)   : 
    Created     : Fri Apr 17 18:03:25 EDT 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipriEf AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdTotBqty AS DECIMAL NO-UNDO.
/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE BUFFER bf-ef   FOR ef.
DEFINE BUFFER bf-eb   FOR eb.
DEFINE BUFFER bfPrimary-ef  FOR ef.
DEFINE BUFFER bf-item FOR ITEM.

DEFINE VARIABLE dQtyInUOM     AS DECIMAL   NO-UNDO.
               
FIND bfPrimary-ef NO-LOCK 
    WHERE ROWID(bfPrimary-ef) EQ ipriEf 
    NO-ERROR.
IF NOT AVAILABLE bfPrimary-ef THEN RETURN.

FOR EACH bf-ef NO-LOCK
    WHERE bf-ef.company EQ bfPrimary-ef.company
    AND bf-ef.est-no EQ bfPrimary-ef.est-no
    AND ROWID(bf-ef) NE ROWID(bfPrimary-ef)
    AND bf-ef.gsh-len EQ bfPrimary-ef.gsh-len
    AND bf-ef.gsh-wid EQ bfPrimary-ef.gsh-wid
    AND bf-ef.board EQ bfPrimary-ef.board,
    FIRST bf-item NO-LOCK  
        WHERE bf-item.company EQ bf-ef.company
        AND bf-item.i-no = bf-ef.board 
        :   
    
    RUN sys/ref/convquom.p("EA", ipcQtyUOM, bf-item.basis-w,
        bf-ef.gsh-len, bf-ef.gsh-wid, bf-ef.gsh-dep,
        bf-ef.gsh-qty, OUTPUT dQtyInUOM).
    
    opdTotBqty = opdTotBqty + dQtyInUOM.
                              
END.
