
/*------------------------------------------------------------------------
    File        : convUtilitiesToPrgrms.p
    Purpose     : Convert records in utilities table to program master records 
    Syntax      :
    Description : 
    Author(s)   : MYT 
    Created     : Wed Jun 26 06:52:44 EDT 2019
    Notes       : also add as procedure in asiUpdateENV.w after 51238 is committed
    Copyright   : (c) 2019 Advanced Software Inc.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FOR EACH utilities:
    FIND prgrms NO-LOCK WHERE
        prgrms.mnemonic EQ utilities.module
        NO-ERROR.
    IF NOT AVAIL prgrms THEN DO: 
        CREATE prgrms.
    END.        
    ASSIGN 
        prgrms.prgmName = REPLACE(utilities.programName,".r",".")
        prgrms.module = utilities.module
        prgrms.prgtitle = utilities.description
        prgrms.mnemonic = utilities.hotkey
        prgrms.can_create = "*"
        prgrms.can_delete = "*"
        prgrms.can_run = "*"
        prgrms.can_update = "*"
        prgrms.dir_group = "util"
        prgrms.securityLevelDefault = utilities.securityLevel
        prgrms.securityLevelUser = utilities.securityLevel
        prgrms.notes = utilities.notes
        prgrms.mode = "Util"
        . 
END.

FOR EACH prgrms:
    COPY-LOB VALUE(SEARCH(prgrms.image)) 