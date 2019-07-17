/*------------------------------------------------------------------------
    File        : addPrgrmsID.p
    Purpose     : populate prgrms.prgrmsID from sequence
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
FOR EACH prgrms :
    ASSIGN 
        prgrms.prgrmsID = NEXT-VALUE(prgrms_seq).
END.