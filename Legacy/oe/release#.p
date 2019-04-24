/*-----------------------------------------------------------------------------
    File        : oe\release#.p
    Copyright   : (c)1985-2019 Advantzware, Inc. All rights reserved.
    Description : 
    Author(s)   : 
    Created     : Apr 24, 2019 7:55:04 AM
    Notes       : 
                  
---------------------------------------------------------------------------*/
DEF INPUT  PARAM ip-cocode  LIKE oe-relh.company    NO-UNDO.
DEF OUTPUT PARAM op-next#   LIKE oe-relh.release#   NO-UNDO.

ASSIGN 
    op-next# = 0.
RUN oe/getNextRelNo.p (INPUT "release#", OUTPUT op-next#).

