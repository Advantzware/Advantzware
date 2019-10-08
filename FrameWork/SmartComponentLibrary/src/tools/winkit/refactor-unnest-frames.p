/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/

/*------------------------------------------------------------------------
    File        : refactor-unnest-frames
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 07 21:22:25 CET 2012
    Notes       : See src/adeuib/_genrt.i (mostly unchanged since DLC9.1)
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcFile AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE iFrom    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTo      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBlock   AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iComment AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDot     AS INTEGER   NO-UNDO.

/* ***************************  Main Block  *************************** */

COPY-LOB FROM FILE pcFileName TO lcFile . 

iFrom = INDEX (lcFile, "~&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES":U) .

IF iFrom > 0 THEN . 
ELSE RETURN .

iTo = INDEX (lcFile, "~&ANALYZE-RESUME":U, iFrom) .  

IF iTo > 0 THEN . 
ELSE RETURN .

ASSIGN cBlock = SUBSTRING (lcFile, iFrom, iTo - iFrom) .

/* Search for  /* REPARENT FRAME */ */
iComment = INDEX (cBlock, "/* REPARENT FRAME */":U) .

IF iComment > 0 THEN DO:
    iDot = INDEX (cBlock, ".":U, iComment) . 
    
    IF iDot > 0 THEN 
        SUBSTRING (cBlock, iComment, iDot - iComment + 1) = "":U . 
END.

/* Search for  DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO. */
iComment = INDEX (cBlock, "DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.":U) .

IF iComment > 0 THEN DO:
                        /* 1234567890123456789012 */
    iDot = INDEX (cBlock, "/* END-ASSIGN-TABS */.":U, iComment) . 
    
    IF iDot > 0 THEN 
        SUBSTRING (cBlock, iComment, iDot - iComment + 22) = "":U . 
END.

SUBSTRING (lcFile, iFrom, iTo - iFrom) = cBlock . 

COPY-LOB FROM lcFile TO FILE pcFileName . 



