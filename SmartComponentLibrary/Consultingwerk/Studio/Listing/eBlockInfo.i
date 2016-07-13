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
    File        : eBlockInfo.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 26.06.2011 21:29:58
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eBlockInfo NO-UNDO {&REFERENCE-ONLY} BEFORE-TABLE eBlockInfoBefore
    FIELD Sequence AS INTEGER FORMAT ">>>,>>>,>>9":U INIT "0":U LABEL "Sequence"
    FIELD FileName AS CHARACTER FORMAT "X(20)":U LABEL "File Name"
    FIELD LineNum AS INTEGER FORMAT ">>>,>>9":U INIT "0":U LABEL "Line Number"
    FIELD BlockType AS CHARACTER FORMAT "X(12)":U LABEL "Block Type"
    FIELD TransactionBlock AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Transaction"
    FIELD BlockLabel AS CHARACTER FORMAT "X(40)":U LABEL "Block Label"

    INDEX Sequence AS UNIQUE PRIMARY Sequence ASCENDING

    .
