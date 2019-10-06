/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : WebHelper
    Purpose     : Support routines for handling Web Requests
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon May 30 17:43:03 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .

CLASS Consultingwerk.Util.WebHelper:

    /*------------------------------------------------------------------------------
     Purpose: Attempts to decode a previously-encoded URL.
     Notes:
     @param pcURI The URL to decode
     @return CHARACTER A decoded URL
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER DecodeURL (pcURI AS CHARACTER):

        DEFINE VARIABLE cDecodedURI AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iStart      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iPos        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iAsc        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cHex        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE rDecoded    AS RAW       NO-UNDO.
        DEFINE VARIABLE cAscChar    AS CHARACTER NO-UNDO.

        IF pcURI = "":U OR pcURI = ? THEN
            RETURN "":U .

        ASSIGN iStart = 1
               iPos   = INDEX (pcURI, '%':U, iStart).
               .

        DO WHILE iPos GT 0:
            ASSIGN cHex     = SUBSTRING (pcURI, iPos + 1, 2)
                   rDecoded = HEX-DECODE (cHex)
                   iAsc     = GET-BYTE (rDecoded, 1)
                   cAscChar = CHR (iAsc, '1252':U, '1252':U)

                   cDecodedURI = cDecodedURI
                               + SUBSTRING (pcURI, iStart, (iPos - iStart))
                               + cAscChar

                   iStart = iPos + 3
                   iPos   = INDEX (pcURI, '%':U, iStart).
        END.

        /* add anycharacters that appear after the last % */
        ASSIGN cDecodedURI = cDecodedURI + SUBSTRING (pcURI, iStart).

        RETURN cDecodedURI.

    END METHOD.

END CLASS.
