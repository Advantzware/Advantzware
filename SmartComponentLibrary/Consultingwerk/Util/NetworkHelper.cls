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
    File        : NetworkHelper
    Purpose     : Provides access to .NET network routines
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 23 09:49:17 CEST 2012
    Notes       : Supports custom log file messages using the "NetworkHelper" custom message sub system
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.Util.Exceptions.* FROM PROPATH.
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Util.NetworkHelper:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED NetworkHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Downloads a file from a specified web address to a given file
        Notes:   Currently supported on Windows only
        @param pcFileAddress The address (URL) to download from
        @param pcTargetFile The file name (and path) to download to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DownloadFile (pcFileAddress AS CHARACTER,
                                            pcTargetFile AS CHARACTER):

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        DEFINE VARIABLE oWebClient AS System.Net.WebClient NO-UNDO .

        oWebClient = NEW System.Net.WebClient().
        oWebClient:DownloadFile (pcFileAddress,
                                 pcTargetFile).

        oWebClient:Dispose() .

        DELETE OBJECT oWebClient.
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadFile":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Downloads a string from a specified web address to a given file
        Notes:   Currently supported on Windows only
        @param pcFileAddress The address (URL) to download from
        @return A LONGCHAR value with the data from the URL
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LONGCHAR DownloadString (pcFileAddress AS CHARACTER):

        DEFINE VARIABLE lcString AS LONGCHAR NO-UNDO .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        DEFINE VARIABLE oWebClient AS System.Net.WebClient NO-UNDO .

        oWebClient = NEW System.Net.WebClient().
        lcString = oWebClient:DownloadString (pcFileAddress).

        RETURN lcString .

        FINALLY:
            oWebClient:Dispose() .

            DELETE OBJECT oWebClient.
        END FINALLY.
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadString":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .

&IF NOT PROVERSION BEGINS "10.2" &THEN
    /*------------------------------------------------------------------------------
        Purpose: Downloads a string from a specified web address to a given file
        Notes:   Currently supported on Windows only, not supported on OpenEdge 10.2B
                 due to .NET 4.0 dependency
        @param pcFileAddress The address (URL) to download from
        @param poStatus The HttpStatus instance with the http status code and description of the request
        @return A LONGCHAR value with the data from the URL
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LONGCHAR DownloadString (pcFileAddress AS CHARACTER,
                                                  OUTPUT poStatus AS HttpStatus):

        DEFINE VARIABLE lcString AS LONGCHAR NO-UNDO .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        DEFINE VARIABLE oWebClient AS System.Net.WebClient        NO-UNDO .
        DEFINE VARIABLE oFieldInfo AS System.Reflection.FieldInfo NO-UNDO .
        DEFINE VARIABLE oResponse  AS System.Net.HttpWebResponse  NO-UNDO .

        poStatus = NEW HttpStatus () .

        oWebClient = NEW System.Net.WebClient().

        oFieldInfo = oWebClient:GetType():GetField("m_WebResponse":U,
                                                   CAST (Progress.Util.EnumHelper:Or (System.Reflection.BindingFlags:Instance,
                                                                                      System.Reflection.BindingFlags:NonPublic),
                                                         System.Reflection.BindingFlags)) .

        DO ON ERROR UNDO, THROW:
            lcString = oWebClient:DownloadString (pcFileAddress).

            CATCH webex AS System.Net.WebException:
                IF TYPE-OF (webex:Response, System.Net.HttpWebResponse) THEN DO:
                    ASSIGN poStatus:StatusCode        = CAST (webex:Response, System.Net.HttpWebResponse):StatusCode:value__
                           poStatus:StatusDescription = CAST (webex:Response, System.Net.HttpWebResponse):StatusDescription .
                END.
                ELSE
                    UNDO, THROW webex .
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oFieldInfo) THEN DO:
                    oResponse = CAST (oFieldInfo:GetValue (oWebClient), System.Net.HttpWebResponse) .

                    IF VALID-OBJECT (oResponse) THEN
                        ASSIGN poStatus:StatusCode        = oResponse:StatusCode:value__
                               poStatus:StatusDescription = oResponse:StatusDescription .
                END .
            END FINALLY.
        END.

        RETURN lcString .

        FINALLY:
            oWebClient:Dispose() .

            DELETE OBJECT oWebClient.
        END FINALLY.
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadString":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .

&ENDIF

    /**
     * Purpose: Validates that a DotAtom of an EmailAddress consists of valid
     *          characters only.
     * Notes:   Normals charset (always allowed):
     *          A-Za-z0-9.!#$%&'*+-/=?^_`{|}~
     *          Extended charset (only when enclosed by double quotes):
     *          "(),:;<>@[\] plus space
     * @param pcAtom Dot Atom of the email address to be verified
     * @return Returns TRUE if the validation was successful
     */
    METHOD PROTECTED STATIC LOGICAL EmailCheckDotAtom (pcAtom AS CHARACTER):

        DEFINE VARIABLE lExtendedCharsetAllowed AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iCharCount              AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAtomLength             AS INTEGER NO-UNDO.
        DEFINE VARIABLE lCharValid              AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iASCII                  AS INTEGER NO-UNDO.

        iAtomLength = LENGTH (pcAtom, "CHARACTER":U).

        IF iAtomLength = 0 THEN
            RETURN TRUE.

        IF SUBSTRING (pcAtom, 1, 1, "CHARACTER":U) = '"':U THEN DO:

            IF SUBSTRING (pcAtom, iAtomLength, 1, "CHARACTER":U) = '"':U THEN
                lExtendedCharsetAllowed = TRUE.
            ELSE
                RETURN FALSE.
        END.

        DO iCharCount = 1 TO iAtomLength:

            ASSIGN
                lCharValid = FALSE
                iASCII     = ASC (SUBSTRING (pcAtom, iCharCount, 1, "CHARACTER":U))
                .

            /* A-Za-z0-9.!#$%&'*+-/=?^_`{|}~ */
            CASE iASCII :
                WHEN 46  OR                      /* ASC (".")  */
                WHEN 33  OR                      /* ASC ("!")  */
                WHEN 35  OR                      /* ASC ("#")  */
                WHEN 36  OR                      /* ASC ("$")  */
                WHEN 37  OR                      /* ASC ("%")  */
                WHEN 38  OR                      /* ASC ("&")  */
                WHEN 39  OR                      /* ASC ("'")  */
                WHEN 42  OR                      /* ASC ("*")  */
                WHEN 43  OR                      /* ASC ("+")  */
                WHEN 45  OR                      /* ASC ("-")  */
                WHEN 47  OR                      /* ASC ("/")  */
                WHEN 61  OR                      /* ASC ("=")  */
                WHEN 63  OR                      /* ASC ("?")  */
                WHEN 94  OR                      /* ASC ("^")  */
                WHEN 95  OR                      /* ASC ("_")  */
                WHEN 96  OR                      /* ASC ("`")  */
                WHEN 123 OR                      /* ASC ("~{") */
                WHEN 124 OR                      /* ASC ("|")  */
                WHEN 125 OR                      /* ASC ("}")  */
                WHEN 126 THEN lCharValid = TRUE. /* ASC ("~~") */
            END CASE.

            /* A-Z */
            IF iASCII >= 65 AND iASCII <= 90 THEN
                lCharValid = TRUE.

            /* a-z */
            IF iASCII >= 97 AND iASCII <= 122 THEN
                lCharValid = TRUE.

            /* 0-9 */
            IF iASCII >= 48 AND iASCII <= 57 THEN
                lCharValid = TRUE.

            /* "(),:;<>@[\] (ASCII: 32, 34, 40, 41, 44, 58, 59, 60, 62, 64, 91–93) */
            IF lExtendedCharsetAllowed THEN DO:
                CASE iASCII :
                    WHEN 32  OR                      /* ASC (" ")  */
                    WHEN 34  OR                      /* ASC ('"')  */
                    WHEN 40  OR                      /* ASC ("(")  */
                    WHEN 41  OR                      /* ASC (")")  */
                    WHEN 44  OR                      /* ASC (",")  */
                    WHEN 58  OR                      /* ASC (":")  */
                    WHEN 59  OR                      /* ASC (";")  */
                    WHEN 60  OR                      /* ASC ("<")  */
                    WHEN 62  OR                      /* ASC (">")  */
                    WHEN 64  OR                      /* ASC ("@")  */
                    WHEN 91  OR                      /* ASC ("[")  */
                    WHEN 92  OR                      /* ASC ("\")  */
                    WHEN 93  THEN lCharValid = TRUE. /* ASC ("]")  */
                END CASE.
            END.

            IF NOT lCharValid THEN
                RETURN FALSE.

        END.

        RETURN TRUE.

    END METHOD.

    /**
     * Purpose: Splits an EmailAddress into Local Part and Domain
     * Notes:   Checks if there are at least two @ seperated portions and
     *          splits at the last occurance of the @ sign as they are allowed
     *          inside quotes in the Local Part.
     * @param pcEmailAddress Full EmailAddress to be verified
     * @param pcLocalPart Local part of the email address (everything before @domain.xyz)
     * @param pcDomain Domain part of the email address (everything after the last @)
     * @return Returns TRUE if the structure validation was successful
     */
    METHOD PROTECTED STATIC LOGICAL EmailSplitParts (pcEmailAddress AS CHARACTER,
                                                     OUTPUT pcLocalPart AS CHARACTER,
                                                     OUTPUT pcDomain AS CHARACTER):

        DEFINE VARIABLE iPart     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iNumParts AS INTEGER NO-UNDO.

        iNumParts = NUM-ENTRIES (pcEmailAddress, "@":U).

        /* addr-spec = local-part "@" domain */
        IF iNumParts < 2 THEN
            RETURN FALSE.

        DO iPart = 1 TO iNumParts :

            IF iPart < iNumParts THEN
                pcLocalPart = SUBSTITUTE ("&1&2&3":U,
                                          pcLocalPart,
                                          IF pcLocalPart > "":U THEN "@":U ELSE "":U,
                                          ENTRY (iPart, pcEmailAddress, "@":U)).
            ELSE
                pcDomain    = ENTRY (iPart, pcEmailAddress, "@":U).

        END.

        RETURN TRUE.

    END METHOD.

    /**
     * Purpose: Verifies a Domain Name on character base and if wanted
     *          by checking the domains MX record
     * Notes:
     * @param pcDomain Domain part of the email address (everything after the last @)
     * @param plVerifyMX Optional verify the MX for the given Domain
     * @return Returns TRUE if the validation was successful
     */
    METHOD PROTECTED STATIC LOGICAL EmailVerifyDomain (pcDomain AS CHARACTER,
                                                       plVerifyMX AS LOGICAL):

        DEFINE VARIABLE iAtom       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAtomCount  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAtomLength AS INTEGER NO-UNDO.

        iAtomCount = NUM-ENTRIES (pcDomain, ".":U).

        /* domain = dot-atom / domain-literal / obs-domain */
        IF iAtomCount < 2 THEN
            RETURN FALSE.

        DO iAtom = 1 TO iAtomCount :

            iAtomLength = LENGTH (ENTRY (iAtom, pcDomain, ".":U), "CHARACTER":U).

            /* Atom is filled at all ".." is not allowed */
            IF iAtomLength = 0 THEN
                RETURN FALSE.

            /* Minimum length for Toplevel Domains */
            IF iAtom = iAtomCount AND
               iAtomLength < 2 THEN
                RETURN FALSE.

            /* Charset check */
            IF NOT EmailCheckDotAtom (ENTRY (iAtom, pcDomain, ".":U)) THEN
                RETURN FALSE.

        END.

        IF plVerifyMX THEN
            IF NOT VerifyMXRecord (pcDomain) THEN
                RETURN FALSE.

        RETURN TRUE.

    END METHOD.

    /**
     * Purpose: Verifies a LocalPart on character base
     * Notes:
     * @param pcLocalPart Local part of the email address (everything before @domain.xyz)
     * @return Returns TRUE if the validation was successful
     */
    METHOD PROTECTED STATIC LOGICAL EmailVerifyLocalPart (pcLocalPart AS CHARACTER):

        DEFINE VARIABLE iAtom      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAtomCount AS INTEGER NO-UNDO.

        iAtomCount = NUM-ENTRIES (pcLocalPart, ".":U).

        DO iAtom = 1 TO iAtomCount :
            IF NOT EmailCheckDotAtom (ENTRY (iAtom, pcLocalPart, ".":U)) THEN
                RETURN FALSE.
        END.

        RETURN TRUE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Extracts the Host portion from an URL
        Notes:
        @param pcUrl The URL that contains the host
        @return The hostname
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER HostFromUrl (pcUrl AS CHARACTER):

        IF NUM-ENTRIES (pcUrl, "/":U) < 3 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcUrl":U, pcUrl,
                                                            "Consultingwerk.Util.NetworkHelper":U) .

        IF ENTRY (2, pcUrl, "/":U) <> "":U THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcUrl":U, pcUrl,
                                                            "Consultingwerk.Util.NetworkHelper":U) .

        RETURN ENTRY (1, ENTRY (3, pcUrl, "/":U), ":":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Extracts the Resource portion from an http or https URL
        Notes:
        @param pcUrl The URL that contains the resource
        @return The resource name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ResourceFromUrl (pcUrl AS CHARACTER):

        DEFINE VARIABLE i    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

        DO i = 1 TO 2:
            iPos = INDEX (pcUrl, "/":U, iPos + 1) .
        END.

        IF NUM-ENTRIES (pcUrl, "/":U) > 3 THEN
            RETURN SUBSTRING (pcUrl, INDEX (pcUrl, "/":U, iPos + 1)) .

        IF INDEX (pcUrl, "?":U) > 0 THEN
            RETURN "/":U + SUBSTRING (pcUrl, INDEX (pcUrl, "?":U)) .

        RETURN "/":U .

    END METHOD .

    /**
     * Purpose: Validates an Email Address by splitting it into Domain and Local Part.
     * Notes:
     * @param pcEmailAddress Full EmailAddress to be verified
     * @return Returns TRUE if the validation was successful
     */
    METHOD PUBLIC STATIC LOGICAL ValidateEmailAddress (pcEmailAddress AS CHARACTER):

        RETURN ValidateEmailAddress (pcEmailAddress,
                                     FALSE).

    END METHOD.

    /**
     * Purpose: Validates an Email Address by splitting it into Domain and Local Part.
     * Notes:
     * @param pcEmailAddress Full EmailAddress to be verified
     * @param plVerifyMX Optional verify the MX for the given Domain
     * @return Returns TRUE if the validation was successful
     */
    METHOD PUBLIC STATIC LOGICAL ValidateEmailAddress (pcEmailAddress AS CHARACTER,
                                                       plVerifyMX AS LOGICAL):

        DEFINE VARIABLE cLocalPart AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDomain    AS CHARACTER NO-UNDO.

        IF NOT EmailSplitParts (pcEmailAddress,
                                OUTPUT cLocalPart,
                                OUTPUT cDomain) THEN
            RETURN FALSE.

        IF NOT EmailVerifyDomain (cDomain,
                                  plVerifyMX) THEN
            RETURN FALSE.

        IF NOT EmailVerifyLocalPart (cLocalPart) THEN
            RETURN FALSE.

        RETURN TRUE.

    END METHOD.

    /**
     * Purpose: Verifies the Domain by checking the existance of the MX record
     * Notes:   Needs different logic for Windows and Unix
     * @param pcDomain Domain string to lookup as an MX
     * @return Returns TRUE if the MX lookup was successful
     */
    METHOD PROTECTED STATIC LOGICAL VerifyMXRecord (pcDomain AS CHARACTER):

        DEFINE VARIABLE cOutput        AS CHARACTER NO-UNDO FORMAT "x(70)":U.
        DEFINE VARIABLE cFilename      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cErrorMessage  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lReturnedValue AS LOGICAL   NO-UNDO.

        DEFINE VARIABLE iValue AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        IF OPSYS BEGINS "WIN":U THEN DO:

            cFilename = SUBSTITUTE ("&1~\nslookup.txt":U, SESSION:TEMP-DIRECTORY).

            LogManager:WriteMessage ("Filename: ":U + cFilename, "NetworkHelper":U).

            OUTPUT TO VALUE (cFilename).

            PUT UNFORMATTED "set q=mx":U SKIP .
            PUT UNFORMATTED pcDomain SKIP .

            OUTPUT CLOSE .

            INPUT THROUGH VALUE (SUBSTITUTE ("type &1 | nslookup":U, QUOTER (cFilename))) NO-ECHO NO-MAP NO-CONVERT .

            importLoop:
            REPEAT ON ERROR UNDO, THROW:
                IMPORT UNFORMATTED cOutput .

                LogManager:WriteMessage ("Output: ":U + cOutput, "NetworkHelper":U).

                IF INDEX (cOutput, "***":U) > 0 THEN DO:

                    IF NUM-ENTRIES (cOutput, ":":U) >= 2 THEN DO:
                        cErrorMessage = TRIM (ENTRY (2, cOutput, ":":U)) + " (&1)":U.
                        LEAVE importLoop.
                    END.
                    ELSE
                        cErrorMessage = "Unknown Error occured for Domain: &1":U.

                END.
            END.

            LogManager:WriteMessage ("Domain: ":U + pcDomain, "NetworkHelper":U).
            LogManager:WriteMessage ("ErrorMessage: ":U + cErrorMessage, "NetworkHelper":U).
        END.
        ELSE DO:

            INPUT THROUGH VALUE (SUBSTITUTE ("dig &1 MX +short":U, pcDomain)) NO-ECHO NO-MAP NO-CONVERT .

            importLoop:
            REPEAT ON ERROR UNDO, THROW:
                IMPORT UNFORMATTED cOutput .

                lReturnedValue = TRUE.

                IF LENGTH (cOutput, "CHARACTER":U) < 1 THEN
                    NEXT importLoop.

                ASSIGN
                    cValue = ENTRY (1, cOutput, " ":U)
                    iValue = DataTypeHelper:ToInteger (cValue)
                    .

                IF iValue = ? THEN
                    cErrorMessage = "Unexpected Error occured for '&1'":U.
            END.

            IF NOT lReturnedValue THEN
                cErrorMessage = "No MX record found for '&1'.":U.
        END.

        LogManager:WriteMessage ("ErrorMessage: ":U + cErrorMessage, "NetworkHelper":U).

        IF cErrorMessage > "":U THEN
/*            UNDO, THROW NEW DomainValidationException (cErrorMessage, pcDomain).*/
            RETURN FALSE.

        RETURN TRUE.

        FINALLY:

            IF OPSYS BEGINS "WIN":U AND
               FileHelper:Exists (QUOTER (cFilename)) THEN
                OS-DELETE VALUE (QUOTER (cFilename)).

        END FINALLY.

    END METHOD.

END CLASS.
