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
    File        : FormatHelper
    Purpose     :
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Jun 03 15:26:16 CEST 2009
    Notes       : OpenEdge Help - FORMAT option, format phrase
                  Infragistics Docu - http://help.infragistics.com/NetAdvantage/WinForms/2012.2/CLR4.0/html/Infragistics4.Win.UltraWinMaskedEdit.v12.2~Infragistics.Win.UltraWinMaskedEdit.UltraMaskedEdit~InputMask.html?page=Infragistics4.Win.UltraWinMaskedEdit.v12.2~Infragistics.Win.UltraWinMaskedEdit.UltraMaskedEdit~InputMask.html

                  Problematic Masks:

                  FormatHelper:ToInputMask("CHARACTER", "!(4)x(8)")
                  The ! sign means that the following four letters will be uppercased.
                  Infragistics does not support this functionality like Progress does.
                  Either the full Mask will be uppercased by adding the > sign at the
                  beginning or nothing will be upper or lowercased.

                  Infragistics only supports converting all following characters.
                  > Convert all the characters that follow to uppercase.
                  < Convert all the characters that follow to lowercase.

                  Uppercasing the InputMask if only ! are present in the Progress
                  Format Expression like "!(4)" is implemented.

                  FormatHelper:ToInputMask("INTEGER", "HH:MM:SS.SSS+HH:MM")
                  The Time Mask when passed as an INTEGER is not analysed so H, M and S
                  will be replaced by n but the Mask will not be validated in any way.
                  For Time input only a Mask like "HH:MM:SSSS" without a dot within the
                  seconds and maximum four second placeholders is valid. The Timezone is
                  not supported by Infragistics.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.FormatHelper :

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor.
        Notes:   There's no need to create instances of the helper classes
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED FormatHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns an Infragistics Date Mask.
                 Invoked by ToInputMask.
        Notes:
        @param pcDateMask The ABL DATE FORMAT string
        @return The matching Infragistics InputMask for the date field
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC CHARACTER BuildDateMask (INPUT pcDateMask AS CHARACTER):

        pcDateMask = REPLACE (pcDateMask, ".":U, "/":U).
        pcDateMask = REPLACE (pcDateMask, "-":U, "/":U).

        IF pcDateMask MATCHES "99/99/9999":U THEN
            RETURN "mm/dd/yyyy":U.
        ELSE
            RETURN "mm/dd/yy":U.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an Infragistics Time Mask.
                 Invoked by ToInputMask.
        Notes:
        @param pcTimeMask The ABL TIME FORMAT string
        @return The matching Infragistics InputMask for the time field
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC CHARACTER BuildTimeMask (INPUT pcTimeMask AS CHARACTER):

        pcTimeMask = REPLACE (pcTimeMask, ".":U, "":U).

        pcTimeMask = REPLACE (pcTimeMask, "H":U, "h":U).
        pcTimeMask = REPLACE (pcTimeMask, "M":U, "m":U).
        pcTimeMask = REPLACE (pcTimeMask, "S":U, "s":U).

        RETURN pcTimeMask.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a display Format to be used in the Grid build from the edit
                 Mask returned from the ToInputMask method.
        Notes:
        @param pcDataType The Progress Data-Type that belongs to the FormatMask
        @param pcIGInputMask The Infragistics input mask as returned by the ToInputMask method
        @return The Infragistics format string matching the Infragistics edit mask
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetFormatFromInputMask (pcDataType    AS CHARACTER,
                                                           pcIGInputMask AS CHARACTER):

        DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO.

        cFormat = REPLACE (REPLACE (pcIGInputMask, "~{LOC~}-":U, "":U), "~{LOC~}":U, "":U).

        CASE pcDataType:
            WHEN "INTEGER":U
            OR WHEN "INT64":U
            OR WHEN "DECIMAL":U THEN DO:
                cFormat = REPLACE (REPLACE (cFormat, "n.nn":U, "0.00":U), "n":U, "#":U).

                cFormat = REPLACE (cFormat, "%":U, "'%":U).
            END.
            OTHERWISE .
        END CASE .

        RETURN cFormat.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the width pixels for a given ABL format
        Notes:   Creates a fill-in on the fly and assigns the format. Return value
                 is dependent on the current font settings
        @param pcFormat The format to measure
        @return The width in pixels for the given format
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER GetFormatWidthPixels (pcFormat AS CHARACTER):

        DEFINE VARIABLE hFillIn AS HANDLE  NO-UNDO .
        DEFINE VARIABLE iWidth  AS INTEGER NO-UNDO .

        CREATE FILL-IN hFillIn
          ASSIGN FORMAT = pcFormat .

        iWidth = hFillIn:WIDTH-PIXELS .

        RETURN iWidth .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hFillIn) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the required integer format for an upper limit, including
                 leading zero's
        Notes:   Useful for sequences of field names (EXTENT field resolution)
        @param piMaxValue The maximum value that should be visualized using the FORMAT
        @return The FORMAT that can visualize up to the given maximum value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER IntegerFormatForMax (piMaxValue AS INTEGER):

        DEFINE VARIABLE iLength AS INTEGER   NO-UNDO .

        ASSIGN iLength = LENGTH (STRING (piMaxValue)) .

        RETURN FILL ("9":U, iLength) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an interpreted InputMask.
                 "x(4)"     -> "xxxx"
                 "!(4)x(8)" -> "!!!!xxxxxxxx"
        Notes:
        @param cFormat An ABL CHARCTER FORMAT using the repeat mask syntax, e.g. x(8)
        @return The ABL CHARACTER FORMAT resolved by the repaiting characters
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC CHARACTER ResolveRepeatingChars (INPUT cFormat AS CHARACTER):

        DEFINE VARIABLE iBracketOpen  AS INTEGER     NO-UNDO INITIAL 2.
        DEFINE VARIABLE iBracketClose AS INTEGER     NO-UNDO.
        DEFINE VARIABLE iRepeat       AS INTEGER     NO-UNDO.
        DEFINE VARIABLE cRepeat       AS CHARACTER   NO-UNDO.

        ASSIGN iBracketOpen = INDEX (cFormat, "(":U, iBracketOpen).

        DO WHILE iBracketOpen > 0:

            ASSIGN iBracketClose = INDEX (cFormat, ")":U, iBracketOpen)
                   cRepeat       = SUBSTRING (cFormat,
                                              iBracketOpen + 1,
                                              iBracketClose - iBracketOpen - 1)
                   iRepeat       = INTEGER (cRepeat) NO-ERROR .

            IF iRepeat > 0 THEN DO:
                ASSIGN cRepeat = SUBSTRING (cFormat, iBracketOpen - 1, 1)

                       cFormat = (IF iBracketOpen > 2 THEN SUBSTRING (cFormat, 1, iBracketOpen - 2) ELSE "":U) +
                                 FILL (cRepeat, iRepeat) +
                                 SUBSTRING (cFormat, iBracketClose + 1) .
            END.

            ASSIGN iBracketOpen = INDEX (cFormat, "(":U, iBracketOpen + 1).

        END.

        RETURN cFormat.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Date and the Time part of an ImputMask.
        Notes:
        @param pcMask The ABL FORMAT for a DATETIME or DATETIME-TZ value
        @param pcDateMask OUTPUT The Date part of the FORMAT
        @param pcTimeMask OUTPUT The Time part of the FORMAT
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC VOID SplitDateAndTimeMasks (INPUT  pcMask     AS CHARACTER,
                                                      OUTPUT pcDateMask AS CHARACTER,
                                                      OUTPUT pcTimeMask AS CHARACTER):

        DEFINE VARIABLE iIndexSpace AS INTEGER NO-UNDO.

        ASSIGN
            pcDateMask  = ENTRY (1, pcMask, " ":U)
            iIndexSpace = INDEX (pcMask, " ":U, 1)
            pcTimeMask  = TRIM (SUBSTRING (pcMask, iIndexSpace, LENGTH (pcMask) - iIndexSpace))
            pcTimeMask  = TRIM (ENTRY (1, pcTimeMask, " ":U))
            pcTimeMask  = TRIM (ENTRY (1, pcTimeMask, "+":U))
            .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: Returns an Infragistics InputMask translated from a Progress Edit
                 Mask.
        Notes:
        @param pcDataType The Progerss Data-Type that belongs to the FORMAT mask
        @param pcProgressEditMask The Progress ABL FORMAT mask, e.g. X(8)
        @return The Infragistics InputMask matching the ABL FORMAT mask
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ToInputMask (INPUT pcDataType         AS CHARACTER,
                                                INPUT pcProgressEditMask AS CHARACTER):

        DEFINE VARIABLE cDateMask      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTimeMask      AS CHARACTER NO-UNDO.

        DEFINE VARIABLE lUppercaseMask AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO.

        CASE pcDataType:
            WHEN "DATE":U THEN DO:
                RETURN BuildDateMask (pcProgressEditMask).
            END.
            WHEN "DATETIME":U
            OR WHEN "DATETIME-TZ":U THEN DO:
                IF NUM-ENTRIES (pcProgressEditMask, " ":U) > 1 THEN DO:
                    SplitDateAndTimeMasks (pcProgressEditMask, OUTPUT cDateMask, OUTPUT cTimeMask).

                    RETURN SUBSTITUTE ("~{LOC~}&1 &2":U,
                                       BuildDateMask (cDateMask),
                                       BuildTimeMask (cTimeMask)).
                END.
                ELSE
                    RETURN BuildDateMask (pcProgressEditMask).
            END.
            WHEN "INTEGER":U
            OR WHEN "INT64":U
            OR WHEN "DECIMAL":U THEN DO:
                pcProgressEditMask = REPLACE (pcProgressEditMask, "cr":U, "":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "db":U, "":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "dr":U, "":U).

                /* Marko Rüterbories, Consultingwerk Ltd. 12.12.2013
                   SCL-137 Replace 9 with 0 to have leading zeros and not an
                   empty display */
                pcProgressEditMask = REPLACE (pcProgressEditMask, "9":U, "0":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "z":U, "n":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, ">":U, "n":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "*":U, "n":U).

                /* HH:MM:SS */
                pcProgressEditMask = REPLACE (pcProgressEditMask, "h":U, "n":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "m":U, "n":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "s":U, "n":U).

                IF pcProgressEditMask MATCHES "*-":U THEN
                    pcProgressEditMask = "-":U + REPLACE (pcProgressEditMask, "-":U, "":U).

                IF pcProgressEditMask MATCHES "*+":U THEN
                    pcProgressEditMask = "+":U + REPLACE (pcProgressEditMask, "+":U, "":U).

                IF  pcProgressEditMask MATCHES "(*)":U THEN DO:
                    pcProgressEditMask = "-":U + REPLACE (REPLACE (pcProgressEditMask, "(":U, "":U), ")":U, "":U).
                END.

                RETURN SUBSTITUTE ("~{LOC~}&1":U, pcProgressEditMask).
            END.
            OTHERWISE DO:
                /* If not handled above treat the EditMask like a CHARACTER */

                pcProgressEditMask = ResolveRepeatingChars (pcProgressEditMask).

                /* Uppercasing the InputMask if only ! are present in the Progress Format Expression "!(4)" */
                IF INDEX (pcProgressEditMask, "!":U) > 0 THEN DO:
                    ASSIGN lUppercaseMask = TRUE.

                    DO i = 1 TO LENGTH (pcProgressEditMask):
                        IF SUBSTRING (pcProgressEditMask, i, 1) <> "!":U THEN
                            /* Not possible to return an uppercased Input Mask */
                            ASSIGN lUppercaseMask = FALSE.
                    END.

                    IF lUppercaseMask THEN
                        ASSIGN pcProgressEditMask = ">":U + pcProgressEditMask.
                END.

                pcProgressEditMask = REPLACE (pcProgressEditMask, "x":U, "C":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "n":U, "a":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "a":U, "?":U).
                pcProgressEditMask = REPLACE (pcProgressEditMask, "!":U, "?":U).

                RETURN pcProgressEditMask.
            END.
        END CASE.

        CATCH e AS Progress.Lang.Error:
            MESSAGE "Error while translating a Mask!"{&TRAN} SKIP
                    pcProgressEditMask SKIP
                    e:GetMessage (1)
                VIEW-AS ALERT-BOX ERROR TITLE "Format not recognized"{&TRAN} .
        END CATCH.
    END METHOD.

END CLASS.
