/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CodepageHelper
    Purpose     : Provides helper methods for code page operations or
                  validation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 17 07:55:52 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Util.CodepageHelper:

    /**
     * Purpose: Verifies if a given CHARACTER or LONGCHAR value complies with a
     *          target code page
     * Notes:   Tries to assign the value to a LONGCHAR fixed to the target codepage
     *          and handles error ** Unable to update  Field. (142)
     * @param plcValue The LONGCHAR or CHARACTER value to test
     * @param pcTargetCodepage The target code page to verify against
     * @return Logical value indicating of the value complies with the target codepage
     */
    METHOD PUBLIC STATIC LOGICAL CompliesWithCodepage (plcValue AS LONGCHAR,
                                                       pcTargetCodepage AS CHARACTER):

        DEFINE VARIABLE lcTarget AS LONGCHAR NO-UNDO .

        FIX-CODEPAGE (lcTarget) = pcTargetCodepage . /* throw errors from invalid codepage,
                                                        e.g. Code page attribute table for ISO88xyz was not found in convmap.cp. (912) */

        DO ON ERROR UNDO, THROW:
            /* Attempt to assign plcValue to LONGCHAR fixed to target code page */
            lcTarget = plcValue .

            CATCH err AS Progress.Lang.Error:
                /* ** Unable to update  Field. (142) */
                IF err:GetMessageNum (1) = 142 THEN
                    RETURN FALSE .
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

        RETURN TRUE .

        FINALLY:
            lcTarget = "":U .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Verifies if a given ProDataset or Temp-Table complies with a target
     *          code page
     * Notes:   Saves the temp-table as XML to an UTF-8 LONGCHAR and verifies code page
     *          compliance of that LONGCHAR
     * @param phData The Handle of the ProDataset or Temp-Table to test
     * @param pcTargetCodepage The target code page to verify against
     * @return Logical value indicating of the value complies with the target codepage
     */
    METHOD PUBLIC STATIC LOGICAL CompliesWithCodepage (phData AS HANDLE,
                                                       pcTargetCodepage AS CHARACTER):

        DEFINE VARIABLE lcData AS LONGCHAR NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phData """phData"":U"} .

        IF phData:TYPE <> Consultingwerk.WidgetTypeEnum:TempTable THEN
           {Consultingwerk/Assertion/HandleAssert/WidgetType.i phData Consultingwerk.WidgetTypeEnum:Dataset} .

        FIX-CODEPAGE (lcData) = "utf-8":U .

        phData:WRITE-XML ("LONGCHAR":U, lcData, FALSE, ?, ?) .

        RETURN CodepageHelper:CompliesWithCodepage (lcData, pcTargetCodepage) .

        FINALLY:
            ASSIGN lcData = "":U .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Verifies if a given ISerializable instance complies with a target
     *          code page
     * Notes:   Writes the ISerializable to an UTF-8 LONGCHAR and verifies code page
     *          compliance of that LONGCHAR
     * @param poData The reference to the ISerializable instance to test
     * @param pcTargetCodepage The target code page to verify against
     * @return Logical value indicating of the value complies with the target codepage
     */
    METHOD PUBLIC STATIC LOGICAL CompliesWithCodepage (poData AS Consultingwerk.ISerializable,
                                                       pcTargetCodepage AS CHARACTER):

        DEFINE VARIABLE lcData AS LONGCHAR NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poData """phData"":U"} .

        FIX-CODEPAGE (lcData) = "utf-8":U .

        lcData = poData:Serialize() .

        RETURN CodepageHelper:CompliesWithCodepage (lcData, pcTargetCodepage) .

        FINALLY:
            ASSIGN lcData = "":U .
        END FINALLY.

    END METHOD .

END CLASS.
