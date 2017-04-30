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
    File        : saasynctaskhandler.p
    Purpose     : Generic async AppServer callback

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 11 14:36:09 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER poServiceAdapter AS ServiceAdapter       NO-UNDO .
DEFINE INPUT PARAMETER poContext        AS Progress.Lang.Object NO-UNDO .
DEFINE INPUT PARAMETER poParameter      AS ISerializable        NO-UNDO .

DEFINE INPUT  PARAMETER phDataset1 AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER phDataset2 AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER phDataset3 AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER phDataset4 AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER phDataset5 AS HANDLE NO-UNDO.

/* ***************************  Main Block  *************************** */

PROCEDURE AsyncTaskCompleted:

    DEFINE INPUT PARAMETER plcDataset1     AS LONGCHAR NO-UNDO .
    DEFINE INPUT PARAMETER plcDataset2     AS LONGCHAR NO-UNDO .
    DEFINE INPUT PARAMETER plcDataset3     AS LONGCHAR NO-UNDO .
    DEFINE INPUT PARAMETER plcDataset4     AS LONGCHAR NO-UNDO .
    DEFINE INPUT PARAMETER plcDataset5     AS LONGCHAR NO-UNDO .
    DEFINE INPUT PARAMETER plcParameterOut AS LONGCHAR NO-UNDO .

    DEFINE VARIABLE oParameter AS ISerializable NO-UNDO .

    IF plcDataset1 > "":U AND VALID-HANDLE (phDataset1) THEN DO:
        phDataset1:READ-XML ("LONGCHAR":U, plcDataset1, "EMPTY":U, ?, ?) .
    END.

    IF plcDataset2 > "":U AND VALID-HANDLE (phDataset2) THEN DO:
        phDataset2:READ-XML ("LONGCHAR":U, plcDataset2, "EMPTY":U, ?, ?) .
    END.

    IF plcDataset3 > "":U AND VALID-HANDLE (phDataset3) THEN DO:
        phDataset3:READ-XML ("LONGCHAR":U, plcDataset3, "EMPTY":U, ?, ?) .
    END.

    IF plcDataset4 > "":U AND VALID-HANDLE (phDataset4) THEN DO:
        phDataset4:READ-XML ("LONGCHAR":U, plcDataset4, "EMPTY":U, ?, ?) .
    END.

    IF plcDataset5 > "":U AND VALID-HANDLE (phDataset5) THEN DO:
        phDataset5:READ-XML ("LONGCHAR":U, plcDataset5, "EMPTY":U, ?, ?) .
    END.

    IF plcParameterOut > "":U THEN
        ASSIGN oParameter = Serializable:DeserializeInstance (plcParameterOut) .

    /* Mike Fechner, Consultingwerk Ltd. 01.11.2014
       Raise Service Adapter event - SELF is the current ASYNC-REQUEST handle */
    poServiceAdapter:OnAsyncTaskCompleted (NEW AsyncTaskCompletedEventArgs (poContext,
                                                                            SELF,
                                                                            oParameter)) .

    FINALLY:
        ASSIGN poServiceAdapter = ?
               poContext        = ?
               poParameter      = ? .

        DELETE OBJECT THIS-PROCEDURE .
    END FINALLY.

END PROCEDURE .
