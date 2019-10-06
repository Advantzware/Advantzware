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
    File        : Stopwatch
    Purpose     : Elternative to ETIME
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 14 12:19:11 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.Stopwatch:

    DEFINE VARIABLE dtStart AS DATETIME-TZ NO-UNDO INIT NOW .
    DEFINE VARIABLE dtStop  AS DATETIME-TZ NO-UNDO INIT ? .

    /**
     * Purpose: Returns the elapsed time
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Elapsed AS INT64 NO-UNDO
    GET:

        IF dtStop <> ? THEN
            RETURN INTERVAL (dtStop, dtStart, "milliseconds":U) .
        ELSE
            RETURN INTERVAL (NOW, dtStart, "milliseconds":U) .

    END GET.

    /**
     * Purpose: Starts the Stopwatch
     * Notes:
     */
    METHOD PUBLIC VOID Start():

        ASSIGN dtStart = NOW
               dtStop  = ? .

    END METHOD.

    /**
     * Purpose: Stops the Stopwatch
     * Notes:
     * @return The number of elapsed milliseconds
     */
    METHOD PUBLIC INT64 Stop ():

        ASSIGN dtStop = NOW .

        RETURN INTERVAL (dtStop, dtStart, "milliseconds":U) .

    END METHOD.

END CLASS.
