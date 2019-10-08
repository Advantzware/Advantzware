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
    File        : RecordLockedException
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 31 10:26:27 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.RecordLockedException
    INHERITS Exception:

    /**
     * Purpose: Gets and sets the name of the buffer that is locked
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the ROWID of the record that is locked
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Rowid AS ROWID NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the RecordLockedException class
     * Notes:
     * @param pcErrorString The error message of this exception
     * @param piMessageNumber The error message number of this exception
     * @param pcBufferName The name of the buffer that caused this exception
     * @param proRowid The ROWID of the record that caused this exception
     */
    CONSTRUCTOR PUBLIC RecordLockedException (pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcBufferName AS CHARACTER,
                                              proRowid AS ROWID):
        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:BufferName = pcBufferName
               THIS-OBJECT:Rowid      = proRowid .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the RecordLockedException class
     * Notes:
     * @param pcErrorString The error message of this exception
     * @param piMessageNumber The error message number of this exception
     */
    CONSTRUCTOR PUBLIC RecordLockedException (pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the RecordLockedException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC RecordLockedException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
