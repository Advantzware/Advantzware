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
    File        : Const
    Purpose     : Generic framework constants
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 13 10:42:23 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Const:

    /**
     * Purpose: The list of built in ABL data types
     * Notes:   BLOB,CHARACTER,CLOB,COM-HANDLE,DATE,DATETIME,DATETIME-TZ,DECIMAL
     *          HANDLE,INT64,INTEGER,LOGICAL,Progress.Lang.Object,RECID,ROWID
     */
    DEFINE PUBLIC STATIC PROPERTY ABL_DATA_TYPES AS CHARACTER INITIAL 'BLOB,CHARACTER,CLOB,COM-HANDLE,DATE,DATETIME,DATETIME-TZ,DECIMAL,HANDLE,INT64,INTEGER,LOGICAL,Progress.Lang.Object,RECID,ROWID':U NO-UNDO
    GET.

    /**
     * Purpose: The longest possible delimited list pattern for a SUBSTITUTE operation
     * Notes:   &9 (ampersand 9) represents the list delimiter
     *          &1&9&2&9&3&9&4&9&5&9&6&9&7&9&8
     */
    DEFINE PUBLIC STATIC PROPERTY DELIMITED_LIST_FOR_SUBSTITUTE AS CHARACTER INITIAL '&1&9&2&9&3&9&4&9&5&9&6&9&7&9&8':U NO-UNDO
    GET.

    /**
     * Purpose: Returns a comma delimited list of characters that are not allowed
     *          in file names on Windows
     * Notes:   https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#naming_conventions
     */
    DEFINE PUBLIC STATIC PROPERTY ILLEGAL_CHARACTERS_IN_FILENAMES AS CHARACTER INITIAL '<,>,:,",/,~\,|,?,*':U NO-UNDO
    GET.

    /**
     * Purpose: The maximum value a 32 bit integer variable can hold
     * Notes:   2147483647
     */
    DEFINE PUBLIC STATIC PROPERTY MAX_INTEGER AS INTEGER NO-UNDO INITIAL 2147483647
    GET.

    /**
     * Purpose: The maximum value a 64 bit integer variable can hold
     * Notes:   9223372036854775807
     */
    DEFINE PUBLIC STATIC PROPERTY MAX_INT64 AS INT64 NO-UNDO INITIAL 9223372036854775807
    GET.

    /**
     * Purpose: The minimum value a 32 bit integer variable can hold
     * Notes:   -2147483648
     */
    DEFINE PUBLIC STATIC PROPERTY MIN_INTEGER AS INTEGER NO-UNDO INITIAL -2147483648
    GET.

    /**
     * Purpose: The minimum value a 64 bit integer variable can hold
     * Notes:   -9223372036854775808
     */
    DEFINE PUBLIC STATIC PROPERTY MIN_INT64 AS INT64 NO-UNDO INITIAL -9223372036854775808
    GET.

    /**
     * Purpose: The number of milliseconds per day
     * Notes:   86.400.000
     */
    DEFINE PUBLIC STATIC PROPERTY MSECS_PER_DAY AS INTEGER NO-UNDO INITIAL 86400000
    GET.

    /**
     * Purpose: The number of milliseconds per hour
     * Notes:   3.600.000
     */
    DEFINE PUBLIC STATIC PROPERTY MSECS_PER_HOUR AS INTEGER NO-UNDO INITIAL 3600000
    GET.

    /**
     * Purpose: The number of milliseconds per minute
     * Notes:   60.000
     */
    DEFINE PUBLIC STATIC PROPERTY MSECS_PER_MINUTE AS INTEGER NO-UNDO INITIAL 60000
    GET.

    /**
     * Purpose: The number of milliseconds per second
     * Notes:   1.000
     */
    DEFINE PUBLIC STATIC PROPERTY MSECS_PER_SECOND AS INTEGER NO-UNDO INITIAL 1000
    GET.

    /**
     * Purpose: Constructor for the Const class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PRIVATE Const ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
