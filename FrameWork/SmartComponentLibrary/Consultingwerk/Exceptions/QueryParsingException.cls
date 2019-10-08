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
    File        : QueryParsingException
    Purpose     : Exception thrown by the Consultingwerk.QueryParser class
                  when an unsupported query term was foudn
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 25 10:23:17 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.QueryParsingException
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the String that caused the QueryParsingException
     * Notes:
     */
    DEFINE PUBLIC PROPERTY StringToParse AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor of the QueryParsingException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param pcTitle The title for the Exception
     * @param pcStringToParse The string that caused the QueryParsingException
     */
    CONSTRUCTOR PUBLIC QueryParsingException (poInnerException AS Progress.Lang.Error,
                                              pcErrorString AS CHARACTER,
                                              pcTitle AS CHARACTER,
                                              pcStringToParse AS CHARACTER):

        SUPER (poInnerException, pcErrorString, pcTitle).

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Error parsing: &1":U, pcStringToParse), 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the QueryParsingException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this QueryParsingException object
     * @param piMessageNumber The error message number associated with this Exception object
     * @param pcTitle The title for the Exception
     * @param pcStringToParse The string that caused the QueryParsingException
     */
    CONSTRUCTOR PUBLIC QueryParsingException (poInnerException AS Progress.Lang.Error,
                                              pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcTitle AS CHARACTER,
                                              pcStringToParse AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber, pcTitle).

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Error parsing: &1":U, pcStringToParse), 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the QueryParsingException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcTitle The title for the Exception
     * @param pcStringToParse The string that caused the QueryParsingException
     */
    CONSTRUCTOR PUBLIC QueryParsingException (poInnerException AS Progress.Lang.Error,
                                              pcTitle AS CHARACTER,
                                              pcStringToParse AS CHARACTER):

        SUPER (poInnerException, pcTitle).

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Error parsing: &1":U, pcStringToParse), 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the QueryParsingException class
     * Notes:
     * @param pcErrorString The error message associated with this QueryParsingException object
     * @param pcTitle The title for the Exception
     * @param pcStringToParse The string that caused the QueryParsingException
     */
    CONSTRUCTOR PUBLIC QueryParsingException (pcErrorString AS CHARACTER,
                                              pcTitle AS CHARACTER,
                                              pcStringToParse AS CHARACTER):


        SUPER (pcErrorString, pcTitle).

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Error parsing: &1":U, pcStringToParse), 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the QueryParsingException class
     * Notes:
     * @param pcErrorString The error message associated with this QueryParsingException object
     * @param piMessageNumber The error message number associated with this QueryParsingException object
     * @param pcTitle The title for the Exception
     * @param pcStringToParse The string that caused the QueryParsingException
     */
    CONSTRUCTOR PUBLIC QueryParsingException (pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcTitle AS CHARACTER,
                                              pcStringToParse AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber, pcTitle).

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Error parsing: &1":U, pcStringToParse), 0) .

    END CONSTRUCTOR.

END CLASS.
