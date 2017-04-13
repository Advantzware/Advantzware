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
    File        : InvalidTypeException
    Purpose     : An error object thrown to signal invalid types
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 28 14:04:34 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.InvalidTypeException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the (WIDGET-)TYPE that was expected by the code raising this
     *          error
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ExpectedType AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the (WIDGET-)TYPE that was received by the code raising this
     *          error
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Type AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor of the InvalidTypeException class
     * Notes:
     * @param pcExpectedType The expected type
     * @param pcType The current type
     */
    CONSTRUCTOR PUBLIC InvalidTypeException (pcExpectedType AS CHARACTER,
                                             pcType AS CHARACTER):

        SUPER (SUBSTITUTE ("Type &1 should be &2."{&TRAN}, pcType, pcExpectedType), 0).

        ASSIGN THIS-OBJECT:ExpectedType = pcExpectedType
               THIS-OBJECT:Type         = pcType
               .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the InvalidTypeException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcExpectedType The expected type
     * @param pcType The current type
     */
    CONSTRUCTOR PUBLIC InvalidTypeException (poInnerException AS Error,
                                             pcExpectedType AS CHARACTER,
                                             pcType AS CHARACTER):

        SUPER (poInnerException,
               SUBSTITUTE ("Type &1 should be &2."{&TRAN},
                           pcType,
                           pcExpectedType),
               0).

        ASSIGN THIS-OBJECT:ExpectedType = pcExpectedType
               THIS-OBJECT:Type         = pcType
               .

    END CONSTRUCTOR.

END CLASS.
