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
    File        : DomainValidationException
    Purpose     : Exception thrown by NetworkHelper:VerifyMXRecord
                  when the Domain lookup did not return a valid MX
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Nov 29 10:13:09 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH.
USING Progress.Lang.*             FROM PROPATH.

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.Exceptions.DomainValidationException
    INHERITS Exception {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the Domain that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DomainName AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the DomainValidationException class
       Notes:   The pcDomainName will be substituted for &1 the pcErrorMessage
       @param pcErrorMessage The error message for the exception
       @param pcDomainName The name of the Domain that caused the exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DomainValidationException (pcErrorMessage AS CHARACTER,
                                                  pcDomainName AS CHARACTER):

        SUPER (SUBSTITUTE (pcErrorMessage, pcDomainName), 0).

        ASSIGN THIS-OBJECT:DomainName = pcDomainName.

    END CONSTRUCTOR.

END CLASS.