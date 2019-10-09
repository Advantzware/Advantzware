/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ValidationHelper
    Purpose     : Provides Assertion-Style validation routines for ProDataset
                  buffers
    Syntax      : Static methods
    Description : This class is a facade to the Consultingwerk.OERA.Validate
                  class. This class can be used to avoid compile error caused
                  by conflicts of code using the abreviated class name Validate
                  when referenceing the static methods and some database tables
                  contian fields that are named Validate....
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 23 11:50:46 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.ValidationHelper
    INHERITS Consultingwerk.OERA.Validate:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Validate class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ValidationHelper ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
