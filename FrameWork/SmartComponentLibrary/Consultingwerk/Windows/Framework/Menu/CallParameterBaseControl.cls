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
    File        : CallParameterBaseControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri Dec 07 21:18:59 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Controls.*          FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.CallParameterBaseControl
    INHERITS EmbeddedAbstractBaseControl:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CallParameterBaseControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CallParameterBaseControl ():
        SUPER ().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CallParameterBaseControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CallParameterBaseControl ():

    END DESTRUCTOR .

END CLASS.
