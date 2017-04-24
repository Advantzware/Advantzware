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
    File        : startGenerateLlConst.p
    Purpose     : Startup procedure for the GenerateLlConst class

    Syntax      :

    Description :

    Author(s)   : Mohamed Elcaid / Consultingwerk Ltd.
    Created     : Mon Jun 27 17:46:06 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*                 FROM PROPATH .
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .

{Consultingwerk/products.i }

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE oGenerator AS GenerateLlConst NO-UNDO.

oGenerator = NEW GenerateLlConst ().

oGenerator:GenerateClassFile ("Consultingwerk/Windows/ListAndLabel/cmLL{&ListAndLabelVersion}.i":U ).

CATCH ple AS Progress.Lang.Error :
    ErrorHelper:ShowErrorMessage (ple).
END CATCH.

FINALLY:
    GarbageCollectorHelper:DeleteObject (oGenerator).
END FINALLY.

