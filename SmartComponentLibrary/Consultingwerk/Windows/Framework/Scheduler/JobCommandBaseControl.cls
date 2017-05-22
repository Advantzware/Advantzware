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
    File        : JobCommandBaseControl
    Purpose     : Base Control to maintain specialized scheduler job commands
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Fri May 27 13:49:59 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Scheduler.*   FROM PROPATH .
USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH .
USING Progress.Lang.*                             FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Framework.Scheduler.JobCommandBaseControl
    INHERITS EmbeddedAbstractBaseControl:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CallParameterBaseControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JobCommandBaseControl ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CallParameterBaseControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC JobCommandBaseControl ():

    END DESTRUCTOR .

END CLASS.
