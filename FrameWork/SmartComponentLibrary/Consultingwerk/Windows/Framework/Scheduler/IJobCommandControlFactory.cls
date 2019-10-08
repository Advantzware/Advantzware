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
    File        : IJobCommandControlFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 11:21:50 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Windows.Framework.Scheduler.IJobCommandControlFactory:  

    /*------------------------------------------------------------------------------
        Purpose: Returns an instance of a JobCommandControl based on the type of
                 the JobCommand
        Notes:   
        @param poJobCommandType The class reference of the JobCommand for which a UserControl should be provided 
        @return The specialized JobCommandControl instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.UserControl GetJobCommandControl (poJobCommandType AS Progress.Lang.Class).
  
END INTERFACE.