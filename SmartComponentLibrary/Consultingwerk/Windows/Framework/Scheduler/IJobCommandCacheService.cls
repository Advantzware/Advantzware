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
    File        : IJobCommandCacheService
    Purpose     : Service Interface for services caching the available
                  ISchedulerJobCommand implementations
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 14:09:03 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

{Consultingwerk/products.i}

USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.Scheduler.IJobCommandCacheService:

    /*------------------------------------------------------------------------------
        Purpose: ValueList of JobCommands
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobCommands AS Infragistics.Win.ValueList NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Populates the ValueList properties of this class
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeService ().

END INTERFACE.