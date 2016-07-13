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
    File        : ILlLicensePrivider
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 23 22:36:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Windows.ListAndLabel.ILlLicenseProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List & Label License Info
        Notes:   See List & Label Documentation, LL_OPTIONSTR_LICENSINGINFO
        @return The list an Label License Info, usually a 6 character string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetLicenseInfo ().

END INTERFACE.