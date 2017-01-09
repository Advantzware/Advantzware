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
    File        : IFetchDataBySkip
    Purpose     : Interface for IFetchDataRequest implementations that 
                  do provide the "Skip" parameter as required for the 
                  server side batching/paging with JSDO/Kendo UI
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 25 13:54:33 CEST 2015
    Notes       : SCL-819/SCL-822
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IFetchDataBySkip:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of records to be skipped from the beginning when
                 reading data 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Skip AS INTEGER NO-UNDO 
	GET.

END INTERFACE.
