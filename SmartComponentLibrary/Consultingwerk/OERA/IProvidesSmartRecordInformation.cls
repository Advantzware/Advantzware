/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IProvidesSmartRecordInformation
    Purpose     : Interface for Data Access classes that provide the 
                  SmartRecordKey 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 05 12:07:11 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IProvidesSmartRecordInformation
    &IF NOT PROVERSION BEGINS "10" &THEN
        INHERITS Consultingwerk.OERA.IDataAccess
    &ENDIF :  
  
    /*------------------------------------------------------------------------------
        Purpose: Assign SmartRecordKey and Attachment/Comment function 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignSmartRecordInformation () .   
  
END INTERFACE.