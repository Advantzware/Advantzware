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
    File        : IDataAccessCanFind
    Purpose     : Interface for DataAccess implemementation that support
                  the CanFind method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 11 07:24:01 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH.

INTERFACE Consultingwerk.OERA.IDataAccessCanFind:  

    /*------------------------------------------------------------------------------
        Purpose: Returns a TRUE value if a record is found that meets the specified 
                 FIND criteria; otherwise it returns FALSE. CAN-FIND does not make 
                 the record available to the Business Entity or it's caller. 
        Notes:   Result will be assigned to the Result property of the CanFindParameter
                 instance
        @param poParameter The CanFindParameter with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CanFind (poParameter AS CanFindParameter) .
      
END INTERFACE.
