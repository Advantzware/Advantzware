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
    File        : IRelationDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 24 13:32:16 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.System.IRelationDataAccess:

    { Consultingwerk/SmartFramework/System/dsMissingRelations.i &BY-REFERENCE=BY-REFERENCE}

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of missing Database Relations to the caller
        Notes:
        @param dsMissingRelations INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMissingDatabaseRelations (INPUT-OUTPUT DATASET dsMissingRelations,
                                                    poParameter AS Consultingwerk.SmartFramework.System.GetMissingDatabaseRelationsParameter) .

END INTERFACE.
