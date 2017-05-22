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
    File        : ISmartDataSourceWithDataset
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 23 21:52:02 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartDataSourceWithDataset:

    /**
     * Purpose: Returns the handle to the ProDataset instance managed by the
     *          SmartBusinessEntityAdapter
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataSet AS HANDLE NO-UNDO
    GET.

END INTERFACE.