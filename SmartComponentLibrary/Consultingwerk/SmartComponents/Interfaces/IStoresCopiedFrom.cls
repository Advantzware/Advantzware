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
    File        : IStoresCopiedFrom
    Purpose     : Interface for SmartDataAdapter implementations that
                  support storing the source record key when a record
                  is copied
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 16 11:59:07 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IStoresCopiedFrom:

    /**
     * Purpose: Returns if the first table of the Adapter provides a field
     *          SmartCopiedFrom
     * Notes:   Used by the SmartViewerControl and SmartUpdatableBrowser during copy
     * @return Logical value indicating if the first table of the Adapter provides a field SmartCopiedFrom
     */
    METHOD PUBLIC LOGICAL HasCopiedFromField ().

    /**
     * Purpose:  Stores the source record key information with the new record (copy)
     * Notes:
     * @param pcSourceRecordKey The record key of the source record to store with the copied record
     */
    METHOD PUBLIC VOID StoreCopiedFrom (pcSourceRecordKey AS CHARACTER).

END INTERFACE.
