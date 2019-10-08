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
    File        : ISmartRepositoryAttributeLookupService
    Purpose     : Interface for services that can be used as lookups for
                  the class, object master or instance attribute (DIALOG)
    Syntax      :
    Description : This is the actual service interface for the attributes used
                  by the SmartComponent Library and SmartComponents.Web2.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 16 18:25:24 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.Repository.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.Repository.ISmartRepositoryAttributeLookupService
&IF NOT PROVERSION BEGINS "10.2B" &THEN
    INHERITS IRepositoryAttributeLookupService
&ENDIF
    :

    /*------------------------------------------------------------------------------
        Purpose: Displays the property Dialog
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param pcObjectTypeId The GUID of the Object Type
        @param pcObjectMasterId The GUID of the Object Master
        @param pcObjectInstanceId The GUID of the Obejct Instance
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @param poOwnerForm The reference to the Form to be used as the owner for the Dialog
        @param poValue INPUT-OUTPUT The value to update (BOX ABL primitive values)
        @return Logical value indicating if OK (true) or Cancel (false) was clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ShowPropertyDialog (pcAttributeLabel AS CHARACTER,
                                              pcObjectTypeId AS CHARACTER,
                                              pcObjectMasterId AS CHARACTER,
                                              pcObjectInstanceId AS CHARACTER,
                                              phDesignAttributeTable AS HANDLE,
                                              poOwnerForm AS System.Windows.Forms.Form,
                                              INPUT-OUTPUT poValue AS System.Object).

END INTERFACE.
