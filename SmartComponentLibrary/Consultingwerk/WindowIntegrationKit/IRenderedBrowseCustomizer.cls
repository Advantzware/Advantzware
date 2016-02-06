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
    File        : IRenderedBrowseColumnCustomizer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 11 14:32:24 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.IRenderedBrowseCustomizer:

    /*------------------------------------------------------------------------------
        Purpose: Callback method into Service classes, allows the customization of
                 RenderedBrowseControl itself
        Notes:   Invoked during the Constructor of the RenderedBrowseControl
        @param phProcedure The procedure handle that created the original BROWSE widget
        @param phBrowseHandle The browse handle that is the foundation for the RenderedBrowseControl
        @param poGrid The reference to the RenderedBrowseControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignBrowseProperties (phProcedure AS HANDLE,
                                               phBrowseHandle AS HANDLE,
                                               poGrid AS RenderedBrowseControl).

    /*------------------------------------------------------------------------------
        Purpose: Callback method into Service classes, allows the customization of
                 RenderedBrowseControl itself
        Notes:   Invoked during the OnInitializeLayout override
        @param phProcedure The procedure handle that created the original BROWSE widget
        @param phBrowseHandle The browse handle that is the foundation for the RenderedBrowseControl
        @param poGrid The reference to the RenderedBrowseControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeLayout (phProcedure AS HANDLE,
                                         phBrowseHandle AS HANDLE,
                                         poGrid AS RenderedBrowseControl).

END INTERFACE.