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
    File        : SaveImagePlugin
    Purpose     : Saves an Image (PNG) of the Business Entity Design whenever
                  the .bedgm file is saved
    Syntax      :
    Description : Stores the Image of the Design Canvas in the same folder
                  that contains the .bedgm file of the Business Entity
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 26 12:27:11 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SaveImagePlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterFileSave event of the Controller
        Notes:
        @param sender The reference to the object that raised the event
        @param e The FileEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterFileSaveHandler (sender AS Progress.Lang.Object,
                                                e AS Consultingwerk.BusinessEntityDesigner.Services.FileEventArgs):

        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        ASSIGN cFileName = SUBSTRING (e:FileName, 1, R-INDEX (e:FileName, ".":U), "CHARACTER":U) + "png":U .

        oController:Model:Save (cFileName, Crainiate.ERM4.SaveFormat:Png) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, "Error saving image file"{&TRAN}) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the plugin
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

        /* noop */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the plugin
        Notes:
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController,
                                poForm AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm,
                                poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager):


        oController = poController .

        poController:AfterFileSave:Subscribe (AfterFileSaveHandler) .

    END METHOD .

END CLASS.
