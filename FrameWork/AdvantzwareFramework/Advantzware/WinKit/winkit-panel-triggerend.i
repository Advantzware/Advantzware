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
    File        : winkit-panel-triggerend.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     : Sun Feb 07 21:16:25 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

    DEFINE VARIABLE hWinKitWidgetHandle AS HANDLE NO-UNDO.

/* ***************************  Main Block  *************************** */

    FINALLY:

        IF VALID-OBJECT (oForm) THEN DO:
            Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager, FALSE, FALSE) .

            {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oTool in oForm:ToolbarsManager:Tools}

                hWinKitWidgetHandle = WIDGET-HANDLE (UNBOX (oTool:Tag)) .

                IF VALID-HANDLE (hWinKitWidgetHandle) AND CAN-QUERY (hWinKitWidgetHandle, "LABEL":U) THEN DO:
                    oTool:SharedProps:Caption = hWinKitWidgetHandle:LABEL .

                    {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oInstance in oTool:SharedProps:ToolInstances }

                        oInstance:InstanceProps:Caption = hWinKitWidgetHandle:LABEL .

                    END.
                END.
            END.
        END.

    END FINALLY.

