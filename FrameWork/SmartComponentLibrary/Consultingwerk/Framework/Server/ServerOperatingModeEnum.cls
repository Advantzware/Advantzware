/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ServerOperatingModeEnum
    Purpose     : Character Enumeration of Server Operating modes as returned
                  by SESSION:SERVER-OPERATING-MODe
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd
    Created     : Sun Oct 05 13:32:58 CEST 2014
    Notes       : Classic AppServer operating modes:
                  state-reset, state-aware, stateless, state-free
                  PASOE AppServer operting modes:
                  session-managed, session-free
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Server.* FROM PROPATH . 
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Framework.Server.ServerOperatingModeEnum: 

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the stateless mode
        Notes:   PASOE operating  mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SessionFree AS CHARACTER NO-UNDO INITIAL "session-free":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the stateless mode
        Notes:   PASOE operating  mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SessionManaged AS CHARACTER NO-UNDO INITIAL "session-managed":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the state-aware mode
        Notes:   Classic OpenEdge AppServer operating mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY StateAware AS CHARACTER NO-UNDO INITIAL "State-aware":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the state-free mode
        Notes:   Classic OpenEdge AppServer operating mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY StateFree AS CHARACTER NO-UNDO INITIAL "State-free":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the state-reset mode
        Notes:   Classic OpenEdge AppServer operating mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY StateReset AS CHARACTER NO-UNDO INITIAL "State-reset":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The AppServer is started with the stateless mode
        Notes:   Classic OpenEdge AppServer operating mode
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Stateless AS CHARACTER NO-UNDO INITIAL "Stateless":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the ServerOperatingModeEnum class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ServerOperatingModeEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is session-free
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               session-free
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsSessionFree ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:SessionFree .

    END METHOD.
    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is session-managed
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               session-managed
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsSessionManaged ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:SessionManaged .

    END METHOD.
    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is state-aware
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               state-aware
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsStateAware ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:StateAware .

    END METHOD.
    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is state-free
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               state-free
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsStateFree ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:StateFree .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is state-reset
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               state reset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsStateReset ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:StateReset .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Returns if the current AppServer operating mode is stateless
       Notes:
       @return Logical value indicating if the current AppServer operating mode is 
               state less
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsStateless ():
        
        RETURN SESSION:SERVER-OPERATING-MODE = ServerOperatingModeEnum:Stateless .

    END METHOD.

END CLASS.
