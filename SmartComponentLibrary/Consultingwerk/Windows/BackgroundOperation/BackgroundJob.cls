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
    File        : BackgroundJob
    Purpose     : Shell for executing Progress AVM's as background jobs
                  from an interactive session
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 30 22:35:24 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                             FROM PROPATH.
USING Consultingwerk.Framework.*                   FROM PROPATH.
USING Consultingwerk.Framework.Collections.*       FROM PROPATH.
USING Consultingwerk.Support.*                     FROM ASSEMBLY.
USING Consultingwerk.Util.*                        FROM PROPATH.
USING Consultingwerk.Windows.BackgroundOperation.* FROM PROPATH.
USING Progress.Lang.*                              FROM PROPATH.
USING System.Diagnostics.*                         FROM ASSEMBLY.
USING System.Net.Sockets.*                         FROM ASSEMBLY.
USING System.Windows.Forms.*                       FROM ASSEMBLY.

CLASS Consultingwerk.Windows.BackgroundOperation.BackgroundJob:

    DEFINE VARIABLE oDataReader AS ProcessOutputReader        NO-UNDO .
    DEFINE VARIABLE oProcess    AS System.Diagnostics.Process NO-UNDO .

    &IF DEFINED (UseZeroMQ) NE 0 &THEN
    &IF PROVERSION BEGINS "10.2":U &THEN
    /* .NET 2.0 version of clrzmq */
    DEFINE VARIABLE oContext   AS ZMQ.Context NO-UNDO .
    DEFINE VARIABLE oPublisher AS ZMQ.Socket  NO-UNDO .
    &ELSE
    DEFINE VARIABLE oContext   AS ZeroMQ.ZmqContext NO-UNDO .
    DEFINE VARIABLE oPublisher AS ZeroMQ.ZmqSocket  NO-UNDO .
    &ENDIF
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Raised when the background job has finished
        Notes:
        @param sender The object that raised the ProcessFinished event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ProcessFinished SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised when the background job has (partial) data available
        Notes:
        @param sender The object that raised the ProcessOutputChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ProcessOutputChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                             e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Control should be added to the Job Monitor
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddToJobMonitor AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get's and set's if this job may get killed without user intervention
                 when the application is left
        Notes:   Default is TRUE
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AllowClose AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the comma delimited list of database names
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Databases AS CharacterList NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns an unique ID for this Job
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobId AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets additional parameters for the -param startup parameter
        Notes:   When using this value in the background job, be aware, that the
                 first two entries delimited by pipe are reserved for the messaging
                 (e.g. ZeroMQ)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobParameter AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Job title
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobTitle AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Output Data of the Process
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OutputData AS LONGCHAR NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets additional session parameter
        Notes:   You are not supposed to pass in the -param parameter using this
                 property. When your Job requires the -param parameter, please set
                 the value to the JobParameter properties. Additional info there
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Parameter AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the procedure executed by this background job
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProcedureName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the backgorund Job should receive a "QUIT" message
                 via ZeroMQ before killing it
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SendQuit AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the UI Environment of the background job
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UiEnvironment AS UiEnvironmentEnum NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BackGroundJob class
        Notes:
        @param pcProcedureName The procedure to execute in a background job
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BackGroundJob (pcProcedureName AS CHARACTER):
        SUPER ().

        THIS-OBJECT:Databases     = NEW CharacterList () .
        THIS-OBJECT:ProcedureName = pcProcedureName .
        THIS-OBJECT:UiEnvironment = UiEnvironmentEnum:Tty .
        THIS-OBJECT:JobId         = GUID .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Finds the UI Environment for the current background job
        Notes:
        @return The executable name for the UI environment
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER FindExecutable ():

        DEFINE VARIABLE cExe AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.

        ASSIGN cDLC = SessionHelper:DLC .

        IF Consultingwerk.ClientType:IsWebClient() THEN
            cExe = SUBSTITUTE ("&1~\bin~\prowc.exe":U, cDLC) .
        ELSE DO:

            CASE THIS-OBJECT:UiEnvironment:
                WHEN UiEnvironmentEnum:Gui THEN
                    ASSIGN cExe = SUBSTITUTE ("&1~\bin~\prowin32.exe":U, cDLC) .
                WHEN UiEnvironmentEnum:Tty THEN
                    ASSIGN cExe = SUBSTITUTE ("&1~\bin~\_progres.exe":U, cDLC) .
                OTHERWISE
                    UNDO, THROW NEW AppError ("Invalid UI environment.":U, 0) .
            END CASE .
        END.

        FILE-INFORMATION:FILE-NAME = cExe .

        /* Mike Fechner, Consultingwerk Ltd. 04.10.2014
           SCL-487: Attempt 64 bit client when prowin32.exe is not available */
        IF FILE-INFORMATION:FULL-PATHNAME = ? AND THIS-OBJECT:UiEnvironment = UiEnvironmentEnum:Gui THEN DO:
            ASSIGN cExe = SUBSTITUTE ("&1~\bin~\prowin.exe":U, cDLC) .

            FILE-INFORMATION:FILE-NAME = cExe .
        END.

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            RETURN cExe .

        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to locate executable: &1."{&TRAN}, cExe), 0) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the database startup parameter
        Notes:
        @return The startup parameters for the database connections
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetDatabaseParameter ():

        DEFINE VARIABLE cParam AS CHARACTER NO-UNDO.

        {Consultingwerk/foreachPrimitiveList.i Character cLDBName in THIS-OBJECT:Databases}

            ASSIGN cParam = SUBSTITUTE ("&1 &2":U,
                                        cParam,
                                        REPLACE (DBPARAM (cLDBName), ",":U, " ":U)) .
        END.

        RETURN cParam .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DataAvailable event of the
                 Consultingwerk.Support.ProcessOutputReader control instance
        Notes:   Raises the ProcessOutputChanged event of the BackgroundJob instance
        @param sender The reference to the object that raised the event
        @param e The default System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ProcessDataAvailableHandler (sender AS System.Object,
                                                     e AS System.EventArgs):

        DEFINE VARIABLE lcChunk AS LONGCHAR NO-UNDO.

        ASSIGN lcChunk = oDataReader:GetCurrentData (TRUE)
               THIS-OBJECT:OutputData = THIS-OBJECT:OutputData + lcChunk .

        THIS-OBJECT:OnProcessOutputChanged (Consultingwerk.EventArgs:Empty) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ProcessFinished
        Notes:
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnProcessFinished (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:ProcessFinished:Publish (THIS-OBJECT, e) .

        &IF DEFINED (UseZeroMQ) NE 0 &THEN
        IF VALID-OBJECT (oPublisher) THEN
            oPublisher:Dispose() .

        IF VALID-OBJECT (oContext) THEN
            oContext:Dispose() .
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ProcessOutputChanged
        Notes:
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnProcessOutputChanged (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:ProcessOutputChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Exited event of the Process
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessExitedHandler (sender AS System.Object,
                                                e AS System.EventArgs):

        OnProcessFinished (Consultingwerk.EventArgs:Empty) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the background job
        Notes:   The data reader control needs to be added to a control container to
                 be able to handle asynchronous messages from the child process
        @param poParentControl The ControlContainer to add the DataReader Control to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Start (poParentControl AS Control):

        DEFINE VARIABLE oJobProgressControl AS BackgroundJobProgressControl NO-UNDO .
        DEFINE VARIABLE cAssemblies         AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cParam              AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cTemp               AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE iPort               AS INTEGER INIT 5566            NO-UNDO .

        IF Consultingwerk.ClientType:IsWebClient() OR
           THIS-OBJECT:UiEnvironment = UiEnvironmentEnum:Gui THEN
            ASSIGN cAssemblies = StartupParameterHelper:GetStartupParameter ("-assemblies":U) .

        IF THIS-OBJECT:AddToJobMonitor THEN DO:
            oJobProgressControl = NEW BackgroundJobProgressControl () .
            oJobProgressControl:InitializeJobMonitor (THIS-OBJECT) .
            poParentControl = oJobProgressControl .
        END .

        IF NOT VALID-OBJECT (poParentControl) THEN
            poParentControl = FrameworkSettings:MdiContainer .

        oProcess = NEW System.Diagnostics.Process () .

        oProcess:StartInfo:FileName = THIS-OBJECT:FindExecutable() .

        &IF DEFINED (UseZeroMQ) NE 0 &THEN
        IF THIS-OBJECT:SendQuit THEN DO:

            &IF NOT PROVERSION BEGINS "10" &THEN
            /* The 64bit compatible version of the zero mq library (used on OpenEdge 11)
               needs to know the Assemblies folder */
            ZeroMQ.Interop.AvmInterop:AssembliesFolder = SessionHelper:AssembliesFullPath .
            &ENDIF

            &IF PROVERSION BEGINS "10.2":U &THEN
            /* .NET 2.0 version of clrzmq */
            oContext = NEW ZMQ.Context (1) .
            oPublisher = oContext:Socket(ZMQ.SocketType:PUB) .
            &ELSE
            oContext = ZeroMQ.ZmqContext:Create () .
            oPublisher = oContext:CreateSocket(ZeroMQ.SocketType:PUB) .
            &ENDIF

            bindBlock: REPEAT ON ERROR UNDO, THROW:

                DO ON ERROR UNDO, THROW:
                    oPublisher:Bind (SUBSTITUTE ("tcp://*:&1":U, iPort)).

                    LEAVE bindBlock .

                    &IF PROVERSION BEGINS "10.2":U &THEN
                    /* .NET 2.0 version of clrzmq */
                    @SuppressUnusedWarnings.
                    CATCH err AS ZMQ.Exception:
                    &ELSE
                    CATCH err AS ZeroMQ.ZmqException:
                    &ENDIF
                        ASSIGN iPort = iPort + 1 .

                        NEXT bindBlock .
                    END CATCH.
                END.
            END.
        END.
        &ENDIF

        IF THIS-OBJECT:JobParameter > "":U THEN
            ASSIGN cParam = SUBSTITUTE ("-param &1|&2|&3":U,
                                        THIS-OBJECT:JobId, iPort, THIS-OBJECT:JobParameter) .
        ELSE
            ASSIGN cParam = SUBSTITUTE ("-param &1|&2":U,
                                        THIS-OBJECT:JobId, iPort) .

        /* Mike Fechner, Consultingwerk Ltd. 15.10.2014
           SCL-494: SESSION:TEMP-DIRECTORY returns the temp-directory with a trailing backslash (on Windows).
                    As we need to put quotes around the temp-parameters when it contains a directory name with
                    spaces the resulting parameter looked like: -T "c:\progress\webclientapps\test test\test test\"
                    Windows seems to interpret the backslash just before the closing slash as an escape character
                    thus ignoring the closing quote */
        ASSIGN cTemp = RIGHT-TRIM (SESSION:TEMP-DIRECTORY, "~\":U) .

        oProcess:StartInfo:Arguments = SUBSTITUTE ("-b &1 -T &2 -p &3 &4 &5 &6":U,
                                                   THIS-OBJECT:GetDatabaseParameter (),
                                                   QUOTER (cTemp),
                                                   QUOTER (THIS-OBJECT:ProcedureName),
                                                   THIS-OBJECT:Parameter,
                                                   cAssemblies,
                                                   cParam) .

        oProcess:StartInfo:WorkingDirectory = System.Environment:CurrentDirectory .

        /* obtain access to stdout of the process */
        oProcess:StartInfo:RedirectStandardOutput = TRUE .

        oProcess:StartInfo:UseShellExecute = FALSE .
        oProcess:StartInfo:CreateNoWindow = TRUE .

        oProcess:StartInfo:ErrorDialog = TRUE .

        /* start process, get "reference" */
        oProcess:Start () .

        PROCESS EVENTS .

        oDataReader = NEW ProcessOutputReader (oProcess) .
        poParentControl:Controls:Add (oDataReader) .
        oDataReader:Show () .
        oDataReader:DataAvailable:Subscribe (ProcessDataAvailableHandler) .
        oDataReader:ProcessExited:Subscribe (THIS-OBJECT:ProcessExitedHandler) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stops the background job
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Stop ():

        @SuppressUnusedWarnings.
        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (oProcess) THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2013
           Handle that the Background's Job Process may actually not be valid.
           System.InvalidOperationException: Diesem Objekt ist kein Prozess zugeordnet.

           .NET Stack Trace:
              bei System.Diagnostics.Process.EnsureState(State state)
              bei System.Diagnostics.Process.get_HasExited()  */

        DO ON ERROR UNDO, THROW:
            IF oProcess:HasExited THEN
                RETURN .

            @SuppressUnusedWarnings.
            CATCH ex AS System.InvalidOperationException:
                RETURN .
            END CATCH.
        END.

        &IF DEFINED (UseZeroMQ) NE 0 &THEN
        IF THIS-OBJECT:SendQuit THEN DO:

            &IF PROVERSION BEGINS "10.2":U &THEN
            /* .NET 2.0 version of clrzmq */
            oPublisher:Send (SUBSTITUTE ("&1 QUIT":U,
                                         THIS-OBJECT:JobId),
                             System.Text.Encoding:Unicode) .
            &ELSE
            ASSIGN cMessage = SUBSTITUTE ("&1 QUIT":U,
                                          THIS-OBJECT:JobId) .

            oPublisher:Send (DataTypeHelper:CharacterToByteArray(cMessage),
                             LENGTH (cMessage),
                             ZeroMQ.SocketFlags:DontWait) .
            &ENDIF
        END.
        ELSE
        &ENDIF

        oProcess:Kill () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BackGroundJob class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BackGroundJob ():

        &IF DEFINED (UseZeroMQ) NE 0 &THEN
        IF VALID-OBJECT (oPublisher) THEN
            oPublisher:Dispose() .

        IF VALID-OBJECT (oContext) THEN
            oContext:Dispose() .
        &ENDIF

        THIS-OBJECT:Stop () .

        GarbageCollectorHelper:DeleteObject (oProcess) .

    END DESTRUCTOR .

END CLASS.
