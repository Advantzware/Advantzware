import com.progress.open4gl.javaproxy.*;
import com.progress.open4gl.Parameter;
import com.progress.open4gl.RunTimeProperties;
import com.progress.open4gl.ConnectException;

public class RequestDispatcherAS{
    private String ipAppServerURL;
    private String ipRoute;
    private String ipVerb;
    private String ipUsername;
    private String ipPassword;
    private String ipRequestDataType;
    private String ipRequestData;
    private String procedureReturnValue;
    private String opResponseData;
    private String ipDLC;

    public void dispatch(String inputs[]){
        try{      
            ipAppServerURL = inputs[0];
            ipRoute = inputs[1];
            ipVerb = inputs[2];
            ipUsername = inputs[3];
            ipPassword = inputs[4];
            ipRequestDataType = inputs[5];
            ipRequestData = inputs[6];
            ipDLC = inputs[7];
            procedureReturnValue = "";
            opResponseData = "";
            
            // Remove the below comment in case Appserver is running in state-free mode
            // RunTimeProperties.setIntProperty("PROGRESS.Session.sessionModel",1);
            
            Connection asConnection = new Connection(ipAppServerURL, "", "", "");

            OpenAppObject appObject = new OpenAppObject(asConnection, "AppServerConnection");
            
            
            // Create the Parameters Array
            ParamArray procedureParams = new ParamArray(7);

            // Set up input parameters
            procedureParams.addCharacter(0, ipRoute, ParamArrayMode.INPUT);
            procedureParams.addCharacter(1, ipVerb, ParamArrayMode.INPUT);
            procedureParams.addCharacter(2, ipUsername, ParamArrayMode.INPUT);
            procedureParams.addCharacter(3, ipPassword, ParamArrayMode.INPUT);
            procedureParams.addCharacter(4, ipRequestDataType, ParamArrayMode.INPUT);
            procedureParams.addLongchar (5, ipRequestData, ParamArrayMode.INPUT);
            
            // Set up Out parameters - notice the value is null
            procedureParams.addLongchar(6, null, ParamArrayMode.OUTPUT);

            // Run the procedure
            appObject.runProc("api/inbound/APIRequestRouterAS.p", procedureParams);
  
            // Get output parameters - Returned as Object, so must cast
            opResponseData = (String) procedureParams.getOutputParameter(6);

            // Get RETURN-VALUE - Will return null for AddCustomer() procedure
            procedureReturnValue = (String)(procedureParams.getProcReturnString());
            appObject._release();

            System.out.println(opResponseData); 

        } // try to catch all unexpected exceptions
        catch (Exception e){
            handleException(ipRoute,
                            ipVerb,
                            ipRequestDataType,
                            ipRequestData,
                            e.getMessage().replace("\"","")
                            );
        }       
        catch (Throwable e){
            handleException(ipRoute,
                            ipVerb,
                            ipRequestDataType,
                            ipRequestData,
                            e.getMessage().replace("\"","")
                            );
        }           
    }
    private void handleException(String ipRoute, String ipVerb, String ipRequestDataType, String ipRequestData, String ipExceptionMessage ){
        System.out.println("{\"response_code\":500,\"response_message\":\"Internal Server Error\",\"exception\":\"" + ipExceptionMessage + "\"}");
    }
}