//this package is required for express
const express = require('express')
//this package is required for routing
const router = express.Router();
// this is required to parse request body 
const bodyParser = require('body-parser');
//this is common config 
const {config} = require("./config.js");
//this is common functions library 
var lib = require("./lib.js");
//require('body-parser-xml')(bodyParser);
router.use(bodyParser.urlencoded({ extended: true }));
router.use(bodyParser.text());

// this function initiates API Route call
function handleRoute(req,res){
	var responseCode = "";
	var response = "";
	var responseMessage = "";
    var responseExceptionMessage = "";
	var responseWithException = "";
    try {
        if (!lib.Authenticated(req)){
            res.set('WWW-Authenticate', 'Basic realm="401"');
		    res.setHeader('Content-Type', 'text/xml');
            res.status(401).send(lib.XMLResponse(401,"Invalid Username/Password supplied"));
        }
        else {
			if (lib.getRequestDataType(req) != "plain"){
				responseCode = 400
				response = lib.XMLResponse(responseCode,"This API accepts Text/plain request");
			}
			else{
				const XMLRequestData = lib.getCompactXML(req.body); 
				if(lib.isBadXML(XMLRequestData)){
					responseCode = 400
				    response = lib.XMLResponse(responseCode,"Invalid XML Request Data");
			    }
				else{					
					response = lib.callJavaProgram(req,config,XMLRequestData);
					if (response.length > 0) {
						response = JSON.parse(response);
						if (!response) {
							response = {"response_code":500,"response_message":"Internal Server Error"};
						}
						responseCode = response.response_code;	
						responseMessage = response.response_message;
						responseExceptionMessage = response.exception;
						
						response = lib.XMLResponse(response.response_code,response.response_message);
						
						if (responseCode === 500 || responseCode === 404){
							// writes the request data to csv file in case AppServer is down
							responseWithException = lib.XMLResponseWithException(responseCode,responseMessage,responseExceptionMessage);
							lib.CSVFileDataCreate(req,XMLRequestData,responseWithException);
						}
					}
					else {
						responseCode = 500;
						response = lib.XMLResponse(responseCode,"Internal Server Error");						
					}
				}
            }
			 
			res.setHeader('Content-Type', 'text/xml');
			if (responseMessage.includes("cXML")) {
				response = responseMessage;
			}
			
			res.status(responseCode).send(response);
			res.end();
        }
    }
    catch ( e ) {
		res.setHeader('Content-Type', 'text/xml');
		response = lib.XMLResponse(400,"Bad response " + e);
        res.status(400).send(response);
        res.end();      
    }   
}
// This will handle if any invalid route is supplied    
function routeNotFound(res){
	res.setHeader('Content-Type', 'text/xml');
	res.status(404).send(lib.XMLResponse(404,"route not found!"));
	res.end();
}

/* ----------------------------- routes  ---------------------------------*/

//This route is for fetching the inventory details
router.post('/cxmlorder', (req,res) => {
	handleRoute(req,res);
});

// validation for undefined routes for POST
router.post('*', function(req, res) {
	routeNotFound(res);
});

// validation for undefined routes for GET
router.get('*', function(req, res) {
	routeNotFound(res);
});

module.exports = router;
