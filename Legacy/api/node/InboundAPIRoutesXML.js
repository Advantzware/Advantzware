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
					response = JSON.parse(lib.callJavaProgram(req,config,XMLRequestData));
					responseCode = response.response_code;
					response = lib.XMLResponse(response.response_code,response.response_message);
					 
					if (responseCode === 500 || responseCode === 404){
						// writes the request data to csv file in case AppServer is down
						lib.CSVFileDataCreate(req,XMLRequestData,response);
					}
				}
            }
			 
			res.setHeader('Content-Type', 'text/xml');
			res.status(responseCode).send(response);
			res.end();
        }
    }
    catch ( e ) {
		res.setHeader('Content-Type', 'text/xml');
		response = lib.XMLResponse(400,"Bad response" + e);
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
router.post('/amazoncXML', (req,res) => {
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
