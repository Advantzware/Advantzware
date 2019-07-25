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
// this is required to parse JSON request data */
router.use(bodyParser.urlencoded({ extended: true }));
router.use(bodyParser.json());

// check the syntax of request JSON 
router.use(function(err, req, res, next) {
  if (err instanceof SyntaxError && err.status === 400 && "body" in err) {
	res.status(400).send(lib.JSONResponse(400,"Invalid JSON Request Data"));
  } else next();
});

// this function initiates API Route call
function handleRoute(req,res){
	var responseCode = "";
	var response = "";
	
    try {
        if (!lib.Authenticated(req)){
            res.set('WWW-Authenticate', 'Basic realm="401"');
			res.setHeader('Content-Type', 'text/json');
            res.status(401).send(lib.JSONResponse(401,"Invalid Username/Password supplied"));
            res.end();
        }
        else {
			if (lib.getRequestDataType(req) != "json"){
				responseCode = 400
				response = lib.JSONResponse(responseCode,"This API accepts JSON request");
			}			 
			 else {
				var JSONRequestData = lib.getFlatJSON(req.body);  
				JSONRequestData = JSON.stringify(JSONRequestData);
				
				response = JSON.parse(lib.callJavaProgram(req,config,JSONRequestData));
				responseCode = response.response_code;
				
				if (responseCode === 500 || response.response_code === 404){
					// writes the request data to csv file in case AppServer is down
					lib.CSVFileDataCreate(req,JSONRequestData,response);
					response = lib.JSONResponse(response.response_code,response.response_message);
				}   
			}
			 
            res.setHeader('Content-Type', 'text/json');
            res.status(responseCode).send(response);
            res.end();
        }
    }
    catch ( e ) {
        res.setHeader('Content-Type', 'text/json');
		response = lib.JSONResponse(400,"Bad response" + e);
        res.status(400).send(response);
        res.end();      
    }   
}

// This will handle if any invalid route is supplied    
function routeNotFound(res){
	res.setHeader('Content-Type', 'text/json');
	res.status(404).send(lib.JSONResponse(404,"route not found!"));
	res.end();
}

/* ----------------------------- routes handlers ---------------------------------*/

//This route is for fetching the inventory details
router.get('/getInventory', (req,res) => {
    handleRoute(req, res)
});

//This will create an inventory transfer record
router.post('/createInventoryTransfer', (req,res) => {
    handleRoute(req, res)
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
