//this package is required for express
const express = require('express');
//this package is required for routing
const router = express.Router();
// this is required to parse request body 
const bodyParser = require('body-parser');
//this is common config 
const {config} = require("./config.js");
//this is common functions library 
var lib = require("./lib.js");
// this is required to parse XML request data */
router.use(bodyParser.urlencoded({ extended: true }));
router.use(bodyParser.text());

/* ----------------------------- routes  ---------------------------------*/
//This route is for fetching the inventory details
router.post('/cxmlorder', (req,res) => {
	lib.handleRouteXML(req,res);
});

// validation for undefined routes for POST
router.post('*', function(req, res) {
	if (lib.getRequestDataType(req) === "json"){
		lib.routeNotFoundJSON(res);
	}
	
	if (lib.getRequestDataType(req) === "plain"){
		lib.routeNotFoundXML(res);
	}
});

// validation for undefined routes for GET
router.get('*', function(req, res) {
	if (lib.getRequestDataType(req) === "json"){
		lib.routeNotFoundJSON(res);
	}
	
	if (lib.getRequestDataType(req) === "plain"){
		lib.routeNotFoundXML(res);
	}
});

module.exports = router;
