//this package is required for express
const express = require('express');

//this package is required for routing
const router = express.Router();

// this is required to parse request body 
const bodyParser = require('body-parser');

//this is common config 
const {config} = require("./config.js");

//this is common functions library 
const lib = require("./lib.js");

// this is required to parse XML request 
require('body-parser-xml')(bodyParser);

router.use(bodyParser.urlencoded({ extended: true }));

router.use((req, res, next) => {
    bodyParser.xml({
        verify: addRawBody,
    })(req, res, (err) => { // this is syntax error check for XML
        if (err) {
            res.setHeader('Content-Type', 'text/xml');
			console.log(lib.getDateTimeString() + " " + err);
            const response = lib.XMLResponse(400,"Bad request XML (Syntax Error)");
			res.status(400).send(response);
            return;
        }
        next();
    });
});

// this is required to modify XML request into string 
function addRawBody(req, res, buf, encoding) {
    req.rawBody = buf.toString();
}

/* ----------------------------- routes handlers ---------------------------------*/
//This route is to post cXML Orders
router.post('/cxmlorder', (req,res) => {
	lib.handleRoutecXML(req,res);
});

// validation for undefined routes for POST
router.post('*', function(req, res) {
	lib.routeNotFound(req,res);
});

// validation for undefined routes for GET
router.get('*', function(req, res) {
	lib.routeNotFound(req,res);
});

module.exports = router;
