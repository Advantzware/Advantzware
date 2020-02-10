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

router.use(bodyParser.urlencoded({ extended: true }));

// this is required to parse JSON request data */
router.use(bodyParser.json());

// check the syntax of request JSON 
router.use(function(err, req, res, next) {
  if (err instanceof SyntaxError && err.status === 400 && "body" in err) {
	res.status(400).send(lib.JSONResponse(400,"Bad request JSON - "+err));
  } else next();
});

/* ----------------------------- routes handlers ---------------------------------*/
//This route is for fetching the inventory details
router.post('/getinventory', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will transfer inventory to new location and bin
router.post('/createinventorytransfer', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will create an inventory stock alias for po
router.post('/createtagforPO', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will create BOL 
router.post('/createbilloflading', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will get purchase orders 
router.post('/getpurchaseorder', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will post inventory receipt 
router.post('/createinventoryreceipt', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will consume the inventory via BOL 
router.post('/consumeinventoryviabol', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This route fetches the inventory details for a BOL 
router.post('/getinventoryforbol', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will create and post inventory count transaction 
router.post('/createinventorycount', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will create vendor invoices
router.post('/createvendorinvoice', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will update itemfg records
router.post('/updateitem', (req,res) => {
    lib.handleRouteJSON(req, res);
});

//This will split a tag
router.post('/splittag', (req,res) => {
    lib.handleRouteJSON(req, res);
});
module.exports = router;
